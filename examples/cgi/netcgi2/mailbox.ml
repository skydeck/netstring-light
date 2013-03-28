(* $Id: mailbox.ml 817 2004-07-31 13:15:46Z stolpmann $ *)

(**********************************************************************
 * This CGI displays the contents of a mailbox. First, the available
 * mail messages are listed, with a hyperlink for every message. If
 * the user clicks at this link, the selected message can be viewed.
 *
 * Messages in MIME format can be decoded and displayed part by part.
 * Images and HTML pages are included "inline" into the generated
 * page. Attachments are represented by further hyperlinks allowing
 * the attachments to be viewed and saved.
 *
 * Potentially dangerous contents like scripts are removed from the
 * HTML pages, so the message viewer can be considered as safe with
 * respect to viruses etc. Furthermore, images with a "src" attribute
 * referring to another part of the message (using "cid" (content-id)
 * URIs) are displayed in the HTML page.
 *
 * The mailbox must be in "mbox" format, i.e. there is a "From " line
 * before every message.
 *
 * This program demonstrates the following techniques:
 * - How to design a CGI with several pages
 * - How a CGI can refer to itself
 * - How to parse "mbox" mailboxes
 * - How to parse MIME messages
 * - How to cope with international character encodings
 * - How to parse and transform HTML pages
 **********************************************************************)


(**********************************************************************
 * CONFIGURATION                                 READ THIS CAREFULLY!!!
 *
 * You MUST change the following variables to your local conventions!
 ***********************************************************************)

let mbox_file = "testbox"
  (* The mailbox file in mbox(5) format. *)

(**********************************************************************)

open Netcgi
open Netchannels
open Printf


(**********************************************************************
 * MAILBOX ACCESS FUNCTIONS
 *
 * Various methods to extract mail data from the mailbox
 **********************************************************************)

let from_re = Pcre.regexp ~flags:[`MULTILINE] "^From ";;
let lf_re = Pcre.regexp "\n";;


let input_max ch s pos len =
  (* Try to read as much as possible *)
  let n = ref (ch # input s pos len) in
  let k = ref 1 in
  ( try
      while !n < len && !k <> 0 do
	k := (ch # input s (pos + !n) (len - !n));
	n := !n + !k
      done;
    with
	End_of_file -> ()
  );
  !n
;;


let scan_mailbox() =
  (* Scans the mailbox for "From " lines. Returns a list of tuples
   * (from_pos, header_pos, end_pos).
   * from_pos: The byte position of the "From " line
   * header_pos: The byte position of the line following the "From " line
   *   which is expected to be the first line of the header
   * end_pos: The byte position of the "From " line of the next message,
   *   or the EOF position.
   *
   * Note: [end_pos] is not correct. The better [end_pos] is the line
   * before the next "From " line. But this is not visible to the user.
   *)

  let chunksize = 4096 in
  let blocksize = 2 * chunksize in
  let block = String.create blocksize in

  let rec scan_blocks ch offset blocklen pos =
    let limit = min blocklen chunksize in
    try
      if pos > limit then raise Not_found;
      let r = Pcre.exec ~rex:from_re ~pos block in
      let from_pos, after_from_pos = Pcre.get_substring_ofs r 0 in
      if from_pos > limit || after_from_pos > blocklen then raise Not_found;

      let r = Pcre.exec ~rex:lf_re ~pos:after_from_pos block in
      let lf_pos, header_pos = Pcre.get_substring_ofs r 0 in
      if header_pos > blocklen then raise Not_found;

      let pair = (from_pos+offset, header_pos+offset) in
      pair :: scan_blocks ch offset blocklen header_pos
    with
	Not_found ->
	  if blocklen = blocksize then begin
	    (* Get the next block *)
	    String.blit block chunksize block 0 chunksize;
	    let n = input_max ch block chunksize chunksize in
	    scan_blocks ch (offset+chunksize) (chunksize+n) 1
	  end
	  else []  (* EOF *)
  in

  let rec norm_list eof_pos l =
    match l with
	(from_pos, header_pos) :: (((from_pos', header_pos') :: _) as l') ->
	  (from_pos, header_pos, from_pos') :: norm_list eof_pos l'
      | [from_pos, header_pos] ->
	  [from_pos, header_pos, eof_pos]
      | [] ->
	  []
  in

  let msg_list, eof_pos =
    with_in_obj_channel
      (new input_channel (open_in_bin mbox_file))
      (fun ch ->
	 let n = input_max ch block 0 blocksize in
	 let l = scan_blocks ch 0 n 0 in
	 (l, ch # pos_in)
      )
  in
  norm_list eof_pos msg_list
;;


let extract_header (from_pos, header_pos, end_pos) =
  (* Extracts the header for the specified mail from the mailbox *)
  let real_ch = open_in_bin mbox_file in
  try
    seek_in real_ch header_pos;
    let stream = new Netstream.input_stream ~len:(end_pos - header_pos)
		   (new input_channel real_ch) in
    let h = Netmime.read_mime_header stream in
    close_in real_ch;
    h
  with error ->
    close_in real_ch;
    raise error
;;


let extract_message (from_pos, header_pos, end_pos) =
  (* Extracts the complete message from the mailbox *)
  let real_ch = open_in_bin mbox_file in
  try
    seek_in real_ch header_pos;
    let stream = new Netstream.input_stream ~len:(end_pos - header_pos)
		   (new input_channel real_ch) in
    let h = Netmime.read_mime_message stream in
    close_in real_ch;
    h
  with error ->
    close_in real_ch;
    raise error
;;


let rec extract_from_mime_stream f stream path =
  (* Find the part addressed by [path] within [stream], and call [f]. *)
  if path = [] then
    f stream
  else begin
    let h_obj = Netmime.read_mime_header stream in
    let mime_type, mime_type_params =
      try h_obj#content_type() with Not_found -> ("text/plain", []) in
    let multipart = "multipart/" in
    let is_multipart_type =
      (String.length mime_type >= String.length multipart) &&
      (String.sub mime_type 0 (String.length multipart) = multipart) in

    if is_multipart_type then begin
      let n :: path' = path in
      (* --- Divide the message into parts: --- *)
      let boundary =
        try List.assoc "boundary" mime_type_params
        with Not_found -> failwith "missing boundary parameter"
      in
      let k = ref 1 in
      let _ =
	Mimestring.read_multipart_body
	  (fun substream ->
	     if !k = n then (* Found the right part *)
	       extract_from_mime_stream f substream path';
	     incr k
	  )
	  (Mimestring.param_value boundary)
	  stream
      in
      ()
    end
  end
;;


let extract_part (from_pos, header_pos, end_pos) path =
  (* Extracts only the requested part of the message. [path] is a list
   * of list positions addressing the part.
   *)
  let msg = ref None in
  let real_ch = open_in_bin mbox_file in
  try
    seek_in real_ch header_pos;
    extract_from_mime_stream
      (fun stream ->
	 msg := Some (Netmime.read_mime_message ~multipart_style:`None stream))
      (new Netstream.input_stream ~len:(end_pos - header_pos)
          (new input_channel real_ch))
      path;
    close_in real_ch;
    begin match !msg with
    | None -> failwith "Message entity not found"
    | Some m -> m
    end
  with error ->
    close_in real_ch;
    raise error
;;


(**********************************************************************
 * GENERIC HTML COMPONENTS
 **********************************************************************)

let html_re = Pcre.regexp "[\\<\\>\\&\\\"]" ;;

let text s =
  (* This function encodes "<", ">", "&", double quotes, but no other
   * characters
   *)
  Pcre.substitute ~rex:html_re
    ~subst:(function
		"<" -> "&lt;"
	      | ">" -> "&gt;"
	      | "&" -> "&amp;"
	      | "\"" -> "&quot;"
	      | x -> x)
    s
;;


let i18n_text s =
  (* [s] is a mail header string that optionally contains RFC 2047-style
   * encoded words. These are converted to HTML, if possible. The returned
   * charset is always UTF-8.
   *)
  let words = Mimestring.scan_encoded_text_value s in
  let html_words =
    List.map
      (fun w ->
	 try
	   let data = Mimestring.get_decoded_word w in
	   let charset = Mimestring.get_charset w in
	   let enc = Netconversion.encoding_of_string charset in
	   Netconversion.recode_string ~in_enc:enc ~out_enc:`Enc_utf8 data
         with
	   Failure s ->
	     ("[Error: Cannot decode word]")
      )
      words
  in
  text(String.concat "" html_words)
;;


let begin_page (cgi : cgi) title =
  (* Output the beginning of the page with the passed [title]. *)
  let out = cgi # out_channel # output_string in
  out "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \
	\"http://www.w3.org/TR/html4/strict.dtd\">\n";
  out "<html>\n";
  out "<head>\n";
  out ("<title>" ^ text title ^ "</title>\n");
  out "</head>\n";
  out "<body>\n";
  out ("<h1>" ^ text title ^ "</h1>\n")
;;


let end_page (cgi : cgi) =
  let out = cgi # out_channel # output_string in
  out "</body>\n";
  out "</html>\n"
;;


(**********************************************************************
 * LIST OF MESSAGES
 *
 * This page lists the messages found in the mailbox.
 **********************************************************************)

let display_list_page (cgi : cgi) =
  let out = cgi # out_channel # output_string in

  begin_page cgi "List of Messages";

  (* Output a table with three columns: subject, sender, date. The subject
   * is a hyperlink, and when you follow it, you can see the whole message
   *)

  let messages = scan_mailbox() in

  out "<table>\n";
  out "<tr><th>Subject</th><th>From</th><th>Date of submission</th></tr>\n";

  List.iter
    (fun (from_pos, header_pos, end_pos as msg) ->
       (* Create the arguments for the hyperlink: *)
       let args =
	 [ new simple_argument "entity" "view-message";
	   new simple_argument "from_pos" (string_of_int from_pos);
	   new simple_argument "header_pos" (string_of_int header_pos);
	   new simple_argument "end_pos" (string_of_int end_pos)
	 ]
       in
       let href = cgi # url
		    ~with_query_string: (`Args args) () in

       (* Extract the header for [msg], and display the three requested
	* header fields.
	*)

       let h = extract_header msg in

       (* [h] is a [mime_header] object, see module [Netmime] *)

       let subject =
	 i18n_text (try h # field "subject" with Not_found -> "(No subject)") in
       let sender =
	 i18n_text (try h # field "from" with Not_found -> "(No sender)") in
       let date =
	 (try h # field "date" with Not_found -> "(No date)") in

       out (sprintf "<tr><td><a href=\"%s\">%s</a></td>\
		<td>%s</td><td>%s</td></tr>\n"
	      href
	      subject
	      sender
	      date);
    )
    messages;

  out "</table>\n";

  end_page cgi
;;


(**********************************************************************
 * VIEW MESSAGE
 *
 * Display the whole message with all parts
 **********************************************************************)

let split_type t =
  (* Split a mime type like "text/html" into its two parts *)
  try
    let k = String.index t '/' in
    (String.sub t 0 k), (String.sub t (k+1) (String.length t - k - 1))
  with
      Not_found -> t, ""
;;


let allowed_html_elements =
  [ "tt"; "i"; "b"; "big"; "small"; "u"; "s"; "strike"; "em"; "strong";
    "dfn"; "code"; "samp"; "kbd"; "var"; "cite"; "abbr"; "acronym";
    "sup"; "sub"; "span"; "bdo"; "br"; "a"; "img"; (* no object *)
    (* no script *) "map"; "q"; (* no applet *) "font"; "basefont";
    (* no iframe *) "input"; "select"; "textarea"; "label"; "button";
    "p"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "ul"; "ol"; "dir"; "menu";
    "pre"; "dl"; "div"; "noscript"; "blockquote"; "form"; "hr"; "table";
    "fieldset"; "address"; "center"; "noframes"; "isindex";
    (* no body *) "area"; (* no link *) "param"; "ins"; "del"; "dt";
    "dd"; "li"; "optgroup"; "option"; "legend"; "caption"; "thead";
    "tbody"; "tfoot"; "colgroup"; "col"; "tr"; "th"; "td";
    (* no head *) (* no title *) (* no base *) (* no meta *)
    (* no style *) (* no html *) (* no frameset *) (* no frame *)
  ]
;;


let harmful_html_elements = (* Remove them completely *)
  [ "object"; "script"; "applet"; "iframe"; "style"; "title" ]
;;


let allowed_html_attributes =
  [ "abbr"; "accept-charset"; "accept"; "accesskey"; "align"; "alink";
    "alt"; (* no archive *) "axis"; "background"; "bgcolor"; "border";
    "cellpadding"; "char"; "charoff"; "charset"; "checked"; "cite";
    "class"; (* no classid *) "clear"; (* no code *) (* no codebase *)
    (* no codetype *) "color"; "cols"; "colspan"; "compact"; "content";
    "coords"; (* no data *) "datetime"; (* no declare *) (* no defer *)
    "dir"; "disabled"; "enctype"; "face"; "for"; "frame"; "frameborder";
    "headers"; "height"; "hreflang"; "hspace"; "http-equiv"; "id";
    "ismap"; "label"; "lang"; (* no language *) "link"; "longdesc";
    "marginheight"; "marginwidth"; "maxlength"; "media"; "method";
    "multiple"; "name"; "nohref"; "noresize"; "noshade"; "nowrap";
    (* no object *) (* no onXXX *) (* no profile *) "prompt"; "readonly";
    "rel"; "rev"; "rows"; "rowspan"; "rules"; "scheme"; "scope"; "scrolling";
    "selected"; "shape"; "size"; "span"; "standby"; "start"; "style";
    "summary"; "tabindex"; "target"; "text"; "title"; "type"; "usemap";
    "valign"; "value"; "valuetype"; "version"; "vlink"; "vspace"; "width";
  ]
;;


let restricted_html_attributes = [ "action"; "href"; "src" ]


let http_re = Pcre.regexp "^\\s*http:.*$" ;;
let ftp_re = Pcre.regexp "^\\s*ftp:.*$" ;;
let mailto_re = Pcre.regexp "^\\s*mailto:.*$" ;;
let unrestricted_uris = [ http_re; ftp_re; mailto_re ] ;;

let cid_uri_re = Pcre.regexp "^\\s*cid:(.*)$";;


let clean_tree mk_cid_href html =
  (* Transfrom the HTML tree:
   * - Remove all potentially dangerous elements and attributes
   * - Convert cid: URIs
   *)
  let elements = Hashtbl.create 50 in
  let atts = Hashtbl.create 50 in
  List.iter (fun e -> Hashtbl.add elements e ()) allowed_html_elements;
  List.iter (fun a -> Hashtbl.add atts a ()) allowed_html_attributes;

  let rec recurse html =
    match html with
	Nethtml.Element (name, attlist, subtrees) ->
	  if Hashtbl.mem elements name then begin
	    (* The element is ok. Look at the attribute list *)
	    let attlist' =
	      List.flatten
		(List.map
		   (fun (aname,aval) ->
		      if Hashtbl.mem atts aname then
			[ aname,aval ]
		      else
			if List.mem aname restricted_html_attributes then begin
			  if List.exists
			    (fun rex -> Pcre.pmatch ~rex aval)
			    unrestricted_uris
			  then
			    [ aname,aval ]
			  else
			    if Pcre.pmatch ~rex:cid_uri_re aval then begin
			      (* Convert this attribute *)
			      let [| _; cid |] =
				Pcre.extract ~rex:cid_uri_re aval in
			      try
				let href = mk_cid_href cid in (* or Not_found *)
				[ aname, href ]
			      with
				  Not_found -> []
			    end
			    else
			      []  (* drop *)
			end
			else
			  []  (* unknown: drop *)
		   )
		   attlist
		)
	    in
	    let subtrees' = List.flatten(List.map recurse subtrees) in
	    [ Nethtml.Element (name,attlist',subtrees') ]
	  end
	  else begin
	    if List.mem name harmful_html_elements then
	      []
	    else
	      List.flatten(List.map recurse subtrees)
	  end
      | Nethtml.Data s ->
	  [ Nethtml.Data s ]
  in
  recurse html
;;


let cid_re = Pcre.regexp "^\\s*\\<(.*)\\>\\s*$";;

let display_message_page (cgi : cgi) =
  let out = cgi # out_channel # output_string in

  let print_header h =
    let subject =
      i18n_text (try h # field "subject" with Not_found -> "(No subject)") in
    let sender =
      i18n_text (try h # field "from" with Not_found -> "(No sender)") in
    let receiver =
      i18n_text (try h # field "to" with Not_found -> "(No receiver)") in
    let date =
      (try h # field "date" with Not_found -> "(No date)") in

    out "<table>\n";
    out (sprintf "<tr><td>Subject:</td><td>%s</td></tr>\n" subject);
    out (sprintf "<tr><td>Date:</td><td>%s</td></tr>\n" date);
    out (sprintf "<tr><td>From:</td><td>%s</td></tr>\n" sender);
    out (sprintf "<tr><td>To:</td><td>%s</td></tr>\n" receiver);
    out "</table>\n";
    out "<br />\n";
  in

  let print_text_body params simple_body =
    try
      let charset =
	try Mimestring.param_value(List.assoc "charset" params)
	with Not_found -> "us-ascii" in
      let enc = Netconversion.encoding_of_string charset in
      let data = Netconversion.recode_string ~in_enc:enc ~out_enc:`Enc_utf8
		   simple_body#value in
      let html_data = text data in
      out "<pre>\n";
      out html_data;
      out "</pre>\n";
    with
	Failure s ->
	  out ("[Cannot decode this part: " ^ s ^ "]")
      | Netconversion.Malformed_code ->
	  out ("[Cannot decode this part: Bad character encoding]")
  in

  let print_html_body cid_map params simple_body =
    try
      let charset =
	try Mimestring.param_value(List.assoc "charset" params)
	with Not_found -> "us-ascii" in
      let enc = Netconversion.encoding_of_string charset in
      let data = Netconversion.recode_string ~in_enc:enc ~out_enc:`Enc_utf8
		   simple_body#value in
      (* Now parse [data] as HTML text: *)
      let html_tree =
	Nethtml.parse
	    ~dtd:Nethtml.relaxed_html40_dtd
	    (new input_string data) in
      (* Throw out all dangerous stuff (scripts), and replace links to
       * cid:xxx by the right self url
       *)
      let base_args =
	[ new simple_argument "entity" "view-part";
	  cgi # argument "from_pos";
	  cgi # argument "header_pos";
	  cgi # argument "end_pos";
	] in
      let mk_cid_href cid =
	try
	  let path = Hashtbl.find cid_map cid in
	  let args = (new simple_argument "path" path) :: base_args in
	  cgi # url
	    ~with_query_string: (`Args args) ()
	with
	    Not_found -> ""
      in
      let html_tree' =
	List.flatten (List.map (clean_tree mk_cid_href) html_tree) in
      (* Finally write the tree: *)
      let outch = (cgi#output :> out_obj_channel) in
      out "<div>\n";
      Nethtml.write ~dtd:Nethtml.relaxed_html40_dtd outch html_tree';
      out "</div>\n";
    with
	Failure s ->
	  out ("[Cannot decode this part: " ^ s ^ "]")
      | Netconversion.Malformed_code ->
	  out ("[Cannot decode this part: Bad character encoding]")
  in


  let print_image path =
    let args =
      [ new simple_argument "entity" "view-part";
	cgi # argument "from_pos";
	cgi # argument "header_pos";
	cgi # argument "end_pos";
	new simple_argument "path" (String.concat "."
				      (List.map string_of_int path));
      ]
    in
    let href = cgi # url  ~with_query_string:(`This args) () in
    out (sprintf "<img src=\"%s\">\n" href)
  in

  let print_link mime_type path =
    let inline_args =
      [ new simple_argument "entity" "view-part";
	cgi # argument "from_pos";
	cgi # argument "header_pos";
	cgi # argument "end_pos";
	new simple_argument "path" (String.concat "."
				      (List.map string_of_int path));
      ]
    in
    let attachment_args =
      ( new simple_argument "attachment" "yes" ) :: inline_args in
    let inline_href = cgi # url
                 ~with_query_string: (`This inline_args) () in
    let attachment_href = cgi # url
	         ~with_query_string: (`This attachment_args) () in
    out (sprintf "Content-type: %s<BR>\n" mime_type);
    out (sprintf "<a href=\"%s\">View</a>\n" inline_href);
    out (sprintf "<a href=\"%s\">Save</a>\n" attachment_href);
  in


  let rec collect_content_ids cid_map path (header,cbody) =
    begin try
      let cid_s = header # field "content-id" in  (* or Not_found *)
      let [| _; cid |] = Pcre.extract ~rex:cid_re cid_s in (* or Not_found *)
      let path_s = String.concat "." (List.map string_of_int path) in
      Hashtbl.add cid_map cid path_s
    with
	Not_found -> ()
    end;
    match cbody with
	`Parts l ->
	  let k = ref 0 in
	  List.iter
	    (fun p ->
	       collect_content_ids cid_map (path @ [!k]) p;
	       incr k)
	    l
      | _ ->
	  ()
  in

  let rec print_complex_message (cmsg : Netmime.complex_mime_message) cid_map path =
    let (header,body) = cmsg in

    if path = [] then
      print_header header
    else
      out (sprintf "<h2>Message entity %s</h2>\n"
		      (String.concat "." (List.map string_of_int path)));
    match body with
	`Body simple_body ->
	  let mime_type, params =
	    try header # content_type()
	    with Not_found ->
	      if path = [] then
		("text/plain", [])
	      else
		("application/octet-stream", [])
	  in
	  let major_type, minor_type = split_type mime_type in

	  let disp, _ =
	    try header # content_disposition()
	    with Not_found -> "inline", []
	  in

	  if disp = "inline" then begin
	    match major_type, minor_type with
	      | "text", "html" ->
		  print_html_body cid_map params simple_body
	      | "text", _ ->
		  print_text_body params simple_body
	      | "image", ("gif"|"jpeg"|"png") ->
		  print_image path
	      | _, _ ->
		print_link mime_type path
	  end
	  else
	    print_link mime_type path

      | `Parts parts ->
	  (* It is guaranteed that the header HAS a content-type *)
	  let mime_type, _ = header # content_type() in
	  out (sprintf "<b>Multipart type: %s </b>\n" mime_type);
	  let n = ref 1 in
	  List.iter
	    (fun part ->
	       if !n > 1 then out "<hr>\n";
	       print_complex_message part cid_map (path @ [!n]);
	       incr n
	    )
	    parts
  in

  begin_page cgi "Message";

  let from_pos = int_of_string (cgi # argument_value "from_pos") in
  let header_pos = int_of_string (cgi # argument_value "header_pos") in
  let end_pos = int_of_string (cgi # argument_value "end_pos") in

  let msg = extract_message (from_pos,header_pos,end_pos) in

  let cid_map = Hashtbl.create 50 in
  collect_content_ids cid_map [] msg;

  print_complex_message msg cid_map [];

  end_page cgi
;;

(**********************************************************************
 * VIEW MESSAGE PART
 *
 * This is for fragments of the message that need to be downloaded
 * separately. For example, images.
 **********************************************************************)

let dot_re = Pcre.regexp "\\.";;

let display_part_fragment (cgi : cgi) =
  let from_pos = int_of_string (cgi # argument_value "from_pos") in
  let header_pos = int_of_string (cgi # argument_value "header_pos") in
  let end_pos = int_of_string (cgi # argument_value "end_pos") in
  let attachment = cgi # argument_value "attachment" = "yes" in
  let path =
    List.map int_of_string
      (Pcre.split ~rex:dot_re (cgi # argument_value "path")) in

  let msg_hdr, msg_body =
    match extract_part (from_pos,header_pos,end_pos) path with
    | (hdr, `Body body) -> (hdr,body)
    | _                 -> assert false
  in

  if attachment then begin
    cgi # set_header
      ~content_type:"application/octet-stream"
      ~filename:("part-" ^ cgi # argument_value "path")
      ()
  end
  else begin
    cgi # set_header
      ~content_type:(try msg_hdr # field "content-type"
		     with Not_found -> "application/octet-stream")
      ()
  end;

  let ch = msg_body # open_value_rd() in
  cgi # out_channel # output_channel ch;
  ch # close_in()
;;



(**********************************************************************
 * REQUEST BROKER
 **********************************************************************)

let process (cgi:cgi) =
  (* Set a default header: This might be overridden later, but if an
   * early error happens, we have a header nethertheless.
   *)
  cgi # set_header ~content_type:"text/html; charset=utf-8" ();

  (* Get the [entity] argument. It determines which part of the
   * mailbox has been requested. If it does not exist, it defaults to
   * "list".  *)
  let entity = cgi # argument_value ~default:"list" "entity" in
  let display = match entity with
    | "list" ->         display_list_page
    | "view-message" -> display_message_page
    | "view-part" ->    display_part_fragment
    | _ -> failwith "Unknown entity"
  in
  display cgi;

  (* Commit everything: *)
  cgi # out_channel # commit_work()


let () =
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_cgi.run ~output_type:(`Transactional buffered) process


(* ======================================================================
 * History:
 *
 * $Log$
 * Revision 1.3  2004/07/31 13:15:46  stolpmann
 * 	Updated: End_of_file is caught in input_max
 *
 * Revision 1.2  2002/11/01 21:29:27  stolpmann
 * 	Fix: The ~len argument of input_stream is now correct
 *
 * Revision 1.1  2002/02/02 18:57:53  stolpmann
 * 	Initial revision.
 *)
