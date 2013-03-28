open Html;;

let db_filename = ref "db";;
  (* The name of the file storing the current knowledge *)


let pipeline_length = ref 10;;
  (* Maximum number of requests to put on the pipeline.
   * Also limits the number of parallel connections.
   *)

let max_age = ref 6000.0;;
  (* If the entries are older than this amount of seconds, they are
   * checked again.
   *)

(**********************************************************************)
(***                      AUXILIARY FUNCTIONS                       ***)
(**********************************************************************)


let slash_re = Str.regexp "[/]";;

let text_html_re = Str.regexp "^[ \t\r]*text/html";;

let norm_path s =
  (* remove ".", "..", "//" as much as possible 
   * Example:
   * norm_path "/x/y/..//z" = "/x/z"
   *)
  let rec simplify_dot l =
    match l with
	"." :: l'               -> simplify_dot l'
      | ""  :: l' when l' <> [] -> simplify_dot l'
      | x   :: l'               -> x :: simplify_dot l'
      | []                      -> []
  in
  let rec simplify_dot_dot l =
    match l with
      | ".." :: l' -> ".." :: simplify_dot_dot l'
      | x :: l' ->
	  let l'' = simplify_dot_dot l' in
	  begin match l'' with
	      ".." :: l''' ->
		l'''
	    | _ -> x :: l''
	  end
      | [] -> []
  in

  let l = Str.split_delim slash_re s in
  let s' =
    match l with
	"" :: l' ->
	  (* absolute path *)
	  "/" ^  String.concat "/" (simplify_dot_dot (simplify_dot l'))
      | _ ->
	  (* relative path *)
	  String.concat "/" (simplify_dot_dot (simplify_dot l))
  in
  if s' = "" then "." else s'
;;


let norm_url s =
  (* Splits the URL into the "host" part "http://..." and the path.
   * Raises Not_found on any problem.
   * Example:
   * norm_url "http://localhost:8080/where/to/go"
   *   = "http://localhost:8080", "/where/to/go"
   *)
  let user,password,host,port,path =
    Http_client_aux.match_http s in
  "http://" ^
  (match user with
       None -> ""
     | Some u -> u) ^
  (match password with
       None -> ""
     | Some p -> ":" ^ p) ^
  (match user with
       None -> ""
     | Some _ -> "@") ^
  String.lowercase host ^ ":" ^
  string_of_int port,
  norm_path path
;;



(**********************************************************************)
(***                 HTML PARSING AND DATABASE UPDATES              ***)
(**********************************************************************)

let hash_re = Str.regexp "^\\([^#]*\\)#.*";;

let last_save = ref 0.0;;
  (* when the database was last saved; in seconds since the epoch *)

let parse_html db base s =
  (* Parses the HTML document s, and puts new links into 'db'.
   * 'base' is the URL of 's'.
   *)
  (* TODO: The 'base' element of HTML is not recognized. *)

  let base_host, base_path = 
    try norm_url base 
    with Not_found -> failwith ("parse_html: bad base URL: " ^ base)
  in

  let add_link href =
    (* Adds the hyperlink 'href' to the database 'db'. *)
    (* Remove everything after '#': *)
    let href' =
      if Str.string_match hash_re href 0 then
	Str.matched_group 1 href
      else
	href 
    in
    (* Absolute or relative URL? - Not_found on bad URL *)
    try
      let href'' =
	if String.length href' >= 7 & String.sub href' 0 7 = "http://" then begin
	  let host_part, path = norm_url href' in
	  host_part ^ path
	end 
	else begin
	  (* what's with ".", ".."? *)
	  if String.length href' >= 1 & href'.[0] = '/' then
	    base_host ^ href'
	  else begin
	    let d = 
	      if base_path = "/" or base_path = "" 
	      then "/" 
	      else Filename.dirname base_path in
	    base_host ^ norm_path(d ^ "/" ^ href')
	  end
	end
      in
      (* prerr_endline ("Found URL "  ^ href''); *)
      Database.add db href'';
      let t0 = Unix.gettimeofday() in
      if t0 -. !last_save >= 60.0 then begin
	prerr_endline "************* SAVING ****************";
	Database.save db !db_filename;
	last_save := t0
      end
    with
	Not_found -> ()
  in

  let rec traverse_list doclist =
    (* Walks through the document given as 'doclist', and enters all
     * hyperlinks into 'db'.
     * All "src" and "href" attributes are recognized as hyperlinks.
     *)
    match doclist with
	[] -> ()
      | Data _ :: doclist' ->
	  traverse_list doclist'
      | Element (name, atts, doclist') :: doclist'' ->
	  begin try
	    let href = List.assoc "href" atts in
	    add_link href
	  with Not_found -> ()
	  end;
	  begin try
	    let src = List.assoc "src" atts in
	    add_link src
	  with Not_found -> ()
	  end;
	  traverse_list doclist';
	  traverse_list doclist''
  in

  traverse_list(Parse.parse_string s)
;;


(**********************************************************************)
(***                          THE USER AGENT                        ***)
(**********************************************************************)

let run_through_db pipeline db =
  (* Uses 'pipeline' to process 'db' *)
  let s = Database.iter db !max_age !max_age in
    (* 's': a stream of database entries which needs to be processed *)

  let rec add_get_request url =
    (* Retrieves the document for the URL 'url' from the content server,
     * checks whether it is an HTML document, and if so, parses this document
     * and enters all new hyperlinks to the database 'db'.
     * After that, 'add_next' is called to put new requests on to the pipeline.
     *)
    let request = new Http_client.get url in
    pipeline # add_with_callback
      request
      (fun m ->
	 (* Parse the response if it is OK and mime type = HTML: *)
	 begin try
	   let http_version, code, _ = m # dest_status() in
	   if code >= 200 && code <= 299 then begin
	     let mime_type = 
	       try m # assoc_resp_header "content-type"
	       with Not_found -> "" in
	     let t0 = Unix.gettimeofday() in
	     Database.update
	       db 
	       url
	       code
	       t0
	       t0
	       mime_type;
	     if Str.string_match text_html_re mime_type 0 then begin
	       (* TODO: or "text/html;...options..." *)
	       let base_url =
		 try m # assoc_resp_header "content-base"
		 with
		     Not_found ->
		       try m # assoc_resp_header "content-location"
		       with
			   Not_found ->
			     url
	       in
	       parse_html db base_url (m # get_resp_body());
	     end;
	     prerr_endline ("Done " ^ url);
	   end
	   else begin
	     let t0 = Unix.gettimeofday() in
	     Database.update
	       db 
	       url
	       code
	       0.0
	       t0
	       "";
	     prerr_endline ("Error " ^ url);
	   end
	 with
	     any ->
	       prerr_endline ("Serious error: " ^
			      Printexc.to_string any);
	       let t0 = Unix.gettimeofday() in
	       Database.update
		 db 
		 url
		 999
		 0.0
		 t0
		 "";
	       prerr_endline ("Error " ^ url);
	 end;
	 (* Put the next request into the pipeline: *)
	 add_next()
      )

  and add_head_request url =
    (* Retrieves the header for the document for the URL 'url' from the 
     * content server, checks whether it is an HTML document, and if so, 
     * adds a GET request to the pipeline (by calling add_get_request).
     * If the document is not an HTML document (or if an error occurred),
     * 'add_next' is called to put new requests on to the pipeline.
     *)
    let request = new Http_client.head url in
    pipeline # add_with_callback
      request
      (fun m ->
	 (* Parse the response if it is OK and mime type = HTML: *)
	 begin try
	   let http_version, code, _ = m # dest_status() in
	   if code >= 200 && code <= 299 then begin
	     let mime_type = 
	       try m # assoc_resp_header "content-type"
	       with Not_found -> "" in
	     let t0 = Unix.gettimeofday() in
	     Database.update
	       db 
	       url
	       code
	       t0
	       t0
	       mime_type;
	     if  Str.string_match text_html_re mime_type 0 then begin
	       (* TODO: or "text/html;...options..." *)
	       add_get_request url
	     end
	     else begin 
	       prerr_endline ("Done " ^ url);
	       add_next()
	     end
	   end
	   else begin
	     let t0 = Unix.gettimeofday() in
	     Database.update
	       db 
	       url
	       code
	       0.0
	       t0
	       "";
	     prerr_endline ("Error " ^ url);
	     add_next()
	   end
	 with
	     any ->
	       prerr_endline ("Serious error: " ^
			      Printexc.to_string any);
	       let t0 = Unix.gettimeofday() in
	       Database.update
		 db 
		 url
		 999
		 0.0
		 t0
		 "";
	       prerr_endline ("Error " ^ url);
	       add_next()
	 end;
      )

  and add_next() =
    (* Fetch the next task(s) from the stream 's', and add new requests
     * onto the pipeline.
     *)
    try
      let (next_url,_,_,_,_) = Stream.next s in   (* or Stream.Failure *)
      try
	add_head_request next_url;
	if pipeline # number_of_open_messages < !pipeline_length then
	  add_next()
      with
	| any ->
	    prerr_endline ("Error while adding to the pipeline: " ^
			   Printexc.to_string any);
	    prerr_endline "Proceeding with next element...";
	    let t0 = Unix.gettimeofday() in
	    Database.update
	      db 
	      next_url
	      999
	      0.0
	      t0
	      "";
	    add_next()
    with
	Stream.Failure -> ()
	    (* 's' is empty: all is done *)
  in	 
	     
  let rec run_pipeline () =
    try
      pipeline # run()
    with
	Unix.Unix_error(_,_,_) ->
	  prerr_endline ("Unix error. Continuing with next element...");
	  run_pipeline()
      | Sys_error s ->
	  prerr_endline ("System error: " ^ s ^ 
			 " - Continuing with next element...");
	  run_pipeline()
      | Failure s ->
	  prerr_endline ("Failure: " ^ s ^ 
			 " - Continuing with next element...");
	  run_pipeline()
  in

  add_next();           (* Add the initial requests to the pipeline *)
  run_pipeline()        (* Start executing the pipeline *)
;;



let main() =
  let start_points = ref [] in
  let proxy_host = ref "" in
  let proxy_port = ref 8080 in
  let no_proxy = ref [] in
  Arg.parse
      [ "-db", Arg.String (fun s -> db_filename := s),
	    " <name>           Name of the database file (default: db)";
	"-limit", Arg.Int (fun n -> pipeline_length := n),
	       " <n>           Limit for the length of the pipeline (default: 1000)";
	"-proxy-host", Arg.String (fun s -> proxy_host := s),
	            " <name>   Host name of the proxy to use";
	"-proxy-port", Arg.Int (fun n -> proxy_port := n),
	            " <n>      Port of the proxy to use (default: 8080)";
	"-no-proxy", Arg.String (fun s -> no_proxy := s :: !no_proxy),
	          " <name>     Do not use proxy for this host or this domain";
	"-max-age", Arg.Int (fun n -> max_age := float_of_int n),
	         " <n>         Maximum age of database entries";
      ]
      (fun s -> start_points := s :: !start_points)
      "usage: spider [options] URL ...";

  let pipeline = new Http_client.pipeline in

  if !proxy_host <> "" then
    pipeline # set_proxy !proxy_host !proxy_port;

  pipeline # avoid_proxy_for !no_proxy;

  let db = Database.restore !db_filename in
  List.iter
    (fun s ->
       try
	 let host_part, path = norm_url s in
	 prerr_endline ("Adding " ^ host_part ^ path);
	 Database.add db (host_part ^ path)
       with
	   Not_found ->
	     prerr_endline ("Bad URL: " ^ s)
    )
    !start_points;

  let opt = pipeline # get_options in
  pipeline # set_options
    { opt with 
	Http_client.number_of_parallel_connections = 1;
	Http_client.verbose_status = true;
        Http_client.verbose_connection = true;
        Http_client.verbose_response_header = true;
(*        Http_client.verbose_response_contents = true;
        Http_client.verbose_request_header = true;
        Http_client.verbose_request_contents = true;
*)
    };

  run_through_db pipeline db;
  Database.save db !db_filename
;;

