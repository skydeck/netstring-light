(* Based on code written by Xavier Leroy <Xavier.Leroy@inria.fr> to
   handle the 2001 ICFP contest submissions.  Xavier posted his code
   on the (now defunct) web-caml mailing list in order to be used as a
   testbed for an OCaml web framework.

   The following version differs sensibly from the original one but is
   still quite low-level -- for example the HTML is part of this file
   and the validation is done by hand.
*)

open Netcgi
open Printf

(***********************************************************************
 * CONFIGURATION
 ***********************************************************************)

let submission_dir = "/tmp/icfp-contest"
  (** Directory where submissions are stored.  It must be writable by
      the owner running the CGI scripts (usually "www-data").  This
      directory must NOT be under the web root for security reasons. *)

let tar_pgm = "/bin/tar"
let gunzip_pgm = "/bin/gunzip"
let unzip_pgm = "/usr/bin/unzip"

let judges_email = "judges@pauillac.inria.fr"

let counter_file = Filename.concat submission_dir "NEXT"


(***********************************************************************
 * HTML PAGES generation
 ***********************************************************************)

let text = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ()
  (* This function encodes "<", ">", "&", double quotes, and Latin 1
     characters as character entities -- e.g. text "<" = "&lt;". *)

let begin_html (out:string -> unit) ~title =
  out "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \
	\"http://www.w3.org/TR/html4/strict.dtd\">\n";
  out "<html>\n<head>\n";
  out ("<title>" ^ text title ^ "</title>\n");
  (*---------------------------- CSS --------------------------------*)
  out ("<style type=\"text/css\">\n");
  out "body { background: white; color: black; }\n";
  out "h1 { text-align: center; background: black; color: white; \
    padding: 0.5ex; }\n";
  out ".error { background: #dc3333; color: white; }\n";
  out "fieldset.submit { margin-left: auto; margin-right: auto; \
	    width: 40%;  border: 2px solid #cccccc; padding: 4px; }\n";
  out "</style>\n";
  out "</head>\n<body>\n";
  out (sprintf "<h1>%s</h1>\n" (text title))

let end_html out = out "</body>\n</html>"


(***********************************************************************
 * SUBMISSION UPLOAD
 ***********************************************************************)

(* Check if a text entry is valid (in particular non empty). *)
let is_valid s =
  try
    let valid = ref false in
    for i = 0 to String.length s - 1 do
      if Char.code(s.[i]) < Char.code(' ') || s.[i] = '\127' then raise Exit
      else if s.[i] <> ' ' then valid := true
    done;
    !valid
  with Exit -> false


(* Display the upload page (possibly showing errors).  If the upload
   passes the simple validation of the arguments, return the
   corresponding entry struct. *)
let upload_page (cgi:cgi) =
  let form = Buffer.create 0x1000 in
  let out = Buffer.add_string form in
  let submitted = cgi#argument_value "page" = "Submit" in
  let at_least_one_error = ref false in
  (* Construct the row with entry description [desc] and CGI parameter
     name [name].  If the form has been submitted, check the entries
     and highlight the erroneous ones. *)
  let tr desc ?(entry="text") ?(mandatory=true) name =
    let value =
      try
        let a = cgi#argument name in
        match a#filename with Some f -> f | None -> a#value
      with _ -> "" in
    let err =
      if not submitted || not mandatory || is_valid value then "" else (
        at_least_one_error := true;
        "class=\"error\""
      ) in
    out(sprintf "  <tr><td>%s</td><td>" (text desc));
    out(sprintf "<input type=%s name=%S value=%S %s/></td></tr>\n"
          entry name (text value) err)
  in
  tr "Team name:"      "team";
  tr "Program name:"   "program";
  tr "Language:"       "language";
  tr "Contact email:"  "email";
  tr "Resubmission?"   "resubmission" ~entry:"checkbox" ~mandatory:false;
  tr "File upload:"    "file" ~entry:"file";
  tr "Length (bytes):" "size" ~mandatory:false;
  tr "MD5 checksum:"   "md5" ~mandatory:false;
  if not(submitted) || !at_least_one_error then begin
    cgi#set_header ~cache:`No_cache ();
    let out = cgi#out_channel#output_string in
    begin_html out "ICFP 2001: Submit your entry";
    out (sprintf "<form method=\"post\" action=\"%s\" \
      enctype=\"multipart/form-data\">\n"               (cgi#url()));
    out "  <fieldset class=\"submit\"><legend>Submit your entry</legend>\n\
      <table cellspacing=\"0\">";
    if !at_least_one_error then
      out "<tr><td colspan=2>Please fill correctly the <i>required</i> \
      fields in <span class=\"error\">in red</span>.</td></tr>\n";
    out(Buffer.contents form);
    out "<tr><td></td><td>\
      <input type=submit name=\"page\" value=\"Submit\"/></td>\n";
    out "</table></fieldset>\n</form>";
    end_html out;
    cgi#out_channel#commit_work();
    raise Exit (* Need to (re)submit the form *)
  end


(* Validate file length and MD5
 ***********************************************************************)

(* [arg_store] mandates that the "file" argument is stored in [`File] *)
let tempfile cgi =
  match (cgi#argument "file")#store with `File s -> s | _ -> assert false

type 'a value = No_value | Parse_error | Value of 'a

let validate_file (cgi:cgi) =
  let tempfile = tempfile cgi in
  let actual_len = (Unix.stat tempfile).Unix.st_size in
  let actual_md5 = Digest.to_hex(Digest.file tempfile) in
  let claimed_len =
    try
      let s = (cgi#argument "size")#value in
      if s = "" then
        No_value
      else if Str.string_match (Str.regexp "[ \t]*\\([0-9]+\\)") s 0 then
        Value(int_of_string (Str.matched_group 1 s))
      else
        Parse_error
    with
    | Failure _ -> Parse_error
    | Not_found -> No_value in
  let claimed_md5 =
    try
      let s = (cgi#argument "md5")#value in
      if s = "" then
        No_value
      else if Str.string_match (Str.regexp "[ \t]*\\([0-9A-Za-z]+\\)") s 0
      then (
        let md5 = String.lowercase(Str.matched_group 1 s) in
        if String.length md5 = 32 then Value md5 else Parse_error
      )
      else
        Parse_error
    with Not_found -> No_value in
  if (match claimed_len with Value l -> l <> actual_len | _ -> false)
    || (match claimed_md5 with Value m -> m <> actual_md5 | _ -> false)
  then begin
    cgi#set_header ~cache:`No_cache ();
    let out = cgi#out_channel#output_string in
    begin_html out "Error";
    out "<p>Error during file transmission:</p>\n";
    begin match claimed_len with
    | Value l when l <> actual_len ->
        out(sprintf "<p>Actual file length is %d bytes, \
          instead of %d as claimed.</P>\n" actual_len l)
    | _ -> ()
    end;
    begin match claimed_md5 with
    | Value m when m <> actual_md5 ->
        out(sprintf "<p>Actual file MD5 is <code>%s</code>, \
          instead of <code>%s</code> as claimed.</p>\n" actual_md5 m)
    | _ -> ()
    end;
    out(sprintf "<p>Did you send the wrong file by any chance?  Please
  <a href=\"%s\">try again</a>. If the problem persists, maybe something
  is wrong in this file upload script; please notify
  <a href=\"mailto:%s\">the contest judges</a>.</p>"
          (text (cgi#url ~with_query_string:`Env ()))  judges_email);
    end_html out;
    cgi#out_channel#commit_work();
    raise Exit
  end;
  (actual_len, claimed_len, actual_md5, claimed_md5)


(* Determine and validate file type
 ***********************************************************************)

type file_type = Tar | Tar_gz | Zip

let re_tar_gz = Str.regexp_case_fold ".*\\.\\(tgz\\|tar\\.gz\\|tar\\.z\\)"
let re_tar = Str.regexp_case_fold ".*\\.tar"
let re_zip = Str.regexp_case_fold ".*\\.zip"

let validate_file_type (cgi:cgi) =
  let tempfile = tempfile cgi in
  let name =
    match (cgi#argument "file")#filename with Some n -> n | None -> "(none)" in
  (* Check extension *)
  let typ =
    if Str.string_match re_tar_gz name 0 then Tar_gz
    else if Str.string_match re_tar name 0 then Tar
    else if Str.string_match re_zip name 0 then Zip
    else begin
      cgi#set_header ~cache:`No_cache ();
      let out = cgi#out_channel#output_string in
      begin_html out "Error";
      out(sprintf "<p>File type <code>%s</code> is not recognized.</p>
  This script can only accept the following type of files:
  <ul>
    <li>Compressed tar files: <code>.tgz</code>, <code>.tar.gz</code>,
     <code>.tar.Z</code>
    <li>Uncompressed tar files: <code>.tar</code>
    <li>ZIP archives: <code>.zip</code>
  </ul>
  <p>Please provide a file of one of these types, with the appropriate
  extension on the file name.</p>"
            (text name));
      out(sprintf "<a href=\"%s\">Try again</a>\n"
            (text (cgi#url ~with_query_string:`Env ())));
      end_html out;
      cgi#out_channel#commit_work();
      raise Exit
    end in
  (* Check the un-archiving command works *)
  let cmd =
    match typ with
    | Tar ->
        sprintf "%s tf %s >/dev/null 2>/dev/null" tar_pgm tempfile
    | Tar_gz ->
        sprintf "%s -c %s 2>/dev/null | %s tf - >/dev/null 2>/dev/null"
          gunzip_pgm tempfile tar_pgm
    | Zip ->
        sprintf "%s -t %s >/dev/null 2>/dev/null" unzip_pgm tempfile in
  (* FIXME: This is what Xavier Leroy used but it does not seem tp be
     reliable (tar returns 0 even if the file is not an archive!).
     Maybe using "file" is better. *)
  if Sys.command cmd <> 0 then begin
    cgi#set_header ~cache:`No_cache ();
    let out = cgi#out_channel#output_string in
    begin_html out "Error";
    out(sprintf "<p>The file <code>%s</code> seems corrupted: <code>%s</code>
  does not recognize it.</p>"
          name (match typ with
                | Tar -> "tar tf"
                | Tar_gz -> "tar tzf"
                | Zip -> "unzip -t"));
    out("<p>Please make sure that you sent the right file and gave it a
  file extension that matches its contents.  If the problem persists,
  maybe something is wrong in this file upload script; please notify
  <a href=\"mailto:" ^ judges_email ^ "\">the contest judges</a>.</p>");
    end_html out;
    cgi#out_channel#commit_work();
    raise Exit
  end;
  typ


(* Record the submission
 ***********************************************************************)

(* Return a string representation of the current time *)
let now() =
  let gmt = Unix.gmtime(Unix.time()) in
  sprintf "%04d-%02d-%02d %02d:%02d:%02d GMT"
    (1900 + gmt.Unix.tm_year)  (1 + gmt.Unix.tm_mon)  gmt.Unix.tm_mday
    gmt.Unix.tm_hour  gmt.Unix.tm_min  gmt.Unix.tm_sec


let record_submission (cgi:cgi)
    (actual_len, claimed_len, actual_md5, claimed_md5) filetype start_date =
  (* Open and lock log file -- we use it as the master lock *)
  let log =
    open_out_gen [Open_wronly; Open_creat] 0o600
      (Filename.concat submission_dir "LOG") in
  Unix.lockf (Unix.descr_of_out_channel log) Unix.F_LOCK 0;
  (* Determine next submission number *)
  let nextid = open_in counter_file in
  let num = int_of_string (input_line nextid) in
  close_in nextid;
  (* Increment submission number *)
  let nextid = open_out counter_file in
  fprintf nextid "%d\n" (num + 1);
  close_out nextid;
  (* Move temp file to final file name *)
  let filename =
    match filetype with
    | Tar -> sprintf "%d.tar" num
    | Tar_gz -> sprintf "%d.tar.gz" num
    | Zip -> sprintf "%d.zip" num in
  let tempfile = tempfile cgi in
  Unix.link tempfile (Filename.concat submission_dir filename);
  (* Record submission info in log *)
  let current_date = now() in
  let hostip = cgi#environment#cgi_remote_addr in
  let hostname = cgi#environment#cgi_remote_host in
  seek_out log (out_channel_length log);
  fprintf log
"-----------------------------------------------------
Submission number: %d
Submission began at: %s
Submission recorded at: %s
Submitted from: %s (%s)
Team name: %s
Program name: %s
Language: %s
Resubmission: %b
E-mail contact: %s
MD5: %s
File: %s\n\n"
    num
    start_date  current_date
    hostname hostip
    (cgi#argument_value "team")
    (cgi#argument_value "program")
    (cgi#argument_value "language")
    (cgi#argument_exists "resubmission")
    (cgi#argument_value "email")
    actual_md5
    filename;
  close_out log;
  num


(* Print a warm fuzzy acknowlegdement
 ***********************************************************************)

let acknowledge (cgi:cgi) (actual_len, claimed_len, actual_md5, claimed_md5)
    filetype subm_num =
  cgi#set_header ~cache:`No_cache ();
  let out = cgi#out_channel#output_string in
  begin_html out "Submission acknowledgement";
  out (sprintf "<p>Your submission to the ICFP programming contest was
    received in good order.</p>
    <blockquote>
      Submission number: <b>%d</b><br />
      Team name: <b>%s</b><br />
      Program name: <b>%s</b><br />
      Language: <b>%s</b><br />
      File length: <b>%d</b><br />
      File MD5 checksum: <b><code>%s</code></b><br />
      Resubmission? <b>%b</b><br />
      E-mail contact: <b>%s</b>
    </blockquote>
    %s
    %s
    <p>A listing of the contents of the submitted file is appended below
    so that you can check that everything is OK.</p>
    <p>Thanks for your submission!</p>
    <hr />
    <pre>"
         subm_num
    (text (cgi#argument_value "team"))
    (text (cgi#argument_value "program"))
    (text (cgi#argument_value "language"))
    actual_len
    actual_md5
    (cgi#argument_exists "resubmission")
    (text (cgi#argument_value "email"))
    (match claimed_len with
     | Value _ -> ""
     | No_value ->
         "<p>Warning: you did not provide the length of \
          the submitted file.  Please make sure the length above \
          is correct.</p>"
     | Parse_error ->
         "<p>Warning: the file length you provided could not be parsed. \
          Please make sure the length above is correct.</p>")
    (match claimed_md5 with
     | Value _ -> ""
     | No_value ->
         "<p>Warning: you did not provide the MD5 checksum of \
          the submitted file.  Please make sure the checksum above \
          is correct.</p>"
     | Parse_error ->
         "<p>Warning: the MD5 checksum you provided could not be parsed. \
          Please make sure the checksum above is correct.</p>");
      );
  let cmd =
    let tempfile = tempfile cgi in
    match filetype with
    | Tar ->
        sprintf "%s tvf %s 2>&1" tar_pgm tempfile
    | Tar_gz ->
        sprintf "( %s -c %s | %s tvf - ) 2>&1" gunzip_pgm tempfile tar_pgm
    | Zip ->
        sprintf "%s -l %s 2>&1" unzip_pgm tempfile in
  let toc = Unix.open_process_in cmd in
  cgi#out_channel#output_channel(new Netchannels.input_channel toc);
  ignore(Unix.close_process_in toc);
  out "    </pre>\n<hr />";
  end_html out;
  cgi#out_channel#commit_work()



let main (cgi:cgi) =
  let start_date = now() in
  upload_page cgi; (* raise Exit if entries to set/change *)
  let fileinfo = validate_file cgi in
  let filetype = validate_file_type cgi in
  let subm_num = record_submission cgi fileinfo filetype start_date in
  acknowledge cgi fileinfo filetype subm_num


let () =
  (* Custom exn handler *)
  let exn_handler env f =
    try f()
    with
    | Netcgi_common.HTTP _ as e -> raise e (* browser error *)
    | Exit -> () (* Acceptable way of ending early *)
    | exn ->
        let exn = Printexc.to_string exn in
        env#log_error(sprintf "The script %S raised %S" Sys.argv.(0) exn);
        (* Send email to the judges *)
        let msg = Netsendmail.compose
          ~from_addr:("ICFP 2001", "webmaster@pauillac.inria.fr")
          ~to_addrs:[("Judges", judges_email)]
          ~subject:"Erreur script ICFP"
          (sprintf "Error in CGI script: uncaught exception %s\n" exn) in
        Netsendmail.sendmail msg;
        (* Generate error page *)
        env#send_output_header();
        let out = env#out_channel#output_string in
        begin_html out "Internal error in CGI script";
        out (sprintf "\
  <p>This CGI script encountered an internal error during processing:
  <pre>Uncaught exception %s</pre>
  The judges have been notified.  Try resubmitting your entry in a
  couple of hours.  If the deadline is approaching, send it by e-mail
  to <a href=\"mailto:judges@pauillac.inria.fr\">the contest judges</a>
  (preferred format: MIME attachment).  Thanks for your patience!</p>"
               exn);
        end_html out;
        env#out_channel#close_out()
  in
  let config = { default_config with
                   tmp_directory = Filename.concat submission_dir ".tmp" } in
  (* Setup: create the dirs and files if needed *)
  (try
     Unix.mkdir submission_dir 0o770;
     Unix.mkdir config.tmp_directory 0o770;
     if not(Sys.file_exists counter_file) then (
       let fh = open_out counter_file in
       fprintf fh "0\n";
       close_out fh
     )
   with _ -> ());
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  let arg_store _ name _ = if name = "file" then `File else `Memory in
  Netcgi_cgi.run ~config ~arg_store ~exn_handler
    ~output_type:(`Transactional buffered)
    main



(* Custom exn handler *)
let exn_handler env f =
  try f()
  with
  | Netcgi_common.HTTP _ as e -> raise e (* browser error *)
  | Exit -> () (* Acceptable way of ending early *)
  | exn ->
      let exn = Printexc.to_string exn in
      env#log_error(sprintf "The script %S raised %S" Sys.argv.(0) exn);
      (* Send email to the judges *)
      let msg = Netsendmail.compose
        ~from_addr:("ICFP 2001", "webmaster@pauillac.inria.fr")
        ~to_addrs:[("Judges", judges_email)]
        ~subject:"Erreur script ICFP"
        (sprintf "Error in CGI script: uncaught exception %s\n" exn) in
      Netsendmail.sendmail msg;
      (* Generate error page *)
      env#send_output_header();
      let out = env#out_channel#output_string in
      begin_html out "Internal error in CGI script";
      out (sprintf "\
  <p>This CGI script encountered an internal error during processing:
  <pre>Uncaught exception %s</pre>
  The judges have been notified.  Try resubmitting your entry in a
  couple of hours.  If the deadline is approaching, send it by e-mail
  to <a href=\"mailto:judges@pauillac.inria.fr\">the contest judges</a>
  (preferred format: MIME attachment).  Thanks for your patience!</p>"
              exn);
      end_html out;
      env#out_channel#close_out()



let config = { default_config with
  tmp_directory = Filename.concat submission_dir ".tmp" }

let () =
  (* Setup: create the dirs and files if needed *)
  (try
    Unix.mkdir submission_dir 0o770;
    Unix.mkdir config.tmp_directory 0o770;
    if not(Sys.file_exists counter_file) then (
      let fh = open_out counter_file in
      fprintf fh "0\n";
      close_out fh
    )
   with _ -> ())
