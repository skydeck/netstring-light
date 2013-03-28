(* $Id: filemanager.ml,v 1.8 2005/11/04 00:18:47 chris_77 Exp $ *)


(** This is an advanced example: A simple filemanager.  Every user has
    a flat directory where he/she can upload files, download them,
    delete them (... and perhaps some more actions ...)

    User authentication can be done by the web server, or by this CGI
    itself.

    This program demonstrates the following techniques:
    - How to design a CGI with many pages
    - How to use the POST method
    - How to upload and download files
    - How to set and read cookies
    - How to find out which user has been logged in

    Require: [Cryptokit] for cryptographically secure pseudo random
    number generator.
*)


(**********************************************************************
 * CONFIGURATION                                 READ THIS CAREFULLY!!!
 *
 * You MUST change the following variables to your local conventions,
 * and create the appropriate directories etc.
 ***********************************************************************)

let file_store = "/var/spool/webfilemanager"
let file_store = "/tmp/webfilemanager"
  (* This directory will contain one subdirectory per user, and stores
     the files.

     File permissions must be:
     - [file_store] must be owned by the user as which the script runs
       (this is usually the same as the web server user but might be
       different e.g. for external scripts).
     - [file_store] must be rwx for the script owner.
       You may add rx for other users if you want.

     [file_store] must not reside below the document root of the web
     server (security risk).

     There must be a directory ".tmp" in [file_store] (with the same
     permissions).

     For every user there must be a subdirectory with the same
     permissions.  By creating the subdirectory, the user will begin
     to exist for the file manager.

     EXAMPLE:
     mkdir /var/spool/webfilemanager
     mkdir /var/spool/webfilemanager/.tmp
     mkdir /var/spool/webfilemanager/tim
     mkdir /var/spool/webfilemanager/tina
     chown -R www:other /var/spool/webfilemanager
     chmod -R 755 /var/spool/webfilemanager
  *)

let password_file = Some(Filename.concat file_store "passwd")
  (* If you set [password_file] to [Some filename], the filemanager
     will display its own login page.  If set to [None], there will be
     no login page, and it is required that the web server is
     configured to do the password dialog.

     The format of the file is "user:password".

     EXAMPLE:
     touch /var/spool/webfilemanager/passwd
     chown www:other /var/spool/webfilemanager/passwd
     chmod 400 /var/spool/webfilemanager/passwd
     Contents of passwd:
     tim:Tim's password
     tina:foo
  *)

let session_timeout = 3600.0
  (* After this number of seconds idle users are logged out.  (This is
     the lifetime of the cookie.)  This works only if the filemanager
     uses its own password dialog.  *)

let secret_key_file = Filename.concat file_store "secretkey"
  (* This file will contain the keys used to sign cookies.  You need not
     to create the file, this is done automatically.  *)

let secret_key_timeout = 0.1 *. session_timeout
  (* After this number of seconds a new secret key is generated.
     There is no need to change this value.  *)

(* GENERAL NOTES ABOUT SECURITY:

   If HTTP is used as transport protocol, passwords are transmitted in
   an insecure way.  That the cookies are signed does not change this
   weakness.

   If HTTPS is used as transport protocol, the application is very
   secure.  Every piece of information is sent in an encrypted way.
   Furthermore, the signed cookies do make sense, because stolen cookies
   are hard to abuse.  You can login by such a cookie only if you have
   the same IP address.  Furthermore, this must happen before the cookie
   expires, and this date is contained in the cookie (the browser is not
   trusted).  The password cannot be extracted from a cookie.
*)

(* END OF CONFIGURATION
 *********************************************************************)

open Netcgi
open Printf

exception User_error of string

let rng = Cryptokit.Random.secure_rng

(**********************************************************************
 * USER/PASSWORD MANAGEMENT
 *
 * The following functions help to find out which user is logged in,
 * whether the user exists, and which is the workspace of the user.
 ***********************************************************************)

let colon_re = Pcre.regexp ":"

(* Splits the string [s] into the colon-separated parts *)
let split s = Pcre.split ~rex:colon_re ~max:(-1) s

exception Password of string  (* locally used in [user_exists] *)

(** [user_exists username] checks whether there is a directory for
    this user, and whether the required passwd entry exists.
    @return
    - `False: if the user does not exist
    - `Password pw: if this password is required to log in
    - `Passes: if no password is required (authentication by the web server)
*)
let user_exists username =
  let workspace_dir_exists =
    username <> "" && username.[0] <> '.' &&
    try
      (Unix.stat(Filename.concat file_store username)).Unix.st_kind = Unix.S_DIR
    with
      Unix.Unix_error(_,_,_) -> false
  in
  match password_file with
  | None ->
      (* We can assume that the user is already authenticated by the
	 web server.  So return `Passes or `False.  *)
      if workspace_dir_exists then `Passes else `False
  | Some pwfile ->
      (* Read the password file line by line, and search the entry for
         the user.  *)
      if not workspace_dir_exists then
	`False
      else (
	try
	  let f = open_in pwfile in
	  Netchannels.with_in_obj_channel (new Netchannels.input_channel f)
	    (fun ch ->
	       while true do
		 let line = ch#input_line() in    (* or End_of_file *)
		 match split line with
		 | [uname; pword] ->
		     if uname = username then raise(Password pword)
		 | _ ->
		     ()   (* => Ignore malformed lines *)
	       done
	    );
	  assert false (* This point is never reached *)
	with
	| End_of_file -> `False
	    (* The user is unknown because the entry is missing *)
	| Password pw -> `Password pw
	    (* The user is known and has password [pw] *)
      )


(* Secret keys: these are used to sign cookies.  The secret key file
   contains usually several secret keys.  It is known when every key
   was generated.

   - If we get a cookie from the browser, we accept it if it was
   signed with any of the keys and the key is not older than
   [session_timeout] seconds.

   - If we send a cookie to the browser, we use the newest key.  If
   the key is older than [secret_key_timeout], we create a new key,
   and add it to the list of secret keys.

   This way we can exactly control which cookies are accepted and need
   not to trust the browser implementation.
*)

(** Read in the file with secret keys. There is one key per line in
    the format:
    <creation-time>:<key (base64 encoded)>

    The creation time is in seconds since the epoch.  If the key is
    older than [session_timeout], it will not be returned by this
    function, and will no longer be used to check signed cookies.  *)
let read_secret_keys() =
  let t_valid = Unix.time() -. session_timeout in
  let rec read_file ch =
    try
      let line = ch#input_line() in  (* or End_of_file *)
      match split line with
      | [t_string; key] ->
	  let t = float_of_string t_string in
	  (* Only add the key if it is not too old: *)
	  if t >= t_valid then
	    (t, Netencoding.Base64.decode key) :: read_file ch
	  else read_file ch
      | _ -> assert false
    with
      End_of_file -> []
  in
  if not(Sys.file_exists secret_key_file) then
    (* No file: This is perfectly legal. This file will be created
       when the modified key list will be saved.  *)
    []
  else
    Netchannels.with_in_obj_channel
      (new Netchannels.input_channel (open_in secret_key_file))
      read_file


(** Returns a key that is acceptable to sign cookies.  If required,
    this function makes a new key and updates the [secret_key_file].
    Argument [key_list]: All known keys as pairs (creation_time, key)
    -- as returned by the function [read_secret_keys].  *)
let current_secret_key key_list =
  (* Find the newest key [key_max] which was created at [t_max].
     Default value of [(0.0, "")] is too old anyway. *)
  let (t_max, key_max) =
    let newer ((t,_) as k) ((t',_) as k') = if t > t' then k else k' in
    List.fold_left newer (0.0, "") key_list in
  (* Is the key acceptable? *)
  let now = Unix.gettimeofday() in
  if t_max >= now -. secret_key_timeout then
    (* Yes! *)
    key_max
  else begin
    (* Create a new key (128 bits). *)
    let key = Cryptokit.Random.string rng 16 in
    (* Save the new key.  We have to take into account that there
       might be a parallel CGI process doing the same.  Because of
       this, we open the file in append mode, and add the line.  There
       is no race condition if the line is short enough such that the
       [write] call is atomic.  This is normally true if the line is
       shorter than 512 bytes.  *)
    (* TODO: From time to time write a new file and throw all old keys
       away *)
    let flags = [Open_wronly; Open_append; Open_creat] in
    let fh = open_out_gen flags 0o600 secret_key_file in
    fprintf fh "%.0f:%s\n" now (Netencoding.Base64.encode key);
    close_out fh;
    key (* return the new key *)
  end


(** Creates the cookie string for [user] using the [key] to sign the
    cookie.  The [password] and the [ip_address] must be passed, too.
    The cookie string has the format

    <username>:<verifier>

    where <username> is the contents of the cookie, and <verifier> a
    piece of information ensuring that nobody has changed the contents
    (a "signature").
*)
let make_cookie user password ip_address key =
  let raw_verifier =
    Digest.string(String.concat ":" [user; password; ip_address; key]) in
  (* [raw_verifier] is the "signature". *)
  let verifier = Netencoding.Base64.encode raw_verifier in
  user ^ ":" ^ verifier


(** [check_cookie key_list ip_address cookie] checks whether the
    cookie with the string [cookie] is an authenticated user.  If so,
    the function returns [Some(username, password)], otherwise [None]
    is passed back.

    The [key_list] are the possible (valid) keys.  The [ip_address] of
    the client must be available, too.
*)
let split_cookie s =
  try (match split s with
       | [ u; v ] -> (u, Netencoding.Base64.decode v)
       | _ -> assert false)
  with Not_found -> assert false

let check_cookie key_list ip_address cookie =
  let rec check_keys user password cookie_verifier = function
    | [] -> None (* No valid key found *)
    | (_, key) :: keys' ->
	let test_verifier =
	  Digest.string(String.concat ":" [user; password; ip_address; key]) in
	if test_verifier = cookie_verifier then Some(user, password)
	else check_keys user password cookie_verifier keys'
  in
  let c = Cookie.value cookie in
  if c  = "" then
    None
  else
    let (user, cookie_verifier) = split_cookie c in
    match user_exists user with
    | `False -> None          (* maybe the user has been removed *)
    | `Password pw -> check_keys user pw cookie_verifier key_list
    | `Passes -> assert false (* strange *)



(**********************************************************************
 * HTML GENERATORS
 *
 * Some functions helping to generate HTML.
 **********************************************************************)

let text = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ()
  (* This function encodes "<", ">", "&", double quotes, and Latin 1
     characters as character entities. E.g. text "<" = "&lt;", and
     text "ä" = "&auml;" *)


(* Encodes [x] such that it can be safely used inside the *names*
   parameters of HTML forms.  We have to be careful because there
   might be bugs in browsers.

   The chosen encoding: Alphanumeric characters are represented by
   themselves.  The other characters are represented by an underscore
   followed by the hexadecimal encoding.
*)
let encode_filename x =
  let q = Netencoding.Q.encode x in      (* Uses '=' instead of '_' *)
  Pcre.qreplace ~pat:"=" ~templ:"_" q

let decode_filename x =
  (* The reverse function to [encode_filename] *)
  let q = Pcre.qreplace ~pat:"_" ~templ:"=" x in
  Netencoding.Q.decode q


let begin_page (cgi:cgi) title =
  (* Output the beginning of the page with the passed [title]. *)
  let out = cgi#out_channel#output_string in
  out "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \
	\"http://www.w3.org/TR/html4/strict.dtd\">\n";
  out "<html>\n";
  out "<head>\n";
  out ("<title>" ^ text title ^ "</title>\n");

  (*---------------------------- CSS --------------------------------*)
  out ("<style type=\"text/css\">\n");
  out "body { background: white; color: black; }\n";
  out "h2 { width: 95%; background: black; color: white; padding: 5px; \
            font-size: large; font-weight: normal }\n";
  out "fieldset.login { margin-left: auto; margin-right: auto; \
	    width: 40%;  border: 2px solid #cccccc; padding: 4px; }\n";
  out "fieldset.login legend { color: #bbbbbb; }\n";
  out "table.login { margin: auto; padding: 1ex; }\n";
  (* Style of file listing: *)
  (* (1) Rows *)
  out ".list-hdr-row  { background: #ff7777; }\n";
  out ".list-even-row { background: #ffffff; }\n";
  out ".list-odd-row  { background: #cccccc; }\n";
  (* (2) Columns *)
  out ".list-sel  { text-align: center; }\n";
  out ".list-name { text-align: left; }\n";
  out ".list-size { text-align: right; }\n";
  out ".list-date { text-align: right; }\n";
  out ".list-sel, .list-name, .list-size, .list-date \
	{ padding-left: 6px; padding-right: 6px }\n";
  (* Error page *)
  out "p.error { color: red; background-color: #cccccc; padding: 1ex;}\n";
  out "</style>\n";
  out "</head>\n";
  out "<body>\n";
  out ("<h1>" ^ text title ^ "</h1>\n")

let end_page (cgi:cgi) =
  cgi#out_channel#output_string "</body>\n</html>\n"


let begin_form ?(enctype = "application/x-www-form-urlencoded") (cgi:cgi) =
  cgi#out_channel#output_string
    (sprintf "<form method=POST action=\"%s\" enctype=\"%s\">\n"
	 (text(cgi#url()))
	 enctype)

let end_form (cgi:cgi) =
  cgi#out_channel#output_string "</form>\n"

let hidden_field (cgi:cgi) name value =
  cgi#out_channel#output_string
    (sprintf "<input type=hidden name=\"%s\" value=\"%s\">\n"
       (text name)
       (text value))


(* Check whether there is a CGI argument whose name is passed in the
   list [possible_buttons].  If so, the name of the argument is
   returned.  If not, "no_button" is returned.  *)
let pressed_button (cgi:cgi) possible_buttons =
  let is_button arg = List.mem arg#name possible_buttons in
  try (List.find is_button cgi#arguments)#name
  with Not_found -> "no_button"



(**********************************************************************
 * LOGIN PAGE
 **********************************************************************)

(* The login page is handled in [main] below, because the login
   page can "happen" at any time.  *)
let display_login_page (cgi:cgi) =
  let out = cgi#out_channel#output_string in
  begin_page cgi "Filemanager Login";
  begin_form cgi;
  out "<fieldset class=\"login\">\n";
  out "<legend>Please enter your user name and password</legend>\n";
  out "<table class=\"login\" cellspacing=\"0\">\n";
  out "<tbody>\n";
  out "<tr>\n";
  out "<td><label for=\"user\">User name:</label></td>\
	<td><input type=text name=\"cred_user\" id=\"user\"></td>\n";
  out "</tr>\n<tr>\n";
  out "<td><label for=\"passwd\">Password:</label></td>\
	<td><input type=password name=\"cred_password\" id=\"passwd\"></td>\n";
  out "</tr>\n<tr>\n";
  out "<td>&nbsp;</td>\n";
  out "<td><input type=submit name=\"LOGIN\" value=\"Login\"></td>\n";
  out "</tr>\n";
  out "</tbody>\n";
  out "</table>\n";
  out "</fieldset>\n";
  end_form cgi;
  end_page cgi


(**********************************************************************
 * LIST PAGE
 *
 * The list page displays the file listing, with lots of buttons.
 **********************************************************************)

(* Read the directory [dir] and return a list of files (name, size,
   modification time) sorted by name.  *)
let directory_list dir =
  let d = Unix.opendir dir in
  let l = ref [] in
  try
    while true do
      let n = Unix.readdir d in      (* or End_of_file *)
      let st = Unix.stat (Filename.concat dir n) in
      if n <> "." && n <> ".." then
	l := (n, st.Unix.st_size, st.Unix.st_mtime) :: !l
    done;
    assert false
  with
  | End_of_file ->
      Unix.closedir d;
      List.sort (fun (a,_,_) (b,_,_) -> String.compare a b) !l
  | err ->
      Unix.closedir d;
      raise err


let display_list_page user (cgi:cgi) =
  (* Just output the listing: *)
  let out = cgi#out_channel#output_string in
  begin_page cgi "Filemanager";

  begin_form cgi;
  hidden_field cgi "page" "list";
  out (sprintf "User:&nbsp;%s&nbsp;&nbsp;&nbsp;" (text user));
  let logout = Argument.simple "cred_logout" "yes" in
  out (sprintf "<a href=\"%s\">Log out</a>\n"
	 (text(cgi#url ~with_query_string:(`This[logout]) () )));

  out "<h2>List of files</h2>\n";
  out "<table cellspacing=\"0\" frame=\"border\" \
	rules=\"cols\" border=\"1\">\n";
  (* COLGROUPs would be helpful for this type of table. No browser
     seems to support them.  *)
  out "<thead>\n";
  out "<tr class=\"list-hdr-row\">\n";
  out "<th class=\"list-sel\">Select</th>\n";
  out "<th class=\"list-name\">Name</th>\n";
  out "<th class=\"list-size\">Size</th>\n";
  out "<th class=\"list-date\">Last modified</th>\n";
  out "</tr>\n";
  out "</thead>\n";
  out "<tbody>\n";
  let odd = ref false in
  let display_file (name, size, date) =
    let args = [ Argument.simple "page" "list";
		 Argument.simple "download_link" "yes";
		 Argument.simple "name" name
	       ] in
    let download_url = cgi#url ~with_query_string:(`This args) () in
    let row_class = if !odd then "list-odd-row" else "list-even-row" in
    out (sprintf "<tr class=\"%s\">\n" row_class);
    out (sprintf "  <td class=\"list-sel\">\
	<input type=\"checkbox\" name=\"sel_%s\" value=\"ON\"></td>\n"
	   (encode_filename name)
	);
    out (sprintf "  <td class=\"list-name\"><a href=\"%s\">%s</a></td>\n  \
	   <td class=\"list-size\">%d</td>\n  \
	   <td class=\"list-date\">%s</td>\n"
	   (text download_url)
	   (text name)
	   size
	   (Netdate.format ~fmt:"%Y-%m-%d %H:%m" (Netdate.create date)));
    out "</tr>\n";
    odd := not !odd;
  in
  let files = directory_list(Filename.concat file_store user) in
  List.iter display_file files;
  out "</tbody>\n";
  out "</table>\n";

  out "<h2>Actions</h2>\n";
  out "Select one or more files and press one of the following buttons:\n";
  out "<p><input type=\"submit\" name=\"delete_button\" value=\"Delete\">";
  end_form cgi;

  out "<h2>Upload</h2>\n";
  (* The upload form is a different form because we need a special enctype. *)
  begin_form ~enctype:"multipart/form-data" cgi;
  hidden_field cgi "page" "list";
  out "<p>You can upload a new file here:\n";
  out "<p><input type=\"file\" name=\"file\">\n";
  out "<input type=\"submit\" name=\"upload_button\" \
	value=\"Upload this file\">\n";
  end_form cgi;

  end_page cgi



let sel_re = Pcre.regexp "^sel_(.*)$"

(* Which of the files of the file list are selected?  Returns a list
   of filenames.  *)
let selected_files (cgi:cgi) =
  let extract name =
    try Pcre.extract ~rex:sel_re ~full_match:false name
    with Not_found -> [| |] in
  let names =
    List.map (fun arg ->
		match extract arg#name with
		| [| enc_name |] ->
		    if arg#value = "ON" then decode_filename enc_name else ""
		| _ -> ""
	     ) cgi#arguments
  in
  List.filter (fun name -> name <> "") names


(** This function is called if one of the buttons from the list page
    has been pressed.  The function returns the name of the
    continuation.  *)
let handle_list_page user (cgi:cgi) =
  (* Check which button has been pressed: *)
  let button = pressed_button cgi [ "upload_button";
				    "delete_button";
				    "download_link" ] in
  (* Execute the code for the button: *)
  match button with
  | "delete_button" ->
      (* Find out which files to delete: *)
      let files = selected_files cgi in
      if files = [] then raise(User_error "Please select at least one file!");
      `Confirm_delete files
  | "download_link" ->
      `Download(cgi#argument_value "name")
  | "upload_button" ->
      let file = cgi#argument "file" in
      (* Because the [~arg_store] parameter of [cgi] specified that
	 the argument called "file" will be put into a temporary
	 file, we can assume this now.  *)
      (* [tmp_filename]: The name of the temporary file where the
         uploaded material is stored now *)
      let tmp_filename =
	match file#store with `File name -> name | _ -> assert false in
      (* [user_filename]: The file name entered by the user *)
      let user_filename =
	match file#filename with Some name -> name | _ -> assert false in
      (* Normalize the user_filename now: *)
      (* (1) Convert backslashes into slashes *)
      let user_filename =
	Pcre.qreplace ~pat:"\\\\" ~templ:"/" user_filename in
      (* (2) Get the part after the last slash: *)
      let user_filename = Filename.basename user_filename in
      if user_filename = "." || user_filename = "" || user_filename = "/" then
	raise(User_error "Bad file");
      (* Move the [tmp_filename] to its place: *)
      Sys.rename tmp_filename (Filename.concat
				 (Filename.concat file_store user)
				 user_filename);
      (* Show the file list again: *)
      `List
  | _ ->
      (* By default: do nothing, and display the list again *)
      `List


(**********************************************************************
 * CONFIRM DELETE PAGE
 **********************************************************************)

let display_confirm_delete_page files user (cgi:cgi) =
  let out = cgi#out_channel#output_string in
  begin_page cgi "Filemanager";
  begin_form cgi;
  hidden_field cgi "page" "confirm_delete";
  out "<h2>Delete</h2>\n";
  out "Do you really want to delete the following files?\n";
  out "<ul>\n";
  List.iter
    (fun name ->
       out (sprintf "<li> %s\n" (text name));
       hidden_field cgi ("sel_" ^ encode_filename name) "ON"
    )
    files;
  out "</ul>\n";
  out "<p><input type=\"submit\" name=\"yes_button\" value=\"Yes\">\n";
  out "<input type=submit name=\"no_button\" value=\"No\">\n";
  end_form cgi;
  end_page cgi



let handle_confirm_delete_page user (cgi:cgi) =
  (* Check which button has been pressed: *)
  let button = pressed_button cgi [ "yes_button"; "no_button" ] in
  if button = "yes_button" then begin
    let files = selected_files cgi in
    List.iter
      (fun name ->
	 Sys.remove (Filename.concat (Filename.concat file_store user) name)
      )
      files
  end;
  `List


(**********************************************************************
 * DOWNLOAD PAGE
 **********************************************************************)

let display_download_page filename user (cgi:cgi) =
  (* Special download code: Because [cgi] has an output buffer, we
     bypass this buffer.  (There is no need for this buffer here.)  *)
  let env = cgi#environment in

  let fname = Filename.concat (Filename.concat file_store user) filename in
  let fh = open_in_bin fname in

  (* Get the size of the file such that we can pass it to the browser
     as content-length. For many browsers, this improves the download
     dialog.  *)
  let size = in_channel_length fh in

  (* Remove critical characters (space, double quotes, backslashes)
     from the filename: *)
  let user_filename =
    Pcre.qreplace ~rex:(Pcre.regexp "[ \\\"\\\\]") ~templ:"_" filename in

  (* Set now the header: *)
  cgi#set_header
    ~content_type:"application/octet-stream"
    ~content_length:size
    ~cache:`No_cache
    ~filename:user_filename
    ();
  env#send_output_header(); (* send the header immediately. *)
  (* Send the file to [env#out_channel], and NOT [cgi#out_channel].
     The difference is that the latter is buffered, and the first is
     unbuffered.  *)
  let ch = new Netchannels.input_channel fh in
  env#out_channel#output_channel ch;
  ch#close_in();
  env#out_channel#close_out()



(**********************************************************************
 * REQUEST BROKER
 *
 * This function calls the various page-specific handlers.
 **********************************************************************)

let main (cgi:cgi) =
  (* The [try] block catches [User_error]. *)
  try
    (* Find out the current user:
       [user] is either the trusted user name or "" (no user)
       [next_cookie] is one of:
       - None: Do not set a cookie
       - Some "": Delete the current cookie
       - Some v:  Set the cookie with value v

       If there is no password file, the web server must authenticate the
       user. In this case, we believe the web server.

       If there is a password file, we do authentication ourselves. There
       are a number of possible cases:
       - There is a cookie, but the CGI argument "cred_logout" = "yes":
         ==> Delete the cookie, and force the login page to be displayed.
       - There is a cookie, but it cannot be verified.  This means that the
         cookie is too old, and the matching key has already been removed
         from the list of valid keys.
         ==> Delete the cookie, and force the login page to be displayed.
       - There is a cookie, and it can be verified.  In this case we trust
         the cookie.  Furthermore, a new cookie is created immediately,
         because the current key may have changed.
       - There is no cookie, but CGI arguments cred_user and cred_password:
         Verify the password.  If it is right, create a new cookie containing
         the user name.  Furthermore, the cookie is signed.
         If the password is incorrect, display the login page again.
       - There is no cookie and no CGI arguments containing the credentials:
         Display the login page.
    *)
    let user, next_cookie =
      match password_file with
      | None ->
	  (* The web server authenticates the user. It is available
	     by the CGI property remote_user: *)
	  (cgi#environment#cgi_remote_user, None)
      | Some _ ->
	  (* Check whether there is a cookie containing the user name: *)
	  let key_list = read_secret_keys() in
	  let ip_addr = cgi#environment#cgi_remote_addr in
	  try
	    if cgi#argument_value "cred_logout" = "yes" then
	      ("", Some "")                       (* delete the cookie *)
	    else
	      let cookie =
		cgi#environment#cookie "credentials" in (* or Not_found *)
	      if Cookie.value cookie = "" then raise Not_found;
	      (match check_cookie key_list ip_addr cookie with
	       | None ->
		   (* The cookie has expired.  The user must
		      authenticate again.  *)
		   ("", Some "")                  (* delete the cookie! *)
	       | Some (user, password) ->
		   (* Create the new cookie: *)
		   let key = current_secret_key key_list in
		   let cookie' = make_cookie user password ip_addr key in
		   (user, Some cookie')
	      )
	  with Not_found ->
	    (* If there are "cred_user" and "cred_password" CGI
	       arguments (from the login screen) use these *)
	    let user = cgi#argument_value "cred_user" in
	    let password = cgi#argument_value "cred_password" in
	    (match user_exists user with
	     | `False -> ("", None)
	     | `Password pw ->
		 if password = pw then begin
		   (* Create the new (first) cookie: *)
		   let key = current_secret_key key_list in
		   let cookie' = make_cookie user password ip_addr key in
		   (user, Some cookie')
		 end
		 else ("", None)
	     | `Passes -> assert false
	    )
    in
    (* Set a header that is perfect for HTML pages. *)
    let cookies =
      match next_cookie with
      | None -> []
      | Some v ->
	  [ Cookie.make "credentials" v
	      ?max_age:(if v = "" then Some 0 else None)
	      (* Expiration is checked by ourselves *)
	      ~secure:cgi#environment#cgi_https
	  ] in
    cgi#set_header
      ~cache:`No_cache
      ~content_type: "text/html; charset=iso-8859-1"
      ~set_cookies:cookies
      ();

    (* If user = "", the user has not yet logged in.  Display the login
       page (or fail, if disabled).  *)
    if user = "" then begin
      if password_file = None then
	failwith "No remote user available (badly configured web server?)";
      display_login_page cgi
    end
    else begin
      (* Call the function that handles the visible page: *)
      let visible_page = cgi#argument_value ~default:"list" "page" in
      let handle = match visible_page with
	| "list"           -> handle_list_page
	| "confirm_delete" -> handle_confirm_delete_page
(*	| "confirm_rename" -> handle_confirm_rename_page *)
	| _ ->
            let msg = sprintf "Page %S not found." visible_page in
            raise(Netcgi_common.HTTP(`Not_found, msg))
      in
      (* and determine the next page to display: *)
      let next_page = handle user cgi in
      let display = match next_page with
	| `List			-> display_list_page
	| `Confirm_delete files -> display_confirm_delete_page files
(*	| `Confirm_rename file	-> () *)
	| `Download file 	-> display_download_page file
      in
      display user cgi
    end;

    (* Commit everything (if channel not closed already): *)
    (try cgi#out_channel#commit_work() with _ -> ());
    cgi#finalize() (* Cleanup *)
  with
    User_error s ->
      (* An error has happened.  Generate now an error page instead of
         the current page.  By rolling back the output buffer, any
         uncomitted material is deleted.  *)
      let out = cgi#out_channel#output_string in
      cgi#out_channel#rollback_work();
      begin_page cgi "Error";
      out "<p class=\"error\">";
      out s;
      out "</p>\n";
      out (sprintf "<a href=\"%s\">Restart</a>\n" (text(cgi#url())));
      end_page cgi;
      cgi#out_channel#commit_work();	(* Commit the error page *)
      cgi#finalize()


let script run =
  (* Our configuration.  We use a different [tmp_directory] to ensure
     that the uploaded files are in the same filesystem as
     [file_store].  *)
  let config = { default_config with
	           tmp_directory = Filename.concat file_store ".tmp"
               } in
  (* We use buffered output here.
     NOTE: For downloaded files, the buffer is bypassed. See the function
     [display_download_page].

     The [~arg_store] type is `File only for the CGI argument "file".
     This argument is used for file uploads.  *)
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  let arg_store _ name _ = if name = "file" then `File else `Memory in
  run ~config ~arg_store ~output_type:(`Transactional buffered)
    (fun cgi -> main(cgi :> cgi))

