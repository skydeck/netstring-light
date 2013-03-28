(* netcgi_apache.ml

   Copyright (C) 2005-2007

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

open Netcgi_common
open Printf

let log_error msg =
  let zone = Netdate.localzone in
  prerr_endline
    ("[" ^ Netdate.format "%c" (Netdate.create ~zone (Unix.gettimeofday()))
     ^ "] [Netcgi_apache] " ^ msg)


module Conf = Netcgi_apache_mod.Conf
module Handler = Netcgi_apache_mod.Handler


module Apache =
struct
  (* Here we shadow some external functions of the Apache stub to make
     them safer or respecting Netcgi conventions. *)

  module Table = Netcgi_apache_mod.Raw_Apache.Table
  module Server = Netcgi_apache_mod.Raw_Apache.Server
  module Connection = Netcgi_apache_mod.Raw_Apache.Connection

  module Request = struct (* Request_rec functions. *)
    include Netcgi_apache_mod.Raw_Apache.Request

    (* If the return code [i] is an error, raise HTTP with the message [msg]. *)
    let possible_error msg i : unit =
      (* DECLINED -1, DONE -2, OK 0 *)
      if i > 0 then raise(HTTP(Nethttp.http_status_of_int i, msg))

    let setup_client_block r p =
      possible_error "Netcgi_apache.Apache.Request.setup_client_block"
        (setup_client_block r p)

    let get_client_block r =
      try get_client_block r
      with Failure msg -> raise(HTTP(`Internal_server_error, msg))

    let get_client_block_buf r buf ofs len =
      if ofs < 0 || ofs + len > String.length buf then
        invalid_arg "Netcgi_apache.Apache.Request.get_client_block_buf";
      let r = get_client_block_buffer r buf ofs len in
      if r < 0 then
        raise(HTTP(`Internal_server_error,
                  "Netcgi_apache.Apache.Request.get_client_block_buf"))
      else r

    let discard_request_body r =
      possible_error "Netcgi_apache.Apache.Request.discard_request_body"
        (discard_request_body r)

    let get_basic_auth_pw r =
      let i, pw = get_basic_auth_pw r in
      possible_error "Netcgi_apache.Apache.Request.get_basic_auth_pw" i;
      pw

    let rflush r =
      if rflush r < 0 then raise End_of_file
  end

end (* module Apache -------------------------------------------------- *)


(* CGI and Environment objects ---------------------------------------- *)

open Apache

(** Useful functions *)

(* [rm_htspace s] removes heading and trailing spaces of [s]. *)
let rm_htspace =
  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  fun s -> rm_htspace is_space s 0 (String.length s)

(* Monomorphic version *)
let min (i:int) j = if i <= j then i else j

(* [index_in_range s i0 i1 c] returns the index [j] of the first
   occurrence of [c] in s.[i0 .. i1-1].  If no occurence of [c]
   is found, it will return [i1].  It is assumed that 0 <= i0 and i1
   <= String.length s. *)
let index_in_range s i0 i1 c =
  let rec examine i =
    if i < i1 then
      if String.unsafe_get s i = c then i
      else examine (i+1)
    else i1 in
  examine i0


(************************************************************************)
(** Input channel *)

(* The "stdin" data is here accessible through the "get_client_block"
   function of the [Request.t] type.  We wrap it into a in_object to
   use Netstring parsing functions.

   WARNING: You should NOT create this object if one is not ready to
   read content.  Never create several [in_obj] on the same request.
*)
(* http://www.auburn.edu/docs/apache/misc/client_block_api.html *)

class in_obj (r:Apache.Request.t) : Netchannels.in_obj_channel =
object(self)
  val r = r
  val in_buf = String.create 8192 (* valid: buf.[in0 .. in1-1] *)
  val mutable in0 = 0
  val mutable in1 = 0 (* in1 < 0 indicates the channel is closed *)
  val mutable read = 0 (* number of byte delivered to the user *)

  initializer
    Request.setup_client_block r Request.CHUNKED_ERROR;
    if not(Request.should_client_block r) then (
      (* e.g. for GET request -- should not happen if used correctly *)
      (* XXX Do not terminate the process. *)
      log_error "Netcgi_apache.in_obj: should_client_block returned false \
		for a POST request (ignored)";
(*       failwith "Netcgi_apache.in_obj: should_client_block" *)
    )

  (* [#fill_in_buf] refills [in_buf] if needed (when empty).  After
     this [in0 < in1] or [End_of_file] is raised. *)
  method private fill_in_buf =
    if in0 >= in1 then (
      in0 <- 0;
      in1 <- Request.get_client_block_buf r in_buf 0 8192;
      if in1 = 0 then raise End_of_file;
    )

  method private unsafe_input buf ofs len =
    self#fill_in_buf;
    let r = min len (in1 - in0) in
    String.blit in_buf in0 buf ofs r;
    in0 <- in0 + r;
    read <- read + r;
    r

  (* rec_in_channel *)
  method input buf ofs len =
    if in1 < 0 then raise Netchannels.Closed_channel;
    if ofs < 0 || len < 0 || ofs + len > String.length buf then
      invalid_arg "Netcgi_apache.in_obj#input";
    self#unsafe_input buf ofs len

  method close_in () =
    if in1 < 0 then raise Netchannels.Closed_channel;
    in0 <- 0;
    in1 <- -1

  (* raw_in_channel *)
  method pos_in = read

  (* Convenience methods *)
  method really_input buf ofs len =
    if in1 < 0 then raise Netchannels.Closed_channel;
    if ofs < 0 || len < 0 || ofs + len > String.length buf then
      invalid_arg "Netcgi_apache.in_obj#really_input";
    let ofs = ref ofs
    and len = ref len in
    while !len > 0 do
      let r = self#unsafe_input buf !ofs !len in
      ofs := !ofs + r;
      len := !len - r
    done

  method input_char () =
    if in1 < 0 then raise Netchannels.Closed_channel;
    self#fill_in_buf;
    let c = String.unsafe_get in_buf in0 in
    in0 <- in0 + 1;
    read <- read + 1;
    c

  method input_byte () = Char.code(self#input_char())

  method input_line () =
    if in1 < 0 then raise Netchannels.Closed_channel;
    let line = ref ""
    and i = ref in1 (* to enter the loop *) in
    while !i = in1 (* '\n' not found *) do
      self#fill_in_buf; (* => in0 < in1 *)
      i := index_in_range in_buf in0 in1 '\n';
      let r = !i - in0 in
      line := !line ^ (String.sub in_buf in0 r);
      read <- read + r + 1; (* +1 for '\n' *)
      in0 <- !i + 1; (* skip '\n' *)
    done;
    !line
end


(* When no input is required, we need a dummy object to play the input
   channel role.  The following one will give us appropriate
   information in case of misuse. *)
class dummy_in_obj : Netchannels.in_obj_channel =
object
  method input (_:string) (_:int) (_:int) = invalid_arg "dummy_in_obj"
  method close_in () = invalid_arg "dummy_in_obj"
  method pos_in = invalid_arg "dummy_in_obj"
  method really_input (_:string) (_:int) (_:int) = invalid_arg "dummy_in_obj"
  method input_char () = invalid_arg "dummy_in_obj"
  method input_byte () = invalid_arg "dummy_in_obj"
  method input_line () = invalid_arg "dummy_in_obj"
end



(************************************************************************)
(** Output channel: though output functions of the request *)

class out_obj (r:Apache.Request.t) : Netchannels.out_obj_channel =
object(self)
  val r = r
  val mutable sent = 0
  val mutable closed = false
    (* At the moment, all buffering is left to Apache. *)

  (* rec_out_channel *)
  method output s ofs len =
    if closed then raise Netchannels.Closed_channel;
    let w = Request.output r s ofs len in
    sent <- sent + w;
    w

  method flush () =
    if closed then raise Netchannels.Closed_channel;
    try Request.rflush r
    with _ -> raise(Sys_error "Netcgi_apache#out_channel#flush: EOF")

  method close_out () = (* Apache closes the channel itself *)
    if not closed then (
      closed <- true;
      try
	self#flush();
      with
	| error ->
	    Netlog.logf `Err
	      "Netcgi_apache: Suppressed error in close_out: %s"
	      (Netexn.to_string error);
    )

  (* raw_out_channel *)
  method pos_out = sent

  (* convenience methods *)
  method output_char c =
    if closed then raise Netchannels.Closed_channel;
    Request.print_char r c;
    sent <- sent + 1

  method output_byte i = self#output_char(Char.unsafe_chr(i land 0xFF))

  method private unsafe_really_output s ofs len =
    let len = ref len
    and ntries = ref 0 (* avoid infinite loop *) in
    while !len > 0 && !ntries < 100 do
      let w = Request.output r s ofs !len (* checks bounds *) in
      sent <- sent + w;
      len := !len - w; (* if not = 0, try again *)
      incr ntries;
    done;
    if !len > 0 then failwith "Netcgi_apache#out_channel#really_output"

  method output_string s =
    if closed then raise Netchannels.Closed_channel;
    self#unsafe_really_output s 0 (String.length s)

  method output_buffer b = self#output_string(Buffer.contents b)

  method really_output s ofs len =
    if closed then raise Netchannels.Closed_channel;
    if ofs < 0 || len < 0 || ofs + len > String.length s then
      invalid_arg "Netcgi_apache#out_channel#really_output";
    self#unsafe_really_output s ofs len

  method output_channel ?len in_obj =
    if closed then raise Netchannels.Closed_channel;
    let buf = String.create 8192 in
    match len with
    | None -> (* read till the end *)
	(try
	   while true do
	     let r = in_obj#input buf 0 8192 in
	     if r = 0 then raise Sys_blocked_io;
	     self#unsafe_really_output buf 0 r;
	   done
	 with End_of_file -> ())
    | Some len -> (* read at most that many bytes *)
	(try
	   let len = ref len in
	   while !len > 0 do
	     let r = in_obj#input buf 0 (min 8192 !len) in
	     if r = 0 then raise Sys_blocked_io;
	     self#unsafe_really_output buf 0 r;
	     len := !len - r
	   done
	 with End_of_file -> ())
end


(************************************************************************)
(** Header classes bound to Apache table *)

(* Apache already has a representation for MIME headers so we bind
   directly to them instead of using OcamlNet [Netmime] module.  The
   Apache functions already treat keys case-insensitively as we want.
*)

class mime_header ?(ro=false) table : Netmime.mime_header =
object (self)
  val ro = ro
  val t = table

  (* Get *)

  method ro = ro
  method fields = Apache.Table.fields t
  method field name = Apache.Table.get t name
  method multiple_field name = Apache.Table.get_all t name

  (* Set *)

  method private immutable =
    raise (Netmime.Immutable "Netcgi_apache.mime_header");

  method set_fields h =
    if ro then self#immutable;
    Apache.Table.clear t;
    List.iter (fun (k,v) -> Apache.Table.add t k v) h

  method update_field name v =
    if ro then self#immutable;
    Apache.Table.set t name v

  method update_multiple_field name vl =
    if ro then self#immutable;
    match vl with
    | [] -> Apache.Table.unset t name
    | v :: tl ->
	Apache.Table.set t name v;
	List.iter (fun v -> Apache.Table.add t name v) tl

  method delete_field name =
    if ro then self#immutable;
    Apache.Table.unset t name

  (* Access methods for frequent standard fields *)

  method content_length() =
    int_of_string(rm_htspace(self#field "content-length"))
  method content_type() =
    Mimestring.scan_mime_type_ep (self#field "content-type") []
  method content_disposition() =
    Mimestring.scan_mime_type_ep (self#field "content-disposition") []
  method content_transfer_encoding() =
    String.lowercase(self#field "content-transfer-encoding")
end


(************************************************************************)
(** Environment *)

(* The environment properties does not come from a list that we have
   to parse.  Rather, properties come from various entries in the
   [request_rec] structure, i.e. various functions in the
   [Apache.Request] module.
*)
class cgi_environment ~config r : Netcgi.cgi_environment =
  let path_info = try Request.path_info r with Not_found -> "" in
object(self)
  inherit Netcgi_common.cgi_environment ~config
    ~properties:[] ~input_header:[] (new out_obj r)

  (* Properties settable by Request functions.  Store their initial value. *)
  val script_name =
    let s = try Request.uri r with Not_found -> "" in
    if Filename.check_suffix s path_info then
      Filename.chop_suffix s path_info (* suppress path_info *)
    else s
  val path_info = path_info
  val r_filename = try Request.filename r with Not_found -> ""
  val query_string = try Request.args r with Not_found -> ""


  val input_header = new mime_header ~ro:true (Request.headers_in r)
  val output_header = new mime_header ~ro:false (Request.headers_out r)
    (* XXX Should we use [err_headers_out] because they persist on
       errors and across internal redirect?  What about other connectors? *)
  val r = r (* must be last for the above [r] to be the arg. *)
  val mutable properties_list = lazy(assert false)
  val mutable input_content_type = lazy(assert false)

  initializer
    (* Content-type MSIE bug fix has not been applied; do it now.  *)
    input_content_type <- lazy(
      let work_around =
	List.mem `MSIE_Content_type_bug self#config.workarounds
	&& (try is_MSIE(self#user_agent) with Not_found -> false) in
      let ct = self#input_header_field "content-type" in
      let ct = if work_around then fix_MSIE_Content_type_bug ct else ct in
      Mimestring.scan_mime_type_ep ct []
    );
    (* We probably do not want the list of all properties, so compute
       it on demand only. *)
    properties_list <- lazy (
      let l =
	[ ("GATEWAY_INTERFACE", self#cgi_gateway_interface);
	  ("SERVER_NAME", self#cgi_server_name);
	  ("SERVER_PROTOCOL", self#cgi_server_protocol);
	  ("SERVER_SOFTWARE", self#cgi_server_software);
	  ("SERVER_ADMIN", Server.admin(Request.server r));
	  ("REQUEST_METHOD", self#cgi_request_method);
	  ("SCRIPT_NAME", self#cgi_script_name);
	  ("PATH_INFO", self#cgi_path_info);
	  ("PATH_TRANSLATED", self#cgi_path_translated);
	  ("AUTH_TYPE", self#cgi_auth_type);
	  ("REMOTE_ADDR", self#cgi_remote_addr);
	  ("REMOTE_HOST", self#cgi_remote_host);
	  ("REMOTE_USER", self#cgi_remote_user);
	  ("REMOTE_IDENT", self#cgi_remote_ident);
	  ("QUERY_STRING", self#cgi_query_string);
	  ("SCRIPT_FILENAME", r_filename);
	] in
      let l = match self#cgi_server_port with
	| None -> l
	| Some p -> ("SERVER_PORT", string_of_int(p)) :: l in
      l
    )

  (* CGI properties *)

  method cgi_properties = Lazy.force properties_list
  method cgi_property ?default name =
    try List.assoc name (Lazy.force properties_list)
    with Not_found -> (match default with
    | None -> raise Not_found
    | Some d -> d)

  (* None of the following methods should raise [Not_found].  Return
     "" instead. *)
  method cgi_gateway_interface = Conf.gateway_interface
  method cgi_server_name = try Request.hostname r with Not_found -> ""
  method cgi_server_port = Some(Request.port r)
  method cgi_server_protocol = try Request.protocol r with Not_found -> ""
  method cgi_server_software = Conf.server_software
  method cgi_request_method = try Request.method_name r with Not_found -> ""
    (* Beware that it is used by the [Netcgi_common.cgi_with_args] to
       pass a it to [[Netcgi_common.cgi] to set the [out_channel] to
       an appropriate value. *)
  method cgi_script_name = script_name
  method cgi_path_info = path_info
  method cgi_path_translated = "" (* FIXME: don't know how to get it *)
  method cgi_auth_type = try Request.auth_type r with Not_found -> ""
  method cgi_remote_addr =
    try Connection.remote_ip(Request.connection r) with Not_found -> ""
  method cgi_remote_host =
    try Connection.remote_host(Request.connection r) with Not_found -> ""
  method cgi_remote_user = try Request.user r with Not_found -> ""
  method cgi_remote_ident = "" (* FIXME: is it only supported? *)
  method cgi_query_string = query_string
    (* method protocol is inherited and use [server_protocol]; OK *)
  method cgi_https = false (* FIXME: don't know how to get it *)


  (* Input -- other methods are defined in terms of [input_header], so
     no nedd to override them. *)
  method input_header = input_header

  (* Output -- again other methods are defined in terms of
     [output_header] and do not need to be overridden. *)
  method output_header = output_header

  method send_output_header () =
    if header_not_sent then (
      (* Must treat Status specially *)
      (try
	  let st = int_of_string(self#output_header#field "Status") in
	  Request.set_status r st
        with Not_found | Failure _ -> ());
      (* Must treat Content-Type specially *)
      Request.set_content_type r
	(try self#output_header#field "Content-Type"
	  with Not_found -> "text/html"
	    (* In [Netcgi_common.environment], it was decided to have a
	       default Content-type. *)
	);
      Request.send_http_header r;
      header_not_sent <- false
    )

(* method out_channel is fine as inherited *)

(* method log_error: funny as it is, printing on stderr is fine for
   Apache modules. *)
end


(************************************************************************)
(** CGI abstraction *)

(* Enriched CGI class *)
class type cgi =
object
  inherit Netcgi.cgi
  method request : Apache.Request.t
end

class cgi_mod (r:Request.t) env op request_method args : cgi =
object
  inherit Netcgi_common.cgi env op request_method args  as super

  val r = r
  method request = r
end


exception Error of Nethttp.http_status * string * string
  (* [Error(status, log-msg, user-msg)]: errors raised before the
     handler is created. *)


(* This will be used by the script to set its entry point (function to
   be run) when loaded with [Dynlink]. *)
let script_run = ref None

(* Load a file and return the function to run. *)
let load_file filename =
  script_run := None;
  (try  Dynlink.loadfile_private filename;
   with
   | Dynlink.Error e ->
       raise(Error(`Internal_server_error,
		   filename ^ ": Dynlink.Error: " ^ Dynlink.error_message e,
		   "Couldn't load the script."))
   | e -> raise(Error(`Internal_server_error,
		      filename ^ ": " ^ Netexn.to_string e,
		      "Couldn't load the script.")));
  (* If the file was loaded and thus the toplevel code executed,
     [current_script] should now be [Some handler]. *)
  match !script_run with
  | None ->
      raise(Error(`Not_found,
		  "You must register your main function with Netcgi_apache.run!",
		  "No entry point in the script."))
  | Some script -> script


(* Tells wich function of the script is the one to run on each
   request.  This is to be only called 1 time per script.  Exceptions
   will be dealt with in the handler (see below).  *)
let run ?(config=Netcgi.default_config)
    ?(output_type=(`Direct "" : Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic : Netcgi.arg_store))
    ?(exn_handler=(fun _ f -> f()))
    f =
  if !script_run <> None then
    raise(Error(`Internal_server_error,
	       "Netcgi_apache.run should only be called once!",
	       "Multiple entry points in the script."));
  let script r =
    (* Netcgi_apache specific environment -- including output object *)
    let env = new cgi_environment ~config r in
    exn_handler_default env ~exn_handler
      (fun () ->
        let in_obj =
	  match env#cgi_request_method with
	  | "POST" | "PUT" ->
	      new in_obj r (* => only created if necessary *)
	  | _ ->
	      Request.discard_request_body r;
	      new dummy_in_obj (* raise an exception if used *)
        in
        let cgi =
	  cgi_with_args (new cgi_mod r) env output_type in_obj arg_store in
        (try
	    f cgi;
            cgi#out_channel#commit_work();
	    cgi#finalize()
          with e when config.default_exn_handler ->
            cgi#finalize(); raise e);
        None (* no "special" internal exception *)
      )
      ~finally:(fun () ->
        (try env#out_channel#close_out() with _ -> ()); (* flush *)
      );
    Handler.OK
  in
  (* Register the main function *)
  script_run := Some script




(************************************************************************)
(** Registry *)

(* This part is loosely based on mod_caml Registry *)


(* This is an error page for the exceptions launched before the
   environment object can be created. *)
let error_page r status log_msg user_msg =
  (* Log the error *)
  let code = Nethttp.int_of_http_status status in
  log_error(Printf.sprintf  "%s (Status %i)" log_msg code);
  (* Display an error page *)
  Request.set_status r code;
  Request.set_content_type r "text/html";
  let t = Request.headers_out r in
  Apache.Table.set t "Cache-control" "max-age=3600";
  Apache.Table.add t "Cache-control" "must-revalidate";
  let secs = Netdate.mk_mail_date(Unix.time() +. 3600.) in
  Apache.Table.set t "Expires" secs;
  Request.send_http_header r;
  if not(Request.header_only r) then (
    let out s = ignore(Request.print_string r s) in
    out "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \
	\"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
    out "<html xmlns=\"http://www.w3.org/1999/xhtml\" \
	xml:lang=\"en\" lang=\"en\">
<head>
<meta name=\"generator\" content=\"OCamlNet (http://ocamlnet.sf.net)\">
<style>
  p.msg { color: black; background-color: #cccccc; padding: 1ex; }
  h2 { font-size: large; }
</style>
</head>
<body>\n";
    let info = Nethttp.string_of_http_status status in
    out ("<h1>Netcgi_apache &mdash; " ^ info ^ "</h1>\n");
    out "<p class=\"msg\">";
    out user_msg;
    out "</p>\n";
    out "<p>Additional information is available in the web server \
	error log file.</p>\n";
    out "</body>\n</html>\n";
    ignore(Request.rflush r)
  )


(* Table of scripts we have already loaded.  Maps filename ->
   (handler, mtime) so we can reload the file if the mtime has
   changed.  *)
let loaded_scripts = Hashtbl.create 32


(* Handler for requests. *)
let handler r =
  try
    (* Without [filename] or [mtime] we cannot do anything so we
       decline the request. *)
    let filename =
      try Apache.Request.filename r
      with Not_found ->	raise(Error(`Not_found,
				    "Request.filename not found!",
				    "No such script.")) in
    let mtime =
      match Apache.Request.finfo r with
      | Some finfo -> finfo.Unix.st_mtime
      | None ->
	  raise(Error(`Not_found,
		      sprintf "Request.finfo not found.  It probably means \
			that the file %S does not exist." filename,
		      "No such script.")) in
    (* Get the script main function for this file. *)
    let script =
      try
	let (old_handler, old_mtime) = Hashtbl.find loaded_scripts filename in
	if old_mtime < mtime then (
          (* Reload the file. *)
	  let handler = load_file filename in
	  Hashtbl.replace loaded_scripts filename (handler, mtime);
	  handler
	) else
	  old_handler
      with Not_found ->
	(* Load the file for the first time. *)
	let handler = load_file filename in
	Hashtbl.add loaded_scripts filename (handler, mtime);
	handler in

    (* Run the script. *)
    script r
  with Error(status, log_msg, user_msg) ->
    error_page r status log_msg user_msg;
    (* Returning [Handler.DECLINED] is not fine.  Indeed, Apache then
       tries his own handler and then propose to download the script
       -- this is a security problem! *)
    Handler.DONE


let () =
  (* We are in the same module than the Handler (no bytecode to load),
     so specify the full module name to use. *)
  Handler.register handler "bytecode"
