(* netcgi_test.ml

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(* $Id: netcgi_test.ml,v 1.8 2005/11/04 00:14:54 chris_77 Exp $ *)

open Netcgi_common
open Printf

(* Default values of arguments *)
let arguments = ref [] 	(* simple arguments *)
let fileargs = ref []	(* file uploads; (name, file, mimetype, filename) *)
let request_method = ref(`GET:request_method)
  (* The request method is constructed from the arguments on the
     command line. *)

let props = ref [	(* environment properties *)
  "GATEWAY_INTERFACE",	"CGI/1.1";
  "SERVER_SOFTWARE",	"Netcgi_test";
  "SERVER_NAME",	"localhost";
  "SERVER_ADMIN",	"webmaster@localhost";
  "SERVER_PROTOCOL",	"HTTP/1.0";
  "SERVER_ADDR",	"127.0.0.1";
  "SERVER_PORT",	"80";
  "SCRIPT_NAME",	Sys.executable_name;
  "REMOTE_HOST",	"localhost";
  "REMOTE_ADDR",	"127.0.0.1";
]

let input_header = ref [
  ("accept",		"*/*");
  ("user-agent",	"Netcgi_test");
]
let out_file = ref ""	(* "" = stdout *)


let is_space c = c = ' ' || c = '\t'
let rm_htspace =
  Netcgi_common.rm_htspace (fun c -> c = ' ' || c = '\t')

let split_name_val option s =
  try
    let i = String.index s '=' in
    (rm_htspace s 0 i, rm_htspace s (i+1) (String.length s))
  with Not_found ->
    prerr_endline(option ^ " argument \"" ^ s
		  ^ "\" must be of the form name=value");
    exit 1

(* Add a (name, _) to the ref list [l], removing duplicate names *)
let add ((name, _) as a) l =
  l := a :: List.filter (fun (n,_) -> n <> name) !l

(* [arg_of_file (name, file, mimetype, filename)] if [file] exists,
   make a MIME argument [a] out of it (with [name], [mimetype] and
   [filename] attributes) and return [Some a].  Otherwise return
   [None].  *)
let arg_of_file (name, file, mimetype, filename) =
  if Sys.file_exists file then (
    let cd = Buffer.create 80 in
    let cd_ch = new Netchannels.output_buffer cd in
    cd_ch#output_string "form-data";
    Mimestring.write_value cd_ch
      (Mimestring.param_tokens [
	 ("name", Mimestring.mk_param name);
	 ("filename", Mimestring.mk_param filename) ]);
    let hdr = new Netmime.basic_mime_header
      [ "content-disposition", Buffer.contents cd;
	"content-type", mimetype; ] in
    (* ~fin:false because files passed to this script are existing
       files and we do not want them to be deleted by #finalize. *)
    let body = new Netmime.file_mime_body ~fin:false file in
    Some(Netcgi.Argument.mime ~name (hdr, `Body body))
  )
  else (
    eprintf "File argument \"%s=%s\" has been ignored (%S does not exists).\n"
      name file file;
    flush stderr;
    None
  )


let script_args =
  let set v x = Arg.Unit(fun () -> v := x) in
  let mimetype = ref "text/plain" in
  let filename = ref None in
  Arg.align [
    ("-get", set request_method `GET, " Set the method to GET (the default)");
    ("-head", set request_method `HEAD, " Set the method to HEAD");
    ("-post", set request_method `POST, " Set the method to POST");
    ("-put", Arg.String
       (fun fname ->
	  match arg_of_file("BODY", fname, "application/octet-stream", fname)
	  with
	  | None ->
	      let a = (new simple_arg "BODY" "INVALID" :> cgi_argument) in
	      request_method := `PUT(a)
	  | Some a -> request_method := `PUT(a)),
     "file Set the method to PUT with the file as argument");
    ("-delete", set request_method `DELETE, " Set the method to DELETE");

    ("-mimetype", Arg.Set_string mimetype, "type \
     Set the MIME type for the next file argument(s) (default: text/plain)");
    ("-filename",
     Arg.String (fun s -> filename := (if s = "" then None else Some s)),
     "path Set the filename property for the next file argument(s)");
    ("-filearg", Arg.String
       (fun s ->
	  let (name, file) = split_name_val "-filearg" s in
	  let filename = match !filename with
	    | None -> file (* default filename *)
	    | Some f -> f in
	  fileargs := (name, file, !mimetype, filename) :: !fileargs),
     "name=file Specify a file argument whose contents are in the file");

    ("-user", Arg.String(fun user ->
			   props := ("REMOTE_USER", user)
			         :: ("AUTH_TYPE", "basic") :: !props),
     "name Set REMOTE_USER to this name");
    ("-prop", Arg.String(fun s -> add (split_name_val "-prop" s) props),
     "name=value Set the environment property");
    ("-header", Arg.String(fun s ->
			     add (split_name_val "-header" s) input_header),
     "name=value Set the request header field");
    ("-o", Arg.Set_string out_file,
     "file Set the output file (default: stdout)");
  ]

let anonymous_args s =
  arguments := split_name_val "anonymous" s :: !arguments

let usage =
  (Filename.basename Sys.executable_name)
  ^ " [options] name1=value1 ... nameN=valueN"




let rec remove_invalid_args = function
  | [] -> []
  | None :: tl -> remove_invalid_args tl
  | (Some a) :: tl -> a :: remove_invalid_args tl


let run ?(config=Netcgi.default_config)
    ?(output_type=(`Direct "":Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic))
    ?(args=[])
    f =
  Arg.parse script_args anonymous_args usage;
  (* Insert the request method in the properties *)
  add ("REQUEST_METHOD", string_of_request_method !request_method) props;
  (* Put arguments in the order they were entered *)
  arguments := List.rev !arguments;
  fileargs := List.rev !fileargs;
  (* Add a query string for good measure (we will not use it ourselves) *)
  (match !request_method with
   | `GET
   | `HEAD ->
       let arguments =
	 !arguments @ (List.map (fun a -> (a#name, a#value)) args) in
       add ("QUERY_STRING", Netencoding.Url.mk_url_encoded_parameters arguments) props
   | _ -> ());

  (* Output channel *)
  let out_obj = new Netchannels.output_channel (
    if !out_file = "" || !out_file = "-" then stdout
    else open_out !out_file) in

  (* Environment object *)
  let env = new cgi_environment ~config ~properties:!props
    ~input_header:!input_header out_obj in

  (* Create the arguments according to the request method *)
  let args =
    (List.map (fun (n,v) -> Netcgi.Argument.simple n v) !arguments)
    @ args in
  let args =
    match !request_method with
    | `POST ->
	args @ remove_invalid_args(List.map arg_of_file !fileargs)
    | _ ->
	if !fileargs <> [] then
	  prerr_endline "Warning: Ignoring -filearg arguments (needs -post).";
	args  in
  (* Let exceptions be visible in the shell -- do not catch them *)
  let cgi = new cgi env output_type !request_method args in
  f cgi;
  (try env#out_channel#close_out() with _ -> ()) (* & flush *)
