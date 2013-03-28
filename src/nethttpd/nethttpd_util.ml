(* $Id: nethttpd_util.ml 1410 2010-02-14 19:44:28Z gerd $ *)

open Nethttpd_types
open Printf

let std_error_response p = 
  let b = Buffer.create 500 in
  bprintf b "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \
           \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
  bprintf b 
    "<html xmlns=\"http://www.w3.org/1999/xhtml\" \
           xml:lang=\"en\" lang=\"en\">\010\
    <head>\010\
    <meta name=\"generator\" \
          content=\"OCamlNet \
                   (http://projects.camlcity.org/projects/ocamlnet)\">\010\
    <style>\010\
      p.msg { color: black; background-color: #cccccc; padding: 1ex; }\010\
      h2 { font-size: large; }\010\
    </style>\010\
    </head>\010\
    <body>\n";
  let code = p # response_status_code in
  let text =
    try Nethttp.string_of_http_status(Nethttp.http_status_of_int code)
    with Not_found -> "Non-standard code" in
  bprintf b "<h1>Error %i - %s</h1>\n" code text;
  bprintf b "The server could not process your request.\n";
  bprintf b "For additional information consult the error log of the\n";
  bprintf b "server.\n";
  bprintf b "<hr>\n";
  bprintf b "%s - Ocamlnet Nethttpd server\n"
    (Netdate.format "%c"
       (Netdate.create ~zone:Netdate.localzone
	  (Unix.time())));
  bprintf b "</body>\n";
  bprintf b "</html>\n";
  Buffer.contents b


let std_error_log_string p msg =
  let peeraddr_opt = 
    try Some(p # remote_socket_addr) with Not_found -> None in
  Printf.sprintf "[%s] [%s] %s"
    ( match peeraddr_opt with
	| Some addr ->
	    Netsys.string_of_sockaddr addr
	| None ->
	    "-"
    )
    ( try
	let m = p # request_method in
	let u = p # request_uri in
	m ^ " " ^ u
      with Not_found ->
	"-"
    )
    msg


let std_access_log_string p =
  let code =
    p # response_status_code in
  let peerstr =
    try Netsys.string_of_sockaddr p#remote_socket_addr
    with Not_found -> "-" in
  let meth =
    try p#request_method
    with Not_found -> "-" in
  let uri =
    try p#request_uri 
    with Not_found -> "-" in
  let user =
    try List.assoc "REMOTE_USER" p#cgi_properties
    with Not_found -> "-" in
  let respsize =
    p#output_body_size in
  let referrer =
    try p#input_header#field "Referer" 
    with Not_found -> "-" in
  let user_agent =
    try p#input_header#field "User-agent" 
    with Not_found -> "-" in
  Printf.sprintf "%s %s %s \"%s\" %d %Ld \"%s\" \"%s\""
    peerstr
    user
    meth
    (String.escaped uri)
    code
    respsize
    (String.escaped referrer)
    (String.escaped user_agent)


let std_debug_access_log_string p =
  let b = Buffer.create 500 in
  let b_ch = new Netchannels.output_buffer b in
  Printf.bprintf b "%s\n" (std_access_log_string p);
  Printf.bprintf b "Request header:\n";
  ( try
      Mimestring.write_header ~soft_eol:"\n" ~eol:"\n" b_ch p#input_header#fields
    with Not_found ->
      Printf.bprintf b "(missing)\n";
  );
  ( try
      Printf.bprintf b "Request body size: %Ld\n" p#input_body_size
    with Not_found ->
      Printf.bprintf b "Request body size: (missing)\n"
  );
  Printf.bprintf b "Request body rejected: %b\n\n" p#request_body_rejected;
  Printf.bprintf b "CGI properties:\n";
  ( try
      Mimestring.write_header ~soft_eol:"\n" ~eol:"\n" b_ch p#cgi_properties
    with Not_found ->
       Printf.bprintf b "(missing)\n";
  );
  Printf.bprintf b "Response header (code %d):\n" p#response_status_code;
  Mimestring.write_header ~soft_eol:"\n" ~eol:"\n" b_ch p#output_header#fields;
  Printf.bprintf b "Response body size: %Ld\n" p#output_body_size;
  Buffer.contents b
