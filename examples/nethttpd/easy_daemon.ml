(* A test daemon, only using the kernel *)

open Printf


let generate resp =
  printf "Generating response\n"; flush stdout;

  let h =
    new Netmime.basic_mime_header
      [ "Content-type", "text/html" ] in

  let data =
    "<html>\n" ^
    "  <head><title>Easy Daemon</title></head>\n" ^
    "  <body>\n" ^
    "    <a href='foo'>GET something</a><br>\n" ^ 
    "    <form method=POST encoding='form-data'>\n" ^
    "      <input type=hidden name=sample value='sample'>\n" ^
    "      <input type=submit value='POST something'>\n" ^
    "    </form>\n" ^
    "  </body>\n" ^
    "</html>" in

  resp # send (`Resp_status_line (200, "OK"));
  resp # send (`Resp_header h);
  resp # send (`Resp_body (data, 0, String.length data));
  resp # send `Resp_end
;;


let generate_error resp =
  printf "Generating error response\n"; flush stdout;

  let h =
    new Netmime.basic_mime_header
      [ "Content-type", "text/html" ] in

  let data =
    "<html>\n" ^
    "  <head><title>Bad Request from Easy Daemon</title></head>\n" ^
    "  <body>\n" ^
    "  Bad Request!\n" ^
    "  </body>\n" ^
    "</html>" in

  resp # send (`Resp_status_line (400, "Bad Request"));
  resp # send (`Resp_header h);
  resp # send (`Resp_body (data, 0, String.length data));
  resp # send `Resp_end;
;;

let serve fd =
  let config = Nethttpd_kernel.default_http_protocol_config in
  let proto = new Nethttpd_kernel.http_protocol config fd in

  let rec next_token () =
    if proto # recv_queue_len = 0 then (
      proto # cycle ~block:(-1.0) ();    (* block forever *)
      next_token()
    )
    else
      proto # receive() 
  in

  let cur_tok = ref ( next_token() ) in
  let cur_resp = ref None in
  while !cur_tok <> `Eof do
    ( match !cur_tok with
	| `Req_header  (((meth, uri), v), hdr, resp) ->
	    printf "Request: method = %s, uri = %s\n" meth uri;
	    flush stdout;
	    cur_resp := Some resp
	| `Req_expect_100_continue ->
	    ( match !cur_resp with
		| Some resp -> resp # send Nethttpd_kernel.resp_100_continue
		| None -> assert false
	    )
	| `Req_end ->
	    printf "Pipeline length: %d\n" proto#pipeline_len;
	    ( match !cur_resp with
		| Some resp -> generate resp
		| None -> assert false
	    );
	    cur_resp := None
	| `Fatal_error e ->
	    let name = Nethttpd_kernel.string_of_fatal_error e in
	    printf "Fatal_error: %s\n" name;
	    flush stdout;
	| `Bad_request_error (e, resp) ->
	    let name = Nethttpd_kernel.string_of_bad_request_error e in
	    printf "Bad_request_error: %s\n" name;
	    flush stdout;
	    generate_error resp
	| `Timeout ->
	    printf "Timeout\n";
	    flush stdout;
	| _ ->
	    ()
    );
    cur_tok := next_token()
  done;
  
  (* Send the remaining responses:*)
  while proto # resp_queue_len > 0 do
    proto # cycle ~block:(-1.0) ();
  done;

  proto # shutdown();

  if proto # need_linger then (
    printf "Lingering close!\n";
    flush stdout;
    let lc = new Nethttpd_kernel.lingering_close fd in
    while lc # lingering do
      lc # cycle ~block:true ()
    done
  )
  else
    Unix.close fd
;;

let start() =
  let master_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt master_sock Unix.SO_REUSEADDR true;
  Unix.bind master_sock (Unix.ADDR_INET(Unix.inet_addr_any, 8765));
  Unix.listen master_sock 100;
  printf "Listening on port 8765\n";
  flush stdout;
  
  while true do
    try
      let conn_sock, _ = Unix.accept master_sock in
      Unix.set_nonblock conn_sock;
      serve conn_sock
    with
	Unix.Unix_error(Unix.EINTR,_,_) -> ()  (* ignore *)
  done
;;


let conf_debug() =
  (* Set the environment variable DEBUG to either:
       - a list of Netlog module names
       - the keyword "ALL" to output all messages
       - the keyword "LIST" to output a list of modules
     By setting DEBUG_WIN32 additional debugging for Win32 is enabled.
   *)
  let debug = try Sys.getenv "DEBUG" with Not_found -> "" in
  if debug = "ALL" then
    Netlog.Debug.enable_all()
  else if debug = "LIST" then (
    List.iter print_endline (Netlog.Debug.names());
    exit 0
  )
  else (
    let l = Netstring_str.split (Netstring_str.regexp "[ \t\r\n]+") debug in
    List.iter
      (fun m -> Netlog.Debug.enable_module m)
      l
  );
  if (try ignore(Sys.getenv "DEBUG_WIN32"); true with Not_found -> false) then
    Netsys_win32.Debug.debug_c_wrapper true
;;


Netsys_signal.init();
conf_debug();
start();;
