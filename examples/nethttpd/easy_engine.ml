(* Simple example for event-based engines *)

open Printf

let generate (cgi : Netcgi.cgi_activation) =
  (* A Netcgi-based content provider *)
  cgi # set_header
    ~cache:`No_cache
    ~content_type:"text/html; charset=\"iso-8859-1\""
    ();
  let data =
    "<html>\n" ^
    "  <head><title>Easy Engine</title></head>\n" ^
    "  <body>\n" ^
    "    <a href='foo'>GET something</a><br>\n" ^
    "    <form method=POST encoding='form-data'>\n" ^
    "      <input type=hidden name=sample value='sample'>\n" ^
    "      <input type=submit value='POST something'>\n" ^
    "    </form>\n" ^
    "  </body>\n" ^
    "</html>" in
  cgi # output # output_string data;
  cgi # output # commit_work();
;;

let on_request notification =
  (* This function is called when the full HTTP request has been received. For
   * simplicity, we create a [std_activation] to serve the request.
   *
   * An advanced implementation could set up further notifications to get informed
   * whenever there is space in the response buffer for additional output.
   * Currently, data is fully buffered (first
   * in the transactional buffer, then in the response buffer), and only when
   * the message is complete, the transmission to the client starts. 
   * By generating only the next part of the response when there is space in
   * the response buffer, the advanced implementation can prevent that the
   * buffers become large.
   *)
  printf "Received HTTP request\n";
  flush stdout;
  ( try
      let env = notification # environment in
      let cgi =
	Netcgi_common.cgi_with_args 
	  (new Netcgi_common.cgi)
	  (env :> Netcgi.cgi_environment)
	  Netcgi.buffered_transactional_outtype
	  env#input_channel
	  (fun _ _ _ -> `Automatic) in
      generate cgi;
    with
	e ->
	  printf "Uncaught exception: %s\n" (Printexc.to_string e);
          flush stdout
  );
  notification # schedule_finish()
;;

let on_request_header (notification : Nethttpd_engine.http_request_header_notification) =
  (* After receiving the HTTP header: We always decide to accept the HTTP body, if any
   * is following. We do not set up special processing of this body, it is just
   * buffered until complete. Then [on_request] will be called.
   *
   * An advanced server could set up a further notification for the HTTP body. This
   * additional function would be called whenever new body data arrives. (Do so by
   * calling [notification # environment # input_ch_async # request_notification].)
   *)
  printf "Received HTTP header\n";
  flush stdout;
  notification # schedule_accept_body ~on_request ()
;;

let serve_connection ues fd =
  (* Creates the http engine for the connection [fd]. When a HTTP header is received
   * the function [on_request_header] is called.
   *)
  printf "Connected\n";
  flush stdout;
  let config = Nethttpd_engine.default_http_engine_config in
  Unix.set_nonblock fd;
  let http_engine = 
    new Nethttpd_engine.http_engine ~on_request_header () config fd ues in
  ()
;;

let rec accept ues srv_sock_acc =
  (* This function accepts the next connection using the [acc_engine]. After the
   * connection has been accepted, it is served by [serve_connection], and the
   * next connection will be waited for (recursive call of [accept]). Because
   * [server_connection] returns immediately (it only sets the callbacks needed
   * for serving), the recursive call is also done immediately.
   *)
  let acc_engine = srv_sock_acc # accept() in
  Uq_engines.when_state ~is_done:(fun (fd,fd_spec) ->
			        if srv_sock_acc # multiple_connections then (
			          serve_connection ues fd;
			          accept ues srv_sock_acc
                                   ) else 
				  srv_sock_acc # shut_down())
                        ~is_error:(fun _ -> srv_sock_acc # shut_down())
                        acc_engine;
;;

let start() =
  (* We set up [lstn_engine] whose only purpose is to create a server socket listening
   * on the specified port. When the socket is set up, [accept] is called.
   *)
  printf "Listening on port 8765\n";
  flush stdout;
  let ues = Unixqueue.create_unix_event_system () in
  (* Unixqueue.set_debug_mode true; *)
  let opts = { Uq_engines.default_listen_options with
		 Uq_engines.lstn_backlog = 20;
		 Uq_engines.lstn_reuseaddr = true } in
  let lstn_engine =
    Uq_engines.listener
      (`Socket(`Sock_inet(Unix.SOCK_STREAM, Unix.inet_addr_any, 8765) ,opts)) ues in
  Uq_engines.when_state ~is_done:(accept ues) lstn_engine;
  (* Start the main event loop. *)
  Unixqueue.run ues
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
