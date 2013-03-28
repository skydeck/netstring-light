(* Advanced example for event-based engines. When receiving a HTTP request, the engine
 * connects to a remote TCP service, reads all data and forwards it to the HTTP client.
 * E.g. add to /etc/inetd.conf:
 *   99 stream tcp nowait nobody /bin/ps ps -ef
 * Then port 99 outputs a process listing.
 *
 * Use the URL /?host=<ip_address>&port=<port>
 *
 * You can also request arbitrary files: /?file=<filename>
 *
 * The whole example is realized with event-based I/O.
 *)

open Printf

let error_service_connection (cgi : Netcgi.cgi_activation) host port finish err =
  (* This is called when the connection with the proxied service could not be
   * established.
   *)
  printf "Error while connecting to service\n";
  flush stdout;
  cgi # set_header
    ~status: `Internal_server_error
    ~content_type:"text/plain"
    ();
  cgi # output # output_string (sprintf "Could not connect to host <%s> port <%d>: %s\n"
				    host port (Printexc.to_string err));
  finish()
;;


let on_service_connection env (cgi : Netcgi.cgi_activation) host port finish ues conn =
  (* The connection with the proxied service is established. Now just copy all data
   * from the descriptor to the asynchronous output channel in [env].
   *)
  let fd = Uq_engines.client_socket conn in
  (* Some versions of Equeue have a bug in the service connector. So check first
   * whether we are really connected.
   *)
  let cont =
    ( try ignore(Unix.getpeername fd); true
      with err ->
	error_service_connection cgi host port finish err; false
    ) in
  if cont then (
    printf "Connected to service, copying data\n";
    flush stdout;
    (* But first set the output header: *)
    cgi # set_header
      ~status: `Ok
      ~content_type:"text/plain"
      ();
    let copy_engine = new Uq_engines.receiver ~src:fd ~dst:env#output_ch_async ues in
    (* When the [copy_engine] is done, call [finish]: *)
    Uq_engines.when_state ~is_done:(fun _ -> finish())
                          ~is_error:(fun _ -> finish())
                            (* The error is not reported to anybody... *)
                          copy_engine;
    ()
  )
;;

let error_file_service (cgi : Netcgi.cgi_activation) filename finish err =
  printf "Error opening file\n";
  flush stdout;
  cgi # set_header
    ~status: `Internal_server_error
    ~content_type:"text/plain"
    ();
  cgi # output # output_string (sprintf "Could open file <%s> for reading: %s\n"
				    filename (Printexc.to_string err));
  finish()
;;

let setup_file_service env (cgi : Netcgi.cgi_activation) filename finish ues =
  try
    let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
    Unix.set_nonblock fd;
     (* First set the output header: *)
    cgi # set_header
      ~status: `Ok
      ~content_type:"text/plain"
      ();
    (* Then copy data from file to the output channel *)
    let copy_engine = new Uq_engines.receiver ~src:fd ~dst:env#output_ch_async ues in
    (* When the [copy_engine] is done, call [finish]: *)
(*
    let file_finish() =
      ( try Unix.close fd with _ -> ());
      finish() 
    in
 *)
    Uq_engines.when_state ~is_done:(fun _ -> finish())
                          ~is_error:(fun _ -> finish())
                            (* The error is not reported to anybody... *)
                          copy_engine;
    ()
  with
      err ->
	error_file_service cgi filename finish err
;;

let on_request ues notification =
  (* This function is called when the full HTTP request has been received. We set up
   * another engine to connect to the remote service, and copy data from this service
   * to our output channel.
   *)
  printf "Received HTTP request\n";
  flush stdout;
  ( try
      let env = notification # environment in
      (* Create [cgi], and check CGI arguments. Mode is [`Direct], i.e. there is no
       * output buffering by [cgi]. This is important for us because this means that
       * [cgi # output] and [env # output_ch_async] are just the same channel - we
       * are going to use the latter for our gateway.
       *)
      let cgi =
	Netcgi_common.cgi_with_args 
	  (new Netcgi_common.cgi)
	  (env :> Netcgi.cgi_environment)
	  (`Direct "")
	  env#input_channel
	  (fun _ _ _ -> `Automatic) in
      let file = try Some( (cgi # argument "file")#value ) with Not_found -> None in
      let host = cgi # argument_value ~default:"127.0.0.1" "host" in
      let port = int_of_string (cgi # argument_value ~default:"37" "port") in
                 (* 37 = time service, often available *)
      ( match file with
	  | None ->
	      (* Now connect to this service *)
	      let sockspec = `Sock_inet(Unix.SOCK_STREAM, 
					Unix.inet_addr_of_string host, port) in
	      let opts = Uq_engines.default_connect_options in
	      let conn_engine = Uq_engines.connector (`Socket(sockspec,opts)) ues in
	      let fin = notification # schedule_finish in
	      Uq_engines.when_state 
		~is_done:(on_service_connection env cgi host port fin ues)
                  ~is_error:(error_service_connection cgi host port fin)
                  conn_engine;
	  | Some filename ->
	      let fin = notification # schedule_finish in
	      setup_file_service env cgi filename fin ues
      )

    with
	e ->
	  printf "Uncaught exception: %s\n" (Printexc.to_string e);
           flush stdout;
	  notification # schedule_finish()
  );
;;

let on_request_header ues 
                      (notification : Nethttpd_engine.http_request_header_notification) =
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
  notification # schedule_accept_body ~on_request:(on_request ues) ()
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
    new Nethttpd_engine.http_engine ~on_request_header:(on_request_header ues) () 
                                    config fd ues in
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
