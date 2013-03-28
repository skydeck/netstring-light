(* $Id: qserver_auth_ssl.ml 289 2006-04-30 17:50:29Z gerd $ *)

(* Configure qserver for SSL authentication (UNSAFE) *)

Ssl.init();;  (* Don't forget! *)

let ctx = Ssl.create_server_context Ssl.TLSv1 "server.crt" "server.key" ;;

Ssl.set_verify ctx [ Ssl.Verify_peer; Ssl.Verify_fail_if_no_peer_cert ] None;
Ssl.set_verify_depth ctx 99;
Ssl.load_verify_locations ctx "ca.crt" "" ;;


let ssl_socket_config =
  Rpc_ssl.ssl_server_socket_config 
    ~get_peer_user_name:(fun ctx sslsock -> 
			   prerr_endline "get_peer_user_name";
			   let cert = Ssl.get_certificate sslsock in
			   let user = Ssl.get_subject cert in
			   prerr_endline ("user=" ^ user);
			   Some user)
    ctx ;;

Qserver.pluggable_auth_module :=
  ( "auth_ssl",
    (`Socket(Rpc.Tcp, Rpc_server.Portmapped, ssl_socket_config)),
    (fun srv ->
       Rpc_server.set_auth_methods
	 srv
	 [ Rpc_server.auth_transport ]
    )
  )
;;
