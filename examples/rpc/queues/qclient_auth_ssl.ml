(* $Id: qclient_auth_ssl.ml 289 2006-04-30 17:50:29Z gerd $ *)

module C1 = Queues_clnt.QUEUESPROG.QUEUESVERS1 ;;

Ssl.init();;  (* Don't forget! *)

let ctx = Ssl.create_context Ssl.TLSv1 "client.crt" "client.key" ;;

Ssl.set_verify ctx [ Ssl.Verify_peer ] None;
Ssl.set_verify_depth ctx 99;
Ssl.load_verify_locations ctx "ca.crt" "" ;;


let ssl_socket_config =
  Rpc_ssl.ssl_client_socket_config ctx ;;

Qclient.pluggable_auth_module :=
  ( "auth_ssl",
    (fun host ->
       let clnt = C1.create_client2 
	 (`Socket(Rpc.Tcp, 
		  Rpc_client.Portmapped host,
		  ssl_socket_config)) in
       clnt
    )
  )
;;
