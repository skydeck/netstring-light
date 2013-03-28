(* $Id: qserver_auth_dh.ml 286 2006-04-29 16:21:42Z gerd $ *)

(* Configure qserver for Diffie-Hellman authentication *)

Qserver.pluggable_auth_module :=
  ( "auth_dh",
    (`Socket(Rpc.Tcp, Rpc_server.Portmapped, Rpc_server.default_socket_config)),
    (fun srv ->
       Rpc_server.set_auth_methods
	 srv
	 [ Rpc_auth_dh.server_auth_method() ]
    )
  )
;;
