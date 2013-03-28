(* $Id: rpc_ssl.ml 1672 2011-09-23 09:58:59Z gerd $ *)

class  ssl_client_socket_config ctx : Rpc_client.socket_config =
object(self)
  inherit Rpc_client.default_socket_config

  method multiplexing ~close_inactive_descr prot fd esys =
    if prot <> Rpc.Tcp then
      new Uq_engines.epsilon_engine
	(`Error (Failure "SSL encapsulation is only possible for stream sockets"))
	esys
    else
      let sockname = `Sockaddr(Unix.getsockname fd) in
      let peername = 
	try `Sockaddr(Netsys.getpeername fd) with _ -> `Implied in
      let ssl_mplex =
	Uq_ssl.create_ssl_multiplex_controller 
	  ~close_inactive_descr:true
	  ~preclose:(fun () -> Netlog.Debug.release_fd fd)
	  fd
	  ctx
	  esys in
      new Uq_engines.map_engine
	~map_done:(fun () ->
		     let rpc_mplex =
		       new Rpc_transport.stream_rpc_multiplex_controller
			 sockname
			 peername
			 None
			 (Some fd)
			 (ssl_mplex :> Uq_engines.multiplex_controller)
			 esys in
		     `Done rpc_mplex
		  )
	~map_error:(fun err ->
		      ssl_mplex # inactivate();
		      `Error err)
	~map_aborted:(fun () ->
			ssl_mplex # inactivate();
			`Aborted)
	(Uq_ssl.ssl_connect_engine ssl_mplex)
end


let ssl_client_socket_config ctx =
  new ssl_client_socket_config ctx


class  ssl_server_socket_config ?(get_peer_user_name = fun _ _ -> None)
          ctx : Rpc_server.socket_config =
object(self)
  inherit Rpc_server.default_socket_config

  method multiplexing ~close_inactive_descr prot fd esys =
    if prot <> Rpc.Tcp then
      new Uq_engines.epsilon_engine
	(`Error (Failure "SSL encapsulation is only possible for stream sockets"))
	esys
    else
      let sockname = `Sockaddr(Unix.getsockname fd) in
      let peername = `Sockaddr(Unix.getpeername fd) in
      let ssl_mplex =
	Uq_ssl.create_ssl_multiplex_controller 
	  ~close_inactive_descr:true
	  ~preclose:(fun () -> Netlog.Debug.release_fd fd)
	  fd
	  ctx
	  esys in
      new Uq_engines.map_engine
	~map_done:(fun () ->
		     let peer_user_name_opt = 
		       get_peer_user_name ctx (ssl_mplex # ssl_socket) in
		     let rpc_mplex =
		       new Rpc_transport.stream_rpc_multiplex_controller
			 sockname
			 peername
			 peer_user_name_opt
			 (Some fd)
			 (ssl_mplex :> Uq_engines.multiplex_controller)
			 esys in
		     `Done rpc_mplex
		  )
	~map_error:(fun err ->
		      ssl_mplex # inactivate();
		      `Error err)
	~map_aborted:(fun () ->
			ssl_mplex # inactivate();
			`Aborted)
	(Uq_ssl.ssl_accept_engine ssl_mplex)
end


let ssl_server_socket_config ?get_peer_user_name ctx =
  new ssl_server_socket_config ?get_peer_user_name ctx
