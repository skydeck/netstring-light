(* $Id: rpc_ssl.mli 289 2006-04-30 17:50:29Z gerd $ *)

(** Securing RPC by SSL
  *
  * The following configuration objects allow it to encapsulate a
  * stream with SSL. Note that this is only possible with stream
  * sockets.
 *)

(** {1 Clients}
  *
  * SSL is only supported for the new [Rpc_client.create2] interface.
  * If you are using [ocamlrpcgen] look for the generated 
  * [create_client2] functions.
  *
  * Use [`Socket(Rpc.Tcp, connector, (Rpc_ssl.ssl_client_socket_config ctx))]
  * as [mode2] argument for the [create2] function. The [connector] can
  * be any supported connector. Pass the SSL context as [ctx].
 *)

class ssl_client_socket_config : Ssl.context -> Rpc_client.socket_config
  (** SSL configuration class for clients *)

val ssl_client_socket_config : Ssl.context -> Rpc_client.socket_config
  (** SSL configuration object for clients *)

(** {1 Servers}
  *
  * SSL is only supported for the new [Rpc_server.create2] interface.
  * If you are using [ocamlrpcgen] new-style functions are generated
  * when you pass the -srv2 argument.
  *
  * Use [`Socket(Rpc.Tcp, connector, (Rpc_ssl.ssl_server_socket_config ctx))]
  * as [mode2] argument for the [create2] function. The [connector] can
  * be any supported connector. Pass the SSL context as [ctx].
 *)

class ssl_server_socket_config : 
        ?get_peer_user_name:(Ssl.context -> Ssl.socket -> string option) ->
        Ssl.context -> Rpc_server.socket_config
  (** SSL configuration class for servers *)

val ssl_server_socket_config : 
        ?get_peer_user_name:(Ssl.context -> Ssl.socket -> string option) ->
        Ssl.context -> Rpc_server.socket_config
  (** SSL configuration object for servers *)
