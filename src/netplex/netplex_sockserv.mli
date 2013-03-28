(* $Id: netplex_sockserv.mli 1302 2009-11-30 23:09:36Z gerd $ *)

(** Socket service creation
  *
  * A socket service object is an encapsulation of a user-defined processor
  * for a list of sockets.
 *)

open Netplex_types

val create_socket_service :
      processor ->
      socket_service_config ->
        socket_service
  (** Create the socket service (usually only internally used) *)

val create_server_socket : string -> protocol -> extended_address -> 
                              Unix.file_descr
  (** [create_server_socket service_name proto addr]: Creates a server socket
      as specified in [proto] for the address [addr] and returns it.

      Addresses of type [`Container] are not supported.
   *)

val close_server_socket : Unix.file_descr -> unit
  (** Closes a socket as opened with [create_server_socket] *)

val any_file_client_connector : string -> Rpc_client.connector
  (** Interprets a file name as connector for a local RPC service. The
      file must either be a Unix Domain socket, or it must be a text
      file as written by Netplex with the details of the service
      endpoint.
   *)

val client_connector : extended_address -> Rpc_client.connector
  (** Returns the RPC client connector for this Netplex address *)
