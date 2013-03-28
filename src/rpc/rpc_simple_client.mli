(* $Id: rpc_simple_client.mli 279 2006-04-24 14:45:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Synchronous API for RPC clients *)

(** This is a synchronous client that suffices for most applications
 * of RPC.
 *)

open Xdr

type t
  (** The type of simple clients *)

val create : Rpc_client.connector -> Rpc.protocol -> Rpc_program.t -> t
  (** Create a simple client that connects with the given server using
   * the given protocol type. The program argument specifies the remote
   * program.
   *)

val call : t -> string -> xdr_value -> xdr_value
  (** [call simple_client procedure_name procedure_arg]:
   * Call the procedure with the given name and the given argument. Returns
   * the result of the procedure on success; otherwise raises an exception.
   *
   * Note that it is possible that the connection is aborted and the
   * client is reset. In this case [call] raises an exception but it is
   * currently difficult to recognize this case.
   * Other reasons may be that the server refuses the call (e.g. because
   * of missing authentication).
   *
   * Frequent exceptions:
   *
   * - [Rpc.Rpc_server condition]:  The server did not like the call;
   *                                the 'condition' explains why
   * - [Rpc.Rpc_cannot_unpack reason]:  Got a badly formatted message.
   * - [Rpc_client.Message_lost]:   The stream ended before there was any
   *                                reply (perhaps not even the call could
   *                                be sent over the stream)
   * - [Rpc_client.Message_timeout]: There was no reply for the specified
   *                                period of time
   * - [Rpc_client.Communication_error x]:
   *                                An I/O error or a serious internal
   *                                error happened
   * [Failure s]:                   Mostly: The internal state of the client
   *                                does not allow to do another call
   *
   * Note that only the [Rpc_server] exception has clear semantics as it is
   * known that the procedure has not been invoked.
   * It is a good choice to shut down the connection if one of the other
   * exceptions happens. (Note that the connection may already have been
   * shutted down, but it is ok to shut down twice.)
   *)

val shut_down : t -> unit
  (** Shut the connection down. After the client has been shutted down, it
   * is not possible to call further remote procedures. Throw the client
   * away and create a new one if you want to continue.
   *)
