(* $Id: rpc_portmapper.mli 279 2006-04-24 14:45:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Portmapper version 2 *)

(** Call the portmapper version 2. Note that version 2 is an older version
 * (version 3 and 4 are called 'rpcbind'), but it is normally available.
 *
 * The task of the portmapper is to map program numbers to port numbers.
 * A RPC service that should be available in the whole network should:
 * - on startup: call the [set] procedure to establish a mapping of the
 *   own program number to the port that has been allocated previously
 * - on shutdown: call the [unset] procedure to remove the mapping
 *   (this is NEVER done automatically!)
 *
 * To call an RPC service which is only known by its program number one should
 * contact the portmapper using [getport] to find out the port where the
 * service is actually listening.
 *)

open Rtypes
open Rpc
open Xdr

type t
  (** represents a client for the portmapper *)

val create : Rpc_client.connector -> t
  (** Connects to the portmapper service listening on the given connector. *)

val create_inet : string -> t
  (** Connects to a portmapper listening on an Internet port. The argument
   * is the hostname where the portmapper is running or its internet
   * address. This function connects always to the port 111 on the given
   * host; this is the standard for portmapper daemons.
   *)

val shut_down : t -> unit
  (** Shuts down the connection to the portmapper. *)

val null : t -> unit
  (** Calls the 'NULL' procedure of the portmapper. This procedure has no
   * effect. You can use 'null' to determine whether a procedure call is
   * possible or not.
   *)

val set : t -> uint4 -> uint4 -> protocol -> int -> bool
  (** [set pm_client program_nr version_nr protocol port_nr]:
   * Extends the mapping managed by the portmapper: The triple
   * [(program_nr, version_nr, protocol)] is mapped to the given
   * [port_nr].
   * It is not allowed to overwrite an existing mapping.
   * The procedure returns [true] if the mapping has been extended
   * and [false] otherwise.
   * Note that it usually only possible to [set] a mapping on the local
   * host.
   *)

val unset : t -> uint4 -> uint4 -> protocol -> int -> bool
  (** [unset pm_client program_nr version_nr protocol port_nr]:
   * removes the mapping.
   * The procedure returns [true] if the mapping has been removed
   * and [false] otherwise.
   * Note that it usually only possible to [unset] a mapping on the local
   * host.
   *)

val getport : t -> uint4 -> uint4 -> protocol -> int
  (** [getport pm_client program_nr version_nr protocol]:
   * finds out the port where the given service runs. Returns 0 if the
   * service is not registered.
   *)

val dump : t -> (uint4 * uint4 * protocol * int) list
  (** returns the list of known mappings. The quadrupels have the meaning
   * [(program_nr, version_nr, protocol, port)]
   *)

val callit : t -> Rpc_program.t -> string -> xdr_value -> (int * xdr_value)
  (** [callit pm_client program_spec proc_name argument]:
   * This is an alternate way of calling a remote procedure. Instead
   * of directly invoking the procedure, the portmapper does it for you.
   *
   * You must pass a program specification, the procedure name and the
   * argument to [callit]. On return, you get the port of the service
   * and the result of the procedure invocation.
   *
   * Note that there are several disadvantages:
   * - Error conditions cannot be transmitted back from the portmapper.
   *   [callit] gets a timeout in this case.
   * - The portmapper uses always UDP. This means that the length of
   *   the messages that are exchanged is limited to a total of 8000 bytes.
   *
   * It is NOT recommended to use [callit] for indirecting calls.
   * The [callit] interface was designed for broadcasts and should only
   * be used for this purpose.
   * {b This} implementation of a [callit] client cannot be used for
   * broadcasts, however.
   *)

val port_of_program : Rpc_program.t -> string -> protocol -> int
  (** [port_of_program program host protocol]:
   * queries the portmapper running on [host] for the [program] registered
   * for [protocol].
   * Returns the port number or fails if the number is not known.
   *)

