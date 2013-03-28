(* $Id: rpc_auth_sys.mli 287 2006-04-29 16:28:25Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Authentication module AUTH_SYS *)

(** This module implements system authentication (AUTH_SYS, also known as
 * AUTH_UNIX).
 *
 * ***********************************************************************
 * WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
 *
 * This authentication method is insecure because it does not use any
 * verifier. I strongly dissuade everybody from using this authentication
 * method for newly written RPC systems. I include it here only for
 * compatibility with older systems.
 *
 * WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
 * ***********************************************************************
 *)

type identity =
    [ `Effective_user
    | `Real_user
    | `This_user of (int * int * int array * string)
	(* (uid, gid, supplementary group IDs, hostname) *)
    ]
      (** Specifies the user:
        * - [`Effective_user]: Take the effective user of the process
        * - [`Real_user]: Take the real user of the process
        * - [`This_user(uid,gid,sup_groups,hostname)]: Pretend to be
        *   this user
       *)


val client_auth_method : ?identity : identity ->       (* default: `Real_user *)
                         unit ->
			   Rpc_client.auth_method
  (** Pass the result of this function to [Rpc_client.set_auth_methods] to
   * configure client authentication.
   *
   * [identity]: As whom the clients authenticates.
   *)


type user_name_format =
    [ `Full
    | `UID
    | `Custom of int32 -> int32 -> int32 array -> string -> string
    ]
    (** How [Rpc_server.get_user] returns the user as string:
      * - [`Full]: The format includes all transmitted details:
      *   ["<uid>.<gid>.<gid1>.<gid2>...@<hostname>"].
      *   All user and group IDs are numeric. The first two numbers, <uid> and
      *   <gid> are always present. The other numbers are the supplementary
      *   group IDs and can be omitted. The <hostname> is the name passed in
      *   the credentials.
      * - [`UID]: The string is the numeric user ID
      * - [`Custom f]: The string is returned by the function [f]. The
      *   arguments are [uid], [gid], the array of the supplementary 
      *   group IDs and the hostname.
     *)

val server_auth_method : ?lookup_hostname:bool ->          (* default: true *)
                         ?require_privileged_port:bool ->  (* default: true *)
                         ?user_name_as:user_name_format -> (* default: `Full *)
			 unit ->
			   Rpc_server.auth_method
  (** Pass the result of this function to [Rpc_server.set_auth_methods] to
   * configure authentication.
   *
   * [lookup_hostname]: if true, the hostname contained in the credentials
   *    is checked (by gethostbyname)
   *
   * [require_privileged_port]: if true, the client must use a privileged
   *    port. Note that Unix domain sockets are rejected in this mode.
   *
   * User name strings as formatted as specified by [user_name_as]. 
   * This defaults to [`Full].
   *)

val parse_user_name : string -> (int * int * int array * string)
  (** Parses a user name as returned by [Rpc_server.get_user] in conjunction
   * with the AUTH_SYS authentication and [`Full] formatting.
   * Returns [(uid,gid,sup_groups,hostname)].1
   *)
