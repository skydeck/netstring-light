(* $Id: rpc_auth_local.mli 1676 2011-10-11 10:56:36Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* Authentication for Unix domain sockets
 *
 * Some operating systems allow it to check the uid of the connecting user
 * of a Unix domain socket. This feature can be used for a very reliable
 * way of authentication.
 *
 * To use this method, connect with AUTH_NONE on the client side. On the
 * server side, put the following [server_auth_method] into the list of
 * permitted methods (in addition to {!Rpc_server.auth_none}).
 *
 * This method formats user names as strings
 *   "<uid>.<gid>@localhost"
 * where <uid> is the effective user ID and <gid> is the effective group ID
 * of the calling user.
 * Note that you can parse this string with {!Rpc_auth_sys.parse_user_name}.
 *
 * If the file descriptor is not a Unix domain socket, this method generates
 * the error [Auth_too_weak].
 * The same happens if the operating system does not provide a way to
 * get the credentials of the connecting user (or I have not yet implemented
 * it).
 *
 * {2 Supported OS}
 *
 * Currently this works {b only} on Linux.
 *
 * {2 Interface}
 *)

val server_auth_method : unit -> Rpc_server.auth_method
  (** Return the authentication method [AUTH_LOCAL].
   *
   * Note that you need another authentication method that operates at
   * message level (like AUTH_NONE, AUTH_SYS, AUTH_DH), otherwise you will
   * get an error [Auth_too_weak]. [AUTH_LOCAL] overrides the result of the
   * other authentication method.
   *)


val get_peer_credentials : Unix.file_descr -> (int * int);;
  (** Return the pair (euid,egid) for a Unix domain socket.
   * The function raises [Invalid_argument] if it is not available for this
   * operating system. (Generally, this should work on Linux, BSD, and
   * Solaris, and OS with compatible system functions - supported methods are
   * [SO_PEERCRED], [getpeerucred], and [getpeereid]).
   *
   * Some OS support this only for named Unix domain sockets but not for
   * socketpairs.
   *)

(*
val peek_peer_credentials : Unix.file_descr -> (int * int);;
  (** Peeks at the next message and returns the pair (euid,egid) for a
   * Unix domain socket. This function must be called before data is
   * read from the socket (and it blocks until data is available).
   * The function raises [Invalid_argument] if it is not available for this
   * operating system.
   * The exception [Not_found] is raised if the credentials cannot be
   * extracted from the control block.
   * [peek_peer_credentials] seems to be more portable than
   * [get_peer_credentials].
   *)
*)

