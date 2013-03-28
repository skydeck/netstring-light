(* $Id: rpc_time.mli 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


(** Get the time of the server (using the RFC 868 netdate protocol) *)

exception Time_not_available

val remote_time : ?timeout:int -> Unix.inet_addr -> float
  (** Returns the time of the passed server in seconds since the epoch.
   * A connection to TCP port 37 of the server is opened, and the time
   * is read.
   * If an error happens, or the timeout occurs, the exception
   * [Time_not_available] is raised.
   * The timeout is 5 seconds by default but can be set using the
   * [timeout] parameter.
   *
   * TCP has been chosen for security reasons, for example, there may be
   * a firewall between this system and the remote host. It is much simpler
   * to get a TCP port opened than a UDP port.
   *)
