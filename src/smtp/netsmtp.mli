(* $Id: netsmtp.mli 847 2005-05-23 10:49:08Z mad_coder $
 * ----------------------------------------------------------------------
 *)

(**
 * This is an interface for the Simple Mail Tranfer Protocol (SMTP)
 * as specified by RFC 2821.
 *)

open Netchannels

exception Protocol_error
exception Transient_error of int * string
exception Permanent_error of int * string

val tcp_port : int
(** default TCP port for SMTP *)

(** The class [client] implements the SMTP protocol.  Client objects are created
 * by
 * {[ new client in_ch out_ch]}
 * where [in_ch] is an input channel representing the input direction of the
 * TCP stream, and where [out_ch] is an output channel representing the output
 * direction of the TCP stream.
 *)
                               
class client :
  in_obj_channel -> out_obj_channel ->
object

  method helo : ?host:string -> unit -> string list
    (** Sends an HELLO command to the server.  the optionnal argument [?host]
     * defaults to the default hostname of the machine.  This function returns
     * the ESMTP lines returned by the server.
     *)

  method mail : string -> unit
    (** Performs a MAIL FROM command.  the [string] argument is the mail address
     * (without < >) that sends the mail.
     *)

  method rcpt : string -> unit
    (** Performs a RCPT TO command.  the [string] argument is one of the mail
     * address the mail has to be sent to.  You have to use that function for
     * each recipient of the mail.
     *
     * If the server returns a 551 error (user relocated, see RFC 2821, section
     * 3.4), the relocated adress is silently used, and the error is not raised
     *)

  method data : in_obj_channel -> unit
    (** This method really send the mail.
     * Do not issue that command without having used [mail] once, and at least
     * [rcpt] once too
     *)
  
  method rset : unit -> unit
    (** Reset the current transaction *)

  method expn : string -> string list option
    (** Expand command : [expn list] will try to expand the Mailing list
     * [list].  If the list cannot be Expanded (reply 252) then [None] is
     * returned.
     *)

  method help : unit -> string list
    (** Performs the Help command.  Returns the server multiline answer.  *)

  method noop : unit -> unit
    (** NOOP : does nothing, keeps the connection alive.  *)

  method quit : unit -> unit
    (** Requests the server to end this session. *)

end

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.2  2005/05/23 10:49:08  mad_coder
 * add doc to netsmtp
 *
 *
 *)
