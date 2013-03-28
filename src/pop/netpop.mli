(* $Id: netpop.mli 820 2004-08-01 10:24:06Z stolpmann $
 * ----------------------------------------------------------------------
 *)

(**
 * This is an interface for the Post Office Protocol - Version 3
 * (POP3) as specifed by RFC 1939. The protocol is intended to permit
 * a workstation to dynamically access a maildrop on a server host in
 * a useful fashion.
 *)

open Netchannels

type state = 
  [ `Authorization
  | `Transaction
  | `Update
  ]

exception Protocol_error
exception Err_status of string
exception Bad_state

val tcp_port : int
(** Default TCP port for POP version 3 *)

(** The class [client] implements the POP3 protocol. Client objects
 * are created by
 * {[ new client in_ch out_ch ]}
 * where [in_ch] is an input channel representing the input direction of
 * the TCP stream, and where [out_ch] is an output channel representing
 * the output direction of the TCP stream.
 *)
class client : 
  in_obj_channel -> out_obj_channel ->
object

  method state : state
    (** Current state of this session. *)

  (* General Commands *)

  method quit : unit -> unit
    (** Requests the server to end this session. If the session is 
     * currently in the [`Transaction] state, the server will attempt
     * to remove all messages marked as deleted before closing its 
     * side of the connection.
     *)

  (* Authorization Commands *)

  method user : user:string -> unit
    (** Specifies the name of the mailbox the client would like to open
       using plain-text authentication. Normal completion of this function
       should be followed by the [pass] command. *)

  method pass : pass:string -> unit
    (** Authenticates a user with the plain-text password [pass]. *)

  method apop : user:string -> pass:string -> unit
    (** Specifies the user and password using APOP authentication.
     * APOP is a more secure method of authentication than what is
     * provided by the [user]/[pass] command sequence.
     *)

  (* Transaction Commands *)

  method stat : unit -> int * int * string
    (** Returns information about the current mailbox as tuple
     * [(count,size,ext)] where [count] is the number of messages in 
     * the mailbox, [size] is the size of the mailbox in octets, 
     * and [ext] is any server extension data.
     *)

  method list : ?msgno:int -> unit -> (int,int * string) Hashtbl.t
    (** Returns the scan listing for an optional message number or
     * for all messages in the current mailbox. The result is a hash
     * table that maps a message number to a tuple [(size,ext)] where
     * [size] is the size of the message in octets, and [ext] is any 
     * server extension data.
     *)

  method retr : msgno:int -> in_obj_channel
    (** Retrieves a message from the server. *)

  method dele : msgno:int -> unit
    (** Marks the message number of the current mailbox for deletion. *)

  method noop : unit -> unit
    (** Pings the server to keep the session alive. *)

  method rset : unit -> unit
    (** Unmarks any messages that have previously been marked as
     * deleted.
     *)

  method top  : ?lines:int -> msgno:int -> unit -> in_obj_channel
    (** Returns the message header plus a limited number of lines
     * of the message body. The default body length is 0 lines.
     *)

  method uidl : ?msgno:int -> unit -> (int,string) Hashtbl.t
    (** Returns the unique identifier(s) for an optional message number
     * or for all messages in the current mailbox. The result is a
     * hash table that maps a message number to its unique id.
     *)
end

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.2  2004/08/01 10:24:06  stolpmann
 * 	Pre-release changes: ocamldoc, further doc updates
 *
 * Revision 1.1  2001/11/11 21:43:14  pdoane
 * 	Initial revision
 *
 * 
 *)
