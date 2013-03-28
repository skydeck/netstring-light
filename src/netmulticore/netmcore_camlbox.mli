(* $Id: netmcore_camlbox.mli 1668 2011-09-09 13:28:26Z gerd $ *)

(** Camlboxes for use in netmulticore programs *)

open Netcamlbox
open Netmcore

val create_camlbox : string -> int -> int -> 'a camlbox * res_id
  (** [create_camlbox prefix n size]: Creates a new camlbox with a 
      unique name derived from [prefix]. [prefix] must not contain
      any slahes. The box can have up to [n] messages of [size] bytes.

      The returned camlbox can receive messages. It is only meaningful
      in the process that created it, and only this process can receive
      messages.

      Other processes can send messages to this box. They first have
      to look up the sender object via [lookup_camlbox_sender] below.

      It is suggested that the result of [create_camlbox] is immediately
      coerced to the right type [t], e.g.
      {[
        let box = (create_camlbox prefix n size : t camlbox * res_id)
      ]}
      as this ensures type safety for all following operations.

   *)

val lookup_camlbox_address : res_id -> camlbox_address
  (** Returns the address of the camlbox identified by this resource ID *)

val lookup_camlbox_sender : res_id -> 'a camlbox_sender
  (** [lookup_camlbox_sender res_id]: 
      Returns the sender object of the camlbox identified by this resource ID 

      It is suggested that the result of [lookup_camlbox_sender] is immediately
      coerced to the right type [t], e.g.
      {[
        let box = (lookup_camlbox_sender id : t camlbox_sender)
      ]}
      as this ensures type safety for all following operations.
   *)
