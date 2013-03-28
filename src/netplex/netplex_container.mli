(* $Id: netplex_container.mli 1256 2009-08-09 19:25:49Z gerd $ *)

(** Containers
  *
  * The container is the management object for the concurrently running
  * service processor.
 *)

open Netplex_types

val create_container : 
      parallelization_type -> socket_service -> container
  (** The container for normal services *)

val create_admin_container : 
      Unixqueue.unix_event_system -> parallelization_type -> socket_service -> container
  (** {b Internally used.} The container for the special admin service *)

module Debug : sig
  val enable : bool ref
    (** Enables debug logging of containers *)
end

(**/**)

(* internal stuff: *)
val string_of_event : Netplex_ctrl_aux.event -> string
