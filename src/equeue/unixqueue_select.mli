(* $Id: unixqueue_select.mli 1616 2011-06-10 15:08:57Z gerd $ *)

open Unixqueue_util

class type sb_event_system =
object
  (* Public interface *)
  method new_group : unit -> group
  method new_wait_id : unit -> wait_id
  method exists_resource : operation -> bool
  method add_resource : group -> (operation * float) -> unit
  method add_weak_resource : group -> (operation * float) -> unit
  method add_close_action : group -> (Unix.file_descr * (Unix.file_descr -> unit)) -> unit
  method add_abort_action : group -> (group -> exn -> unit) -> unit
  method remove_resource : group -> operation -> unit
  method add_handler : group -> (Unixqueue.event_system -> event Equeue.t -> event -> unit) -> unit
  method add_event : event -> unit
  method clear : group -> unit
  method run : unit -> unit
  method is_running : bool
  method when_blocking : (unit -> unit) -> unit
  (* Protected interface *)
  method private setup : unit -> (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list * float)
  method private queue_events : (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list) -> bool
  method private source : event Equeue.t -> unit
end


class select_based_event_system : unit -> sb_event_system
  (** This the old [Unix.select]-based imeplementation of event systems
      which was the default one until Ocamlnet-2.2.

      Please avoid in new code. This module merely exists to allow
      comparisons between the old implementation and the new one.

      Note that this class definition also includes some private
      methods. These are required in some other Unixqueue implementations
      inheriting from this class.
   *)

val select_based_event_system : unit -> Unixqueue.event_system
  (** Create a new [Unix.select]-based event system *)
