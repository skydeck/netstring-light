(* $Id: unixqueue_pollset.mli 1204 2008-09-28 22:32:24Z gerd $ *)

(** Unixqueue implementation on top of {!Netsys_pollset} *)

class pollset_event_system : 
      Netsys_pollset.pollset -> Unixqueue_util.event_system

val pollset_event_system : 
      Netsys_pollset.pollset -> Unixqueue_util.event_system_t
  (** Implements a unixqueue on top of a pollset.
   *)
