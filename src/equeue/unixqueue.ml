(* 
 * $Id: unixqueue.ml 1696 2012-02-08 19:27:53Z gerd $
 *)

open Printf;;


(**********************************************************************)
(* Basic type definitions                                             *)
(**********************************************************************)

type group = Unixqueue_util.group

type wait_id = Unixqueue_util.wait_object

type operation = Unixqueue_util.operation = 
    Wait_in  of Unix.file_descr
  | Wait_out of Unix.file_descr
  | Wait_oob of Unix.file_descr
  | Wait of wait_id

type event = Unixqueue_util.event =
    Input_arrived of (group * Unix.file_descr)
  | Output_readiness of (group * Unix.file_descr)
  | Out_of_band of (group * Unix.file_descr)
  | Timeout of (group * operation)
  | Signal
  | Extra of exn
  | Immediate of (group * (unit -> unit))



class type event_system =
object
  (* Public interface *)
  method new_group : unit -> group
  method new_wait_id : unit -> wait_id
  method exists_resource : operation -> bool
  method add_resource : group -> (operation * float) -> unit
  method add_weak_resource : group -> (operation * float) -> unit
  method add_close_action : group -> (Unix.file_descr * (Unix.file_descr -> unit)) -> unit  method add_abort_action : group -> (group -> exn -> unit) -> unit
  method remove_resource : group -> operation -> unit
  method add_handler : group -> (event_system -> event Equeue.t -> event -> unit) -> unit
  method add_event : event -> unit
  method clear : group -> unit
  method run : unit -> unit
  method is_running : bool
  method when_blocking : (unit -> unit) -> unit
  (* Protected interface *)
  method private setup : unit -> (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list * float)
  method private queue_events : (Unix.file_descr list * Unix.file_descr list * Unix.file_descr
list) -> bool
  method private source : event Equeue.t -> unit
end
  (* Note: This has to match the object type Unixqueue_util.event_system *)


type resource_prop = Unixqueue_util.resource_prop

type handler = Unixqueue_util.handler

exception Abort = Unixqueue_util.Abort

exception Exit;;   (* used locally in uq_handler *)


class standard_event_system() =
  Unixqueue_pollset.pollset_event_system
    (Netsys_pollset_generic.standard_pollset())

class unix_event_system = standard_event_system

let standard_event_system() =
  new standard_event_system()

let create_unix_event_system =
  standard_event_system

class performance_event_system() =
  Unixqueue_pollset.pollset_event_system
    (Netsys_pollset_generic.performance_pollset())

let performance_event_system() = new performance_event_system()
  

let new_group ues =
  ues # new_group()

let new_wait_id ues =
  ues # new_wait_id()

let exists_resource ues =
  ues # exists_resource

let add_resource ues =
  ues # add_resource

let add_weak_resource ues =
  ues # add_weak_resource

let add_close_action ues =
  ues # add_close_action

let add_abort_action ues =
  ues # add_abort_action

let remove_resource ues =
  ues # remove_resource

let add_handler ues =
  ues # add_handler

let add_event ues =
  ues # add_event

let clear ues =
  ues # clear

let run ues =
  ues # run ()

let is_running ues =
  ues # is_running

let once ues =
  Unixqueue_util.once ues

let weak_once ues =
  Unixqueue_util.weak_once ues

let epsilon ues =
  Unixqueue_util.epsilon ues

module Debug = Unixqueue_util.Debug

(*
let exn_log ues =
  Unixqueue_util.exn_log ues

let debug_log ues =
  Unixqueue_util.debug_log ues

let set_debug_mode =
  Unixqueue_util.set_debug_mode

let set_debug_target = 
  Unixqueue_util.set_debug_target
 *)
