(* $Id: netplex_cenv.mli 1774 2012-04-03 21:28:51Z gerd $ *)

(** Container environment
  *
  * Some helper functions to explore the environment from a container.
  * Most of the following functions {b must} be called from a container context,
  * i.e. from a process or thread that acts as container, otherwise
  * the exception [Not_in_container_thread] is raised. There are also some
  * functions that can be called from controller context for convenience.
  *
  * {b Thread safety:} Full. The functions in this module can be called from any
  * thread.
 *)

open Netplex_types

exception Not_in_container_thread
  (** Raised when the caller's thread is not a container thread *)

(** {2 Logging} *)

(** Logging functions can be invoked from both container and controller 
    contexts.
 *)

val log : level -> string -> unit
  (** Writes a log message *)

val logf : level -> ('a, unit, string, unit) format4 -> 'a
  (** Writes a log message like [printf] *)

val report_connection_string : Unix.file_descr -> string -> string
  (** Output a log line for the [netplex.connections] admin message.
      The string is the detail to report.
   *)

(** {2 Timer} *)

(** Timer functions can only be invoked from container contexts. 
    More documentation is available in {!Netplex_advanced.timers}.
 *)

type timer
  (** A timer *)

val create_timer : (timer -> bool) -> float -> timer
  (** [create_timer f tmo]: Creates a timer with timeout value [tmo]:
      In [tmo] seconds [f] is called, and if this function returns [true],
      the timer remains active, and another round of timing is arranged.
      If the functions returns [false] or raises an exception, the timer
      is stopped.

      Timers are also stopped on container shutdown.

      Timers are attached to the container event system, and run only
      if this event system runs. Also note that [f] is always called from
      the main thread of the container.
   *)

val cancel_timer : timer -> unit
  (** Cancels the timer: The callback function is not called any longer *)

val cancel_all_timers : unit -> unit
  (** Cancels all active timers *)

val timer_id : timer -> int
  (** Returns an ID, e.g. useful for debugging *)


(** {2 Container variables} *)

(** Container variables exist once per container. Primary access is
    done via the [var] and [set_var] methods of the container class.
    The following functions are often more convenient, however.

    These functions can only be invoked from container contexts. 

    More documentation: {!Netplex_advanced.contvars}
 *)

exception Container_variable_not_found of string
  (** The variable does not exist *)

exception Container_variable_type_mismatch of string
  (** The (dynamically typed) variable has the wrong type *)


val int_var : string -> int
val string_var : string -> string
val float_var : string -> float
val bool_var : string -> bool
  (** Access a variable with simple type. May raise 
      [Container_variable_not_found] or [Container_variable_type_mismatch]
   *)

val set_int_var : string -> int -> unit
val set_string_var : string -> string -> unit
val set_float_var : string -> float -> unit
val set_bool_var : string -> bool -> unit
  (** Set a variable with simple type *)

val make_var_type :
      ('a -> encap) -> (encap -> 'a) -> ((string -> 'a) * (string -> 'a -> unit))
  (** Create get and set functions for any (monomorphic) type. For example,
      to create such function for a type [foo], do

      {[ 
          module E = Netplex_encap.Make_encap(struct type t = foo end)
          let (get, set) = 
            make_var_type E.wrap E.unwrap
      ]}

      Read on for using functors to create [get] and [set].
   *)

module type TYPE = sig type t end
  (** Just a (monomorphic) type [t] *)

module type VAR_TYPE = sig
  type t 
  val get : string -> t
  val set : string -> t -> unit
end
  (** A (monomorphic) type [t] with two functions [get] and [set]
      accessing the container variables
   *)

module Make_var_type(T:TYPE) : VAR_TYPE with type t = T.t
  (** Creates [get] and [set] like [make_var_type]. Call it like

      {[
         module Foo_var = 
           Make_var_type(struct t = foo end)
      ]}

      and use [Foo_var.get] and [Foo_var.set] to access the container
      variables of type [foo].
   *)




(** {2 System control} *)

(** System control functions can be invoked from both container and controller 
    contexts.
 *)

val system_shutdown : unit -> unit
  (** Initiates a system shutdown (like the [shutdown] method of the
      controller)
     *)

val system_restart : unit -> unit
  (** Initiates a system restart (like the [restart] method of the
      controller)
     *)


(** {2 Inter-Container Communication} *)

(** These functions can only be invoked from container contexts, 
    except [send_message].
 *)


val send_message : string -> string -> string array -> unit
  (** [send_message service_pattern msg_name msg_arguments]: Sends
       a message to all services and message receivers matching
       [service_pattern]. The pattern may include the wildcard [*].

       See the {!Netplex_types.controller.send_message} method for
       the notification guarantees.

       This function can be invoked from both container and controller
       contexts.
   *)

val lookup : string -> string -> string option
  (** [lookup service_name protocol_name] tries to find a Unix domain
      socket for the service and returns it.

      On Win32, the returned path refers to a file describing the
      IPC mechanism. Use {!Netplex_sockserv.any_file_client_connector}
      to convert the path into an RPC connector.
   *)

val lookup_container_sockets : string -> string -> string array 
  (** [lookup_container_sockets service_name protocol_name]: returns
      the Unix Domain paths of all container sockets for this service and
      protocol. These are the sockets declared with address type
      "container" in the config file.

      On Win32, the returned paths refer to files describing the
      IPC mechanism. Use {!Netplex_sockserv.any_file_client_connector}
      to convert the paths into RPC connectors.

      Container sockets are explained here:
      {!Netplex_advanced.contsocks}
   *)


(** {2 Direct container and admin interface access} *)

val self_cont : unit -> container
  (** Returns the container running the code of the caller,
      or raise [Not_in_container_thread] if called from outside a 
      container context.
   *)

val self_obj : unit -> [ `Container of container |
			 `Controller of controller ]
  (** Returns the container or the controller running the code of the
      caller, or raise [Not_found] if called from
      neither a container not a controller thread.
   *)

val current_sys_id : unit -> [ `Thread of int | `Process of int ]
  (** Returns the system-dependent thread identifier of the caller
      (which must be in container or controller context)
   *)


val admin_connector : unit -> Rpc_client.mode2
  (** Determines the admin socket of the controller, and returns an RPC
      client connector suitable for connecting with the admin interface
      of the controller. For instance to initiate a system shutdown from
      the context of a container:

      {[
         let conn = Netplex_cenv.admin_connector() in
         let client = Netplex_ctrl_clnt.Admin.V2.create_client2 conn in
         Netplex_ctrl_clnt.Admin.V2.system_shutdown client ();
         Rpc_client.shut_down client
       ]}

      Note that the admin interface is going to evolve, and it is advisable
      to avoid admin calls whenever possible.

      This function must be called from container context.
   *)

val run_in_controller_context : controller -> (unit -> unit) -> unit
  (** [run_in_controller_context ctrl f]: Arranges that [f()] is executed
      in the context of the controller. {b This is only possible for
      multi-threading but not for multi-processing style!} For
      programs using multi-processing, see {!Netplex_cenv.Make_lever}
      for a workaround.

      This function can be called from any thread. The function [f] is
      executed by pushing it onto the event queue, and calling it when
      the pushed event is reached. This is usually a safe point for
      many kinds of operations, but if controller methods are invoked
      the details are left unspecified.

      For example, this allows it to start helper threads via
      {!Netplex_kit.add_helper_service} at any time.

      An example can be found here: {!Netplex_advanced.levers}
   *)


val run_in_container_context : container -> (unit -> unit) -> unit
  (** [run_in_container_context cont f]: Arranges that [f()] is executed
      in the context of the container [cont]. {b This is only possible for
      multi-threading but not for multi-processing style!}

      This function can be called from any thread. The function [f] is
      executed by pushing it onto the event queue, and calling it when
      the pushed event is reached. This is usually a safe point for
      many kinds of operations, but if container method are invoked
      the details are left unspecified.

      There is no guarantee that [f] is called anytime soon - if the
      container is busy with something else than with the event queue
      the execution will be blocked until these other activities are
      over.
   *)


(** Levers are a way to send messages to the controller, and to effectively
    run functions there that were previously registered. 

    More documentation: {!Netplex_advanced.levers}
 *)

(** Abstraction for function types [s->t] *)
module type FUN_TYPE = 
  sig 
    type s  (** argument type *)
    type r  (** result type *)
  end

module type LEVER = sig
  type s  (** argument type *)
  type r  (** result type *)
  type t = s->r

  val register : Netplex_types.controller -> 
                 (Netplex_types.controller -> t) -> t
    (** [let reg_lever = register ctrl raw_lever]:
        Registers [raw_lever] in the controller [ctrl], so one can call
        [reg_lever] to activate it. For example:

        {[ 
           module LT = struct type s = unit type r = int end
           module L = Make_lever(LT)
         
           let get_num_services =
             L.register ctrl (fun ctrl () -> List.length ctrl#services)
        ]}

        The registration must be done in controller context, e.g.
        in the [pre_start_hook] of a container.

        From the running container, one can now call:

        {[ get_num_services() ]}

        to get the current length of the [ctrl#services] list.
     *)
end

module Make_lever(T:FUN_TYPE) : LEVER with type s=T.s and type r=T.r
  (** Creates a [LEVER] module from a function type as specified in
      [FUN_TYPE]
   *)


(** {2 Persistent kernel objects} *)

val pmanage : unit -> Netsys_pmanage.pmanage
  (** Access the manager for persistent kernel objects with limited
      lifetime. Among these objects there are shared memory objects,
      and named semaphores. These objects can usually be deleted when
      the program finishes (or crashes), but this is not done automatically
      because of kernel persistency.

      See {!Netplex_admin.unlink} for more information.
   *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging *)

end


(**/**)

val register_par : parallelizer -> unit
val register_cont : container -> par_thread -> unit
val unregister_cont : container -> par_thread -> unit
val register_ctrl : controller -> unit
val unregister_ctrl : controller -> unit
