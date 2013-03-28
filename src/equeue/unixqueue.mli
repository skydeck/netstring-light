(* 
 * $Id: unixqueue.mli 1696 2012-02-08 19:27:53Z gerd $
 *)

open Unix;;
open Sys;;

(** Unixqueues are one of the two forms of system event loops provided
    by Ocamlnet. Besides Unixqueue, there is also pollset (see
    {!Netsys_pollset}). The pollsets are much simpler (there is no
    queuing of events), and nowadays Unixqueue bases upon pollset,
    and extends its functionality. Historically, however, Unixqueue
    precede pollset, and there are still implementations of the former
    in Ocamlnet not using pollset as its base data structure.

    The common idea of both data structures is the generalization of 
    watching for events, as it is also provided by the [Unix.select]
    function. Note, however, that recent implementations no longer
    use [Unix.select], but better system interfaces for the same.

    When there is something to do for a file descriptor (reading, 
    writing, accepting out-of-band data), this is called an {b event},
    and the task of Unixqueue is to check when events happen, and to
    tell some consumer about the events.

    There are three further types of events: Timeout events, signal
    events, and user-defined events.

    The events are queued up, and they are presented to event handlers
    that may process them.

    You can describe what types of event conditions are watched by adding
    {b resources}. You can think a resource being a condition (bound to
    a real resource of the operating system) for which 
    events are generated if the condition becomes true. Currently, only
    file descriptors and timers are supported as resources.
 *)

(** {b Relation to other modules.} This module is thought as the primary
    interface to Unixqueues. If there isn't any specialty one has to deal
    with, just use this module:

    - It defines all required types like [group], [wait_id], etc. Note that
      these types are reexported from [Unixqueue_util]. Please consider
      this as implementation detail, and don't use it in your code.
    - It defines a standard implementation [standard_event_system], which
      is a good default implementation, although it might not be the best
      available for all purposes.
    - It defines a set of access functions like [add_event] which 
      simply call the methods of the event system object of the same name.
      Note that these functions work for all event system implementation,
      not only for [standard_event_system].

    There are further modules that have to do with Unixqueue:

    - {!Unixqueue_pollset} is the implementation behind 
      [standard_event_system]. If you want to use other pollsets than
      the standard one, it is possible to create Unixqueues on top of these
      by using this module directly.
    - {!Unixqueue_select} is the historic default implementation. It
      calls directly [Unix.select]. It is still available because it 
      serves as a reference implementation for now.
    - [Unixqueue_util] is an internal module with implementation details.
      Please don't call it directly.
    - {!Uq_gtk} is an implementation of Unixqueue mapping to the 
      GTK event loop. Useful for multiplexing event-based I/O and
      GTK graphics operations.
    - {!Uq_tcl} is an implementation of Unixqueue mapping to the 
      TCL event loop. Useful for multiplexing event-based I/O and
      event-based code written in TCL (especially TK).
 *)

(** {b Thread safety.} The default implementation of Unixqueue is
    thread-safe, and operations can be called from different threads.
    For other implementations, please look at the modules implementing
    them.
 **)


(** {1 Types and exceptions} *)


type group = Unixqueue_util.group
  (** A group is an abstract tag for a set of events, resources, and
   * event handlers. Usually every event handler creates a new group,
   * and all events and resources processed by the handler are 
   * members of this group.
   *)

exception Abort of (group * exn);;
  (** Event handlers can raise this exception to cancel a group 
   * of handlers, events, and resources. If an abort action
   * is defined for the group, it will be executed. Next, all members
   * of the group are removed from the event system. 
   *
   * First argument is the group. The second argument
   * is an arbitrary exception (must not be [Abort] again) which is
   * passed to the abort action.
   *
   * Abort handlers are a questionable feature of Unixqueues. You
   * can also call the [clear] operation, and raise the exception
   * directly. {b Do not use in new code!}
   *)


type wait_id = Unixqueue_util.wait_id
  (** A wait identifier is used to distinguish between several
   * timers, see type [operation].
   *)


type operation = Unixqueue_util.operation =
    Wait_in  of file_descr          (** wait for input data *)
  | Wait_out of file_descr          (** wait until output can be written *)
  | Wait_oob of file_descr          (** wait for out-of-band data *)
  | Wait of wait_id                 (** wait only for timeout *)
  (** An [operation] specifies the condition to wait for. Every kind
   * of operation may have an associated timer (not only [Wait]).
   *)


type event = Unixqueue_util.event =
    Input_arrived of    (group * file_descr)  (** Input data has arrived *)
  | Output_readiness of (group * file_descr)  (** Output is possible now *)
  | Out_of_band of      (group * file_descr)  (** OOB data has arrived *)
  | Timeout of          (group * operation)   (** A timer has expired *)
  | Signal                                    (** A signal has happened *)
  | Extra of exn                              (** User-generated event *)
  | Immediate of (group * (unit -> unit))     (** immediate event *)
  (** An [event] is triggered when the condition of an [operation]
   * becomes true, when a signal happens, or when the event is
   * (artificially) added to the event queue ([add_event], below).
   * The events resulting from an [operation] carry the group of
   * the resource with them. 
   *
   * The event [Signal] is triggered when the [EINTR] condition is
   * caught; this normally means that a signal has just been delivered.
   * The generation of [Signal] events should be considered as
   * unreliable, not every signal delivery can be detected. Reasons for
   * the unrealiability are that user-supplied code happens to
   * get the [EINTR] condition and not the [Unixqueue] event loop,
   * and that there are known race conditions in the O'Caml signal
   * handling routines that may cause signals to be lost. However,
   * it can be expected that almost all signals will trigger [Signal].
   *
   * The event [Extra] can only be artificially added to the queue,
   * and the argument of [Extra] is an exception value that distinguishes
   * between several kinds of user-generated events.
   *
   * The event [Immediate(g,f)] also can only be artificially added to
   * the queue. In contrast to other events, it is not passed to handlers
   * when the event is processed. Instead, an immediate event is processed
   * by calling [f()]. This is a more direct way of notification, and
   * it is not necessary to define a handler. Even an immediate event is
   * member of a group [g], and if the [clear] function is called for [g],
   * the callback function [f] will no longer be called.
   *)

class type event_system =
object
  (* Public interface *)
  method new_group : unit -> group
  method new_wait_id : unit -> wait_id
  method exists_resource : operation -> bool
  method add_resource : group -> (operation * float) -> unit
  method add_weak_resource : group -> (operation * float) -> unit
  method add_close_action : group -> (file_descr * (file_descr -> unit)) -> unit
  method add_abort_action : group -> (group -> exn -> unit) -> unit
  method remove_resource : group -> operation -> unit
  method add_handler : group -> (event_system -> event Equeue.t -> event -> unit) -> unit
  method add_event : event -> unit
  method clear : group -> unit
  method run : unit -> unit
  method is_running : bool
  method when_blocking : (unit -> unit) -> unit
end
  (** The [event_system] manages events, handlers, resources, groups,
   * etc. It is now a class type, and you may invoke the operations directly
   * for the class. The operations are still available as functions (below).
   *
   * A {b resource} is an operation with an optional timer. The operation
   * describes the condition to watch for, and the timer defines the
   * maximum period of time for that. If the condition becomes true,
   * an [Input_arrived], [Output_readiness], or [Out_of_band] event
   * will be triggered. If the timer expires, a [Timeout] event will be
   * generated. After the event the resource remains active, and the
   * timeout period begins anew.
   *
   * A resource is usually bound to a file descriptor. It is allowed
   * to watch the same descriptor for several different conditions,
   * but it is forbidden to watch the same descriptor for the same kind
   * of condition several times.
   *
   * As a special case, the operation [Wait] is not bound to a
   * file descriptor, but simply starts a timer. The argument of [Wait]
   * can be used to distinguish between several timers that are active
   * at the same time.
   *
   * {b Event handlers} get the events one after the other, and 
   * process them. When a handler is called for an event, there are
   * several possible reactions: (1) The handler can return normally,
   * which means that the event has been accepted, and will not be
   * passed to any other handler. (2) The handler can raise
   * {!Equeue.Reject}, which means that the handler cannot process
   * the event, and that another handler should get it. (3) The handler
   * can raise {!Equeue.Terminate} which means that the event has been
   * accepted, and that the handler is terminated (it will never be
   * called again). (4) The handler can raise [Abort] which means that
   * the event is deferred, and that a special abort mechanism is
   * triggered (see the description for [Abort] above), this is also
   * terminates the handler. The deferred event will again be processed
   * in the future. (5) The handler can raise any other exception.
   * This causes that the event is deferred, and the exception falls
   * through to the caller of [run].
   *
   * {b Groups} are used to simplify the association of events to
   * handlers, and to simplify the termination of handlers (see [clear]).
   * If an event is associated with a group, only handlers associated with
   * the same group will get them.
   *
   * There is a special {b Close handler} which is useful to close file
   * descriptors no longer needed. It is called when all resources are
   * removed from the event system dealing with the file descriptor. 
   * The close handler should close the descriptor. Note that close handlers
   * are only useful under certain circumstances.
   * 
   *)

(** {1 Creating event systems} *)

class standard_event_system : unit -> event_system
  (** The standard implementation of an event system. It uses 
      {!Unixqueue_pollset.pollset_event_system} on top of
      {!Netsys_pollset_generic.standard_pollset}.
   *)

val standard_event_system : unit -> event_system
  (** Create a new, empty, standard event system *)

class unix_event_system : unit -> event_system
  (** An alternate name for [standard_event_system], provided for
      backward compatibility.
   *)

val create_unix_event_system : unit -> event_system
  (** An alternate name for [standard_event_system], provided for
      backward compatibility.
   *)

class performance_event_system : unit -> event_system
  (** The implementation using {!Netsys_pollset_generic.performance_pollset}.
   *)

val performance_event_system : unit -> event_system
  (** Same as function *)


(** {1 Using event systems} *)

(** The following functions work for all kinds of event systems, not
    only for the ones returned by [standard_event_system].
 *)


val new_group : event_system -> group
  (** Create a new, empty group for the event system *)


val new_wait_id : event_system -> wait_id
  (** Create a new unique wait identifier *)


val exists_resource : event_system -> operation  -> bool
  (** Find out if a specific resource already exists (or better: is
   * already watched by an operation).
   *)


val add_resource : event_system -> group -> (operation * float) -> unit
  (** Add a resource such that it is watched for conditions described
   * by the [operation] for the period given by the [float] number.
   * A negative number means that the resource is watched for an infinite
   * period. The resource becomes a member of the [group].
   *
   * You cannot add the same operation several times;
   * if you try it the second operation is silently dropped.
   *
   * The resource remains even if it has generated an event. The timeout
   * period starts again in this case.
   *)

val add_weak_resource : event_system -> group -> (operation * float) -> unit
  (** Similar to [add_resource], but the resource is weak. Such resources
    * do not keep the event system running when only weak resources remain.
    * Normally, [Unixqueue.run] returns to the caller not before
    * all resources are removed and all events are processed. Weak
    * resources do not count for this condition, i.e. [Unixqueue.run]
    * also returns when there are only weak resources left.
    * As an example, weak resources can be used to time out unused
    * file descriptors.
    *
    * Weak resources can be removed with [remove_resource].
    *
    * {b New in Ocamlnet 3.}
   *)

val add_close_action : 
  event_system -> group -> (file_descr * (file_descr -> unit)) 
    -> unit
  (** A close action is added for the file descriptor. The action callback
   * (which gets the descriptor as argument) is called when there is not
   * any watched resource remaining for this descriptor.
   *
   * This may be useful if the descriptor can be closed in this case.
   *
   * The close action becomes member of the passed [group]. The only
   * effect of this is that the action is removed when the [clear] function
   * is called.
   *
   * You can only add (set) one close action for every descriptor.
   *
   * Of course, the idea is to do [add_close_action ... Unix.close]. Note
   * that there is a problem with multi-threaded programs, and this construct
   * must not be used there. In particular, the close action is called from
   * [remove_resource] or [clear], but it is possible that the event system
   * is running, so a watched descriptor might be closed. This has undesired
   * effects. What you should better do is to delay the closure of the
   * descriptor to a sane moment, e.g. by calling
   *   {[ Unixqueue.once esys g 0.0 (fun () -> Unix.close fd) ]}
   * from the close action.
   *)


val add_abort_action : 
  event_system -> group -> (group -> exn -> unit)
    -> unit
  (** An abort action is added to the group. The action callback is
   * called when an arbitrary handler raises [Abort(g,exn)] where
   * [g] is the group the abort action is member of. In this case,
   * the callback function is invoked with the group and [exn] as
   * arguments. After that, the group is cleared.
   *
   * You can only add (set) one abort action for every group.
   *)


val remove_resource : event_system -> group -> operation -> unit
  (** Removes the operation from the watch list of the group.
   * It is an error if the operation is member of another group.
   * If the operation cannot be found at all, the exception [Not_found]
   * will be raised.
   *
   * The removal of resources may trigger close actions.
   *)


val add_handler : 
  event_system -> group -> (event_system -> event Equeue.t -> event -> unit) 
    -> unit
  (** Add an event handler that is associated to the given group. There
   * may be several handlers for a group.
   * 
   * The handler callback function is invoked when there is an event
   * that could be processeable by the handler. As outlined above, the
   * callback function can accept or reject the event, it can terminate
   * itself, and it can abort the whole group.
   *)


val add_event : event_system -> event -> unit
  (** Add an additional event. The event will be processed after the 
   * current list of events is done.
   *)


val clear : event_system -> group -> unit
  (** Terminate the whole group. This means that the handlers of the
   * group are not called any longer, and that all resources and actions
   * are removed. It is possible that there are pending events after
   * termination, but these will be usually be dropped because there is
   * no handler for them.
   *
   * When a group is terminated, it is not allowed to refer to the
   * group any longer. Functions will raise [Invalid_argument] if this
   * is tried nevertheless.
   *)


val run : event_system -> unit
  (** Starts the event loop. This means that the resources are watched,
   * and that events are generated, and that handlers are called.
   *
   * The event loop returns normally when there are not any resources
   * and not any events in the queue. The loop raises
   * {!Equeue.Out_of_handlers} if there are resources but no handlers
   * to process their events. It is possible that exceptions raised
   * from handlers fall through to the [run] call.
   *
   * After the exception is caught and processed, the event loop
   * can be restarted.
   *)

val is_running : event_system -> bool
  (** Whether the event loop is running *)

val once : event_system -> group -> float -> (unit -> unit)
  -> unit
  (** Arranges that the callback function is called once after the 
   * passed period of time (the [float] argument) has elapsed.
   *
   * The arrangement is member of the passed group. By clearing the
   * group, the timer is deleted, too.
   *)

val weak_once : event_system -> group -> float -> (unit -> unit) -> unit
  (** Same as [once], but the timer does not keep the event system running
      if it is the only remaining resource.
   *)

val epsilon : event_system -> (unit -> unit) -> unit
  (** The execution of the function is pushed onto the event queue
      (minimal delay)
   *)


(** {1 Debugging} *)


module Debug : sig
  (** This module controls debugging of all [Unixqueue_*] modules *)

  val enable : bool ref
    (** Enable {!Netlog} debugging *)

  val set_debug_mode : bool -> unit
    (** Sets [enable].
     *)

  val set_debug_target : Equeue.Debug.debug_target -> unit
    (** Restricts debugging to this target. 
     *)
end

(*
(** The status of the following functions is currently a bit unclear.
    When a Ocamlnet-wide logging concept is introduced, these functions
    probably disappear. They are used by other Ocamlnet modules to
    implement debug logging.
 *)

(* val exn_log : event_system ->
              ?suppressed:bool -> ?to_string:(exn -> string) -> 
              ?label:string -> exn -> unit *)
  (** Exceptions log: In event-based programming, it is sometimes not
   * possible to handle exceptions appropriately. It is also bad not
   * to handle them at all. For these cases, the exceptions log might
   * be an alternative: Instead of letting exceptions fall through to
   * the caller in an uncoordinated way, it is better to catch them
   * at the right moment, and to log them for further analysis.
   *
   * By default, [exn_log] does nothing. In debug mode, however, the
   * exceptions are reported as part of the debug log.
   *
   * {b Example:} A typical candidate for the exceptions log are
   * cleanup actions within exception handlers, e.g.
   *
   * {[try ...
   * with Processing_error ->
   *        try
   *          cleanup();   (* e.g. close file descriptors *)
   *          ...
   *        with nested_error ->
   *           Unixqueue.exn_log ~suppressed:true nested_error
   * ]}
   *
   * This is especially useful when the processing error is likely
   * to cause follow-up errors in the cleanup action. For normal
   * operation, one can ignore such errors, but for debugging it is
   * very useful to know that these exceptions happen.
   *
   * @param suppressed This flag indicates that the exception is not
   *   re-raised after logging. Just a hint for debugging. Default
   *   is [false].
   * @param to_string This function is called to convert the exception
   *   into a printable string. Default is [Printexc.to_string].
   * @param label The label is included in the log output. This is
   *   useful to describe where the log message is generated.
   *)

(* val debug_log : event_system -> ?label:string -> string -> unit*)
  (** Outputs a message in the debug log (when enabled).
   *
   * @param label The label is included in the log output. This is
   *   useful to describe where the log message is generated.
   *)

(* val set_debug_mode : bool -> unit *)
  (** Whether to output debug messages. Output goes to stderr.
   * Setting the debug mode implies setting Equeue's debug mode.
   *
   * The debug messages may really help debugging event systems. 
   * Unfortunately, some understanding of the internal processing
   * is required to interpret debug protocols.
   *)

(* val set_debug_target : Equeue.debug_target -> unit *)
  (** More fine-grained debug control *)
 *)
