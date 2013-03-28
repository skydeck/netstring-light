(* $Id: equeue.mli 1262 2009-08-31 18:14:21Z gerd $
 *
 * Event queues
 * written by Gerd Stolpmann
 *)

(**********************************************************************)
(***                                                                ***)
(***                   Simple event handling                        ***)
(***                                                                ***)
(**********************************************************************)

(** [Equeue] implements generic event queues. An {b event system} 
 * consists of an event queue, a list of event handlers, and an 
 * event source. The queue has FIFO semantics, i.e. the first event
 * is processed next, and new events are appended to its end. When
 * an event is delivered to an event handler, all handlers are tried
 * until a handler accepts the event (or the event is dropped if no
 * such handler can be found). See below how a handler can indicate
 * whether to accept or reject an event.
 *
 * When the queue is empty, the event source is called once. The source
 * can add events to the queue, in order to continue event processing.
 * If the queue remains empty, the execution of the event system
 * stops.
 *)

(** {b THREAD SAFETY}
 *
 * The module can be used in multi-threaded program provided no
 * event system is shared by several threads, or if so, access to
 * functions is serialized.
 *)

(* CHECK: This is a bit hard, and Unixqueue does not serialize on
 * this level. Either weaken the condition, or make the module
 * really thread-safe.
 *)


type 'a t
(** This is the type of an event system with events of type 'a
 *)

exception Reject
  (** May be raised by event handlers to reject events *)

exception Terminate
  (** May be raised by event handlers to accept events while terminating
    * themselves
   *)

exception Out_of_handlers
  (** Raised by [run] when the event source adds new events to the queue
   * but there are no event handlers to process them
   *)


val create : ?string_of_event:('a -> string) -> ('a t -> unit) -> 'a t
(** Creates a new event system that has an event source, but is 
 * otherwise empty. The argument of type ['a t -> unit] is the
 * event source. The source can call [add_event] to put new events
 * into the queue.
 *
 * @param string_of_event Optionally, one can pass a printer for events.
 *   This has only an effect for debugging output.
 *)

val add_event : 'a t -> 'a -> unit
(** Puts an event into the event queue of the system.
 *)

val add_handler : 'a t -> ('a t -> 'a -> unit) -> unit
(** Adds a handler to the list of handlers of the system.
 *
 * An event handler is called with the event system and the event as
 * arguments. The handler can return in various ways:
 *
 * - Return normally: This means that the event is accepted by the
 *   handler. No other handler will be asked to process the event.
 * - Raise [Reject]: The event is rejected by the handler. The other
 *   handlers are asked to process the event.
 * - Raise [Terminate]: The event is accepted, but the handler is
 *   terminated, i.e. will never be called again.
 * - Raise another exception: The event is deferred, and will be
 *   processed again in the future, but after the already queued events.
 *   Furthermore, the exception falls through to the caller of
 *   [run].
 *
 * The handler can add new events and new event handlers. The latter
 * will be activated when the next event is processed.
 *)

val run : 'a t -> unit
(** Running a system means that, unless the queue is empty, the events
 * at the time of the [run] invocation and all later added events are
 * gone through. Each event is presented to the handlers until one
 * handler accepts the event. Events rejected by all handlers are
 * dropped silently. If there is no pending event the default event
 * source is called once. If there are still no events the system stops
 * and returns. If there are events to process but no handlers which
 * can do them all events are silently dropped, and the default event
 * source is called once.
 *
 * The exception [Out_of_handlers] is raised if there are events but no
 * handlers after the event source has been called. This is considered
 * as a programming error, and would cause infinite looping if not
 * detected.
 *
 * Note that there is an implicit order among the handlers which is
 * simply the order the handlers have been added to the system. This
 * means that you can set a fallback handler which catches any unprocessed
 * event by adding it last.
 *
 * Note that the events are processed in the order they happen. There
 * is no mechanism to assign priorities to events.
 *
 * Handlers are allowed to raise arbitrary exceptions. Exceptions other
 * than Reject and Terminate are not caught, so the caller has to do this
 * if appropriate. It is possible to restart an event system by just
 * calling [run] again. 
 *)

val is_running : 'a t -> bool
  (** Returns whether the event loop is active *)


module Debug : sig
  type debug_target = [ `Any | `Process of int | `Thread of int ]

  val enable : bool ref
    (** Enable {!Netlog} debugging *)

  val set_debug_mode : bool -> unit
    (** Sets [enable].
     *)

  val set_debug_target : debug_target -> unit
    (** Restricts debugging to this target.
     *)

  (**/**)

  val test_debug_target : debug_target -> bool
    (* internal: returns whether Equeue would output a message *)
end
