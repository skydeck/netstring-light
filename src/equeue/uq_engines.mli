(* 
 * $Id: uq_engines.mli 1628 2011-06-14 20:52:23Z gerd $
 *)


(** An {b engine} performs a certain task in an autonomous way. Engines
 * are attached to a {!Unixqueue.event_system}, and do their task by
 * generating events for resources of the operating system, and 
 * by handling such events. Engines are in one of four states: They
 * may be still {b working}, they may be {b done}, they may be
 * {b aborted}, or they may be in an {b error} state. The three latter
 * states a called {b final states}, because they indicate that the
 * engine has stopped operation.
 *
 * It is possible to ask an engine to notify another object when it
 * changes its state. For simplicity, notification is done by invoking
 * a callback function, and not by issuing notification events.
 *
 * Effectively, engines provide a calculus for cooperative microthreading.
 * This calculus includes combinators for sequential execution and
 * synchronization. Moreover, it is easy to connect it with callback-style
 * microthreading - one can arrange callbacks when an engine is done, and
 * one can catch callbacks and turn them into engines.
 *)

(** {1 Exceptions} *)

exception Closed_channel
  (** Raised when a method of a closed channel object is called (only channel
   * methods count).
   *
   * This exception should be regarded as equivalent to
   * [Netchannels.Closed_channel], but need not be the same exception.
   *)

exception Broken_communication
  (** Some engines indicate this error when they cannot continue because the
   * other endpoint of communication signals an error.
   *
   * This exception is not raised, but used as argument of the [`Error]
   * state.
   *)

exception Watchdog_timeout
  (** Used by the watchdog engine to indicate a timeout. 
   *
   * This exception is not raised, but used as argument of the [`Error]
   * state.
   *)

exception Timeout
  (** Used by [input_engine] and [output_engine] to indicate timeouts *)

exception Addressing_method_not_supported
  (** Raised by [client_socket_connector] and [server_socket_acceptor] to
   * indicate that the passed address is not supported by the class.
   *)


exception Cancelled
  (** The callback function of a [multiplex_controller] is invoked with this
    * exception if the operation is cancelled.
   *)



(** {1 Engine definition} *)

type 't engine_state =
  [ `Working of int
  | `Done of 't
  | `Error of exn
  | `Aborted
  ]
  (** The type of states with result values of type ['t]:
   * - [`Working n]: The engine is working. The number [n] counts the number
   *   of events that have been processed.
   * - [`Done arg]: The engine has completed its task without errors. 
   *   The argument [arg] is the result value of the engine
   * - [`Error exn]: The engine has aborted because of an error. The
   *   argument [exn] describes the error as an exception.
   * - [`Aborted]: The engine has aborted because the [abort] method
   *   was called 
   *)

  (* `Done, `Error, and `Aborted are final states, i.e. the state will
   * not change again. 
   * CHECK: This is a bit strict, and hard to implement. At least `Done
   * must be final, but it is ok when `Error and `Aborted change, however
   * they must not change back to `Working.
   *)
;;


type 't final_state =
  [ `Done of 't
  | `Error of exn
  | `Aborted
  ]
    (** Same as [engine_state] without [`Working]. These are only the final
	states.
     *)


val string_of_state : 'a engine_state -> string
  (** For debug purposes: Returns a string describing the state *)


(** This class type defines the interface an engine must support. The
 * class parameter ['t] is the type of the result values (when the
 * engine goes to state [`Done]).
 *)
class type [ 't ] engine = object
  (** Requirements for engines *)

  method state : 't engine_state
    (** Returns the state of the engine *)
  method abort : unit -> unit
    (** Forces that the engine aborts operation. If the state is already
     * [`Done ], [`Aborted], or [`Error], this method must do nothing (you 
     * cannot abort an already finished engine).
     *)
  method request_notification : (unit -> bool) -> unit
    (** Requests notification about state changes.
     *
     * After the notification has been requested, the passed function must
     * be called whenever [state] changes its value (or might change
     * its value; it is allowed to call the notification function more
     * frequently than necessary). The function returns [true] if there
     * is still interest in notification, and [false] if notification must
     * be disabled; the function must not be called any longer in this
     * case.
     *
     * There can be any number of parallel active notifications. It is
     * allowed that a notification callback function requests further
     * notifications.
     *
     * If the callback raises an exception, this exception is
     * propagated to the caller of {!Unixqueue.run}.
     *)
  method event_system : Unixqueue.event_system
    (** Returns the event system the engine is attached to *)
end
;;


class ['t] delegate_engine : 't #engine -> ['t] engine
   (** Turns an engine value into a class *)

(** {1 Engines and callbacks} *)


val when_state : ?is_done:('a -> unit) ->
                 ?is_error:(exn -> unit) ->
                 ?is_aborted:(unit -> unit) ->
                 ?is_progressing:(int -> unit) ->
                 'a #engine ->
		   unit
  (** Watches the state of the argument engine, and arranges that one of
   * the functions is called when the corresponding state change is done. 
   * Once a final state is reached, the engine is no longer watched.
   * Note that [when_state] only observes future state changes.
   *
   * If one of the functions raises an exception, this exception is
   * propagated to the caller of {!Unixqueue.run}.
   * 
   * @param is_done The state transitions to [`Done]. The argument of
   *   [is_done] is the argument of the [`Done] state.
   * @param is_error The state transitions to [`Error]. The argument of
   *   [is_error] is the argument of the [`Error] state.
   * @param is_aborted The state transitions to [`Aborted].
   * @param is_progressing This function is called when the [`Working]
   *   state changes. The int argument is the new [`Working] arg.
   *)

class ['a] signal_engine : Unixqueue.event_system ->
object
  inherit ['a] engine
  method signal : 'a final_state -> unit
end
  (** [let se = new signal_engine esys]: The engine [se] remains in
      [`Working 0] until the method [se # signal x] is called. At this point
      [e] transitions to [x]. Any further call of [signal] does not
      have any effect.

      Also, if [se] is aborted, [signal] does not have any effect.
      
      The function [signal] may be called from a different thread.
      The signalling event is forwarded to the thread running the
      event system.
   *)

val signal_engine : Unixqueue.event_system -> 
                      'a engine * ('a final_state -> unit)
  (** [let (se, signal) = signal_engine esys]: Same as function *)



(** {1 Combinators} *)

(** The following combinators serve as the control structures to connect
    primitive engines with each other.
 *)

class ['a,'b] map_engine : map_done:('a -> 'b engine_state) ->
                           ?map_error:(exn -> 'b engine_state) ->
                           ?map_aborted:(unit -> 'b engine_state) ->
                           ?propagate_working : bool ->
                           'a #engine ->
			     ['b] engine
  (** The [map_engine] observes the argument engine, and when the
   * state changes to [`Done], [`Error], or [`Aborted], the corresponding
   * mapping function is called, and the resulting state becomes the state
   * of the mapped engine. If the engine is already in one of the
   * mentioned states, the map functions are also called (unlike
   * [when_state]).
   *
   * After the state change to [`Done], [`Error], or [`Aborted] has been
   * observed, the map engine detaches from the argument engine,
   * and no further state changes are recognized.
   *
   * The state [`Working] cannot be mapped to another state. It is an
   * error to map final states to [`Working].
   * The result type of the [map_*] functions is [engine_state] 
   * and not [final_state] because of historic reasons.
   *
   * If the mapped engine is aborted, this request will be forwarded
   * to the argument engine.
   *
   * If one of the mapping functions raises an exception, this causes
   * a transiton to [`Error].
   *
   * @param map_done Maps the [`Done] state of the argument engine to
   *   another state. The argument of [map_done] is the argument of the
   *   [`Done] state. Note that [map_done] is non-optional only because
   *   of typing. If it were optional, the type checker would infer ['a = 'b].
   * @param map_error Maps the [`Error] state of the argument engine to
   *   another state. The argument of [map_error] is the argument of the
   *   [`Error] state. 
   * @param map_aborted Maps the [`Aborted] state of the argument engine to
   *   another state.
   * @param propagate_working Specifies whether changes of the [`Working]
   *   state in the argument engine are propagated. Defaults to [true].
   *   If set to [false], the mapped engine remains in [`Working 0] until
   *   it transitions to a final state.
   *
   *)


val map_engine : map_done:('a -> 'b engine_state) ->
                 ?map_error:(exn -> 'b engine_state) ->
                 ?map_aborted:(unit -> 'b engine_state) ->
                 ?propagate_working : bool ->
                 'a #engine ->
                    'b engine
  (** Same as function *)

class ['a,'b] fmap_engine : 'a #engine -> 
                           ('a final_state -> 'b final_state) -> 
                               ['b] engine
  (** Similar to [map_engine] but different calling conventions: The
      mapping function is called when the argument engine reaches a
      final state, and this state can be mapped to another final state.
   *)
  
val fmap_engine : 'a #engine -> 
                   ('a final_state -> 'b final_state) -> 
                     'b engine
  (** Same as function

      After opening {!Uq_engines.Operators}, this is also available
      as operator [>>], e.g.

      {[ 
         e >>
           (function
             | `Done r -> ...
             | `Error error -> ...
             | `Aborted -> ...
           )
       ]}
   *)

class ['a] meta_engine : 'a #engine -> ['a final_state] engine
  (** maps the final state [s] to [`Done s] *)

val meta_engine : 'a #engine -> 'a final_state engine
  (** Same as function *)

class ['t] epsilon_engine : 
             't engine_state -> Unixqueue.event_system -> ['t] engine
  (** This engine transitions from its initial state [`Working 0] in one
   * step ("epsilon time") to the passed constant state. During this time
   * event processing will continue, so concurrently running engines can
   * make progress. For performance reasons, however, external resources
   * like file descriptors are not watched for new events.
   *
   * In previous versions of this library the class was called [const_engine].
   * However, this is not a constant thing. In particular, it is possible
   * that this engine is aborted, so the passed state is not reached.
   * To avoid programming errors because of the misnomer, this class has been
   * renamed.
   *)


val epsilon_engine : 
     't engine_state -> Unixqueue.event_system -> 't engine
  (** Same as function *)


class ['a, 'b] seq_engine : 'a #engine -> ('a -> 'b #engine) -> ['b] engine
  (** This engine runs two engines in sequential order. It is called
   * 
   * {[ let eng_s = new seq_engine eng_a f ]}
   *
   * When [eng_a] goes to the state [`Done arg], the function [f] is called to
   * obtain
   *
   * {[ let eng_b = f arg ]}
   *
   * [eng_b] runs until it is also in state [`Done].
   *
   * If [eng_a] or [eng_b] go to states [`Aborted] or [`Error], the
   * sequential engine [eng_s] does so, too. If [eng_s] is aborted,
   * this request will be forwarded to the currently active engine,
   * [eng_a] or [eng_b].
   *
   * If calling [f] results in an exception, this is handled as if [eng_a]
   * signaled an exception.
   *)


val seq_engine : 'a #engine -> ('a -> 'b #engine) -> 'b engine
  (** Same as function.
   *
   * After opening {!Uq_engines.Operators}, this is also available
   * as operator [++], e.g.
   * {[ e1 ++ (fun r1 -> e2) ]}
   * (when [e1] and [e2] are engines, and [r1] is the result of [e1]).
   *)

class ['a] stream_seq_engine : 'a -> ('a -> 'a #engine) Stream.t -> 
                               Unixqueue.event_system -> ['a] engine
  (** [let se = new stream_seq_engine x0 s esys]: The constructed engine [se]
    * fetches functions [f : 'a -> 'a #engine] from the stream [s], and
    * runs the engines obtained by calling these functions [e = f x] one
    * after the other. Each function call gets the result of the previous
    * engine as argument. The first call gets [x0] as argument.
    *
    * If one of the engines [e] transitions into an error or aborted state,
    * [se] will also do that. If [se] is aborted, this is passed down to
    * the currently running engine [e].
   *)


val stream_seq_engine : 'a -> ('a -> 'a #engine) Stream.t -> 
                         Unixqueue.event_system -> 'a engine
  (** Same as function *)


class ['a, 'b] sync_engine : 'a #engine -> 'b #engine -> ['a * 'b] engine
  (** This engine runs two engines in parallel, and waits until both
   * are [`Done] (synchronization). The product of the two [`Done] arguments 
   * is taken as the combined result.
   *
   * If one of the engines goes to the states [`Aborted] or [`Error],
   * the combined engine will follow this transition. The other,
   * non-aborted and non-errorneous engine is aborted in this case.
   * [`Error] has higher precedence than [`Aborted].
   *
   * If the combined engine is aborted, this request is forwarded
   * to both member engines.
   *)

val sync_engine : 'a #engine -> 'b #engine -> ('a * 'b) engine
  (** Same as function *)


class ['a,'b] msync_engine : 'a #engine list -> 
                             ('a -> 'b -> 'b) ->
                             'b ->
                             Unixqueue.event_system -> 
                             ['b] engine
  (** Multiple synchronization: 
      [let me = new msync_engine el f x0 esys] - Runs the engines in [el] in
      parallel, and waits until all are [`Done]. The result of [me] is
      then computed by folding the results of the part engines using
      [f], with an initial accumulator [x0].

      If one of the engines goes to the states [`Aborted] or [`Error],
      the combined engine will follow this transition. The other,
      non-aborted and non-errorneous engines are aborted in this case.
      [`Error] has higher precedence than [`Aborted].

      If calling [f] results in an exception, this is handled as if
      the part engine signals an error.

      If the combined engine is aborted, this request is forwarded
      to all member engines.
   *)

val msync_engine : 'a #engine list -> 
                   ('a -> 'b -> 'b) ->
                   'b ->
                   Unixqueue.event_system ->
                      'b engine
  (** Same as function *)			


class ['a ] delay_engine : float -> (unit -> 'a #engine) -> 
                           Unixqueue.event_system ->
                             ['a] engine
  (** [let de = delay_engine d f esys]: The engine [e = f()] is created
      after [d] seconds, and the result of [e] becomes the result of [de].
   *)


val delay_engine : float -> (unit -> 'a #engine) -> 
                   Unixqueue.event_system ->
                     'a engine
  (** Same as function *)

class ['a] timeout_engine : float -> exn -> 'a engine -> ['a] engine
  (** [timeout_engine d x e]: If the engine [e] finishes within [d]
      seconds, the result remains unchanged. If the engine takes longer,
      though, it is aborted, and the state transitions to
      [`Error x]
   *)

val timeout_engine : float -> exn -> 'a engine -> 'a engine
  (** Same as function *)

class watchdog : float -> 
                 'a #engine ->
                   [unit] engine
  (** A watchdog engine checks whether the argument engine makes
   * progress, and if there is no progress for the passed number of
   * seconds, the engine is aborted, and the watchdog state changes
   * to [`Error Watchdog_timeout].
   *
   * The current implementation is not very exact, and it may take
   * a little longer than the passed period of inactivity until the
   * watchdog recognizes inactivity.
   * 
   * If the argument engine terminates, the watchdog changes its state to
   * [`Done ()]
   *
   * Important note: The watchdog assumes that the [`Working] state 
   * of the target engine really counts events that indicate progress.
   * This does not work for:
   * - [poll_process_engine]: there is no way to check whether a subprocess
   *   makes progress
   * - [connector]: It is usually not possible to reflect the progress
   *   on packet level
   * - [listener]: It is usually not possible to reflect the progress
   *   on packet level
   *)

val watchdog : float -> 'a #engine -> unit engine
  (** Same as function *)


  (** A serializer queues up engines, and starts the next engine when the
      previous one finishes.
   *)
class type ['a] serializer_t =
object

  method serialized : (Unixqueue.event_system -> 'a engine) -> 'a engine
    (** [let se = serialized f]: Waits until all the previous engines reach
	a final state, and then runs [e = f esys].

        [se] enters a final state when [e] does.
     *)
end

class ['a] serializer : Unixqueue.event_system -> ['a] serializer_t
  (** Creates a serializer *)

val serializer : Unixqueue.event_system -> 'a serializer_t
  (** Same as function *)


(** A prioritizer allows to prioritize the execution of engines: At any
    time, only engines of a certain priority [p] can be executed. If an
    engine with a higher priority [ph] wants to start, it prevents further
    engines with priority level [p] from being started until the higher
    prioritized engines with level [ph] are done. On the same priority level,
    there is no limit for the number of executed engines.

    Here, higher priorities have lower numbers.
 *)
class type ['a] prioritizer_t =
object
  method prioritized : (Unixqueue.event_system -> 'a engine) -> int -> 'a engine
    (** [let pe = prioritized f p]: Queues up [f] on priority level [p].
	The engine  [e = f esys] can start when there is no waiting
	engine on a higher priority level (i.e. with a number less than
	[p]), and all running engines on lower priority levels are done.

	[pe] enters a final state when [e] does.
     *)
end

class ['a] prioritizer : Unixqueue.event_system -> ['a] prioritizer_t
  (** Creates a prioritizer *)

val prioritizer : Unixqueue.event_system -> 'a prioritizer_t
  (** Same as function *)



(** A cache contains a mutable value that is obtained by running an
    engine.
 *)
class type ['a] cache_t =
object
  method get_engine : unit -> 'a engine
    (** Requests the value. If it is not already in the cache, 
        the engine for getting the value is started, and it is waited
	until the value is available.
     *)
  method get_opt : unit -> 'a option
    (** Returns the cached value if available *)
  method put : 'a -> unit
    (** Puts a value immediately into the cache. It replaces an existing
	value. If it is currently tried to obtain a new value by running
	an engine, this engine is kept running, and [get_engine] will
	return its result. Only future calls of [get_engine] will return
	the value just put into the cache.
     *)
  method invalidate : unit -> unit
    (** Invalidates the cache - if a value exists in the cache, it is removed.
	If in the future the cache value is requested via [get_engine] 
        the engine will be started anew to get the value.

        Note that (as for [put]) any already running [get_engine] is not
	interrupted.
     *)
  method abort : unit -> unit
    (** Any engine running to get the cache value is aborted, and the contents
	of the cache are invalidated. Note that also the engines returned
	by [get_engine] are aborted.
     *)
end

class ['a] cache : (Unixqueue.event_system -> 'a engine) ->
                   Unixqueue.event_system ->
                     ['a] cache_t
  (** [new cache f esys]: A cache that runs [f esys] to obtain values *)

val cache : (Unixqueue.event_system -> 'a engine) ->
            Unixqueue.event_system ->
              'a cache_t
  (** Same as function *)


class ['t] engine_mixin : 't engine_state -> Unixqueue.event_system ->
object
  method state : 't engine_state
  method private set_state : 't engine_state -> unit
  method request_notification : (unit -> bool) -> unit
  method private notify : unit -> unit
  method event_system : Unixqueue.event_system
end
  (** A useful class fragment that implements [state] and 
    * [request_notification].
   *)							     

(** Handy operators: [++], [>>], and [eps_e] *)
module Operators : sig
  (** The most important operators. This module should be opened. *)

  val ( ++ ) : 'a #engine -> ('a -> 'b #engine) -> 'b engine
    (** Another name for [seq_engine]. Use this operator to run engines in
	sequence:

	{[
	    e1 ++ (fun r1 -> e2) ++ (fun r2 -> e3) ++ ...
	]}

	Here [rK] is the result of engine [eK].
     *)

  val ( >> ) : 'a #engine -> 
                   ('a final_state -> 'b final_state) -> 
                     'b engine
    (** Another name for [fmap_engine]. Use this operator to map the
	final value of an engine:

	{[
	    e >> (function `Done x -> ... | `Error e -> ... | `Aborted -> ...)
	]}

     *)

  val eps_e : 't engine_state -> Unixqueue.event_system -> 't engine
    (** Same as [epsilon_engine] *)

end


(** {1 Basic I/O engines} *)

class poll_engine : ?extra_match:(exn -> bool) ->
                    (Unixqueue.operation * float) list -> 
		    Unixqueue.event_system ->
object
  inherit [Unixqueue.event] engine

  (** {1 Additional methods} *)

  method restart : unit -> unit
    (** Activate the engine again when it is already in a final state.
     * This method violates the engine protocol, and should be used
     * with care; it is not allowed to leave a final state.
     *
     * The notification lists are kept, but note that observers often
     * detach when final states are reached. This may cause problems.
     *)

  method group : Unixqueue.group
    (** Returns the group the engine is member of *)

end ;;
  (** This engine waits until one of the passed operations can be 
   * carried out, or until one of the operations times out. 
   * In these cases, the state of the engine  changes to [`Done ev], where 
   * [ev] is the corresponding event.
   *
   * The argument list enumerates the operations to watch for. For every
   * operation there may be a positive timeout value, or a negative number
   * to indicate that no timeout is specified.
   * 
   * After one event has been caught, the engine terminates operation.
   * The method [restart] can be called to activate it again (with the
   * same event condition, and the same notification list). See the
   * description of [restart] for possible problems.
   *
   * @param extra_match This function is called when an [Extra] event is
   *   found. If the function returns [true] for the argument exception
   *   of [Extra], the event is caught; otherwise it is rejected.
   *)


class ['a] input_engine : (Unix.file_descr -> 'a) ->
                          Unix.file_descr -> float -> Unixqueue.event_system ->
                          ['a] engine
  (** Generic input engine for reading from a file descriptor:
      [let e = new input_engine f fd tmo] - Waits until the file descriptor
      becomes readable, and calls then [let x = f fd] to read from the
      descriptor. The result [x] is the result of the engine.

      If the file descriptor does not become readable within [tmo] seconds,
      the resulting engine transitions to [`Error Timeout].

      Use this class to construct engines reading via [Unix.read] or
      comparable I/O functions:

      {[
      let read_engine fd tmo esys =
        new input_engine (fun fd ->
                            let buf = String.create 4096 in
                            let n = Unix.read fd buf 0 (String.length buf) in
                            String.sub buf 0 n
                         )
                         fd tmo esys
      ]}

      This engine returns the read data as string.

      See also {!Uq_io.input_e} for a more generic way of reading with
      engines.
   *)

class ['a] output_engine : (Unix.file_descr -> 'a) ->
                           Unix.file_descr -> float -> Unixqueue.event_system ->
                           ['a] engine
  (** Generic output engine for writing to a file descriptor:
      [let e = new output_engine f fd tmo] - Waits until the file descriptor
      becomes writable, and calls then [let x = f fd] to write to the
      descriptor. The result [x] is the result of the engine.

      If the file descriptor does not become writable within [tmo] seconds,
      the resulting engine transitions to [`Error Timeout].

      Use this class to construct engines writing via [Unix.single_write] or
      comparable I/O functions:

      {[
      let write_engine fd s tmo esys =
        new output_engine (fun fd ->
                             Unix.single_write fd s 0 (String.length s)
                          )
                          fd tmo esys
      ]}

      This engine returns the number of written bytes.

      See also {!Uq_io.output_e} for a more generic way of writing with
      engines.
   *)

class poll_process_engine : ?period:float ->
                            pid:int -> 
                            Unixqueue.event_system ->
			      [Unix.process_status] engine ;;
  (** {b This class is deprecated!} Use the classes in {!Shell_uq} instead.
   * 
   * This engine waits until the process with the ID [pid] terminates.
   * When this happens, the state of the engine changes to 
   * [`Done], and the argument of [`Done] is the process status.
   *
   * The engine does not catch stopped processes.
   *
   * The engine checks the process status every [period] seconds, and
   * whenever there is a [Signal] event on the queue. The idea of the
   * latter is that the user of this engine can increase the responsiveness
   * by defining a signal handler for SIGCHLD signals (the handler need
   * not to perform any special action, it must just be defined). When
   * the sub process terminates, a SIGCHLD signal is sent to the current
   * process. If the event loop happens to wait for new conditions (which
   * is usually very likely), a [Signal] event will be generated, and
   * the engine will check the process status very soon. Note that it is
   * not guaranteed that a terminating process triggers a [Signal] event,
   * although it is very likely.
   *
   * You can define an empty SIGCHLD handler with:
   * 
   * {[ Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> ())) ]}
   *
   * @param period Every [period] seconds the process status is checked.
   *   Defaults to 0.1 seconds.
   *)

(** {2 More I/O}

    The module {!Uq_io} provides a bunch of functions to read and write
    data via various "devices". All these functions return engines, and
    are easy to use. Devices can be file descriptors, but also other
    data structures. In particular, there is also support for buffered I/O
    and for reading line-by-line from an input device.

 *)


(** {1 Transfer engines} *)

(** Transfer engines copy data between file descriptors. This kind
    of engine is likely to be declared as deprecated in
    the future. If possible, one should use multiplex controllers
    (see below), and for copying streams the generic copier 
    {!Uq_io.copy_e} is a better choice.

    The pure types [async_in_channel] and [async_out_channel] have been
    proven to be useful for bridging with {!Netchannels}.
 *)

(** An asynchrounous output channel provides methods to output data to
 * a stream descriptor. It is based on [raw_out_channel], which is 
 * defined by the Ocamlnet module [Netchannels] (see there for an 
 * introduction into the idea of using objects as I/O channels).
 * An asynchronous channel can indicate that there is no space in the
 * output buffer. Furthermore, one can request notification in the case
 * that there is no space or again space in the output buffer.
 *)
class type async_out_channel = object

  (** {1 Methods from [raw_out_channel] } *)

  method output : string -> int -> int -> int
    (** [output s k n]: Writes the substring of [s] beginning at index
     * [k] with length [n] into the channel. The channel is free to
     * accept only a portion of the string (or even nothing), and 
     * returns the number of bytes it accepts.
     *)

  method close_out : unit -> unit
    (** Closes the channel *)

  method pos_out : int
    (** Returns the number of characters output into the channel *)

  method flush : unit -> unit
    (** Flushes the channel. Asynchronous channels usually ignore
     * flush requests. A potential meaning of flushing could be that
     * no more data are accepted until the current buffer is completely
     * processed. Implementing this is optional.
     *)

  (** {1 Additional control methods} *)

  method can_output : bool
    (** Whether output is possible, i.e. the output method accepts at least
     * one byte
     *)

  method request_notification : (unit -> bool) -> unit
    (** After the notification has been requested, the passed function is
     * be called whenever [can_output] changes its value (or might change
     * its value). The function returns [true] if there is still interest
     * in notification, and [false] if notification must be disabled.
     *
     * There can be any number of parallel active notifications. It is
     * allowed that a notification callback requests further notifications.
     *)
end
;;


(** An asynchrounous input channel provides methods to input data from
 * a stream descriptor. It is based on [raw_in_channel], which is 
 * defined by the Ocamlnet module [Netchannels] (see there for an 
 * introduction into the idea of using objects as I/O channels).
 * An asynchronous channel can indicate that there is no data in the
 * input buffer. Furthermore, one can request notification in the case
 * that there is no data or again data in the input buffer.
 *)
class type async_in_channel = object

  (** {1 Methods from [raw_in_channel] } *)

  method input : string -> int -> int -> int
    (** [input s k n]: Reads channel data into the substring of [s]
     * beginning at index [k] with length [n]. The channel is free to
     * fill only a portion of the string (or even nothing). The method 
     * returns the number of bytes actually read.
     *
     * The exception [End_of_file] indicates that the end of the channel
     * is reached. The return value [0], however, means that no data
     * could be read.
     *)

  method close_in : unit -> unit
    (** Closes the channel *)

  method pos_in : int
    (** Returns the number of characters read from the channel *)


  (** {1 Additional control methods} *)

  method can_input : bool
    (** Whether input is possible, i.e. the input method gets at least
     * one byte, or can signal [End_of_file].
     *)

  method request_notification : (unit -> bool) -> unit
    (** After the notification has been requested, the passed function is
     * be called whenever [can_input] changes its value (or might change
     * its value). The function returns [true] if there is still interest
     * in notification, and [false] if notification must be disabled.
     *
     * There can be any number of parallel active notifications. It is
     * allowed that a notification callback requests further notifications.
     *)
end
;;


class pseudo_async_out_channel : 
         #Netchannels.raw_out_channel -> async_out_channel
  (** Takes a {!Netchannels.raw_out_channel} as an asynchronous channel.
      It is always possible to output to this channel.
   *)

class pseudo_async_in_channel : 
         #Netchannels.raw_in_channel -> async_in_channel
  (** Takes a {!Netchannels.raw_in_channel} as an asynchronous channel.
      It is always possible to input from this channel.
   *)


class receiver : src:Unix.file_descr ->
                 dst:#async_out_channel ->
		 ?close_src:bool ->        (* default: true *)
		 ?close_dst:bool ->        (* default: true *)
		 Unixqueue.event_system ->
		   [unit] engine ;;
  (** This engine copies all data from the [src] file descriptor to the
   * [dst] output channel. The engine attaches immediately to the 
   * event system, and detaches automatically. 
   *
   * By default, both the file descriptor and the output channel
   * are closed when the engine stops operation, either successfully
   * or because of an error. 
   *
   * The semantics of the engine is undefined if [src] is not a
   * stream-oriented descriptor.
   *
   * The engine goes to [`Error] state when either reading from [src]
   * or writing to [dst] raises an unexpected exception.
   *
   * For every file descriptor event, the state is advanced from
   * [`Working n] to [`Working (n+1)].
   *
   * TODO: This class cannot yet cope with Win32 named pipes.
   *
   * @param close_src Whether to close [src] when the engine stops
   *   (default: [true])
   * @param close_dst Whether to close [dst] when the engine stops
   *   (default: [true])
   *)


class sender : src:#async_in_channel ->
               dst:Unix.file_descr ->
	       ?close_src:bool ->        (* default: true *)
	       ?close_dst:bool ->        (* default: true *)
	       Unixqueue.event_system ->
	         [unit] engine ;;
  (** This engine copies all data from the [src] input channel to the
   * [dst] file descriptor. The engine attaches immediately to the 
   * event system, and detaches automatically. 
   *
   * By default, both the file descriptor and the output channel
   * are closed when the engine stops operation, either successfully
   * or because of an error. 
   *
   * The semantics of the engine is undefined if [dst] is not a
   * stream-oriented descriptor.
   *
   * The engine goes to [`Error] state when either reading from [src]
   * or writing to [dst] raises an unexpected exception.
   *
   * For every file descriptor event, the state is advanced from
   * [`Working n] to [`Working (n+1)].
   *
   * TODO: This class cannot yet cope with Win32 named pipes.
   *
   * @param close_src Whether to close [src] when the engine stops
   *   (default: [true])
   * @param close_dst Whether to close [dst] when the engine stops
   *   (default: [true])
   *)


class type async_out_channel_engine = object
  inherit [ unit ] engine
  inherit async_out_channel
end
;;
(** Combination of engine + async_out_channel *)


class type async_in_channel_engine = object
  inherit [ unit ] engine
  inherit async_in_channel
end
;;
(** Combination of engine + async_in_channel *)


class output_async_descr : dst:Unix.file_descr ->
                           ?buffer_size:int ->
			   ?close_dst:bool ->    (* default: true *)
                           Unixqueue.event_system ->
                             async_out_channel_engine
  (** This engine implements an [async_out_channel] for the output
   * descriptor [dst]. The engine provides an internal buffer to
   * reduce the number of blocked output operations; by default there
   * is even no limit for the growth of the buffer, and because of this
   * the channel never blocks ([can_output] is always [true]).
   *
   * The engine attaches immediately to the event system, and detaches 
   * automatically. By default, the file descriptor is closed when the
   * engine stops operation, either successfully or because of an
   * error. 
   *
   * If the buffer is full, the class accepts no more data until
   * there is again free space in the buffer. This means that writers
   * must be prepared that [can_output] returns [false], and that
   * the [output] method returns 0. The buffer can only get "full"
   * if the [buffer_size] argument is passed.
   *
   * The notification mechanism is shared by the "engine nature" and
   * by the "channel nature" of this class: If either the [state] or
   * [can_output] change their values, the notification callbacks
   * are invoked.
   *
   * The semantics of the engine is undefined if [dst] is not a
   * stream-oriented descriptor.
   *
   * TODO: This class cannot yet cope with Win32 named piped.
   *
   * @param buffer_size Limits the size of the buffer
   * @param close_dst Whether to close [dst] when the engine stops
   *    (default: [true])
   *)


class input_async_descr : src:Unix.file_descr ->
                          ?buffer_size:int ->
			  ?close_src:bool ->    (* default: true *)
                          Unixqueue.event_system ->
                             async_in_channel_engine
 (** The corresponding class for asynchronous input channels.
   *
   * TODO: This class cannot yet cope with Win32 named piped.
  *)

type copy_task =
    [ `Unidirectional of (Unix.file_descr * Unix.file_descr)
    | `Uni_socket of (Unix.file_descr * Unix.file_descr)
    | `Bidirectional of (Unix.file_descr * Unix.file_descr)
    | `Tridirectional of (Unix.file_descr * Unix.file_descr * Unix.file_descr) 
    ]
  (** Specifies the task the [copier] class has to do:
   *
   * - [`Unidirectional(src,dst)]: Data from [src] are copied to [dst].
   *   EOF of [src] causes that both descriptors are closed.
   * - [`Uni_socket(src,dst)]: Data from [src] are copied to [dst].
   *   EOF of [src] causes that [dst] is shut down for sending; all descriptors
   *   remain open. It is required that [dst] is a socket.
   * - [`Bidirectional(bi1,bi2)]: Data from [bi1] are copied to [bi2],
   *   and data from [bi2] are copied to [bi1]. EOF of one descriptor
   *   causes that the other descriptor is shut down for sending.
   *   When both descriptors are at EOF, both are closed.
   *   It is required that [bi1] and [bi2] are sockets.
   * - [`Tridirectional(bi,dst,src)]: Data from [bi] are copied to [dst],
   *   and data from [src] are copied to [bi] (i.e. a bidirectional
   *   descriptor is split up into two unidirectional descriptors). 
   *   EOF of [bi] causes that [dst] is closed. EOF of [src] causes
   *   that [bi] is shut down for sending. EOF in both directions 
   *   causes that all descriptors are closed. It is required that
   *   [bi] is a socket.
   *)



class copier : copy_task ->
               Unixqueue.event_system ->
		 [unit] engine
  (** This engine copies data between file descriptors as specified by
   * the [copy_task] argument.
   *
   * The task is done when all input descriptors are at EOF. See
   * the description of [copy_task] for details, especially whether
   * the descriptors are closed or not.
   *
   * On error or abort, the descriptors are only closed if they
   * had been closed on regular EOF.
   *
   * The semantics of the engine is undefined if one of the descriptors
   * is not stream-oriented.
   *
   * TODO: This class cannot yet cope with Win32 named piped.
   *)


(** {1 Socket engines} *)

(** Note that Win32 named pipes are also supported by the following
    API's, although they are not sockets. These pipes have a feature
    set comparable to Unix domain sockets.
 *)


type inetspec =
  [ `Sock_inet of (Unix.socket_type * Unix.inet_addr * int)
  | `Sock_inet_byname of (Unix.socket_type * string * int)
  ]

type sockspec =
  [ inetspec
  | `Sock_unix of (Unix.socket_type * string)
  ]
  (** Extended names for socket addresses. Currently, these naming schemes
   * are supported:
   * - [`Sock_unix(stype,path)]: Names the Unix domain socket at [path].
   *   The socket type [stype] is an auxiliary piece of information, but
   *   not a distinguishing part of the name. [path = ""] refers to 
   *   anonymous sockets. Otherwise, the [path] must be an absolute path name.
   * - [`Sock_inet(stype,addr,port)]: Names the Internet socket of type
   *   [stype] bound to the IP address [addr] and the [port].
   *   If [stype = Unix.SOCK_STREAM], a TCP socket is meant, and if 
   *   [stype = Unix.SOCK_DGRAM], a UDP socket is meant. It is allowed
   *   that [addr = Unix.inet_addr_any]. If [port = 0], the name is to
   *   be considered as incomplete.
   * - [`Sock_inet_byname(stype,name,port)]: Names the Internet socket of
   *   type [stype] bound to the IP address corresponding to the 
   *   [name], and bound to the [port]. It is unspecified which naming
   *   service is used to resolve [name] to an IP address, and how it is
   *   used. If the [name] cannot be resolved, no socket is meant; this
   *   is usually an error. [stype] is interpreted as for [`Sock_inet].
   *   If [port = 0], the name is to be considered as incomplete.
   *
   * It is currently not possible to name IP sockets that are bound to
   * several IP addresses but not all IP addresses of the host. 
   *)
;;

val sockspec_of_sockaddr : Unix.socket_type -> Unix.sockaddr -> sockspec
  (** Converts a normal socket address to the extended form *)

val sockspec_of_socksymbol : Unix.socket_type -> Netsockaddr.socksymbol ->
                               sockspec
  (** Converts a {!Netsockaddr.socksymbol} to this form *)


(** {2 Client sockets} *)

type connect_address =
    [ `Socket of sockspec * connect_options
    | `Command of string * (int -> Unixqueue.event_system -> unit)
    | `W32_pipe of Netsys_win32.pipe_mode * string
    ]
  (** Specifies the service to connect to:
   * 
   * {ul
   * {- [`Socket(addr,opts)]: Connect to the passed socket address}
   * {- [`Command(cmd,handler)]: The [cmd] is started with the shell, 
   *   and [stdin] and [stdout] are used to transfer data to the
   *   process and from the process, respectively. Only [SOCK_STREAM]
   *   type is supported. Note that the passed file descriptors are
   *   normal pipes, not sockets (so the descriptors can be individually
   *   closed).
   *
   *   There is not any kind of error detection, so the command should
   *   be failsafe. [stderr] of the command is connected with [stderr] of
   *   the caller process.
   *
   *   No provisions are taken to wait for the process; this is the
   *   task of the caller. After the process has been started, the
   *   [handler] is invoked with the process ID and the event system
   *   to give the caller a chance to arrange that the process will be
   *   waited for.}
   * {- [`W32_pipe(mode,name)]: A Win32 named pipe}
   * }
   *)


and connect_options =
    { conn_bind : sockspec option;
        (** Bind the connecting socket to this address (same family as the
	 * connected socket required). [None]: Use an anonymous port.
	 *)
    }
;;

(* CHECK: inetd-style command call (stdin,stdout are sockets) *)


val default_connect_options : connect_options;;
  (** Returns the default options *)


type connect_status =
    [ `Socket of Unix.file_descr * sockspec
    | `Command of Unix.file_descr * int
    | `W32_pipe of Unix.file_descr
    ]
  (** This type corresponds with {!Uq_engines.connect_address}: An engine
   * connecting with an address `X will return a status of `X.
   *
   * - [`Socket(fd,addr)]: [fd] is the client socket connected with the
   *   service. [addr] is the socket address of the client that must be
   *   used by the server to reach the client.
   * - [`Command(fd, pid)]: [fd] is the Unix domain socket connected with
   *   the running command. [pid] is the process ID.
   * - [`W32_pipe fd]: [fd] is the proxy descriptor of the connected
   *   Win32 named pipe endpoint. See {!Netsys_win32} how to get the
   *   [w32_pipe] object to access the pipe. The proxy descriptor {b cannot}
   *   be used for I/O.
   *)
;;


val client_endpoint : connect_status -> Unix.file_descr ;;
  (** Returns the client endpoint contained in the [connect_status] *)

val client_socket : connect_status -> Unix.file_descr ;;
  (** For backward compatibility. {b Deprecated name} for [client_endpoint] *)



(** This class type provides engines to connect to a service. In order
 * to get and activate such an engine, call [connect].
 *)
class type client_endpoint_connector = object
  method connect : connect_address -> 
                   Unixqueue.event_system ->
		     connect_status engine
    (** Instantiates an engine that connects to the endpoint given by the
     * [connect_address] argument. If successful, the state of the engine
     * changes to [`Done(status)] where [status] contains the socket 
     * details. The connection is established in the background.
     *
     * The type of status will correspond to the type of connect address
     * (e.g. a [`Socket] address will return a [`Socket] status).
     *
     * The close-on-exec flag of the created socket descriptor is always set.
     * The socket descriptor is always in non-blocking mode.
     *)
end
;;


class type client_socket_connector = client_endpoint_connector
  (** For backward compatibility. {b Deprecated name} for 
      [client_endpoint_connector]
   *)


val connector : ?proxy:#client_socket_connector ->
                connect_address ->
                Unixqueue.event_system ->
	          connect_status engine 
  (** This engine connects to a socket as specified by the [connect_address],
   * optionally using the [proxy], and changes to the state
   * [`Done(status)] when the connection is established.
   *
   * If the [proxy] does not support the [connect_address], the class 
   * will raise [Addressing_method_not_supported].
   *
   * The descriptor [fd] (part of the [connect_status]) is in non-blocking mode,
   * and the close-on-exec flag is set.
   * It is the task of the caller to close this descriptor.
   *
   * The engine attaches automatically to the event system, and detaches
   * when it is possible to do so. This depends on the type of the
   * connection method. For direct socket connections, the engine can
   * often detach immediately when the conection is established. For proxy
   * connections it is required that the engine
   * copies data to and from the file descriptor. In this case, the
   * engine detaches when the file descriptor is closed.
   *
   * It is possible that name service queries block execution.
   * 
   * If name resolution fails, the engine will enter
   * [`Error(Uq_resolver.Host_not_found name)]. This is {b new since
   * Ocamlnet-3.3} - before this version, the exception was simply
   * [Not_found].
   *)

(** {b Example} of using [connector]: This engine [e] connects to the
    "echo" service as provided by inetd, sends a line of data to it,
    and awaits the response.

    {[
	let e =
	  Uq_engines.connector
	    (`Socket(`Sock_inet_byname(Unix.SOCK_STREAM, "localhost", 7),
		     Uq_engines.default_connect_options))
	    esys
	  ++ (fun cs ->
		match cs with
		  | `Socket(fd,_) ->
		      let mplex =
			Uq_engines.create_multiplex_controller_for_connected_socket
			  ~supports_half_open_connection:true
			  fd esys in
		      let d_unbuf = `Multiplex mplex in
		      let d = `Buffer_in(Uq_io.create_in_buffer d_unbuf) in
		      Uq_io.output_string_e d_unbuf "This is line1\n"
		      ++ (fun () ->
			    Uq_io.input_line_e d 
			    ++ (fun s ->
				  print_endline s;
				  eps_e (`Done()) esys
			       )
			 )
		  | _ -> assert false
	     )
    ]}

 *)


(** {2 Server sockets} *)

type listen_address =
    [ `Socket of sockspec * listen_options
    | `W32_pipe of Netsys_win32.pipe_mode * string * listen_options
(* ---
 * `Command: Does not work, as the command has no way to tell us when
 * a new connection is accepted. (It should output something for that
 * purpose; is there a standard protocol for this?)
 * Maybe what we really need is a listen_option that filters the whole
 * stream through a command (bidirectional filter).
 * ---
    | `Command of string * (int -> Unixqueue.event_system -> unit)
	(* A command (1st arg) is started with the shell, and it is expected
	 * that the command accepts one connection, and that stdin and stdout
	 * are used to transfer data to the process and from the process,
	 * respectively. Only SOCK_STREAM type is supported. Note that the
	 * passed file descriptors are normal pipes, not sockets (so the
	 * descriptors can be individually closed).
	 *
	 * There is not any kind of error detection, so the command should
	 * be failsafe. stderr of the command is connected with stderr of
	 * the caller process.
	 *
	 * No provisions are taken to wait for the process; this is the
	 * task of the caller. After the process has been started, the
	 * 2nd argument is invoked with the process ID and the event system
	 * to give the caller a chance to arrange that the process will be
	 * waited for.
	 *)
 *)
    ]
  (** Specifies the resource to listen on:
   * 
   * - [`Socket(addr,opts)]: It is listened on a socket with address [addr]
   * - [`W32_pipe(mode,name,opts)]: It is listened on a pipe server with
   *   [name] which accepts pipe connections in [mode].
   *)


and listen_options =
    { lstn_backlog : int;    (** The length of the queue of not yet accepted
			      * connections.
			      *)
      lstn_reuseaddr : bool; (** Whether to allow that the address can be
			      * immediately reused after the previous listener
			      * has its socket shut down. (Only for Internet
                              * sockets.)
			      *)
    }
;;


val default_listen_options : listen_options;;
  (** Returns the default options *)

(** This class type is for service providers that listen for connections.
 * By calling [accept], one gets an engine that waits for the next
 * connection, and establishes it.
 *
 * There are services that can only accept one connection for a 
 * certain contact address. In this case [accept] must only be called
 * once. Normally, services can accept any number of connections
 * (multiplexing), and it is allowed to call [accept] again after
 * the previous accept engine was successful.
 *)
class type server_endpoint_acceptor = object

  method server_address : connect_address
    (** The contact address under which the clients can establish new
     * connections with this server.
     *)

  method multiple_connections : bool
    (** Whether it is possible to accept multiple connections *)

  method accept : unit -> (Unix.file_descr * inetspec option) engine
    (** Instantiates an engine that accepts connections on the listening
     * endpoint. 
     *
     * If the connection is successfully established, the state of the engine
     * changes to [`Done(fd,addr)] where [fd] is the connected file descriptor,
     * and where [addr] (if not-[None]) is the endpoint address of the 
     * connecting client (from the server's perspective). Such addresses are
     * only supported for Internet endpoints. If a proxy is used to accept
     * the connections, the returned address is that from the proxy's 
     * view, and usually different from what [Unix.getpeername] returns.
     *
     * The close-on-exec flag of the created endpoint descriptor is always set.
     * The endpoint descriptor is always in non-blocking mode.
     * 
     * It is allowed to shut down [fd] for sending, and it is required to
     * close [fd] after all data transfers have been performed.
     *
     * A call of [accept] allows it only to establish one connection at a time.
     * However, it is allowed to call [accept] several times to accept several
     * connections, provided the acceptor supports this (returned by
     * [multiple_connections]). It is only allowed to call [accept] again
     * when the previous engine was successful.
     *)

  method shut_down : unit -> unit
    (** The server endpoint is shut down such that no further connections
     * are possible. It is required to call this method even for acceptors
     * that do not support multiple connections. It is also required to
     * call this method when an [accept] was not successful.
     *
     * If there is a engine waiting for connections, it is aborted.
     *)
end
;;


class type server_socket_acceptor = server_endpoint_acceptor
  (** For backward compatibility. {b Deprecated name} for 
      [server_endpoint_acceptor] 
   *)


class direct_acceptor : 
        ?close_on_shutdown:bool ->
        ?preclose:(unit->unit) ->
        Unix.file_descr -> Unixqueue.event_system -> 
          server_endpoint_acceptor
(** An implementation of [server_endpoint_acceptor] for sockets and Win32
    named pipes. For sockets, the passed descriptor must be the master
    socket. For Win32 named pipes, the passed descriptor must be the
    proxy descriptor of the pipe server..
 *)
   
class direct_socket_acceptor :
        Unix.file_descr -> Unixqueue.event_system -> 
          server_endpoint_acceptor
  (** For backward compatibility. {b Deprecated name} for [direct_acceptor] *)



(** This class type represents factories for service providers *)
class type server_endpoint_listener = object

  method listen : listen_address ->
                  Unixqueue.event_system ->
		    server_endpoint_acceptor engine
    (** Instantiates an engine that listens for connections on the socket given
     * by the [listen_address] argument. If successful, the state of the engine
     * changes to [`Done(acc)] where [acc] is the acceptor object guiding
     * you through further operation of the socket (see above). 
     *)

end
;;

class type server_socket_listener = server_endpoint_listener
  (** For backward compatibility. {b Deprecated name} for
      [server_endpoint_listener] 
   *)



val listener : ?proxy:#server_socket_listener ->
               listen_address ->
	       Unixqueue.event_system ->
		 server_socket_acceptor engine ;;
  (** This engine creates a server socket listening on the [listen_address].
   * If passed, the [proxy] is used to create the server socket.
   *
   * On success, the engine goes to state [`Done acc], where [acc] is
   * the acceptor object (see above). The acceptor object can be used
   * to accept incoming connections.
   *)


(** {2 Datagrams} *)

type datagram_type =
    [ `Unix_dgram
    | `Inet_udp
    | `Inet6_udp
    ]
  (** - [`Unix_dgram]: Datagrams over Unix domain sockets
   *  - [`Inet_udp]:   Internet v4 UDP protocol
   *  - [`Inet6_udp]:   Internet v6 UDP protocol
   *)
;;


(** A [wrapped_datagram_socket] allows datagrams to be sent via proxies.
 * It provides versions of the [sendto] and [recvfrom] functions that
 * use extended socket names (which are proxy-friendly).
 *)
class type wrapped_datagram_socket =
object
  method descriptor : Unix.file_descr
    (** The underlying file descriptor. This descriptor must not be used
     * to transfer data ([Unix.send(to)], [Unix.recv(from)], etc.), because the
     * descriptor may be connected with a proxy, and the socket addresses
     * may be wrong that are used by the low-level socket functions.
     * The right way is to use the methods below to transfer data. It is
     * allowed, however, to pass the descriptor to [Unix.select], and to check
     * whether transfers are possible. It is also allowed to set or clear
     * non-blocking mode, and the close-on-exec flag, and to modify the
     * socket options.
     *)
  method sendto : 
    string -> int -> int -> Unix.msg_flag list -> sockspec -> int
    (** Send data over the (unconnected) socket *)
  method recvfrom : 
    string -> int -> int -> Unix.msg_flag list -> (int * sockspec)
    (** Receive data from the (unconnected) socket. The method will
     * raise EAGAIN if the message cannot be processed for some reason,
     * even if the socket is in blocking mode. In this case, the received
     * message is discarded.
     *)
  method shut_down : unit -> unit
    (** Close the descriptor, shuts down any further needed resources *)
  method datagram_type : datagram_type
  method socket_domain : Unix.socket_domain
  method socket_type : Unix.socket_type
  method socket_protocol : int
  (* CHECK: Maybe a method reporting the net size of the send/recv buffers *)
end;;


(** This is a factory for [wrapped_datagram_socket] objects. *)
class type datagram_socket_provider =
object
  method create_datagram_socket : datagram_type ->
                                  Unixqueue.event_system ->
                                    wrapped_datagram_socket engine
    (** Creates an engine that creates a [wrapped_datagram_socket] object
     * and that sets up any further resources the objects needs.
     *)
end ;;


val datagram_provider : ?proxy:#datagram_socket_provider ->
                        datagram_type ->
                        Unixqueue.event_system ->
			  wrapped_datagram_socket engine;;
  (** This engine creates a datagram socket as demanded by the [datagram_type],
   * optionally using [proxy] for sending and receiving datagrams.
   *
   * The socket is unconnected.
   *
   * The socket is in non-blocking mode, and the close-on-exec flag is 
   * set.
   *)




(** {1 Multiplex Controllers} *)

(** A [multiplex_controller] is a quite low-level device to abstract
  * bidirectional socket connections. It is independent of any real
  * device.
  *
  * There can be a reader, a writer (or both), or alternatively,
  * the shutdown process may be in progress. One cannot have more than
  * one reader and more than more writer.
 *)
class type multiplex_controller =
object
  method alive : bool
    (** If the controller is alive, the socket is not yet completely down. *)

  method mem_supported : bool
    (** Whether [start_mem_reading] and [start_mem_writing] are possible *)

  method event_system : Unixqueue.event_system
    (** Returns the event system *)

  method reading : bool
    (** True iff there is a reader *)

  method start_reading : 
    ?peek:(unit -> unit) ->
    when_done:(exn option -> int -> unit) -> string -> int -> int -> unit
    (** Start reading from the connection. When data is available, the
      * [when_done] callback is invoked. The int is the number of read
      * bytes. It is 0 if an error occurred which is indicated by the
      * exception. The exception [End_of_file] is used when the end of the
      * data stream is reached. The exception [Cancelled] indicates that
      * reading has been cancelled in the meantime.
      *
      * This starts one-time read job only, i.e. it is not restarted
      * after [when_done] has been invoked.
      *
      * It is an error to start reading several times.
      *
      * The function [peek] is called immediately before data is read in
      * from the underlying communication channel.
      *
      * For getting an engine-based version of [start_reading], use
      * a [signal_engine]:
      * {[ 
      *    let (e, signal) = signal_engine esys in
      *    mplex # start_reading ~when_done:(fun xo n -> signal (xo,n)) ...
      * ]}
      * Now [e] will transition to [`Done(x0,n)] when the read is done.
     *)

  method start_mem_reading : 
    ?peek:(unit -> unit) ->
    when_done:(exn option -> int -> unit) -> Netsys_mem.memory -> int -> int ->
    unit
    (** Same as [start_reading], but puts the data into a [memory] buffer.
        There is an optimization for the case that the descriptor is a
        connected socket, or supports [Unix.read]. If this is not possible
        the method raises [Mem_not_supported].
     *)

  method cancel_reading : unit -> unit
    (** Cancels the read job. The [when_done] callback is invoked with the
      * number of bytes read so far (which may be 0) and the exception
      * [Cancelled].
      *
      * It is no error if there is no reader.
     *)

  method writing : bool
   (** True iff there is a writer *)

  method start_writing :
    when_done:(exn option -> int -> unit) -> string -> int -> int -> unit
    (** Start writing to the connection. When data is written, the
      * [when_done] callback is invoked. The int is the number of written
      * bytes. It is 0 if an error occurred which is indicated by the
      * exception. The exception [Cancelled] indicates that
      * writing has been cancelled in the meantime.
      *
      * This starts one-time write job only, i.e. it is not restarted
      * after [when_done] has been invoked.
      *
      * It is an error to start writing several times.
      *
      * See the comment for [start_reading] for how to get an engine-based
      * version of this method.
     *)

  method start_mem_writing : 
    when_done:(exn option -> int -> unit) -> Netsys_mem.memory -> int -> int ->
    unit
    (** Same as [start_writing], but takes the data from a [memory] buffer.
        There is an optimization for the case that the descriptor is a
        connected socket, or supports [Unix.write]. If this is not possible
        the method raises [Mem_not_supported].
     *)

  method supports_half_open_connection : bool
    (** Whether the underlying transport mechanism can close the write side
      * of the connection only (half-open connection).
     *)

  method start_writing_eof :
    when_done:(exn option -> unit) -> unit -> unit
    (** Start writing the EOF marker to the connection. When it is written,
      * the [when_done] callback is invoked. The exception [Cancelled] indicates
      * that writing has been cancelled in the meantime.
      *
      * This starts one-time write job only, i.e. it is not restarted
      * after [when_done] has been invoked.
      *
      * It is an error to start writing several times. It is an error to
      * write EOF when the socket does not support half-open connections.
      *
      * See the comment for [start_reading] for how to get an engine-based
      * version of this method.
     *)

  method cancel_writing : unit -> unit
    (** Cancels the write job. The [when_done] callback is invoked with the
      * number of bytes read so far (which may be 0) and the exception
      * [Canelled].
      *
      * It is no error if there is no writer.
     *)

  method read_eof : bool
    (** Whether the EOF marker has been read *)

  method wrote_eof : bool
    (** Whether the EOF marker has been written *)

  method shutting_down : bool
    (** True iff the shutdown is in progress *)

  method start_shutting_down :
    ?linger : float ->
    when_done:(exn option -> unit) -> unit -> unit
    (** Start shutting down the connection. After going through the shutdown
      * procedure, the [when_done] callback is invoked. The exception
      * indicates whether an error happened. [Cancelled] means that the
      * shutdown operation has been cancelled in the meantime.
      *
      * The underlying file descriptor (if any) is not closed. A shutdown
      * is only a protocol handshake. After a shutdown, both [read_eof]
      * and [wrote_eof] are true. Call [inactivate] to close the descriptor.
      *
      * Optionally, one can [linger] for a certain period of time.
      * It is only lingered when the EOF was written before the EOF 
      * is seen on input.
      * Defaults to [linger 60.0]. Set to 0 to turn off.
      *
      * See the comment for [start_reading] for how to get an engine-based
      * version of this method.
     *)

  method cancel_shutting_down : unit -> unit
    (** Cancels the shutdown procedure. After that, the state of the 
      * connection is undefined. The [when_done] callback is invoked with
      * the exception [Cancelled].
      *
      * It is no error if no shutdown is in progress.
     *)

  method inactivate : unit -> unit
    (** Inactivates the connection immediately, and releases any resources
      * the controller is responsible for (e.g. closes file descriptors). 
      * Note that this is more than
      * cancelling all pending operations and shutting the connection down.
      * However, the details of this method are implementation-defined.
      * Callbacks are not invoked.
     *)
end


exception Mem_not_supported
  (** May be raised by multiplex controller methods [start_mem_reading] and
      [start_mem_writing] if these methods are not supported for the kind
      of file descriptor
   *)

val create_multiplex_controller_for_connected_socket : 
      ?close_inactive_descr:bool ->
      ?preclose:(unit -> unit) ->
      ?supports_half_open_connection:bool ->
      ?timeout:(float * exn) ->
      Unix.file_descr -> Unixqueue.unix_event_system -> multiplex_controller
  (** Creates a multiplex controller for a bidirectional socket (e.g.
    * a TCP socket). It is essential that the socket is in connected state.
    * This function also supports Win32 named pipes.
    *
    * Note that the file descriptor is not closed when the attached engines
    * are terminated. One can call [inactivate] manually to do that.
    *
    * [close_inactive_descr]: Whether [inactivate] closes the descriptor.
    * True by default.
    *
    * [preclose]: This function is called just before the descriptor is
    * closed.
    *
    * [supports_half_open_connection]: This implementation does not know
    * how to find out whether the socket supports half-open connections.
    * You can simply set this boolean because of this. Defaults to [false].
    * You can set it to [true] for TCP connections and for Unix-domain
    * connections with stream semantics.
    *
    * [timeout]: If set to [(t, x)], a general timeout of [t] is set.
    * When an operation has been started, and there is no I/O activity within
    * [t] seconds, neither by the started operation nor by another operation,
    * the connection times out. In this case, the operation returns the
    * exception [x].
   *)


(** Additional methods for unconnected datagram handling *)
class type datagram_multiplex_controller =
object
  inherit multiplex_controller

  method received_from : Unix.sockaddr
    (** Returns the socket address of the last received datagram. This
      * value is updated just before the [when_done] callback of the
      * reader is invoked.
     *)

  method send_to : Unix.sockaddr -> unit
    (** Sets the socket address of the next datagram to send. *)

end

val create_multiplex_controller_for_datagram_socket : 
      ?close_inactive_descr:bool ->
      ?preclose:(unit -> unit) ->
      ?timeout:(float * exn) ->
      Unix.file_descr -> Unixqueue.unix_event_system -> 
        datagram_multiplex_controller
  (** Creates a multiplex controller for datagram sockets (e.g. UDP socket).
    *
    * Note that the file descriptor is not closed when the attached engines
    * are terminated. One can call [inactivate] manually to do that.
    *
    * [close_inactive_descr]: Whether [inactivate] closes the descriptor.
    * True by default.
    *
    * [preclose]: This function is called just before the descriptor is
    * closed.
    *
    * [timeout]: If set to [(t, x)], a general timeout of [t] is set.
    * When an operation has been started, and there is no I/O activity within
    * [t] seconds, neither by the started operation nor by another operation,
    * the connection times out. In this case, the operation returns the
    * exception [x].
   *)


type onshutdown_out_spec =
    [ `Ignore
    | `Initiate_shutdown
    | `Action of async_out_channel_engine -> multiplex_controller -> 
                   unit engine_state -> unit
    ]
  (** See class [output_async_mplex] for explanations *)

type onshutdown_in_spec =
    [ `Ignore
    | `Initiate_shutdown
    | `Action of async_in_channel_engine -> multiplex_controller -> 
                   unit engine_state -> unit
    ]
  (** See class [input_async_mplex] for explanations *)

class output_async_mplex : 
       ?onclose:[ `Write_eof | `Ignore ] ->
       ?onshutdown:onshutdown_out_spec ->
       ?buffer_size:int ->
       multiplex_controller ->
         async_out_channel_engine
  (** Creates an asynchronous output channel writing to the multiplex
    * controller (see also [output_async_descr] for the corresponding
    * class writing to a single descriptor).
    *
    * [onclose]: What to do when the [close_out] method is invoked.
    * Defaults to [`Ignore]. [`Write_eof] means to write the EOF marker.
    * Anyway, after doing the close action, the multiplex controller
    * is shutdown.
    *
    * [onshutdown]: What to do when all data (and optionally, the EOF marker)
    * have been written. It is also invoked in case of I/O errors.
    * The default is [`Ignore]. The value [`Initiate_shutdown] means that
    * it is started to shutdown the socket. The success of this action
    * is not waited upon, however. One can also pass [`Action f] in which
    * case the function [f] is called with this object, the
    * multiplex controller, and the proposed next state as arguments. 
    * By checking the proposed next state the function can see why the
    * shutdown function was called.
    *
    * [buffer_size]: The size of the internal buffer. By default unlimited.
    *
    * Note that the engine is done when the output channel is closed.
    * The socket is not shut down, and the underlying file descriptor
    * is not closed! You can define the [shutdown] callback to do something
    * in this case.
   *)


class input_async_mplex : 
       ?onshutdown:onshutdown_in_spec ->
       ?buffer_size:int ->
       multiplex_controller ->
         async_in_channel_engine
  (** Creates an asynchronous input channel reading from the multiplex
    * controller.
    *
    * [onshutdown]: See [output_async_mplex].
    *
    * [buffer_size]: The size of the internal buffer. By default unlimited.
    *
    * Note that the engine is done when the input channel is closed.
    * The socket is not shut down, and the underlying file descriptor
    * is not closed! You can define the [shutdown] callback to do something
    * in this case.
   *)

(** {1 Recursion} *)

(** When programming with engines, it is normal to use recursion for any
    kind of loops. For example, to read the lines from a file:

    {[
      open Uq_engines.Operators  (* for ">>" and "++" *)

      let fd = 
        Unix.openfile filename [Unix.O_RDONLY] 0 in
      let d = 
        `Buffer_in(Uq_io.create_in_buffer(`Polldescr(`Read_write,fd,esys))) in

      let rec read_lines acc =
        Uq_io.input_line_e d >>
          (function                       (* catch exception End_of_file *)
            | `Done line -> `Done(Some line)
            | `Error End_of_file -> `Done None
            | `Error error -> `Error error
            | `Aborted -> `Aborted
          ) ++
          (function
            | Some line ->
                read_lines (line :: acc)
            | None ->
                eps_e (`Done (List.rev acc)) esys
          ) in

      let e = read_lines []
    ]}

    There is generally the question whether this style leads to stack
    overflows. This depends on the mechanisms that come into play:

    - The engine mechanism passing control from one engine to the next is
      not tail-recursive, and thus the stack can overflow when the
      recursion becomes too deep
    - The event queue mechanism, however, does not have this problem.
      Control falls automatically back to the event queue whenever I/O
      needs to be done.

    In this example, this means that only the engine mechanism is used
    as long as the data is read from the buffer. When the buffer needs
    to be refilled, however, control is passed back to the event queue
    (so the stack is cleaned), and the continuation of the execution
    is only managed via closures (which only allocate memory on the
    heap, not on the stack). Usually, this is a good compromise: The
    engine mechnism is a lot faster, but I/O is an indicator for using
    the better but slower technique.

    Also note another difference: The event queue mechanism allows that
    other asynchronous code attached to the same event queue may run
    (control maybe yielded to unrelated execution contexts). The
    pure engine mechanism does not allow that. This may be handy when
    exclusive access to variables is needed. (But be careful here -
    this is very sensitive to minimal changes of the implementation.)

    Certain engines enforce using the event queue mechanisms although they
    are unrelated to I/O. Especially {!Uq_engines.delay_engine} is
    useful here: A "delay" of 0 seconds is already sufficient to
    go back to the event queue. If recursions sometimes lead to
    stack overflows the solution is to include such a zero delay
    before doing the self call.
 *)

(** {1 More Engines} *)

(**
  Pointers to other modules related to engines:

  - RPC clients: The function {!Rpc_proxy.ManagedClient.rpc_engine} allows
    to call an RPC via an engine. When the call is done, the engine transitions
    to [`Done r], and [r] is the result of the remote call.
  - Subprograms: The class {!Shell_uq.call_engine} allows to start an
    external program, and to monitor it via an engine.
 *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging *)

end
