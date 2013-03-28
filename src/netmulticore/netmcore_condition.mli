(* $Id: netmcore_condition.mli 1715 2012-02-20 14:47:01Z gerd $ *)

(** Condition variables *)

(** Condition variables are here defined as values that reside in shared heaps
    ({!Netmcore_heap}), for example in the header field of 
    {!Netmcore_array} or somewhere else in heap-allocated
    data structures.

    In order to ensure that the condition variable is in the heap, the
    special function [create_condition] must be used to initialize it
    there. As [create_condition] requires a mutator as argument, this is
    only possible by calling [create_condition] from the callback of
    {!Netmcore_heap.modify}.

    Condition variables are special values, and cannot be copied or moved.

    Condition variables are implemented on top of semaphores. Compared to
    the [pthreads] version of condition variables, the user needs here to
    allocate special [wait_entry] slots, one for each process. An entry
    can be used for all condition variables a process needs to wait for.
    (Actually, such entries also exist in typical [pthreads] implementations,
    but are hidden from the user in the thread control block. We just
    don't have here a place where we could allocate process-specific
    shared memory.)

    Since Ocamlnet-3.5, there are also special wait entries [wait_entry_e]
    which can be used to wait from within a running Unixqueue. For each
    such wait entry, however, a named pipe needs to be allocated.
 *)

type condition
  (** The condition variable *)

type wait_entry
  (** Each process that wants to [wait] needs a [wait_entry]. These entries
      can be used for several condition variables, so typically each process
      has only one entry for each heap.
   *)

type wait_entry_e
  (** A special kind of [wait_entry] for intergration into an event
      loop
   *)

type wait_set
  (** A set of [wait_entry], for easier management. This set can e.g. be
      stored side by side with the condition variable(s). It is important
      that the [wait_set] resides in the same shared heap as the
      condition variable.
   *)

val dummy_condition : unit -> condition
  (** A dummy condition is non-functional, but can be used to put something
      into [condition]-typed variables
   *)

val dummy_wait_set : unit -> wait_set
  (** A dummy [wait_set] is non-functional, but can be used to put something
      into [wait_set]-typed variables
   *)


val create_condition : Netmcore_heap.mutator -> condition
  (** [create m]: Creates a condition variable, and
      pushes it to the heap, using the mutator [m].

      After being pushed to the heap, the variable can be used. It is
      nonsense to copy it outside the heap.
   *)

val create_wait_set :  Netmcore_heap.mutator -> wait_set
  (** Creates a [wait_set] in the heap designated by the mutator *)

val alloc_wait_entry : Netmcore_heap.mutator -> wait_set -> wait_entry
  (** Allocates a [wait_entry] *)

val free_wait_entry : Netmcore_heap.mutator -> wait_set -> wait_entry -> unit
  (** Frees a [wait_entry] *)

val alloc_wait_entry_e : Netmcore_heap.mutator -> wait_set -> string ->
                            wait_entry_e
  (** [alloc_wait_entry_e mut set filename]: Allocates a new wait entry
      with notification via named pipe. The [filename] must refer to an
      existing named pipe.
   *)

val free_wait_entry_e : Netmcore_heap.mutator -> wait_set -> wait_entry_e -> 
                         unit
  (** Frees a [wait_entry_e]. The named pipe is deleted. *)

val wait : wait_entry -> condition -> Netmcore_mutex.mutex -> unit
  (** [wait we c m] atomically unlocks the mutex [m] and suspends the
      calling process on the condition variable [c]. The process will
      restart after the condition variable [c] has been signalled.
      The mutex [m] is locked again before [wait] returns.

      At the time of calling, the [wait_entry] [we] must not be used to
      manage another [wait].  When allocating a separate [wait_entry]
      per process this problem does not occur.
   *)

val wait_e : ?debug_name:string ->
             wait_entry_e -> condition -> Netmcore_mutex.mutex -> 
             Unixqueue.event_system ->
             (unit -> 'a Uq_engines.engine) ->
               'a Uq_engines.engine
  (** Like [wait], but the suspension is done by waiting on a
      named pipe event (i.e. "nonblocking suspension"):

      [wait_e we c m esys cont] atomically unlocks the mutex [m] and suspends
      the calling engine on the condition variable [c]. The engine will
      restart after the condition variable [c] has been signalled.
      The mutex [m] is locked again, at which time [cont] is called.
      The result of [cont()] is the result of [wait_e].

      At the time of calling, the [wait_entry_e] [we] must not be used to
      manage another [wait_e].  When allocating a separate [wait_entry_e]
      per process (or thread within the process) this problem does not occur.
   *)


(* UNCLEAR what this means:
      Another important restriction: There must be at most one 
      active [wait_e] per process or thread.
 *)


val signal : condition -> unit
  (** [signal c] restarts one of the processes waiting on the
      condition variable [c].
   *)

val broadcast : condition -> unit
  (** [broadcast c] restarts all processes waiting on the
      condition variable [c].
   *)

val pipe_name : wait_entry_e -> string
  (** Returns the name of the pipe *)

val destroy_condition : condition -> unit
val destroy_wait_set : wait_set -> unit
  (** Destroys these objects *)


module Debug : sig
  val enable : bool ref
end
