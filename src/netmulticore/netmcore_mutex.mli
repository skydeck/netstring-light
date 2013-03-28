(* $Id: netmcore_mutex.mli 1570 2011-04-08 14:47:16Z gerd $ *)

(** Mutexes *)

(** Mutexes are here defined as values that reside in shared heaps
    ({!Netmcore_heap}), for example in the header field of 
    {!Netmcore_array} or somewhere else in heap-allocated
    data structures.

    In order to ensure that the mutex is in the heap, the
    special function [create] must be used to initialize it
    there. As [create] requires a mutator as argument, this is
    only possible by calling [create] from the callback of
    {!Netmcore_heap.modify}.

    Mutexes are special values, and cannot be copied or moved.

    Mutexes are implemented on top of semaphores. For mutex types with
    deadlock handling, the process is considered as the owner, and
    process ID is the identifier.

    Only mutexes of type [`Normal] are thread-safe, i.e. can be used
    when the worker processes also use threads internally.
 *)

type mutex_type =
    [ `Normal | `Errorcheck | `Recursive ]
    (** Types:
	- [`Normal] mutexes have no checks for deadlocks.
	- [`Errorcheck] mutexes check whether the process locks the mutex
	  again, and fail in this case. Also, only the owning process can
	  unlock a mutex.
	- [`Recursive] mutexes allow that the owner locks several times.
	  The same number of unlock requests need to be issued to give
	  up the ownership
     *)

type mutex

val dummy : unit -> mutex
  (** A dummy mutex is non-functional, but can be used to put something
      into [mutex]-typed variables
   *)

val create : Netmcore_heap.mutator -> mutex_type -> mutex
  (** [create m t]: Creates a mutex of type [t], and
      pushes it to the heap, using the mutator [m].

      After being pushed to the heap, the mutex can be used. It is
      nonsense to copy it outside the heap.
   *)

val lock : mutex -> unit
  (** Locks the mutex *)

val unlock : mutex -> unit
  (** Unlocks the mutex *)

val destroy : mutex -> unit
  (** Destroys the mutex *)
