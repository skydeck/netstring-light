(* $Id: netmcore_sem.mli 1570 2011-04-08 14:47:16Z gerd $ *)

(** Semaphores *)

(** Semaphores are here defined as values that reside in shared heaps
    ({!Netmcore_heap}), for example in the header field of 
    {!Netmcore_array} or somewhere else in heap-allocated
    data structures.

    In order to ensure that the semaphore is in the heap, the
    special function [create] must be used to initialize it
    there. As [create] requires a mutator as argument, this is
    only possible by calling [create] from the callback of
    {!Netmcore_heap.modify}.

    Semaphores are special values, and cannot be copied or moved.

    Note that {!Netsys_posix} provides wrappers for direct use
    of the semaphore functionality of the OS. These wrappers, however,
    cannot be used together with heaps, because semaphores are
    there represented as bigarrays, which cannot be pushed to heaps.
    This module here uses a trick to make it possible nevertheless.
 *)

type semaphore

val dummy : unit -> semaphore
  (** A dummy semaphore is non-functional, but can be used to put something
      into [semaphore]-typed variables
   *)

val create : Netmcore_heap.mutator -> int -> semaphore
  (** [create m value]: Creates a semaphore with initial [value], and
      pushes it to the heap, using the mutator [m].

      After being pushed to the heap, the semaphore can be used. It is
      nonsense to copy it outside the heap.
   *)

val destroy : semaphore -> unit
  (** Destroys the semaphore *)

val getvalue : semaphore -> int
  (** Returns the value of the semaphore *)

val post : semaphore -> unit
  (** Increments the semaphore by one, and notifies one of the waiting
      processes.
   *)

val wait : semaphore -> Netsys_posix.sem_wait_behavior -> unit
  (** Decrements the semaphore by one. Semaphores cannot become negative.
      If the semaphore is already 0, the wait behavior defines how to go
      on:
      - {!Netsys_posix.SEM_WAIT_BLOCK}: Wait until the semaphore is
        posted
      - {!Netsys_posix.SEM_WAIT_NONBLOCK}: Raise [EAGAIN]

      Waiting is restarted after running a signal handler.
   *)
