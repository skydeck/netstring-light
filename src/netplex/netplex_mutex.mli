(* $Id: netplex_mutex.mli 1708 2012-02-16 00:12:17Z gerd $ *)

(** Netplex-wide mutexes *)

open Netplex_types

(** These are mutexes that work in both multi-processing and
    multi-threading netplex environments. They are implemented on top of
    {!Netplex_semaphore}, and use the same name space for named objects.

    For implementation issues, see {!Netplex_semaphore}.

    It is not required to create mutexes: A mutex is automatically created
    when the name is used for the first time. Mutexes are initially always
    in unlocked state.

    Mutexes do not stack: A container cannot lock a mutex several times.
    (The second lock attempt would result in a deadlock.)

    There is no deadlock detection.

    If a container holding mutexes terminates the mutexes are automatically
    unlocked.

    Mutex are not owned by their container. It is allowed that a different
    container unlocks the mutex.

    Technically, a mutex named [n] is implemented by a semaphore [n].
    If the mutex is locked, the semaphore has the value 0, and if it is
    unlocked, it has value 1.

    {b Thread safety:} Full. The functions can be called from any thread.
 *)

val plugin : plugin
  (** To enable mutexes, call the controller's [add_plugin] method 
      with this object as argument. This can e.g. be done in the
      [post_add_hook] of the processor.
   *)

(** The following functions can {b only} be invoked in container
    contexts. Outside of such a context the exception
    {!Netplex_cenv.Not_in_container_thread} is raised. 
 *)

type mutex

val access : string -> mutex
  (** Access the named mutex. If the mutex does not exist, it is created in
      unlocked state
   *)

val lock : mutex -> unit
  (** Wait until the mutex is free, and locks it *)

val unlock : mutex -> unit
  (** Unlocks the mutex. This is a no-op if the mutex is not locked *)

