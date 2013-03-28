(* $Id: netplex_semaphore.mli 1708 2012-02-16 00:12:17Z gerd $ *)

(** Netplex-wide semaphores *)

open Netplex_types

(** Semaphores are counters with atomic increment and decrement operations.
    They are very useful for counting the number of uses of a shared
    resource, and allow the identification of the first use (so the resource
    must be made available at all), and the last use (the resource can be
    released).

    This implementation works in both multi-processing and
    multi-threading netplex environments. It is, however, not very
    fast, because the counters live in the controller, and the
    increment/decrement operations are realized by RPC's. It is good
    enough when these operations are only infrequently called, e.g. in
    the post-start and pre-finish processor callbacks.

    This interface is designed so that a later re-implementation with
    POSIX semaphores is relatively straight-forward.

    {b Thread safety:} Full. The functions can be called from any thread.
  *)

val plugin : plugin
  (** To enable semaphores, call the controller's [add_plugin] method 
      with this object as argument. This can e.g. be done in the
      [post_add_hook] of the processor.
   *)


(** The following functions can {b only} be invoked in container
    contexts. Outside of such a context the exception
    {!Netplex_cenv.Not_in_container_thread} is raised. 
 *)

val increment : string -> int64
  (** Increment the named semaphore by 1, and return the new value.
      If the semaphore does not exist yet, it is created with an initial
      value of 0, which is then incremented. 

      Semaphore names are global to the whole netplex system. By convention,
      these names are formed like ["service_name.local_name"], i.e. they
      are prefixed by the socket service to which they refer.
   *)

val decrement : ?wait:bool -> string -> int64
  (** Decrement the named semaphore by 1, and return the new value.
      Semaphore values cannot become negative. If the value is already 0,
      it is not decremented anymore if [wait = false]. However, (-1)
      is then returned nevertheless.

      If the value is already 0 and [wait=true], the operation waits until
      the value exceeds 0, and when this happens, the semaphore is then
      decremented again. If several waiters exist, only one waiter gets
      the chance to decrement.
   *)

val get : string -> int64
  (** Get the value of the named semaphore. Useful e.g. for monitoring
      the semaphore. If the semaphore does not exist, a value of 0 is
      returned.

      [get] can also be invoked from the controller process.
   *)

val create : ?protected:bool -> string -> int64 -> bool
  (** Create the semaphore with this initial value. Returns [true] if the
      creation is successful, and [false] if the semaphore already existed.

      If [protected], the semaphore
      is automatically decremented by some value when the container
      calling this function terminates. This value is [pi - d] where
      [pi] is the number of increments and [d] is the number
      of (successful) decrements requested by the container.

      A semaphore needs not to be explicitly created by calling [create].
      It is automatically created at the first use time with a value of 0
      and [protected=true].

      [create] can also be invoked from the controller process.
   *)

val destroy : string -> unit
  (** Destroy this semaphore. Any waiting [decrement] will immediately
      get (-1L).

      Note that there is no protection against unintended re-creation
      after [destroy].

      [destroy] can also be invoked from the controller process.
   *)

val ctrl_increment : string -> Netplex_types.container_id -> int64
  (** Increment the named semaphore from controller context, substituting
      an increment from a container (e.g. a container that terminated
      or is otherwise no longer able to do so). In this
      case the ID of the container needs to be passed 
   *)


(** Example (code fragment):

    Override the processor callbacks as follows to count the number of
    containers for the service:

  {[ 
    method post_add_hook sockserv ctrl =
      ctrl # add_plugin Netplex_semaphore.plugin

    method post_start_hook container =
      let sem_name = container#socket_service#name ^ ".counter" in
      let n =
        Netplex_semaphore.increment sem_name in
      if n=1 then
        prerr_endline "First container"

    method pre_finish_hook container =
      let sem_name = container#socket_service#name ^ ".counter" in
      let n =
        Netplex_semaphore.decrement sem_name in
      if n=0 then
        prerr_endline "Last container"
   ]}
 *)

