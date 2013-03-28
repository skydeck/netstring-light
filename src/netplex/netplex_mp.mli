(* $Id: netplex_mp.mli 1276 2009-10-12 01:08:45Z gerd $ *)

(** Multi-processing provider *)

class mp : ?keep_fd_open:bool -> ?terminate_tmo:int -> unit -> 
              Netplex_types.parallelizer
  (** Uses [Unix.fork] to create new threads.

      After forking the child process is initialized by 
      calling [Netsys_posix.run_post_fork_handlers].

      By default, all file descriptors are closed that are not explicitly
      shared with the parent process. This can be wrong for some kind
      of applications. By setting [keep_fd_open] another behavior can
      be demanded: The descriptors are kept open except those that need
      to be closed.

      When the controller wants to shut down the process, it notifies
      the process, and waits [terminate_tmo] seconds until the process
      terminates. If this timeout expires, the process is killed. Defaults
      to 60. A negative value disables this function.
   *)

val mp : ?keep_fd_open:bool -> ?terminate_tmo:int -> unit -> 
           Netplex_types.parallelizer
  (** Uses [Unix.fork] to create new threads. See {!class: Netplex_mp.mp}
      for details.
   *)
