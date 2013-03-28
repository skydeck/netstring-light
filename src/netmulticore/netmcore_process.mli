(* $Id: netmcore_process.mli 1580 2011-04-14 16:06:32Z gerd $ *)

(** Statically typed processes *)

(** This module provides a slightly higher-level view on processes,
    but is also a bit more restricted than the primitives in
    {!Netmcore}.
 *)

type 'a fork_point
  (** A fork point can start a new process with argument ['a] *)

type 'b join_point
  (** A join point can wait until a process finishes with a result ['b] *)

val def_process : ('a -> 'b) -> 'a fork_point * 'b join_point
  (** [let (fp,jp) = def_process proc]: Defines a process (which must happen
      in the master process), so that:

      - [let pid = start fp arg]: Starts the new process and passes the
        argument [arg] to it. The process runs [proc arg] which will
        finally result in a value [r]
      - [let r_opt = join jp pid]: Waits until the process identified by
        [pid] finishes, and returns the result as [r_opt = Some r].
        On error, it returns [None].

      [def_process] should be called at module initialization time
      before any process is started. This interface is not designed for
      calling [def_process] later.
   *)

val start : ?inherit_resources:Netmcore.inherit_request -> 
            'a fork_point -> 'a -> Netmcore.process_id
  (** [let pid = start fp arg]: Starts a new process at the fork point
      [fp] with argument [arg]. This means that the process is forked
      from the master process, and that the value of [arg] is sent to it
      using [Marshal]. The returned process ID is Netmulticore's own
      ID and not to be confused with the ID assigned by the operating
      system.

      Option [inherit_resources]: Certain resources are only accessible by
      the process when they are inherited to it. This is the case for
      [`Posix_shm_preallocated]. This can be set to [`All] to inherit
      all inheritable resources, or to [`Resources l] to only inherit
      the resources of [l]. By default, no resources are inherited.
   *)

val join : 'b join_point -> Netmcore.process_id -> 'b option
  (** [let r_opt = join jp pid]: Waits until the process [pid] finishes,
      and uses the join point [jp] to extract the result. The result
      is returned as [Some r] on success, and [None] on error.
      Result values are transmitted from the joining process to
      this function using [Marshal]. Errors include uncaught exceptions
      as well as unexpected process termination ([exit], signal).

      If the process referenced by [pid] is not an instance that belongs
      to [jp], the function will fail.
   *)

val release_fork_point : 'a fork_point -> unit
  (** Releases a fork point so it is deleted from the internal
      resource table. 
   *)

val release_join_point : 'b join_point -> unit
  (** Releases a join point so it is deleted from the internal
      resource table. 
   *)
