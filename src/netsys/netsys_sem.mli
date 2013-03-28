(* $Id: netsys_sem.mli 1802 2012-10-18 23:23:39Z gerd $ *)

(** {1 Generic anonymous semaphores} *)

(** This module purely exists to also support "kind of anonymous"
    sempahores on MacOS X (which only has named semaphores). On
    other OS, it is just a wrapper for the functions in
    {!Netsys_posix}.

    Note that on OS X named semaphores have a max name length of
    31 characters (including the / at the beginning), and that
    [Netsys_sem] uses 9 characters for its own purposes, leaving
    22 characters for the prefix. (On other OS this is less restricted.)
 *)

val have_anon_semaphores : unit -> bool
  (** Returns [true] if anonymous semaphores are supported on this 
      system, either natively or emulated via named semaphores.
   *)

(** {b Constants.} *)

val sem_value_max : int
  (** The maximum value of a semaphore, but at most [max_int] *)

val sem_size : int
  (** The size of an anonymous semaphore in bytes ([sizeof(sem_t)]) *)

(** {b Types.} *)

type container
  (** The container of the semaphore is the shared memory object *)

type prefix = string
  (** A name starting with a slash. Must not contain further slashes *)

type anon_semaphore

type sem_open_flag = Netsys_posix.sem_open_flag =
  | SEM_O_CREAT
  | SEM_O_EXCL

(** {b Container functions.} *)

val container : prefix -> container
  (** [container prefix]: The prefix shall identify the container uniquely.
      Once can e.g. use the path of the shared memory object. The prefix
      is used to construct names for persistent objects.

      {b Note that containers have kernel persistence! They are not
      automatically deleted when the process finishes. Call [drop]
      to delete containers, or [create_container] to force their
      creation as fresh objects.}

      If the container does not exist yet, it is created. Otherwise the
      container is just opened.
   *)

val create_container : prefix -> container
  (** [create_container prefix]: Like [container], but the container is
      always created. A previous instance is first deleted.
   *)

val prefix : container -> prefix
  (** Return the prefix *)

val drop : container -> unit
  (** Drop the semaphores in this container, and delete the container.

      This function is a no-op if the OS supports anonymous semaphores
      directly (because in this case the deletion of the container will
      automatically destroy the semaphores).
   *)

(** Note that there is a general problem when a container is deleted
    or dropped by a process while it is still being used by another one.
    This will generally not generate errors, but can cause a lot of
    confusion, because the processes will partly access the same
    semaphores, and partly different semaphores.
 *)

val unlink : prefix -> unit
  (** Unlinks the identified container if it exists, and unlinks all
      possible named semaphores.
   *)

(** {b Semaphore functions.} *)

val sem_init : container -> Netsys_types.memory -> int -> bool -> int -> 
                 anon_semaphore
  (** [sem_init cont mem pos pshared init_value]: Initializes the memory
      at position [pos] to [pos + sem_size() - 1] as anonymous semaphore.
      If [pshared] the semaphore is shared between processes. 
      [init_value] is the initial non-negative value (max is 
      [sem_value_max]).
   *)

val sem_destroy : container -> anon_semaphore -> unit
  (** Destroys the anonymous semaphore *)

val as_sem : container -> Netsys_types.memory -> int -> anon_semaphore
  (** [as_sem mem pos]: Interprets the memory at position [pos]
      to [pos + sem_size() - 1] as anonymous semaphore.
      The memory region must already have been initialized.
   *)

val sem_getvalue : anon_semaphore -> int
  (** Returns the value of the semaphore. If the value is bigger than
      what can be represented as [int], an [EINVAL] error is returned.

      The returned value is non-negative - if the underlying POSIX
      function reports a negative value zero is returned instead.

      {b Unavailable on MacOS.}
   *)

val sem_post : anon_semaphore -> unit
  (** Unlocks the semaphore (increases the value by 1) *)

type sem_wait_behavior = Netsys_posix.sem_wait_behavior =
  | SEM_WAIT_BLOCK
  | SEM_WAIT_NONBLOCK

val sem_wait : anon_semaphore -> sem_wait_behavior -> unit
  (** Locks the semaphore (decreases the value by 1). If the semaphore
      value is already zero, and [SEM_WAIT_BLOCK] is given, the function
      waits until another process or thread posts. If [SEM_WAIT_NONBLOCK]
      the error [EAGAIN] is returned.

      [sem_wait] may be interrupted by signals.
   *)

module Debug : sig
  val enable : bool ref
end

(**/**)

val force_emulation : unit -> unit
