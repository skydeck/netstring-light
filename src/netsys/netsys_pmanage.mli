(* $Id: netsys_pmanage.mli 1774 2012-04-03 21:28:51Z gerd $ *)

(** Manage persistent objects *)

(** This module allows you to keep a list of all objects with
    kernel persistency in an external file.
 *)

type pm_obj =
    [ `File of string
    | `Posix_shm of string
    | `Posix_sem of string
    | `Sem_cont of string
    ]

class type pmanage =
object
  method register_file : string -> unit
    (** Register a temporary file *)

  method unregister_file : string -> unit
    (** Unregister a temporary file *)

  method register_posix_shm : string -> unit
    (** Register a POSIX shared memory object identified by this path *)

  method unregister_posix_shm : string -> unit
    (** Unregister a POSIX shared memory object identified by this path *)

  method register_posix_sem : string -> unit
    (** Register a POSIX named semaphore identified by this path *)

  method unregister_posix_sem : string -> unit
    (** Unregister a POSIX named semaphore identified by this path *)

  method register_sem_cont : string -> unit
    (** Register a semaphore container (see {!Netsys_sem}) *)

  method unregister_sem_cont : string -> unit
    (** Unregister a semaphore container (see {!Netsys_sem}) *)

  method registered : pm_obj list
    (** Return the registered objects *)

  method register : pm_obj list -> unit
    (** Mass-register a list of objects *)

  method unlink : unit -> unit
    (** Delete all registered objects *)

end

val pmanage : string -> pmanage
  (** Create a new manager for persistent kernel objects. The data is stored
      in a text file whose path is passed to this function. If the file already
      exists, it is opened and used. If the file does not exist yet, an
      empty manager will be created.

      This function can be safely used from several threads and several
      processes.
   *)


val fake_pmanage : unit -> pmanage
  (** A pseudo manager that does not save anything to a text file *)

val unlink_this : pm_obj -> unit
  (** Unlinks this object *)
