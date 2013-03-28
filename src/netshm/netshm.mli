(* $Id: netshm.mli 1159 2007-11-29 13:19:21Z gerd $ *)

(** Shared memory for O'Caml programs using multi-processing *)

(** {1 Shared Memory Descriptors} *)

type shm_descr
  (** A shared memory descriptor refers to a shared memory object.
    * Such a descriptor must only be opened once for every process.
   *)

type shm_type =
  [ `POSIX
      (** POSIX shared memory (system calls [shm_open] and [mmap]) *)
  | `File
      (** File-based, but not necessarily shared memory (system call [mmap]) *)
  ]

val supported_types : shm_type list
  (** The types supported for this OS *)

type shm_name =
  [ `POSIX of string
      (** This name refers to a POSIX shared memory object. The name
        * must start with a slash, and must not contain further slashes,
        * e.g. ["/my_shm"].
       *)
  | `File of string
      (** This is the name of an arbitrary file used to store the
        * data. Note that this is not shared memory unless the file
        * system is RAM-based.
       *)
  ]
  (** A [shm_name] is a name for a shared memory object. *)


val shm_type_of_name : shm_name -> shm_type

val open_shm : shm_name -> Unix.open_flag list -> int -> shm_descr
  (** Opens the shared memory object.
    *
    * For [POSIX_shm] not all open flags can be specified. The flags
    * are limited to [O_RDONLY], [O_RDWR], [O_CREAT], [O_EXCL] and
    * [O_TRUNC].
   *)

val create_unique_shm : shm_name -> int -> shm_descr
  (** Create a shared memory object under a name that is derived
    * from the passed [shm_name]. The actual name can be queried with
    * [name_of_shm] (below).
    *
    * For [POSIX] and [File] names, deriving works by replacing
    * the 'X' letters in the file name by random digits and letters.
    * For example, if the name is [File "/my/directory/ocaml.XXXXXX"]
    * six random characters are generated and replace the 'X' letters
    * such that the file is unique.
    *
    * The integer is the file permission.
   *)

val name_of_shm : shm_descr -> shm_name
  (** Returns the name of an object *)

val close_shm : shm_descr -> unit
  (** Closes the object. The object remains existent and can be
    * opened again.
   *)

val unlink_shm : shm_name -> unit
  (** Removes the name permanently from the system *)

val chmod_shm : shm_descr -> int -> unit
val chown_shm : shm_descr -> int -> int -> unit
  (** Set file permission bits, user and group ownership of the object *)

type locking_method =
  [ `No_locking
      (** Do not use locking at all *)
  | `Record_locking
      (** Use record locking as provided by [Unix.lockf]. This type of
        * locking is compatible with shared memory types [POSIX] and [File].
       *)
  ]
  (** The locking method is used to ensure that parallel read and write
    * operations to the memory object do not interfer with each other.
    * If [No_locking] is selected, such protection is not done - this
    * is ok if only read accesses occur or if the user can ensure that
    * never a write access is done in parallel with another access.
    * The locking method must be compatible with the type of shared memory.
   *)

val best_locking_method : shm_type -> locking_method
  (** Return the best locking method other than [No_locking] *)

(** {1 Shared Memory Tables} *)

(** This is a quite basic data structure implemented for shared memory:
  * hash tables with [int32] as key and one-dimensional [int32]
  * bigarray as values.
 *)

(** The semantics resembles the [Hashtbl] of stdlib *)

type shm_table

type int32_array = 
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

exception Corrupt_file of string
  (** Raised when a violation of the object format is detected *)

exception Deadlock
  (** Raised when a deadlock situation was detected. Deadlocks can occur
    * when the [group] function is used
    *)

val manage : ?pagesize:int -> 
             ?init:int -> 
             locking_method -> 
             shm_descr ->
               shm_table
  (** Starts managing an open shared memory object as [shm_table]. If
    * the object is empty, it is automatically enlarged to the 
    * minimum size and initialized. If the object is non-empty it is
    * expected that it already contains a valid [shm_table] structure.
    *
    * The object automatically grows in size when new elements are
    * added to the object. By removing elements, however, the
    * object is never shrinked. Unused memory is held back for later
    * reallocation by the same [shm_table].
    *
    * By default, the table uses a page size of 256 bytes. The page size is
    * the unit of memory allocation. The parameter [pagesize] overrides
    * this default. The page size must be at least 160 and divisible by 4.
    * The page size must be the same when the table was created.
    *
    * By passing [init] it is enforced that the table is re-initialized
    * (deleted and again created). The argument of [init] is a hint
    * for the number of elements the table will contain. The data
    * structure is then created such that this many elements can be added
    * without needing expensive data reorganization.
    *
    * Special note for using [shm_table] with multiple processes: Every
    * process must create its own [shm_descr], and every process must
    * call [manage] to manage it. It is not sufficient to just fork
    * a new process, and to keep using the already existing
    * [shm_descr] or [shm_table] in the subprocess. (This doesn't work
    * because the underlying file descriptor would be shared.)
   *)

val group : shm_table -> ('a -> 'b ) -> 'a -> 'b
  (** Execute a sequence of operations in a group:
    *
    * {[
    * let r =
    *    group table
    *      (fun arg ->
    *         operation1; operation2; ...)
    *      arg
    * ]}
    *
    * Operations can be any reading or writing functions from below. The
    * effect is that the locking requirements of the operations are merged,
    * so that no operation of another process can interfer with the grouped
    * sequence. Note, however, that this gives no real atomicity as there
    * is no way to roll back half-executed sequences.
    *
    * Groups can be nested.
    *
    * An example of [group] is to set a binding depending on the previous
    * value of the binding. Here, we add 1:
    *
    * {[
    * let add_one table =
    *    group table
    *      (fun key ->
    *        let ba =
    *          try find table key 
    *          with Not_found -> 
    *            Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
    *        ba.{ 0 } <- Int32.succ ba.{ 0 };
    *        replace table key ba
    *      )
    * ]}
    *
    * Grouping protects the update from modifications done by other processes
    * at the same time. In particular, without grouping it can happen that
    * another process also modifies the same value between [find] and
    * [replace].
   *)

val add : shm_table -> int32 -> int32_array -> unit
  (** [add tbl key value]: Adds the binding of [key] to [value] to the
    * table. Previous bindings are not removed, but simply hidden.
   *)

val find : shm_table -> int32 -> int32_array
  (** [find tbl key]: Finds the current binding of [key] in [tbl] or
    * raises [Not_found] if no such binding exists.
   *)

val find_all : shm_table -> int32 -> int32_array list
(** [find_all tbl key] returns the list of all data
  * associated with [key] in [tbl].
  * The current binding is returned first, then the previous
  * bindings, in reverse order of introduction in the table. 
  *)

val mem : shm_table -> int32 -> bool
(** [mem tbl key] checks if [key] is bound in [tbl]. *)

val remove : shm_table -> int32 -> unit
(** [remove tbl key] removes the current binding of [key] in [tbl],
  * restoring the previous binding if it exists.
  * It does nothing if [key] is not bound in [tbl].
  *)

val replace : shm_table -> int32 -> int32_array -> unit
(** [replace tbl key value] replaces the current binding of [key]
  * in [tbl] by a binding of [key] to [value].  If [key] is unbound in [tbl],
  * a binding of [key] to [value] is added to [tbl].
 *)

(*
val rename : shm_table -> int32 -> int32 -> unit
  (** [rename tbl key1 key2]: Changes the key of the current binding of
    * [key1] such that it becomes the current binding of [key2]. Raises
    * [Not_found] if [key1] is unbound.
   *)
 *)

(*
val swap : shm_table -> int32 -> int32 -> unit
  (** [swap tbl key1 key2]: determins the current bindings of [key1] and
    * [key2], and renames them to [key2] and [key1], respectively.
    * Raises [Not_found] if [key1] or [key2] is unbound.
   *)
 *)

val iter : (int32 -> int32_array -> unit) -> shm_table -> unit
(** [iter f tbl] applies [f] to all bindings in table [tbl].
  * [f] receives the key as first argument, and the associated value
  * as second argument. Each binding is presented exactly once to [f].
  * The order in which the bindings are passed to [f] is unspecified.
  * However, if the table contains several bindings for the same key,
  * they are passed to [f] in reverse order of introduction, that is,
  * the most recent binding is passed first. 
  *
  * While the iteration is in progress, the table is read-locked.
  * That means you cannot modify it during the iteration.
  *)

val iter_keys : (int32 -> unit) -> shm_table -> unit
  (** [iter_keys f tbl] applies [f] to all keys in table [tbl]. If there
    * are several bindings for a key, [f] is only called once.
    *
    * While the iteration is in progress, the table is locked.
    * That means you cannot modify it during the iteration.
   *)

val fold : (int32 -> int32_array -> 'a -> 'a) -> shm_table -> 'a -> 'a
(** [fold f tbl init] computes
  * [(f kN dN ... (f k1 d1 init)...)],
  * where [k1 ... kN] are the keys of all bindings in [tbl],
  * and [d1 ... dN] are the associated values.
  * Each binding is presented exactly once to [f].
  * The order in which the bindings are passed to [f] is unspecified.
  * However, if the table contains several bindings for the same key,
  * they are passed to [f] in reverse order of introduction, that is,
  * the most recent binding is passed first. 
  *
  * While the iteration is in progress, the table is locked.
  * That means you cannot modify it during the iteration.
  *)


val length : shm_table -> int
(** [length tbl] returns the number of bindings in [tbl].
  * Multiple bindings are counted multiply, so [length]
  * gives the number of times [iter] calls its first argument. 
 *)


(** {1 Enhanced API to shared memory tables} *)

exception Next
exception Break
val read_blocks : shm_table -> int32 -> (int32_array option -> unit) -> unit
  (** [find_blocks tbl key f]: The values may be stored in several
    * disk blocks. This interface allows one to access the values block by
    * block. As [find_all], all bindings for [key] in [tbl] are determined
    * in reverse order, i.e. the newest binding first, the oldest last.
    * For every binding [value], the function [f] is invoked in a sequence
    * [f (Some v1)], [f (Some v2)], ..., [f (Some vN)], [f None] 
    * where
    * [value] is the array concatenation of [v1], [v2], ..., [vN].
    * The function [f] may raise the exception [Next] to go
    * directly to the start of the next binding of [key].
    * The exception [Break] stops the iteration immediately.
    *
    * Note that the [int32_array] fragments [vK] point to shared memory.
    * Any assignment would modify the shared memory object directly!
    * The binding is at that time, however, only read-locked, so this
    * should be avoided.
   *)

type write_op = 
    [ `Remove_binding ]
 (* Future extensions:
  * - `Truncate_binding of int
  *)

type ctrl_op =
    [ `Nop | write_op ]

val write_blocks : shm_table -> write_op list -> int32 -> 
                   (int32_array option -> ctrl_op) -> unit
  (** [write_blocks tbl ops key f]: Like [read_blocks] this function iterates
    * over the blocks of all bindings for [key]. For every binding [value],
    * the function [f] is invoked in a sequence
    * [f (Some v1)], [f (Some v2)], ..., [f (Some vN)], [f None].
    * Unlike [read_blocks] the function [f] returns a value.
    * The last non-[`Nop] result value in this sequence determines the
    * modification to carry out for the binding:
    *
    * - [`Remove_binding]: The whole binding is removed from the table.
    *
    * If all invocations of [f] just return [`Nop], no further modification
    * is done.
    *
    * The modifications must be announced in the [ops] argument. It is
    * not allowed that [f] returns a value not being a member of [ops]
    * (except [`Nop]).
    *
    * It is possible to raise the special exceptions [Next] and [Break]
    * just as for [read_blocks].
    *
    * Note that the [int32_array] fragments [vK] point to shared memory.
    * Any assignment will modify the shared memory object directly!
    * The binding is at that time write-locked, so such assignments
    * are protected against concurrent writes.
   *)


(* debug stuff *)
val dump : shm_table -> unit
val bigarray : int array -> int32_array
val memory : shm_table -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
