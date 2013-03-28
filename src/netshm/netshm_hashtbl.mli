(* $Id: netshm_hashtbl.mli 999 2006-09-19 20:28:01Z gerd $ *)

(** Hash tables in shared memory *)

type ('a, 'b) t

val manage : ?pagesize:int -> 
             ?init:int -> 
             'a Netshm_data.data_manager ->
             'b Netshm_data.data_manager ->
             Netshm.locking_method -> 
             Netshm.shm_descr ->
               ('a,'b) t
  (** Manages a shared memory object as a hash table like [Netshm.manage],
    * and additionally represent arbitrary O'Caml values.
    *
    * It is essential that the same data managers are passed as at the time
    * when the hash table was initialized.
    *
    * For example, to get a hash table from [int] to [string], use
    * {[ manage Netshm_data.int_manager Netshm_data.string_manager lm sd ]}
   *)

val add : ('a,'b) t -> 'a -> 'b -> unit
  (** [add tbl key value]: Adds the binding of [key] to [value] to the
    * table. Previous bindings are not removed, but simply hidden.
   *)

val find : ('a,'b) t -> 'a -> 'b
  (** [find tbl key]: Finds the current binding of [key] in [tbl] or
    * raises [Not_found] if no such binding exists.
   *)

val find_all : ('a,'b) t -> 'a -> 'b list
(** [find_all tbl key] returns the list of all data
  * associated with [key] in [tbl].
  * The current binding is returned first, then the previous
  * bindings, in reverse order of introduction in the table. 
  *)

val mem : ('a,'b) t -> 'a -> bool
(** [mem tbl key] checks if [key] is bound in [tbl]. *)

val remove : ('a,'b) t -> 'a -> unit
(** [remove tbl key] removes the current binding of [key] in [tbl],
  * restoring the previous binding if it exists.
  * It does nothing if [key] is not bound in [tbl].
  *)

val replace : ('a,'b) t -> 'a -> 'b -> unit
(** [replace tbl key value] replaces the current binding of [key]
  * in [tbl] by a binding of [key] to [value].  If [key] is unbound in [tbl],
  * a binding of [key] to [value] is added to [tbl].
 *)

val iter : ('a -> 'b -> unit) -> ('a,'b) t -> unit
(** [iter f tbl] applies [f] to all bindings in table [tbl].
  * [f] receives the key as first argument, and the associated value
  * as second argument. Each binding is presented exactly once to [f].
  * The order in which the bindings are passed to [f] is unspecified.
  * However, if the table contains several bindings for the same key,
  * they are passed to [f] in reverse order of introduction, that is,
  * the most recent binding is passed first. 
  *
  * While the iteration is in progress, the table is locked.
  * That means you cannot modify it during the iteration.
  *)

val iter_keys : ('a -> unit) -> ('a,'b) t -> unit
  (** [iter_keys f tbl] applies [f] to all keys in table [tbl]. If there
    * are several bindings for a key, [f] is only called once.
    *
    * While the iteration is in progress, the table is locked.
    * That means you cannot modify it during the iteration.
   *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
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


val length : ('a,'b) t -> int
(** [length tbl] returns the number of bindings in [tbl].
  * Multiple bindings are counted multiply, so [length]
  * gives the number of times [iter] calls its first argument. 
 *)


val shm_table : ('a,'b) t -> Netshm.shm_table
  (** Returns the underlying shared memory table used to implement hash
    * tables.
   *)
