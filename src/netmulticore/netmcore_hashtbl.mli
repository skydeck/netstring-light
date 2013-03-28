(* $Id: netmcore_hashtbl.mli 1574 2011-04-10 15:13:54Z gerd $ *)

(** Shared hashtables *)

(** This is a shared memory version of [Hashtbl]. Note that the degree
    of parallelization is quite restricted - practically all operations
    need to be serialized (with the exception of the "ro" variants).
 *)

type ('a, 'b, 'h) t
(** The type of hash tables from type ['a] to type ['b] and a
    header of type ['h]
 *)

type ('a, 'b, 'h) t_descr
  (** The marshallable descriptor of a shared hash table *)

val create : Netmcore.res_id -> 'h -> ('a, 'b, 'h) t
(** [Hashtbl.create pool h] creates a new, empty hash table in [pool]
    with a header [h]. *)

val clear : ('a, 'b, 'h) t -> unit
(** Empty a hash table. *)


val add : ('a, 'b, 'h) t -> 'a -> 'b -> unit
(** [add tbl x y] adds a binding of [x] to [y] in table [tbl].
   Previous bindings for [x] are not removed, but simply
   hidden. That is, after performing {!Netmcore_hashtbl.remove}[ tbl x],
   the previous binding for [x], if any, is restored.
   (Same behavior as with association lists.) *)

val find_ro : ('a, 'b, 'h) t -> 'a -> 'b
(** [find_ro tbl x] returns the current binding of [x] in [tbl],
   or raises [Not_found] if no such binding exists. If it is possible
    that the table is being modified at the same time, this function
    can crash. (Suffix "_ro" = for "read-only" hashtables.)
 *)

val find_p : ('a, 'b, 'h) t -> 'a -> ('b -> 'r) -> 'r
  (** [find_p tbl x f] looks up the current binding of [x] in [tbl],
      and calls [f] with this binding as argument. During the execution
      of [f] the binding is pinned and cannot be garbage-collected.
      Raises [Not_found] if there is no such binding.
   *)

val find_c : ('a, 'b, 'h) t -> 'a -> 'b
  (** Like [find] but returns a copy of the binding in normal RAM *)

val find_all_ro : ('a, 'b, 'h) t -> 'a -> 'b list
(** [Hashtbl.find_all tbl x] returns the list of all data
   associated with [x] in [tbl].
   The current binding is returned first, then the previous
   bindings, in reverse order of introduction in the table.
    If it is possible
    that the table is being modified at the same time, this function
    can crash. (Suffix "_ro" = for "read-only" hashtables.)
 *)


val find_all_p : ('a, 'b, 'h) t -> 'a -> ('b list -> 'r) -> 'r
  (** Version of [find_all] with pinned result *)

val find_all_c : ('a, 'b, 'h) t -> 'a -> 'b list
  (** Version of [find_all] with copied result *)

val mem_ro : ('a, 'b, 'h) t -> 'a -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. If it is possible
    that the table is being modified at the same time, this function
    can crash. (Suffix "_ro" = for "read-only" hashtables.)
 *)

val mem : ('a, 'b, 'h) t -> 'a -> bool
  (** Safe version of [mem_ro] in the presence of parallel modifications.
      It is a bit slower, though.
   *)


val remove : ('a, 'b, 'h) t -> 'a -> unit
(** [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
   restoring the previous binding if it exists.
   It does nothing if [x] is not bound in [tbl]. *)

val replace : ('a, 'b, 'h) t -> 'a -> 'b -> unit
(** [Hashtbl.replace tbl x y] replaces the current binding of [x]
   in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
   a binding of [x] to [y] is added to [tbl].
   This is functionally equivalent to {!Hashtbl.remove}[ tbl x]
   followed by {!Hashtbl.add}[ tbl x y]. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b, 'h) t -> unit
(** [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
   [f] receives the key as first argument, and the associated value
   as second argument. Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first.

    The table cannot be modified while [iter] is running. Any attempt
    will result in a deadlock.
 *)

val length : ('a, 'b, 'h) t -> int
(** [Hashtbl.length tbl] returns the number of bindings in [tbl].
   Multiple bindings are counted multiply, so [Hashtbl.length]
   gives the number of times [Hashtbl.iter] calls its first argument. *)

val header : ('a, 'b, 'h) t -> 'h
  (** Returns the header *)

val heap : ('a, 'b, 'h) t -> Obj.t Netmcore_heap.heap
  (** Returns the heap backing this data structure *)

val descr_of_hashtbl : ('a,'b,'h) t -> ('a,'b,'h) t_descr
  (** Returns the descriptor *)

val hashtbl_of_descr : Netmcore.res_id -> ('a,'b,'h) t_descr -> ('a,'b,'h) t
  (** Look up the hash table for this descriptor *)


