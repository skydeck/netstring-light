(* $Id: netmcore_array.mli 1580 2011-04-14 16:06:32Z gerd $ *)

(** Shared arrays *)

(** This type of array is to some degree comparable with [Array], but
    there are a few extensions:
    - There is a so-called header. The header can have any type, and
      is typically used for managing concurrent access (e.g. mutexes).
      The header also lives in shared memory.
    - The arrays can be grown in size
 *)

type ('e,'h) sarray
  (** Arrays where the elements have type ['e] and the header has
      type ['h]
   *)

type ('e,'h) sarray_descr
  (** The marshallable descriptor of a shared array *)

val create : Netmcore.res_id -> 'e array -> 'h -> ('e,'h) sarray
  (** [create pool_id a h]:
      Creates a shared array by deeply copying a normal array [a]
      and using the copy of [h] as header
   *)

val make : Netmcore.res_id -> int -> 'e -> 'h -> ('e,'h) sarray
  (** [make pool_id n x h]:
      Creates a shared array of the passed number of elements, 
      copies the element [x], and initializes each element of the new array
      with the single copy of [x]. The value [h] is copied and used
      as header.
   *)

val init : Netmcore.res_id -> int -> (int -> 'e) -> 'h -> ('e,'h) sarray
  (** [init pool_id n f h]:
      Creates a shared array of the passed number of elements, 
      and for getting the element at position [k] the function
      [f k] is run, and the copy of the result is written to the
      position. The header is set to the copy of [h].
   *)

val grow : ('e,_) sarray -> int -> 'e -> unit
  (** [grow sa n x]: Grows the array to [n] elements. The new elements
      are initialized to a (single) copy of [x].

      If [n] is smaller than the current length, the function will do
      nothing, and keep the length.
   *)

val set : ('e,_) sarray -> int -> 'e -> unit
  (** [set sa k x]: Sets the [k-th] element of the array [sa] to a
      deep copy of [x].
   *)

val get_ro : ('e,_) sarray -> int -> 'e
  (** [get_ro sa k]: Gets the [k]-th element of the shared array [sa].
      Note that there is no guarantee that this value still exists if
      it is returned, and a parallely running [set] changes this element.
      If such values are accessed the program may crash!
   *)

val get_p : ('e,_) sarray -> int -> ('e -> 'a) -> 'a
  (** [get_p sa k f]: Gets the [k]-th element of the shared array [sa]
      and call [f] with this element, and returns the result of [f].
      During the execution of [f] the requested element cannot be
      garbage collected.
   *)

val get_c : ('e,_) sarray -> int -> 'e
  (** [get_c sa k]: Gets a copy of the [k]-th element of the shared array
      [sæ]
   *)

val length : (_,_) sarray -> int
  (** Returns the length *)

val header : (_,'h) sarray -> 'h
  (** Returns the header *)

val deref : ('e,_) sarray -> 'e array
  (** Returns the raw array in shared memory for unprotected access *)

val heap : (_,_) sarray -> Obj.t Netmcore_heap.heap
  (** Return the backing heap structure *)

val descr_of_sarray : ('e,'h) sarray -> ('e,'h) sarray_descr
  (** Returns the descriptor *)

val sarray_of_descr : Netmcore.res_id -> ('e,'h) sarray_descr -> ('e,'h) sarray
  (** Look up the buffer for this descriptor *)



(** {3 Mutating header fields}

    Special care has to be taken when mutating header fields. The header
    must completely live in the same heap. For adding new values, one
    has to use {!Netmcore_heap.modify}. Example for a header of type:

    {[
    type header =
      { mutable n : int;
        mutable name : string
      }
    ]}

    Here, the field [n] can be directly assigned because an [int]
    is always an unboxed value. So,

    {[
    h.n <- new_value
    ]}

    is legal. However, strings are heap-allocated. For an assignment
    to [name] we need to use {!Netmcore_heap.modify}, as in

    {[
    Netmcore_heap.modify
      (Netmcore_array.heap sa)
      (fun mutator ->
        h.name <- Netmcore_heap.add mutator new_value
      )
    ]}

    The function {!Netmcore_heap.add} pushes a copy of the [new_value]
    to the heap, and this allows us to do the assignment.

    During {!Netcore_heap.modify} certain operations are prohibited
    because they would cause a deadlock:
    - [grow]
    - [set]
    - [get_p]
    - [get_c]

    (This may be relaxed in a future version.)
 *)
