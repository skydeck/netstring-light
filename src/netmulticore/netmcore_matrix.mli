(* $Id: netmcore_matrix.mli 1580 2011-04-14 16:06:32Z gerd $ *)

(** Shared 2-dimensional arrays (matrices) *)

type ('e,'h) sarray2
  (** Arrays where the elements have type ['e] and the header has
      type ['h]
   *)

type ('e,'h) sarray2_descr
  (** The marshallable descriptor of a shared matrix *)

val create : Netmcore.res_id -> int -> 'e array array -> 'h -> ('e,'h) sarray2
  (** [create pool_id n2 a h]:
      Creates a matrix by deeply copying a normal array [a]
      and using the copy of [h] as header. In [n2] one has
      to pass the second dimension.
   *)

val make : Netmcore.res_id -> int -> int -> 'e -> 'h -> ('e,'h) sarray2
  (** [make pool_id n1 n2 x h]:
      Creates a matrix of the passed number of elements (n1 * n2), 
      copies the element [x], and initializes each element of the new array
      with the single copy of [x]. The value [h] is copied and used
      as header.
   *)

val init : Netmcore.res_id -> int -> int -> (int -> int -> 'e) -> 'h -> 
              ('e,'h) sarray2
  (** [init pool_id n1 n2 f h]:
      Creates a matrix of the passed number of elements (n1 * n2),
      and for getting the element at position [k] the function
      [f k] is run, and the copy of the result is written to the
      position. The header is set to the copy of [h].
   *)

val set : ('e,_) sarray2 -> int -> int -> 'e -> unit
  (** [set sa k1 k2 x]: Sets the [(k1,k2)-th] element of the matrix [sa] to a
      deep copy of [x].
   *)

val get_ro : ('e,_) sarray2 -> int -> int -> 'e
  (** [get_ro sa k1 k2]: Gets the [(k1,k2)]-th element of the matrix [sa].
      Note that there is no guarantee that this value still exists if
      it is returned, and a parallely running [set] changes this element.
      If such values are accessed the program may crash! 
  *)

val get_p : ('e,_) sarray2 -> int -> int -> ('e -> 'a) -> 'a
  (** [get_p sa k1 k2 f]: Gets the [(k1,k2)]-th element of the matrix [sa]
      and call [f] with this element, and returns the result of [f].
      During the execution of [f] the requested element cannot be
      garbage collected.
   *)

val get_c : ('e,_) sarray2 -> int -> int -> 'e
  (** [get_c sa k1 k2]: Gets a copy of the [(k1,k2)]-th element of the matrix
      [sæ]
   *)

val dim : (_,_) sarray2 -> int * int
  (** Returns the dimenstions *)

val header : (_,'h) sarray2 -> 'h
  (** Returns the header *)

val deref : ('e,_) sarray2 -> 'e array array
  (** Returns the raw array in shared memory for unprotected access *)

val heap : (_,_) sarray2 -> Obj.t Netmcore_heap.heap
  (** Return the backing heap structure *)

val descr_of_sarray2 : ('e,'h) sarray2 -> ('e,'h) sarray2_descr
  (** Returns the descriptor *)

val sarray2_of_descr : 
      Netmcore.res_id -> ('e,'h) sarray2_descr -> ('e,'h) sarray2
  (** Look up the matrix for this descriptor *)

