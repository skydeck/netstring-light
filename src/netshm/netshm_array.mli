(* $Id: netshm_array.mli 999 2006-09-19 20:28:01Z gerd $ *)

(** Arrays in shared memory *)

type 'a t

val manage : ?pagesize:int -> 
             ?init:int -> 
             'a ->
             'a Netshm_data.data_manager ->
             Netshm.locking_method -> 
             Netshm.shm_descr ->
               'a t
  (** Manages a shared memory object as an array,
    * including the representation of arbitrary O'Caml values.
    * The first argument of type ['a] is the default value of the
    * array elements.
    *
    * This bases on [Netshm.manage], and the arguments [pagesize],
    * [locking_method] and [shm_descr] are documented there.
    *
    * If an empty memory object is managed, it is initialized as
    * array with zero elements. If a non-empty memory object is
    * managed, it must contain a valid array structure.
    * The size of the array is then the same as when the array
    * was managed the last time.
    *
    * By passing [init] with argument [n], the array is reinitialized as
    * array with [n] elements containing the default value.
    *
    * It is essential that the same data managers are passed as at the time
    * when the array was initialized.
    *
    * Arrays are implemented as [(int32, 'a) Netshm_hashtbl.t].
   *)

val length : 'a t -> int
  (** Returns the length of the array *)

val get : 'a t -> int -> 'a
  (** [get a k]: Returns the contents of the array element number [k] where
    * [0 <= k < length a].
    *
    * If you do [module Array = Netshm_array] in your code you can also
    * use the notation [a.(k)].
   *)

val set : 'a t -> int -> 'a -> unit
  (** [set a k x]: Sets the contents of the array element number [k] to [x]
    * where [0 <= k < length a].
    *
    * If you do [module Array = Netshm_array] in your code you can also
    * use the notation [a.(k) <- x].
   *)

(*
val swap : 'a t -> int -> int -> unit
  (** [swap a j k]: Swaps the contents of the array elements [j] and [k].
    * This function is a lot more efficient than programming swapping with
    * [get] and [set].
   *)
 *)

val resize : 'a t -> int -> unit
  (** [resize a n]: Resizes the array to length [n]. If the array is enlarged
    * the new elements will be initialized to the default value.
   *)

val default_value : 'a t -> 'a
  (** Returns the default value *)

val shm_table : 'a t -> Netshm.shm_table
  (** Returns the underlying shared memory table used to implement hash
    * tables
   *)
