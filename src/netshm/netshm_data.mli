(* $Id: netshm_data.mli 999 2006-09-19 20:28:01Z gerd $ *)

(** Data representation for shared memory *)

type 'a data_manager = 
    { to_int32_array : 'a -> Netshm.int32_array;
        (** Represent the value as an [int32_array] *)
      of_int32_array : Netshm.int32_array list -> 'a;
        (** Read the value back from its [int32_array] representation.
          * The array is given as list of array fragments in reverse
          * order. The fragments must not be empty.
         *)
      of_int32_array_prefix : (Netshm.int32_array list -> 'a option) option;
	(** Read the value back from its [int32_array] representation.
          * Unlike [of_int32_array], it is allowed to pass a prefix
          * of the whole array to this function. As [of_int32_array],
          * this prefix is given as list of array fragments in reverse
          * order. The function may return [None] if it is not yet
          * possible to reconstruct the value. Otherwise the value is
          * returned as [Some v].
         *)
      hash_fn : 'a -> int32
        (** Hash function *)
    }
  (** The data manager consists of several manager functions.
    * [of_int32_array_prefix] is optional.
   *)

val int32_manager : int32 data_manager
  (** Represents an [int32] as one-element [int32_array] *)

val int64_manager : int64 data_manager
  (** Represents an [int64] as two-element [int32_array] *)

val nativeint_manager : nativeint data_manager
  (** Uses either [int32_manager] or [int64_manager] to represent [nativeint],
    * depending on the size of [nativeint].
   *)

val int_manager : int data_manager
  (** Uses either [int32_manager] or [int64_manager] to represent [int],
    * depending on the size of [int].
   *)

val int32_array_manager : Netshm.int32_array data_manager
  (** The identity representation manager *)

val string_manager : string data_manager
  (** Represents a string in the following way. The first element 
    * is the size of the string. The following elements store the
    * bytes. The last word is filled up with zero bytes if necessary.
   *)

val pair_manager : 'a data_manager -> 'b data_manager -> ('a * 'b) data_manager
  (** Creates a compound manager for pairs from two input managers *)

val left_pair_manager : 'a data_manager -> 'a data_manager
  (** Uses the same representation as [pair_manager], but the resulting
    * data manager only reads the left value of the pair.
    *
    * This data manager does not support [to_int32_array].
   *)

val option_manager : 'a data_manager -> 'a option data_manager
  (** Creates a data manager from an input data manager for optional values *)


