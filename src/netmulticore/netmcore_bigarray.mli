(* $Id: netmcore_bigarray.mli 1580 2011-04-14 16:06:32Z gerd $ *)

(** Shared bigarrays

    This modules allows to easily create bigarrays that are shared between
    worker processes. The access to the bigarrays is managed via
    {!Netmcore.res_id} resource IDs. The lifetime of the bigarray can be 
    controlled by releasing the resource ID - if all workers release the
    ID, the bigarray is deleted (see {!Netmcore.release}).

    Note that bigarrays do not exist as part of a memory pool, but are
    always indepdendent shared objects.
 *)

val create_bigarray : ('a, 'b) kind -> 'c layout -> int array -> res_id
val create_bigarray1 : ('a, 'b) kind -> 'c layout -> int -> res_id
val create_bigarray2 : ('a, 'b) kind -> 'c layout -> int -> int -> res_id
val create_bigarray3 : ('a, 'b) kind -> 'c layout -> int -> int -> int -> res_id
  (** Create bigarrays like [Bigarray.Genarray.create] and 
      [Bigarray.ArrayX.create] in a shared resource and return the resource
      ID.
      
      The caller of these functions counts as user of the bigarray, and
      needs to release the resource when it is done.
   *)

val map_bigarray : res_id -> ('a, 'b) kind -> 'c layout -> int array -> 
                     ('a,'b,'c) Bigarray.Genarray.t
val map_bigarray1 : res_id -> ('a, 'b) kind -> 'c layout -> int -> 
                     ('a,'b,'c) Bigarray.Array1.t
val map_bigarray2 : res_id -> ('a, 'b) kind -> 'c layout -> int -> int ->
                     ('a,'b,'c) Bigarray.Array2.t
val map_bigarray3 : res_id -> ('a, 'b) kind -> 'c layout -> int -> int -> int ->
                     ('a,'b,'c) Bigarray.Array3.t
  (** Maps bigarrays to this worker process. The arguments describing the
      bigarray must be the same as at the time the bigarray was created.
      
      The caller of these functions counts as user of the bigarray, and
      needs to release the resource when it is done.
   *)
