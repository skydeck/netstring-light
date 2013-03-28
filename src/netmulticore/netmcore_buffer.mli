(* $Id: netmcore_buffer.mli 1576 2011-04-10 21:42:08Z gerd $ *)

(** Shared buffer

    One can add more data to the end of the buffer, and one can
    remove data from the beginning of the buffer.

    Additions and deletions of data are atomic, and are strictly
    serialized. Read accesses can occur in parallel, and can even
    overlap with modifications (to some degree). It is, however,
    ensured that reads do not see the parallel modification, i.e.
    reads always base on the state from the beginning of the read
    operation.

    It is not excluded that additions can be executed in parallel. If
    done this way, it is guaranteed that the effects of parallel
    additions are the same as if they were executed in a serial
    way. In particular, if an addition operation returns, this
    addition and all parallel additions affecting preceding index
    positions must be done. (The current implementation does not
    attempt this optimization.)

    Index positions are "eternal", i.e. the index position of a
    byte does not change when preceding bytes are deleted. Instead,
    a deletion merely advances the start index of the valid data
    (which is not necessarily 0). This model is more consistent
    with parallel modifications.

    On 32 bit platforms it can happen that index positions wrap
    around (at 1G). The position following [max_int] is 0. The
    length is restricted to [max_int-bsize] on these platforms.

 *)

type 'h buffer
  (** A buffer with a header of type ['h] *)

type 'h buffer_descr
  (** The marshallable buffer descriptor *)

val create : Netmcore.res_id -> int -> 'h -> 'h buffer
  (** [create pool bsize h]: Creates a buffer in [pool] with a block
      size of [bsize]. The block size can be an arbitrary positive integer
      which is always rounded up to the next multiple of the page size
      of the operating system. Blocks are the units of allocation
      of memory.
   *)

val destroy : 'h buffer -> unit
  (** Destroys the buffer *)


(** The valid index positions (e.g. for [sub]) are [start] to
    [start+length-1]:
 *)

val start : 'h buffer -> int
  (** Returns the start index *)

val length : 'h buffer -> int
  (** Returns the length *)

val contents : 'h buffer -> string
  (** Returns the full contents *)

val sub : 'h buffer -> int -> int -> string
  (** Returns a substring *)

val blit_to_string : 'h buffer -> int -> string -> int -> int -> unit
  (** Blits contents to a string *)

val blit_to_memory : 'h buffer -> int -> Netsys_mem.memory -> int -> int -> unit
  (** Blits contents to a memory buffer *)

val access : 'h buffer -> int -> (string -> int -> int -> 'a) -> 'a
  (** [access b pos f]: Gets access to the internal string backing the
      byte at position [pos]. The function [f] is called as [f s k n]
      so that [s.[k]] is the requested byte at [pos]. The number [n]
      is the number of valid bytes in the string.

      During the execution of [f] the string [s] is pinned and cannot be
      deleted by the garbage collector.
   *)

val add_string : 'h buffer -> string -> unit
  (** Adds a string to the end of the buffer *)

val add_sub_string : 'h buffer -> string -> int -> int -> unit
  (** Adds a sub string to the end of the buffer *)

val add_sub_memory : 'h buffer -> Netsys_mem.memory -> int -> int -> unit
  (** Adds a sub memory buffer to the end of the buffer *)

val delete_hd : 'h buffer -> int -> unit
  (** [delete_hd b n]: Deletes [n] bytes from the beginning of the buffer.
      This means that the [start] index is increased by [n].
   *)

val clear : 'h buffer -> unit
  (** Deletes all contents of the buffer *)

val header : 'h buffer -> 'h
  (** Returns the header *)

val descr_of_buffer : 'h buffer -> 'h buffer_descr
  (** Returns the descriptor *)

val buffer_of_descr : Netmcore.res_id -> 'h buffer_descr -> 'h buffer
  (** Look up the buffer for this descriptor *)

val heap : _ buffer -> Obj.t Netmcore_heap.heap
  (** Returns the underlying heap *)
