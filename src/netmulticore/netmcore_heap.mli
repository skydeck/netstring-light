(* $Id: netmcore_heap.mli 1826 2013-01-13 17:57:16Z gerd $ *)

(** Shared heaps of structured values

    These heaps live in {!Netmcore_mempool}-type shared memory pools,
    and can contain an arbitrary number of Ocaml values. These values
    can be mutable, but special care has to be taken when modifying them.
    The first value pushed onto the heap is called the {i root element}.
    All other values must be (directly or indirectly) reachable from the
    root element.

    Heaps are created with a certain initial size. The heaps remain
    connected with the memory pool, and they are enlarged if necessary
    by allocating more blocks in the pool.

    As the heaps are shared between processes, it must be taken care
    that no reference is made from shared heaps to normal process-local
    memory. These references would only be valid in the process creating
    them, and following such references from other processes would probably
    crash the program (or at least return wrong values). In order to ensure
    this, it is strictly forbidden to directly manipulate mutable
    data structures. The {!Netmcore_heap.modify} function has to be used,
    as this function makes it possible to copy more values to the heap.
    Unfortunately, there is nothing in the type system that would prevent
    direct mutation. so this can only be ensured by the discipline of the
    programmer.

    The values of the heap are also garbage-collected: If all allocated
    space is used and more values need to be added, it is first tried
    to get rid of old unreachable values. The garbarge collection is done
    by the process that happens to initiate the addition of the value
    that does no more fit onto the heap. During garbage collection, no
    other additions can be done, but read accesses are not prevented.
    The garbage collector does not move values (addresses remain unchanged).

    The garabage collector only considers values as reachable that are
    reachable via the root element. It is not sufficient when a value
    is only reachable via a process-specific reference.
 *)

type 'a heap
  (** A heap where the type of the root element is ['a] *)

type 'a descr
  (** A descriptor ("address") pointing to the heap. Descriptors
      can be marshalled.
   *)

val create_heap : Netmcore.res_id -> int -> 'a -> 'a heap
  (** [create_heap pool_id size root]: Creates a new heap with [size]
      bytes in the pool identified by [pool_id]. This ID must refer
      to a {!Netmcore_mempool}-managed pool.

      The value [root] is copied to the new heap. This is done by
      deeply duplicating [root] and all values pointed to by [root],
      and storing these duplicates in the heap.

      The possible types of value [root] are restricted, see the [add]
      function for more.
   *)

val minimum_size : 'a -> int
  (** Returns the [size] value one must pass to [create_heap] at minimum
      to put this root element onto the heap.
   *)

val root : 'a heap -> 'a
  (** Returns the root element *)

val descr_of_heap : 'a heap -> 'a descr
  (** Get the descriptor of a heap *)

val heap_of_descr : Netmcore.res_id -> 'a descr -> 'a heap
  (** [heap_of_descr pool d]:
      Get the heap for a descriptor. This assumes that the heap still
      exists.
   *)

type mutator
  (** Mutators allow it to push new values onto the heap.

      Caveat: pushed values are not considered as roots, and thus they
      need immediately be attached to the existing data structure.
      Otherwise, the next push might trigger a garbage collection, and
      the new value is deleted. If this is not possible, one can
      call [pin] instead (see below).
   *)

val modify : 'a heap -> (mutator -> 'r) -> 'r
  (** [modify h mutate]: This function locks the heap so that this process
      has exclusive write access to it for the duration of the [mutate]
      function. The [mutate] function is immediately called back, and
      the argument of [mutate] is the mutator [m] that allows one to push
      values onto the heap.

      By calling [add m x] from the body of [mutate] one can create a copy
      of [x] that is stored in the heap. 
   *)

val add : mutator -> 'a -> 'a
  (** Pushes a new value onto the heap. This creates a deep copy of the
      value.

      Not all values can be pushed here. In particular, forbidden are:
      - Functions
      - Objects
      - Unsupported custom blocks, e.g. [in_channel] and [out_channel].
        Supported custom blocks are only [int32], [int64], [nativeint],
        and bigarrays.
      - Heaps (a heap cannot contain a heap)
      - Values containing semaphores and other synchronization primitives.
        Explicitly allowed are dummy primitives like {!Netmcore_mutex.dummy}.
   *)

val add_immutable : mutator -> 'a -> 'a
  (** Pushes a new value onto the heap. This function must only be used
      if the added value is immutable. An important optimization is applied
      here: if parts of the value are already living on the heap, these
      parts are not copied, but shared with the output value.

      The same value restrictions apply as for [add]. Note that
      [add_immutable] cannot be used if the value to copy lives in a different
      heap of the same pool (as a whole or partially). In this case use
      [add].
   *)

val add_uniform_array : mutator -> int -> 'a -> 'a array
  (** [add_uniform_array m n x]: Pushes a new value with n elements onto
      the heap. Each index position of the array is initialized with
      the same copy of [x].

      You should not call this function with [n=0], because this results in
      a copied atom, which is an illegal representation in OCaml.
   *)

val add_init_array : mutator -> int -> (int -> 'a) -> 'a array
  (** [add_init_array m n f]: Pushes a new value with n elements onto
      the heap. The index position [k] is inititialized by running
      [f k] and pushing the copy of this onto the heap.

      You should not call this function with [n=0], because this results in
      a copied atom, which is an illegal representation in OCaml.
   *)

val add_some : mutator -> 'a -> 'a option
  (** [add_some mut x]: Returns [Some x] where the O'Caml value representing
      [Some] is allocated in the heap using [mut]. It is assumed that [x] is
      already  a resident of the heap. This means [x] is not copied!
   *)

val add_string : mutator -> int -> string
  (** [let s = add_string mut len]: Adds an uninitialized string of length
      [len] to the heap using [mut], and returns the string
   *)

val pin : mutator -> 'a -> unit
  (** [pin m x]: Pins a shared value [x] so it cannot be deleted by
      the garbage collector. The value remains pinned for the lifetime
      of the mutator [m] (i.e. the runtime of the [modify] function).

      Pinning is relatively expensive if done in masses, and should be
      avoided if possible.
   *)

val copy : 'a -> 'a
  (** Creates a deep copy of the input value, and stores the duplicate
      in normal process memory.
   *)

val with_value : 'a heap -> (unit -> 'b) -> ('b -> 'c) -> 'c
  (** [with_value h find process]: Logically, this runs
      [process (find ())] and returns the result. While [find] is being
      executed, the heap is write-locked. This returns a value [x].
      While [process] is being executed, the value [x] is temporarily
      added to the set of reachable values, so that a parallely running
      garbage collection will not delete it.
      
      Note that [x] {b must} reside in the heap!

      Calling [modify] from [find] will cause a deadlock. Calling 
      it from [process] is allowed.
   *)

val with_value_2 : 'a heap -> (unit -> ('b * 'c)) -> ('b * 'c -> 'z) -> 'z
val with_value_3 : 'a heap -> (unit -> ('b * 'c * 'd)) -> ('b * 'c * 'd -> 'z) -> 'z
val with_value_4 : 'a heap -> (unit -> ('b * 'c * 'd * 'e)) -> ('b * 'c * 'd * 'e -> 'z) -> 'z
val with_value_5 : 'a heap -> (unit -> ('b * 'c * 'd * 'e * 'f)) -> ('b * 'c * 'd * 'e * 'f -> 'z) -> 'z
  (** Same as [with_value], but a tuple of values can be passed down
   *)

val with_value_n : 'a heap -> (unit -> 'b list) -> ('b list -> 'c) -> 'c
  (** Same as [with_value], but a list of values can be passed down
   *)

val destroy : 'a heap -> unit
  (** Destroys the heap and gives the memory back to the pool *)

val gc : 'a heap -> unit
  (** Lock the heap and do a GC pass *)

val pool : 'a heap -> Netmcore.res_id
  (** Return the pool ID *)

val mut_pool : mutator -> Netmcore.res_id
  (** Return the pool ID *)

val sem_container : 'a heap -> Netsys_sem.container
  (** Return the semaphore container *)

val mut_sem_container : mutator -> Netsys_sem.container
  (** Return the semaphore container *)

val debug_info : 'a heap -> string
  (** Returns a multi-line debug string *)


(** {2 Example: Mutable Variable}

    This example creates a heap that stores a single value. (This is
    available as {!Netmcore_ref}.)

    {[
    let shared_ref x =
      (* The shm version of [ref x] *)
      let r = ref x in
      let init_size = minimum_size r in
      let hp = create_heap pool_id init_size r in
      hp

    let deref sref =
      (* The shm version of [!] *)
      !(root sref)

    let assign sref x =
      (* The shm version of [:=] - however, a copy of x is done *)
      modify sref
        (fun add ->
          (root sref) := add x
        )
    ]}

 *)


module Debug : sig
  val enable : bool ref
end
