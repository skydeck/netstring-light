(* $Id: netmcore_queue.mli 1574 2011-04-10 15:13:54Z gerd $ *)

(** Shared queues *)

type ('e, 'h) squeue
  (** Queues where the elements have type ['e] and the header has
      type ['h]
   *)

type ('e,'h) squeue_descr
  (** The marshallble descriptor of queues *)

exception Empty
  (** Raised when the queue is empty and the operation cannot be done *)

val create : Netmcore.res_id -> 'h -> ('e,'h) squeue
  (** [create pool h]: Creates an empty queue in the [pool], with header
      [h]
   *)

val push : 'e -> ('e,'h) squeue -> unit
  (** [push x q]: Pushes a copy of [x] to the end of the queue [q] *)

val pop_p : ('e,'h) squeue -> ('e -> 'a) -> 'a
  (** [pop_p q f]: Takes the first element [x] from the queue, removes it
      there, and calls [f x]. During the execution of [f] the value [x]
      is pinned and cannot be garbage-collected.

      Raises [Empty] if the queue is empty.
   *)

val pop_c : ('e,'h) squeue -> 'e
  (** [pop_c q]: Takes the first element [x] from the queue, removes it
      there, and returns a copy of [x] in normal memory.

      Raises [Empty] if the queue is empty.
   *)

val top_p : ('e,'h) squeue -> ('e -> 'a) -> 'a
  (** [pop_p q f]: Takes the first element [x] of the queue, 
      and calls [f x], without removing [x] from the queue. 
      During the execution of [f] the value [x]
      is pinned and cannot be garbage-collected.

      Raises [Empty] if the queue is empty.
   *)

val top_c : ('e,'h) squeue -> 'e
  (** [pop_p q f]: Takes the first element [x] of the queue, 
      and calls [f x], without removing [x] from the queue. 
      Returns a copy of [x] in normal memory.

      Raises [Empty] if the queue is empty.
   *)

val clear : ('e,'h) squeue -> unit
  (** Removes all elements from the queue *)

val is_empty : ('e,'h) squeue -> bool
  (** Tests whether the queue is empty *)

val length : ('e,'h) squeue -> int
  (** Returns the number of elements in the queue (O(1)) *)

val iter : ('e -> unit) -> ('e,'h) squeue -> unit
  (** [iter f q]: Iterates over the elements of the queue and calls [f x]
      for each element [x]. The function considers the list of elements
      at the time of calling [iter] as the list to iterate over. The
      queue is not locked during the iteration, and hence elements can be
      popped from the queue and pushed to the queue in parallel. The
      iteration does not take these modifications into account, though.

      The elements [x] are pinned during the execution of [f] and will
      not be garbage-collected, even if a parallel [pop] removes them from
      the queue.
   *)

val fold : ('a -> 'e -> 'a) -> 'a -> ('e,'h) squeue -> 'a
  (** [fold f accu q] *)

val header : ('e,'h) squeue -> 'h
  (** Returns the header *)

val heap : (_,_) squeue -> Obj.t Netmcore_heap.heap
  (** Returns the underlying heap *)

val descr_of_squeue : ('e,'h) squeue -> ('e,'h) squeue_descr
  (** Returns the descriptor *)

val squeue_of_descr : Netmcore.res_id -> ('e,'h) squeue_descr -> ('e,'h) squeue
  (** Look up the queue for this descriptor *)

