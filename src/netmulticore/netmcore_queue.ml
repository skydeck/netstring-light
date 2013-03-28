(* $Id: netmcore_queue.ml 1679 2011-10-18 17:08:44Z gerd $ *)

open Netmcore_heap

type ('e, 'h) q =
    { mutable head : 'e cell option;
      mutable tail : 'e cell option;
      mutable length : int;
      header : 'h
    }

and 'e cell =
    { conts : 'e;
      mutable next : 'e cell option
    }

type ('e, 'h) squeue = ('e, 'h) q heap

type ('e, 'h) squeue_descr = ('e, 'h) q descr

exception Empty


let descr_of_squeue = descr_of_heap
let squeue_of_descr = heap_of_descr

let create res_id h =
  create_heap
    res_id
    4096
    { head = None;
      tail = None;
      length = 0;
      header = h
    }

let push x sq =
  modify sq
    (fun mut ->
       let q = root sq in
       let c_orig =
	 { conts = x;
	   next = None
	 } in
       let some_c = add mut (Some c_orig) in
       match q.tail with
	 | None ->
	     q.tail <- some_c;
	     q.head <- some_c;
	     q.length <- 1
	 | Some t ->
	     t.next <- some_c;
	     q.tail <- some_c;
	     q.length <- q.length + 1
    )

let pop_p sq f =
  with_value sq
    (fun () ->
       let q = root sq in
       match q.head with
	 | None ->
	     raise Empty
	 | Some h ->
	     q.head <- h.next;
	     if q.head = None then q.tail <- None;
	     q.length <- q.length - 1;
	     h.conts
    )
    f

let pop_c sq =
  pop_p sq copy

let top_p sq f =
  with_value sq
    (fun () ->
       let q = root sq in
       match q.head with
	 | None ->
	     raise Empty
	 | Some h ->
	     h.conts
    )
    f

let top_c sq =
  top_p sq copy

let clear sq =
  modify sq
    (fun mut ->
       let q = root sq in
       q.head <- None;
       q.tail <- None;
       q.length <- 0
    )

let is_empty sq =
  (root sq).length = 0

let length sq =
  (root sq).length

let iter f sq =
  let len = ref 0 in
  with_value sq
    (fun () ->
       let q = root sq in
       len := q.length;
       q.head
    )
    (fun h_opt ->
       let r = ref h_opt in
       while !len > 0 do
	 ( match !r with
	     | None -> assert false
	     | Some c ->
		 r := c.next;
		 f c.conts
	 );
	 decr len;
       done
    )

let fold f acc0 sq =
  let acc = ref acc0 in
  iter (fun x -> acc := f !acc x) sq;
  !acc

let header sq = (root sq).header

let heap sq = Obj.magic sq
