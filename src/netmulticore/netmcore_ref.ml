(* $Id: netmcore_ref.ml 1580 2011-04-14 16:06:32Z gerd $ *)

open Netmcore_heap

type 't sref = 't ref heap

type 't sref_descr = 't ref descr

let descr_of_sref = descr_of_heap
let sref_of_descr = heap_of_descr


let sref res_id x =
  create_heap res_id (minimum_size x) (ref x)

let assign r x =
  modify r (fun mut -> (root r) := add mut x)

let deref_ro r =
  !(root r)

let deref_p r f =
  with_value r (fun () -> !(root r)) f

let deref_c r =
  deref_p r copy

let heap r =
  Obj.magic r
