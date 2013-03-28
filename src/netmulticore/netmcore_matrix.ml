(* $Id: netmcore_matrix.ml 1580 2011-04-14 16:06:32Z gerd $ *)

open Netmcore_heap

type ('e,'h) t =
    { mutable array : 'e array array;
      dim1 : int;
      dim2 : int;
      header : 'h
    }

type ('e,'h) sarray2 = ('e,'h) t heap
type ('e,'h) sarray2_descr = ('e,'h) t descr

let descr_of_sarray2 = descr_of_heap
let sarray2_of_descr = heap_of_descr

let create res_id n2 a h =
  let ra = { array = a; dim1 = Array.length a; dim2 = n2; header = h } in
  create_heap 
    res_id 
    (minimum_size ra)
    ra

let make res_id n1 n2 x_orig h =
  let ra = { array = [| |]; dim1 = n1; dim2 = n2; header = h } in
  let sa = create_heap res_id 4096 ra in
  modify
    sa
    (fun mut ->
       let a = add_uniform_array mut n1 [| |] in
       (root sa).array <- a;
       for k = 0 to n1-1 do
	 a.(k) <- add_uniform_array mut n2 (Obj.magic 0)
       done;
       if n1 > 0 && n2 > 0 then (
	 let x = add mut x_orig in
	 for k = 0 to n1-1 do
	   Array.fill a.(k) 0 n2 x
	 done
       );
    );
  sa
    
let init res_id n1 n2 f h =
  let ra = { array = [| |]; dim1 = n1; dim2 = n2; header = h } in
  let sa = create_heap res_id 4096 ra in
  modify
    sa
    (fun mut ->
       let a = add_uniform_array mut n1 [| |] in
       (root sa).array <- a;
       for k1 = 0 to n1-1 do
	 a.(k1) <- add_init_array mut n2 (fun k2 -> f k1 k2)
       done;
    );
  sa

let set sa k1 k2 x =
  modify
    sa
    (fun mut ->
       let a = (root sa).array in
       a.(k1).(k2) <- add mut x
    )

let get_ro sa k1 k2 =
  let a = (root sa).array in
  a.(k1).(k2)

let get_p sa k1 k2 f =
  with_value
    sa
    (fun () ->
       let a = (root sa).array in
       a.(k1).(k2)
    )
    f

let get_c sa k1 k2 =
  get_p sa k1 k2 copy

let dim sa =
  let r = root sa in
  (r.dim1, r.dim2)

let deref sa =
  (root sa).array

let header sa =
  (root sa).header

let heap sa =
  Obj.magic sa
