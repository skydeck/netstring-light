(* $Id: netshm_array.ml 996 2006-09-19 15:39:34Z gerd $ *)

open Netshm
open Netshm_data

type 'a t =
    { table : shm_table;
      htbl : (int, 'a) Netshm_hashtbl.t;
      (* We use the hash table to store the normal elements. For the
       * length of the array, we use the index (-1) which is mapped
       * to an int instead of 'a. Implemented by bypassing htbl in this
       * case.
       *)
      idx_only_manager : int data_manager;
      idx_len_manager : (int * int) data_manager;
      default_val : 'a;
    }


let shm_table t = t.table

let with_length f t =
  let r = ref None in
  Netshm.read_blocks
    t.table
    (-1l)
    (fun frag_opt ->
       match frag_opt with
	 | Some frag ->
	     let length = Int32.to_int frag.{ 0 } in
	     r := Some (f length);
	     raise Break
	 | None ->
	     assert false
    );
  match !r with
    | None -> 
	assert false
    | Some x ->
	x

let length t = with_length (fun l -> l) t


let get t k = 
  with_length
    (fun length ->
       if k < 0 || k >= length then
	 invalid_arg "Netshm_array.get: index out of bounds";
       try
	 Netshm_hashtbl.find t.htbl k
       with
	 | Not_found -> t.default_val
    )
    t
;;


let set t k x = 
  with_length
    (fun length ->
       if k < 0 || k >= length then
	 invalid_arg "Netshm_array.set: index out of bounds";
       Netshm_hashtbl.replace t.htbl k x
    )
    t

let resize t n = 
  if n < 0 then invalid_arg "Netshm_array.resize";
  with_length
    (fun old_len ->
       (* First remove any element at indexes >= n *)
       if n < old_len then (
	 let l = ref [] in
	 Netshm_hashtbl.iter_keys
	   (fun idx -> 
	      if idx >= n then l := idx :: !l )
	   t.htbl;
	 List.iter
	   (fun idx ->
	      Netshm_hashtbl.remove t.htbl idx
	   )
	   !l
       );

       (* Finally just set the new length field: *)
       Netshm.write_blocks
	 t.table
	 []
	 (-1l)
	 (fun frag_opt ->
	    match frag_opt with
	      | Some frag ->
		  frag.{ 0 } <- Int32.of_int n;
		  raise Break
	      | None ->
		  assert false
	 )
    )
    t


let default_value t = t.default_val

let shm_table t = t.table


let manage ?pagesize ?init defval val_manager lm sd =
  let shm_init =
    match init with
      | Some n -> Some 1000
      | None -> None in
  let idx_manager = Netshm_data.int_manager in
  let htbl = 
    Netshm_hashtbl.manage
      ?pagesize ?init:shm_init idx_manager val_manager lm sd in
  let table = Netshm_hashtbl.shm_table htbl in
  let idx_only_manager =
    left_pair_manager idx_manager in
  let idx_len_manager =
    pair_manager idx_manager Netshm_data.int_manager in
  let t = 
    { table = table;
      htbl = htbl;
      idx_only_manager = idx_only_manager;
      idx_len_manager = idx_len_manager;
      default_val = defval
    } in
  Netshm.group table
    (fun () ->
       if not (Netshm.mem table (-1l)) then
	 let length_field = 
	   Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
	 length_field.{ 0 } <- 0l;
	 Netshm.add table (-1l) length_field
    )
    ();
  ( match init with
      | None ->
	  ()
      | Some n ->
	  resize t n
  );
  t
