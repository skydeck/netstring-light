(* $Id: netshm_hashtbl.ml 996 2006-09-19 15:39:34Z gerd $ *)

open Netshm
open Netshm_data

type ('a, 'b) t =
    { table : shm_table;
      key_only_manager : 'a data_manager;
      key_val_manager : ('a * 'b) data_manager;
      key_hash_fn : 'a -> int32;
    }

let manage ?pagesize ?init key_manager val_manager lm sd =
  let t = Netshm.manage ?pagesize ?init lm sd in
  let key_only_manager =
    left_pair_manager key_manager in
  let key_val_manager =
    pair_manager key_manager val_manager in
  { table = t;
    key_only_manager = key_only_manager;
    key_val_manager = key_val_manager;
    key_hash_fn = key_manager.hash_fn;
  }


let shm_table t = t.table


let add t k v =
  let h = t.key_hash_fn k in
  let kv_array = t.key_val_manager.to_int32_array (k,v) in
  Netshm.add t.table h kv_array
;;


let find t k =
  let decode_key =
    match t.key_only_manager.of_int32_array_prefix with
      | None -> assert false
      | Some f -> f in
  let h = t.key_hash_fn k in
  let data = ref [] in
  let v_opt = ref None in
  read_blocks t.table h
    (fun data_frag_opt ->
       match data_frag_opt with
         | Some data_frag ->
             data := data_frag :: !data;
	     ( match decode_key !data with
		 | None -> ()
		 | Some k' ->
		     if k <> k' then ( data := []; raise Netshm.Next )
	     )
         | None ->
             (* Important: we must decode the data while the binding is
              * read-locked!
              *)
	     let (k',v) = t.key_val_manager.of_int32_array !data in
	     assert (k = k');
	     v_opt := Some v;
	     raise Netshm.Break
    );
  ( match !v_opt with
      | None -> raise Not_found
      | Some v -> v
  )
;;


let find_all t k =
  let decode_key =
    match t.key_only_manager.of_int32_array_prefix with
      | None -> assert false
      | Some f -> f in
  let h = t.key_hash_fn k in
  let data = ref [] in
  let l = ref [] in
  read_blocks t.table h
    (fun data_frag_opt ->
       match data_frag_opt with
         | Some data_frag ->
             data := data_frag :: !data;
	     ( match decode_key !data with
		 | None -> ()
		 | Some k' ->
		     if k <> k' then ( data := []; raise Netshm.Next )
	     )
         | None ->
             (* Important: we must decode the data while the binding is
              * read-locked!
              *)
	     let (k',v) = t.key_val_manager.of_int32_array !data in
	     assert (k = k');
	     l := v :: !l;
	     data := []
    );
  List.rev !l
;;


let mem t k =
  let decode_key =
    match t.key_only_manager.of_int32_array_prefix with
      | None -> assert false
      | Some f -> f in
  let h = t.key_hash_fn k in
  let data = ref [] in
  let is_mem = ref false in
  read_blocks t.table h
    (fun data_frag_opt ->
       match data_frag_opt with
         | Some data_frag ->
             data := data_frag :: !data;
	     ( match decode_key !data with
		 | None -> ()
		 | Some k' ->
		     if k <> k' then ( data := []; raise Netshm.Next )
	     )
         | None ->
	     is_mem := true;
	     raise Netshm.Break
    );
  !is_mem
;;


let remove t k =
  let decode_key =
    match t.key_only_manager.of_int32_array_prefix with
      | None -> assert false
      | Some f -> f in
  let h = t.key_hash_fn k in
  let data = ref [] in
  let can_stop = ref false in
  write_blocks t.table [`Remove_binding] h
    (fun data_frag_opt ->
       if !can_stop then raise Break;
       match data_frag_opt with
         | Some data_frag ->
             data := data_frag :: !data;
	     ( match decode_key !data with
		 | None -> 
		     `Nop
		 | Some k' ->
		     if k <> k' then ( data := []; raise Netshm.Next );
		     can_stop := true;
		     `Remove_binding
	     )
         | None ->
	     assert false
    )
;;


let replace t k v =
  Netshm.group t.table
    (fun () ->
       remove t k;
       add t k v
    ) 
    ()


let iter f t =
  Netshm.iter
    (fun _ data ->
       let (k,v) = t.key_val_manager.of_int32_array [ data ] in
       f k v
    )
    t.table
;;


let iter_keys f t =
  let decode_key =
    match t.key_only_manager.of_int32_array_prefix with
      | None -> assert false
      | Some f -> f in
  let last_key = ref None in
  let data = ref [] in
  Netshm.iter_keys
    (fun shm_key ->
       Netshm.read_blocks
	 t.table
	 shm_key
	 (fun data_frag_opt ->
	    match data_frag_opt with
              | Some data_frag ->
		  data := data_frag :: !data;
		  ( match decode_key !data with
		      | None -> ()
		      | Some k as sk ->
			  if sk <> !last_key then
			    f k;
			  last_key := sk;
			  data := [];
			  raise Netshm.Next
		  )
              | None ->
		  ()
	 );
    )
    t.table
;;


let fold f t x0 =
  Netshm.fold
    (fun _ data acc ->
       let (k,v) = t.key_val_manager.of_int32_array [ data ] in
       f k v acc)
    t.table
    x0
;;


let length t = Netshm.length t.table
