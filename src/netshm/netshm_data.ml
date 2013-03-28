(* $Id: netshm_data.ml 1716 2012-02-20 16:59:27Z gerd $ *)

type 'a data_manager = 
    { to_int32_array : 'a -> Netshm.int32_array;
      of_int32_array : Netshm.int32_array list -> 'a;
      of_int32_array_prefix : (Netshm.int32_array list -> 'a option) option;
      hash_fn : 'a -> int32
    }

let int32_manager =
  { to_int32_array =
      (fun n -> 
	 let v = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
	 v.{ 0 } <- n;
	 v);
    of_int32_array =
      (fun l ->
	 match l with
	   | [ v ] ->
	       if Bigarray.Array1.dim v <> 1 then
		 raise(Netshm.Corrupt_file "Netshm_data.int32_manager: Cannot decode");
	       v.{ 0 }
	   | _ -> 
	       raise(Netshm.Corrupt_file "Netshm_data.int32_manager: Cannot decode")
      );
    of_int32_array_prefix = None;
    hash_fn = (fun n -> n)
  }


let int64_manager =
  { to_int32_array =
      (fun n -> 
	 let v = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 2 in
	 v.{ 0 } <- Int64.to_int32 (Int64.shift_right_logical n 32);
	 v.{ 1 } <- Int64.to_int32 n;
	 v);
    of_int32_array =
      (fun l ->
	 match l with
	   | [ v ] ->
	       if Bigarray.Array1.dim v <> 2 then
		 raise(Netshm.Corrupt_file "Netshm_data.int64_manager: Cannot decode");
	       Int64.logor
		 (Int64.shift_left (Int64.of_int32 v.{ 0 }) 32)
		 (Int64.logand (Int64.of_int32 v.{ 1 }) 0xffff_ffffL)
	   | [ v; u ] ->  (* Note: reverse order! *)
	       if Bigarray.Array1.dim u <> 1 || Bigarray.Array1.dim v <> 1 then
		 raise(Netshm.Corrupt_file "Netshm_data.int64_manager: Cannot decode");
	       Int64.logor
		 (Int64.shift_left (Int64.of_int32 u.{ 0 }) 32)
		 (Int64.logand (Int64.of_int32 v.{ 0 }) 0xffff_ffffL)
	   | _ -> 
	       raise(Netshm.Corrupt_file "Netshm_data.int64_manager: Cannot decode")
      );
    of_int32_array_prefix = None;
    hash_fn = Int64.to_int32
  }


let sel_32_or_64_manager to32 of32 to64 of64 =
  match Sys.word_size with
    | 32 ->
	{ to_int32_array =
	    (fun n -> 
	       int32_manager.to_int32_array (to32 n));
	  of_int32_array =
	    (fun l -> 
	       of32 (int32_manager.of_int32_array l));
	  of_int32_array_prefix = None;
	  hash_fn = to32
	}
    | 64 ->
	{ to_int32_array =
	    (fun n -> 
	       int64_manager.to_int32_array (to64 n));
	  of_int32_array =
	    (fun l -> 
	       of64 (int64_manager.of_int32_array l));
	  of_int32_array_prefix = None;
	  hash_fn = (fun n -> Int64.to_int32 (to64 n))
	}
    | _ ->
	assert false

let nativeint_manager =
  sel_32_or_64_manager
    Nativeint.to_int32
    Nativeint.of_int32
    Int64.of_nativeint
    Int64.to_nativeint

let int_manager =
  sel_32_or_64_manager
    Int32.of_int
    Int32.to_int
    Int64.of_int
    Int64.to_int

let int32_array_manager =
  { to_int32_array = (fun v -> v);
    of_int32_array =
      (fun l ->
	 let size =
	   List.fold_left
	     (fun acc v -> acc + Bigarray.Array1.dim v) 0 l in
	 let v_total =
	   Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout size in
	 let size' =
	   List.fold_left
	     (fun start' v ->
		let len = Bigarray.Array1.dim v in
		let start = start' - len in
		Bigarray.Array1.blit
		  v
		  (Bigarray.Array1.sub v_total start len);
		start)
	     size
	     l in
	 assert(size' = 0);
	 v_total);
    of_int32_array_prefix = None;
    hash_fn = (fun v -> Int32.of_int (Hashtbl.hash v))
  }

let string_manager =
  { to_int32_array =
      (fun s ->
	 let s_len = String.length s in
	 if Int32.of_int s_len > Int32.max_int then 
	   failwith "Netshm_data: string too long";
	 let size = if s_len = 0 then 1 else (s_len - 1) / 4 + 2 in
	 let v =
	   Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout size in
	 v.{ 0 } <- Int32.of_int s_len;

	 let m =
	   Netsys_mem.memory_of_bigarray_1 v in
	 Netsys_mem.blit_string_to_memory s 0 m 4 s_len;
	 v
      );

    of_int32_array =
      (fun l ->
	 if l = [] then
	   raise(Netshm.Corrupt_file
		   "Netshm_data.string_manager: Cannot decode");
	 let size = ref 0 in
	 let v_last = ref None in
	 List.iter
	   (fun v -> 
	      size := !size + Bigarray.Array1.dim v;
	      v_last := Some v
	   )
	   l;
	 let s_len =
	   ( match !v_last with
	       | None -> assert false
	       | Some v ->
		   if Bigarray.Array1.dim v = 0 then
		     raise(Netshm.Corrupt_file 
			     "Netshm_data.string_manager: Cannot decode");
		   Int32.to_int v.{ 0 }
	   ) in
	 let s_len_4 = s_len mod 4 in
	 let size' = if s_len = 0 then 1 else (s_len - 1) / 4 + 2 in
	 if !size <> size' then
	   raise(Netshm.Corrupt_file 
		   "Netshm_data.string_manager: Cannot decode");
	 let pos = ref !size in
	 let s = String.create s_len in
	 let s_pos = ref s_len in
	 List.iter
	   (fun v ->
	      let l = Bigarray.Array1.dim v in
	      let has_last_content_word = !pos = !size && s_len > 0 in
	      pos := !pos - l;
	      let has_length_word = !pos = 0 in

	      let m = Netsys_mem.memory_of_bigarray_1 v in
	      let bsize =
		4 * l -
		  (if has_last_content_word && s_len_4 <> 0 then
		     4 - s_len_4
		   else
		     0
		  ) -
		  (if has_length_word then
		     4
		   else
		     0
		  ) in
	      s_pos := !s_pos - bsize;
	      Netsys_mem.blit_memory_to_string
		m
		(if has_length_word then 4 else 0)
		s
		!s_pos
		bsize;
	   )
	   l;
	 assert (!pos = 0);
	 assert (!s_pos = 0);
	 s
      );
 
    of_int32_array_prefix = None;
    hash_fn = (fun v -> Int32.of_int (Hashtbl.hash v))
  }
    

let pair_manager x_manager y_manager =
  { to_int32_array =
      (fun (x,y) ->
	 let vx = x_manager.to_int32_array x in
	 let vy = y_manager.to_int32_array y in
	 let lx = Bigarray.Array1.dim vx in
	 let ly = Bigarray.Array1.dim vy in
	 let size = lx + ly + 1 in
	 let v = 
	   Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout size in
	 v.{ 0 } <- Int32.of_int lx;
	 Bigarray.Array1.blit
	   vx
	   (Bigarray.Array1.sub v 1 lx);
	 Bigarray.Array1.blit
	   vy
	   (Bigarray.Array1.sub v (lx+1) ly);
	 v
      );

    of_int32_array =
      (fun l ->
	 if l = [] then
	   raise(Netshm.Corrupt_file "Netshm_data.pair_manager: Cannot decode");
	 let l' = List.rev l in
	 let vl = List.hd l' in
	 assert(Bigarray.Array1.dim vl > 0);
	 let lx = Int32.to_int vl.{0} in
	 
	 let l1 = ref [] in
	 let l2 = ref [] in
	 let c1 = ref 0 in
	 List.iter
	   (fun v ->
	      let l = Bigarray.Array1.dim v in
	      let p0 = if !c1 = 0 then 1 else 0 in
	      let p1 =
		if !c1 < lx then (
		  let r = min (l - p0) (lx - !c1) in
		  if p0 = 0 && r = l then
		    l1 := v :: !l1
		  else
		    l1 := (Bigarray.Array1.sub v p0 r) :: !l1;
		  c1 := !c1 + r;
		  p0+r
		) else p0 in
	      if !c1 = lx then (
		let r = l - p1 in
		if p1 = 0 then
		  l2 := v :: !l2
		else
		  l2 := (Bigarray.Array1.sub v p1 r) :: !l2;
	      )
	   )
	   l';
	 let x = x_manager.of_int32_array !l1 in
	 let y = y_manager.of_int32_array !l2 in
	 (x,y)
      );

    of_int32_array_prefix = None;
    hash_fn = (fun v -> Int32.of_int (Hashtbl.hash v))
  }


let left_pair_manager x_manager =
  { to_int32_array =
      (fun x -> 
	 failwith "Netshm_data.left_pair_manager: Encoding not supported"
      );

    of_int32_array =
      (fun l ->
	 if l = [] then
	   raise(Netshm.Corrupt_file "Netshm_data.left_pair_manager: Cannot decode");
	 let l' = List.rev l in
	 let vl = List.hd l' in
	 let lx = Int32.to_int vl.{0} in
	 
	 let l1 = ref [] in
	 let c1 = ref 0 in
	 List.iter
	   (fun v ->
	      let l = Bigarray.Array1.dim v in
	      let p0 = if !c1 = 0 then 1 else 0 in
	      if !c1 < lx then (
		let r = min (l - p0) (lx - !c1) in
		if p0 = 0 && r = l then
		  l1 := v :: !l1
		else
		  l1 := (Bigarray.Array1.sub v p0 r) :: !l1;
		c1 := !c1 + r;
	      );
	   )
	   l';
	 x_manager.of_int32_array !l1
      );

    of_int32_array_prefix = 
      Some 
	(fun l ->
	   let size = ref 0 in
	   let last = ref None in
	   List.iter
	     (fun v ->
		size := !size + Bigarray.Array1.dim v;
		last := Some v
	     )
	     l;
	   ( match !last with
	       | None ->
		   None
	       | Some last ->
		   let lx = Int32.to_int last.{0} in
		   if !size >= lx + 1 then (
		     let l1 = ref [] in
		     let c1 = ref 0 in
		     List.iter
		       (fun v ->
			  let l = Bigarray.Array1.dim v in
			  let p0 = if !c1 = 0 then 1 else 0 in
			  if !c1 < lx then (
			    let r = min (l - p0) (lx - !c1) in
			    if p0 = 0 && r = l then
			      l1 := v :: !l1
			    else
			      l1 := (Bigarray.Array1.sub v p0 r) :: !l1;
			    c1 := !c1 + r;
			  );
		       )
		       (List.rev l);
		     Some(x_manager.of_int32_array !l1)
		   )
		   else
		     None
	   )
	);

    hash_fn = (fun v -> Int32.of_int (Hashtbl.hash v))
  }


let option_manager x_manager =
  { to_int32_array =
      (fun x_opt ->
	 match x_opt with
	   | Some x ->
	       let vx = x_manager.to_int32_array x in
	       let lx = Bigarray.Array1.dim vx in
	       let v = 
		 Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (lx+1) in
	       v.{ 0 } <- 1l;
	       Bigarray.Array1.blit
		 vx
		 (Bigarray.Array1.sub v 1 lx);
	       v
	   | None ->
	       let v = 
		 Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
	       v.{ 0 } <- 0l;
	       v
      );

    of_int32_array =
      (fun l ->
	 if l = [] then
	   raise(Netshm.Corrupt_file "Netshm_data.option_manager: Cannot decode");
	 let l' = List.rev l in
	 let vl = List.hd l' in
	 let x0 = vl.{0} in
	 
	 if x0 = 0l then (
	   if Bigarray.Array1.dim vl <> 1 || List.length l <> 1 then
	     raise(Netshm.Corrupt_file "Netshm_data.option_manager: Cannot decode");
	   None
	 )
	 else
	   if x0 = 1l then (
	     let l1 = ref [] in
	     let is_first = ref true in
	     List.iter
	       (fun v ->
		  if !is_first then (
		    let l = Bigarray.Array1.dim v in
		    l1 := (Bigarray.Array1.sub v 1 (l-1)) :: !l1
		  )
		  else
		    l1 := v :: !l1;

		  is_first := false
	       )
	       l';
	     Some(x_manager.of_int32_array !l1)
	   )
	   else
	      raise(Netshm.Corrupt_file "Netshm_data.option_manager: Cannot decode")
      );

    of_int32_array_prefix = None;
    hash_fn = (fun v -> Int32.of_int (Hashtbl.hash v))
  }

