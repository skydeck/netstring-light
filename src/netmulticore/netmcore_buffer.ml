(* $Id: netmcore_buffer.ml 1773 2012-03-30 20:06:28Z gerd $ *)

open Netmcore_heap
open Printf

type 'h t =
    { bsize : int;

      mutable blocks : string array;
      (* Allocated strings have length [bsize]. Unused strings are just "" *)

      mutable null_index : int;
      (* The index of the first byte in the first block *)

      mutable start_index : int;
      (* The start index *)

      mutable length : int;
      (* The length *)

      mutable add_lock : Netmcore_mutex.mutex;
      (* ensures that only one addition can be done at a time *)

      header : 'h
    }

type 'h buffer = 'h t heap

type 'h buffer_descr = 'h t descr

let descr_of_buffer = descr_of_heap
let buffer_of_descr = heap_of_descr

let create pool bsize h =
  let bsize =
    ((bsize - 1) / Netsys_mem.pagesize + 1) * Netsys_mem.pagesize in
  let sb =
    create_heap 
      pool
      (bsize + 4096) 
      { bsize = bsize;
	blocks = [| "" |];
	null_index = 0;
	start_index = 0;
	length = 0;
	add_lock = Netmcore_mutex.dummy();
	header = h
      } in
  modify sb
    (fun mut ->
       (root sb).add_lock <- Netmcore_mutex.create mut `Normal
    );
  sb

let destroy sb =
  let b = root sb in
  Netmcore_mutex.destroy b.add_lock;
  Netmcore_heap.destroy sb


let start sb =
  (root sb).start_index

let length sb =
  (root sb).length

let ( ++ ) a b =
  (* addition modulo maxint+1 *)
  let c = a+b in
  if c<0 then c+max_int+1 else c

let ( -- ) a b =
  (* subtraction modulo maxint+1 *)
  let c = a-b in
  if c<0 then c+max_int+1 else c


let check b pos n =
  let end_index = b.start_index ++ b.length in
  if b.start_index <= end_index then (
    if pos < b.start_index || pos > end_index - n then (
(*
      eprintf "Bad index pos=%d n=%d start_index=%d end_index=%d length=%d\n%!"
        pos n b.start_index end_index b.length;
 *)
      invalid_arg "Netmcore_buffer: bad index[1]"
    )
  )
  else
    let epos = pos ++ n in
    if (pos > end_index && pos < b.start_index) ||
       (epos > end_index && epos < b.start_index) 
    then (
(*
      eprintf "Bad index pos=%d epos=%d n=%d start_index=%d end_index=%d length=%d\n%!"
        pos epos n b.start_index end_index b.length;
 *)
      invalid_arg "Netmcore_buffer: bad index[2]"
    )

let blit_to sb sb_pos_opt get_n f =
  let bsize = (root sb).bsize in
  with_value_3 sb
    (fun () ->
       let b = root sb in
       let n = get_n b.length in
       let sb_pos =
	 match sb_pos_opt with
	   | None -> b.start_index
	   | Some sb_pos -> sb_pos in
       check b sb_pos n;
       let buf_pos = sb_pos -- b.null_index in
       (b.blocks, buf_pos, n)
    )
    (fun (blocks, buf_pos, n) ->
       if n > 0 then (
	 let k1 = buf_pos / bsize in
	 let k2 = (buf_pos + n - 1) / bsize in
	 for k = k1 to k2 do
	   let p_start =
	     if k=k1 then buf_pos mod bsize else 0 in
	   let p_end =
	     if k=k2 then(buf_pos + n - 1) mod bsize else bsize-1 in
	   let p_num = p_end-p_start+1 in
	   f blocks.(k) p_start p_num
	 done
       )
    )

let blit_to_string sb sb_pos str str_pos n =
  if n < 0 then
    invalid_arg "Netmcore_buffer: bad length";
  let str_len = String.length str in
  if str_pos < 0 || str_pos > str_len - n then
    invalid_arg "Netmcore_buffer: bad index";
  let q = ref str_pos in
  blit_to sb (Some sb_pos) (fun _ -> n)
    (fun block block_pos num ->
       String.blit block block_pos str !q num;
       q := !q + num
    )

let blit_to_memory sb sb_pos mem mem_pos n =
  if n < 0 then
    invalid_arg "Netmcore_buffer: bad length";
  let mem_len = Bigarray.Array1.dim mem in
  if mem_pos < 0 || mem_pos > mem_len - n then
    invalid_arg "Netmcore_buffer: bad index";
  let q = ref mem_pos in
  blit_to sb (Some sb_pos) (fun _ -> n)
    (fun block block_pos num ->
       Netsys_mem.blit_string_to_memory block block_pos mem !q num;
       q := !q + num
    )

let sub sb sb_pos n =
  if n < 0 then
    invalid_arg "Netmcore_buffer: bad length";
  let q = ref 0 in
  let s = ref "" in
  let s_alloc = ref false in
  blit_to sb (Some sb_pos)
    (fun _ -> n)
    (fun block block_pos num ->
       if not !s_alloc then ( s := String.create n; s_alloc := true );
       String.blit block block_pos !s !q num;
       q := !q + num
    );
  !s

let contents sb =
  let q = ref 0 in
  let s = ref "" in
  let s_alloc = ref false in
  let n = ref 0 in
  blit_to sb None
    (fun length -> n := length; length)
    (fun block block_pos num ->
       if not !s_alloc then ( s := String.create !n; s_alloc := true );
       String.blit block block_pos !s !q num;
       q := !q + num
    );
  !s
  
let access sb sb_pos f =
  let bsize = (root sb).bsize in
  with_value_3 sb
    (fun () ->
       let b = root sb in
       check b sb_pos 1;
       let buf_pos = sb_pos -- b.null_index in
       (b.blocks, buf_pos, b.length-(sb_pos -- b.start_index))
    )
    (fun (blocks, buf_pos, remaining) ->
       let k = buf_pos / bsize in
       let p_start = buf_pos mod bsize in
       let p_end = min (p_start + remaining) bsize in
       f blocks.(k) p_start (p_end - p_start)
    )

let delete_hd sb n =
  if n < 0 then
    invalid_arg "Netmcore_buffer: bad length";
  with_value sb
    (fun () ->
       let b = root sb in
       check b b.start_index n;
       b.start_index <- b.start_index ++ n;
       b.length <- b.length - n;
    )
    (fun () -> ())

let add_to sb n f =
  Netmcore_mutex.lock (root sb).add_lock;
  let debug = ref 0 in
  try
    (* First check everything, and if necessary, resize [blocks]: *)
    let length = ref 0 in
    let start_index = ref 0 in
    modify sb
      (fun mut ->
	 let b = root sb in
	 let buf_pos = b.start_index -- b.null_index in
         assert(buf_pos >= 0);
	 let end_buf_pos = buf_pos + b.length in
         assert(end_buf_pos >= buf_pos);
	 let n_blocks = Array.length b.blocks in
         assert(n_blocks > 0);
	 let max_buf_pos = (n_blocks * b.bsize) - 1 in
	 let need_resize = end_buf_pos - 1 > max_buf_pos - n in

	 let new_end_buf_pos = end_buf_pos + n in
	 if new_end_buf_pos < end_buf_pos then (* wrap-around *)
	   failwith "Netmcore_buffer: too large";

	 if need_resize then (
           debug := 1;
	   let n_drop = buf_pos / b.bsize in
	   (* Can drop this number of buffers at the beginning *)
	   
	   let n_blocks_1 =
	     (new_end_buf_pos-1) / b.bsize + 1 - n_drop in
	   let n_blocks_2 =
	     max n_blocks n_blocks_1 in  (* never shrink *)
	   let n_blocks_3 =
	     if n_blocks_2 > n_blocks then
               let max_blocks = max_int / b.bsize in
	       max (min (2*n_blocks) max_blocks) n_blocks_2
                 (* double if possible *)
	     else
	       n_blocks_2 in
	   
	   let n_keep = n_blocks - n_drop in

           assert(n_blocks_3 > 0);
           assert(n_blocks_3 >= n_blocks_1);
           debug := 2;
	   let blocks = add_uniform_array mut n_blocks_3 "" in
           debug := 3;
	   Array.blit b.blocks n_drop blocks 0 n_keep;
           debug := 4;
	   pin mut blocks;
           debug := 5;

	   let orig =
	     if b.blocks.(0) <> "" then b.blocks.(0) else
	       String.create b.bsize in
           debug := 6;

	   for k = 0 to n_blocks_1 - 1 do
	     if String.length blocks.(k) = 0 then
	       blocks.(k) <- add mut orig
	   done;
           debug := 7;

	   b.blocks <- blocks;
	   b.null_index <- b.null_index ++ n_drop * b.bsize;
	 )
	 else (
	   (* Maybe we have to allocate strings *)
           debug := 8;
	   let n_blocks_1 =
	     (new_end_buf_pos-1) / b.bsize + 1 in
           assert(n_blocks >= n_blocks_1);

	   let orig =
	     if b.blocks.(0) <> "" then b.blocks.(0) else
	       String.create b.bsize in

           debug := 9;
	   for k = 0 to n_blocks_1 - 1 do
	     if String.length b.blocks.(k) = 0 then
	       b.blocks.(k) <- add mut orig
	   done;
           debug := 10;
	 );

	 (* Once we leave [modify] it is possible that [delete_hd]
	    runs. So keep our view here.
	  *)
	 length := b.length;
	 start_index := b.start_index;
      );
    debug := 12;

    (* Now copy the data. This does not change anything in the variables,
       and hence we can do it without heap lock. (We still keep the add_lock
       preventing concurrent adds, though.)
     *)

    let b = root sb in
    debug := 13;
    let buf_pos = !start_index -- b.null_index in
    let end_buf_pos = buf_pos + !length in
    let new_end_buf_pos = end_buf_pos + n in

    let k1 = end_buf_pos / b.bsize in
    let k2 = (new_end_buf_pos - 1) / b.bsize in
    assert(k1 >= 0);
    assert(k2 < Array.length b.blocks);
    for k = k1 to k2 do
      debug := 130;
      let p_start =
	if k=k1 then end_buf_pos mod b.bsize else 0 in
      let p_end =
	if k=k2 then(new_end_buf_pos - 1) mod b.bsize else b.bsize-1 in
      let p_num = p_end-p_start+1 in
      let block = b.blocks.(k) in
      debug := 131;
      f block p_start p_num
    done;
    debug := 14;

    (* Make the data visible *)
    modify sb
      (fun mut ->
	 (* In general, b.length <> !length is now possible *)
	 b.length <- b.length + n
      );
    debug := 15;
    Netmcore_mutex.unlock (root sb).add_lock;
  with
    | error ->
        let bt = Printexc.get_backtrace() in
        Netlog.logf `Crit
          "Netmcore_buffer.add_to: %s, debug code: %d backtrace: %s"
          (Netexn.to_string error) !debug bt;
	Netmcore_mutex.unlock (root sb).add_lock;
	raise error



let add_sub_string sb str str_pos n =
  if n < 0 then
    invalid_arg "Netmcore_buffer: bad length";
  let str_len = String.length str in
  if str_pos < 0 || str_pos > str_len - n then
    invalid_arg "Netmcore_buffer: bad index";
  let q = ref str_pos in
  add_to sb n
    (fun s p n ->
       String.blit str !q s p n;
       q := !q + n
    )


let add_string sb str =
  add_sub_string sb str 0 (String.length str)


let add_sub_memory sb mem mem_pos n =
  if n < 0 then
    invalid_arg "Netmcore_buffer: bad length";
  let mem_len = Bigarray.Array1.dim mem in
  if mem_pos < 0 || mem_pos > mem_len - n then
    invalid_arg "Netmcore_buffer: bad index";
  let q = ref mem_pos in
  add_to sb n
    (fun s p n ->
       Netsys_mem.blit_memory_to_string mem !q s p n;
       q := !q + n
    )


let clear sb =
  Netmcore_mutex.lock (root sb).add_lock;
  try
    modify sb
      (fun mut ->
	 let b = root sb in
	 b.blocks <- add mut [| "" |];
	 b.null_index <- 0;
	 b.start_index <- 0;
	 b.length <- 0;
      );
    Netmcore_mutex.unlock (root sb).add_lock;
  with
    | error ->
	Netmcore_mutex.unlock (root sb).add_lock;
	raise error


let header sb =
  (root sb).header

let heap sb =
  Obj.magic sb
