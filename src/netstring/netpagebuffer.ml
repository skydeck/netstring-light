(* $Id: netpagebuffer.ml 1651 2011-08-03 16:38:17Z gerd $ *)

type t =
    { pgsize : int;
      mutable pages : Netsys_mem.memory array;
      (* Used pages have size pgsize. Unused pages are set to a dummy page *)
      mutable n_pages : int;
      (* The pages 0 .. n_pages-1 are used. n_pages >= 1 (exception below) *)
      mutable free_page : (unit -> unit) array;
      (* For each element of [pages] a function for freeing the page
	 (quicker than by GC)
       *)
      mutable start_index : int;
      (* start_index: The first byte in the first page has this index *)
      mutable stop_index : int;
      (* stop_index: The first free byte in the last page *)
      mutable pool : Netsys_mem.memory_pool
      (* Pages that can be reclaimed *)
    }

(* Except for one case we have this invariant:

   invariant: there is at least one free byte on the last page 

   The exception is that we also tolerate n_pages=0, which is treated
   in the same way as an empty single page. When needed this empty
   single page is allocated to enforce the invariant (fix_invariant).
 *)


let dummy_page =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let length buf =
  if buf.n_pages = 0 then
    0
  else
    buf.n_pages * buf.pgsize - buf.start_index - (buf.pgsize - buf.stop_index)

let alloc_pages buf n =
  let need_resize =
    n + buf.n_pages > Array.length buf.pages in
  if need_resize then (
    let new_size =
      max 
	(min (2 * Array.length buf.pages) Sys.max_array_length)
	(buf.n_pages + n) in
    if new_size > Sys.max_array_length then
      failwith "Netpagebuffer: too large";
    let pages' =
      Array.make new_size dummy_page in
    Array.blit
      buf.pages 0 pages' 0 buf.n_pages;
    let free_page' =
      Array.make new_size (fun () -> ()) in
    Array.blit
      buf.free_page 0 free_page' 0 buf.n_pages;
    buf.pages <- pages';
    buf.free_page <- free_page'
  );
  let n_pages' = buf.n_pages + n in
  for k = buf.n_pages to n_pages'-1 do
    let p, f = Netsys_mem.pool_alloc_memory2 buf.pool in
    buf.pages.(k) <- p;
    buf.free_page.(k) <- f
  done;
  buf.n_pages <- n_pages'


let create pgsize =
  let sys_pgsize = Netsys_mem.pagesize in
  if pgsize mod sys_pgsize <> 0 then
    failwith "Netpagebuffer.create: invalid pagesize";
  let pool = 
    if pgsize = Netsys_mem.default_block_size then
      Netsys_mem.default_pool
    else
      if pgsize = Netsys_mem.small_block_size then
	Netsys_mem.small_pool
      else
	Netsys_mem.create_pool pgsize in
  { pgsize = pgsize;
    pages = [| dummy_page |];
    n_pages = 0;
    free_page = [| fun () -> () |];
    start_index = 0;
    stop_index = 0;
    pool = pool;
  }


let fix_invariant buf =
  if buf.n_pages = 0 then (
    alloc_pages buf 1;
    buf.start_index <- 0;
    buf.stop_index <- 0;
  )


let blit_to_string buf pos s s_pos len =
  let buf_len = length buf in
  let s_len = String.length s in
  if pos < 0 || s_pos < 0 || len < 0 || len > buf_len - pos || 
     len > s_len - s_pos then
       invalid_arg "Netpagebuffer.blit_to_string";
  let abs_pos1 = pos + buf.start_index in
  let pg1 = abs_pos1 / buf.pgsize in
  let idx1 = abs_pos1 mod buf.pgsize in
(*
  let abs_pos2 = abs_pos1 + len in
  let pg2 = abs_pos2 / buf.pgsize in
  let idx2 = abs_pos2 mod buf.pgsize in
 *)

  let cur_pg = ref pg1 in
  let cur_s_pos = ref s_pos in
  let rem_len = ref len in
  while !rem_len > 0 do
    let l = 
      min
	(if !cur_pg = pg1 then buf.pgsize - idx1 else buf.pgsize)
	!rem_len in
    Netsys_mem.blit_memory_to_string
      buf.pages.( !cur_pg )
      (if !cur_pg = pg1 then idx1 else 0)
      s
      !cur_s_pos
      l;
    cur_s_pos := !cur_s_pos + l;
    rem_len := !rem_len - l;
    incr cur_pg;
  done
  

let blit_to_memory buf pos m m_pos len =
  let buf_len = length buf in
  let m_len = Bigarray.Array1.dim m in
  if pos < 0 || m_pos < 0 || len < 0 || len > buf_len - pos || 
     len > m_len - m_pos then
       invalid_arg "Netpagebuffer.blit_to_memory";
  let abs_pos1 = pos + buf.start_index in
  let pg1 = abs_pos1 / buf.pgsize in
  let idx1 = abs_pos1 mod buf.pgsize in
(*
  let abs_pos2 = abs_pos1 + len in
  let pg2 = abs_pos2 / buf.pgsize in
  let idx2 = abs_pos2 mod buf.pgsize in
 *)  

  let cur_pg = ref pg1 in
  let cur_m_pos = ref m_pos in
  let rem_len = ref len in
  while !rem_len > 0 do
    let l = 
      min
	(if !cur_pg = pg1 then buf.pgsize - idx1 else buf.pgsize)
	!rem_len in
    Bigarray.Array1.blit
      (Bigarray.Array1.sub
	 buf.pages.( !cur_pg )
	 (if !cur_pg = pg1 then idx1 else 0)
	 l)
      (Bigarray.Array1.sub
	 m
	 !cur_m_pos
	 l);
    cur_m_pos := !cur_m_pos + l;
    rem_len := !rem_len - l;
    incr cur_pg;
  done
  

let sub buf pos len =
  let buf_len = length buf in
  if pos < 0 || len < 0 || len > buf_len - pos then
    invalid_arg "Netpagebuffer.sub";
  let s = String.create len in
  blit_to_string buf pos s 0 len;
  s


let contents buf =
  sub buf 0 (length buf)


let add_sub_string buf s pos len =
  let s_len = String.length s in
  if pos < 0 || len < 0 || len > s_len - pos then
    invalid_arg "Netpagebuffer.add_sub_string";
  fix_invariant buf;
  let len_for_new_pages =
    len - (buf.pgsize - buf.stop_index) in
  let new_pages =
    if len_for_new_pages >= 0 then
      len_for_new_pages / buf.pgsize + 1
    else
      0 in
  let old_last_page = buf.n_pages - 1 in
  alloc_pages buf new_pages;
  let len_old_last_page = min len (buf.pgsize - buf.stop_index) in
  Netsys_mem.blit_string_to_memory
    s
    pos
    buf.pages.(old_last_page)
    buf.stop_index
    len_old_last_page;
  buf.stop_index <- buf.stop_index + len_old_last_page;
  if buf.stop_index = buf.pgsize then buf.stop_index <- 0;
  let len_remaining = ref (len - len_old_last_page) in
  let cur_pos = ref (pos + len_old_last_page) in
  let cur_pg = ref(old_last_page + 1) in
  while !len_remaining > 0 do
    let l = min !len_remaining buf.pgsize in
    Netsys_mem.blit_string_to_memory
      s
      !cur_pos
      buf.pages.(!cur_pg)
      0
      l;
    cur_pos := !cur_pos + l;
    len_remaining := !len_remaining - l;
    incr cur_pg;
    if !len_remaining = 0 then (
      buf.stop_index <- l;
      if l = buf.pgsize then buf.stop_index <- 0
    )
  done
  

let add_string buf s =
  add_sub_string buf s 0 (String.length s)


let add_sub_memory buf m pos len =
  (* very similar to add_sub_string. For performance reasons this is a
     copy of the above algorithm
   *)
  let m_len = Bigarray.Array1.dim m in
  if pos < 0 || len < 0 || len > m_len - pos then
    invalid_arg "Netpagebuffer.add_sub_memory";
  fix_invariant buf;
  let len_for_new_pages =
    len - (buf.pgsize - buf.stop_index) in
  let new_pages =
    if len_for_new_pages >= 0 then
      len_for_new_pages / buf.pgsize + 1
    else
      0 in
  let old_last_page = buf.n_pages - 1 in
  alloc_pages buf new_pages;
  let len_old_last_page = min len (buf.pgsize - buf.stop_index) in
  Bigarray.Array1.blit
    (Bigarray.Array1.sub 
       m pos len_old_last_page)
    (Bigarray.Array1.sub
       buf.pages.(old_last_page) buf.stop_index len_old_last_page);
  buf.stop_index <- buf.stop_index + len_old_last_page;
  if buf.stop_index = buf.pgsize then buf.stop_index <- 0;
  let len_remaining = ref (len - len_old_last_page) in
  let cur_pos = ref (pos + len_old_last_page) in
  let cur_pg = ref(old_last_page + 1) in
  while !len_remaining > 0 do
    let l = min !len_remaining buf.pgsize in
    Bigarray.Array1.blit
      (Bigarray.Array1.sub m !cur_pos l)
      (Bigarray.Array1.sub buf.pages.(!cur_pg) 0 l);
    cur_pos := !cur_pos + l;
    len_remaining := !len_remaining - l;
    incr cur_pg;
    if !len_remaining = 0 then (
      buf.stop_index <- l;
      if l = buf.pgsize then buf.stop_index <- 0
    )
  done


let page_for_additions buf =
  fix_invariant buf;
  let last_page = buf.n_pages - 1 in
  ( buf.pages.(last_page), buf.stop_index, buf.pgsize - buf.stop_index )


let advance buf n =
  fix_invariant buf;
  if n < 0 || n > buf.pgsize - buf.stop_index then
    invalid_arg "Netpagebuffer.advance";
  buf.stop_index <- buf.stop_index + n;
  if buf.stop_index = buf.pgsize then (
    alloc_pages buf 1;
    buf.stop_index <- 0;
  )


let add_inplace buf f =
  let (page, pos, len) = page_for_additions buf in
  let n = f page pos len in
  if n < 0 || n > len then
    invalid_arg "Netpagebuffer.add_inplace";
  advance buf n;
  n


let page_for_consumption buf =
  fix_invariant buf;
  let stop =
    if buf.n_pages = 1 then buf.stop_index else buf.pgsize in
  ( buf.pages.(0), buf.start_index, stop )


let delete_hd buf n =
  let blen = length buf in
  if n < 0 || n > blen then
    invalid_arg "Netpagebuffer.delete_hd";
  if n > 0 then (
    (* hence, blen > 0, and the invariant holds *)
    let l_first_page = buf.pgsize - buf.start_index in
    if n < l_first_page then
      buf.start_index <- buf.start_index + n
    else (
      let pages_to_delete =
	(n - l_first_page) / buf.pgsize + 1 in
      let new_start_index =
	(n - l_first_page) mod buf.pgsize in
      for k = 0 to pages_to_delete-1 do
	buf.free_page.(k) ()
      done;
      let m = buf.n_pages - pages_to_delete in
      Array.blit
	buf.pages pages_to_delete buf.pages 0 m;
      Array.blit
	buf.free_page pages_to_delete buf.free_page 0 m;
      buf.n_pages <- buf.n_pages - pages_to_delete;
      buf.start_index <- new_start_index;
      for k = buf.n_pages to Array.length buf.pages - 1 do
	buf.pages.(k) <- dummy_page;
	buf.free_page.(k) <- (fun () -> ())
      done
    );
    if buf.n_pages = 1 && buf.start_index = buf.stop_index then (
      buf.free_page.(0) ();
      buf.pages.(0) <- dummy_page;
      buf.free_page.(0) <- (fun () -> ());
      buf.n_pages <- 0;
      buf.start_index <- 0;
      buf.stop_index <- 0;
    )
  )


let clear buf =
  for k = 0 to buf.n_pages - 1 do
    buf.free_page.(k) ();
    buf.pages.(k) <- dummy_page;
    buf.free_page.(k) <- (fun () -> ())
  done;
  buf.n_pages <- 0;
  buf.start_index <- 0;
  buf.stop_index <- 0


exception Found of int

let index_from buf k c =
  if k < 0 || k > length buf then  (* we allow k=length *)
    invalid_arg "Netpagebuffer.index_from";

  let abs_pos1 = k + buf.start_index in
  let pg1 = abs_pos1 / buf.pgsize in
  let idx1 = abs_pos1 mod buf.pgsize in
  let pg = ref pg1 in
  let idx = ref idx1 in

  try
    while !pg < buf.n_pages do
      let page = buf.pages.( !pg ) in
      let stop_idx =
	if !pg = buf.n_pages - 1 then 
	  buf.stop_index
	else
	  buf.pgsize in
      while !idx < stop_idx && Bigarray.Array1.unsafe_get page !idx <> c do
	incr idx
      done;
      if !idx < stop_idx then (
	let pos = !pg * buf.pgsize + !idx - buf.start_index in
	raise(Found pos)
      );
      incr pg;
      idx := 0
    done;
    raise Not_found
  with
    | Found pos -> pos

