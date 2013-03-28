(* $Id: netmcore_mempool.ml 1755 2012-03-24 20:18:30Z gerd $ *)

open Printf


module Debug = struct
  let enable = ref false
  let enable_alloc = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netmcore_mempool" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netmcore_mempool" Debug.enable

let dlog_alloc = 
  Netlog.Debug.mk_dlog "Netmcore_mempool.alloc" Debug.enable_alloc
let dlogr_alloc = 
  Netlog.Debug.mk_dlogr "Netmcore_mempool.alloc" Debug.enable_alloc

let () =
  Netlog.Debug.register_module "Netmcore_mempool" Debug.enable;
  Netlog.Debug.register_module "Netmcore_mempool.alloc" Debug.enable_alloc



exception Out_of_pool_memory

(* Header:
   - The first 8 bytes are simply: MEMPOOL\n
   - Now the offset to the [header] follows (8 bytes) - 0 if not yet
     initialized. The offset is relative to the start of the pool buffer
   - Now the semaphore for locking the pool
   - Now the space for the [header] value
 *)

type header =
    { pool_size : int;
      mutable pool_free : int;
      mutable pool_free_contiguous : int;
      
      (* Freelists: for k>=1 the array element pool_fl.(k) points to the
	 beginning of the free list for blocks with exactly k free pages.
	 pool_fl.(0) points to the free list for blocks with more
	 free pages than this array is long.

	 A free block has a record free_list at the beginning, see below.
       *)
      pool_fl : int array;

      (* Binary tree of allocated blocks. The tree is represented in an
	 array with a fixed number of elements. The tree is
	 AVL-balanced. Keys of the tree are offsets, and the values contain
	 the bsize.
       *)
      mutable pool_alloc : Netmcore_util.AVL.header;

      mutable pool_start : int; (* offset of the first payload page *)
    }

(* Free list entry: Unused blocks are initialized as follows:
   - The first 8 bytes are simply: MEMFREE\n 
   - The offset to [pool_fl] (8 bytes). The offset is relative to the start 
     of the free block
   - Now space for [pool_fl]
 *)
and pool_fl =
    { mutable pool_fl_next : int;       (* 0 if end *)
      mutable pool_fl_bsize : int;      (* size of block (multiple of page size) *)
    }

let pool_fl_size = 64
  (* size of pool_fl *)

let management_factor = 4
  (* Every this number of pages another alloc_tree element is preallocated *)

let magic = "MEMPOOL\n"
  (* must have length 8 *)

let fl_magic = "MEMFREE\n"
  (* must have length 8 *)

let page_size = Netsys_mem.pagesize

let alloc_mem n =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout n

let to_page n =
  n / page_size

let prec_block mem hdr offs =
  (* If there is an allocated block preceding the block at offs, the
     preceding block with the highest address is returned as
     [Some (p_offs,p_bsize)]. Otherwise [None] is returned.
   *)
  Netmcore_util.AVL.find_pred hdr.pool_alloc offs

let check_block mem hdr offs =
  (* If there is an allocated block at offs or at a succeeding address, the
     allocated block with the lowest address is returned as
     [Some (s_offs,s_bsize)]. Otherwise [None] is returned.
   *)
  Netmcore_util.AVL.find hdr.pool_alloc offs


let assert_is_free mem hdr offs =
  (* Check that the block at offs has the magic of a free block *)
  let u = String.create(String.length fl_magic) in
  Netsys_mem.blit_memory_to_string mem offs u 0 (String.length fl_magic);
  if u <> fl_magic then
    failwith 
      "Netmcore_mempool: Mem block does not have the signature of free blocks"


let get_int8 mem offs =
  let s = String.create 8 in
  Netsys_mem.blit_memory_to_string mem offs s 0 8;
  Netnumber.int_of_int8 (Netnumber.HO.read_int8 s 0)


let lookup_fl_entry mem hdr offs =
  (* Return the pool_fl value of this free block *)
  assert_is_free mem hdr offs;
  let p = String.length fl_magic in
  let eoffs = get_int8 mem (offs+p) in
  let entry = (Netsys_mem.as_value mem eoffs : pool_fl) in
  entry
  

let prec_adj_free_block mem hdr offs =
  (* If there is a free block immediately preceding the block at
     offs, it is returned as [Some(p_offs,p_bsize)]
   *)
  if offs > hdr.pool_start then (
    match prec_block mem hdr offs with
      | None ->
	  assert_is_free mem hdr hdr.pool_start;
	  Some(hdr.pool_start, offs - hdr.pool_start)
      | Some(p_offs,p_bsize) ->
	  let q = p_offs + p_bsize in
	  if q < offs then (
	    assert_is_free mem hdr q;
	    Some(q, offs - q)
	  )
	  else
	    None
  )
  else
    None


let check_free_block mem hdr offs =
  (* If the block at offs is a free block, it is returned as
     [Some(s_offs,s_bsize)]
   *)
  let pool_end = Bigarray.Array1.dim mem in
  let q = offs in
  if q < pool_end then (
    match check_block mem hdr offs with
      | None ->
	  assert_is_free mem hdr q;
	  Some(q, pool_end-q)
      | Some(s_offs,s_bsize) ->
	  if q < s_offs then (
	    assert_is_free mem hdr q;
	    Some(q, s_offs - q)
	  )
	  else
	    None
  )
  else
    None

let del_in_pool_fl_at mem hdr offs bsize prev =
  (* Delete this block from the freelist, and remove fl_magic.
     prev points to the preceding entry in the free list (or 0).
   *)
  dlogr (fun () ->
	   sprintf "del_in_pool_fl_at offs=%d bsize=%d prev=%d"
	     offs bsize prev
	);
  let psize = bsize / page_size in
  let k = if psize < pool_fl_size then psize else 0 in
  let entry = lookup_fl_entry mem hdr offs in
  if prev = 0 then
    hdr.pool_fl.(k) <- entry.pool_fl_next
  else (
    let prev_entry = lookup_fl_entry mem hdr prev in
    prev_entry.pool_fl_next <- entry.pool_fl_next
  );
  let u = String.make (String.length fl_magic) ' ' in
  Netsys_mem.blit_string_to_memory u 0 mem offs (String.length fl_magic);
  dlog "del_in_pool_fl_at: done"


let del_in_pool_fl mem hdr offs bsize =
  (* Delete this block from the freelist, and remove fl_magic *)
  dlogr (fun () ->
	   sprintf "del_in_pool_fl offs=%d bsize=%d"
	     offs bsize
	);
  let psize = bsize / page_size in
  let k = if psize < pool_fl_size then psize else 0 in
  let prev = ref 0 in
  let cur = ref hdr.pool_fl.(k) in
  while !cur <> offs && !cur <> 0 do
    let e = lookup_fl_entry mem hdr !cur in
    prev := !cur;
    cur := e.pool_fl_next
  done;
  if !cur = 0 then
    failwith "Netmcore_mempool: Cannot find free block in free list";
  del_in_pool_fl_at mem hdr offs bsize !prev


let add_to_pool_fl mem hdr offs bsize =
  (* Add the block at mem+offs to the free list. This function does not
     check whether the block can be joined with adjacent blocks.
     bsize is the length of the block.
   *)
  dlogr (fun () -> sprintf "add_to_pool_fl offs=%d bsize=%d" offs bsize);
  let p = String.length fl_magic in
  Netsys_mem.blit_string_to_memory fl_magic 0 mem offs p;
  let entry_orig =
    { pool_fl_next = 0;
      pool_fl_bsize = bsize
    } in
  let (voffs, _) = Netsys_mem.init_value mem (offs+p+8) entry_orig [] in
  let entry = (Netsys_mem.as_value mem voffs : pool_fl) in
  let voffs_s =
    Netnumber.HO.int8_as_string (Netnumber.int8_of_int voffs) in
  Netsys_mem.blit_string_to_memory voffs_s 0 mem (offs+p) 8;
  let psize = bsize / page_size in
  let k = if psize < pool_fl_size then psize else 0 in
  entry.pool_fl_next <- hdr.pool_fl.(k);
  hdr.pool_fl.(k) <- offs;
  if bsize > hdr.pool_free_contiguous then
    hdr.pool_free_contiguous <- bsize;
  dlog "add_to_pool_fl done"


let merge_with_pool_fl mem hdr offs bsize =
  (* Same as add_to_pool_fl, but it is also checked whether the block
     can be joined with adjacent free blocks
   *)
  let prec_opt = prec_adj_free_block mem hdr offs in
  ( match prec_opt with
      | None -> ()
      | Some(p_offs,p_bsize) -> del_in_pool_fl mem hdr p_offs p_bsize	  
  );
  let succ_opt = check_free_block mem hdr (offs + bsize) in
  ( match succ_opt with
      | None -> ()
      | Some(s_offs,s_bsize) -> del_in_pool_fl mem hdr s_offs s_bsize	  
  );
  match prec_opt, succ_opt with
    | None, None ->
	add_to_pool_fl mem hdr offs bsize
    | Some(p_offs,p_bsize), None ->
	add_to_pool_fl mem hdr p_offs (p_bsize + bsize)
    | None, Some(s_offs,s_bsize) ->
	add_to_pool_fl mem hdr offs (bsize + s_bsize)
    | Some(p_offs,p_bsize), Some(s_offs,s_bsize) ->
	add_to_pool_fl mem hdr p_offs (bsize + p_bsize + s_bsize)


let find_free_block mem hdr bsize =
  (* Looks a free block up with at least bsize length, or raises 
     Out_of_pool_memory. The block is returned as (offs, prev) 
     where offs is the mem offset to the block, and prev is the 
     mem offset to the block preceding in the free list (or 0 if
     this is the first block in the free list)
   *)
  let psize = bsize / page_size in
  let k = ref(if psize < pool_fl_size then psize else 0) in
  let prev = ref 0 in
  let cur = ref hdr.pool_fl.( !k ) in
  let found = ref false in
  let best = ref 0 in
  let best_prev = ref 0 in
  let best_size = ref max_int in
  while not !found && (!cur <> 0 || !k > 0) do
    (* dlogr (fun () -> sprintf "k=%d cur=%d" !k !cur); *)
    if !cur = 0 then (
      incr k;
      if !k = pool_fl_size then k := 0;
      prev := 0;
      cur := hdr.pool_fl.( !k )
    ) else (
      let e = lookup_fl_entry mem hdr !cur in
      if e.pool_fl_bsize >= bsize && e.pool_fl_bsize < !best_size then (
	best := !cur;
	best_prev := !prev;
	best_size := e.pool_fl_bsize
      );
      if e.pool_fl_bsize=bsize then found := true;
      prev := !cur;
      cur := e.pool_fl_next;
    )
  done;
  if !best = 0 then
    raise Out_of_pool_memory;
  dlogr (fun () -> 
	   sprintf "find_free_block: bsize=%d best=%d best_prev=%d"
	     bsize !best !best_prev
	);
  (!best, !best_prev)


let set_pfc mem hdr =
  (* Set pool_free_contiguous  *)
  dlog "set_pfc";
  if hdr.pool_fl.(0) <> 0 then (
    let cur = ref hdr.pool_fl.( 0 ) in
    let best_bsize = ref 0 in
    while !cur <> 0 do
      let e = lookup_fl_entry mem hdr !cur in
      best_bsize := max !best_bsize e.pool_fl_bsize;
      cur := e.pool_fl_next
    done;
    hdr.pool_free_contiguous <- !best_bsize
  )
  else (
    hdr.pool_free_contiguous <- 0;
    let k = ref (pool_fl_size - 1) in
    while !k > 0 do
      if hdr.pool_fl.(!k) <> 0 then (
	hdr.pool_free_contiguous <- !k * page_size;
	k := 0
      )
      else decr k
    done
  )


let really_alloc_mem mem hdr bsize =
  (* bsize must here be a multiple of the page size *)
  dlogr (fun () -> sprintf "really_alloc_mem: bsize=%d" bsize);
  let (offs, prev) = find_free_block mem hdr bsize in
  let e = lookup_fl_entry mem hdr offs in
  let orig_fl_bsize = e.pool_fl_bsize in
  let alloc_offs =
    if e.pool_fl_bsize = bsize then (
      (* We need to remove this block from the free list *)
      dlog "really_alloc_mem: new block fits exactly";
      del_in_pool_fl_at mem hdr offs bsize prev;
      offs
    )
    else (
      (* We consume the block only partially. We split the block into two
	 parts: The first part remains a shorter free block, and the second
	 part is the newly allocated block. In some cases, the remaining
	 free part of the block has to be moved to a different free list.
       *)
      dlog "really_alloc_mem: have to split block";
      let new_fl_bsize = e.pool_fl_bsize - bsize in

      let orig_fl_psize = orig_fl_bsize / page_size in
      let orig_k = if orig_fl_psize < pool_fl_size then orig_fl_psize else 0 in

      let new_fl_psize = new_fl_bsize / page_size in
      let new_k = if new_fl_psize < pool_fl_size then new_fl_psize else 0 in

      if new_k <> orig_k then (
	(* Move the block to a different free list *)
	dlog "really_alloc_mem: switching free list";
	del_in_pool_fl_at mem hdr offs orig_fl_bsize prev;
	add_to_pool_fl mem hdr offs new_fl_bsize
      )
      else
	e.pool_fl_bsize <- new_fl_bsize;
      
      offs + new_fl_bsize
    ) in

  dlogr (fun () -> sprintf "really_alloc_mem: alloc_offs=%d" alloc_offs);
  
  (* Now add the newly allocated block to the tree: *)
  ( try
      Netmcore_util.AVL.add hdr.pool_alloc alloc_offs bsize
    with
      | Netmcore_util.AVL.Tree_full ->
	  raise Out_of_pool_memory
  );

  hdr.pool_free <- hdr.pool_free - bsize;
  assert(hdr.pool_free >= 0);

  if orig_fl_bsize = hdr.pool_free_contiguous then (
    (* We have to recompute hdr.pool_free_contiguous *)
    set_pfc mem hdr
  );

  dlog "really_alloc_mem: done";

  (* Return a memory bigarray: *)
  Bigarray.Array1.sub mem alloc_offs bsize
  

let really_free_mem mem hdr offs =
  match check_block mem hdr offs with
    | None ->
	failwith "Netmcore_mempool.free_mem: memory block not found"
    | Some(s_offs,bsize) ->
	if s_offs <> offs then
	  failwith "Netmcore_mempool.free_mem: memory block not found";
	
	(* remove this block from the tree: *)
	Netmcore_util.AVL.remove hdr.pool_alloc offs;

	(* add to the free list: *)
	merge_with_pool_fl mem hdr offs bsize;

	hdr.pool_free <- hdr.pool_free + bsize


let really_size_mem mem hdr offs =
  match check_block mem hdr offs with
    | None ->
	failwith "Netmcore_mempool.size_mem: memory block not found"
    | Some(s_offs,bsize) ->
	if s_offs <> offs then
	  failwith "Netmcore_mempool.size_mem: memory block not found";
	bsize


(* Prob: init_pool is called by a process that normally does not have access
   to mem! Idea: delay initialization until first access.
 *)

let delayed_init_pool mem =
  dlog "delayed_init_pool";
  let size = Bigarray.Array1.dim mem in
  let pg = to_page size in
  let size_pool_at = 64 + (pg / management_factor) in
  dlogr (fun () -> 
	   sprintf "size=%d pg=%d entries=%d"
	     size pg size_pool_at
	);
  let dummy = Netmcore_util.AVL.create_node() in
  let pool_alloc = Netmcore_util.AVL.create_header() in
  pool_alloc.Netmcore_util.AVL.nodes <- Array.make size_pool_at dummy;
  let hdr_orig =
    { pool_size = size;
      pool_free = 0; (* later *)
      pool_free_contiguous = 0; (* later *)
      pool_fl = Array.make pool_fl_size 0;
      pool_alloc = pool_alloc;
      pool_start = 0 (* later *)
    } in
  (* Now move everything to mem: *)
  let p0 = String.length magic in
  let p = ref (p0 + 8 + Netsys_sem.sem_size) in
  let (voffs, n) = Netsys_mem.init_value mem !p hdr_orig [] in
  dlog "delayed_init_pool: init_value done";
  let hoffs_s =
    Netnumber.HO.int8_as_string (Netnumber.int8_of_int voffs) in
  Netsys_mem.blit_string_to_memory hoffs_s 0 mem p0 8;
  dlog "delayed_init_pool: wrote hoffs";
  let hdr = (Netsys_mem.as_value mem voffs : header) in
  p := !p + n;
  (* Now allocate the tree: *)
  for k = 1 to size_pool_at - 1 do
    let (voffs,n) = Netsys_mem.init_value mem !p dummy [] in
    let node = (Netsys_mem.as_value mem voffs) in
    p := !p + n;
    hdr.pool_alloc.Netmcore_util.AVL.nodes.(k) <- node;
  done;
  dlog "delayed_init_pool: allocated nodes";
  Netmcore_util.AVL.init_header hdr.pool_alloc;
  dlog "delayed_init_pool: initialized tree";
  (* At this point we know how much mem we need for the header *)
  let p_block = ((to_page (!p-1)) + 1) * page_size in
  let remaining = size - p_block in
  add_to_pool_fl mem hdr p_block remaining;
  (* Init remaining fields: *)
  hdr.pool_start <- p_block;
  hdr.pool_free <- remaining;
  hdr.pool_free_contiguous <- remaining;
  dlog "delayed_init_pool done";
  hdr


let with_pool mem c f =
  dlog "with_pool";
  let p0 = String.length magic in
  let u = String.create p0 in
  Netsys_mem.blit_memory_to_string mem 0 u 0 p0;
  if u <> magic then
    failwith "Netmcore_mempool: Uninitialized pool";
  let sem = Netsys_sem.as_sem c mem (p0+8) in
  Netsys_sem.sem_wait sem Netsys_posix.SEM_WAIT_BLOCK;
  dlog "with_pool: got lock";
  (* CHECK: signals *)
  try
    let hoffs = get_int8 mem p0 in
    if hoffs = 0 then (
      dlog "with_pool: delayed initialization";
      ignore(delayed_init_pool mem);
      dlog "with_pool: delayed initialization done";
    );
    let hoffs = get_int8 mem p0 in
    assert(hoffs <> 0);
    let hdr = (Netsys_mem.as_value mem hoffs : header) in
    let r = f hdr in
    dlog "with_pool: unlock";
    Netsys_sem.sem_post sem;
    r
  with
    | error ->
	dlog "with_pool: unlock";
	Netsys_sem.sem_post sem;
	raise error
    

let init_pool mem c =
  let p0 = String.length magic in
  Netsys_mem.blit_string_to_memory magic 0 mem 0 p0;
  let hoffs_s = Netnumber.HO.int8_as_string (Netnumber.int8_of_int 0) in
  Netsys_mem.blit_string_to_memory hoffs_s 0 mem p0 8;
  ignore(Netsys_sem.sem_init c mem (p0+8) true 1)


let get_mem_c res_id =
  dlog "get_mem_c";
  let res = Netmcore.get_resource res_id in
  match res#repr with
    | `Posix_shm_preallocated_sc(_,mem,c) -> 
	dlog "get_mem_c successful";
	(mem,c)
    | _ -> failwith "Netmcore_mempool: this resource is not a pool"
  

let shm_name res_id =
  let res = Netmcore.get_resource res_id in
  match res#repr with
    | `Posix_shm_preallocated_sc(name,_,_) -> 
	name
    | _ -> failwith "Netmcore_mempool: this resource is not a pool"


let sem_container res_id =
  let res = Netmcore.get_resource res_id in
  match res#repr with
    | `Posix_shm_preallocated_sc(_,_,c) -> 
	c
    | _ -> failwith "Netmcore_mempool: this resource is not a pool"


let alloc_mem res_id bsize =
  (* round up to multiple of page_size: *)
  if bsize <= 0 then
    invalid_arg "Netmcore_mempool.alloc_mem: bad size";
  let bsize = ((bsize - 1) / page_size + 1) * page_size in
  let mem,c = get_mem_c res_id in
  with_pool mem c
    (fun hdr -> 
       let r =
	 really_alloc_mem mem hdr bsize in
       dlogr_alloc
	 (fun () ->
	    sprintf "alloc (id=%d) size=%d addr=0x%nx"
	      (match res_id with `Resource id -> id)
	      bsize
	      (Netsys_mem.memory_address r)
	 );
       dlogr_alloc
	 (fun () ->
	    sprintf "stats (id=%d) total=%d free=%d contiguous=%d"
	      (match res_id with `Resource id -> id)
	      hdr.pool_size hdr.pool_free hdr.pool_free_contiguous
	 );
       r
    )


let free_mem res_id m =
  let mem,c = get_mem_c res_id in
  let offs =
    Nativeint.to_int
      (Nativeint.sub
	 (Netsys_mem.memory_address m)
	 (Netsys_mem.memory_address mem)) in
  with_pool mem c
    (fun hdr -> 
       let r = really_free_mem mem hdr offs in
       dlogr_alloc
	 (fun () ->
	    sprintf "free (id=%d) addr=0x%nx"
	      (match res_id with `Resource id -> id)
	      (Netsys_mem.memory_address m)
	 );
       dlogr_alloc
	 (fun () ->
	    sprintf "stats (id=%d) total=%d free=%d contiguous=%d"
	      (match res_id with `Resource id -> id)
	      hdr.pool_size hdr.pool_free hdr.pool_free_contiguous
	 );
       r
   )


let size_mem_at_addr res_id addr =
  let mem,c = get_mem_c res_id in
  let offs =
    Nativeint.to_int
      (Nativeint.sub
	 addr
	 (Netsys_mem.memory_address mem)) in
 with_pool mem c
   (fun hdr -> really_size_mem mem hdr offs)

let size_mem res_id m =
  size_mem_at_addr res_id (Netsys_mem.memory_address m)


let stats res_id =
  let mem,c = get_mem_c res_id in
  with_pool mem c
    (fun hdr ->
       (hdr.pool_size, hdr.pool_free, hdr.pool_free_contiguous)
    )


let debug_info res_id =
  let mem,c = get_mem_c res_id in
  with_pool mem c
    (fun hdr ->
       let free_list_string k =
	 let b = Buffer.create 100 in
	 let first = ref true in
	 Buffer.add_string b "{";
	 let cur = ref hdr.pool_fl.( k ) in
	 while !cur <> 0 do
	   let e = lookup_fl_entry mem hdr !cur in
	   if not !first then bprintf b ",";
	   bprintf b "%d->%d" !cur e.pool_fl_bsize;
	   first := false;
	   cur := e.pool_fl_next
	done;
	 Buffer.add_string b "}";
	 Buffer.contents b
       in
       let b = Buffer.create 100 in
       bprintf b "pool_size = %d\n" hdr.pool_size;
       bprintf b "pool_free = %d\n" hdr.pool_free;
       bprintf b "pool_free_contiguous = %d\n" hdr.pool_free_contiguous;
       bprintf b "pool_start = %d\n" hdr.pool_start;
       bprintf b "allocations (offs->bsize): %s\n" 
	 (Netmcore_util.AVL.as_debug_list hdr.pool_alloc);
       for k = 0 to pool_fl_size - 1 do
	 bprintf b "free_list[%d]: %s\n" k (free_list_string k)
       done;
       Buffer.contents b
    )
    

let create_mempool ?(alloc_really=false) size =
  (* round up to multiple of page_size: *)
  if size <= 0 then
    invalid_arg "Netmcore_mempool.create_mempool: bad size";
  let size = ((size - 1) / page_size + 1) * page_size in
  let res_id, full_name, sc = 
    Netmcore.create_preallocated_shm_sc ~value_area:true "/mempool" size in
  (* Map the first page only *)
  let fd = Netsys_posix.shm_open full_name [Netsys_posix.SHM_O_RDWR] 0 in
  if alloc_really then (
    for k = 0 to size/page_size-1 do
      ignore(Unix.lseek fd (k*page_size) Unix.SEEK_SET);
      ignore(Unix.write fd "\000" 0 1);
    done;
    ignore(Unix.lseek fd 0 Unix.SEEK_SET);
  );
  let mem =
    try
      let mem = Netsys_mem.memory_map_file fd true page_size in
      Unix.close fd;
      mem
    with
      | error -> Unix.close fd; raise error in
  init_pool mem sc;
  dlogr_alloc
    (fun () ->
       sprintf "mempool (id=%d) space=%d created"
	 (match res_id with `Resource id -> id)
	 size
    );
  res_id


let unlink_mempool res_id =
  let res = Netmcore.get_resource res_id in
  match res#repr with
    | `Posix_shm_preallocated_sc(name,mem,c) -> 
        Netsys_sem.unlink (Netsys_sem.prefix c);
        Netsys_posix.shm_unlink name;
    | _ -> failwith "Netmcore_mempool: this resource is not a pool"
