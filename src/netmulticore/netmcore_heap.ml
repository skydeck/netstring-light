(* $Id: netmcore_heap.ml 1826 2013-01-13 17:57:16Z gerd $ *)

(* Structure of the heaps:

   Heaps consist of a root block and a list of extension blocks
   (which is initially empty). Extension blocks are added when no more
   memory is available in the blocks so far allocated.  Extension
   blocks can also be given back to the pool when they become empty
   again. Blocks are never changed in size. The blocks are managed
   with a doubly-linked list.

   Both allocated memory and free memory in the heap must have
   "Ocaml structure", i.e. there is always a header preceding the
   value. The header includes the length and the bits for gabage
   collection.

   Free memory is also added to free lists if the memory area
   consists of at least 2 words (1 word free memory can be first
   reclaimed by the garabage collector). There are several free lists
   for different sizes of the free areas.

   When there is no more available memory, a garbage collection is
   triggered (see below). If this is also not sufficient, a further
   extension block is added.

   Garbage collection is done by marking and sweeping, all in one
   go. We use only the colors "black" and "white". In the sweep
   phase also the free lists are completely rebuilt. This makes
   it possible to merge adjacent free memory, and to reclaim
   one word fragments.

   Note that custom values (incl. bigarrays, int32, int64, nativeint)
   cannot live in heaps, because the GC is not able to figure out which
   mem regions are used by the custom blocks.
 *)

(* FIXME: use better locking scheme (r/w locks) *)

(* FIXME: flag is_white: whether all values are white. Protect against
   crashes during the mark phase
 *)

open Printf

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netmcore_heap" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netmcore_heap" Debug.enable

let () =
  Netlog.Debug.register_module "Netmcore_heap" Debug.enable


type 'a heap =
    { heap_sem : Netsys_mem.memory;
      heap_pool : Netmcore.res_id;
      mutable heap_value : 'a;
      heap_ext : ext_block;
      heap_fl : Obj.t array;
      mutable heap_roots : Obj.t array;
    }

(* Repr details:

   heap_ext is the first extension block (root block). See below for
   repr details.

   heap_fl: The free lists of unused Ocaml values. This array starts
   several free lists: heap_fl.(k) is the free list for values with
   a size of k words (k > 0). heap_fl.(0) is the free list for values
   of any size.

   The free list pointer references the free Obj.t (which is always
   preceded by a header). The next element of the free list is in 
   field 0 of the Obj.t.

   The special value null_obj is used for "None".

   The values in [heap_roots] are considered as the roots. Values
   null_obj are ignored in this array. The [heap_roots] array resides
   in the heap. There is right now no free list providing fast access
   to unused elements of this array (TBD).

   Note that the heap_sem bigarray is ok because it is not stored in
   the value area (bigarrays would be incompatible with our GC).
 *)


and ext_block =
    { mutable ext_prev : int;  (* encoded start address *)
      mutable ext_next : int;  (* encoded start address *)
      ext_addr : nativeint;    (* start address of the mem block *)
      ext_size : int;          (* size of the mem block *)
      mutable ext_start : int; (* offset of value area start *)
      ext_end : int;           (* offset of value area end *)
    }

(* Repr details:

   Extension blocks: The first extension block is simply put into the
   root memory block. The other extension blocks have this format:
   At the beginning there is the [magic] string, followed by the
   Ocaml value of the [ext_block] record. The remaining part of the memory
   block can be used for storing values. This area starts at the
   offset [ext_start] (relative to the beginning of the mem block), and
   ends at [ext_end] (i.e. last byte is at [ext_end-1]).

   ext_prev and ext_next: contain the encoded address of the memory block
   containing the ext_block we are referring to. The encoding: Shift the
   address 1 bit to the right, so it fits into an [int] value. The
   special value [no_ext_block] is used to denote a "none". The special
   value [root_ext_block] is used to denote the first extension block
   (remember it does not have a mem block of its own, and is a special
   case).

   ext_addr: this is the address of the mem block containing the
   ext_block record. ext_size is the size of this mem block in bytes.

   Note that the ext_addr custom value is ok because it is not stored in
   the value area (custom values would be incompatible with our GC).
 *)

type 'a descr = int

type mutator =
    { heap : Obj.t heap;
      mutable alive : bool;
      mutable pinned : int list;
    }

let fl_size = 64

let null_obj = Obj.repr 0

let magic = "NETHEAP\n"
let magic_len = 8

let min_ext_size = 65536
  (* min size of an extension block *)

let max_ext_block = 256
  (* max space for ext_block record in bytes *)

let no_ext_block = 0
  (* used in ext_prev and ext_next *)

let root_ext_block = 1
  (* used in ext_prev and ext_next *)

let n_roots = 20
  (* initial number of root values *)


let bytes_per_word =
  match Sys.word_size with
    | 32 -> 4
    | 64 -> 8
    | _ -> assert false

let descr_of_heap heap =
  let addr = heap.heap_ext.ext_addr in
  Nativeint.to_int(Nativeint.shift_right addr 1)

let heap_of_descr pool ptr =
  let addr = Nativeint.shift_left (Nativeint.of_int ptr) 1 in
  try
    let size = Netmcore_mempool.size_mem_at_addr pool addr in
    let mem = Netsys_mem.grab addr size in
    let u = String.create magic_len in
    Netsys_mem.blit_memory_to_string mem 0 u 0 magic_len;
    if u <> magic then raise Not_found;
    let hoffs_s = String.create 8 in
    Netsys_mem.blit_memory_to_string mem 8 hoffs_s 0 8;
    let hoffs =
      Netnumber.int_of_int8 (Netnumber.HO.read_int8 hoffs_s 0) in
    Netsys_mem.as_value mem hoffs
  with
    | Not_found ->
	failwith "Netmcore_heap.heap_of_descr: no heap structure found \
                  at this address"

let create_mutator heap =	
  { heap = Obj.magic heap; alive = true; pinned = [] }

let ext_mem ext =
  Netsys_mem.grab ext.ext_addr ext.ext_size


let ext_block heap (ptr:int) : ext_block =
  (* for following ext_prev and ext_next *)
  if ptr = no_ext_block then
    failwith "Netmcore_heap.ext_block: null pointer";
  if ptr = root_ext_block then
    heap.heap_ext
  else (
    let nat_ptr = Nativeint.shift_left (Nativeint.of_int ptr) 1 in
    let mem = Netsys_mem.grab nat_ptr max_ext_block in
    let u = String.create magic_len in
    Netsys_mem.blit_memory_to_string mem 0 u 0 magic_len;
    if u <> magic then
      failwith "Netmcore_heap.ext_block: bad magic";
    Netsys_mem.as_value mem (magic_len + bytes_per_word)
  )


let ptr_to_ext_block heap (ext:ext_block) : int =
  if ext == heap.heap_ext then
    root_ext_block
  else
    Nativeint.to_int (Nativeint.shift_right ext.ext_addr 1)


let debug_info heap =
  let b = Buffer.create 80 in
  bprintf b "pool = %d\n"
    (match heap.heap_pool with
       | `Resource id -> id
    );
  bprintf b "value = @0x%nx\n"
    (Netsys_mem.obj_address (Obj.repr heap.heap_value));
  for k = 0 to Array.length heap.heap_roots - 1 do
    if heap.heap_roots.(k) != null_obj then
      bprintf b "root[%d] = @0x%nx\n"
	k (Netsys_mem.obj_address heap.heap_roots.(k));
  done;
  for k = 0 to Array.length heap.heap_fl - 1 do
    if heap.heap_fl.(k) != null_obj then
      bprintf b "fl[%d] = @0x%nx\n"
	k (Netsys_mem.obj_address heap.heap_fl.(k));
  done;
  let p = ref 0 in
  let ext = ref (Some heap.heap_ext) in
  while !ext <> None do
    match !ext with
      | Some x ->
	  let next =
	    if x.ext_next = no_ext_block then
	      None
	    else
	      Some(ext_block heap x.ext_next) in
	  bprintf b "ext[%d] = @0x%nx, size 0x%x\n"
	    !p x.ext_addr x.ext_size;
	  incr p;
	  ext := next;
      | None -> assert false
  done;
  Buffer.contents b


let extend_heap heap size =
  (* Add another extension block to the heap so that a [size] value fits
     into it
   *)
  dlogr (fun () -> sprintf "extend_heap size=%d" size);
  let req_size = size + max_ext_block in
  (* N.B. choosing max_ext_block high enough is crucial *)
  let mem_size = max req_size min_ext_size in
  let mem = Netmcore_mempool.alloc_mem heap.heap_pool mem_size in
  try
    let mem_real_size = Netmcore_mempool.size_mem heap.heap_pool mem in
    (* mem_real_size >= mem_size! *)
    Netsys_mem.blit_string_to_memory magic 0 mem 0 magic_len;
    let old_next = heap.heap_ext.ext_next in
    let ext_orig =
      { ext_prev = ptr_to_ext_block heap heap.heap_ext;
	ext_next = old_next;
	ext_addr = Netsys_mem.memory_address mem;
	ext_size = mem_real_size;
	ext_start = 0; (* later *)
	ext_end = mem_real_size;
      } in
    let (voffs, n) = 
      Netsys_mem.init_value mem magic_len ext_orig
	[Netsys_mem.Copy_bigarray; Netsys_mem.Copy_custom_int; 
	 Netsys_mem.Keep_atom] in
    let ext =
      Netsys_mem.as_value mem voffs in
    (* If the block is larger than the typical size of 64K, we initialize
       it so that only the requested value fits exactly
     *)
    ext.ext_start <- 
      if req_size = mem_size then
	mem_real_size - size
      else
	magic_len + n;
    assert(ext.ext_start >= magic_len+n);
    heap.heap_ext.ext_next <- ptr_to_ext_block heap ext;
    if old_next <> no_ext_block then
      (ext_block heap old_next).ext_prev <- ptr_to_ext_block heap ext;
    dlogr (fun () -> sprintf "extent_heap addr=%nx real_size=%d usable=%d"
	     ext.ext_addr ext.ext_size (ext.ext_end - ext.ext_start));
    Some(mem, ext.ext_start, ext.ext_end - ext.ext_start)
  with
    | error ->
	Netmcore_mempool.free_mem heap.heap_pool mem;
	raise error

let shrink_heap heap ext =
  (* Remove ext from the chaining, and give the mem block back to the pool *)
  dlogr (fun () -> sprintf "shrink_heap addr=%nx" ext.ext_addr);
  assert (ext != heap.heap_ext);
  let mem = Netsys_mem.grab ext.ext_addr ext.ext_size in
  let u = String.create magic_len in
  Netsys_mem.blit_memory_to_string mem 0 u 0 magic_len;
  if u <> magic then
    failwith "Netmcore_heap.shrink_heap";
  let v = String.make magic_len ' ' in
  Netsys_mem.blit_string_to_memory v 0 mem 0 magic_len;
  (* ext is never the first element in the chain *)
  let prev = ext_block heap ext.ext_prev in
  prev.ext_next <- ext.ext_next;
  if ext.ext_next <> no_ext_block then (
    let next = ext_block heap ext.ext_next in
    next.ext_prev <- ext.ext_prev
  );
  Netmcore_mempool.free_mem heap.heap_pool mem;
  dlog "shrink_heap done"


let init_as_block mem offs size =
  let words = size / bytes_per_word in
  assert(words >= 2);
  Netsys_mem.init_header mem offs Obj.string_tag (*block size:*)(words-1)


let init_as_atom mem offs =
  Netsys_mem.init_header mem offs (*tag:*)0 (*block size:*)0


let del_in_fl heap (entry:Obj.t) (prev:Obj.t) =
  let next = Obj.field entry 0 in
  if prev == null_obj then (
    (* it must be one of the root pointers *)
    for k=0 to fl_size-1 do
      if heap.heap_fl.(k) == entry then
	heap.heap_fl.(k) <- next
    done
  )
  else
    Obj.set_field prev 0 next


let add_to_fl heap mem offs len =
  let words = len / bytes_per_word in
  let k = if words < fl_size then words else 0 in
  let old_head = heap.heap_fl.(k) in
  init_as_block mem offs len;
  let v = Netsys_mem.as_value mem (offs + bytes_per_word) in
  let o = Obj.repr v in
  Obj.set_field o 0 old_head;
  heap.heap_fl.(k) <- o


let memory_range_of_pool heap =
  let res = Netmcore.get_resource heap.heap_pool in
  let (start_addr, end_addr) =
    match res#repr with
      | `Posix_shm_preallocated_sc(_,mem,_) -> 
	  let mem_size = Bigarray.Array1.dim mem in
          let sa = Netsys_mem.memory_address mem in
	  let ea = Nativeint.add sa (Nativeint.of_int mem_size) in
	  (sa, ea)
      | _ -> 
	  assert false in
  (start_addr, end_addr)


let do_gc heap =
  (* Our assumption is that all values have the GC color "white".
     
     Mark phase: Iter over all roots. For each root, visit the referenced
     values, and set all values to the GC color "black" if they are visited
     for the first time.

     Sweep phase: Iter over all extension blocks. Iter over all values
     in an extension block. "White" values are added to the freelist.
     "Black" values are changed to "white" again, but remain otherwise
     untouched. 

     The freelists are rebuilt during sweep. We check for the special
     case that an extension block is completely empty - in this case
     it is entirely removed.
   *)
  dlog "gc start";

  let (start_addr,end_addr) = memory_range_of_pool heap in
  dlogr (fun () ->
	   sprintf "range: 0x%nx - 0x%nx" start_addr end_addr);

  let debug_addr = Hashtbl.create 20 in
  (* For debugging [mark] *)

  let rec mark (v:Obj.t) =
    (* FIXME: this recursion can cause stack overflows *)
    if Obj.is_block v then (
      let a = Netsys_mem.hdr_address v in
      (* We do not follow blocks that are outside the shm area. In general,
	 such out-of-shm blocks are likely to be erroneous, though
       *)
      if a >= start_addr && a < end_addr then (
	if Netsys_mem.color v = Netsys_mem.White then (
	  dlogr (fun () -> sprintf "marking 0x%nx" 
		   (Netsys_mem.obj_address v));
	  Netsys_mem.set_color v Netsys_mem.Black;
	  if !Debug.enable then
	    Hashtbl.replace debug_addr (Netsys_mem.obj_address v) ();
	  if Obj.tag v < Obj.no_scan_tag then (
	    let sz = Obj.size v in
	    for k = 0 to sz - 2 do
	      mark (Obj.field v k)
	    done;
	    if sz >= 1 then
	      mark (Obj.field v (sz-1))  (* tail-rec *)
	  )
	)
	else (
	  if !Debug.enable then (
	    if not (Hashtbl.mem debug_addr (Netsys_mem.obj_address v)) then (
	      dlog (sprintf "wrong color at 0x%nx"
		      (Netsys_mem.obj_address v))
	    )
	  )
	)
      )
      else dlog "addr out of range"
    ) in
  
  let sweep_ext ext =
    dlogr (fun () -> sprintf "sweep_ext addr=%nx" ext.ext_addr);
    let mem = Netsys_mem.grab ext.ext_addr ext.ext_size in
    let offs = ref ext.ext_start in
    let cur_fl_entry = ref None in
    let all_free = ref true in
    let free_size = ref 0 in
    let push() =
      match !cur_fl_entry with
	| Some(fl_offs,fl_len) ->
	    free_size := !free_size + fl_len;
	    if fl_len > bytes_per_word then
	      add_to_fl heap mem fl_offs fl_len
	    else
	      init_as_atom mem fl_offs;
	    cur_fl_entry := None
	| None -> ()
    in
    let bigarray_data_size p =
      (* Check for bigarrays. Netsys_mem.init_value uses a special
         convention for marking the data part of the bigarray: The
         data part is started with an _empty_ abstract block, followed
         by the size of the data part, and finally followed by the
         data part.
       *)
      if p + bytes_per_word < ext.ext_end then (
        let v1 = Netsys_mem.as_value mem (p + bytes_per_word) in
        if Obj.tag v1 = Obj.abstract_tag && Obj.size v1 = 0 then (
          let data_size_s = String.create bytes_per_word in
          Netsys_mem.blit_memory_to_string
            mem (p + bytes_per_word) data_size_s 0 bytes_per_word;
          let data_size =
            match bytes_per_word with
              | 4 -> Netnumber.int_of_uint4
                       (Netnumber.HO.read_uint4 data_size_s 0)
              | 8 -> Netnumber.int_of_uint8
                       (Netnumber.HO.read_uint8 data_size_s 0)
              | _ -> assert false in
          data_size + 2   (* 2 for the abstract block and the length *)
        )
        else 0
      )
      else 0 
    in
    while !offs < ext.ext_end do
      let v = Netsys_mem.as_value mem (!offs + bytes_per_word) in
      let sz = Obj.size v in
      let next_offs = !offs + (sz+1)*bytes_per_word in
      let extra_size =
        if Obj.tag v = Obj.custom_tag && Netsys_mem.is_bigarray v then
          bigarray_data_size next_offs
        else
          0 in
      ( match Netsys_mem.color v with
	  | Netsys_mem.White ->
	      dlogr (fun () -> sprintf "freeing 0x%nx"
		       (Nativeint.add ext.ext_addr
			  (Nativeint.of_int (!offs + bytes_per_word))));
              let sz_total = sz + 1 + extra_size in
	      ( match !cur_fl_entry with
		  | None ->
		      cur_fl_entry := Some(!offs, sz_total * bytes_per_word)
		  | Some(fl_offs, fl_len) ->
		      cur_fl_entry := Some(fl_offs,
					   fl_len +
					     sz_total * bytes_per_word)
	      );
	  | _ ->
	      dlogr (fun () -> sprintf "keeping 0x%nx"
		       (Nativeint.add ext.ext_addr 
			  (Nativeint.of_int (!offs + bytes_per_word))));
	      all_free := false;
	      Netsys_mem.set_color v Netsys_mem.White;
	      push()
      );
      offs := next_offs + extra_size * bytes_per_word;
    done;
    if !all_free && ext != heap.heap_ext then
      shrink_heap heap ext
    else
      push();

    dlogr (fun () -> sprintf "sweep_ext free_size=%d" !free_size);

    (!free_size, ext.ext_end - ext.ext_start)
  in

  let sweep () =
    (* Reset the free lists: *)
    for k = 0 to fl_size - 1 do
      heap.heap_fl.(k) <- null_obj
    done;
    (* Iterate over the extension blocks: *)
    let ext = ref (Some heap.heap_ext) in
    let free_total = ref 0 in
    let size_total = ref 0 in
    while !ext <> None do
      match !ext with
	| Some x ->
	    (* Get the [next] block now, because [x] may be deleted *)
	    let next =
	      if x.ext_next = no_ext_block then
		None
	      else
		Some(ext_block heap x.ext_next) in
	    let (f,s) = sweep_ext x in
	    free_total := !free_total + f;
	    size_total := !size_total + s;
	    ext := next
	| None -> assert false
    done;
    (!free_total, !size_total) in

  dlog "mark";
  let root = Obj.repr heap.heap_roots in
  (* root is the only value that is not reset to white color! *)
  Netsys_mem.set_color root Netsys_mem.White;
  mark root;

  dlog "sweep";
  let (f,s) = sweep() in
  dlog "gc done";
  (f, s)


let do_gc_adjust heap size =
  (* Do a GC pass and adjust the amount of free mem. If new mem is allocated
     it should be at least [size]
   *)
  let (free, total) = do_gc heap in
  if free < total/2 then (
    let alloc_size0 = max size (total/2 - free) in
    let alloc_size = ((alloc_size0 - 1) / 8 + 1) * 8 in
    dlogr (fun () -> sprintf "do_gc_adjust: alloc_size=%d" alloc_size);
    ( match extend_heap heap alloc_size with
	| Some(mem, offs, len) ->
	    add_to_fl heap mem offs len
	| None ->
	    ()
    )
  )
  

let find_free_block heap size =
  (* Find a free block >= [size] in the freelists *)
  dlogr (fun () -> sprintf "find_free_block size=%d" size);
  let words = size / bytes_per_word in
  let k = ref(if words < fl_size then words else 0) in
  let prev = ref null_obj in
  let cur = ref heap.heap_fl.( !k ) in
  let found = ref false in
  let best = ref null_obj in
  let best_prev = ref null_obj in
  let best_size = ref max_int in
  while not !found && (!cur != null_obj || !k > 0) do
    if !cur == null_obj then (
      incr k;
      if !k = fl_size then k := 0;
      prev := null_obj;
      cur := heap.heap_fl.( !k )
    ) else (
      let n = Obj.size !cur in
      (* Actually, we have one more word than n because of the value header *)
      if n+1 >= words && n+1 < !best_size then (
        best := !cur;
        best_prev := !prev;
	best_size := n+1;
      );
      if n+1=words then found := true;
      prev := !cur;
      cur := Obj.field !cur 0
    )
  done;
  if !best != null_obj then (
    dlog "found free block";
    let addr = Netsys_mem.hdr_address !best in
    let byte_size = (Obj.size !best + 1) * bytes_per_word in
    let mem = Netsys_mem.grab addr byte_size in
    Some(mem, 0, byte_size, !best, !best_prev)
  )
  else
    None


let alloc_in_free_block heap size mem offs len entry prev =
  (* Take a part of the free block at [mem+offs..mem+offs+len-1] to
     satisfy the allocation of a block of [size]. [entry] is the
     entry in the freelist. [prev] is the  predecessor in the freelist
     or [null].
   *)
  dlogr (fun () -> 
	   sprintf "alloc_in_free_block size=%d len=%d" size len);
  del_in_fl heap entry prev;
  init_as_block mem offs size;
  if len = size then
    (* The whole block can be used *)
    (mem, offs)
  else (
    (* The block needs to be split *)
    if len = size + bytes_per_word then (
      (* The remaining part would only have 1 word. We initialize this word
	 as zero-length block, but it is not entered into a freelist
       *)
      init_as_atom mem (offs+size);
      (mem, offs)
    )
    else (
      (* the remaining part is added to a freelist *)
      add_to_fl heap mem (offs+size) (len - size);
      (mem, offs)
    )
  )


let alloc heap size =
  (* First search in the freelists *)
  (* assert: size divisible by word size *)
  dlogr (fun () -> sprintf "alloc size=%d" size);
  match find_free_block heap size with
    | Some(mem, offs, len, obj, prev) ->
	dlog "alloc: got block from free list";
	alloc_in_free_block heap size mem offs len obj prev
    | None ->
	(* Nothing found in the freelists: Do now a GC pass, and try again.
	 *)
	( do_gc_adjust heap size;
	  match find_free_block heap size with
	    | Some(mem, offs, len, obj, prev) ->
		dlog "alloc: got block from free list";
		alloc_in_free_block heap size mem offs len obj prev
	    | None ->
		(* Still unsuccessful. Add another block and try again *)
		dlog "alloc: extending heap";
		( match extend_heap heap size with
		    | Some(mem, offs, len) ->
(*
eprintf "mem=%nx offs=%x len=%d\n%!"
  (Netsys_mem.memory_address mem)
  offs
  len;
 *)
			if len = size then (
			  init_as_block mem offs size;
			  (mem,offs)
			)
			else (
			  assert(len <> size + bytes_per_word);
			  init_as_block mem offs size;
			  add_to_fl heap mem (offs+size) (len - size);

			  (mem,offs)
			)
		    | None ->
			raise Netmcore_mempool.Out_of_pool_memory
		)
	)


let add mut newval =
  (* It is assumed that we already got the lock for the heap *)
  dlog "add";
  if not mut.alive then
    failwith "Netmcore_heap.add: invalid mutator";
  if Obj.is_int (Obj.repr newval) then
    newval
  else (
    let heap = mut.heap in
    let heap_mem = ext_mem heap.heap_ext in
    let _, size =
      Netsys_mem.init_value
	heap_mem 0 newval 
	[ Netsys_mem.Copy_simulate; Netsys_mem.Keep_atom; 
	  Netsys_mem.Copy_custom_int; Netsys_mem.Copy_bigarray;
	] in
    assert(size mod bytes_per_word = 0);
    (* We need [size] bytes to store [newval] *)
    let (mem, offs) = alloc heap size in
    (* Do the copy: Note that we need the same flags here as above,
       except Copy_simulate which is omitted
     *)
    let voffs, size' =
      Netsys_mem.init_value
	mem offs newval 
	[Netsys_mem.Keep_atom; Netsys_mem.Copy_custom_int; 
         Netsys_mem.Copy_bigarray] in
    assert(size = size');
    (* Return the new value: *)
    dlog "add done";
    Netsys_mem.as_value mem voffs
  )


let add_immutable mut newval =
  (* It is assumed that we already got the lock for the heap *)
  dlog "add";
  if not mut.alive then
    failwith "Netmcore_heap.add_immutable: invalid mutator";
  if Obj.is_int (Obj.repr newval) then
    newval
  else (
    let heap = mut.heap in
    let heap_mem = ext_mem heap.heap_ext in
    let (start_addr,end_addr) = memory_range_of_pool heap in
    let cc = [ (start_addr,end_addr) ] in
    let _, size =
      Netsys_mem.init_value
	~cc heap_mem 0 newval 
	[ Netsys_mem.Copy_simulate; Netsys_mem.Keep_atom; 
	  Netsys_mem.Copy_custom_int; Netsys_mem.Copy_bigarray;
          Netsys_mem.Copy_conditionally
	] in
    assert(size mod bytes_per_word = 0);
    (* We need [size] bytes to store [newval] *)
    let (mem, offs) = alloc heap size in
    (* Do the copy: Note that we need the same flags here as above,
       except Copy_simulate which is omitted
     *)
    let voffs, size' =
      Netsys_mem.init_value
	~cc mem offs newval 
	[Netsys_mem.Keep_atom; Netsys_mem.Copy_custom_int; 
         Netsys_mem.Copy_bigarray; Netsys_mem.Copy_conditionally
        ] in
    assert(size = size');
    (* Return the new value: *)
    dlog "add done";
    Netsys_mem.as_value mem voffs
  )


let add_string mut length =
  (* It is assumed that we already got the lock for the heap *)
  dlog "add";
  if not mut.alive then
    failwith "Netmcore_heap.add_string: invalid mutator";
  let heap = mut.heap in
  let size = Netsys_mem.init_string_bytelen length in
  assert(size mod bytes_per_word = 0);
  (* We need [size] bytes to store [newval] *)
  let (mem, offs) = alloc heap size in
  let voffs, size' = Netsys_mem.init_string mem offs length in
  assert(size = size');
  (* Return the new value: *)
  dlog "add_string done";
  Netsys_mem.as_value mem voffs


let add_some mut (x:'a) =
  (* Very low-level! *)
  let y_orig = (Some (Obj.magic 0) : 'a option) in
  let y = add mut y_orig in
  Obj.set_field (Obj.repr y) 0 (Obj.repr x);
  y

let set_tmp_root heap x =
  if Obj.is_block (Obj.repr x) then (
    dlog "set_tmp_root: searching for free root element";
    (* Look for a free entry in the list of roots. There is always a
       free entry
     *)
    let n = Array.length heap.heap_roots in
    let found = ref false in
    let k = ref (-1) in
    while not !found && !k < n-1 do
      incr k;
      found := heap.heap_roots.( !k ) == null_obj
    done;
    assert(!found);
    dlogr
      (fun () -> sprintf "set_tmp_root: root element %d" !k);
    heap.heap_roots.( !k ) <- Obj.repr x;
    (* If the array of roots is full, reallocate it.
       At this point we can reallocate, because x is already member of
       the roots array. (Realloction can trigger the GC!)
     *)
    let j = ref !k in
    found := false;
    while not !found && !j < n-1 do
      incr j;
      found := heap.heap_roots.( !j ) == null_obj
    done;
    if not !found then (
      dlog "set_tmp_root: reallocation";
      let r_orig = Array.make (2*n) null_obj in
      let mut = create_mutator heap in
      let r = add mut r_orig in
      Array.blit heap.heap_roots 0 r 0 n;
      heap.heap_roots <- r;
    );
    !k
  )
  else (-1)


let release_tmp_root heap k =
  dlog "release_tmp_root: freeing root";
  if k >= 0 then
    heap.heap_roots.(k) <- null_obj


let add_uniform_array mut n x_orig =
  if not mut.alive then
    failwith "Netmcore_heap.add_uniform_array: invalid mutator";
  let heap = mut.heap in
  let heap_mem = ext_mem heap.heap_ext in
  let x_orig_obj = Obj.repr x_orig in
  let x_is_float =
    Obj.is_block x_orig_obj && Obj.tag x_orig_obj = Obj.double_tag in
  let x_is_block =
    Obj.is_block x_orig_obj && Obj.tag x_orig_obj <> Obj.double_tag in
  let x_size =
    if x_is_block then
      snd (
	Netsys_mem.init_value
	  heap_mem 0 x_orig
	  [ Netsys_mem.Copy_simulate; Netsys_mem.Keep_atom;
	    Netsys_mem.Copy_custom_int; Netsys_mem.Copy_bigarray
	  ])
    else
      0 in
  let a_size = 
    if x_is_float then
      Netsys_mem.init_float_array_bytelen n
    else
      Netsys_mem.init_array_bytelen n in
  let t_size = x_size + a_size in
  (* allocate in one go, so the new value cannot be garbage collected *)
  let (mem,offs) = alloc heap t_size in
  let x =
    if x_is_block then (
      let x_voffs, _ =
	Netsys_mem.init_value
	  mem offs x_orig
	  [Netsys_mem.Keep_atom; Netsys_mem.Copy_custom_int;
           Netsys_mem.Copy_bigarray
          ] in
      Netsys_mem.as_value mem x_voffs
    )
    else x_orig in
  let a_offs = offs + x_size in
  let a =
    if x_is_float then (
      let (a_voffs, _) = Netsys_mem.init_float_array mem a_offs n in
      let a = (Netsys_mem.as_value mem a_voffs : _ array) in
      let a_obj = Obj.repr a in
      let x_float = (Obj.obj x_orig_obj : float) in
      for k = 0 to n-1 do
	Obj.set_double_field a_obj k x_float
      done;
      a
    )
    else (
      let (a_voffs, _) = Netsys_mem.init_array mem a_offs n in
      let a = (Netsys_mem.as_value mem a_voffs : _ array) in
      let a_obj = Obj.repr a in
      let x_obj = Obj.repr x in
      for k = 0 to n-1 do
	Obj.set_field a_obj k x_obj
      done;
      a
    ) in
  a


let add_init_array mut n f =
  if not mut.alive then
    failwith "Netmcore_heap.add_init_array: invalid mutator";
  if n=0 then
    Obj.magic(add_uniform_array mut 0 0)
  else (
    let x0 = f 0 in
    let a = add_uniform_array mut n x0 in
    let r = set_tmp_root mut.heap a in
    for k = 1 to n-1 do
      Array.unsafe_set a k (add mut (f k))
    done;
    release_tmp_root mut.heap r;
    a
  )


let with_lock heap f =
  dlog "with_lock waiting";
  let c = Netmcore_mempool.sem_container heap.heap_pool in
  let sem = Netsys_sem.as_sem c heap.heap_sem 0 in
  Netsys_sem.sem_wait sem Netsys_posix.SEM_WAIT_BLOCK;
  dlog "with_lock cont";
  try
    let r = f() in
    Netsys_sem.sem_post sem;
    dlog "with_lock returning";
    r
  with
    | error ->
	Netsys_sem.sem_post sem;
	dlog "with_lock exception";
	raise error


let gc heap =
  with_lock heap
    (fun () ->
       ignore(do_gc heap)
    )


let pin mut x =
  (* FIXME: there is a cheaper way of pinning, because we have the
     heap lock. We could also just gather the roots in a list, and
     consider this list during GC
   *)
  let k = set_tmp_root mut.heap x in
  mut.pinned <- k :: mut.pinned


let modify heap mutate =
  with_lock heap
    (fun () ->
       let mut = create_mutator (Obj.magic heap) in
       let finish() =
	 mut.alive <- false;
	 List.iter (fun k -> release_tmp_root heap k) mut.pinned in
       try
	 let r = mutate mut in
	 finish();
	 r
       with
	 | error ->
	     finish();
	     raise error
    )


let copy x =
  if Obj.is_block (Obj.repr x) then
    Netsys_mem.copy_value 
      [Netsys_mem.Keep_atom; Netsys_mem.Copy_custom_int; 
       Netsys_mem.Copy_bigarray]
      x
  else
    x


let with_value_n heap find process =
  dlog "with_value";
  let l, k_list =
    with_lock heap
      (fun () ->
	 let l = find() in
	 let k_list = List.map (fun x -> set_tmp_root heap x) l in
	 l, k_list
      ) in
  dlog "with_value: process";
  let y = process l in
  (* We need the lock again *)
  with_lock heap
    (fun () ->
       List.iter (release_tmp_root heap) k_list
    );
  dlog "with_value: returning";
  y

let with_value heap find process =
  with_value_n
    heap
    (fun () -> [find()])
    (function [x] -> process x | _ -> assert false)

let with_value_2 heap (find : unit -> ('t1 * 't2)) process =
  with_value_n
    heap
    (fun () -> 
       let (x1,x2) = find() in
       [ Obj.repr x1; Obj.repr x2 ]
    )
    (function
       | [x1; x2] -> process ((Obj.obj x1 : 't1), (Obj.obj x2 : 't2))
       | _ -> assert false
    )

let with_value_3 heap find process =
  with_value_n
    heap
    (fun () -> 
       let (x1,x2,x3) = find() in
       [ Obj.repr x1; Obj.repr x2; Obj.repr x3 ]
    )
    (function
       | [x1; x2; x3] -> process ((Obj.obj x1), (Obj.obj x2), (Obj.obj x3))
       | _ -> assert false
    )

let with_value_4 heap find process =
  with_value_n
    heap
    (fun () -> 
       let (x1,x2,x3,x4) = find() in
       [ Obj.repr x1; Obj.repr x2; Obj.repr x3; Obj.repr x4 ]
    )
    (function
       | [x1; x2; x3; x4] -> 
	   process ((Obj.obj x1), (Obj.obj x2), (Obj.obj x3), (Obj.obj x4))
       | _ -> assert false
    )

let with_value_5 heap find process =
  with_value_n
    heap
    (fun () -> 
       let (x1,x2,x3,x4,x5) = find() in
       [ Obj.repr x1; Obj.repr x2; Obj.repr x3; Obj.repr x4; Obj.repr x5 ]
    )
    (function
       | [x1; x2; x3; x4; x5] -> 
	   process
	     ((Obj.obj x1), (Obj.obj x2), (Obj.obj x3), (Obj.obj x4),
	      (Obj.obj x5))
       | _ -> assert false
    )


let root heap =
  heap.heap_value


let dummy_mem =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout bytes_per_word


let minimum_size x =
  if Obj.is_block (Obj.repr x) then
    let (_, n) =
      Netsys_mem.init_value
	dummy_mem 0 x
	[ Netsys_mem.Copy_simulate; Netsys_mem.Keep_atom; 
	  Netsys_mem.Copy_custom_int; Netsys_mem.Copy_bigarray
	] in
    n + ((40 + fl_size + n_roots) * bytes_per_word)
      (* this is just an estimate *)
  else
    ((40 + fl_size + n_roots) * bytes_per_word)


let destroy heap =
  let c = Netmcore_mempool.sem_container heap.heap_pool in
  let ext = ref (Some heap.heap_ext) in
  let first = ref true in
  while !ext <> None do
    match !ext with
      | Some x ->
	  (* Get the [next] block now, because [x] is deleted *)
	  let next =
	    if x.ext_next = no_ext_block then
	      None
	    else
	      Some(ext_block heap x.ext_next) in
	  if not !first then
	    shrink_heap heap x;
	  ext := next;
	  first := false
      | None -> assert false
  done;
  let sem = Netsys_sem.as_sem c heap.heap_sem 0 in
  Netsys_sem.sem_destroy c sem;
  let heap_mem =
    Netsys_mem.grab heap.heap_ext.ext_addr heap.heap_ext.ext_size in
  Netmcore_mempool.free_mem heap.heap_pool heap_mem


let pool heap =
  heap.heap_pool

let mut_pool mut =
  pool (mut.heap)

let sem_container heap =
  Netmcore_mempool.sem_container heap.heap_pool

let mut_sem_container mut =
  sem_container (mut.heap)


let create_sem_mem pool_id =
  let m =
    Bigarray.Array1.create
      Bigarray.char Bigarray.c_layout Netsys_sem.sem_size in
  let c = Netmcore_mempool.sem_container pool_id in
  ignore(Netsys_sem.sem_init c m 0 true 1);
  m

let create_heap pool size rootval_orig =
  if not (Obj.is_block (Obj.repr rootval_orig)) then
    failwith "Netmcore_heap.create_heap: the root element is not a block";
  let heap_mem = Netmcore_mempool.alloc_mem pool size in
  try
    let heap_ext_orig =
      { ext_prev = no_ext_block;
	ext_next = no_ext_block;
	ext_addr = Netsys_mem.memory_address heap_mem;
	ext_size = Bigarray.Array1.dim heap_mem;
	ext_start = 0;   (* fixed later *)
	ext_end = Bigarray.Array1.dim heap_mem
      } in
    let heap_orig =
      { heap_sem = create_sem_mem pool;
	heap_pool = pool;
	heap_value = Obj.obj null_obj;
	heap_ext = heap_ext_orig;
	heap_fl = Array.make fl_size null_obj;
	heap_roots = Array.make n_roots null_obj;
	(* FIXME: the initial roots array should better be allocated in the
	   value area 
	 *)
      } in
    let p = ref 0 in
    Netsys_mem.blit_string_to_memory magic 0 heap_mem !p magic_len;
    p := !p + magic_len;
    let p_hoffs = !p in
    p := !p + 8;
    let (voffs, n) = 
      Netsys_mem.init_value heap_mem !p heap_orig
	[Netsys_mem.Copy_bigarray; Netsys_mem.Copy_custom_int; 
	 Netsys_mem.Keep_atom] in
    let hoffs_s =
      Netnumber.HO.int8_as_string (Netnumber.int8_of_int voffs) in
    Netsys_mem.blit_string_to_memory hoffs_s 0 heap_mem p_hoffs 8;
    p := !p + n;
    let heap = (Netsys_mem.as_value heap_mem voffs : _ heap) in
    heap.heap_ext.ext_start <- !p;
    add_to_fl heap heap_mem !p (heap.heap_ext.ext_end - !p);
    let mut = create_mutator heap in
    let rootval = add mut rootval_orig in
    heap.heap_value <- rootval;
    heap.heap_roots.(0) <- Obj.repr rootval;
    heap
  with
    | error ->
	Netmcore_mempool.free_mem pool heap_mem;
	raise error


