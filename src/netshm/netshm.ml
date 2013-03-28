(* $Id: netshm.ml 1742 2012-02-29 17:39:17Z gerd $ *)

(* FILE FORMAT:
 *
 * Page 0 is called the master page, and is usally not modified:
 *
 * 0/0: File magic 0
 * 0/1: File magic 1
 * 0/2: File magic 2
 * 0/3: File magic 3
 * 0/4: Base_shm version number
 * 0/5: Pointer to descriptor page
 *
 * The descriptor page [d] contains global data that must be frequently
 * updated:
 *
 * d/0: Descriptor magic
 * d/1: Total number of pages
 * d/2: Size of the hash table S (used plus free entries)
 * d/3: Number of used entries in the hash table
 * d/4: Total number of bindings
 * d/5: Pointer to the first free content page (head of free list)
 * d/6: Number of free content pages
 * d/7: unused
 * d/8: Pointer to first extent of hash table
 * d/9: Size of first extent of hash table in pages
 * d/10: Pointer to first extent of hash table or 0 if unused
 * d/11: Size of first extent of hash table in pages or 0 if unused
 * ...
 * d/38: Pointer to 16th extent of hash table or 0 if unused
 * d/39: Size of first extent of hash table in pages or 0 if unused
 *
 * The hash table is the array concatenation of 1 to 16 extents. Extents
 * are added when the hash table is resized. Extents are contiguous 
 * sequences of pages.
 *
 * The hash table array consists of S cells. Every cell has four words:
 *
 * 0: Key
 * 1: Pointer to content page. 
 *    0 means: Unused cell
 *   -1 means: Used cell but there is currenty no binding
 * 2: Spare word used for resizing
 * 3: Spare word used for resizing
 *
 * Every hash table page contains P/4-1 cells (when P is the number of
 * words per page). The four first words of the page have a special meaning:
 *
 * 0: Hash table page magic
 * 1: Sequential number of the page in the table
 * 2: unused
 * 3: unused
 * 
 * Used content pages have this layout:
 *
 * c/0: Content page magic
 * c/1: Key
 * c/2: Pointer to next content page for this entry (or 0)
 * c/3: Pointer to first content page of next binding for this key, or 0
 * c/4: Position where the value begins (which word)
 * c/5: Length of the value in words
 * c/6 - P-1: Content words
 *
 * Follow-up content pages:
 *
 * c'/0: Follow-up content page magic
 * c'/1: Key
 * c'/2: Pointer to next content page for this entry (or 0)
 * c'/3: Sequence number of this content page
 * c'/4 - P-1: Content words
 *
 * The position in c/4 is relative to the concatenated content
 * areas, i.e. position 0 is c/6 etc.
 *
 * Free content pages:
 *
 * f/0: Free page magic
 * f/1: Pointer to next free page or 0
 *
 * LOCKING
 *
 * Locking is page-wise.
 *
 * A Lock of the descriptor page is also interpreted as a lock for the
 * whole hash table.
 *
 * A lock of the first content page is also interpreted as a lock of
 * the remaining content pages of the entry.
 *
 * - Reading a single entry:
 * 
 *   1. Read-lock descriptor page
 *   2. Look up entry
 *   3. Read-lock content page
 *   4, Unlock descriptor page
 *   5. Read content
 *   6. Unlock content page
 *
 * - Reading all bindings:
 *
 *   1. Read-lock descriptor page
 *   2. Look up entry
 *   3. Read-lock 1st content page
 *   4, Unlock descriptor page
 *   5. Read 1st content
 *   6. Lock 2nd content page
 *   7. Unlock 1st content page
 *   8. Read 2nd content
 *   9. Lock 3rd content page
 *  10. Unlock 2nd content page
 *     ...
 *   N. Unlock n-th content page
 *
 * - Adding a single entry:
 *
 *   1. Write-lock descriptor page
 *   2. If necessary, do a resize pass (see below)
 *   4. Find entry in hash table and fill it
 *   4. Find free content page(s) and fill it/them
 *   5. Unlock descriptor page
 *
 * - Removing a single entry:
 * 
 *   1. Write-lock descriptor page
 *   2. Modify hash table
 *   3. Write-lock old content page
 *   4. Make the old content page a free page
 *   5. Unlock descriptor page
 *   6. Unlock content page
 *)

open Printf

module Int = struct
  type t = int
  let compare = (Pervasives.compare : int -> int -> int)
end

module IntMap = Map.Make(Int)

(**********************************************************************)
(* shm_descr                                                          *)
(**********************************************************************)

type shm_descr =
    [ `POSIX of string * Unix.file_descr * bool ref
    | `File of string * Unix.file_descr * bool ref
    ]
      (* (name, fd, is_open) *)

type shm_type = [ `POSIX | `File ]

let supported_types =
  ( if Netsys.have_posix_shm() then [ `POSIX ] else [] )
  @ [ `File ]

type shm_name = [ `POSIX of string | `File of string ]

let shm_type_of_name =
  function
    | `POSIX _ -> `POSIX
    | `File _ -> `File

let open_shm name flags perm =
  match name with
    | `File n ->
	let fd = Unix.openfile n flags perm in
	`File(n, fd, ref true)
    | `POSIX n ->
	let flags' =
	  List.flatten
	    (List.map
	       (function
		  | Unix.O_RDONLY -> [ Netsys.SHM_O_RDONLY ]
		  | Unix.O_RDWR   -> [ Netsys.SHM_O_RDWR ]
		  | Unix.O_CREAT  -> [ Netsys.SHM_O_CREAT ]
		  | Unix.O_EXCL   -> [ Netsys.SHM_O_EXCL ]
		  | Unix.O_TRUNC  -> [ Netsys.SHM_O_TRUNC ]
		  | Unix.O_WRONLY ->
		      invalid_arg "Netsys.open_shm: O_WRONLY not supported"
		  | _ -> 
		      []
	       )
	       flags) in
	let fd = Netsys.shm_open n flags' perm in
	`POSIX(n, fd, ref true)


let rndsrc = Random.State.make_self_init()


let chars =
  [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; 
     '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F';
     'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N';
     'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V';
     'W'; 'X'; 'Y'; 'Z'
  |]
    (* We do not include lower case chars because there are case-insensitive
     * file systems
     *)


let create_unique_shm name perm =
  let inst_string n =
    (* Replace 'X' with random chars *)
    let n' = String.copy n in
    for k = 0 to String.length n - 1 do
      if n'.[ k ] = 'X' then
	n'.[ k ] <- chars.( Random.State.int rndsrc (Array.length chars) )
    done;
    n'
  in

  let rec create iter =
    if iter = 1000 then
      failwith "Netshm.create_unique_shm: Unable to generate name for shared memory object";
    let name' =
      match name with
	| `File n ->
	    `File(inst_string n)
	| `POSIX n ->
	    `POSIX(inst_string n) in
    try
      open_shm name' [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL ] perm
    with
      | Unix.Unix_error(Unix.EEXIST,_,_) ->
	  create (iter+1)
  in

  create 0


let name_of_shm =
  function
    | `File(n,_,_) -> `File n
    | `POSIX(n,_,_) -> `POSIX n


let close_shm =
  function
    | `File(_,fd,is_open) -> if !is_open then Unix.close fd; is_open := false
    | `POSIX(_,fd,is_open) -> if !is_open then Unix.close fd; is_open := false


let unlink_shm =
  function
    | `File n -> Unix.unlink n
    | `POSIX n -> Netsys.shm_unlink n



let chmod_shm_fd fd is_open perm =
  if not !is_open then
    failwith "Netshm.chmod_shm: descriptor is not open";
  ( try Unix.fchmod fd perm
    with Unix.Unix_error(Unix.EINVAL,_,_) -> ()
      (* OSX seems to throw EINVAL here *)
  )

let chmod_shm =
  function
    | `File(_,fd,is_open) -> chmod_shm_fd fd is_open
    | `POSIX(_,fd,is_open) -> chmod_shm_fd fd is_open


let chown_shm_fd fd is_open uid gid =
  if not !is_open then
    failwith "Netshm.chown_shm: descriptor is not open";
  Unix.fchown fd uid gid


let chown_shm =
  function
    | `File(_,fd,is_open) -> chown_shm_fd fd is_open
    | `POSIX(_,fd,is_open) -> chown_shm_fd fd is_open


let size_of_shm_fd fd is_open =
  if not !is_open then
    failwith "Netshm.size_of_shm: descriptor is not open";
  let n64 =
    (Unix.LargeFile.fstat fd).Unix.LargeFile.st_size in
  if n64 > Int64.of_int max_int then
    failwith "Netshm.size_of_shm: Shared memory object too large";
  Int64.to_int n64


let size_of_shm =
  function
    | `File(_,fd,is_open) -> size_of_shm_fd fd is_open
    | `POSIX(_,fd,is_open) -> size_of_shm_fd fd is_open


let resize_shm_fd fd is_open n =
  if not !is_open then
    failwith "Netshm.resize_shm: descriptor is not open";
  Unix.LargeFile.ftruncate fd (Int64.of_int n)


let resize_shm =
  function
    | `File(_,fd,is_open) -> resize_shm_fd fd is_open
    | `POSIX(_,fd,is_open) -> resize_shm_fd fd is_open


let map_int32matrix_fd fd is_open rows cols =
  Bigarray.Array2.map_file
    fd Bigarray.int32 Bigarray.c_layout true rows cols

let map_int32matrix =
  function
    | `File(_,fd,is_open) -> map_int32matrix_fd fd is_open
    | `POSIX(_,fd,is_open) -> map_int32matrix_fd fd is_open

let dummy_int32matrix() =
  Bigarray.Array2.create Bigarray.int32 Bigarray.c_layout 0 0

(**********************************************************************)
(* locking                                                            *)
(**********************************************************************)

type locking_method =
    [ `No_locking | `Record_locking ]


let best_locking_method _ = `Record_locking

type lock_type = Read | Write

exception Deadlock


(* record_locking_descr represents the locking requirements. rld_table
 * maps pages to a list of locking requirements for the pages. The
 * strongest requirement is passed through to the lockf interface.
 *)

type record_locking_descr =
    { rld_sd : shm_descr;
      mutable rld_table : (lock_type list ref) IntMap.t;
      rld_pagesize : int;
    }


let rec rl_do_lock rld page lt =
  let fd, is_open =
    match rld.rld_sd with
      | `POSIX(_,fd,is_open) -> fd, is_open
      | `File(_,fd,is_open) -> fd, is_open in
  if not !is_open then
    failwith "Netshm: Shared memory object is not open";
  try
    ignore(Unix.lseek fd (page * rld.rld_pagesize) Unix.SEEK_SET);
    Unix.lockf fd (if lt = Read then Unix.F_RLOCK else Unix.F_LOCK) 1;
  with
    | Unix.Unix_error(Unix.EDEADLK,_,_) ->
	raise Deadlock
    | Unix.Unix_error(Unix.EINTR,_,_) ->
	rl_do_lock rld page lt


let rl_lock rld page lt =
  try
    let lt_list = IntMap.find page rld.rld_table in
    ( match lt with
	| Read -> lt_list := Read :: !lt_list
	| Write ->
	    if not(List.mem Write !lt_list) then
	      rl_do_lock rld page Write;
	    lt_list := Write :: !lt_list
    )
  with
    | Not_found ->
	rl_do_lock rld page lt;
	rld.rld_table <- IntMap.add page (ref [ lt ]) rld.rld_table


let rl_unlock rld page =
  (* releases the last (most recent) lock requirement for [page] *)
  let fd, is_open =
    match rld.rld_sd with
      | `POSIX(_,fd,is_open) -> fd, is_open
      | `File(_,fd,is_open) -> fd, is_open in
  if not !is_open then
    failwith "Netshm: Shared memory object is not open";
  try
    let lt_list = IntMap.find page rld.rld_table in
    ( match !lt_list with
	| [ _ ] ->
	    (* release lock entirely *)
	    ignore(Unix.lseek fd (page * rld.rld_pagesize) Unix.SEEK_SET);
	    Unix.lockf fd Unix.F_ULOCK 1;
	    rld.rld_table <- IntMap.remove page rld.rld_table;
	| Read :: lt_list' ->
	    lt_list := lt_list'
	| Write :: lt_list' ->
	    if not(List.mem Write lt_list') then
	      rl_do_lock rld page Read;   (* downgrade from Write to Read *)
	    lt_list := lt_list'
	| [] ->
	    assert false
    )
  with
    | Not_found ->
	()

type locking_descr =
    [ `No_locking
    | `Record_locking of record_locking_descr
    ]


let create_locking_descr sd pagesize lm =
  match lm with
    | `No_locking -> 
	`No_locking
    | `Record_locking -> 
	`Record_locking
	  { rld_sd = sd;
	    rld_table = IntMap.empty;
	    rld_pagesize = pagesize;
	  }


(* A groupable locking descriptor has the property that one can wrap
 * several operations as a "group". The locks are not released while
 * the operations of the group are executed. This has the effect that
 * the group is executed atomically.
 *)

type groupable_locking_descr =
    { gld : locking_descr;
      mutable gld_groups : int;
      mutable gld_deferred_unlocks : int list
    }


let gld_lock gld p ltype =
  ( match gld.gld with
      | `Record_locking rld ->
	  rl_lock rld p ltype;
      | `No_locking ->
	  ()
  );
  if gld.gld_groups > 0 then
    gld.gld_deferred_unlocks <- p :: gld.gld_deferred_unlocks
;;


let gld_unlock gld p =
  if gld.gld_groups = 0 then (
    match gld.gld with
      | `Record_locking rld ->
	  rl_unlock rld p
      | `No_locking ->
	  ()
  );
;;


let gld_start gld =
  gld.gld_groups <- gld.gld_groups + 1


let gld_end gld =
  gld.gld_groups <- gld.gld_groups - 1;
  if gld.gld_groups = 0 then (
    List.iter
      (fun p ->
	 gld_unlock gld p
      )
      gld.gld_deferred_unlocks;
    gld.gld_deferred_unlocks <- []
  )


let create_gld sd pagesize lm =
  { gld = create_locking_descr sd pagesize lm;
    gld_groups = 0;
    gld_deferred_unlocks = []
  }


(**********************************************************************)
(* shm_table                                                          *)
(**********************************************************************)

type shm_table = 
    { sd : shm_descr;
      lm : groupable_locking_descr;
      mutable mem : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t;
      pagesize : int;   (* in words! *)
      dpage : int;  (* descriptor page *)
      mutable self_locked : bool;
          (* Whether locked against mutations by this process *)
    }

type int32_array = 
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

exception Corrupt_file of string
exception Next
exception Break

let file_magic1 = 0xde871209l
let file_magic2 = 0xde881209l
let file_magic3 = 0xde871309l
let file_magic4 = 0xde87120al

let descriptor_magic = 0x6712DE9Fl
let content1_magic   = 0x12DE9F67l
let content2_magic   = 0xDE9F6712l
let hash_table_magic = 0x9F6712DEl
let free_page_magic  = 0xf3eef3eel


let to_int =
  match Sys.word_size with
    | 32 -> 
	let max32 = Int32.of_int max_int in
	let min32 = Int32.of_int min_int in
	(fun n ->
	   if n > max32 || n < min32 then
	     raise(Corrupt_file "Integer too large");
	   Int32.to_int n
	)
    | 64 ->
	Int32.to_int
    | _ -> assert false
;;


let of_int =
  match Sys.word_size with
    | 32 -> 
	Int32.of_int
    | 64 ->
	let max32 = Int32.to_int Int32.max_int in
	let min32 = Int32.to_int Int32.min_int in
	(fun n ->
	   if n > max32 || n < min32 then
	     raise(Corrupt_file "Integer too large");
	   Int32.of_int n
	)
    | _ -> assert false
;;


let remap t pages =
  let mem = map_int32matrix t.sd pages t.pagesize in
  t.mem <- mem;
;;


let lock_page t p ltype =
  gld_lock t.lm p ltype
;;


let unlock_page t p =
  gld_unlock t.lm p
;;


let unlock_protect t l f arg =
  try f arg
  with
    | error -> List.iter (fun p -> unlock_page t p) l; raise error


let self_locked t f arg =
  let old = t.self_locked in
  t.self_locked <- true;
  try
    let r = f arg in
    t.self_locked <- old;
    r
  with
    | error ->
	t.self_locked <- old;
	raise error


let group t f arg =
  try
    gld_start t.lm;
    let r = f arg in
    gld_end t.lm;
    r
  with
    | error ->
	gld_end t.lm;
	raise error
      

let hash_cell t k =
  (* Returns the k-th hash cell as pair (page, position), or Not_found *)
  
  let entries_per_page = t.pagesize / 4 - 1 in

  let rec find_hash_cell ext k =
    if ext < 16 then (
      let pos = 2 * ext + 8 in
      let ext_page = t.mem.{ t.dpage, pos } in
      let ext_size = t.mem.{ t.dpage, pos+1 } in
      if ext_page = 0l || ext_size = 0l then raise Not_found;
      
      let ext_length = (to_int ext_size) * entries_per_page in
      if k < ext_length then
	let ext_offset = k / entries_per_page in
	let ext_pos = k mod entries_per_page in
	((to_int ext_page) + ext_offset, ext_pos * 4 + 4)
      else
	find_hash_cell (ext+1) (k - ext_length)
    )
    else raise Not_found
  in
  find_hash_cell 0 k
;;


let quad_probing =
  [| 0; 1; 4; 9; 16; 25; 36; 49; 64; 81 |]


let rec hash_lookup t key seq spare_flag =
  (* Finds [key] in the hash table and returns the entry as triple
   * (page, position, found). [found] indicates whether the [key]
   * was actually found. If not, the entry is the unused entry that
   * will store [key].
   *
   * seq is used to find the hash entry and must be initially 0l.
   *
   * spare_flag: If set, the spare entries are returned instead of the
   * normal ones.
   *)

  let remainder x y =
    let r = Int32.rem x y in
    if r >= 0l then
      r
    else
      Int32.add y r in

  let total_pages = t.mem.{ t.dpage, 1 } in
  let ht_size = t.mem.{ t.dpage, 2 } in
  let (ht_page, ht_pos) = 
    try
      let offset =
	(* The first ten positions in this probing sequence are the same as
         * for quadratic probing. Then we simply continue with linear probing.
         * This way, we get most of the good properties of quadratic hashing,
         * but there is no size restriction on the table.
         *)
	if seq <= 9l then
	  Int32.of_int quad_probing.( Int32.to_int seq )
	else
	  Int32.add 72l seq in
      hash_cell t (to_int (remainder (Int32.add key offset) ht_size))
    with
      | Not_found ->
	  raise(Corrupt_file "Too few hash table extents found") in

  if ht_page <= 0 || (of_int ht_page) >= total_pages then
    raise(Corrupt_file "Bad page number found");
  if ht_pos < 0 || ht_pos >= t.pagesize then
    raise(Corrupt_file "Bad page position found");

  let spare_offset = 
    if spare_flag then 2 else 0 in

  let ht_key = t.mem.{ ht_page, ht_pos+spare_offset } in
  let ht_ptr = t.mem.{ ht_page, ht_pos+spare_offset+1 } in
  
  if ht_ptr = 0l then 
    (ht_page, ht_pos, false)
  else
    if ht_key = key then
      (ht_page, ht_pos, true)
    else
      hash_lookup t key (Int32.succ seq) spare_flag
;;


let alloc_hash_extent t seq n =
  let q_ht = (t.pagesize-4) / 4 in  (* entries per ht page *)
  let total_pages = t.mem.{ t.dpage, 1 } in
  let total_pages' = to_int total_pages + n in
  remap t total_pages';
  t.mem.{ t.dpage, 1 } <- of_int total_pages';
  t.mem.{ t.dpage, 2 } <- Int32.add t.mem.{ t.dpage, 2 } (of_int (n * q_ht));
  for k = 0 to n-1 do
    let p = to_int total_pages + k in
    t.mem.{ p, 0 } <- hash_table_magic;
    t.mem.{ p, 1 } <- of_int (seq + k);
    t.mem.{ p, 2 } <- 0l;
    t.mem.{ p, 3 } <- 0l;
    for k = 0 to q_ht - 1 do
      t.mem.{ p, 0 + 4*k } <- 0l;
      t.mem.{ p, 1 + 4*k } <- 0l;
      t.mem.{ p, 2 + 4*k } <- 0l;
      t.mem.{ p, 3 + 4*k } <- 0l;
    done
  done;
  to_int total_pages
;;


let add_hash_extent t =
  (* The new extent is twice as large as the largest extent *)

  let rec loop count largest ext =
    if ext < 16 then (
      let pos = 2 * ext + 8 in
      let ext_page = t.mem.{ t.dpage, pos } in
      let ext_size = t.mem.{ t.dpage, pos+1 } in
      if ext_page = 0l || ext_size = 0l then (
	(* found the empty slot *)
	let n = 2 * largest in
	let pg = alloc_hash_extent t count n in
	t.mem.{ t.dpage, pos } <- of_int pg;
	t.mem.{ t.dpage, pos+1 } <- of_int n;
      )
      else (
	loop (count + to_int ext_size) (max (to_int ext_size) largest) (ext+1)
      )
    )
    else
      failwith "add: cannot add further hash table extent"
  in
  loop 0 0 0
;;


let iter_hash_table t f =
  let q_ht = (t.pagesize-4) / 4 in  (* entries per ht page *)
  
  let rec loop ext =
    if ext < 16 then (
      let pos = 2 * ext + 8 in
      let ext_page = t.mem.{ t.dpage, pos } in
      let ext_size = t.mem.{ t.dpage, pos+1 } in
      if ext_page <> 0l && ext_size <> 0l then (
	for k = 0 to to_int ext_size - 1 do
	  let p = to_int ext_page + k in
	  for j = 0 to q_ht - 1 do
	    let q = 4 + j*4 in
	    f p q
	  done
	done;
	loop (ext+1)
      )
    ) in
  loop 0
;;


let refill_hash_table t =
  let fill_spare () =
    iter_hash_table
      t
      (fun p q ->
	 let ht_key = t.mem.{ p, q } in
	 let ht_ptr = t.mem.{ p, q+1 } in
	 if ht_ptr <> 0l && ht_ptr <> (-1l) then (
	   let (ht_page, ht_pos, ht_found) = 
	     hash_lookup t ht_key 0l true in
	   if ht_found then
	     raise(Corrupt_file "Duplicate key found in hash table");
	   t.mem.{ ht_page, ht_pos + 2 } <- ht_key;
	   t.mem.{ ht_page, ht_pos + 3 } <- ht_ptr;
	 )
      ) in

  let copy_back () =
    iter_hash_table
      t
      (fun p q ->
	 t.mem.{ p, q   } <- t.mem.{ p, q+2 };
	 t.mem.{ p, q+1 } <- t.mem.{ p, q+3 };
	 t.mem.{ p, q+2 } <- 0l;
	 t.mem.{ p, q+3 } <- 0l;
      ) in

  fill_spare ();
  copy_back ()
;;


let hash_lookup_for_addition t key =
  (* Returns (page, pos, inc).
   * inc is true if a new ht cell was allocated
   *)
  let (ht_page_0, ht_pos_0, ht_found_0) = hash_lookup t key 0l false in
  if ht_found_0 then 
    (ht_page_0, ht_pos_0, false)
  else (
    (* We have to allocate another cell of the hash table. Check whether
     * we have to resize the hash table!
     *)
    let n_used  = to_int t.mem.{ t.dpage, 3 } in
    let n_total = to_int t.mem.{ t.dpage, 2 } in
    if n_used * 2 > n_total then (
      (* more than 1/2 is used, so resize *)
      add_hash_extent t;
      refill_hash_table t;
	(* Note: total_pages has changed! *)
      let (ht_page_1, ht_pos_1, ht_found_1) = hash_lookup t key 0l false in
      assert(not ht_found_1);
      (ht_page_1, ht_pos_1, true)
    )
    else (
      (ht_page_0, ht_pos_0, true)
    )
  )
;;


(* Iterate over all bindings for a given key *)

type iterator =
    { i_table : shm_table;
      i_key : int32;
      i_total_pages : int32;
      mutable i_binding_pg : int;        (* 1st page of current binding *)
      mutable i_next_binding_pg : int;   (* 1st page of next binding *)
      mutable i_current_pg : int;        (* current page of current binding *)
      mutable i_current : int32_array;   (* same, the page itself *)
      mutable i_next_pg : int;           (* next page of current binding *)
      mutable i_seq : int;               (* sequence number *)
      mutable i_start_val : int;         (* start position of value fragment *)
      mutable i_len_val : int;           (* length of value fragment *)
      mutable i_rest_len_val : int;      (* rem. length of value after fragment *)
    }

(* Note: the iterator functions do not do any locking *)


let start_binding it pg =
  (* Go to the start of the binding at page [pg] *)
  let current = Bigarray.Array2.slice_left it.i_table.mem pg in
  let cp_magic      = current.{ 0 } in
  let cp_key        = current.{ 1 } in
  let cp_cpage'     = current.{ 2 } in
  let cp_cpage_next = current.{ 3 } in
  let cp_val_pos    = current.{ 4 } in
  let cp_val_len    = current.{ 5 } in
	 
  if cp_magic <> content1_magic then
    raise(Corrupt_file "Bad content1 magic");
  if cp_key <> it.i_key then
    raise(Corrupt_file "Wrong content page");
  if cp_cpage' < 0l || cp_cpage' >= it.i_total_pages then
    raise(Corrupt_file "Page pointer out of bounds");
  if cp_cpage_next < 0l || cp_cpage' >= it.i_total_pages then
    raise(Corrupt_file "Page pointer out of bounds");
  if cp_val_pos < 0l || cp_val_len < 0l then
    raise(Corrupt_file "Negative string pointers");
	 
  let ca_start = 6 in  (* first word of content area is at pos 6 *)
  let ca_len = it.i_table.pagesize - ca_start in
	 
  let cp_cpage'     = to_int cp_cpage' in
  let cp_cpage_next = to_int cp_cpage_next in
  let cp_val_pos    = to_int cp_val_pos in
  let cp_val_len    = to_int cp_val_len in
  
  if cp_val_pos >= ca_len then
    raise(Corrupt_file
	    "value does not start in first content page");

  let len_val = min ca_len cp_val_len in

  it.i_binding_pg <- pg;
  it.i_next_binding_pg <- cp_cpage_next;
  it.i_current_pg <- pg;
  it.i_current <- current;
  it.i_next_pg <- cp_cpage';
  it.i_seq <- 0;
  it.i_start_val <- cp_val_pos + ca_start;
  it.i_len_val <- len_val;
  it.i_rest_len_val <- cp_val_len - len_val;
;;


let create_iterator t key total_pages pg =
  (* Create an iterator for the list of bindings starting at page [pg] *)
  let current = Bigarray.Array2.slice_left t.mem pg in
  let it =
    { i_table = t;
      i_key = key;
      i_total_pages = total_pages;
      i_binding_pg = 0;
      i_next_binding_pg = 0;
      i_current_pg = 0;
      i_current = current;
      i_next_pg = 0;
      i_seq = 0;
      i_start_val = 0;
      i_len_val = 0;
      i_rest_len_val = 0;
    } in
  start_binding it pg;
  it
;;


let next_page it =
  (* Switches to the next page of the current binding. 
   * Precondition: it.i_next_pg <> 0
   *)
  let pg = it.i_next_pg in
  assert(pg <> 0);

  let current = Bigarray.Array2.slice_left it.i_table.mem pg in
  let cp'_magic  = current.{ 0 } in
  let cp'_key    = current.{ 1 } in
  let cp'_cpage' = current.{ 2 } in
  let cp'_seq    = current.{ 3 } in
	       
  if cp'_magic <> content2_magic then
    raise(Corrupt_file "Bad content2 magic");
  if cp'_key <> it.i_key then
    raise(Corrupt_file "Wrong content page");
  if cp'_cpage' < 0l || cp'_cpage' >= it.i_total_pages then
    raise(Corrupt_file "Page pointer out of bounds");
  if cp'_seq <> of_int (it.i_seq + 1) then
    raise(Corrupt_file "Bad sequence number");

  let ca_start = 4 in  (* first word of content area is at pos 4 *)
  let ca_len = it.i_table.pagesize - ca_start in
  let len_val = min ca_len it.i_rest_len_val in

  it.i_current_pg <- pg;
  it.i_current <- current;
  it.i_next_pg <- to_int cp'_cpage';
  it.i_seq <- it.i_seq + 1;
  it.i_start_val <- ca_start;
  it.i_len_val <- len_val;
  it.i_rest_len_val <- it.i_rest_len_val - len_val
;;


let next_binding it =
  start_binding it it.i_next_binding_pg
;;



(* the read_blocks implementation bases on iterators *)

let rec read_blocks_start t key f total_pages prev_pg pg () =
  lock_page t pg Read;
  if prev_pg > 0 then unlock_page t prev_pg;

  let followup =
    unlock_protect t [ pg ]
      (fun () ->
	 let it =
	   create_iterator t key total_pages pg in

	 read_blocks_extract it f pg
      )
      () in
  followup ()
  
and read_blocks_extract it f pg () =
  let followup =
    unlock_protect it.i_table [ pg ]
      (fun () ->
	 let val_frag = 
	   Bigarray.Array1.sub it.i_current it.i_start_val it.i_len_val in
	 try
	   self_locked it.i_table 
	     f (Some val_frag);

	   if it.i_next_pg <> 0 then (
	     next_page it;
	     read_blocks_extract it f pg
	   )
	   else (
	     self_locked it.i_table 
	       f None;
	     raise Next
	   )

	 with
	   | Next ->
	       read_blocks_next it f pg
	   | Break ->
	       (fun () -> 
		  unlock_page it.i_table pg)
      )
      () in
  followup()

and read_blocks_next it f prev_pg () =
  let pg = it.i_next_binding_pg in
  if pg <> 0 then (
    lock_page it.i_table pg Read;
    unlock_page it.i_table prev_pg;

    next_binding it;

    read_blocks_extract it f pg ()
  )
  else
    unlock_page it.i_table prev_pg
;;


let read_blocks t key f =
  lock_page t t.dpage Read;

  unlock_protect t [ t.dpage ]
    (fun () ->

       let total_pages = t.mem.{ t.dpage, 1 } in

       if Bigarray.Array2.dim1 t.mem <> (to_int total_pages) then
	 remap t (to_int total_pages);

       let (ht_page, ht_pos, ht_found) = hash_lookup t key 0l false in
       
       if ht_found then (
	 let ht_ptr = t.mem.{ ht_page, ht_pos+1 } in
	 if ht_ptr = (-1l) then (
	   (* key is unbound *)
	   unlock_page t t.dpage;
	   ()
	 )
	 else
	   if ht_ptr > 0l && ht_ptr < total_pages then (
	     (* The following call will unlock t.dpage *)
	     read_blocks_start
	       t key f total_pages t.dpage (to_int ht_ptr) ()
	   )
	   else
	     raise(Corrupt_file "Bad page number found")
       )
       else (
	 (* key is unbound *)
	 unlock_page t t.dpage;
	 ()
       )
    )
    ()
;;


let find_all t key =
  let l = ref [] in
  let v = ref [] in
  let v_size = ref 0 in
  read_blocks t key 
    (fun val_frag_opt ->
       match val_frag_opt with
	 | Some val_frag ->
	     v := val_frag :: !v;
	     v_size := !v_size + Bigarray.Array1.dim val_frag
	 | None ->
	     let v_total = 
	       Bigarray.Array1.create 
		 Bigarray.int32 Bigarray.c_layout !v_size in
	     List.iter
	       (fun val_frag ->
		  let len = Bigarray.Array1.dim val_frag in
		  v_size := !v_size - len;
		  Bigarray.Array1.blit
		    val_frag
		    (Bigarray.Array1.sub v_total !v_size len)
	       )
	       !v;
	     assert(!v_size = 0);
	     l := v_total :: !l;
	     v := []
    );
  List.rev !l
;;


exception Done of int32_array

let find t key =
  let v = ref [] in
  let v_size = ref 0 in
  try
    read_blocks t key 
      (fun val_frag_opt ->
	 match val_frag_opt with
	   | Some val_frag ->
	       v := val_frag :: !v;
	       v_size := !v_size + Bigarray.Array1.dim val_frag
	   | None ->
	       (* Important: we must concatenate the fragments while
                * the binding is read-locked!
                *)
	       let v_total = 
		 Bigarray.Array1.create 
		   Bigarray.int32 Bigarray.c_layout !v_size in
	       List.iter
		 (fun val_frag ->
		    let len = Bigarray.Array1.dim val_frag in
		    v_size := !v_size - len;
		    Bigarray.Array1.blit
		      val_frag
		      (Bigarray.Array1.sub v_total !v_size len)
		 )
		 !v;
	       assert(!v_size = 0);
	       raise (Done v_total)
      );
    raise Not_found
  with
    | Done v_total ->
	v_total
;;


let mem t key =
  try
    read_blocks t key 
      (fun _ -> raise Exit);
    false
  with
    | Exit ->
	true
;;


let iter_keys f t =
  lock_page t t.dpage Read;

  unlock_protect t [ t.dpage ]
    (fun () ->

       let total_pages = t.mem.{ t.dpage, 1 } in

       if Bigarray.Array2.dim1 t.mem <> (to_int total_pages) then
	 remap t (to_int total_pages);

       iter_hash_table
	 t
	 (fun p q ->
	    let ht_key = t.mem.{ p, q } in
	    let ht_ptr = t.mem.{ p, q+1 } in
	    if ht_ptr <> 0l && ht_ptr <> (-1l) then
	      self_locked t 
		f ht_key
	 )
    )
    ();

  unlock_page t t.dpage 
;;


let iter f t =
  
  let v = ref [] in
  let v_size = ref 0 in

  let reassemble key val_frag_opt =
    match val_frag_opt with
      | Some val_frag ->
	  v := val_frag :: !v;
	  v_size := !v_size + Bigarray.Array1.dim val_frag
      | None ->
	  let v_total = 
	    Bigarray.Array1.create 
	      Bigarray.int32 Bigarray.c_layout !v_size in
	     List.iter
	       (fun val_frag ->
		  let len = Bigarray.Array1.dim val_frag in
		  v_size := !v_size - len;
		  Bigarray.Array1.blit
		    val_frag
		    (Bigarray.Array1.sub v_total !v_size len)
	       )
	       !v;
	     assert(!v_size = 0);
	     v := [];
	     f key v_total
  in

  lock_page t t.dpage Read;

  unlock_protect t [ t.dpage ]
    (fun () ->

       let total_pages = t.mem.{ t.dpage, 1 } in

       if Bigarray.Array2.dim1 t.mem <> (to_int total_pages) then
	 remap t (to_int total_pages);

       iter_hash_table
	 t
	 (fun p q ->
	    let ht_key = t.mem.{ p, q } in
	    let ht_ptr = t.mem.{ p, q+1 } in
	    if ht_ptr <> 0l && ht_ptr <> (-1l) then (
	      read_blocks_start
		t ht_key (reassemble ht_key) total_pages 0 (to_int ht_ptr) () 
	    )
	 )
    )
    ();

  unlock_page t t.dpage 
;;


let fold f t x0 =
  let acc = ref x0 in
  iter
    (fun key v ->
       acc := f key v !acc
    )
    t;
  !acc
;;


let length t =
  lock_page t t.dpage Read;

  let n =
    unlock_protect t [ t.dpage ]
      (fun () ->
	 to_int t.mem.{ t.dpage, 4 } )
      () in

  unlock_page t t.dpage;
  n
;;


let get_page_from_free_list t =
  (* Assumes that there is a page! *)
  let p = t.mem.{ t.dpage, 5 } in
  if p = 0l then
    raise(Corrupt_file "Free list is empty when it should not be empty");

  let p = to_int p in
  
  if t.mem.{ p, 0 } <> free_page_magic then
    raise(Corrupt_file "Bad free page magic");

  let p' = t.mem.{ p, 1} in
  t.mem.{ t.dpage, 5 } <- p';
  t.mem.{ t.dpage, 6 } <- Int32.pred t.mem.{ t.dpage, 6 };

  p
;;


let add t key v =
  if t.self_locked then
    failwith "Netshm: Cannot modify table locked by caller";

  lock_page t t.dpage Write;
  
  unlock_protect t [ t.dpage ]
    (fun () ->

       let total_pages = t.mem.{ t.dpage, 1 } in

       if Bigarray.Array2.dim1 t.mem <> (to_int total_pages) then
	 remap t (to_int total_pages);

       let (ht_page, ht_pos, ht_inc) =
	 hash_lookup_for_addition t key in

       let old_cpage = t.mem.{ ht_page, ht_pos+1 } in
       let old_cpage = 
	 if old_cpage = (-1l) then 0l else old_cpage in

       (* How many pages do we need? *)
       let words = Bigarray.Array1.dim v in
       let pages = (words + 2 - 1) / (t.pagesize - 4) + 1 in

       (* How many pages do we need to allocate? *)
       let pages_in_free_list = to_int t.mem.{ t.dpage, 6 } in
       let pages_to_allocate = max 0 (pages - pages_in_free_list) in

       (* Allocate pages and enter into free list: *)
       if pages_to_allocate > 0 then (
	 let total_pages = to_int t.mem.{ t.dpage, 1 } in
	 remap t (total_pages + pages_to_allocate);
	 t.mem.{ t.dpage, 1 } <- of_int (total_pages + pages_to_allocate);
	 for k = 0 to pages_to_allocate - 1 do
	   let p = total_pages + k in
	   t.mem.{ p, 0 } <- free_page_magic;
	   t.mem.{ p, 1 } <- t.mem.{ t.dpage, 5 };
	   t.mem.{ t.dpage, 5 } <- of_int p;
	 done;
	 t.mem.{ t.dpage, 6 } <- 
	   of_int (pages_in_free_list + pages_to_allocate)
       );

       (* MAYBE TODO: Unlock before writing to content pages *)

       (* Write content pages: *)
       let val_idx = ref 0 in
       let val_len = Bigarray.Array1.dim v in
       let cpage = get_page_from_free_list t in
       t.mem.{ cpage, 0 } <- content1_magic;
       t.mem.{ cpage, 1 } <- key;
       t.mem.{ cpage, 2 } <- 0l;    (* later updated if necessary *)
       t.mem.{ cpage, 3 } <- old_cpage;
       t.mem.{ cpage, 4 } <- 0l;
       t.mem.{ cpage, 5 } <- of_int val_len;
       
       let cur_cpage = ref cpage in
       let pos = ref 6 in
       let seq = ref 0 in
       while !val_idx < val_len do
	 if !pos >= t.pagesize then (
	   let cpage = get_page_from_free_list t in
	   incr seq;
	   t.mem.{ cpage, 0 } <- content2_magic;
	   t.mem.{ cpage, 1 } <- key;
	   t.mem.{ cpage, 2 } <- 0l;    (* later updated if necessary *)
	   t.mem.{ cpage, 3 } <- of_int !seq;
	   t.mem.{ !cur_cpage, 2 } <- of_int cpage;
	   cur_cpage := cpage;
	   pos := 4
	 );
	 let len = val_len - !val_idx in
	 let ca_len = t.pagesize - !pos in
	 let words = min len ca_len in
	 let mem_pg = Bigarray.Array2.slice_left t.mem !cur_cpage in
	 Bigarray.Array1.blit
	   (Bigarray.Array1.sub v !val_idx words)
	   (Bigarray.Array1.sub mem_pg !pos words);
	 
	 val_idx := !val_idx + words;
	 pos := !pos + words;
       done;

       (* Write hash table entry: *)
       t.mem.{ ht_page, ht_pos } <- key;
       t.mem.{ ht_page, ht_pos + 1 } <- of_int cpage;
       
       if ht_inc then
	 t.mem.{ t.dpage, 3 } <- Int32.succ t.mem.{ t.dpage, 3 };

       t.mem.{ t.dpage, 4 } <- Int32.succ t.mem.{ t.dpage, 4 };
    )
    ();
  
  (* Unlock: *)
  unlock_page t t.dpage
;;


let rec unalloc_pages_of_binding t cpage =
  let cpage' = to_int t.mem.{ cpage, 2 } in
  t.mem.{ cpage, 0 } <- free_page_magic;
  t.mem.{ cpage, 1 } <- t.mem.{ t.dpage, 5 };
  t.mem.{ t.dpage, 5 } <- of_int cpage;
  t.mem.{ t.dpage, 6 } <- Int32.succ t.mem.{ t.dpage, 6 };
  if cpage' <> 0 then (
    if t.mem.{ cpage', 0 } <> content2_magic then
      raise(Corrupt_file "Bad content2 magic");
    unalloc_pages_of_binding t cpage'
  )
;;


(* the write_blocks implementation bases on iterators *)

type write_op = 
    [ `Remove_binding ]

type ctrl_op =
    [ `Nop | write_op ]

type op_announcement =
    { op_remove_binding : bool }

type pointer_to_binding =
    [ `Prev_binding of int
    | `Hash_entry of int * int
    ]


let remove_binding t key ptr pg =
  let next_cpage = t.mem.{ pg, 3 } in
  
  ( match ptr with
      | `Hash_entry(ht_page, ht_pos) ->
	  t.mem.{ ht_page, ht_pos + 1 } <- (if next_cpage = 0l then
					      (-1l)
					    else
					      next_cpage);
      | `Prev_binding pg' ->
	  t.mem.{ pg', 3 } <- next_cpage;
	  
  );

  t.mem.{ t.dpage, 4 } <- Int32.pred t.mem.{ t.dpage, 4 };
  (* Note: mem.{t.dpage, 3}, i.e. the number of used entries, does not change,
   * even if the hash cell is set to (-1).
   *)
	      
  unalloc_pages_of_binding t pg;

  (* The new pointer to the next binding is the old after the deletion: *)
  ptr
;;


let write_op it ops ptr op =
  match op with
    | `Nop -> 
	`Prev_binding it.i_binding_pg
    | `Remove_binding ->
	if not ops.op_remove_binding then
	  failwith "Netshm.write_blocks: Cannot do write operation unless announced";
	remove_binding it.i_table it.i_key ptr it.i_binding_pg
;;


let rec write_blocks_start t ops key f total_pages (ptr : pointer_to_binding) pg () =
  (* ptr: If [`Prev_binding pg], there is a previous binding that starts on
   * page pg. If [`Hash_entry(pg,pos)] this is the first binding, and the
   * hash entry on page [pg] and [pos] points to it.
   *
   * locking: the current, and if `Remove_binding is announced in ops, 
   * the previous binding (if any) are write locked.
   * Additionally, t.dpage is locked over the whole time, too.
   *)
  lock_page t pg Write;

  let followup =
    unlock_protect t [ pg ]
      (fun () ->
	 let it =
	   create_iterator t key total_pages pg in

	 write_blocks_extract it ops f ptr pg `Nop
      )
      () in
  followup ()
  
and write_blocks_extract it ops f ptr pg old_op () =
  let locked =
    if ops.op_remove_binding then
      match ptr with
	| `Prev_binding pg' -> [ pg'; pg ]
	| `Hash_entry(_,_)  -> [ pg ] 
    else
      [ pg ]
  in
    
  let followup =
    unlock_protect it.i_table locked
      (fun () ->
	 let val_frag = 
	   Bigarray.Array1.sub it.i_current it.i_start_val it.i_len_val in
	 let op = ref old_op in
	 try
	   let new_op = 
	     self_locked it.i_table 
	       f (Some val_frag) in
	   if new_op <> `Nop then
	     op := new_op;

	   if it.i_next_pg <> 0 then (
	     next_page it;
	     write_blocks_extract it ops f ptr pg !op
	   )
	   else (
	     let new_op = 
	       self_locked it.i_table 
		 f None in
	     if new_op <> `Nop then
	       op := new_op;
	     raise Next
	   )
	 with
	   | Next ->
	       let ptr' =
		 write_op it ops ptr !op in
	       write_blocks_next it ops f ptr pg ptr'
	   | Break ->
	       let _ptr' =
		 write_op it ops ptr !op in
	       (fun () -> 
		  unlock_page it.i_table pg)
      )
      ()in
  followup()

and write_blocks_next it ops f prev_ptr prev_pg ptr' () =
  let pg = it.i_next_binding_pg in
  if pg <> 0 then (
    lock_page it.i_table pg Write;
    if ops.op_remove_binding then 
      ( match prev_ptr with
	  | `Prev_binding pg' -> unlock_page it.i_table pg'
	  | `Hash_entry(_,_)  -> ()
      )
    else
      unlock_page it.i_table prev_pg;

    next_binding it;

    write_blocks_extract it ops f ptr' pg `Nop ()
  )
  else (
    if ops.op_remove_binding then
      ( match prev_ptr with
	  | `Prev_binding pg' -> unlock_page it.i_table pg'
	  | `Hash_entry(_,_)  -> ()
      );
    unlock_page it.i_table prev_pg
  )
;;


let write_blocks t ops key f =
  let ops' =
    { op_remove_binding = List.mem `Remove_binding ops } in

  if t.self_locked then
    failwith "Netshm: Cannot modify table locked by caller";

  lock_page t t.dpage Write;

  unlock_protect t [ t.dpage ]
    (fun () ->

       let total_pages = t.mem.{ t.dpage, 1 } in

       if Bigarray.Array2.dim1 t.mem <> (to_int total_pages) then
	 remap t (to_int total_pages);

       let (ht_page, ht_pos, ht_found) = hash_lookup t key 0l false in
       
       if ht_found then (
	 let ht_ptr = t.mem.{ ht_page, ht_pos+1 } in
	 if ht_ptr = (-1l) then (
	   (* key is unbound *)
	   ()
	 )
	 else
	   if ht_ptr > 0l && ht_ptr < total_pages then (
	     let ptr = `Hash_entry(ht_page,ht_pos) in
	     write_blocks_start
	       t ops' key f total_pages ptr (to_int ht_ptr) ()
	   )
	   else
	     raise(Corrupt_file "Bad page number found")
       )
    )
    ();

  unlock_page t t.dpage;
;;


let remove t key =
  let first = ref true in
  write_blocks t [`Remove_binding] key
    (fun _ -> 
       if !first then (
	 first := false;
	 `Remove_binding
       )
       else
	 raise Break)
;;


let replace t key v =
  (* This is the trivial implementation... Not really what we want. Anything
   * better that reuses the pages of the old binding is a lot of work...
   *)
  group t
    (fun () ->
       remove t key;
       add t key v
    )
    ()
;;


let manage ?(pagesize = 256) ?init lm sd =
  if pagesize < 160 then
    failwith "open_table: minimum pagesize is 160";
  if pagesize mod 4 <> 0 then
    failwith "open_table: pagesize must be divisible by 4";

  let pagesize = pagesize/4 in

  if size_of_shm sd = 0 || init <> None then (
    (* re-initialize table *)
    let ld = create_gld sd pagesize lm in
    let t =
      { sd = sd;
	lm = ld;
	mem = dummy_int32matrix();
	pagesize = pagesize;
	dpage = 0;
	self_locked = false;
      } in
    let n = 
      match init with
	| None -> 1000
	| Some n -> n in
    let n_ht = n * 2 in   (* need 2 times more ht entries *)
    let q_ht = (pagesize-4) / 4 in  (* entries per ht page *)
    let p_ht = (n_ht - 1) / q_ht + 1 in
    (* number of hash table pages *)

    remap t 2;

    t.mem.{ 0, 0 } <- file_magic1;
    t.mem.{ 0, 1 } <- file_magic2;
    t.mem.{ 0, 2 } <- file_magic3;
    t.mem.{ 0, 3 } <- file_magic4;
    t.mem.{ 0, 4 } <- 1l;

    t.mem.{ 1, 0 } <- descriptor_magic;
    t.mem.{ 1, 1 } <- 2l;
    t.mem.{ 1, 2 } <- 0l;
    t.mem.{ 1, 3 } <- 0l;
    t.mem.{ 1, 4 } <- 0l;
    t.mem.{ 1, 5 } <- 0l;
    t.mem.{ 1, 6 } <- 0l;
    t.mem.{ 1, 7 } <- 0l;
    for k = 0 to 15 do
      t.mem.{ 1, 8 + 2*k } <- 0l;
      t.mem.{ 1, 9 + 2*k } <- 0l;
    done;

    let t = 
      { t with
	  dpage = 1
      } in

    let p = alloc_hash_extent t 0 p_ht in
    t.mem.{ 1, 8 } <- of_int p;
    t.mem.{ 1, 9 } <- of_int p_ht;

    t
  )
  else (
    (* manage existing table *)
    let ld = create_gld sd pagesize lm in
    let t =
      { sd = sd;
	lm = ld;
	mem = dummy_int32matrix();
	pagesize = pagesize;
	dpage = 0;
	self_locked = false;
      } in
    remap t 1;
    if (t.mem.{ 0, 0 } <> file_magic1 ||
	t.mem.{ 0, 1 } <> file_magic2 ||
	t.mem.{ 0, 2 } <> file_magic3 ||
	t.mem.{ 0, 3 } <> file_magic4)
    then
      raise(Corrupt_file "Bad file magic");

    let dpage = to_int t.mem.{ 0, 4 } in
    remap t (dpage+1);
    if t.mem.{dpage, 0} <> descriptor_magic then
      raise(Corrupt_file "Bad descriptor magic");
    let total_size = to_int t.mem.{ dpage, 1 } in
    remap t total_size;
    { t with
	dpage = dpage
    }
  )


(* Debug stuff: *)

let dump t =
  printf "<shm length=%d\n" (length t);
  iter
    (fun key v ->
       printf "  %ld => [| " key;
       let l = Bigarray.Array1.dim v in
       for k = 0 to l - 1 do
	 if k > 0 then printf "; ";
	 printf "%ld" v.{ k }
       done;
       printf " |]\n";
    )
    t;
  printf ">\n%!"
;;


let bigarray x =
  Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout 
    (Array.map Int32.of_int x)
;;


let memory t = t.mem
