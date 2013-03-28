(* $Id: netcamlbox.ml 1755 2012-03-24 20:18:30Z gerd $ *)

(* Format of the shm object:

   - Const: CAPACITY

     Number of message slots

   - Const: MSG_SIZE

     Max size of messages

   - Sem: NOTIFICATIONS

     This semaphore is increased by the sender to wake up the receiver,
     and decreased by the receiver.

   - Sem: FREE_SLOTS

     Number of free slots. Increased by the receiver on [delete].
     Decreased by the sender when waiting for space.

   - For each slot: SLOT_READABLE (one byte per slot).

     The number 1 if the slot is filled and has a readable message,
     or 0 if it is free or should be ignored by the receiver.

   - Array SLOT_LIST and number SPLIT_IDX:

     The array positions 0 .. SPLIT_IDX-1 contain the numbers of the
     filled slots. The array positions SPLIT_IDX .. CAPACITY-1 contain
     the numbers of the free slots.

     Sem SLOT_LOCK: mutex for SLOT_LIST and SPLIT_IDX.


  Sending:
   - sem_wait(FREE_SLOTS)
   - lock SLOT_LOCK
   - find a free slot, and move it to the part of the slot list with the
     filled slots
   - unlock SLOT_LOCK
   - copy the value to the chosen slot
   - SLOT_READABLE := 1
   - sem_post(NOTIFICATIONS)

  Receiving:
  1. Wait for message:
      - sem_wait(NOTIFICATIONS)
      - lock SLOT_LOCK
      - get the part of the slot list with the filled slots
      - unlock SLOT_LOCK
      - for each filled slot:
          - get the value of SLOT_READABLE. If it is 0 skip the slot.
          - if the slot was already reported to the user, skip the slot
          - otherwise include the slot in the result list

  2. Get message:
      - test SLOT_READABLE. If it is 1 read the message

  3. Delete the message:
      - Remove the message from the messages that "wait" already reported
      - SLOT_READABLE := 0
      - lock SLOT_LOCK
      - move the slot to the part of the slot list with the empty slots
      - unlock SLOT_LOCK
      - sem_post(FREE_SLOTS)


   FORMAT OF THE SHARED MEMORY OBJECT:

   byte 0: <header>
   byte <sem_offset>: <array of semaphores>
   byte <sl_offset>:  <slot list>
   byte <offset>:     first slot. Slots come one after the other, and
                      are aligned at 128 byte boundaries

   <header>: the ocaml representation of type [header]
   <array of semaphores>: the semaphores one after the other
   <slot list>: the ocaml representation of type [int array]

 *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netcamlbox" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netcamlbox" Debug.enable

let () =
  Netlog.Debug.register_module "Netcamlbox" Debug.enable



open Netsys_mem
open Printf

type camlbox_address = string

(* The header must not contain pointers, so it is relocatable *)
type header =
    { mutable address : int;    (* address lsr 1 *)
      mutable sem_offset : int; (* offset to semaphore area *)
      mutable sl_offset : int;  (* offset to slot_list *)
      mutable offset : int;     (* offset to first slot *)
      capacity : int;
      msg_size : int;
      mutable free_slots : int;    
      mutable split_idx : int;
      (* function pointers for custom operations: the header contains the
	 pointers the receiver uses.
       *)
      bigarray_custom_ops : int;
      int32_custom_ops : int;
      int64_custom_ops : int;
      nativeint_custom_ops : int;
    }

type semaphores =
    { s_notifications : Netsys_sem.anon_semaphore;
      s_free_slots : Netsys_sem.anon_semaphore;
      s_slot_lock : Netsys_sem.anon_semaphore ;
      slot_readable : int array;   (* pointer to the byte *)
    }

type 'a camlbox =
    { mem : memory;
      hdr : header;            (* pointing into mem *)
      slot_list : int array;   (* pointing into mem *)
      sem : semaphores;        (* pointing into mem *)
      is_new : bool array;     (* local *)
      addr : string;
      cont : Netsys_sem.container; (* local *)
    }

type 'a camlbox_sender = 'a camlbox

exception Empty
exception Message_too_big

let align = 128
  (* header and slots are aligned by this *)

let ws_bytes =   (* word size in bytes *)
    Sys.word_size / 8

let this_bigarray_custom_ops =
  Netsys_mem.get_custom_ops
    (Bigarray.Array1.create Bigarray.char Bigarray.c_layout 1)

let this_int32_custom_ops =
  Netsys_mem.get_custom_ops 0l

let this_int64_custom_ops =
  Netsys_mem.get_custom_ops 0L

let this_nativeint_custom_ops =
  Netsys_mem.get_custom_ops 0n

let ntoi n = (* for addresses *)
  Nativeint.to_int(Nativeint.shift_right n 1)

let iton i = (* for addresses *)
  Nativeint.shift_left (Nativeint.of_int i) 1


let create_header capacity msg_size =
  let dummy = Bigarray.Array1.create Bigarray.char Bigarray.c_layout  0 in
  let hdr =
    { address = 0;
      sem_offset = 0;
      sl_offset = 0;
      offset = 0;
      capacity = capacity;
      msg_size = msg_size;
      free_slots = capacity;
      split_idx = 0;
      bigarray_custom_ops = ntoi (snd this_bigarray_custom_ops);
      int32_custom_ops = ntoi (snd this_int32_custom_ops);
      int64_custom_ops = ntoi (snd this_int64_custom_ops);
      nativeint_custom_ops = ntoi (snd this_nativeint_custom_ops);
    } in
  let _, hdr_bytelen = init_value dummy 0 hdr [ Copy_simulate ] in
  hdr.sem_offset <- hdr_bytelen;
  let sem_bytelen = 3 * Netsys_sem.sem_size + capacity in
  let cap8 = sem_bytelen mod 8 in
  let sem_padding = if cap8 = 0 then 0 else 8 - cap8 in
  let slot_list = Array.init capacity (fun k -> k) in
  let _, sl_bytelen = init_value dummy 0 slot_list [ Copy_simulate ] in
  hdr.sl_offset <- hdr_bytelen + sem_bytelen + sem_padding;
  let bytelen = hdr_bytelen + sem_bytelen + sem_padding + sl_bytelen in
  hdr.offset <- (((bytelen-1) / align) + 1) * align;
  dlogr
    (fun () ->
       sprintf "create_header cap=%d msgsize=%d sem_offs=%d sl_offs=%d offs=%d"
         capacity msg_size hdr.sem_offset hdr.sl_offset hdr.offset
    );
  (hdr, slot_list)

let init_semaphores hdr mem cont =
  let offset = hdr.sem_offset in
  let init_sem k v =
    ignore(
      Netsys_sem.sem_init cont mem (offset+k*Netsys_sem.sem_size) true v) in
  init_sem 0 0;            (* notifications *)
  init_sem 1 hdr.capacity; (* free_slots *)
  init_sem 2 1;            (* slot_lock *)
  let b = 3 in
  let b_bytes = offset+b*Netsys_sem.sem_size in
  for k = 0 to hdr.capacity-1 do
    mem.{ b_bytes + k } <- '\000'
  done

let destroy_semaphores hdr mem cont =
  (* Must be called before shm is unmapped (only in the receiver) *)
  let offset = hdr.sem_offset in
  for k = 0 to 2 do
    let s = Netsys_sem.as_sem cont mem (offset+k*Netsys_sem.sem_size) in
    Netsys_sem.sem_destroy cont s
  done

let free_mem cont mem =
  let hdr =
    (as_value mem ws_bytes : header) in
  destroy_semaphores hdr mem cont

let get_semaphores hdr mem cont =
  let offset = hdr.sem_offset in
  let get_sem k =
    Netsys_sem.as_sem cont mem (offset+k*Netsys_sem.sem_size) in
  let semrec =
    { s_notifications = get_sem 0;          (* notifications *)
      s_free_slots = get_sem 1;             (* free_slots *)
      s_slot_lock = get_sem 2;              (* slot_lock *)
      slot_readable =
        Array.init hdr.capacity
          (fun k -> offset + 3*Netsys_sem.sem_size + k)
    } in
  dlog "get_semaphores";
  semrec


let mk_camlbox fn_name addr capacity msg_size cont f = 
  if not (Netsys_sem.have_anon_semaphores()) then
    invalid_arg (sprintf "%s (no anonymous semaphores)" fn_name);
  if capacity < 1 || capacity > Sys.max_array_length || msg_size < 1 then
    invalid_arg (sprintf "%s (bad params)" fn_name);
  if capacity > Netsys_sem.sem_value_max then
    invalid_arg (sprintf "%s (capacity exceeds sem_value_max)" fn_name);
  let slot_size = (((msg_size-1) / align) + 1) * align in
  if slot_size < msg_size then (* overflow *)
    invalid_arg (sprintf "%s (too large)" fn_name);
  let (hdr0, slot_list0) = create_header capacity msg_size in
  let shm_size = hdr0.offset + capacity * slot_size in
  if shm_size < hdr0.offset then (* overflow *)
    invalid_arg (sprintf "%s (too large)" fn_name);
  if (shm_size - hdr0.offset) / slot_size <> capacity then (* overflow *)
    invalid_arg (sprintf "%s (too large)" fn_name);
  let mem = f shm_size in
  value_area mem;
  hdr0.address <- ntoi (memory_address mem);
  let hdr_voffs, _ = init_value mem 0 hdr0 [] in
  let hdr = (as_value mem hdr_voffs : header) in
  init_semaphores hdr mem cont;
  let sl_voffs, _ = init_value mem hdr.sl_offset slot_list0 [] in
  let slot_list = (as_value mem sl_voffs : int array) in
  Gc.finalise (free_mem cont) mem;
  let sem = get_semaphores hdr mem cont in
  dlogr
    (fun () ->
       sprintf 
         "mk_camlbox fn_name=%s cap=%d msgsize=%d slot_size=%d shm_size=%d"
         fn_name capacity msg_size slot_size shm_size
    );
  { mem = mem;
    hdr = hdr;
    slot_list = slot_list;
    sem = sem;
    is_new = Array.make capacity true;
    addr = addr;
    cont = cont
  }

let format_camlbox addr fd capacity msg_size =
  if not (Netsys_posix.have_posix_shm()) then
    invalid_arg "format_camlbox (no POSIX shm)";
  if String.contains addr '/' then
    invalid_arg "format_camlbox (bad name)";

  let cont = Netsys_sem.container ("/" ^ addr) in
  mk_camlbox "format_camlbox" addr capacity msg_size cont
    (fun shm_size ->
       Unix.ftruncate fd shm_size;
       Netsys_mem.memory_map_file fd true shm_size
    )

let create_camlbox addr capacity msg_size =
  if not (Netsys_posix.have_posix_shm()) then
    invalid_arg "create_camlbox (no POSIX shm)";
  if String.contains addr '/' then
    invalid_arg "create_camlbox (bad name)";

  let cont = Netsys_sem.create_container ("/" ^ addr) in
  let box =
    mk_camlbox "create_camlbox" addr capacity msg_size cont
      (fun shm_size ->
         let shm =
	   Netsys_posix.shm_open
	     ("/" ^ addr)
	     [ Netsys_posix.SHM_O_RDWR;
	       Netsys_posix.SHM_O_CREAT;
	       Netsys_posix.SHM_O_EXCL
	     ]
	     0o600 in
         let mem = 
	   try
	     Unix.ftruncate shm shm_size;
	     let mem = Netsys_mem.memory_map_file shm true shm_size in
	     Unix.close shm;
	     mem
	   with
	     | error ->
	         Unix.close shm;
	         Netsys_sem.drop cont;
	         raise error in
         mem
      ) in
  dlog "create_camlbox returning";
  box


let unlink_camlbox addr =
  Netsys_posix.shm_unlink ("/" ^ addr);
  Netsys_sem.unlink ("/" ^ addr)


let camlbox_fd addr =
  Netsys_posix.shm_open
    ("/" ^ addr)
    [ Netsys_posix.SHM_O_RDWR ]
    0o600

let with_fd fd f =
  try
    let r = f fd in
    Unix.close fd;
    r
  with
    | error ->
	Unix.close fd;
	raise error

let dummy_header () =
  { address = 0;
    sem_offset = 0;
    sl_offset = 0;
    offset = 0;
    capacity = 0;
    msg_size = 0;
    split_idx = 0;
    free_slots = 0;
    bigarray_custom_ops = 0;
    int32_custom_ops = 0;
    int64_custom_ops = 0;
    nativeint_custom_ops = 0;
  }


let header_size = lazy(
  let dummy = Bigarray.Array1.create Bigarray.char Bigarray.c_layout  0 in
  let hdr = dummy_header () in
  let voffset, bytelen =
    init_value
      dummy 0 hdr 
      [ Copy_simulate ] in
  bytelen
)


let camlbox_open cont addr fd =
  (* does not initialize is_new! *)
  let hdr_size = Lazy.force header_size in
  let mini_mem = Netsys_mem.memory_map_file fd true hdr_size in
  let hdr0 = (as_value mini_mem ws_bytes : header) in
  let offset = hdr0.offset in
  let slot_size = (((hdr0.msg_size-1) / align) + 1) * align in
  let shm_size = offset + hdr0.capacity * slot_size in
  let mem = Netsys_mem.memory_map_file fd true shm_size in
  let hdr = (as_value mem ws_bytes : header) in
  let sl = (as_value mem (hdr.sl_offset+ws_bytes) : int array) in
  dlogr
    (fun () ->
       sprintf
         "camlbox_open hdr_size=%d offset=%d slot_size=%d shm_size=%d"
         hdr_size offset slot_size shm_size
    );
  { mem = mem;
    hdr = hdr;
    slot_list = sl;
    sem = get_semaphores hdr mem cont;
    is_new = [| |];
    addr = addr;
    cont = cont;
  }

let camlbox_addr box =
  box.addr

let camlbox_saddr box =
  box.addr

let camlbox_sender addr =
  let cont = Netsys_sem.container ("/" ^ addr) in
  with_fd (camlbox_fd addr) (camlbox_open cont addr)

let camlbox_sender_of_fd addr =
  let cont = Netsys_sem.container ("/" ^ addr) in
  camlbox_open cont addr

let camlbox_bcapacity box =
  box.hdr.capacity

let camlbox_scapacity = camlbox_bcapacity

let camlbox_capacity addr =
  let cont = Netsys_sem.container ("/" ^ addr) in
  with_fd
    (camlbox_fd addr)
    (fun fd ->
       let box = camlbox_open cont addr fd in
       camlbox_bcapacity box
    )

let camlbox_bmsg_size box =
  box.hdr.msg_size

let camlbox_smsg_size = camlbox_bmsg_size

let camlbox_msg_size addr =
  let cont = Netsys_sem.container ("/" ^ addr) in
  with_fd
    (camlbox_fd addr)
    (fun fd ->
       let box = camlbox_open cont addr fd in
       camlbox_bmsg_size box
    )

let camlbox_bmessages box =
  let free =
    try 
      Netsys_sem.sem_getvalue box.sem.s_free_slots
    with Unix.Unix_error(Unix.ENOSYS,_,_) -> (* OS X *)
      Netsys_sem.sem_wait box.sem.s_slot_lock Netsys_sem.SEM_WAIT_BLOCK;
      let free = box.hdr.free_slots in
      Netsys_sem.sem_post box.sem.s_slot_lock;
      free in
  box.hdr.capacity - free

let camlbox_smessages = camlbox_bmessages

let camlbox_messages addr =
  let cont = Netsys_sem.container ("/" ^ addr) in
  with_fd
    (camlbox_fd addr)
    (fun fd ->
       let box = camlbox_open cont addr fd in
       camlbox_bmessages box
    )


let is_slot_readable_nolock fn_name box k =
  let p = box.sem.slot_readable.(k) in
  box.mem.{p} <> '\000'

let is_slot_readable fn_name box k =
  let used =
    Netsys_sem.sem_wait box.sem.s_slot_lock Netsys_sem.SEM_WAIT_BLOCK;
    let p = box.sem.slot_readable.(k) in
    let used = box.mem.{p} <> '\000' in
    Netsys_sem.sem_post box.sem.s_slot_lock;
    used in
  dlogr
    (fun () ->
       sprintf "is_slot_readable fn_name=%s slot=%d used=%B p=%d p_used=%B" 
         fn_name k used (box.sem.slot_readable.(k)) 
         (box.mem.{box.sem.slot_readable.(k)} <> '\000')
    );
  used


let camlbox_get box k =
  if k < 0 || k >= box.hdr.capacity then
    invalid_arg "camlbox_get";
  if not (is_slot_readable "camlbox_get" box k) then raise Empty;
  let slot_size = (((box.hdr.msg_size-1) / align) + 1) * align in
  let slot_offset = box.hdr.offset + k * slot_size in
  dlogr
    (fun () ->
       sprintf
         "camlbox_get slot=%d slot_offset=%d slot_size=%d"
         k slot_offset slot_size
    );
  as_value box.mem (slot_offset + ws_bytes)

let camlbox_get_copy box k =
  Netsys_mem.copy_value
    [Netsys_mem.Copy_custom_int; Netsys_mem.Copy_bigarray]
    (camlbox_get box k)

let swap (x:int array) p q =
  let u = x.(p) in
  x.(p) <- x.(q);
  x.(q) <- u

let camlbox_delete box k =
  if k < 0 || k >= box.hdr.capacity then
    invalid_arg "camlbox_delete";
  if not (is_slot_readable "camlbox_delete" box k) then raise Empty;
  box.is_new.(k) <- true;
  Netsys_sem.sem_wait box.sem.s_slot_lock Netsys_sem.SEM_WAIT_BLOCK;
  let l = box.slot_list in
  let j = ref 0 in
  ( try
      for i = 0 to box.hdr.split_idx - 1 do
	if l.(i) = k then ( j := i; raise Exit )
      done;
      assert false
    with
      | Exit -> ()
  );
  swap l !j (box.hdr.split_idx-1);
  box.hdr.split_idx <- box.hdr.split_idx - 1;
  box.hdr.free_slots <- box.hdr.free_slots + 1;
  let p = box.sem.slot_readable.(k) in
  box.mem.{p} <- '\000';
  Netsys_sem.sem_post box.sem.s_slot_lock;
  Netsys_sem.sem_post box.sem.s_free_slots
  
let camlbox_wait box =
  dlog "camlbox_wait <waiting>";
  Netsys_sem.sem_wait box.sem.s_notifications Netsys_sem.SEM_WAIT_BLOCK;
  let n = ref 0 in
  ( try
      while true do   (* decrement to 0 *)
	Netsys_sem.sem_wait 
	  box.sem.s_notifications Netsys_sem.SEM_WAIT_NONBLOCK;
        incr n;
      done
    with
      | Unix.Unix_error(Unix.EAGAIN,_,_) -> ()
  );
  dlogr (fun () -> sprintf "camlbox_wait #notifications=%d" !n);
  Netsys_sem.sem_wait box.sem.s_slot_lock Netsys_sem.SEM_WAIT_BLOCK;
  let l = box.slot_list in
  let filled = ref [] in
  for i = 0 to box.hdr.split_idx - 1 do
    let k = l.(i) in
    if is_slot_readable_nolock "camlbox_wait" box k then
      filled := k :: !filled
    (* NB. Because of the 2-phase sending (first allocate, then fill the
       slot), it is possible to find a used slot that is not yet completely
       filled with a message
     *)
  done;
  dlogr
    (fun () ->
       sprintf "camlbox_wait slots=%s"
         (String.concat " "
            (List.map 
               (fun i -> 
                  sprintf "%d<%s>" 
                    i (if box.is_new.(i) then "new" else "old")
               )
               !filled
            ))
    );
  let r =
    List.filter
      (fun i ->
	 box.is_new.(i)
      )
      !filled in
  Netsys_sem.sem_post box.sem.s_slot_lock;
  List.iter
    (fun k -> box.is_new.(k) <- false)
    r;
  r

let camlbox_cancel_wait box =
  Netsys_sem.sem_post box.sem.s_notifications

let camlbox_wake = camlbox_cancel_wait

let find_free_slot box p =
  let rec loop i =
    if i < box.hdr.capacity then (
      if box.slot_list.(i) = p then
	Some(p,i)
      else
	loop (i+1)
    )
    else
      None in
  loop box.hdr.split_idx


let camlbox_send ?prefer ?slot box value =
  (* First phase: find a free slot *)
  Netsys_sem.sem_wait box.sem.s_free_slots Netsys_sem.SEM_WAIT_BLOCK;
  Netsys_sem.sem_wait box.sem.s_slot_lock Netsys_sem.SEM_WAIT_BLOCK;
  assert(box.hdr.split_idx < box.hdr.capacity);
  let i_opt =
    match prefer with
      | None -> None
      | Some p -> find_free_slot box p in
  let k = 
    match i_opt with
      | None ->
	  box.slot_list.(box.hdr.split_idx)
      | Some(p,i) ->
	  let save = box.slot_list.(box.hdr.split_idx) in
	  box.slot_list.(box.hdr.split_idx) <- p;
	  box.slot_list.(i) <- save;
	  p in
  box.hdr.split_idx <- box.hdr.split_idx + 1;
  ( match slot with
      | None -> ()
      | Some s -> s := k
  );
  box.hdr.free_slots <- box.hdr.free_slots - 1;
  Netsys_sem.sem_post box.sem.s_slot_lock;
  (* Phase 2: Fill the slot. The slot_lock is released *)
  let slot_size = (((box.hdr.msg_size-1) / align) + 1) * align in
  let slot_offset = box.hdr.offset + k * slot_size in
  let target_custom_ops =
    [ fst this_bigarray_custom_ops, iton box.hdr.bigarray_custom_ops;
      fst this_int32_custom_ops, iton box.hdr.int32_custom_ops;
      fst this_int64_custom_ops, iton box.hdr.int64_custom_ops;
      fst this_nativeint_custom_ops, iton box.hdr.nativeint_custom_ops;
    ] in
  let (_, _) =
    try
      init_value 
	~targetaddr:(iton box.hdr.address)
	~target_custom_ops
	box.mem 
	slot_offset
	value
	[ Netsys_mem.Copy_bigarray;
	  Netsys_mem.Copy_custom_int;
	  Netsys_mem.Copy_atom
	] 
    with
	Out_of_space -> raise Message_too_big in
  (* Finally make the filled slot visible for the receiver *)
  Netsys_sem.sem_wait box.sem.s_slot_lock Netsys_sem.SEM_WAIT_BLOCK;
  let p = box.sem.slot_readable.(k) in
  dlogr
    (fun () ->
       sprintf "camlbox_send slot=%d p=%d" k p
    );
  assert(box.mem.{p} = '\000');
  box.mem.{p} <- '\001';
  Netsys_sem.sem_post box.sem.s_slot_lock;
  Netsys_sem.sem_post box.sem.s_notifications

    
