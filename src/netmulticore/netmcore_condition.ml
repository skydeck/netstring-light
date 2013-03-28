(* $Id: netmcore_condition.ml 1808 2012-11-02 23:37:50Z gerd $ *)

(* Literature:
   "Implementing Condition Variables with Semaphores", by Andrew D. Birrell,
   Microsoft Research Silicon Valley, January 2003

   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.125.3384&rep=rep1&type=pdf
 *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netmcore_condition" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netmcore_condition" Debug.enable

let () =
  Netlog.Debug.register_module "Netmcore_condition" Debug.enable

open Printf
open Uq_engines.Operators

type condition =
    { dummy_cond : bool;
      mutable waiters : wait_entry;
      mutable null : wait_entry;   (* means: "no more entry" *)
      mutable lock : Netmcore_sem.semaphore;
    }

and wait_entry =
    { mutable empty : bool;  
      (* if empty, this entry is the last in the set, and it is considered
	 as representing no waiter
       *)
      mutable sem : Netmcore_sem.semaphore;
      mutable next : wait_entry;      (* may be cond.null *)
      mutable set_next : wait_entry;  (* not meaningful if [empty] *)
      mutable pipe : string option;
    }

and wait_entry_e = wait_entry

and wait_set = 
    { dummy_set : bool;
      mutable alloc_lock : Netmcore_sem.semaphore;
      mutable head : wait_entry;
    }

let empty_wait_entry() =
  let rec we =
    { empty = true;
      sem = Netmcore_sem.dummy();
      next = we;
      set_next = we;
      pipe = None
    } in
  we


let dummy_condition () =
  { dummy_cond = true;
    waiters = empty_wait_entry ();
    null = empty_wait_entry ();
    lock = Netmcore_sem.dummy()
  }


let dummy_wait_set() =
  { dummy_set = true;
    alloc_lock = Netmcore_sem.dummy();
    head = empty_wait_entry()
  }


let create_condition mut =
  let null = empty_wait_entry () in
  let cond_orig =
    { dummy_cond = false;
      waiters = null;
      null = null;
      lock = Netmcore_sem.dummy()
    } in
  let cond = Netmcore_heap.add mut cond_orig in
  Netmcore_heap.pin mut cond;
  cond.lock <- Netmcore_sem.create mut 1;
  cond

let destroy_condition c =
  if not c.dummy_cond then (
    Netmcore_sem.destroy c.lock
  )


let create_wait_set mut =
  let wset_orig =
    { dummy_set = false;
      alloc_lock = Netmcore_sem.dummy();
      head = empty_wait_entry()
    } in
  let wset = Netmcore_heap.add mut wset_orig in
  Netmcore_heap.pin mut wset;
  wset.alloc_lock <- Netmcore_sem.create mut 1;
  wset
  

let destroy_wait_set wset =
  if not wset.dummy_set then (
    let we = ref wset.head in
    Netmcore_sem.destroy !we.sem;
    while not !we.empty do
      we := !we.set_next;
      Netmcore_sem.destroy !we.sem;
    done;
    Netmcore_sem.destroy wset.alloc_lock
  )


let with_alloc_lock wset f =
  Netmcore_sem.wait wset.alloc_lock Netsys_posix.SEM_WAIT_BLOCK;
  try
    let r = f () in
    Netmcore_sem.post wset.alloc_lock;
    r
  with error ->
    Netmcore_sem.post wset.alloc_lock;
    raise error


let alloc_wait_entry mut wset =
  if wset.dummy_set then
    failwith "Netmcore_condition.alloc_wait_entry: dummy wait_set";
  with_alloc_lock wset
    (fun () ->
       (* not really fast *)
       let we = ref wset.head in
       while not !we.empty do
	 we := !we.set_next
       done;
       let tail_orig = empty_wait_entry () in
       let tail = Netmcore_heap.add mut tail_orig in
       !we.set_next <- tail;
       !we.empty <- false;
       !we.sem <- Netmcore_sem.create mut 0;
       !we.pipe <- None;
       !we
    )

let alloc_wait_entry_e mut wset file =
  let we = alloc_wait_entry mut wset in
  with_alloc_lock wset
    (fun () ->
       we.pipe <- Netmcore_heap.add mut (Some file)
    );
  we


let free_wait_entry mut wset we_to_free =
  if wset.dummy_set then
    failwith "Netmcore_condition.free_wait_entry: dummy wait_set";
  with_alloc_lock wset
    (fun () ->
       (* not really fast *)
       let we = ref wset.head in
       let prev = ref None in
       while not !we.empty && !we != we_to_free do
	 prev := Some !we;
	 we := !we.set_next
       done;
       if !we.empty then
	 failwith "Netmcore_condition.free_wait_entry: not found";
       ( match !prev with
	   | None ->
	       wset.head <- !we.set_next
	   | Some p ->
	       p.set_next <- !we.set_next
       );
       !we.set_next <- !we;
       !we.next <- !we;
       match !we.pipe with
	 | None -> ()
	 | Some p ->
	     ( try Unix.unlink p with _ -> () )
    )


let free_wait_entry_e = free_wait_entry


let wait we c m =
  if c.dummy_cond then
    failwith "Netmcore_condition.wait: dummy condition";
  if we.next != we then
    failwith "Netmcore_condition.wait: the wait entry is being used";
  Netmcore_sem.wait c.lock Netsys_posix.SEM_WAIT_BLOCK;
  let old_waiters = c.waiters in
  c.waiters <- we;
  we.next <- old_waiters;
  Netmcore_sem.post c.lock;
  Netmcore_mutex.unlock m;
  Netmcore_sem.wait we.sem Netsys_posix.SEM_WAIT_BLOCK;
  Netmcore_mutex.lock m


let pipe_name we =
  match we.pipe with
    | None -> assert false
    | Some p -> String.copy p


let wait_e_d name we c m esys cont =
  dlog "Netmcore_condition.wait_e";
  if c.dummy_cond then
    failwith "Netmcore_condition.wait_e: dummy condition";
  if we.empty then
    failwith "Netmcore_condition.wait_e: this is the reserved guard wait_entry";
  if we.next != we then
    failwith "Netmcore_condition.wait_e: the wait entry is being used";
  let p = pipe_name we in
  let fd = Unix.openfile p [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 in
  let fd_open = ref true in
  let close() = 
    if !fd_open then Unix.close fd;
    fd_open := false in
  Netmcore_sem.wait c.lock Netsys_posix.SEM_WAIT_BLOCK;
  let old_waiters = c.waiters in
  c.waiters <- we;
  we.next <- old_waiters;
  Netmcore_sem.post c.lock;
  let name = name ^ " (" ^ p ^ ")" in
  dlogr
    (fun() -> sprintf "Netmcore_condition.wait_e %s: unlocking mutex" name);
  Netmcore_mutex.unlock m;
  dlogr
    (fun() -> 
       sprintf "Netmcore_condition.wait_e %s: waiting for pipe signal" name);

  let rec poll_and_read_e rep =
    ( new Uq_engines.poll_engine [ Unixqueue.Wait_in fd, (-1.0) ] esys
      >> (fun st ->
            match st with
              | `Done ev -> 
                  `Done ()
              | (`Error err) as st ->
                  Netlog.logf `Err
                    "Netmcore_condition.wait_e: exception from poll_engine: %s"
                    (Netexn.to_string err);
                  st
              | `Aborted ->
                  close(); `Aborted
         )
    )
    ++ (fun () ->
          dlogr
            (fun () -> 
               sprintf
		 "Netmcore_condition.wait_e %s: poll wakeup rep=%B" name rep);
          let s = String.make 1 ' ' in
          let p = Netsys.blocking_gread `Read_write fd s 0 1 in
          (* If p=0 this is a spurious wakeup. This can basically happen
             when the previous poster still had the file open at the time
             we opened it. When the poster closes the file, we will get
             an EOF. It is meaningless, though, and can be ignored.
             Just restart polling.
           *)
          if p=0 then
            Uq_engines.delay_engine 0.01
              (fun _ -> eps_e (`Done()) esys)
              esys
            ++ (fun () ->
                  poll_and_read_e true
               )
          else (
            (* The semaphore is always 1 here - because we already read the
               byte, and thus the semaphore was already posted. Also, there
               can only be one poster (the wait entry is removed from the
               list by post)

	       NB: No getvalue on OSX, so make this error-tolerant.
             *)
            dlogr
              (fun () -> 
                 sprintf "Netmcore_condition.wait_e %s: reading done, sem=%s" name
                   (try
		      string_of_int(Netmcore_sem.getvalue we.sem)
		    with Unix.Unix_error(Unix.ENOSYS,_,_) -> "n/a"
		   )
              );
            assert(try Netmcore_sem.getvalue we.sem = 1
		   with Unix.Unix_error(Unix.ENOSYS,_,_) -> true
                  );
            Netmcore_sem.wait we.sem Netsys_posix.SEM_WAIT_BLOCK;
            close();
            dlogr
              (fun () ->
                 sprintf "Netmcore_condition.wait_e %s: locking mutex" name);
            Netmcore_mutex.lock m;
            cont()
          )
       ) in
  poll_and_read_e false

let wait_e ?(debug_name="") we c m esys cont = 
  wait_e_d debug_name we c m esys cont


let post c =
  let we = c.waiters in
  assert(not we.empty);
  c.waiters <- we.next;
  we.next <- we;
  match we.pipe with
    | None ->
	Netmcore_sem.post we.sem
    | Some p ->
	dlogr (fun () -> sprintf "Netmcore_condition.post(%s)" p);
	let fd = Unix.openfile p [Unix.O_WRONLY; Unix.O_NONBLOCK] 0 in
	Netmcore_sem.post we.sem;
	dlogr (fun () -> 
		 sprintf "Netmcore_condition.post(%s): writing" p);
	try
	  let s = "X" in
	  Netsys.really_gwrite `Read_write fd s 0 1;
	  Unix.close fd
	with error -> 
	  Unix.close fd; raise error


let signal c =
  if c.dummy_cond then
    failwith "Netmcore_condition.signal: dummy condition";
  Netmcore_sem.wait c.lock Netsys_posix.SEM_WAIT_BLOCK;
  try
    if c.waiters != c.null then
      post c;
    Netmcore_sem.post c.lock
  with
    | error ->
	Netmcore_sem.post c.lock;
	raise error

let broadcast c =
  if c.dummy_cond then
    failwith "Netmcore_condition.broadcast: dummy condition";
  Netmcore_sem.wait c.lock Netsys_posix.SEM_WAIT_BLOCK;
  try
    while c.waiters != c.null do
      post c;
    done;
    Netmcore_sem.post c.lock
  with
    | error ->
	Netmcore_sem.post c.lock;
	raise error
