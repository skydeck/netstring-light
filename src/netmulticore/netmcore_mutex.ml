(* $Id: netmcore_mutex.ml 1570 2011-04-08 14:47:16Z gerd $ *)

type mutex_type =
    [ `Normal | `Errorcheck | `Recursive ]

type mutex =
    { is_dummy : bool;
      mtype : mutex_type;
      mutable lock : Netmcore_sem.semaphore;
      mutable proplock : Netmcore_sem.semaphore;
      mutable owner : int;  (* pid *)
      mutable number : int;
    }

let dummy() =
  { is_dummy = true;
    mtype = `Normal;
    lock = Netmcore_sem.dummy();
    proplock = Netmcore_sem.dummy();
    owner = 0;
    number = 0;
  }

let create mut mtype =
  let mutex_orig =
    { is_dummy = false;
      mtype = mtype;
      lock = Netmcore_sem.dummy();
      proplock = Netmcore_sem.dummy();
      owner = 0;
      number = 0;
    } in
  let mutex = Netmcore_heap.add mut mutex_orig in
  Netmcore_heap.pin mut mutex;
  let lock = Netmcore_sem.create mut 1 in
  mutex.lock <- lock;
  if mtype <> `Normal then (
    let proplock = Netmcore_sem.create mut 1 in
    mutex.proplock <- proplock;
  );
  mutex

let serialized sem f =
  Netmcore_sem.wait sem Netsys_posix.SEM_WAIT_BLOCK;
  try
    let r = f() in
    Netmcore_sem.post sem;
    r
  with
    | error ->
	Netmcore_sem.post sem;
	raise error


let lock mutex =
  if mutex.is_dummy then
    failwith "Netmcore_mutex.lock: dummy mutex";
  match mutex.mtype with
    | `Normal ->
	Netmcore_sem.wait mutex.lock Netsys_posix.SEM_WAIT_BLOCK
    | `Errorcheck ->
	let pid = Unix.getpid() in
	serialized mutex.proplock
	  (fun () ->
	     if mutex.owner = pid then
	       failwith "Netmcore_mutex.lock: already locked by this process"
	  );
	Netmcore_sem.wait mutex.lock Netsys_posix.SEM_WAIT_BLOCK;
	serialized mutex.proplock
	  (fun () ->
	     mutex.owner <- pid
	  )
    | `Recursive ->
	let pid = Unix.getpid() in
	let need_lock =
	  serialized mutex.proplock
	    (fun () -> 
	       mutex.owner <> pid || (
		 mutex.number <- mutex.number + 1;
		 false
	       )
	    ) in
	if need_lock then (
	  Netmcore_sem.wait mutex.lock Netsys_posix.SEM_WAIT_BLOCK;
	  serialized mutex.proplock
	    (fun () ->
	       mutex.owner <- pid;
	       mutex.number <- 1;
	    )
	)

let unlock mutex =
  if mutex.is_dummy then
    failwith "Netmcore_mutex.unlock: dummy mutex";
  match mutex.mtype with
    | `Normal ->
	Netmcore_sem.post mutex.lock
    | `Errorcheck ->
	let pid = Unix.getpid() in
	serialized mutex.proplock
	  (fun () ->
	     if mutex.owner <> pid then
	       failwith "Netmcore_mutex.unlock: not locked by this process";
	     Netmcore_sem.post mutex.lock;
	     mutex.owner <- 0;
	  )
    | `Recursive ->
	let pid = Unix.getpid() in
	serialized mutex.proplock
	  (fun () ->
	     if mutex.owner <> pid then
	       failwith "Netmcore_mutex.unlock: not locked by this process";
	     Netmcore_sem.post mutex.lock;
	     mutex.number <- mutex.number - 1;
	     if mutex.number = 0 then
	       mutex.owner <- 0
	  )

let destroy mutex =
  if not mutex.is_dummy then (
    Netmcore_sem.destroy mutex.lock;
    Netmcore_sem.destroy mutex.proplock;
  )

  
