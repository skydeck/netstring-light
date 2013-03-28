(* $Id: netsys_pollset_posix.ml 1696 2012-02-08 19:27:53Z gerd $ *)

open Netsys_pollset

let fd_equal =
  match Sys.os_type with
    | "Win32" ->
	(fun fd1 fd2 -> fd1=fd2)
    | _ ->
	(fun (fd1:Unix.file_descr) fd2 ->
	   (Obj.magic fd1 : int) = (Obj.magic fd2 : int)
	)

let fd_hash =
  match Sys.os_type with
    | "Win32" ->
	(fun fd -> Hashtbl.hash fd)
    | _ ->
	(fun fd -> (Obj.magic fd : int))


module FdTbl =
  Hashtbl.Make
    (struct
       type t = Unix.file_descr
       let equal = fd_equal
       let hash = fd_hash
     end
    )


let oothr = !Netsys_oothr.provider

let while_locked mutex f =
  Netsys_oothr.serialize mutex f ()

let ne_limit = 4
  (* We keep up to [ne_limit] obejcts of type [not_event] for interrupting
     [poll].
     If more than this number of objects become unused, they are closed.
   *)

let ne_list = ref []
let ne_m = oothr # create_mutex()
let ne_pid = ref None
  (* When the process is forked, we give up our saved [not_event]s, to avoid the
     confusion when several processes use the same descriptors
   *)

let reset_locked() =
  let l = !ne_list in
  ne_list := [];
  ne_pid := None;
  List.iter Netsys_posix.destroy_event l


let reset() =
  while_locked 
    ne_m
    reset_locked


let() =
  Netsys_posix.register_post_fork_handler
    ( object
	method name = "Netsys_pollset_posix"
	method run() = reset()
      end
    )


let get_not_event() =
  while_locked
    ne_m
    (fun () ->
       let pid = Unix.getpid() in
       if !ne_pid <> None && !ne_pid <> Some pid then (
	 reset_locked();
       );
       ne_pid := Some pid;
       match !ne_list with
	 | [] ->
	     let ne = Netsys_posix.create_event() in
	     Netsys_posix.set_nonblock_event ne;
	     ne
	 | ne :: r ->
	     ne_list := r;
	     ne
    )


let return_not_event ne =
  while_locked 
    ne_m
    (fun () ->
       if List.length !ne_list >= ne_limit then (
	 Netsys_posix.destroy_event ne;
       )
       else (
	 ne_list := ne :: !ne_list;
       )
    )


let rounded_pa_size l =
  let n = ref 32 in
  while !n < l do
    n := 2 * !n
  done;
  !n


let reset_not_event ne =
  try Netsys.restart Netsys_posix.consume_event ne 
  with Unix.Unix_error((Unix.EAGAIN|Unix.EWOULDBLOCK),_,_) -> ()


let poll_based_pollset () : pollset =
object(self)
  val mutable intr_ne = None
    (* The not_event that can be signalled for interrupting waiting *)

  val mutable cancel_flag = false
    (* The cancel flag and its mutex *)

  val mutable intr_m = oothr # create_mutex()
    (* Mutex protecting intr_fd, intr_flag, and cancel_flag *)

  val mutable ht = FdTbl.create 10
    (* maps fd to req events *)

  val mutable spa = Netsys_posix.create_poll_array 32
    (* saved poll array - for the next time, so we don't have to allocate
       it again for every [wait]
     *)

  method find fd =
    FdTbl.find ht fd

  method add fd ev =
    FdTbl.replace ht fd ev

  method remove fd =
    FdTbl.remove ht fd

  method wait tmo =
    if oothr # single_threaded then (
      if cancel_flag then
	[]
      else
	self # wait_1 tmo None
    )
    else (
      let ne = get_not_event() in
      let have_intr_lock = ref false in
      let r =
	try
	  let no_wait = (
	    intr_m # lock();
	    have_intr_lock := true;
	    if cancel_flag then (
	      intr_ne <- None;
	      have_intr_lock := false;
	      intr_m # unlock();
	      true
	    )
	    else (
	      intr_ne <- Some ne;
	      have_intr_lock := false;
	      intr_m # unlock();
	      false
	    )
	  ) in
	  if no_wait then
	    []
	  else (
	    let ne_fd =
	      Netsys_posix.get_event_fd ne in  (* this is a dup *)
	    let r = 
	      try
		self # wait_1 tmo (Some ne_fd)
	      with error ->
		Unix.close ne_fd;
		raise error in
	    Unix.close ne_fd;
	    intr_m # lock();
	    have_intr_lock := true;
	    reset_not_event ne;
	    intr_ne <- None;
	    have_intr_lock := false;
	    intr_m # unlock();
	    r
	  )
	with
	  | err ->
	      if !have_intr_lock then intr_m # unlock();
	      reset_not_event ne;
	      return_not_event ne;
	      raise err in
      return_not_event ne;
      r
    )


  method cancel_wait b =
    while_locked
      intr_m
      (fun () ->
	 cancel_flag <- b;
	 if b then (
	   match intr_ne with
	     | None -> ()
	     | Some ne ->
		 Netsys_posix.set_event ne
	 )
      )

  method private wait_1 tmo extra_fd_opt =
    let have_extra_fd = extra_fd_opt <> None in
    let ht_l = FdTbl.length ht in
    let l = ht_l + if have_extra_fd then 1 else 0 in
    let pa = 
      if l < Netsys_posix.poll_array_length spa then
	spa
      else (
	let pa = Netsys_posix.create_poll_array(rounded_pa_size l) in
	spa <- pa;
	pa
      ) in
    let j = ref 0 in
    FdTbl.iter
      (fun fd ev ->
	 let c = 
	   { Netsys_posix.poll_fd = fd;
	     poll_req_events = ev;
	     poll_act_events = Netsys_posix.poll_null_events()
	   } in
	 Netsys_posix.set_poll_cell pa !j c;
	 incr j
      )
      ht;
    ( match extra_fd_opt with
	| None -> ()
	| Some fd ->
	    let c = 
	      { Netsys_posix.poll_fd = fd;
		poll_req_events = 
		  Netsys_posix.poll_req_events true false false;
		poll_act_events = Netsys_posix.poll_null_events()
	      } in
	    Netsys_posix.set_poll_cell pa !j c;
    );
    let n = ref(Netsys_posix.poll pa l tmo) in
    let r = ref [] in
    let k = ref 0 in
    while !n > 0 && !k < ht_l do
      let c = Netsys_posix.get_poll_cell pa !k in
      if Netsys_posix.poll_result c.Netsys_posix.poll_act_events then (
	r := (c.Netsys_posix.poll_fd,
	      c.Netsys_posix.poll_req_events, 
	      c.Netsys_posix.poll_act_events) :: !r;
	decr n
      );
      incr k
    done;
    !r

  method dispose() = ()

end


let accelerated_pollset () : pollset =
  let agg = 
    ref(Some(Netsys_posix.create_event_aggregator true)) in
object(self)
  val mutable cancel_flag = false

  val mutable ht = FdTbl.create 10
    (* maps fd to (req_events, event_source) *)

  method find fd =
    fst(FdTbl.find ht fd)

  method add fd ev =
    try
      let (_, es) = FdTbl.find ht fd in
      Netsys_posix.modify_fd_event_source es ev;
      FdTbl.replace ht fd (ev, es)
    with
      | Not_found ->
	  let es = Netsys_posix.fd_event_source fd ev in
	  ( match !agg with
	      | Some g -> Netsys_posix.add_event_source g es
	      | None -> ()
	  );
	  FdTbl.replace ht fd (ev, es)

  method remove fd =
    try
      let (_, es) = FdTbl.find ht fd in
      ( match !agg with
	  | Some g -> Netsys_posix.del_event_source g es
	  | None -> ()
      );
      FdTbl.remove ht fd
    with Not_found -> ()

  method private restore() =
    match !agg with
      | None ->
	  let g = Netsys_posix.create_event_aggregator true in
	  FdTbl.iter
	    (fun fd (ev,es) ->
	       Netsys_posix.add_event_source g es
	    )
	    ht;
	  agg := Some g;
	  g
      | Some g -> g

  method wait tmo =
    let g = self # restore() in
    if cancel_flag then
      Netsys_posix.interrupt_event_aggregator g;
    let es_list = Netsys_posix.poll_event_sources g tmo in
    let triples =
      List.map
	(fun es ->
	   let fd = Netsys_posix.get_fd_of_event_source es in
	   let (req_ev, es') = 
	     try FdTbl.find ht fd
	     with Not_found ->
	       (* Actually, this is a bug in the OS *)
	       assert false in
	   assert(es == es');
	   let resp_ev = Netsys_posix.act_events_of_event_source es in
	   (fd, req_ev, resp_ev)
	)
	es_list in
    triples

  method cancel_wait b =
    cancel_flag <- b;
    match !agg with
      | None -> ()
      | Some g ->
	  if b then
	    Netsys_posix.interrupt_event_aggregator g
    (* else: do nothing. Sure, the current/next wait will be interrupted,
       but the internal cancel flag of the aggregator is automatically
       reset
     *)

  method dispose() =
    match !agg with
      | None -> ()
      | Some g ->
	  (* CHECK: Catch errors from del_event_source? We would also
	     have to delete the erroneous descriptors from ht.
	     Pro: If dispose is called from an error handler this creates
	          less confusion
	     Contra: Hiding errors is never good
	   *)
	  FdTbl.iter
	    (fun _ (_,es) ->
	       Netsys_posix.del_event_source g es
	    )
	    ht;
	  Netsys_posix.destroy_event_aggregator g;
	  agg := None

end


let accelerated_pollset() =
  if Netsys_posix.have_event_aggregation() then
    accelerated_pollset()
  else
    poll_based_pollset()
