(* $Id: unixqueue_pollset.ml 1710 2012-02-16 15:15:27Z gerd $ *)

(* pset # dispose: we try to call it when there are no more operations in
   the queue. This is tested when
    - the resource is removed (and we are not waiting)
    - the group is cleared (and we are not waiting)
    - after every wait
   Note that we must not call [dispose] while [wait] is running!

   Also, we call it when [run] is left by an exception.
 *)


open Unixqueue_util
open Printf

module Float = struct
  type t = float
(*  let compare = ( Pervasives.compare : float -> float -> int ) *)
  let compare (x:float) y =
    if x < y then (-1) else if x = y then 0 else 1
      (* does not work for non-normal numbers but we don't care *)
end


module FloatMap = Map.Make(Float)


let nogroup_id = Oo.id nogroup

let min_key m = 
  (* In ocaml-3.12 there is already a function for this in Map *)
  let k = ref None in
  ( try
      FloatMap.iter
	(fun t _ -> k := Some t; raise Exit)
	m
    with Exit -> ()
  );
  match !k with
    | Some min -> min
    | None -> raise Not_found


let ops_until tmax m =
  (* Look into the FloatMap m, and return all ops for which
     t <= tmax
   *)
  let l = ref [] in
  ( try
      FloatMap.iter
	(fun t ops ->
	   if t > tmax then raise Exit;
	   l := (t,ops) :: !l
	)
	m
    with
      | Exit -> ()
  );
  List.rev !l


exception Term of Unixqueue_util.group
  (* Extra (Term g) is now the group termination event for g *)

exception Keep_alive
  (* Sometimes used to keep the event system alive *)

exception Exit_loop


let () =
  Netexn.register_printer
    (Term nogroup)
    (function
       | Term g ->
	   if g = nogroup then
	     "Term(nogroup)"
	   else
	     "Term(" ^ string_of_int (Oo.id g) ^ ")"
       | _ -> assert false
    )


let pset_set (pset:Netsys_pollset.pollset) fd (i,o,p) =
  if not i && not o && not p then
    pset # remove fd
  else
    pset # add fd (Netsys_posix.poll_req_events i o p)


let pset_find (pset:Netsys_pollset.pollset) fd =
  try Netsys_posix.poll_req_triple(pset#find fd)
  with
    | Not_found -> (false,false,false)



let op_of_event ev =
  match ev with
    | Unixqueue_util.Input_arrived(_,fd)    -> Unixqueue_util.Wait_in fd
    | Unixqueue_util.Output_readiness(_,fd) -> Unixqueue_util.Wait_out fd
    | Unixqueue_util.Out_of_band(_,fd)      -> Unixqueue_util.Wait_oob fd
    | Unixqueue_util.Timeout(_,op)          -> op
    | _ -> assert false

let rec list_mem_op op l =
  match l with
    | h :: t ->
	is_op_eq h op || list_mem_op op t
    | [] ->
	false

let while_locked mutex f =
  Netsys_oothr.serialize mutex f ()


let escape_lock mutex f =
  mutex # unlock();
  let r = 
    try f ()
    with e -> mutex # lock(); raise e in
  mutex # lock();
  r
 

let flatten_map f l =
  (* = List.flatten (List.map f l) *)
  let rec loop l h =
    match l with
      | [] -> List.rev h
      | x :: l' ->
	  let y = f x in
	  loop l' (List.rev_append y h) in
  loop l []


(* A little encapsulation so we can easily identify handlers by Oo.id *)
class ohandler (h:handler) = 
object
  method run esys eq ev =
    h esys eq ev
end


class pollset_event_system (pset : Netsys_pollset.pollset) =
  let mtp = !Netsys_oothr.provider in
  let is_mt = not (mtp#single_threaded) in
  let sys = ref (lazy (assert false)) in   (* initialized below *)
  let ops_of_group = 
    (Hashtbl.create 10 : (group, OpSet.t) Hashtbl.t) in
        (* is for [clear] only *)
  let tmo_of_op =
    (OpTbl.create 10 : (float * float * group * bool) OpTbl.t) in
      (* first number: duration of timeout (or -1)
         second number: point in time (or -1)
	 bool: whether strong
       *)
  let ops_of_tmo = ref(FloatMap.empty : OpSet.t FloatMap.t) in
  let strong_ops = ref 0 in (* number of strong (non-weak) ops *)

  let aborting = ref false in
  let close_tab = 
    (Hashtbl.create 10 :
       (Unix.file_descr, (group * (Unix.file_descr -> unit))) Hashtbl.t) in
  let abort_tab = 
     (Hashtbl.create 10 : (group, (group -> exn -> unit)) Hashtbl.t) in
  let handlers = (Hashtbl.create 10 : (group, ohandler list) Hashtbl.t) in
  let handled_groups = ref 0 in
            (* the number of keys in [handlers] *)

  let waiting = ref false in
  let when_blocking = ref (fun () -> ()) in

  let mutex = mtp # create_mutex() in


  let event_of_op_wl_nf op =
      let (_,_,g,_) = OpTbl.find tmo_of_op op in (* or Not_found *)
      match op with
	| Unixqueue_util.Wait_in fd ->
	    Unixqueue_util.Input_arrived(g,fd)
	| Unixqueue_util.Wait_out fd ->
	    Unixqueue_util.Output_readiness(g,fd)
	| Unixqueue_util.Wait_oob fd ->
	    Unixqueue_util.Out_of_band(g,fd)
	| Unixqueue_util.Wait _ ->
	    assert false in

  let events_of_op_wl op =
    try 
      [ event_of_op_wl_nf op ]
    with
      | Not_found ->
	  (* A "ghost event", i.e. there is no handler anymore
             for it, but wasn't deleted quickly enough from 
             pset
           *)
	  [] in

  let tmo_event_of_op_wl_nf op =
    let (_,_,g,_) = OpTbl.find tmo_of_op op in (* or Not_found *)
    Unixqueue_util.Timeout(g,op) in

  let tmo_events_of_op_wl op =
    try 
      [ tmo_event_of_op_wl_nf op ]
    with
      | Not_found -> [] (* Ghost event, see above *) in
	    

  let add_event_wl e =
    Equeue.add_event (Lazy.force !sys) e;
    pset # cancel_wait true
      (* Set the cancel bit, so that any pending [wait] is interrupted *) in


object(self)

  initializer (
    let equeue_sys = Equeue.create ~string_of_event self#source in
    sys := lazy equeue_sys;
    ignore(Lazy.force !sys);
    (* Add ourselves now at object creation time. The only drawback is that
       we no longer raise [Equeue.Out_of_handlers] - but this is questionable
       anyway since the addition of [Immediate] events.

       In order to generate [Out_of_handlers] we would have to count
       [Immediate] events in addition to handlers.
     *)
    self#equeue_add_handler ()
  )


  method private source  _sys =
    (* locking: the lock is not held when called, because we are called back
       from Equeue
     *)
    assert(Lazy.force !sys == _sys);

    !when_blocking();

    let dbg = !Unixqueue_util.Debug.enable in

    let locked = ref true in   (* keep track of locking state *)
    if is_mt then
      mutex # lock();
    
    let t0 = Unix.gettimeofday() in
    let tmin = try min_key !ops_of_tmo with Not_found -> (-1.0) in
    let delta = if tmin < 0.0 then (-1.0) else max (tmin -. t0) 0.0 in

    if dbg then
      dlogr (fun () ->
	       sprintf "t0 = %f,   #tmo_of_op = %d" 
		 t0 (OpTbl.length tmo_of_op)
	    );
    
    let nothing_to_do =
      (* For this test only non-weak resources count, so... *)
      !strong_ops = 0 in

    let have_eintr = ref false in

    let pset_events = 
      try
	if nothing_to_do then (
	  if dbg then
	    dlogr (fun () -> "nothing_to_do");
	  []
	)
	else (
	  if dbg then
	    dlogr (fun () -> (
		     let ops = 
		       OpTbl.fold (fun op _ l -> op::l) tmo_of_op [] in
		     let op_str = 
		       String.concat ";" (List.map string_of_op ops) in
		     sprintf "wait tmo=%f ops=<%s>" delta op_str));
	  (* Reset the cancel bit immediately before calling [wait]. Any
             event added up to now is considered anyway by [wait] because
             our lock is still held. Any new event added after we unlock will
             set the cancel_wait flag, and cause the [wait] to exit (if it is
             still running).
	   *)
	  pset # cancel_wait false;
	  waiting := true;
	  if is_mt then
	    mutex # unlock();
	  locked := false;
	  pset # wait delta
	)
      with
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    if dbg then
	      dlogr (fun () -> "wait signals EINTR");
	    have_eintr := true;
	    []
	| e ->   
	    (* Usually from [wait], but one never knows... *)
	    if !locked && is_mt then mutex#unlock();
	    waiting := false;
	    raise e
    in

    waiting := false;
    if not !locked && is_mt then mutex # lock();
    locked := true;
    try  
      (* Catch exceptions and unlock *)

      if dbg then
	dlogr (fun () -> (
		 sprintf "wait returns <%d pset events>" 
		   (List.length pset_events)));
      
      let t1 = Unix.gettimeofday() in
      if dbg then
	dlogr (fun () -> (sprintf "t1 = %f" t1));
      (* t1 is the reference for determining the timeouts *)

      (* while waiting somebody might have removed resouces, so ... *)
      if OpTbl.length tmo_of_op = 0 then
	pset # dispose();

      let operations = 	(* flatten_map *)
	(* The possible operations *)
	List.fold_left
	  (fun acc (fd,ev_in,ev_out) ->
	     (* Note that POLLHUP and POLLERR can also mean that we
                have data to read/write!
	      *)
	     let (in_rd,in_wr,in_pri) = 
	       Netsys_posix.poll_req_triple ev_in in
	     let out_rd = 
	       Netsys_posix.poll_rd_result ev_out in 
	     let out_wr = 
	       Netsys_posix.poll_wr_result ev_out in 
	     let out_pri = 
	       Netsys_posix.poll_pri_result ev_out in 
	     let out_hup = 
	       Netsys_posix.poll_hup_result ev_out in 
	     let out_err = 
	       Netsys_posix.poll_err_result ev_out in 
	     let have_input  = 
	       in_rd && (out_rd || out_hup || out_err) in
	     let have_output =
	       in_wr && (out_wr || out_hup || out_err) in
	     let have_pri =
	       in_pri && (out_pri || out_hup || out_err) in
	     let e1 = 
	       if have_pri then Unixqueue_util.Wait_oob fd :: acc else acc in
	     let e2 = 
	       if have_input then Unixqueue_util.Wait_in fd :: e1 else e1 in
	     let e3 = 
	       if have_output then Unixqueue_util.Wait_out fd :: e2 else e2 in
	     e3
	  )
	  []
	  pset_events in

      let ops_timed_out =
	ops_until t1 !ops_of_tmo in
      (* Note: this _must_ include operations until <= t1 (not <t1), otherwise
         a timeout value of 0.0 won't work
       *)

      let ops_timed_out_l =
	(* Determine the operations in [tmo_of_op] that have timed
           out and that are not in [operations]
	   FIXME: [List.mem op operations] is not scalable.
	 *)
	List.fold_left
	  (fun oacc (_, ops) ->
	     OpSet.fold
	       (fun op iacc ->
		  if list_mem_op op operations then iacc else op::iacc) 
	       ops 
	       oacc
	  )
	  []
	  ops_timed_out in

      if dbg then (
	dlogr
	  (fun() -> 
	     sprintf "delivering events <%s>"
	       (String.concat ";" 
		  (flatten_map
		     (fun op ->
			List.map string_of_event (events_of_op_wl op)
		     )
		     operations
		  )));
	dlogr
	  (fun() -> 
	     sprintf "delivering timeouts <%s>"
	       (String.concat ";" 
		  (flatten_map
		     (fun op ->
			List.map string_of_event (tmo_events_of_op_wl op)
		     )
		     ops_timed_out_l
		  )));
      );
      
      (* deliver events *)
      let delivered = ref false in
      let deliver get_ev op =
	try 
	  let ev = get_ev op in
	  delivered := true;
	  Equeue.add_event _sys ev
	with Not_found -> () in
      List.iter (deliver event_of_op_wl_nf)     operations;
      List.iter (deliver tmo_event_of_op_wl_nf) ops_timed_out_l;

      if !have_eintr then (
	dlogr (fun () -> "delivering Signal");
	Equeue.add_event _sys Unixqueue_util.Signal
      ) else
	if not !delivered && not nothing_to_do then (
          (* Ensure we always add an event to keep the event loop running: *)
	  if dbg then
	    dlogr (fun () -> "delivering Keep_alive");
	  Equeue.add_event _sys (Unixqueue_util.Extra Keep_alive)
	);
    
      (* Update ops_of_tmo: *)
      List.iter
	(fun (t,_) ->
	   ops_of_tmo := FloatMap.remove t !ops_of_tmo
	)
	ops_timed_out;

      (* Set a new timeout for all delivered events:
         (Note that [pset] remains unchanged, because the set of watched
         resources remains unchanged.)
	 rm_done is true when the old timeout is already removed from
	 ops_of_tmo.
       *)
      let update_tmo rm_done oplist =
	List.iter
	  (fun op ->
	     try
	       let (tmo,t1,g,is_strong) = 
		 OpTbl.find tmo_of_op op in (* or Not_found *)
	       if tmo >= 0.0 then (
		 let t2 = t1 +. tmo in
		 self#sched_upd_tmo_wl g op tmo t2 is_strong 
		   (if rm_done then (-1.0) else t1)
	       )
	     with
	       | Not_found -> ()
		   (* It is possible that resources were removed while
		      we were waiting for events. This can lead to
		      [Not_found] here. We just ignore this.
		    *)
	  )
	  oplist in
      update_tmo false operations;
      update_tmo true  ops_timed_out_l;

      if is_mt then mutex # unlock();
      locked := false

    with
      | e ->
	  (* exceptions are unexpected, but we want to make sure not to mess
             with locks
	   *)
	  if !locked && is_mt then mutex # unlock();
	  raise e


  (* Note: suffix _wl = while locked *)

  method private sched_remove_wl op =
    try
      let tmo, t1, g, is_strong = OpTbl.find tmo_of_op op in  (* or Not_found *)
      dlogr(fun () -> (sprintf "sched_remove %s" (string_of_op op)));
      OpTbl.remove tmo_of_op op;
      if is_strong then decr strong_ops;
      ( try
	  let l_ops =
	    if tmo >= 0.0 then
	      FloatMap.find t1 !ops_of_tmo 
	    else raise Not_found in
	  let l_ops' =
	    OpSet.remove op l_ops in
	  if l_ops' = OpSet.empty then
	    ops_of_tmo := FloatMap.remove t1 !ops_of_tmo
	  else
	    ops_of_tmo := FloatMap.add t1 l_ops' !ops_of_tmo
	with Not_found -> ()
      );
      if Oo.id g <> nogroup_id then (
	let old_set = Hashtbl.find ops_of_group g in
	let new_set = OpSet.remove op old_set in
	if new_set = OpSet.empty then
	  Hashtbl.remove ops_of_group g
	else
	  Hashtbl.replace ops_of_group g new_set
      )

    with
      | Not_found -> ()


  method private pset_remove_wl op =
    match op with
      | Wait_in fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (false,o,p)
      | Wait_out fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (i,false,p)
      | Wait_oob fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (i,o,false)
      | Wait _ ->
	  ()
	    

  method private sched_add_wl g op tmo t1 is_strong =
    dlogr(fun () -> (sprintf "sched_add %s tmo=%f t1=%f is_strong=%b"
		       (string_of_op op) tmo t1 is_strong));
    OpTbl.replace tmo_of_op op (tmo, t1, g, is_strong);
    if is_strong then
      incr strong_ops;
    let l_ops =
      try FloatMap.find t1 !ops_of_tmo with Not_found -> OpSet.empty in
    if tmo >= 0.0 then
      ops_of_tmo := FloatMap.add t1 (OpSet.add op l_ops) !ops_of_tmo;
    if Oo.id g <> nogroup_id then (
      let old_set =
	try Hashtbl.find ops_of_group g with Not_found -> OpSet.empty in
      let new_set =
	OpSet.add op old_set in
      Hashtbl.replace ops_of_group g new_set
    )

  method private pset_add_wl op =
    match op with
      | Wait_in fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (true,o,p)
      | Wait_out fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (i,true,p)
      | Wait_oob fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (i,o,true)
      | Wait _ ->
	  ()

  method private sched_upd_tmo_wl g op tmo t1 is_strong old_t1 =
    (* only for tmo>=0 *)
    dlogr(fun () -> (sprintf "sched_upd_tmo %s tmo=%f t1=%f is_strong=%b"
		       (string_of_op op) tmo t1 is_strong));
    OpTbl.replace tmo_of_op op (tmo, t1, g, is_strong);

    (* We assume old_t1 is already removed form ops_of_tmo if old_t1 < 0 *)
    if old_t1 >= 0.0 then (
      try
	let l_ops =
	  FloatMap.find old_t1 !ops_of_tmo in
	let l_ops' =
	  OpSet.remove op l_ops in
	if l_ops' = OpSet.empty then
	  ops_of_tmo := FloatMap.remove old_t1 !ops_of_tmo
	else
	  ops_of_tmo := FloatMap.add old_t1 l_ops' !ops_of_tmo
      with Not_found -> ()
    );

    let l_ops_new =
      try FloatMap.find t1 !ops_of_tmo with Not_found -> OpSet.empty in
    ops_of_tmo := FloatMap.add t1 (OpSet.add op l_ops_new) !ops_of_tmo

  method exists_resource op =
    while_locked mutex
      (fun () -> self # exists_resource_wl op)

  method private exists_resource_wl op =
    OpTbl.mem tmo_of_op op


  method private exists_descriptor_wl fd =
    self#exists_resource_wl (Unixqueue_util.Wait_in fd) ||
    self#exists_resource_wl (Unixqueue_util.Wait_out fd) ||
    self#exists_resource_wl (Unixqueue_util.Wait_oob fd)


  method add_resource g (op, tmo) =
    while_locked mutex
      (fun () ->
	 self # add_resource_wl g (op, tmo) true
      )

  method add_weak_resource g (op, tmo) =
    while_locked mutex
      (fun () ->
	 self # add_resource_wl g (op, tmo) false
      )


  method private add_resource_wl g (op, tmo) is_strong =
    if g # is_terminating then
      invalid_arg "Unixqueue.add_resource: the group is terminated";
    if not (OpTbl.mem tmo_of_op op) then (
      self#pset_add_wl op;
      let t1 = if tmo < 0.0 then tmo else Unix.gettimeofday() +. tmo in
      self#sched_add_wl g op tmo t1 is_strong;
      (* Multi-threading: interrupt [wait] *)
      pset # cancel_wait true;
      (* CHECK: In the select-based impl we add Keep_alive to equeue.
              This idea (probably): If [wait] is about to return, and the
              queue becomes empty, the whole esys terminates. The Keep_alive
              prevents that. 
              My current thinking is that this delays the race condition
              a bit, but does not prevent it from happening
       *)
    )
      (* Note: The second addition of a resource is silently ignored...
              Maybe this should be fixed, so that the timeout can be lowered
       *)


  method remove_resource g op =
    while_locked mutex
      (fun () ->
	 if g # is_terminating then
	   invalid_arg "remove_resource: the group is terminated";
	 let _, t1, g_found, _ = OpTbl.find tmo_of_op op in
	 if Oo.id g <> Oo.id g_found then
	   failwith "remove_resource: descriptor belongs to different group";
	 self#sched_remove_wl op;
	 self#pset_remove_wl op;

	 if not !waiting && OpTbl.length tmo_of_op = 0 then
	   pset # dispose();

	 pset # cancel_wait true;    (* interrupt [wait] *)
	 (* is there a close action ? *)
	 let fd_opt =
	   match op with
             | Wait_in  d -> Some d
             | Wait_out d -> Some d
             | Wait_oob d -> Some d
             | Wait _      -> None in
	 match fd_opt with
	   | Some fd ->
               if not !aborting then (
		 let action = 
		   try Some (snd(Hashtbl.find close_tab fd)) 
		   with Not_found -> None in
		 match action with
		   | Some a ->
                       (* any open resource? *)
		       (* FIXME MT: We don't know yet whether fd can be closed.
                          This shouldn't be done before [wait] returns.
                        *)
                       if not (self#exists_descriptor_wl fd) then (
			 dlogr 
			   (fun () -> 
			      (sprintf "remove_resource \
                                        <running close action for fd %s>"
                                 (string_of_fd fd)));
			 Hashtbl.remove close_tab fd;
			 escape_lock mutex (fun () -> a fd);
                       )
		   | None ->
                       ()
               )
	   | None -> ()
      )


  method new_group () =
    new group_object

  method new_wait_id () =
    new wait_object


  method add_close_action g (d,a) =
    while_locked mutex
      (fun () ->
	 if g # is_terminating then
	   invalid_arg "add_close_action: the group is terminated";
	 (* CHECK: Maybe we should fail if g is a different group than
          * the existing group
	  *)
	 if self#exists_descriptor_wl d then
	   Hashtbl.replace close_tab d (g,a)
             (* There can be only one close action.
              * TODO: Rename to set_close_action
              *)
	 else
	   failwith "add_close_action"
      )


  method add_abort_action g a =
    while_locked mutex
      (fun () ->
	 if g # is_terminating then
	   invalid_arg "add_abort_action: the group is terminated";
	 if Oo.id g = nogroup_id then
	   invalid_arg "add_abort_action: the group is nogroup";
	 Hashtbl.replace abort_tab g a
      )

  method add_event e =
    while_locked mutex
      (fun () -> add_event_wl e)


  method private uq_handler (esys : event Equeue.t) ev =
    (* locking: it is assumed that we do not have the lock when uq_handler
       is called. It is a callback from Equeue
     *)
    (* The single Unixqueue handler. For all added (sub) handlers, uq_handler
     * gets the events, and delivers them
     *)

    let terminate_handler_wl g h =
      if !Unixqueue_util.Debug.enable then
	dlogr
	  (fun() -> 
	     (sprintf "uq_handler <terminating handler group %d, handler %d>"
		(Oo.id g) (Oo.id h)));
      let hlist =
        try Hashtbl.find handlers g with Not_found -> [] in
      let hlist' =
        List.filter (fun h' -> Oo.id h' <> Oo.id h) hlist in
      if hlist' = [] then (
        Hashtbl.remove handlers g;
        handled_groups := !handled_groups - 1;
(*
        if handled_groups = 0 then (
	  dlogr (fun () -> "uq_handler <self-terminating>");
          raise Equeue.Terminate  (* delete uq_handler from esys *)
	)
 *)
      ) else (
        Hashtbl.replace handlers g hlist'
      )
    in

    let rec forward_event_to g (hlist : ohandler list) =
      (* The caller does not have the lock when this fn is called! *)
      match hlist with
          [] ->
	    if !Unixqueue_util.Debug.enable then
	      dlogr (fun () -> "uq_handler <empty list>");
            raise Equeue.Reject
        | h :: hlist' ->
            ( try
                (* Note: ues _must not_ be locked now *)
		if !Unixqueue_util.Debug.enable then
		  dlogr
		    (fun () -> 
		       (sprintf 
			  "uq_handler <invoke handler group %d, handler %d>"
			  (Oo.id g) (Oo.id h)));
                h#run (self :> event_system) esys ev;
		if !Unixqueue_util.Debug.enable then
		  dlogr
		    (fun () -> 
		       (sprintf 
			  "uq_handler <invoke_success handler group %d, handler %d>"
			  (Oo.id g) (Oo.id h)));
              with
                  Equeue.Reject ->
                    forward_event_to g hlist'
                | Equeue.Terminate ->
                    (* Terminate only this handler. *)
                    while_locked mutex
		      (fun () -> terminate_handler_wl g h)
                    (* Any error exceptions simply fall through. Equeue will
                     * catch them, and will add the event to the error queue
                     *)
            )
    in

    let forward_event g =
      (* The caller does not have the lock when this fn is called! *)
      if !Unixqueue_util.Debug.enable then
	dlogr
	  (fun () ->
	     (sprintf "uq_handler <forward_event group %d>" (Oo.id g)));
      let hlist =
        while_locked mutex
	  (fun () -> 
             try Hashtbl.find handlers g with Not_found -> []) in
      forward_event_to g hlist
    in

    let forward_event_to_all() =
      (* The caller does not have the lock when this fn is called! *)
      let hlist_all =
	while_locked mutex
	  (fun () -> 
             Hashtbl.fold (fun g hlist l -> (g,hlist) :: l) handlers []) in
      try
        List.iter
          (fun (g,hlist) ->
             try
               forward_event_to g hlist;
               raise Exit_loop   (* event is delivered, so exit iteration *)
             with
                 (* event is rejected: try next group *)
                 Equeue.Reject -> ()
          )
          hlist_all;
        raise Equeue.Reject (* no handler has accepted the event, so reject *)
      with
          Exit_loop -> ()
    in

    if !Unixqueue_util.Debug.enable then
      dlogr
	(fun () ->
	   (sprintf "uq_handler <event %s>"
	      (string_of_event ev)));

    match ev with
      | Extra (Term g) ->
          (* Terminate all handlers of group g *)
	  while_locked mutex
	    (fun () ->
               if Hashtbl.mem handlers g then (
		 dlogr
		   (fun () ->
		      (sprintf "uq_handler <terminating group %d>" (Oo.id g)));
		 Hashtbl.remove handlers g;
		 handled_groups := !handled_groups - 1;
		 (*
		 if handled_groups = 0 then (
		   dlogr (fun () -> "uq_handler <self-terminating>");
		   raise Equeue.Terminate  (* delete uq_handler from esys *)
		 )
		  *)
               )
               else raise Equeue.Reject (* strange, should not happen *)
	    )
      | Extra Keep_alive ->
          raise Equeue.Reject
      | Input_arrived(g,_) ->
          if g # is_terminating then raise Equeue.Reject;
          forward_event g;
      | Output_readiness(g,_) ->
          if g # is_terminating then raise Equeue.Reject;
          forward_event g;
      | Out_of_band(g,_) ->
          if g # is_terminating then raise Equeue.Reject;
          forward_event g;
      | Timeout(g,_) ->
          if g # is_terminating then raise Equeue.Reject;
          forward_event g;
      | Signal ->
          forward_event_to_all();
      | Extra x ->
          forward_event_to_all();
      | Immediate(g,f) ->
	  if g # is_terminating then raise Equeue.Reject;
	  ( try f()
	    with Equeue.Terminate -> ()
	  )

  method private equeue_add_handler () =
    Equeue.add_handler (Lazy.force !sys) self#uq_handler

  (* CHECK: There is a small difference between Equeue.add_handler and
   * this add_handler: Here, the handler is immediately active (if
   * uq_handler is already active). Can this lead to problems?
   *)

  method add_handler g h =
    while_locked mutex
      (fun () ->
	 let oh = new ohandler h in
	 dlogr
	   (fun () ->
	      (sprintf
		 "add_handler <group %d, handler %d>" (Oo.id g) (Oo.id oh)));
	 
	 if g # is_terminating then
	   invalid_arg "Unixqueue.add_handler: the group is terminated";
	 
	 ( try
             let old_handlers = Hashtbl.find handlers g in
             Hashtbl.replace handlers g (oh :: old_handlers)
	   with
             | Not_found ->
		 (* The group g is new *)
		 Hashtbl.add handlers g [oh];
		 handled_groups := !handled_groups + 1;
		 (* if handled_groups = 1 then
		   self#equeue_add_handler ()
		  *)
	 )
      )

  method clear g =
    if Oo.id g = nogroup_id then
      invalid_arg "Unixqueue.clear: nogroup";
    while_locked mutex
      (fun () -> self # clear_wl g)


  method private clear_wl g =
    dlogr (fun () -> (sprintf "clear <group %d>" (Oo.id g)));
    
    (* Set that g is terminating now: *)
    g # terminate();
    
    (* (i) delete all resources of g: *)
    let ops = 
      try Hashtbl.find ops_of_group g with Not_found -> OpSet.empty in
    OpSet.iter
      self#sched_remove_wl
      ops;
    OpSet.iter
      self#pset_remove_wl
      ops;
    Hashtbl.remove ops_of_group g;

    (* (ii) delete all handlers of g: *)
    add_event_wl (Extra (Term g));
    (* side effect: we also interrupt [wait] *)

    (* (iii) delete special actions of g: *)
    let to_remove =   (* remove from close_tab *)
      Hashtbl.fold
        (fun d (g',_) l -> if g = g' then d :: l else l) close_tab [] in
    List.iter
      (Hashtbl.remove close_tab) to_remove;
    
    Hashtbl.remove abort_tab g;

    (* Note: the Term event isn't caught after all handlers have been
     * deleted. The Equeue module simply discards events that are not
     * handled.
     *)

    if not !waiting && OpTbl.length tmo_of_op = 0 then
      pset # dispose();


  method private abort g ex =
    (* caller doesn't have the lock *)
    (* Note: If g has been terminated, the abort action is removed. So
     * we will never find here one.
     *)
    dlogr (fun () -> (sprintf "abort <group %d, exception %s>"
                         (Oo.id g) (Netexn.to_string ex)));
    let action =
      while_locked mutex
	(fun () ->
	   try Some (Hashtbl.find abort_tab g) with Not_found -> None) in
    match action with
      | Some a ->
          begin
            dlogr (fun () -> "abort <running abort action>");
            let mistake = ref None in
	    while_locked mutex 
	      (fun () -> aborting := true);
            begin try
              a g ex;
            with
              | any ->
                  mistake := Some any (* Wow *)
            end;
	    while_locked mutex 
	      (fun () ->
		 self#clear_wl g;
		 aborting := false
	      );
            match !mistake with
              | None -> ()
              | Some m ->
                  dlogr (fun () -> (sprintf "abort <propagating exception %s>"
                                       (Netexn.to_string m)));
                  raise m
          end
      | None ->
          ()

  method run () =
    (* caller doesn't have the lock *)
    let continue = ref true in
    try
      while !continue do
        continue := false;
        try
          Equeue.run (Lazy.force !sys);
        with
          | Abort (g,an_exception) ->
              begin
                match an_exception with
                  | (Equeue.Reject|Equeue.Terminate) ->
                      (* A serious programming error: *)
                      failwith "Caught 'Abort' exception with Reject or Terminate exception as argument; this is a programming error"
                  | Abort(_,_) ->
                      failwith "Caught 'Abort' exception with an 'Abort' exception as argument; this is a programming error"
                  | _ -> ()
              end;
              self#abort g an_exception;
              continue := true
      done;
    with
      | error ->
	  pset # dispose();
          raise error

  method is_running =
    Equeue.is_running (Lazy.force !sys)


  method when_blocking f =
    when_blocking := f

end


let pollset_event_system pset = 
  (new pollset_event_system pset :> Unixqueue_util.event_system)

