(* $Id: uq_libevent.ml 1635 2011-06-26 15:20:34Z gerd $ *)

module type LIBOEVENT = sig
  type event
  type event_flags = 
      TIMEOUT
    | READ
    | WRITE
    | SIGNAL
  type event_callback = Unix.file_descr -> event_flags -> unit
  val create : unit -> event
  val set : event -> 
    Unix.file_descr -> event_flags list -> persist:bool -> event_callback -> 
    unit
  val add : event -> float option -> unit
  val del : event -> unit
  type loop_flags =
      ONCE            
    | NONBLOCK        
  val loop : loop_flags -> unit
end

module type POLLSET = sig
  val create_pollset : unit -> Netsys_pollset.pollset
  val create_event_system : unit -> Unixqueue.event_system
end


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


module Make(L:LIBOEVENT) = struct

  let create_pollset() : Netsys_pollset.pollset =
    let ht_req = FdTbl.create 5 in
    let ht_act = FdTbl.create 5 in
    let wait_ev = L.create() in
    let wait_ev_added = ref false in
    ( object(self)
	method find fd =
	  let (req,ev) = FdTbl.find ht_req fd in
	  req

	method add fd req =
	  let ev = 
	    try
	      let (_,old_ev) = FdTbl.find ht_req fd in
	      L.del old_ev;
	      old_ev
	    with Not_found ->
	      L.create() in
	  let (r,w,_) = Netsys_posix.poll_req_triple req in
	  let flags =
	    (if r then [L.READ] else []) @ (if w then [L.WRITE] else []) in
	  L.set ev fd flags ~persist:true self#callback;
	  L.add ev None;
	  FdTbl.replace ht_req fd (req,ev)

	method remove fd =
	  try
	    let (_,ev) = FdTbl.find ht_req fd in
	    L.del ev;
	    FdTbl.remove ht_req fd;
	    FdTbl.remove ht_act fd
	  with Not_found -> ()

	method wait tmo =
	  FdTbl.clear ht_act;
	  if !wait_ev_added then L.del wait_ev;
	  L.set wait_ev (Obj.magic (-1)) [] ~persist:true (fun _ _ -> ());
	  L.add wait_ev (Some tmo);
	  wait_ev_added := true;
	  L.loop L.ONCE;
	  FdTbl.fold
	    (fun fd act_n acc ->
	       let (req,ev) = 
		 try FdTbl.find ht_req fd with Not_found -> assert false in
	       let act = Netsys_posix.act_events_of_int act_n in
	       (fd, req, act) :: acc
	    )
	    ht_act
	    []

	method private callback fd flag =
	  try
	    let act =
	      ( match flag with
		  | L.READ -> Netsys_posix.const_rd_event
		  | L.WRITE -> Netsys_posix.const_wr_event
		  | _ -> raise Not_found
	      ) lor
		( try FdTbl.find ht_act fd with Not_found -> 0 ) in
		FdTbl.replace ht_act fd act
	  with
	    | Not_found -> ()

	method dispose() = ()

	method cancel_wait _ =
	  failwith "Uq_libevent.cancel_wait: not supported"
      end
    )

  let create_event_system() =
    Unixqueue_pollset.pollset_event_system
      (create_pollset())

end
