(* $Id: netmcore.ml 1774 2012-04-03 21:28:51Z gerd $ *)

open Printf

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netmcore" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netmcore" Debug.enable

let () =
  Netlog.Debug.register_module "Netmcore" Debug.enable


type res_id =
    [ `Resource of int ]

type process_id =
    [ `Process of int ]

type compute_resource_type =
    [ `File | `Posix_shm | `Posix_shm_sc | `Posix_shm_preallocated 
    | `Posix_shm_preallocated_sc
    | `Posix_sem | `Fork_point | `Join_point
    ]

type inherit_request =
    [ `Resources of res_id list
    | `All
    ]

type compute_resource_repr =
    [ `File of string
    | `Posix_shm of string
    | `Posix_shm_sc of string * Netsys_sem.prefix
    | `Posix_shm_preallocated of string * Netsys_mem.memory
    | `Posix_shm_preallocated_sc of string * Netsys_mem.memory * 
                                       Netsys_sem.container
    | `Posix_sem of string
    | `Fork_point of (inherit_request * Netplex_encap.encap -> process_id)
    | `Join_point of (process_id -> Netplex_encap.encap option)
    ]

type trans_resource_repr =
    [ `File of string
    | `Posix_shm of string
    | `Posix_shm_sc of string * Netsys_sem.prefix
    | `Posix_shm_preallocated
    | `Posix_shm_preallocated_sc
    | `Posix_sem of string
    | `Fork_point
    | `Join_point
    ]

type manage_resource_repr =
    [ `File of string
    | `Posix_shm of string
    | `Posix_shm_sc of string * Netsys_sem.prefix
    | `Posix_sem of string
    ]

type executable =
    [ `Container of int   (* Oo.id of the container obj *)
    | `Controller
    ]

module Executable = struct
  type t = executable
  let compare = compare
end

module ExecSet = Set.Make(Executable)

let self_exec() =
  try
    match Netplex_cenv.self_obj() with
      | `Container c -> Some(`Container (Oo.id c))
      | `Controller c -> Some `Controller
  with
    | Not_found ->
	None (* the caller may opt to handle this as [`Controller] *)


exception No_resource of res_id

class type compute_resource =
object
  method id : res_id
  method typ : compute_resource_type
  method repr : compute_resource_repr
  method release : unit -> unit
end

class type master_resource =
object
  inherit compute_resource
  method used_in : executable -> unit
  method released_in : executable -> unit
  method destroy : unit -> unit

  method join_res : res_id
    (* only meaningful for fork points *)
  method post_start : unit -> unit
    (* This is run at process start time to free unneeded resources
       (resources not inherited to the worker)
     *)
  method process_body : Netplex_types.encap -> Netplex_types.encap
    (* The argument of [def_process] *)

end 

type process_info =
    { pid : process_id;
      join_point : res_id;
      mutable result : Netplex_encap.encap option option;
        (* Some (Some e): Process finished with a result [e]
	   Some None: Process finished without result
	   None: Process is not yet finished
	 *)
    }

module Start_lever =
  Netplex_cenv.Make_lever
    (struct 
       type s = res_id * inherit_request * Netplex_encap.encap 
       type r = process_id option
     end
    )

module Deliver_lever =
  Netplex_cenv.Make_lever
    (struct
       type s = process_id * Netplex_encap.encap option
       type r = unit
     end
    )

module Get_result_lever =
  Netplex_cenv.Make_lever
    (struct
       type s = res_id * process_id
       type r = Netplex_encap.encap option option option
	   (* None: unknown pid 
	      Some None: pid known, but no result yet
	      Some (Some None): result is None
	      Some (Some (Some encap)): result is encap
	    *)
     end
    )
  
module Manage_resource_lever =
  Netplex_cenv.Make_lever
    (struct
       type s = manage_resource_repr * int (* Oo.id of the container *)
       type r = res_id
     end
    )


module Create_prealloc_shm_lever =
  Netplex_cenv.Make_lever
    (struct
       type s = string * int * bool * int (* Oo.id of the container *) * 
                  bool
       type r = res_id * string * string option
     end
    )


module Get_resource_lever =
  Netplex_cenv.Make_lever
    (struct
       type s = res_id * int   (* Oo.id of the container *)
       type r = trans_resource_repr option
     end
    )

module Release_lever =
  Netplex_cenv.Make_lever
    (struct
       type s = res_id * int  (* Oo.id of the container *)
       type r = unit
     end
    )
  

type levers =
    { ctrl_id : int; (* Oo.id of the controller object *)
      start : res_id * inherit_request * Netplex_encap.encap -> process_id option;
      deliver : process_id * Netplex_encap.encap option -> unit;
      get_result : res_id * process_id -> Netplex_encap.encap option option option;
      manage_resource : manage_resource_repr * int -> res_id;
      create_prealloc_shm : string * int * bool * int * bool -> (res_id * string * string option);
      get_resource : res_id * int -> trans_resource_repr option;
      release : res_id * int -> unit;
    }


let levers =
  ref None
  (* This variable is set as soon as the first process is started
     (in the pre_start_hook). We need it only in the child processes.
   *)

let get_levers() =
  match !levers with
    | Some lev -> lev
    | None ->
	failwith "Netmcore: not in container context (worker process)"


let resource_table = Hashtbl.create 5
  (* Master context: maps res_id to master_resource *)

let process_table = Hashtbl.create 5
  (* Master context: maps pid (int) to process_info *)

let initial_process = ref None

let next_pid = ref 0
  (* only in master context *)

let next_resid = ref 0
  (* only in master context *)

let self_pid = ref None
  (* the pid of this worker (if so) *)

let is_worker = ref false
  (* whether this is a worker *)

let enable_pmanage = ref true

let pmanager = ref (Some (Netsys_pmanage.fake_pmanage()))

let create_process_fwd = ref (fun _ _ _ _ _ -> assert false)
  (* defined below *)

let inheritable = [ `Posix_shm_preallocated; `Posix_shm_preallocated_sc ]


let type_of_repr =
  function
    | `File _ -> `File
    | `Posix_shm _ -> `Posix_shm
    | `Posix_shm_sc _ -> `Posix_shm_sc
    | `Posix_shm_preallocated _ -> `Posix_shm_preallocated
    | `Posix_shm_preallocated_sc _ -> `Posix_shm_preallocated_sc
    | `Posix_sem _ -> `Posix_sem
    | `Fork_point _ -> `Fork_point
    | `Join_point _ -> `Join_point



class virtual master_resource_skel res_id typ repr : master_resource =
  let users = ref ExecSet.empty in
  let e = 
    match self_exec() with
      | Some e -> e
      | None -> `Controller in
object(self) 
  method id = res_id
  method typ = typ
  method repr = repr

  method release() =
    self # released_in e

  method used_in (e : executable) =
    users := ExecSet.add e !users

  method released_in (e:executable) =
    users := ExecSet.remove e !users;
    if !users = ExecSet.empty then (
      Hashtbl.remove resource_table res_id;
      self # destroy ()
    )

  method destroy() = ()
  method join_res = assert false
  method post_start _ = ()
  method process_body = assert false
end


let master_start ctrl (fork_res_id,inherit_req,arg) =
  (* [start] from the master process *)
  let `Resource fork_res_id_n = fork_res_id in
  dlogr (fun () -> sprintf "start %d" fork_res_id_n);
  try
    let fork_res = Hashtbl.find resource_table fork_res_id in (* or Not_found *)
    let f = fork_res # process_body in
    let pid = !next_pid in
    incr next_pid;
    let sem_name = sprintf "Netmcore.process_result.%d" pid in
    ignore(Netplex_semaphore.create ~protected:false sem_name 0L);
    let join_res_id = fork_res#join_res in
    !create_process_fwd  f arg (`Process pid) join_res_id inherit_req;
    Some(`Process pid :> process_id)
  with Not_found -> None


let forget_process pid =
  dlogr (fun () -> sprintf "forget_process pid=%d" pid);
  Hashtbl.remove process_table pid;
  let sem_name = sprintf "Netmcore.process_result.%d" pid in
  Netplex_semaphore.destroy sem_name


let is_delivered ctrl (`Process pid) =
  let pi_opt = 
    try Some(Hashtbl.find process_table pid)
    with Not_found -> None in
  match pi_opt with
    | None -> true
    | Some pi -> pi.result <> None


let master_deliver ctrl (`Process pid,res_opt) =
  dlogr (fun () -> sprintf "deliver (lever) pid=%d" pid);
  let pi_opt = 
    try Some(Hashtbl.find process_table pid)
    with Not_found -> None in
  match pi_opt with
    | None -> ()
    | Some pi ->
	pi.result <- Some res_opt
	  (* The notification that the result is available is done by
	     the worker, i.e. the semaphore is increased
	   *)

let master_get_result ctrl (join_point, `Process pid) =
  dlogr (fun () -> sprintf "get_result (lever) pid=%d" pid);
  try
    let info = Hashtbl.find process_table pid in
    if join_point <> info.join_point then raise Not_found;
    let r = info.result in
    if r <> None then
      forget_process pid;
    Some r
  with
    | Not_found -> None


let get_pm() =
  match !pmanager with
    | None ->
        Netplex_cenv.pmanage()
    | Some pm ->
        pm


let pm_register res_repr =
  let pm = get_pm() in
  match res_repr with
    | `File name ->
        pm # register_file name
    | `Posix_shm name ->
        pm # register_posix_shm name
    | `Posix_shm_sc(name,p) ->
        pm # register_posix_shm name;
        pm # register_sem_cont p
    | `Posix_sem name ->
        pm # register_posix_sem name
    | `Posix_shm_preallocated(name,_) ->
        pm # register_posix_shm name
    | `Posix_shm_preallocated_sc(name,_,c) ->
        pm # register_posix_shm name;
        pm # register_sem_cont (Netsys_sem.prefix c)


let pm_unregister res_repr =
  let pm = get_pm() in
  match res_repr with
    | `File name ->
        pm # unregister_file name
    | `Posix_shm name ->
        pm # unregister_posix_shm name
    | `Posix_shm_sc(name,p) ->
        pm # unregister_posix_shm name;
        pm # unregister_sem_cont p
    | `Posix_sem name ->
        pm # unregister_posix_sem name
    | `Posix_shm_preallocated(name,_) ->
        pm # unregister_posix_shm name
    | `Posix_shm_preallocated_sc(name,_,c) ->
        pm # unregister_posix_shm name;
        pm # unregister_sem_cont (Netsys_sem.prefix c)


let manage_resource res_repr post_start exec =
  let res_id = !next_resid in
  incr next_resid;
  dlogr (fun () -> sprintf "manage_resource %d" res_id);
  let typ = type_of_repr res_repr in
  let res =
    ( object
	inherit master_resource_skel 
                  (`Resource res_id) typ (res_repr :> compute_resource_repr)
	method destroy() =
	  ( try
	      match res_repr with
	        | `File name ->
		    Unix.unlink name
	        | `Posix_shm name ->
		    Netsys_posix.shm_unlink name
	        | `Posix_shm_sc(name,p) ->
                    Netsys_sem.unlink p;
		    Netsys_posix.shm_unlink name
	        | `Posix_sem name ->
		    Netsys_posix.sem_unlink name
	        | `Posix_shm_preallocated(name,_) ->
		    Netsys_posix.shm_unlink name
	        | `Posix_shm_preallocated_sc(name,_,c) ->
                    Netsys_sem.unlink (Netsys_sem.prefix c);
		    Netsys_posix.shm_unlink name
	    with error ->
	      Netlog.logf `Err
	        "Unable to destroy resource: %s" (Netexn.to_string error)
          );
          if !enable_pmanage then pm_unregister res_repr;

	method post_start () =
	  post_start ()
      end
    ) in
  Hashtbl.replace resource_table (`Resource res_id) res;
  res # used_in exec;
  if !enable_pmanage then pm_register res_repr;
  (`Resource res_id)


let master_manage_resource ctrl (res_repr, cid) =
  manage_resource res_repr (fun _ -> ()) (`Container cid)


let create_prealloc_shm prefix size value_area exec sem_flag =
  let (fd, name) =
    Netsys_posix.shm_create prefix size in
  let mem =
    Netsys_mem.memory_map_file fd true size in
  Unix.close fd;
  if value_area then
    Netsys_mem.value_area mem;
  dlogr (fun () -> sprintf "create_prealloc_shm %s" name);
  let sc_opt =
    if sem_flag then
      Some(Netsys_sem.create_container name)
    else
      None in
  let post_start _ =
    Netsys_mem.memory_unmap_file mem in
  let res =
    match sc_opt with
      | None -> `Posix_shm_preallocated(name, mem)
      | Some c -> `Posix_shm_preallocated_sc(name, mem, c ) in
  let res_id =
    manage_resource res post_start exec in
  (res_id, name, sc_opt)


let master_create_prealloc_shm ctrl 
                               (prefix,size,value_area,cid,sem_flag) =
  let (res_id, name, sc_opt) =
    create_prealloc_shm prefix size value_area (`Container cid) sem_flag
  in
  (res_id, name, 
   match sc_opt with None -> None | Some c -> Some(Netsys_sem.prefix c)
  )


let master_get_resource ctrl (res_id, cid) =
  let `Resource res_id_n = res_id in
  dlogr (fun () -> sprintf "get_resource (lever) %d" res_id_n);
  try
    let res = Hashtbl.find resource_table res_id in
    res # used_in (`Container cid);
    match res#repr with
      | (`File _|`Posix_shm _|`Posix_shm_sc _|`Posix_sem _) as r ->
	  Some (r :> trans_resource_repr)
      | `Posix_shm_preallocated _ ->
	  Some `Posix_shm_preallocated
      | `Posix_shm_preallocated_sc _ ->
	  Some `Posix_shm_preallocated_sc
      | `Fork_point _ ->
	  Some `Fork_point
      | `Join_point _ ->
	  Some `Join_point
  with Not_found -> None


let master_release ctrl (res_id, cid) =
  let `Resource res_id_n = res_id in
  dlogr (fun () -> sprintf "release %d" res_id_n);
  try
    let res = Hashtbl.find resource_table res_id in
    res # released_in (`Container cid)
  with
    | Not_found -> ()


let maybe_install_levers ctrl =
  let do_install =
    match !levers with
      | None -> true
      | Some lev -> lev.ctrl_id <> Oo.id ctrl in
  if do_install then (
    let lev =
      { ctrl_id = Oo.id ctrl;
	start = Start_lever.register ctrl master_start;
	deliver = Deliver_lever.register ctrl master_deliver;
	get_result = Get_result_lever.register ctrl master_get_result;
	manage_resource = 
	  Manage_resource_lever.register ctrl master_manage_resource;
	create_prealloc_shm =
	  Create_prealloc_shm_lever.register ctrl master_create_prealloc_shm;
	get_resource = Get_resource_lever.register ctrl master_get_resource;
	release = Release_lever.register ctrl master_release;
      } in
    levers := Some lev
  )


let reinit_for_worker inherit_req =
  (* Should be called just after the worker process is forked *)
  is_worker := true;
  Hashtbl.clear process_table;
  (* Get rid of all resources - except inherited resources. *)
  let kept_resources = ref [] in
  Hashtbl.iter
    (fun res_id res ->
       if List.mem res#typ inheritable then (
	 let do_it =
	   match inherit_req with
	     | `Resources l -> List.mem res_id l
	     | `All -> true in
	 if do_it then 
	   kept_resources := (res_id,res) :: !kept_resources
	 else
	   res#post_start ()
       )
    )
    resource_table;
  Hashtbl.clear resource_table;
  List.iter
    (fun (res_id,res) -> Hashtbl.replace resource_table res_id res)
    !kept_resources


let create_process f arg (`Process pid) 
                   join_res_id inherit_req =
  (* Must be run in the master process: Starts a new
     container and runs [f arg] there. [pid] is used
     for creating a unique container name.
   *)
  let ctrl =
    try
      match Netplex_cenv.self_obj() with
	| `Container _ ->
	    failwith "Netmcore.start_worker: not in master context"
	| `Controller ctrl -> ctrl
    with
      | Netplex_cenv.Not_in_container_thread ->
	  failwith "Netmcore.start_worker: not in master context" in
  let name = sprintf "netmcore_%d" pid in
  let sem_name = sprintf "Netmcore.process_result.%d" pid in
  let hooks =
    ( object(self)
	inherit Netplex_kit.empty_processor_hooks()

	method pre_start_hook _ ctrl _ =
	  maybe_install_levers ctrl

	method post_start_hook c =
	  self_pid := Some pid;
	  reinit_for_worker inherit_req;
	  let lev = get_levers() in

	  (* Run the user-supplied function & catch exns *)
	  dlogr (fun () -> sprintf "Start worker pid=%d" pid);
	  ( try
	      let result = f arg in
	      lev.deliver (`Process pid, Some result);
	      ignore(Netplex_semaphore.increment sem_name);
	    with
	      | error ->
		  let bt =
		    if Printexc.backtrace_status() then
		      Printexc.get_backtrace()
		    else "" in
		  Netlog.logf `Err
		    "Exception in worker process %d: %s"
		    pid (Netexn.to_string error);
		  if bt <> "" then
		    Netlog.logf `Err
		      "Backtrace for worker process %d: %s"
		      pid bt;
		  lev.deliver (`Process pid, None);
		  ignore(Netplex_semaphore.increment sem_name);
	  );
	  dlogr (fun () -> sprintf "End worker pid=%d" pid);
	  c # shutdown()

	method post_finish_hook sockserv ctrl cont_id =
	  dlogr (fun () -> sprintf "post_finish_hook pid=%d" pid);
	  if not (is_delivered ctrl (`Process pid)) then (
	    dlogr
	      (fun () -> sprintf "worker terminated abnormally, cleaning up");
	    master_deliver ctrl (`Process pid, None);
	    ignore(Netplex_semaphore.ctrl_increment sem_name cont_id);
	  );
	  Hashtbl.iter
	    (fun res_id res ->
	       res # released_in (`Container (Oo.id cont_id))
	    )
	    resource_table;
	  if not (Hashtbl.mem resource_table join_res_id) then
	    forget_process pid;
	  ( try
	      let (_, sock_ctrl, _) =
		List.find 
		  (fun (sserv, _, _) -> sserv = sockserv) 
		  ctrl#services in
	      sock_ctrl # shutdown()
	    with Not_found ->
	      Netlog.logf `Err
		"Netmcore: socket controller for process not found (pid=%d)" pid
	  );
	  if Some(`Process pid) = !initial_process then (
	    dlog "shutting down";
	    ctrl#shutdown()
	  )
      end
    ) in
  Netplex_kit.add_helper_service ctrl name hooks;
  let pi =
    { pid = `Process pid;
      join_point = join_res_id;
      result = None;
    } in
  Hashtbl.add process_table pid pi


let () =
  create_process_fwd := create_process

	  
let def_process f =
  let e = 
    match self_exec() with
      | Some `Controller -> `Controller
      | None -> `Controller
      | _ ->
	  failwith "Netmcore.def_process: not in master context" in
  let get_ctrl() =
    try
      match Netplex_cenv.self_obj() with
	| `Controller ctrl -> ctrl
	| _ -> raise Not_found
    with
      | Not_found ->
	  failwith "Netmcore: not in master context" in

  let fork_res_id = `Resource !next_resid in
  incr next_resid;
  let join_res_id = `Resource !next_resid in
  incr next_resid;
  
  let fork_repr =
    `Fork_point (fun (inh,arg) -> 
		   let ctrl = get_ctrl() in
		   match master_start ctrl (fork_res_id, inh, arg) with
		     | Some pid -> pid
		     | None -> raise(No_resource fork_res_id)
		) in
  let join_repr =
    `Join_point (fun _ -> failwith "Cannot join workers from the master") in

  let fork_res =
    ( object(self)
	inherit master_resource_skel fork_res_id `Fork_point 
	          (fork_repr :> compute_resource_repr)

	method join_res = join_res_id
	method process_body = f
      end
    ) in

  let join_res =
    ( object(self)
	inherit master_resource_skel join_res_id `Join_point join_repr
      end
    ) in

  Hashtbl.add resource_table fork_res_id fork_res;
  Hashtbl.add resource_table join_res_id join_res;
  fork_res # used_in e;
  join_res # used_in e;
  dlogr (fun () ->
	   let `Resource f_id = fork_res_id in
	   let `Resource j_id = join_res_id in
	   sprintf
	     "def_process: fork_res=%d join_res=%d" 
	     f_id j_id
	);
  (fork_res_id, join_res_id)


let worker_join res_id (`Process pid) =
  dlogr (fun () -> sprintf "worker_join pid=%d" pid);
  let lev = get_levers() in
  match lev.get_result (res_id, `Process pid) with
    | Some (Some res) -> res
    | Some None ->
	(* We know at least that the pid is ok *)
	let sem_name = sprintf "Netmcore.process_result.%d" pid in
	let v = Netplex_semaphore.decrement ~wait:true sem_name in
	dlogr (fun () -> sprintf "worker_join pid=%d sem=%Ld" pid v);
	( match lev.get_result (res_id, `Process pid) with
	    | Some (Some res) -> res
	    | _ ->
		assert false
	)
    | None ->
	failwith "Netmcore: unknown process identifier"


let release res_id =
  match self_exec() with
    | Some(`Container cid) ->
	let lev = get_levers() in
	lev.release (res_id,cid)
    | _ ->
	( try
	    let res = Hashtbl.find resource_table res_id in
	    res#release()
	  with Not_found -> 
	    ()
	)


let get_just_managed res_id repr =
  (* internal: we can assume that the resource exists *)
  try (Hashtbl.find resource_table res_id :> compute_resource)
  with Not_found -> 
    let typ = type_of_repr repr in
    ( object
	method id = res_id
	method typ = typ
	method repr = repr
	method release() = release res_id
      end
    )
	

let get_resource res_id =
  (* In master context, we can simply take the object from 
     [resource_table]. In worker context, we find there only
     inherited resources. For other resource types we have to
     ask the master
   *)
  let `Resource res_id_n = res_id in
  dlogr (fun () -> sprintf "get_resource %d" res_id_n);
  try (Hashtbl.find resource_table res_id :> compute_resource)
  with Not_found -> 
    match self_exec() with
      | Some(`Container cid) ->
	  dlogr (fun () -> 
		   sprintf "get_resource (invoke) %d" res_id_n);
	  (* If this process has been started directly by Netplex and not
	     via [start], it is not yet initialized as worker
	   *)
	  if not !is_worker then
	    reinit_for_worker `All;
	  let lev = get_levers() in
	  ( match lev.get_resource (res_id,cid) with
	      | None -> 
		  raise(No_resource res_id)
	      | Some `Posix_shm_preallocated ->
		  failwith "Netmcore.get_resource: The `Posix_shm_preallocated \
                            resource exists but is not shared with this worker"
	      | Some `Posix_shm_preallocated_sc ->
		  failwith "Netmcore.get_resource: The `Posix_shm_preallocated_sc \
                            resource exists but is not shared with this worker"
	      | Some `Fork_point ->
		  ( object
		      method id = res_id
		      method typ = `Fork_point
		      method repr = 
			`Fork_point (fun (inh,arg) -> 
				       match lev.start(res_id,inh,arg) with
					 | Some pid -> pid
					 | None -> raise(No_resource res_id)
				    )
		      method release() = lev.release (res_id,cid)
		    end
		)
	      | Some `Join_point ->
		  ( object
		      method id = res_id
		      method typ = `Join_point
		      method repr = 
			`Join_point (fun pid -> worker_join res_id pid)
		      method release() = lev.release (res_id,cid)
		    end
		  )
	      | Some (#manage_resource_repr as repr) ->
		  let typ = type_of_repr repr in
		  ( object
		      method id = res_id
		      method typ = typ
		      method repr = repr
		      method release() = lev.release (res_id,cid)
		    end
		  )
	  )
      | _ -> raise(No_resource res_id)

(* API-only stuff *)

let start ?(inherit_resources=`All) fork_res_id arg =
  let r = get_resource fork_res_id in
  match r # repr with
    | `Fork_point f ->
	f (inherit_resources,arg)
    | _ ->
	raise (No_resource fork_res_id)


let join join_res_id pid =
  let r = get_resource join_res_id in
  match r # repr with
    | `Join_point f ->
	f pid
    | _ ->
	raise (No_resource join_res_id)
  
let manage repr =
  let res_id =
    match self_exec() with
      | Some (`Container cid) ->
	  let lev = get_levers() in
	  lev.manage_resource (repr, cid)
      | Some exec ->
	  manage_resource repr (fun _ -> ()) exec
      | None ->
	  failwith "Netmcore.manage_*: unknown context" in
  get_just_managed res_id (repr :> compute_resource_repr)

let manage_file name =
  manage (`File name)

let manage_shm name =
  manage (`Posix_shm name)

let manage_shm_sc name c =
  manage (`Posix_shm_sc(name, Netsys_sem.prefix c))

let manage_sem name =
  manage (`Posix_sem name)

let get_file res_id =
  match (get_resource res_id)#repr with
    | `File name -> name
    | _ ->
	failwith "Netmcore.get_file: the resource is not a file"

let get_shm res_id =
  match (get_resource res_id)#repr with
    | `Posix_shm name -> name
    | `Posix_shm_sc(name,_) -> name
    | `Posix_shm_preallocated(name,_) -> name
    | `Posix_shm_preallocated_sc(name,_,_) -> name
    | _ ->
	failwith "Netmcore.get_shm: the resource is not a shm object"

let get_sem_container res_id =
  match (get_resource res_id)#repr with
    | `Posix_shm_sc(_,p) -> Netsys_sem.container p
    | `Posix_shm_preallocated_sc(_,_,c) -> c
    | _ ->
	failwith "Netmcore.get_shm: the resource is not an object with \
                  semaphore container"

let get_sem res_id =
  match (get_resource res_id)#repr with
    | `Posix_sem name -> name
    | _ ->
	failwith "Netmcore.get_sem: the resource is not a semaphore"
  
let create_preallocated_shm ?(value_area=false) prefix size =
  match self_exec() with
    | Some (`Container cid) ->
	let lev = get_levers() in
	let (res_id,name,_) = 
          lev.create_prealloc_shm (prefix,size,value_area,cid,false) in
        (res_id,name)
    | Some exec ->
	let (res_id,name,_) =
          create_prealloc_shm prefix size value_area exec false in
        (res_id,name)
    | None ->
	let (res_id,name,_) =
	  create_prealloc_shm prefix size value_area `Controller false in
        (res_id,name)

    
let create_preallocated_shm_sc ?(value_area=false) prefix size =
  match self_exec() with
    | Some (`Container cid) ->
	let lev = get_levers() in
	let (res_id,name,p_opt) = 
          lev.create_prealloc_shm (prefix,size,value_area,cid, true) in
        ( match p_opt with
            | None -> assert false
            | Some p ->
                let c = Netsys_sem.container p in
                (res_id,name,c)
        )
    | Some exec ->
	let (res_id,name,sc_opt) =
          create_prealloc_shm prefix size value_area exec true in
        ( match sc_opt with
            | None -> assert false
            | Some c -> (res_id,name,c)
        )
    | None ->
	let (res_id,name,sc_opt) =
	  create_prealloc_shm prefix size value_area `Controller true in
        ( match sc_opt with
            | None -> assert false
            | Some c -> (res_id,name,c)
        )

    
let self_process_id() =
  match !self_pid with
    | None ->
       ( match self_exec() with
	   | Some (`Container cid) ->
	       failwith "Netmcore.self_process_id: This worker has not been \
                         started via Netmcore.start, and does not have a \
                         process ID"
	   | _ ->
	       failwith "Netmcore.self_process_id: not in worker context"
       )
    | Some pid ->
	`Process pid


let add_plugins ctrl =
  ctrl # add_plugin Netplex_semaphore.plugin


let destroy_resources () =
  let old = !enable_pmanage in
  enable_pmanage := false;
  Hashtbl.iter
    (fun res_id res ->
       res # destroy()
    )
    resource_table;
  enable_pmanage := old;
  if old then (
    match !pmanager with
      | None -> ()
      | Some pm -> pm # unlink()
  )


let startup ~socket_directory ?pidfile ?(init_ctrl=fun _ -> ()) 
            ?(disable_pmanage=false) ?(no_unlink=false) ~first_process
            () =
  let config_tree =
    `Section("netplex",
	     [ `Section("controller",
			[ `Parameter ("socket_directory", 
				      `String socket_directory);
			  `Section("logging",
				   [ `Parameter("type", `String "stderr") ]
				  );
			]
		       )
	     ]
	    ) in
  let conf =
    Netplex_main.create
      ~pidfile
      ~foreground:true
      ~config_tree
      () in
  Netplex_main.startup
    ~late_initializer:(fun cf ctrl ->
			 add_plugins ctrl;
			 init_ctrl ctrl;
                         enable_pmanage := not disable_pmanage;
                         ( match !pmanager with
                             | None -> ()
                             | Some pm ->
                                 let real_pm = Netplex_cenv.pmanage() in
                                 if not no_unlink then
                                   real_pm#unlink();
                                 real_pm # register (pm # registered);
                                 pmanager := Some real_pm
                         );
			 let pid = first_process() in
			 initial_process := Some pid;
		      )
    ( Netplex_mp.mp ~keep_fd_open:true ~terminate_tmo:(-1) () )
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    [ ]
    conf

