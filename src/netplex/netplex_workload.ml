(* $Id: netplex_workload.ml 1718 2012-02-21 14:59:45Z gerd $ *)

open Netplex_types
open Printf

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netplex_workload" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netplex_workload" Debug.enable

let () =
  Netlog.Debug.register_module "Netplex_workload" Debug.enable

class constant_workload_manager ?(restart=true) ?(greedy_accepts=false)
                                ?(max_jobs_per_thread = max_int)
                                num_threads : workload_manager =
object(self)
  val mutable allow_adjust = true

  method hello controller =
    ()
      (* TODO: Announce the availability of admin messages *)

  method shutdown() =
    ()
      (* TODO *)

  method adjust sockserv sockctrl =
    match sockctrl # state with
      | `Enabled ->
	  if allow_adjust then (
	    let l = sockctrl # container_state in
	    let n = List.length l in
	    if n < num_threads then (
	      let _n_started =
		sockctrl # start_containers (num_threads - n) in
	      (* If less containers could be started, we ignore the problem.
                 [adjust] will be called again, and the problem will be fixed.
                 Hopefully... We cannot do much more here.
               *)
	      ()
	    );
	    if not restart then allow_adjust <- false
	  )
      | `Disabled ->
	  allow_adjust <- true
      | _ ->
	  ()

  method capacity cid s =
    match s with
      | `Accepting(n,_) -> 
	  if n >= max_jobs_per_thread then
	    `Unavailable
	  else
	    `Normal_quality (max_jobs_per_thread - n, greedy_accepts)
      | `Busy -> `Unavailable
      | `Starting _ -> `Unavailable
      | `Shutting_down -> `Unavailable

end


let create_constant_workload_manager =
  new constant_workload_manager


let constant_workload_manager_factory =
object
  method name = "constant"
  method create_workload_manager ctrl_cfg cf addr =
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr
      [ "type"; "jobs"; "threads"; "max_jobs_per_thread"; "greedy_accepts" ];
    let n =
      try
	cf # int_param
	  (cf # resolve_parameter addr "threads")
      with
	| Not_found ->
	    ( try
		cf # int_param
		  (cf # resolve_parameter addr "jobs")
		  (* Accept [jobs] for some time *)
	      with
		| Not_found ->
		    failwith ("Constant workload manager needs parameter 'threads'")
	    ) in
    if n < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".threads must be > 0");
    let greedy_accepts =
      try cf # bool_param (cf#resolve_parameter addr "greedy_accepts")
      with Not_found -> false in
    let max_jobs_per_thread =
      try
	Some(cf # int_param
	       (cf # resolve_parameter addr "max_jobs_per_thread"))
      with
	| Not_found ->
	    None in
    create_constant_workload_manager ~greedy_accepts ?max_jobs_per_thread n
end



class type dynamic_workload_config =
object
  method max_jobs_per_thread : int
  method recommended_jobs_per_thread : int
  method min_free_job_capacity : int
  method max_free_job_capacity : int
  method inactivity_timeout : int
  method max_threads : int
  method greedy_accepts : bool
end


module ContId = struct
  type t = container_id
  let (compare : t -> t -> int) = Pervasives.compare
end


module ContMap = Map.Make(ContId)


let containers_by_attractivity container_state =
  (* Containers sorted by the number of jobs they execute *)
  let weight s =
    match s with
      | `Accepting(n,_) -> n
      | `Starting _ -> 0
      | `Busy -> max_int
      | `Shutting_down -> max_int in 
  List.sort
    (fun (cid1,_,s1,_) (cid2,_,s2,_) ->
       let n1 = weight s1 in
       let n2 = weight s2 in
       n1 - n2)
    container_state



class dynamic_workload_manager config : workload_manager =
object(self)
  val mutable esys = lazy(assert false)
  val mutable logger = lazy(assert false)

  val mutable inactivated_conts = ContMap.empty
    (* Maps container_ids to Unixqueue groups. These containers are scheduled
     * for inactivation, and are going to be shut down. The group, if present,
     * refers to the inactivity timer.
     *
     * Note that these containers may be idle or busy. Inactivation means
     * that they won't be selected by the scheduler again, and that it is
     * hoped that they become idle soon. It is possible that these containers
     * are reactivated again if the load goes up.
     *
     * When these containers are finally idle, the inactivity timer is started.
     * If they remain idle, they will be shut down when the timer expires.
     *)

  val mutable limit_alert = false
  val mutable last_limit_alert = 0.0

  method hello controller =
    esys <- lazy(controller # event_system);
    logger <- lazy(controller # logger);
    ()

  method shutdown() =
    ContMap.iter
      (fun _ g_opt -> 
	 match g_opt with
	   | None -> ()
	   | Some g -> Unixqueue.clear (Lazy.force esys) g)
      inactivated_conts;
    inactivated_conts <- ContMap.empty;
    ()

  method adjust (sockserv : socket_service) (sockctrl : socket_controller) =
    match sockctrl # state with
      | `Enabled ->
	  (* Determine total capacity of the current Netplex state: *)
	  let container_state = sockctrl # container_state in
	  let all_threads = List.length container_state in
	  let active_threads =
	    List.length
	      (List.filter 
		 (fun (cid,_,s,_) -> 
		    not (ContMap.mem cid inactivated_conts) 
		    && s <> `Shutting_down) 
		 container_state) in
	  let total_cap = 
	    config#recommended_jobs_per_thread * active_threads in

	  (* Determine used capacity: *)
	  let used_cap =
	    List.fold_left
	      (fun acc (cid,_,s,_) ->
		 if ContMap.mem cid inactivated_conts then
		   acc
		 else
		   match s with
		     | `Accepting(n,_) -> 
			 acc + (min n config#recommended_jobs_per_thread)
		     | `Busy -> acc + config#recommended_jobs_per_thread
		     | `Starting _ -> acc
		     | `Shutting_down -> acc)
	      0
	      container_state in

	  (* Free capacity: *)
	  let free_cap = total_cap - used_cap in

	  dlogr
	    (fun () ->
	       sprintf
		 "Service %s: \
                  total_threads=%d avail_threads=%d total_cap=%d used_cap=%d"
		 sockserv#name all_threads active_threads
		 total_cap used_cap
	    );

	  (* Now decide... *)
	  if free_cap < config#min_free_job_capacity then (
	    let needed_cap = config#min_free_job_capacity - free_cap in
	    self # activate_containers sockserv sockctrl all_threads needed_cap
	  )
	  else 
	    if free_cap > config#max_free_job_capacity then (
	      let exceeding_cap = free_cap - config#max_free_job_capacity in
	      let exceeding_threads = 
		exceeding_cap / config # recommended_jobs_per_thread in
	      if exceeding_threads > 0 then (
		(* Try to stop exceeding_thread containers. Look for
                 * the containers with the least numbers of jobs.
                 *)
		let sorted_conts = 
		  containers_by_attractivity container_state in
		let n = ref 0 in
		List.iter
		  (fun (cid,_,s,selected) ->
		     let already_inactivated =
		       ContMap.mem cid inactivated_conts in
		     if !n < exceeding_threads && not already_inactivated 
		        && not selected
		     then (
		       match s with
			 | `Accepting(_,_)
			 | `Starting _ ->
			     incr n;
			     self # inactivate_container sockserv sockctrl cid
			 | _ -> ()
		     )
		  )
		  sorted_conts
	      )
	    );

	  self # inactivation_check sockserv sockctrl

      | _ ->
	  self # inactivation_check sockserv sockctrl


  method private activate_containers sockserv sockctrl all_threads needed_cap =
    (* n is the capacity still needed. The method (re)claims resources
     * and decreases n until it is 0 or negative.
     *)
    let n = ref needed_cap in
    let n_overload = ref needed_cap in  (* includes overload capacity *)
    (* First re-activate the inactivated containers. Look at all containers,
     * sort them by attractivity, and try to reactivate the most attractive
     * inactive containers. In this pass, we only look at containers which
     * are not yet overloaded.
     *)
    let container_state = sockctrl # container_state in
    let sorted_conts = 
      containers_by_attractivity container_state in
    let l = ref [] in
    List.iter
      (fun (cid, _, s, selected) ->
	 try
	   if !n <= 0 then raise Not_found;
	   let g_opt = ContMap.find cid inactivated_conts in
	   let cap, ocap =
	     match s with
	       | `Accepting(m,_) ->
		   let d = config#recommended_jobs_per_thread - m in
		   let od = max 0 (config#max_jobs_per_thread - m) in
		   if d > 0 then (* not overloaded *)
		     d, od
		   else
		     (-1), od (* do not consider these in this pass *)
	       | `Starting _ ->
		   (config#recommended_jobs_per_thread,
		    config#max_jobs_per_thread)
	       | _ -> (-1), 0 (* do not consider these *) in
	   if cap >= 0 then (
	     n := !n - cap;
	     n_overload := !n_overload - ocap;
	     l := cid :: !l;
	     match g_opt with
	       | None -> ()
	       | Some g ->
		   Unixqueue.clear (Lazy.force esys) g;
	   )
	 with
	   | Not_found -> ()
      )
      sorted_conts;
    List.iter
      (fun cid ->
	 inactivated_conts <- ContMap.remove cid inactivated_conts)
      !l;
    if !l <> [] then
      dlogr
	(fun () ->
	   sprintf
	     "Service %s: Reclaiming %d inactivated containers"
	     sockserv#name (List.length !l));
    (* Second pass: If needed, start further containers: *)
    let started_ocap = ref 0 in
    if !n > 0 then (
      let needed_threads =
	(!n-1) / config#recommended_jobs_per_thread + 1 in
      let needed_threads' =
	min (max 0 (config#max_threads - all_threads)) needed_threads in
      let started_threads = sockctrl # start_containers needed_threads' in
      (* If started_threads < needed_threads', we ignore the problem. *)
      let cap = started_threads * config#recommended_jobs_per_thread in
      let ocap = started_threads * config#max_jobs_per_thread in
      n := !n - cap;
      n_overload := !n_overload - ocap;
      started_ocap := !started_ocap + ocap;
    );
    (* Third pass: Also reactivate overloaded containers *)
    if !n_overload > 0 then (
      (* We take n_overload because this number is based on
       * max_jobs_per_thread.
       *)
      let l = ref [] in
      List.iter
	(fun (cid, _, s, selected) ->
	   try
	     if !n_overload <= 0 then raise Not_found;
	     let g_opt = ContMap.find cid inactivated_conts in
	     let cap =
	       match s with
		 | `Accepting(m,_) ->
		   let d = config#max_jobs_per_thread - m in
		   if d > 0 then
		     d
		   else
		     (-1) (* do not consider these in this pass *)
		 | _ -> (-1) (* do not consider these *) in
	     if cap >= 0 then (
	       n_overload := !n_overload - cap;
	       l := cid :: !l;
	       match g_opt with
		 | None -> ()
		 | Some g ->
		     Unixqueue.clear (Lazy.force esys) g;
	     )
	   with
	     | Not_found -> ()
	)
	sorted_conts;
      List.iter
	(fun cid ->
	   inactivated_conts <- ContMap.remove cid inactivated_conts)
	!l;
      if !l <> [] then
	dlogr
	  (fun () ->
	     sprintf "Service %s: \
                     Reclaiming %d inactivated but overloaded containers"
	       sockserv#name (List.length !l)
	  );
    );
    (* Check whether we reach the capacity limit. *)
    let limit_reached =
      !n_overload > 0 && (
	(* [!n_overload > 0] only means we cannot start enough containers to
         * ensure the required capacity. But it is possible that the running
         * containers provide enough capacity nevertheless.
         *)
	let avail_cap = ref !started_ocap in
	List.iter
	  (fun (_, _, s, _) ->
	     match s with
	       | `Accepting(m,_) ->
		   let od = max 0 (config#max_jobs_per_thread - m) in
		   avail_cap := !avail_cap + od
	       | `Starting _ ->
		   avail_cap := !avail_cap + config#max_jobs_per_thread
	       | _ -> ()
	  )
	  sorted_conts;
	needed_cap > !avail_cap
      ) in
    (* Output a capacity alert, but not more often than every 60 seconds: *)
    if limit_reached && not limit_alert then (
      let now = Unix.gettimeofday() in
      if now >= last_limit_alert +. 60.0 then (
	(Lazy.force logger) # log 
	  ~component:sockserv#name
	  ~level:`Alert
	  ~message:"Dyn workload mng: Reaching configured capacity limit";
	limit_alert <- true;
	last_limit_alert <- now;
      )
    );
    if not limit_reached then limit_alert <- false

	(* [CHECK whether this is still true:]
         * Note that the activation may not do enough because inactivated
         * containers can be quite busy. The next [adjust] call will fix
         * this.
         *)


  method private inactivate_container sockserv sockctrl cid =
    inactivated_conts <- ContMap.add cid None inactivated_conts;
    limit_alert <- false;

    dlogr
      (fun () ->
	 sprintf "Service %s: Inactivating 1 container"
	   sockserv#name
      )


  method private inactivation_check sockserv sockctrl =
    (* Check whether there are inactivated containers without timer that
     * have become idle in the meantime. For these containers, start the
     * inactivation timer.
     *)
    let container_state = sockctrl # container_state in
    List.iter
      (fun (cid, _, s, selected) ->
	 try
	   let g_opt = ContMap.find cid inactivated_conts in
	   assert(not selected);
	   (* Well, [not selected] means only the container was not selected
            * at the time of inactivation. However, we know that the scheduler
            * won't select the container again because [capacity] returns
            * [`Unavailable] for it.
            *)
	   match (g_opt, s) with
	     | None, `Accepting(0,_) ->
		 dlogr
		   (fun () ->
		      sprintf "Service %s: Inactivated container becomes idle"
			sockserv#name
		   );
		 let esys = Lazy.force esys in
		 let g = Unixqueue.new_group esys in
		 Unixqueue.once
		   esys g (float config#inactivity_timeout)
		   (fun () ->
		      inactivated_conts <- ContMap.remove cid inactivated_conts;
		      sockctrl # stop_containers [cid]
		   );
		 inactivated_conts <- ContMap.add cid (Some g) inactivated_conts
	     | _ ->
		 ()
	 with
	   | Not_found -> ()
      )
      container_state



  method capacity cid s =
    let g = config#greedy_accepts in
    if ContMap.mem cid inactivated_conts then 
      `Unavailable  (* because we want to shut cid down *)
    else
      match s with
	| `Accepting(n,_) ->
	    if n < config # max_jobs_per_thread then
	      if n < config # recommended_jobs_per_thread then
		`Normal_quality (config # recommended_jobs_per_thread - n, g)
	      else
		`Low_quality (config # max_jobs_per_thread - n, g)
	    else
	      `Unavailable

	| `Busy -> `Unavailable
	| `Starting _ -> `Unavailable
	| `Shutting_down -> `Unavailable


end


let create_dynamic_workload_manager config =
  new dynamic_workload_manager config


let dynamic_workload_manager_factory =
object
  method name = "dynamic"
  method create_workload_manager ctrl_cfg cf addr =
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr [ "type"; "max_jobs_per_thread";
				    "recommended_jobs_per_thread";
				    "min_free_jobs_capacity";
				    "max_free_jobs_capacity";
				    "inactivity_timeout";
				    "max_threads"; "greedy_accepts";
				  ];
    let max_jobs_per_thread =
      try
	cf # int_param
	  (cf # resolve_parameter addr "max_jobs_per_thread")
      with
	| Not_found ->
	    1 in
    let recommended_jobs_per_thread =
      try
	cf # int_param
	  (cf # resolve_parameter addr "recommended_jobs_per_thread")
      with
	| Not_found ->
	    max_jobs_per_thread in
    let min_free_job_capacity =
      try
	cf # int_param
	  (cf # resolve_parameter addr "min_free_jobs_capacity")
      with
	| Not_found ->
	    failwith "Dynamic workload manager needs parameter 'min_free_jobs_capacity'" in
    let max_free_job_capacity =
      try
	cf # int_param
	  (cf # resolve_parameter addr "max_free_jobs_capacity")
      with
	| Not_found ->
	    failwith "Dynamic workload manager needs parameter 'max_free_jobs_capacity'" in
    let max_threads =
      try
	cf # int_param
	  (cf # resolve_parameter addr "max_threads")
      with
	| Not_found ->
	    failwith "Dynamic workload manager needs parameter 'max_threads'" in
    let inactivity_timeout =
      try
	cf # int_param
	  (cf # resolve_parameter addr "inactivity_timeout")
      with
	| Not_found ->
	    15 in
    let greedy_accepts =
      try cf # bool_param (cf#resolve_parameter addr "greedy_accepts")
      with Not_found -> false in

    if max_jobs_per_thread < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".max_jobs_per_thread must be > 0");
    if recommended_jobs_per_thread < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".recommended_jobs_per_thread must be > 0");
    if recommended_jobs_per_thread > max_jobs_per_thread then
      failwith ("Parameter " ^ cf#print addr ^ ".recommended_jobs_per_thread must be <= max_jobs_per_thread");
    if min_free_job_capacity < 0 then
      failwith ("Parameter " ^ cf#print addr ^ ".min_free_job_capacity must be >= 0");
    if max_free_job_capacity < min_free_job_capacity then
      failwith ("Parameter " ^ cf#print addr ^ ".max_free_job_capacity must be >= min_free_job_capacity");
    if max_threads < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".max_threads must be > 0");
    if inactivity_timeout < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".inactivity_tieout must be > 0");

    let cfg =
      ( object
	  method max_jobs_per_thread = max_jobs_per_thread
	  method recommended_jobs_per_thread = recommended_jobs_per_thread
	  method min_free_job_capacity = min_free_job_capacity
	  method max_free_job_capacity = max_free_job_capacity
	  method inactivity_timeout = inactivity_timeout
	  method max_threads = max_threads
	  method greedy_accepts = greedy_accepts
	end
      ) in

    create_dynamic_workload_manager cfg
end


let workload_manager_factories =
  [ constant_workload_manager_factory;
    dynamic_workload_manager_factory
  ]
