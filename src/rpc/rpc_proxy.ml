(* $Id: rpc_proxy.ml 1557 2011-03-04 14:43:05Z gerd $ *)

(* TODO: logging callback for disabled services *)

module ReliabilityCache = struct

  type rcache_policy =
      [ `Independent
      | `Failing_port_disables_host of int
      | `Any_failing_port_disables_host
      | `None
      ]

  type rcache_config =
      { rcache_policy : rcache_policy;
	rcache_disable_timeout_min : float;
	rcache_disable_timeout_max : float;
	rcache_threshold : int;
	rcache_availability : rcache -> Unix.sockaddr -> bool;
      }

  and entry =
      { mutable error_counter : int;
	mutable disabled_until : float option;
	mutable disable_timeout : float;
      }	

  and host_entry =
      { mutable host_disabled_until : float option;
	mutable host_socks : (Unix.sockaddr, unit) Hashtbl.t
      }	

  and rcache =
      { mutex : Netsys_oothr.mutex;
	config : rcache_config;
	ports : (Unix.sockaddr, entry) Hashtbl.t;
	hosts : (Unix.inet_addr, host_entry) Hashtbl.t;
	parent : rcache option;
      }

  let create_rcache_config ?(policy = `None)
                           ?(disable_timeout_min = 1.0)
			   ?(disable_timeout_max = 64.0)
			   ?(threshold = 1)
			   ?(availability = fun _ _ -> true)
			   () =
    { rcache_policy = policy;
      rcache_disable_timeout_min = disable_timeout_min;
      rcache_disable_timeout_max = disable_timeout_max;
      rcache_threshold = threshold;
      rcache_availability = availability;
    }

  let create_rcache cfg =
    { mutex = !Netsys_oothr.provider # create_mutex ();
      config = cfg;
      ports = Hashtbl.create 10;
      hosts = Hashtbl.create 10;
      parent = None
    }

  let g_rcache_config =
    ref (create_rcache_config
	   (* ~policy:`Independent
	   ~disable_timeout_max:1.0 *)
	   ())

  let g_rcache_config_modifiable = ref true

  let global_rcache_config() = 
    g_rcache_config_modifiable := false;
    !g_rcache_config

  let set_global_rcache_config cfg =
    if not !g_rcache_config_modifiable then
      failwith "Cannot change config of global ReliabiltyCache after first use";
    g_rcache_config := cfg

  let g_rcache = lazy (
    create_rcache (global_rcache_config())
  )

  let global_rcache() =
    Lazy.force g_rcache

  let derive_rcache rc cfg =
    let rc' = create_rcache cfg in
    { rc' with parent = Some rc }


  let rcache_config rc = rc.config

  let get_entry rc sa =
    try Hashtbl.find rc.ports sa
    with Not_found ->
      let new_e =
	{ error_counter = 0;
	  disabled_until = None;
	  disable_timeout = rc.config.rcache_disable_timeout_min
	} in
      Hashtbl.add rc.ports sa new_e;
      new_e

  let get_host_entry rc ip =
    try Hashtbl.find rc.hosts ip
    with Not_found ->
      let new_e =
	{ host_disabled_until = None;
	  host_socks = Hashtbl.create 1;
	} in
      Hashtbl.add rc.hosts ip new_e;
      new_e


  let ip_of_sa sa =
    match sa with
      | Unix.ADDR_INET(ip,_) -> ip
      | Unix.ADDR_UNIX _ -> Unix.inet_addr_loopback


  let local_ip ip =
    ip = Unix.inet_addr_loopback || ip = Unix.inet6_addr_loopback


  let rec incr_rcache_error_counter rc sa =
    Netsys_oothr.serialize rc.mutex
      (fun () ->
	 let e = get_entry rc sa in
	 e.error_counter <- e.error_counter + 1;
	 let now = Unix.gettimeofday() in
	 if e.error_counter >= rc.config.rcache_threshold then (
	   let disable_p =
	     rc.config.rcache_policy <> `None &&
	       match e.disabled_until with
		 | None -> true
		 | Some t -> t < now in
	   if disable_p then (
	     let until = now +. e.disable_timeout in
	     e.disabled_until <- 
	       Some until;
	     e.disable_timeout <- 
	       min 
	         (2. *. e.disable_timeout) 
	         rc.config.rcache_disable_timeout_max;
	     let ip = ip_of_sa sa in
	     let disable_host_p =
	       not(local_ip ip)
	       && (match rc.config.rcache_policy with
		     | `None -> assert false
		     | `Independent -> false
		     | `Failing_port_disables_host p -> 
			 ( match sa with
			     | Unix.ADDR_INET(_,p') -> p=p'
			     | _ -> false
			 )
		     | `Any_failing_port_disables_host -> true
		  ) in
	     if disable_host_p then (
	       let he = get_host_entry rc ip in
	       let t1 =
		 match he.host_disabled_until with
		   | None -> until
		   | Some t -> max t until in
	       he.host_disabled_until <- Some t1;
	       Hashtbl.replace he.host_socks sa ();
	     )
	   )
	 )
      )
      ();
    match rc.parent with
      | None -> ()
      | Some rc' -> incr_rcache_error_counter rc' sa

  let rec reset_rcache_error_counter rc sa =
    Netsys_oothr.serialize rc.mutex
      (fun () ->
	 if Hashtbl.mem rc.ports sa then (
	   Hashtbl.remove rc.ports sa;
	   let ip = ip_of_sa sa in
	   if not(local_ip ip) then (
	     let now = Unix.gettimeofday() in
	     let he = get_host_entry rc ip in
	     Hashtbl.remove he.host_socks sa;
	     let t1_opt = 
	       Hashtbl.fold
		 (fun sa1 _ acc ->
		    let e = get_entry rc sa1 in
		    if e.error_counter >= rc.config.rcache_threshold then
		      match e.disabled_until with
			| None -> acc
			| Some t -> 
			    if t < now then None else (
			      match acc with
				| None -> Some t
				| Some t_acc -> Some(max t t_acc)
			    )
		    else
		      acc
		 )
		 he.host_socks
		 None in
	     match t1_opt with
	       | None ->
		   Hashtbl.remove rc.hosts ip
	       | Some t1 ->
		   he.host_disabled_until <- Some t1
	   )
	 )
      )
      ();
    match rc.parent with
      | None -> ()
      | Some rc' -> reset_rcache_error_counter rc' sa

  let host_is_enabled_for rc ip now =
    try
      let he = Hashtbl.find rc.hosts ip in
      match he.host_disabled_until with
	| None -> true
	| Some t1 -> t1 < now
    with
      | Not_found -> true

  let rec host_is_enabled_1 rc ip now =
    Netsys_oothr.serialize rc.mutex
      (fun () ->
	 host_is_enabled_for rc ip now
      )
      () &&
      ( match rc.parent with
	  | None -> true
	  | Some rc' -> host_is_enabled_1 rc' ip now
      )

  let host_is_enabled rc ip =
    let now = Unix.gettimeofday() in
    host_is_enabled_1 rc ip now
     

  let rec sockaddr_is_enabled_1 rc sa now =
    rc.config.rcache_availability rc sa &&
      Netsys_oothr.serialize rc.mutex
      (fun () ->
	 let ip = ip_of_sa sa in
	 host_is_enabled_for rc ip now
	 && ( try
		let e = Hashtbl.find rc.ports sa in
		match e.disabled_until with
		  | None -> true
		  | Some t1 -> t1 < now
	      with
		| Not_found -> true
	    )
      )
      () &&
      ( match rc.parent with
	  | None -> true
	  | Some rc' -> sockaddr_is_enabled_1 rc' sa now
      )

  let sockaddr_is_enabled rc sa =
    let now = Unix.gettimeofday() in
    sockaddr_is_enabled_1 rc sa now

end



module ManagedClient = struct

  exception Service_unavailable

  type mclient_config =
      { mclient_rcache : ReliabilityCache.rcache;
	mclient_socket_config : Rpc_client.socket_config;
	mclient_idle_timeout : float;
	mclient_programs : Rpc_program.t list;
	mclient_msg_timeout : float;
	mclient_msg_timeout_is_fatal : bool;
	mclient_exception_handler : (exn -> unit) option;
	mclient_auth_methods : Rpc_client.auth_method list;
	mclient_user_name : string option;
	mclient_initial_ping : bool;
	mclient_max_response_length : int option;
	mclient_mstring_factories : Xdr_mstring.named_mstring_factories option;
      }

  type state = [ `Down | `Connecting | `Up of Unix.sockaddr option]

  type up_state =
      { client : Rpc_client.t;
	serial : int;
	mutable idle_timer : Unixqueue.group option;
	mutable unavailable : bool;
	mutable fatal_error : bool;
      }

  type conn_state =
      { c_client : Rpc_client.t;
	c_serial : int;
	c_when_up : (up_state -> unit) Queue.t;
	c_when_fail : (exn -> unit) Queue.t;
	mutable c_unavailable : bool;
	mutable c_fatal_error : bool;
      }

  type extended_state =
      [ `Down of int   (* serial *)
      | `Connecting of conn_state
      | `Up of up_state
      ]

  type mclient =
      { id : < >;      (* for comparing clients, used in ManagedSet *)
	config : mclient_config;
	conn : Rpc_client.connector;
	esys : Unixqueue.event_system;
	null_proc_name : string;
	mutable estate : extended_state;
	mutable next_batch_call : bool;
	mutable pending_calls : int;
	mutable pending_calls_callback : int -> int -> unit;
	(* Called when pending_calls is changed, with old and new value.
           Also called on state change.
	 *)
	mutable next_serial : int;
      }
	
  type t = mclient
      (* for USE_CLIENT compatibility *)

  let get_null_proc_name config =
     if config.mclient_initial_ping then
	match Rpc_program.null_proc_name (List.hd config.mclient_programs) with
	  | None -> failwith "Rpc_proxy.ManagedClient.create_mclient_config: \
                              The program does not have a null procedure"
	  | Some n -> n 
      else
	""

  let create_mclient_config
        ?(rcache = ReliabilityCache.global_rcache())
	?(socket_config = Rpc_client.default_socket_config)
	?(idle_timeout = (-1.0))
	?(programs = [])
	?(msg_timeout = (-1.0))
	?(msg_timeout_is_fatal = false)
	?exception_handler
	?(auth_methods = [])
        ?(user_name = None)
 	?(initial_ping = false)
	?max_response_length
	?mstring_factories
	() =
    if initial_ping && programs = [] then
      failwith
	"Rpc_proxy.ManagedClient.create_mclient_config: \
         need a program for initial ping";
    let config =
      { mclient_rcache = rcache;
	mclient_socket_config = socket_config;
	mclient_idle_timeout = idle_timeout;
	mclient_programs = programs;
	mclient_msg_timeout = msg_timeout;
	mclient_msg_timeout_is_fatal = msg_timeout_is_fatal;
	mclient_exception_handler = exception_handler;
	mclient_auth_methods = auth_methods;
	mclient_user_name = user_name;
	mclient_initial_ping = initial_ping;
	mclient_max_response_length = max_response_length;
	mclient_mstring_factories = mstring_factories;
      } in
    ignore(get_null_proc_name config);
    config


  let sockaddr_of_conn conn =
    match conn with
      | Rpc_client.Internet(ip,p) ->
	  Unix.ADDR_INET(ip,p)
      | Rpc_client.Unix p ->
	  Unix.ADDR_UNIX p
      | _ ->
	  failwith
	    "Rpc_proxy.ManagedClient.create_mclient: Unsupported connector"

  let create_mclient config conn esys =
    ignore(sockaddr_of_conn conn);
    let null_proc_name = get_null_proc_name config in
    { id = (object end);
      config = config;
      conn = conn;
      esys = esys;
      estate = `Down 0;
      next_batch_call = false;
      null_proc_name = null_proc_name;
      pending_calls = 0;
      pending_calls_callback = (fun _ _ -> ());
      next_serial = 1;
    }
        
  let mclient_state mc =
    match mc.estate with
      | `Down _ -> `Down
      | `Connecting _ -> `Connecting
      | `Up up -> `Up (try Some(Rpc_client.get_socket_name up.client) 
		       with _ -> None)

  let mclient_serial mc =
    match mc.estate with
      | `Down serial -> serial
      | `Connecting c -> c.c_serial
      | `Up up -> up.serial

  let compare mc1 mc2 =
    Pervasives.compare mc1.id mc2.id

  let pending_calls mc =
    mc.pending_calls

  let change_pending_calls mc delta =
    let old_n = mc.pending_calls in
    let new_n = old_n + delta in
    mc.pending_calls_callback old_n new_n;
    mc.pending_calls <- new_n

  let if_up mc f =
    match mc.estate with
      | `Down _
      | `Connecting _ -> ()
      | `Up up -> f up

  let if_connecting_or_up mc f =
    match mc.estate with
      | `Down _ -> ()
      | `Connecting c -> f c.c_client
      | `Up up -> f up.client

  let cancel_idle_timer mc up =
    match up.idle_timer with
      | None -> ()
      | Some g -> Unixqueue.clear mc.esys g

  let next_serial mc =
    let s = mc.next_serial in
    mc.next_serial <- s + 1;
    s

  let shut_down mc =
    if_connecting_or_up mc
      (fun client -> Rpc_client.shut_down client);
    if_up mc
      (fun up -> cancel_idle_timer mc up);
    mc.estate <- `Down (next_serial mc);
    change_pending_calls mc 0

  let sync_shutdown mc =
    if_connecting_or_up mc
      (fun client -> Rpc_client.sync_shutdown client);
    if_up mc
      (fun up -> cancel_idle_timer mc up);
    mc.estate <- `Down (next_serial mc);
    change_pending_calls mc 0

  let trigger_shutdown mc f =
    if_connecting_or_up mc
      (fun client -> Rpc_client.trigger_shutdown client f);
    if_up mc
      (fun up -> cancel_idle_timer mc up);
    mc.estate <- `Down (next_serial mc);
    change_pending_calls mc 0

  let fatal_error mc =
    (* We count a failed mclient only once *)
    match mc.estate with
      | `Down _ -> ()
      | `Connecting c ->
	  if not c.c_fatal_error then (
	    c.c_fatal_error <- true;
	    ReliabilityCache.incr_rcache_error_counter 
	      mc.config.mclient_rcache
	      (sockaddr_of_conn mc.conn)
	  )
      | `Up up ->
	  if not up.fatal_error then (
	    up.fatal_error <- true;
	    ReliabilityCache.incr_rcache_error_counter 
	      mc.config.mclient_rcache
	      (sockaddr_of_conn mc.conn)
	  )

  let record_unavailability mc =
    fatal_error mc

  let enforce_unavailability mc =
    fatal_error mc;
    ( match mc.estate with
	| `Down _ -> ()
	| `Connecting c ->
	    c.c_unavailable <- true
	| `Up up ->
	    up.unavailable <- true
    );
    trigger_shutdown mc (fun () -> ())

  let set_batch_call mc =
    mc.next_batch_call <- true

  let event_system mc =
    mc.esys

  let use mc prog =
    let prog_id = Rpc_program.id prog in
    if not(List.exists 
	     (fun p -> Rpc_program.id p = prog_id) 
	     mc.config.mclient_programs) 
    then
      failwith "Rpc_proxy.ManagedClient.use: \
                This program is not bound by this client"

  let reconcile_state mc =
    if_connecting_or_up mc
      (fun client ->
	 if not(Rpc_client.is_up client) then (
	   mc.estate <- `Down (next_serial mc);
	   change_pending_calls mc 0
	 )
      )

  let create_up mc cstate =
    { client = cstate.c_client;
      serial = cstate.c_serial;
      idle_timer = None;
      unavailable = cstate.c_unavailable;
      fatal_error = cstate.c_fatal_error;
    }

  let create_up1 mc client serial =
    { client = client;
      serial = serial;
      idle_timer = None;
      unavailable = false;
      fatal_error = false;
    }

  let maybe_start_idle_timer mc =
    if_up mc
      (fun up ->
	 cancel_idle_timer mc up;
	 let tmo = mc.config.mclient_idle_timeout in
	 if tmo = 0.0 then (
	   (* Stop client immediately again! *)
	   Rpc_client.trigger_shutdown up.client (fun () -> ());
	   mc.estate <- `Down(next_serial mc);
	   change_pending_calls mc 0
	 )
	 else
	   if tmo > 0.0 then (
	     (* We start a weak timer *)
	     let g = Unixqueue.new_group mc.esys in
	     Unixqueue.weak_once mc.esys g tmo
	       (fun () ->
		  Rpc_client.trigger_shutdown up.client (fun () -> ());
		  mc.estate <- `Down(next_serial mc);
		  change_pending_calls mc 0
	       );
	     up.idle_timer <- Some g
	   )
      )


  let do_initial_ping mc client serial when_up when_fail =
    (* Arrange that the initial ping is sent. When it arrives,
       when_up will be called, and when_fail on error
     *)
    let prog = List.hd mc.config.mclient_programs in
    let cstate =
      { c_client = client;
	c_serial = serial;
	c_when_up = Queue.create ();
	c_when_fail = Queue.create ();
	c_unavailable = false;
	c_fatal_error = false;
      } in
    Queue.add when_up cstate.c_when_up;
    Queue.add when_fail cstate.c_when_fail;
    ( try
	Rpc_client.unbound_async_call
	  client 
	  prog
	  mc.null_proc_name
	  Xdr.XV_void
	  (fun get_reply ->
	     try
	       let _ = get_reply() in   (* or exn *)
	       let g = Unixqueue.new_group mc.esys in
	       let up = create_up mc cstate in
	       mc.estate <- `Up up;
	       ReliabilityCache.reset_rcache_error_counter
		 mc.config.mclient_rcache
		 (sockaddr_of_conn mc.conn);
	       Queue.iter
		 (fun f_up -> 
		    Unixqueue.once mc.esys g 0.0 (fun () -> f_up up))
		 cstate.c_when_up;
	       change_pending_calls mc 0
	     with error -> 
	       Rpc_client.trigger_shutdown client (fun () -> ());
	       let g = Unixqueue.new_group mc.esys in
	       let p_unavail =
		 (* Is this error the consequence of enforce_unavailability? *)
		 error = Rpc_client.Message_lost && cstate.c_unavailable in
	       if not p_unavail then
		 fatal_error mc;
	       mc.estate <- `Down(next_serial mc);
	       let error =
		 if p_unavail then Service_unavailable else error in
	       Queue.iter
		 (fun f_fail -> 
		    Unixqueue.once mc.esys g 0.0 (fun () -> f_fail error))
		 cstate.c_when_fail;
	       change_pending_calls mc 0
	  )
      with
	| error ->
	    fatal_error mc;
	    when_fail error
    );
    mc.estate <- `Connecting cstate;
    change_pending_calls mc 0

  let bring_up mc when_up when_fail =
    (* Check availability: *)
    let sa = sockaddr_of_conn mc.conn in
    if not (ReliabilityCache.sockaddr_is_enabled 
	      mc.config.mclient_rcache
	      sa) 
    then
      when_fail Service_unavailable
    else (
      match mc.estate with
	| `Down serial ->
	    (* Create a new client and initialize it *)
	    let mode2 = 
	      `Socket(Rpc.Tcp, mc.conn, mc.config.mclient_socket_config) in
	    let client = 
	      Rpc_client.unbound_create mode2 mc.esys in
	    (* The client remains unbound. We check program compatibility here
	     *)
	    Rpc_client.configure client 0 mc.config.mclient_msg_timeout;
	    ( match mc.config.mclient_max_response_length with
		| None -> ()
		| Some m -> Rpc_client.set_max_response_length client m
	    );
	    ( match mc.config.mclient_exception_handler with
		| None -> ()
		| Some eh -> Rpc_client.set_exception_handler client eh
	    );
	    ( match mc.config.mclient_mstring_factories with
		| None -> ()
		| Some fac -> Rpc_client.set_mstring_factories client fac
	    );
	    if mc.config.mclient_auth_methods <> [] then
	      Rpc_client.set_auth_methods client mc.config.mclient_auth_methods;
	    Rpc_client.set_user_name client mc.config.mclient_user_name;
	    if mc.config.mclient_initial_ping then
	      do_initial_ping mc client serial when_up when_fail
	    else (
	      (* Easier: just claim that the client is up *)
	      let up = create_up1 mc client serial in
	      mc.estate <- `Up up;
	      change_pending_calls mc 0;
	      when_up up
	    )

	| `Connecting c ->
	    (* We only have to arrange that when_up/when_fail is called *)
	    Queue.add when_up c.c_when_up;
	    Queue.add when_fail c.c_when_fail;
	      
	| `Up up ->
	    when_up up
    )

  let unbound_async_call mc prog procname param receiver =
    use mc prog;
    reconcile_state mc;
    let batch_flag = mc.next_batch_call in
    mc.next_batch_call <- false;
    bring_up mc
      (fun up ->
	 cancel_idle_timer mc up;
	 if batch_flag then
	   Rpc_client.set_batch_call up.client;
	 try
	   Rpc_client.unbound_async_call up.client prog procname param
	     (fun get_reply ->
		change_pending_calls mc (-1);
		if mc.pending_calls = 0 then
		  maybe_start_idle_timer mc;
		receiver 
		  (fun () ->
		     try
		       let r = get_reply() in
		       ReliabilityCache.reset_rcache_error_counter
			 mc.config.mclient_rcache
			 (sockaddr_of_conn mc.conn);
		       r
		     with
			 (* Is this error the consequence of 
                            enforce_unavailability? *)
		       | Rpc_client.Message_lost when up.unavailable ->
			   raise Service_unavailable
			 (* Look for fatal errors and count them: *)
		       | (Rpc_client.Message_lost
			 | Rpc_client.Communication_error _
			 ) as error ->
			   fatal_error mc;
			   raise error
		       | Rpc_client.Message_timeout as error
			   when mc.config.mclient_msg_timeout_is_fatal -> (
			     fatal_error mc;
			     trigger_shutdown mc (fun () -> ());
			     raise error
			   )
		  )
	     )
	 with
	   | error ->
	       fatal_error mc;
	       change_pending_calls mc (-1);
	       receiver (fun () -> raise error)
      )
      (fun error ->
	 (* fatal_error has already been called if needed *)
	 change_pending_calls mc (-1);
	 receiver (fun () -> raise error)
      );
    change_pending_calls mc 1


  let unbound_sync_call mc prog procname param =
    Rpc_client.synchronize
      mc.esys
      (unbound_async_call mc prog procname)
      param


  let rpc_engine mclient f arg =
    (** The client RPC call of [f] with [arg] is wrapped into an engine *)
    let esys = event_system mclient in
    ( object(self)
	inherit [_] Uq_engines.engine_mixin (`Working 0) esys
	  
	val mutable finished = false
	val mutable aborted = false
	  
	initializer (
	  (** We delay the invocation of [f] minimally. Otherwise, the engine
              could reach a final state before any listeners would have the
              chance to register their callbacks.
	   *)
	  let g = Unixqueue.new_group esys in
	  Unixqueue.once esys g 0.0
	    (fun () ->
	       f
		 mclient
		 arg
		 (fun get_reply ->
		    finished <- true;
		    if aborted then
		      self # set_state `Aborted
		    else
		      try
			let r = get_reply() in
			self # set_state (`Done r)
		      with
			| error ->
			    self # set_state (`Error error)
		 )
	    )
	)
	  
	method event_system =
	  esys
	    
	method abort() =
	  (** There is no way to abort an RPC call. We only can suppress that
              it reaches the caller.
	   *)
	  if not finished then
	    aborted <- true
	      
      end
  )

end


module MclientSet = Set.Make(ManagedClient)


module ManagedSet = struct
  type mset_policy =
      [ `Failover | `Balance_load ]
	
  type mset_config =
      { mset_mclient_config : ManagedClient.mclient_config;
	mset_policy : mset_policy;
	mset_pending_calls_max : int;
	mset_pending_calls_norm : int;
	mset_idempotent_max : int;
        mset_idempotent_wait : float;
      }

  type load_level =
      { mutable clients : MclientSet.t
      }

  type mset =
      { config : mset_config;
	services : (Rpc_client.connector * int) array;
	by_load : (int, load_level) Hashtbl.t array;
	(* For every service there is a hashtable mapping load to the
           clients with the load. Load is measured as pending_calls value
	 *)
	total_load : int array;  	(* Total sum of load *)
	total_clients : int array;      (* Total number of non-down clients *)
	esys : Unixqueue.event_system;
	mutable timer_group : Unixqueue.group;  (* for idempotent repetition *)
	mutable timer_active : < cancel : unit -> unit > list;
      }

  exception Cluster_service_unavailable

  let default_mclient_config =
    ManagedClient.create_mclient_config()

  let create_mset_config 
        ?(mclient_config = default_mclient_config)
        ?(policy = `Balance_load)
        ?(pending_calls_max = max_int)
        ?(pending_calls_norm = 1)
        ?(idempotent_max = 3)
        ?(idempotent_wait = 5.0)
        () =
    { mset_mclient_config = mclient_config;
      mset_policy = policy;
      mset_pending_calls_max = pending_calls_max;
      mset_pending_calls_norm = pending_calls_norm;
      mset_idempotent_max = idempotent_max;
      mset_idempotent_wait = idempotent_wait
    }

  let create_mset config services esys =
    (* Check services: *)
    Array.iter
      (fun (conn,_) -> ignore(ManagedClient.sockaddr_of_conn conn))
      services;
    let by_load =
      Array.map (fun _ -> Hashtbl.create 10 ) services in
    let total_load =
      Array.map (fun _ -> 0) services in
    let total_clients =
      Array.map (fun _ -> 0) services in
     { config = config;
       services = services;
       by_load = by_load;
       total_load = total_load;
       total_clients = total_clients;
       esys = esys;
       timer_group = Unixqueue.new_group esys;
       timer_active = []
     }

  let cancel_timer mset =
    Unixqueue.clear mset.esys mset.timer_group;
    List.iter
      (fun act -> act#cancel())
      mset.timer_active;
    mset.timer_group <- Unixqueue.new_group mset.esys;
    mset.timer_active <- []

  let pending_calls_callback mset k mc old_pc new_pc =
    (* Called when pending_calls or mclient_state changes *)
    let remove_from_level() =
      (* Remove client from old load level: *)
      ( try
	  let old_level = Hashtbl.find mset.by_load.(k) old_pc in
	  if not(MclientSet.mem mc old_level.clients) then 
	    raise Not_found;
	  old_level.clients <- MclientSet.remove mc old_level.clients;
	  if old_level.clients = MclientSet.empty then
	    Hashtbl.remove mset.by_load.(k) old_pc;
	  mset.total_clients.(k) <- mset.total_clients.(k) - 1;
	  true
	with
	  | Not_found -> false
      ) in
    let enter_into_level() =
      (* Enter client into new load level: *)
      let new_level =
	try
	  Hashtbl.find mset.by_load.(k) new_pc
	with
	  | Not_found ->
	      let level = { clients = MclientSet.empty } in
	      Hashtbl.add mset.by_load.(k) new_pc level;
	      level in
      if not (MclientSet.mem mc new_level.clients) then (
	new_level.clients <- MclientSet.add mc new_level.clients;
	mset.total_clients.(k) <- mset.total_clients.(k) + 1
      ) in
    let adjust_total_load opc npc =
      mset.total_load.(k) <- mset.total_load.(k) - opc + npc in
    match ManagedClient.mclient_state mc with
      | `Down ->
	  (* The client is completely removed, even when there are still
             calls pending (which will be called back asap).
	   *)
	  let rflag = remove_from_level() in
	  if rflag then
	    adjust_total_load old_pc 0
      | `Connecting
      | `Up _ ->
	  let rflag = remove_from_level() in
	  enter_into_level();
	  if rflag then
	    adjust_total_load old_pc 0;
	  adjust_total_load 0 new_pc


  let create_mclient mset k =
    let conn = fst(mset.services.(k)) in
    let mc = 
      ManagedClient.create_mclient
	mset.config.mset_mclient_config
	conn
	mset.esys in
    mc.ManagedClient.pending_calls_callback <- 
      (pending_calls_callback mset k mc);
    mc


  let pick_from_service mset k =
    let rec pick_from_level_down pc =
      try
	let level = Hashtbl.find mset.by_load.(k) pc in
	MclientSet.min_elt level.clients
      with
	| Not_found ->
	    if pc > 0 then
	      pick_from_level_down (pc-1)
	    else (
	      (* If possible create new client *)
	      if mset.total_clients.(k) >= snd(mset.services.(k)) then
		raise Not_found;
	      (* Create a new client. No need to enter it into the by_load
                 structure because it is still `Down.
	       *)
	      create_mclient mset k
	    )
    in
    (* Test if this service is available: *)
    let sa = ManagedClient.sockaddr_of_conn (fst mset.services.(k)) in
    if not (ReliabilityCache.sockaddr_is_enabled 
	      mset.config.mset_mclient_config.ManagedClient.mclient_rcache
	      sa) 
    then
      raise Not_found;
    (* Try the levels from pending_calls_norm-1 on downward: *)
    try
      pick_from_level_down (mset.config.mset_pending_calls_norm-1)
    with
      | Not_found ->
	  (* There might be still clients that have not reached
             pending_calls_max
	   *)
	  let norm_pc = mset.config.mset_pending_calls_norm in
	  let max_pc = mset.config.mset_pending_calls_max in
	  let overload_levels =
	    Hashtbl.fold
	      (fun pc _ acc -> 
		 if pc >= norm_pc && pc < max_pc then pc :: acc else acc)
	      mset.by_load.(k)
	      [] in
	  let pc =
	    List.find   (* or Not_found *)
	      (fun pc -> 
		 let level = Hashtbl.find mset.by_load.(k) pc in
		 level.clients <> MclientSet.empty
	      )
	      overload_levels in
	  let level = Hashtbl.find mset.by_load.(k) pc in
	  MclientSet.min_elt level.clients


  let pick_from mset order =
    let rec next j =
      try 
	let k = order.(j) in
	let mc = pick_from_service mset k in
	(mc, k)
      with
	| Not_found -> 
	    let j' = j+1 in
	    if j' < Array.length order then
	      next j'
	    else
	      raise Cluster_service_unavailable in
    next 0


  let mset_pick ?from mset =
    let order =
      match from with
	| None ->
	    Array.init (Array.length mset.services) (fun k -> k)
	| Some l ->
	    List.iter
	      (fun k ->
		 if k < 0 || k >= Array.length mset.services then
		   invalid_arg "Rpc_proxy.ManagedSet.mset_pick"
	      )
	      l;
	    Array.of_list l
    in
    match mset.config.mset_policy with
      | `Failover ->
	  (* Try the services in given order: *)
	  pick_from mset order
      | `Balance_load ->
	  (* Sort the services by total load first: *)
	  Array.sort 
	    (fun j1 j2 ->
	       Pervasives.compare 
		 mset.total_load.(j1)
		 mset.total_load.(j2)
	    )
	    order;
	  pick_from mset order


  let mset_services mset =
    mset.services

  let mset_load mset =
    mset.total_load

  let event_system mset =
    mset.esys

  let trigger_shutdown mset ondown =
    cancel_timer mset;   (* stop timer for idempotent repetition *)
    let all_clients_of_service k =
      Hashtbl.fold
	(fun pc level acc ->
	   MclientSet.union level.clients acc
	)
	mset.by_load.(k)
	MclientSet.empty in
    let all_clients =
      List.fold_left
	(fun acc k ->
	   MclientSet.union (all_clients_of_service k) acc
	)
	MclientSet.empty
	(Array.to_list 
	   (Array.init (Array.length mset.services) (fun k -> k))) in
    let n = ref (MclientSet.cardinal all_clients) in
    MclientSet.iter
      (fun mc ->
	 ManagedClient.trigger_shutdown mc
	   (fun () ->
	      decr n;
	      if !n = 0 then ondown()
	   )
      )
      all_clients;
    if !n = 0 then ondown()

  let sync_shutdown mset =
    if Unixqueue.is_running mset.esys then
      failwith "Rpc_proxy.ManagedSet.sync_shutdown: called from event loop";
    trigger_shutdown mset (fun () -> ());
    Unixqueue.run mset.esys

  let shut_down mset =
    if Unixqueue.is_running mset.esys then
      trigger_shutdown mset (fun () -> ())
    else
      sync_shutdown mset 

  let idempotent_async_call ?from mset async_call arg emit =
    let config_delay = mset.config.mset_idempotent_wait in
    let n = ref 0 in
    let rec next_attempt last_err delay =
      incr n;
      if !n > mset.config.mset_idempotent_max then
	emit (fun () -> raise last_err)
      else
	let cancel_obj =
	  ( object
	      method cancel() =
		emit (fun () -> raise last_err)
	    end
	  ) in
	Unixqueue.once mset.esys mset.timer_group delay
	  (fun () ->
	     mset.timer_active <-
	       List.filter (fun obj -> obj <> cancel_obj) mset.timer_active;
	     try
	       let mc, _ = mset_pick ?from mset in
	       async_call mc arg
		 (fun get_reply ->
		    let repeat_flag =
		      try ignore(get_reply()); None
		      with
			| (Rpc_client.Message_lost
			  | Rpc_client.Message_timeout
			  | Rpc_client.Communication_error _
			  | ManagedClient.Service_unavailable
			  ) as error ->
			    Some error
			| _ -> 
			    None in
		    match repeat_flag with
		      | None ->
			  emit get_reply
		      | Some error ->
			  next_attempt error config_delay
		 )
	     with
	       | (ManagedClient.Service_unavailable
		 | Cluster_service_unavailable
		 ) as error ->
		   next_attempt error config_delay
	       | error ->
		   emit (fun () -> raise error)
	  );
	mset.timer_active <- cancel_obj :: mset.timer_active
    in
    next_attempt 
      Rpc_client.Message_lost   (* most appropriate *)
      0.0

  let idempotent_sync_call ?from mset async_call arg =
    Rpc_client.synchronize
      mset.esys
      (idempotent_async_call ?from mset async_call)
      arg

end
