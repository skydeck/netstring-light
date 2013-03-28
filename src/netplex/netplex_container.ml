(* $Id: netplex_container.ml 1718 2012-02-21 14:59:45Z gerd $ *)

open Netplex_types
open Netplex_ctrl_aux
open Printf

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netplex_container" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netplex_container" Debug.enable

let () =
  Netlog.Debug.register_module "Netplex_container" Debug.enable

let string_of_event = 
  function
    | `event_none -> "NONE"
    | `event_accept n ->  "ACCEPT(" ^ string_of_int n ^ ")"
    | `event_noaccept ->  "NOACCEPT"
    | `event_received_message msg ->
	"RECEIVED_MESSAGE(" ^ msg.msg_name ^ ")"
    | `event_received_admin_message msg -> 
	"RECEIVED_ADMIN_MESSAGE(" ^ msg.msg_name ^ ")"
    | `event_shutdown -> "SHUTDOWN"
    | `event_system_shutdown -> "SYSTEM_SHUTDOWN"

let string_of_sys_id =
  function
    | `Thread id -> "thread " ^ string_of_int id
    | `Process id -> "process " ^ string_of_int id


let t_poll = 0.1


class std_container ?esys
                    ptype sockserv =
  let ssn = sockserv # name in
  let esys =
    match esys with
      | Some esys -> esys
      | None -> sockserv # processor # container_event_system() in
  let sys_esys = Unixqueue.create_unix_event_system() in
  let sys_mon = Uq_mt.create_monitor sys_esys in
  let cont_thr_id = !Netsys_oothr.provider # self # id in
object(self)
  val sys_esys = sys_esys (* for subclasses *)
  val mutable rpc = None
  val mutable sys_rpc = None
  val mutable nr_conns = 0
  val mutable nr_conns_total = 0
  val mutable engines = []
  val mutable vars = Hashtbl.create 10 
  val         vars_mutex = !Netsys_oothr.provider # create_mutex()
  val mutable polling = None
  val mutable polling_timer = None
  val mutable greedy = false

  method container_id = (self :> container_id)

  method socket_service_name = ssn
  method socket_service = sockserv

  method event_system = esys

  method ptype = ptype

  method start fd_clnt sys_fd_clnt =
    (* note that logging is first possible when sys_rpc is initialized! *)
    if rpc <> None then
      failwith "#start: already started";
    let sys_id = Netplex_cenv.current_sys_id() in
    ( match ptype with
	| `Multi_processing ->
	    ( match sockserv # socket_service_config # change_user_to with
		| None -> ()
		| Some(uid,gid) ->
		    (* In Netplex_config it has already been checked whether the
                     * effective uid of the process is root. So the following 
                     * drops all privileges:
		     *)
		    Unix.setgid gid;
		    Unix.setuid uid
	    )
	| _ -> ()
    );
    (* Note: fd_clnt and sys_fd_clnt are closed by the caller *)
    let rpc_cl =
      Netplex_ctrl_clnt.Control.V1.create_client
      ~esys
      (Rpc_client.Descriptor fd_clnt)
      Rpc.Tcp in
    if not !Debug.enable then
      Rpc_client.Debug.disable_for_client rpc_cl;
    rpc <- Some rpc_cl;
    let sys_rpc_cl =
      Netplex_ctrl_clnt.System.V1.create_client
	~esys:sys_esys
	(Rpc_client.Descriptor sys_fd_clnt)
	Rpc.Tcp in
    if not !Debug.enable then
      Rpc_client.Debug.disable_for_client sys_rpc_cl;
    sys_rpc <- Some sys_rpc_cl;
    self # setup_container_sockets();
    dlogr 
      (fun () -> 
	 sprintf "Container %d: Starting (start) in %s" 
	   (Oo.id self) (string_of_sys_id sys_id));
    self # protect "post_start_hook"
      (sockserv # processor # post_start_hook)
      (self : #container :> container);

    self # restart_polling();
    self # protect_run();
    (* protect_run returns when all events are handled. This must include
       [rpc], so the client must now be down.
     *)
    assert(rpc = None);

    self # protect "pre_finish_hook"
      (sockserv # processor # pre_finish_hook)
      (self : #container :> container);
    self # close_container_sockets();
    rpc <- None;

    dlogr
      (fun () -> sprintf "Container %d: Finishing (finish)" (Oo.id self))


  method private protect : 's. string -> ('s -> unit) -> 's -> unit =
    fun label f arg ->
      ( try
	  dlogr
	    (fun () -> 
	       sprintf "Container %d: calling hook %s" (Oo.id self) label);
	  f arg 
	with
	  | error ->
	      ( match rpc with
		  | None -> ()  (* no way to report this error *)
		  | Some r ->
		      self # log `Crit (label ^ ": Exception " ^ 
					  Netexn.to_string error)
	      )
      );
       dlogr
	 (fun () -> 
	    sprintf "Container %d: back from hook %s" (Oo.id self) label)


  method private protect_run () =
    try 
      sockserv # processor # container_run esys
    with
      | error ->
	  self # log `Crit ("run: Exception " ^ Netexn.to_string error);
	  let b = sockserv # processor # global_exception_handler error in
	  if b then self # protect_run()

  (* The following logic means:
     - For synchronous processors, setup_polling is always called after
       the connection is processed, i.e. restart_polling will find that
       polling=None in this case. Because there is no concurrency, the
       poll loop cannot have been started before.
     - For async processors, setup_polling is re-established after each
       received command (e.g. an ACCEPT), and after at most t_poll seconds.
       It is NOT immediately called when the processor is done. This
       means that the controller sees decreased nr_conns and fully_busy params
       after a delay of at most t_poll seconds.
   *)

  method private restart_polling() =
    match polling with
      | None -> 
	  self # setup_polling()
      | Some _ ->
	  (* Greedy: We suppress this setup_polling, but ensure that there is a
	     timer
	   *)
	  if not greedy || polling_timer = None then
	    self # set_polling_timer();
	  

  method private set_polling_timer() =
    let g = Unixqueue.new_group esys in
    Unixqueue.once esys g t_poll 
      (fun () ->
	 self # setup_polling();
      );
    polling_timer <- Some g;
    

  method private reset_polling_timer() =
    match polling_timer with
      | None -> ()
      | Some g ->
	  Unixqueue.clear esys g;
	  polling_timer <- None


  method private setup_polling() =
    self # reset_polling_timer();
    match rpc with
      | None -> ()  (* Already shut down ! *)
      | Some r ->
	  let fully_busy = self#conn_limit_reached in
	  dlogr
	    (fun () -> 
	       sprintf "Container %d: Polling nr_conns=%d fully_busy=%B"
		 (Oo.id self) nr_conns fully_busy);
	  polling <- Some(nr_conns,fully_busy,engines=[]);
	  Netplex_ctrl_clnt.Control.V1.poll'async r (nr_conns,fully_busy)
	    (fun getreply ->
	       polling <- None;
	       let continue =
		 ( try
		     let reply = getreply() in
		     dlogr
		       (fun () -> 
			  sprintf "Container %d: Got event from polling: %s"
			    (Oo.id self) (string_of_event reply));
		     ( match reply with
			 | `event_none ->
			     (* When [setup_calling] is called twice, the
                                second poll loop throws out the first poll
                                loop. The first loop gets then [`event_none],
                                and has to terminate. This is used after
                                a connection is done to gain attention from
                                the server.
			      *)
			     false
			 | `event_accept n_accept -> 
			     self # enable_accepting n_accept;
			     true
			 | `event_noaccept -> 
			     self # disable_accepting();
			     true
			 | `event_received_message msg ->
			     self # protect
			       "receive_message"
			       (sockserv # processor # receive_message
				  (self : #container :> container)
				  msg.msg_name)
			       msg.msg_arguments;
			     true
			 | `event_received_admin_message msg ->
			     self # receive_admin_message msg;
			     true
			 | `event_shutdown ->
			     self # disable_accepting();
			     self # disable_container_sockets();
			     self # protect
			       "shutdown"
			       (sockserv # processor # shutdown)
			       ();
			     ( try
				 Netplex_cenv.cancel_all_timers()
			       with
				   (* can happen in the admin case: *)
				 | Netplex_cenv.Not_in_container_thread -> ()
			     );
			     ( match rpc with
				 | None -> ()
				 | Some r -> 
				     rpc <- None;
				     Rpc_client.trigger_shutdown 
				       r
				       self # shutdown_extra;
				     (* We ensure that shutdown_extra is called
                                        when the client is already taken down
				      *)
			     );
			     false
			 | `event_system_shutdown ->
			     self # protect
			       "system_shutdown"
			       (sockserv # processor # system_shutdown)
			       ();
			     true
		     )
		   with
		     | Rpc_client.Message_lost ->
			 (* This can happen when the container is shut down
                          * and several [poll] calls are active at the same
                          * time.
                          *)
			 false
		     | error ->
			 self # log `Crit ("poll: Exception " ^ 
					    Netexn.to_string error);
			 true
		 ) in
	       if continue then
		 self # setup_polling()
	    )
    
  method private enable_accepting n_accept =
    if engines = [] then (
      List.iter
	(fun (proto, fd_array) ->
	   Array.iter
	     (fun fd ->
		dlogr
		  (fun () ->
		     sprintf "Container %d: Accepting on fd %Ld"
		       (Oo.id self)
		       (Netsys.int64_of_file_descr fd));
		let acc = new Uq_engines.direct_acceptor fd esys in
		let e = acc # accept() in
		Uq_engines.when_state
		  ~is_done:(fun (fd_slave,_) ->
			      (* The first engine accepted a connection. It is
                               * possible that other descriptors are ready
                               * for being accepted at this time. By disabling
                               * accepting, we ensure that these engines
                               * are aborted and the events are ignored.
                               * It is essential that the direct_endpoint_acceptor
                               * accepts in one step so intermediate states
                               * are impossible.
                               *)
			      dlogr
				(fun () ->
				   sprintf "Container %d: Accepted as fd %Ld"
				     (Oo.id self)
				     (Netsys.int64_of_file_descr fd_slave));
			      self # disable_accepting();
			      self # greedy_accepting
				(n_accept - 1) [fd_slave, proto]
			   )
		  ~is_error:(fun err ->
			       self # log `Crit
				 ("accept: Exception " ^ 
				    Netexn.to_string err)
			    )
		  e;
		engines <- e :: engines
	     )
	     fd_array
	)
	sockserv#sockets
    )

  method private disable_accepting() =
    dlogr
      (fun () ->
	 sprintf "Container %d: No longer accepting" (Oo.id self));
    List.iter (fun e -> e # abort()) engines;
    engines <- [];

  method private greedy_accepting n_accept accept_list =
    let n_accept = ref n_accept in
    let accept_list = ref accept_list in
    let sockets = sockserv#sockets in
    let x_sockets =
      List.map
	(fun (proto, fd_array) ->
	   (proto, fd_array, Array.map (fun _ -> true) fd_array)
	)
	sockets in
    ( try
	let cont = ref true in
	if !n_accept > 0 then greedy <- true;
	while !cont && !n_accept > 0 do
	  cont := false;
	  List.iter
	    (fun (proto, fd_array, fd_cont) ->
	       Array.iteri
		 (fun k fd ->
		    if fd_cont.(k) then (
		      dlogr
			(fun () ->
			   sprintf "Container %d: Greedy accepting on fd %Ld"
			     (Oo.id self)
			     (Netsys.int64_of_file_descr fd));
		      try
			let fd_slave, _ = Unix.accept fd in
			cont := true;  (* try for another round *)
			accept_list := (fd_slave, proto) :: !accept_list;
			Unix.set_nonblock fd_slave;
			dlogr
			  (fun () ->
			     sprintf "Container %d: Accepted as fd %Ld"
			       (Oo.id self)
			       (Netsys.int64_of_file_descr fd_slave));
			decr n_accept;
			if !n_accept = 0 then raise Exit;
		      with
			| Unix.Unix_error((Unix.EAGAIN|Unix.EWOULDBLOCK|
					       Unix.EINTR),
					  _,_) ->
			    fd_cont.(k) <- false
			| Unix.Unix_error _ as error ->
			    self # log `Crit
			      ("accept: Exception " ^ Netexn.to_string error);
			    raise Exit
		    )
		 )
		 fd_array
	    )
	    x_sockets
	done
      with
	| Exit ->
	    ()
    );
    match !accept_list with
      | [] -> ()
      | (fd_slave_last,proto_last) :: l ->
	  List.iter
	    (fun (fd_slave, proto) ->
	       self # accepted_greedy fd_slave proto
	    )
	    (List.rev l);
	  (* The last connection in this round is always processed in
	     non-greedy style, so the controller gets a notification.
	   *)
	  self # accepted_nongreedy fd_slave_last proto_last

  method private accepted_nongreedy fd_slave proto =
    (* We first respond with the "accepted" message to the controller.
       This is especially important for synchronous processors, because
       this is the last chance to notify the controller about the state
       change in sync contexts.
     *)
    self # prep_socket fd_slave proto;
    match rpc with
      | None -> assert false
      | Some r ->
	  (* Resetting polling: From this point on the container counts as
	     busy, and we want that this state is left ASAP
	   *)
	  polling <- None;
	  Rpc_client.set_batch_call r;
	  Rpc_client.unbound_async_call
	    r Netplex_ctrl_aux.program_Control'V1 "accepted" Xdr.XV_void
	    (fun _ ->
	       self # process_conn fd_slave proto
	    )

  method private accepted_greedy fd_slave proto =
    self # prep_socket fd_slave proto;
    self # process_conn fd_slave proto

  method private process_conn fd_slave proto =
    nr_conns <- nr_conns + 1;
    nr_conns_total <- nr_conns_total + 1;
    let regid = self # reg_conn fd_slave in
    let when_done_called = ref false in
    dlogr
      (fun () -> 
	 sprintf
	   "Container %d: processing connection on fd %Ld (total conns: %d)" 
	   (Oo.id self) 
	   (Netsys.int64_of_file_descr fd_slave)
	   nr_conns
      );
    self # workload_hook true;
    self # protect
      "process"
      (sockserv # processor # process
	 ~when_done:(fun fd ->
		       (* Note: It is up to the user to close
                          the descriptor. So the descriptor can
                          already be used for different purposes
                          right now!
			*)
		       if not !when_done_called then (
			 nr_conns <- nr_conns - 1;
			 self # unreg_conn fd_slave regid;
			 when_done_called := true;
			 self # workload_hook false;
			 self # restart_polling();
			 dlogr
			   (fun () ->
			      sprintf "Container %d: \
                                               Done with connection on fd %Ld \
                                                  (total conns %d)"
				(Oo.id self) 
				(Netsys.int64_of_file_descr fd_slave)
				nr_conns);
		       )
		    )
	 (self : #container :> container)
	 fd_slave
      )
      proto;
    if not !when_done_called then
      self # restart_polling();

  method private prep_socket fd_slave proto =
    try
      let proto_obj =
	List.find
	  (fun proto_obj ->
	     proto_obj#name = proto
	  )
	  sockserv#socket_service_config#protocols in
      if proto_obj#tcp_nodelay then
	Unix.setsockopt fd_slave Unix.TCP_NODELAY true
    with
      | Not_found -> ()


  val mutable reg_conns = Hashtbl.create 10
  val mutable reg_conns_cnt = 0

  method private reg_conn fd =
    let ifd = Netsys.int64_of_file_descr fd in
    let line = Netplex_cenv.report_connection_string fd "" in
    let cnt = reg_conns_cnt in
    reg_conns_cnt <- cnt+1;
    Hashtbl.replace reg_conns ifd (cnt,line);
    cnt

  method private unreg_conn fd cnt =
    let ifd = Netsys.int64_of_file_descr fd in
    try
      let cnt',_ = Hashtbl.find reg_conns ifd in
      if cnt' = cnt then (
	Hashtbl.remove reg_conns ifd
      )
    with
      | Not_found -> ()

  method n_connections = nr_conns

  method n_total = nr_conns_total

  method private conn_limit_reached =
    match sockserv#socket_service_config#conn_limit with
      | None -> false
      | Some lim -> nr_conns_total >= lim

  val mutable gc_timer = None

  method private workload_hook busier_flag =
    if busier_flag then (
      match gc_timer with
	| None ->  ()
	| Some g -> Unixqueue.clear esys g
    );
    if self#conn_limit_reached && nr_conns = 0 then
      self#shutdown()
    else (
      self # protect "workload_hook"
	(sockserv # processor # workload_hook
	   (self : #container :> container)
	   busier_flag
	)
	nr_conns;
      if nr_conns = 0 && sockserv#socket_service_config#gc_when_idle then (
	match gc_timer with
	  | None -> 
	      let g = Unixqueue.new_group esys in
	      Unixqueue.once esys g 1.0
		(fun () -> Gc.full_major());
	      gc_timer <- Some g
	  | Some _ -> ()
      )
    )

  method update_detail fd detail =
    let ifd = Netsys.int64_of_file_descr fd in
    let line = Netplex_cenv.report_connection_string fd detail in
    try
      let (cnt,_) = Hashtbl.find reg_conns ifd in
      Hashtbl.replace reg_conns ifd (cnt,line)
    with
      | Not_found ->
	  failwith "#update_detail: unknown descriptor"

  method system =
    match sys_rpc with
      | None -> failwith "#system: No RPC client available"
      | Some r -> r

  method system_monitor = sys_mon

  method shutdown() =
    dlogr
      (fun () -> 
	 sprintf
	   "Container %d: shutdown" (Oo.id self));
    (* This method can be called from a different thread. In this case,
       we have to ensure that the shutdown code is run in the main thread
       of the container, and then have to wait until the shutdown is
       complete
     *)
    let mt_case =
      (cont_thr_id <> !Netsys_oothr.provider # self # id) in
    if mt_case then (
      let mutex = !Netsys_oothr.provider # create_mutex() in
      let cond = !Netsys_oothr.provider # create_condition() in
      let g = Unixqueue.new_group esys in
      Unixqueue.once esys g 0.0
	(fun () ->
	   self # shutdown_action();
	   cond # signal()
	);
      cond # wait mutex
    )
    else
      self # shutdown_action()
    

  method private shutdown_action() =
    ( try
	Netplex_cenv.cancel_all_timers()
      with
	  (* can happen in the admin case: *)
	| Netplex_cenv.Not_in_container_thread -> ()
    );
    self # reset_polling_timer();
    self # disable_accepting();
    self # disable_container_sockets();
    ( match rpc with
	| None -> ()
	| Some r -> 
	    rpc <- None;
	    Rpc_client.trigger_shutdown 
	      r
	      self # shutdown_extra;
	    (* We ensure that shutdown_extra is called
	       when the client is already taken down
	     *)
    )

  method private shutdown_extra() = 
    (* to be subclassed *)
    ()

  method log_subch subchannel level message =
    match sys_rpc with
      | None -> ()
      | Some r ->
	  ( try
	      let lev = 
		match level with
		  | `Emerg -> log_emerg
		  | `Alert -> log_alert
		  | `Crit -> log_crit
		  | `Err -> log_err
		  | `Warning -> log_warning
		  | `Notice -> log_notice
		  | `Info -> log_info
		  | `Debug -> log_debug in
	      Uq_mt.monitor_async 
		sys_mon
		(fun arg emit ->
		   Rpc_client.set_batch_call r;
		   Netplex_ctrl_clnt.System.V1.log'async r arg emit
		)
		(lev,subchannel,message);
	    with
	      | error ->
		  prerr_endline("Netplex Catastrophic Error: Unable to send log message - exception " ^ Netexn.to_string error);
		  prerr_endline("Log message is: " ^ message)
	  )

  method log level message =
    self # log_subch "" level message

  method lookup service protocol =
    match sys_rpc with
      | None -> failwith "#lookup: No RPC client available"
      | Some r ->
	  Uq_mt.monitor_async 
	    sys_mon
	    (Netplex_ctrl_clnt.System.V1.lookup'async r)
	    (service,protocol)

  method send_message pat msg_name msg_arguments =
    dlogr
      (fun () -> 
	 sprintf
	   "Container %d: send_message %s to %s" (Oo.id self) msg_name pat);
    match sys_rpc with
      | None -> failwith "#send_message: No RPC client available"
      | Some r ->
	  let msg =
	    { msg_name = msg_name;
	      msg_arguments = msg_arguments
	    } in
	  Uq_mt.monitor_async
	    sys_mon
	    (Netplex_ctrl_clnt.System.V1.send_message'async r)
	    (pat, msg)

  method var name =
    Netsys_oothr.serialize
      vars_mutex
      (Hashtbl.find vars) name

  method set_var name value =
    Netsys_oothr.serialize
      vars_mutex
      (Hashtbl.replace vars name) value

  method call_plugin p proc_name arg =
    dlogr
      (fun () ->
	 sprintf
	   "Container %d: call_plugin id=%d procname=%s" 
	   (Oo.id self) (Oo.id p) proc_name);
    match sys_rpc with
      | None -> failwith "#call_plugin: No RPC client available"
      | Some r ->
	  let (_, arg_ty,res_ty) = 
	    try
	      Rpc_program.signature p#program proc_name
	    with
	      | Not_found -> failwith "call_plugin: procedure not found" in
	  let arg_str = Xdr.pack_xdr_value_as_string arg arg_ty [] in
	  let res_str =
	    Uq_mt.monitor_async
	      sys_mon
	      (Netplex_ctrl_clnt.System.V1.call_plugin'async r)
	      ((Int64.of_int (Oo.id p)), proc_name, arg_str) in
	  let res = Xdr.unpack_xdr_value ~fast:true res_str res_ty [] in
	  res
	    
  method private receive_admin_message msg =
    match msg.msg_name with
      | "netplex.debug.enable" ->
	  ( try
	      let m = 
		match msg.msg_arguments with
		  | [| m |] -> m
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      Netlog.Debug.enable_module m
	    with
	      | Failure s ->
		  self # log 
		    `Err
		    ("netplex.debug.enable: " ^ s)
	  )
      | "netplex.debug.disable" ->
	  ( try
	      let m = 
		match msg.msg_arguments with
		  | [| m |] -> m
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      Netlog.Debug.disable_module m
	    with
	      | Failure s ->
		  self # log 
		    `Err
		    ("netplex.debug.disable: " ^ s)
	  )
      | "netplex.fd_table" ->
	  ( List.iter
	      (fun line ->
		 self # log
		   `Debug
		   (sprintf "fd_table(%5d): %s"
		      ( match Netplex_cenv.current_sys_id() with
			  | `Thread n -> n
			  | `Process n -> n
		      )
		      line)
	      )
	      (Netlog.Debug.fd_table())
	  )
      | "netplex.connections" ->
	  (* First forward the message, so user code can call [update_detail] *)
	  self # forward_admin_message msg;
	  Hashtbl.iter
	    (fun _ (_,line) ->
	       (* There is a chance that this connection is already closed,
                  and even that fd is used for something else. So be very
                  careful here.
		*)
	       self # log `Debug line
	    )
	    reg_conns

      | "netplex.mem.major" ->
	  (* FIXME: This style does not make sense for multi-threaded 
	     programs *)
	  let t0 = Unix.gettimeofday() in
	  Gc.major();
	  let t1 = Unix.gettimeofday() in
	  self # log `Info
	    (sprintf "Gc.major: pid %d - %f seconds" 
	       (Unix.getpid()) (t1 -. t0))

      | "netplex.mem.compact" ->
	  (* FIXME: This style does not make sense for multi-threaded 
	     programs *)
	  let t0 = Unix.gettimeofday() in
	  Gc.compact();
	  let t1 = Unix.gettimeofday() in
	  self # log `Info
	    (sprintf "Gc.compact: pid %d - %f seconds" 
	       (Unix.getpid()) (t1 -. t0))

      | "netplex.mem.pools" ->
	  (* FIXME: This style does not make sense for multi-threaded 
	     programs *)
	  self # log `Info
	    (sprintf "Default pool: pid %d - %s"
	       (Unix.getpid())
	       (Netsys_mem.pool_report Netsys_mem.default_pool));
	  self # log `Info
	    (sprintf "Small pool: pid %d - %s"
	       (Unix.getpid())
	       (Netsys_mem.pool_report Netsys_mem.small_pool))

      | "netplex.mem.stats" ->
	  let (name, inch, outch) = Netchannels.make_temporary_file() in
	  Gc.print_stat outch;
	  close_out outch;
	  let n = in_channel_length inch in
	  let s = String.create n in
	  really_input inch s 0 n;
	  close_in inch;
	  Sys.remove name;
	  self # log `Info
	    (sprintf "GC stats pid %d:\n%s"
	       (Unix.getpid()) s)

      | _ ->
	  self # forward_admin_message msg

  method private forward_admin_message msg =
    self # protect
      "receive_admin_message"
      (sockserv # processor # receive_admin_message
	 (self : #container :> container)
	 msg.msg_name)
      msg.msg_arguments


  method activate_lever id arg_enc =
    match sys_rpc with
      | None -> failwith "#activate_lever: No RPC client available"
      | Some r ->
	  let arg_str = Marshal.to_string arg_enc [] in
	  let res_str =
	    Uq_mt.monitor_async
	      sys_mon
	      (Netplex_ctrl_clnt.System.V1.activate_lever'async r)
	      (id, arg_str) in
	  let res = Marshal.from_string res_str 0 in
	  res

  (* --- container sockets --- *)

  (* Note that the container sockets do not interfer with event polling.
     It is a completely separate execution thread.
   *)

  val mutable cs_engines = []
  val mutable cs_sockets = []

  method private setup_container_sockets() =
    let protos = sockserv # socket_service_config # protocols in
    List.iter
      (fun proto ->
	 Array.iter
	   (function
	      | `Container(dir,sname,pname,_) ->
		  (* sname, pname: we use this only for registration
                     purposes. Otherwise sockserv#name and proto#name are
                     more reliable
		   *)
		  let (dir', path) =
		    Netplex_util.path_of_container_socket
		      dir sname pname (Netplex_cenv.current_sys_id()) in
		  let () = Netplex_util.try_mkdir dir in
		  let () = Netplex_util.try_mkdir dir' in
		  let pseudo_addr =
		    match Sys.os_type with
		      | "Win32" ->
			  `W32_pipe_file path
		      | _ ->
			  `Socket(Unix.ADDR_UNIX path) in
		  let fd =
		    Netplex_util.create_server_socket 
		      ssn proto pseudo_addr in
		  self # register_container_socket sname pname path;
		  self # accept_on_container_socket proto fd;
		  cs_sockets <- (fd, pname, path) :: cs_sockets
	      | _ -> ()
	   )
	   proto#addresses
      )
      protos

  method private accept_on_container_socket proto fd =
    dlogr
      (fun () ->
	 sprintf "Container %d: Accepting on fd %Ld"
	   (Oo.id self)
	   (Netsys.int64_of_file_descr fd));
    let acc = new Uq_engines.direct_acceptor fd esys in
    let e = acc # accept() in
    Uq_engines.when_state
      ~is_done:(fun (fd_slave,_) ->
		  dlogr
		    (fun () ->
		       sprintf "Container %d: Accepted as fd %Ld"
			 (Oo.id self)
			 (Netsys.int64_of_file_descr fd_slave));
		  let when_done_called = ref false in
		  dlogr
		    (fun () -> 
		       sprintf
			 "Container %d: processing container connection \
                          on fd %Ld"
			 (Oo.id self) 
			 (Netsys.int64_of_file_descr fd_slave)
		    );
		  self # protect
		    "process"
		    (sockserv # processor # process
		       ~when_done:(fun fd ->
				     if not !when_done_called then (
				       when_done_called := true;
				       dlogr
					 (fun () ->
					    sprintf "Container %d: \
                                    Done with container connection on fd %Ld"
					      (Oo.id self) 
					      (Netsys.int64_of_file_descr 
						 fd_slave))
				     )
				  )
		       (self : #container :> container)
		       fd_slave)
		    proto#name;
		  self # accept_on_container_socket proto fd
	       )
      ~is_error:(fun err ->
		   self # log `Crit
		     ("accept: Exception " ^ 
			Netexn.to_string err)
		)
      e;
    cs_engines <- e :: cs_engines


  method private disable_container_sockets() =
    dlogr
      (fun () ->
	 sprintf "Container %d: No longer accepting cont conns" (Oo.id self));
    List.iter (fun e -> e # abort()) cs_engines;
    cs_engines <- []


  method private close_container_sockets() =
    List.iter
      (fun (fd, _, _) ->
	 Netplex_util.close_server_socket fd
      )
      cs_sockets;
    cs_sockets <- []


  method private register_container_socket sname pname path =
    match sys_rpc with
      | None -> failwith "#register_container_socket: No RPC client available"
      | Some r ->
	  Uq_mt.monitor_async
	    sys_mon
	    (Netplex_ctrl_clnt.System.V1.register_container_socket'async r)
	    (sname, pname, path)

  method owned_container_sockets =
    List.map
      (fun (_, pname, path) -> (pname,path))
      cs_sockets
    

  method lookup_container_sockets service protocol =
    match sys_rpc with
      | None -> failwith "#lookup_container_sockets: No RPC client available"
      | Some r ->
	  Uq_mt.monitor_async
	    sys_mon
	    (Netplex_ctrl_clnt.System.V1.lookup_container_sockets'async r)
	    (service,protocol)


  method startup_directory =
    sockserv # startup_directory
end


(* The admin container is special because the esys is shared with the
   system-wide controller
 *)

class admin_container esys ptype sockserv =
object(self)
  inherit std_container ~esys ptype sockserv as super

  val mutable c_fd_clnt = None
  val mutable c_sys_fd_clnt = None

  method start fd_clnt sys_fd_clnt =
    if rpc <> None then
      failwith "#start: already started";
    let rpc_cl =
      Netplex_ctrl_clnt.Control.V1.create_client
	~esys
	(Rpc_client.Descriptor fd_clnt)
	Rpc.Tcp in
    Rpc_client.Debug.disable_for_client rpc_cl;
    rpc <- Some rpc_cl;
    let sys_rpc_cl =
      Netplex_ctrl_clnt.System.V1.create_client
	~esys:sys_esys
	(Rpc_client.Descriptor sys_fd_clnt)
	Rpc.Tcp in
    Rpc_client.Debug.disable_for_client sys_rpc_cl;
    sys_rpc <- Some sys_rpc_cl;
    c_fd_clnt <- Some fd_clnt;
    c_sys_fd_clnt <- Some sys_fd_clnt;
    self # restart_polling();

  method private shutdown_extra() =
    (* In the admin case, fd_clnt and sys_fd_clnt are never closed.
       Do this now. (In the non-admin case the caller does this after
       [start] returns.)
     *)
    super # shutdown_extra();
    dlogr
      (fun () ->
	 sprintf "Container %d: Closing admin clients" (Oo.id self));
    ( match c_fd_clnt with
	| None -> ()
	| Some fd -> 
	    let fd_style = Netsys.get_fd_style fd in
	    ( try Netsys.gshutdown fd_style fd Unix.SHUTDOWN_ALL
	      with _ -> ()
	    );
	    Netlog.Debug.release_fd fd;
	    Netsys.gclose fd_style fd;
	    c_fd_clnt <- None
    );
    ( match c_sys_fd_clnt with
	| None -> ()
	| Some fd -> 
	    let fd_style = Netsys.get_fd_style fd in
	    ( try Netsys.gshutdown fd_style fd Unix.SHUTDOWN_ALL
	      with _ -> ()
	    );
	    Netlog.Debug.release_fd fd;
	    Netsys.gclose fd_style fd;
	    c_sys_fd_clnt <- None
    )
end


let create_container ptype sockserv =
  new std_container ptype sockserv

let create_admin_container esys ptype sockserv =
  new admin_container esys ptype sockserv
