(* $Id: netplex_controller.ml 1718 2012-02-21 14:59:45Z gerd $ *)

open Netplex_types
open Netplex_ctrl_aux
open Printf


module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netplex_controller" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netplex_controller" Debug.enable

let () =
  Netlog.Debug.register_module "Netplex_controller" Debug.enable

let ast_re = Netstring_str.regexp "[*]";;

let regexp_of_pattern s =
  let l = Netstring_str.split_delim ast_re s in
  Netstring_str.regexp
    (String.concat ".*" (List.map (fun u -> Netstring_str.quote u) l) ^ "$")


class type extended_socket_controller =
object
  inherit socket_controller
  method forward_message : message -> unit
  method forward_admin_message : message -> unit
  method forward_system_shutdown : (unit -> unit) -> unit
  method detach : unit -> unit
end

and extended_controller =
object
  inherit controller
  method ext_services : (socket_service * extended_socket_controller * workload_manager) list
  method plugin_receive_call : int -> container_id -> string -> string -> (string -> unit) -> (unit->unit) -> unit
  method plugin_container_finished : container_id -> bool -> unit
end

type sds_state = [ `None | `Notify of unit->unit | `Notified of unit->unit ]

type ext_cont_state =
    { container : container_id;
      mutable cont_state : container_state;
      mutable rpc : Rpc_server.t;
      mutable sys_rpc : Rpc_server.t;
      mutable par_thread : par_thread;
      mutable poll_call : (Rpc_server.session * 
			     (Netplex_ctrl_aux.t_Control'V1'poll'res -> unit)
			  ) option;
      mutable messages : message Queue.t;
      mutable admin_messages : message Queue.t;
      mutable shutting_down_system : sds_state; 
                                            (* during system_shutdown phase *)
      mutable shutting_down : bool;         (* real shutdown *)
      mutable t_accept : float;
      mutable cs_paths : string list;   (* container sockets *)
      mutable greediness_max : int;
                 (* how many connections to accept in one go, at max *)
    }


type action =
    [ `None
	(* No container exists/no notification in progress *)
    | `Selected of ext_cont_state
	(* The scheduler selected this container for the next [accept] *)
    | `Notified of ext_cont_state
	(* The container even knows it is selected for [accept] *)
    | `Deselected of ext_cont_state
	(* The container was notified and must be actively deselected
         * because of restart/shutdown
         *)
    ]


let cap_gt cap1 cap2 =
  (* cap1 has more capacity than cap2 *)
  match cap1 with
    | `Unavailable -> false
    | `Low_quality (n1,_) ->
	( match cap2 with
	    | `Unavailable -> true
	    | `Low_quality (n2,_) -> n1 > n2
	    | _ -> false
	)
    | `Normal_quality (n1,_) ->
	( match cap2 with
	    | `Unavailable -> true
	    | `Low_quality _ -> true
	    | `Normal_quality (n2,_) -> n1 > n2
	)
;;


let create_pipe_pair() =
  match Sys.os_type with
    | "Win32" ->
	let (ph2, ph1) = Netsys_win32.pipe_pair Netsys_win32.Pipe_duplex in
	(Netsys_win32.pipe_descr ph1,
	 Netsys_win32.pipe_descr ph2,
	 `W32_pipe
	)
    | _ ->
	let (fd1,fd2) =
	  Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	(fd1,fd2,`Read_write)


(* TODO: represent clist as [Set] *)


class std_socket_controller ?(no_disable = false)
                            rm_service (par: parallelizer) 
                            controller sockserv wrkmng cs_directory
			    ctrl_detach
      : extended_socket_controller =
  (* cs_directory: tuples (service_name, proto_name, path, component) *)
  let name = sockserv # name in
  let esys = controller # event_system in
object(self)
  val mutable state = (`Disabled : socket_state)
  val mutable clist = []
  val mutable action = (`None : action)
  val mutable n_failures = 0
    (* The number of processes in [`Starting] state that never started to
     * poll. Used to detect massive numbers of start-up failures.
     *)

  val mutable group = Unixqueue.new_group esys
    (* The group for timers etc. *)

  val eps_group = Unixqueue.new_group esys
    (* The group for Unixqueue.once 0.0 *)


  initializer (
    Unixqueue.once esys group 1.0 (fun () -> self#alive_check esys group)
  )


  method state = state

  method container_state =
    List.map 
      (fun c -> 
	 ( (c.container :> container_id), 
	   c.par_thread # info_string,
	   c.cont_state,
	   match action with
	     | `Selected c' when c' == c -> true
	     | `Notified c' when c' == c -> true
	     | `Deselected c' when c' == c -> true
	     | _ -> false
	 )
      ) 
      clist


  method private alive_check esys g =
    (* To be called every second or so. This is a "parachute" to prevent
     * problems caused by bugs in the workload manager.
     *)
    if state = `Enabled && action = `None then (
      try
	self # adjust();
	self # schedule();
      with
	| error ->
	    controller # logger # log 
	      ~component:"netplex.controller"
	      ~level:`Crit
	      ~message:("Exception in alive_check: " ^ Netexn.to_string error)
    );
    Unixqueue.once esys g 1.0 (fun () -> self#alive_check esys g)


  method enable() =
    dlogr (fun () -> sprintf "Service %s: enable" name);
    match state with
      | `Disabled ->
	  dlogr 
	    (fun () -> sprintf "Service %s: setting state to `Enabled" name);
	  n_failures <- 0;
	  state <- `Enabled;
	  self # schedule()
      | `Enabled ->
	  ()
      | `Restarting true ->
	  ()
      | `Restarting false ->
	  dlogr 
	    (fun () -> 
	       sprintf "Service %s: setting state to `Restarting" name);
	  state <- `Restarting true
      | _ ->
	  failwith "#enable: service is already down"

  method disable() =
    dlogr (fun () -> sprintf "Service %s: disable" name);
    if no_disable then
      failwith "#disable: not allowed for this service";
    match state with
      | `Disabled ->
	  ()
      | `Enabled ->
	  dlogr 
	    (fun () -> sprintf "Service %s: setting state to `Disabled" name);
	  state <- `Disabled;
	  ( match action with
	      | `None
	      | `Selected _ ->
		  action <- `None
	      | `Notified c ->
		  action <- `Deselected c;
		  self # check_for_poll_reply c
	      | `Deselected _ ->
		  ()
	  )
      | `Restarting true ->
	  dlogr 
	    (fun () -> 
	       sprintf "Service %s: setting state to `Restarting" name);
	  state <- `Restarting false
      | `Restarting false ->
	  ()
      | `Down ->
	  ()

  method restart() =
    dlogr 
      (fun () -> 
	 sprintf "Service %s: restart / setting state to `Restarting" name);
    let flag =
      match state with
	| `Disabled -> false
	| `Enabled -> true
	| `Restarting f -> f
	| `Down ->
	    failwith "#restart: service is already down" in
    state <- `Restarting flag;
    self # stop_all_containers()

  method shutdown() =
    (* We never close the master sockets or remove socket files. That would
     * make it impossible to restart the service later.
     *)
    dlogr 
      (fun () -> 
	 sprintf "Service %s: shutdown / setting state to `Down" name);
    state <- `Down;
    Unixqueue.clear esys group;
    self # stop_all_containers();

  method start_containers n =
    dlogr 
      (fun () -> 
	 sprintf "Service %s: Starting %d new containers" name n);
    let threads = ref [] in
    let j = ref 0 in
    for k = 1 to n do
      match self # start_single_container() with
	| Some thr ->
	    threads := thr :: !threads;
	    incr j
	| None ->
	    ()   (* error - already handled *)
    done;
    dlogr 
      (fun () -> 
	 sprintf 
	   "Service %s: Started %s" 
	   name 
	   (String.concat "," (List.map (fun p -> p#info_string) !threads)));
    !j


  method private start_single_container() =
    let onerror = ref [] in
    try
      let container = sockserv # create_container par#ptype sockserv in
      let fd_clnt_closed = ref false in
      let fd_srv_closed = ref false in
      let (fd_clnt, fd_srv, fd_style) = create_pipe_pair() in
      (* We only track the client here. The server is tracked by Rpc_server *)
      Netlog.Debug.track_fd
	~owner:"Netplex_controller"
	~descr:("Ctrl client for container " ^ string_of_int(Oo.id container))
	fd_clnt;
      onerror := (fun () -> 
		    if not !fd_clnt_closed then (
		      fd_clnt_closed := true;
		      Netlog.Debug.release_fd fd_clnt;
		      Netsys.gclose fd_style fd_clnt
		    );
		    if not !fd_srv_closed then (
		      fd_srv_closed := true;
		      Netsys.gclose fd_style fd_srv
		    )
		 ) :: !onerror;
      dlogr (fun () ->
	       sprintf "Service %s: Container %d uses pipe %Ld:%Ld"
		 name (Oo.id container) 
		 (Netsys.int64_of_file_descr fd_clnt)
		 (Netsys.int64_of_file_descr fd_srv));
      let sys_fd_clnt_closed = ref false in
      let sys_fd_srv_closed = ref false in
      let (sys_fd_clnt, sys_fd_srv, sys_fd_style) = create_pipe_pair() in
      Netlog.Debug.track_fd
	~owner:"Netplex_controller"
	~descr:("Sys ctrl client for container " ^ 
		  string_of_int(Oo.id container))
	sys_fd_clnt;
      onerror := (fun () -> 
		    if not !sys_fd_clnt_closed then (
		      sys_fd_clnt_closed := true;
		      Netlog.Debug.release_fd sys_fd_clnt;
		      Netsys.gclose sys_fd_style sys_fd_clnt
		    );
		    if not !sys_fd_srv_closed then (
		      sys_fd_srv_closed := true;
		      Netsys.gclose sys_fd_style sys_fd_srv
		    )
		 ) :: !onerror;
      dlogr (fun () ->
	       sprintf "Service %s: Container %d uses sys pipe %Ld:%Ld"
		 name (Oo.id container) 
		 (Netsys.int64_of_file_descr sys_fd_clnt)
		 (Netsys.int64_of_file_descr sys_fd_srv));
      let fd_share =   (* descriptors to share between controller and cont *)
	fd_clnt :: sys_fd_clnt ::
	  (List.flatten
	     (List.map
		(fun (_, fd_arr) -> Array.to_list fd_arr)
		sockserv # sockets
	     )
	  ) in
      let fd_close =   (* descriptors to close in the container *)
	if par # ptype = `Multi_processing then
	  [ fd_srv; sys_fd_srv] 
	else
	  [] in
      dlogr
	(fun () ->
	   sprintf "Service %s: Container %d: pre_start"
	     name (Oo.id container));
      sockserv # processor # pre_start_hook 
	sockserv
	(controller :> controller)
	(container :> container_id);
      dlogr
	(fun () ->
	   sprintf "Service %s: Container %d: done pre_start"
	     name (Oo.id container));
      let par_thread =
	par # start_thread
	  (fun par_thread ->
	     Netplex_cenv.register_cont container par_thread;
	     ( try 
		 (* If this is a fork, reclaim memory: *)
		 if par # ptype = `Multi_processing then (
		   ctrl_detach();
		   Netsys_mem.pool_reclaim Netsys_mem.default_pool;
		   Netsys_mem.pool_reclaim Netsys_mem.small_pool;
		 );
		 (* This is the real start of the container: *)
		 container # start fd_clnt sys_fd_clnt;
	       with 
		 | error ->
		     (* It is difficult to get this error written to a log file *)
		     prerr_endline ("Netplex Catastrophic Error in " ^ name ^
				      ": " ^ Netexn.to_string error);
		     ()
	     );
	     (* We return when the container is done. For the admin container
                we always return, because no extra thread is started!
	      *)
	     if par # ptype <> `Controller_attached then (
	       Netplex_cenv.cancel_all_timers();
	       Netplex_cenv.unregister_cont container par_thread;
	       (* indicates successful termination: *)
	       if par # ptype <> `Multi_processing then 
		 List.iter
		   (fun fd ->
		      ( try Netsys.gshutdown fd_style fd Unix.SHUTDOWN_ALL
			with _ -> ()
		      );
		      Netlog.Debug.release_fd fd;
		      Netsys.gclose fd_style fd;
		   )
		   [ fd_clnt; sys_fd_clnt ]
		   (* Multi-processing: we do not close explicitly, but
		      at process exit time. This has the advantage that we
		      never have to wait for the process when the closed
		      descriptor is recognized.
		    *)
	     )
	  )
	  fd_close
	  fd_share
	  sockserv#name
	  controller#logger in
      if par # ptype = `Multi_processing then (
	(* In the multi processing case, the client descriptors are no longer
           needed in the master process (dups of them will still play a role
           in the forked children, of course)
	 *)
	Netlog.Debug.release_fd fd_clnt;
	Netsys.gclose fd_style fd_clnt;
	Netlog.Debug.release_fd sys_fd_clnt;
	Netsys.gclose sys_fd_style sys_fd_clnt;
      );
      (* From now on it does not make sense to close the client descriptors
         in case of errors. Either they are already closed (multi processing),
         or a thread is started which will take care of closing.
       *)
      fd_clnt_closed := true;
      sys_fd_clnt_closed := true;
      dlogr
	(fun () -> sprintf "Service %s: creating control server" name);
      let rpc =
	Rpc_server.create2 
	  (`Socket_endpoint(Rpc.Tcp, fd_srv))
	  controller#event_system in
      if not !Debug.enable then
	Rpc_server.Debug.disable_for_server rpc;
      Rpc_server.set_exception_handler rpc
	(fun err ->
	   controller # logger # log
	     ~component:sockserv#name
	     ~level:`Crit
	     ~message:("Control server caught exception: " ^ 
			 Netexn.to_string err));
      dlogr
	(fun () -> sprintf "Service %s: creating system server" name);
      let sys_rpc =
	Rpc_server.create2 
	  (`Socket_endpoint(Rpc.Tcp, sys_fd_srv))
	  controller#event_system in
      if not !Debug.enable then
	Rpc_server.Debug.disable_for_server sys_rpc;
      Rpc_server.set_exception_handler sys_rpc
	(fun err ->
	   controller # logger # log
	     ~component:sockserv#name
	     ~level:`Crit
	     ~message:("System server caught exception: " ^ 
			 Netexn.to_string err));
      let c =
	{ container = (container :> container_id);
	  cont_state = `Starting (Unix.gettimeofday());
	  rpc = rpc;
	  sys_rpc = sys_rpc;
	  par_thread = par_thread;
	  poll_call = None;
	  messages = Queue.create();
	  admin_messages = Queue.create();
	  shutting_down_system = `None;
	  shutting_down = false;
	  t_accept = 0.0;
	  cs_paths = [];
	  greediness_max = 1;
	} in
      clist <- c :: clist;
      (* Now that the server descriptors are handed off to the rpc servers
         and are implicitly included in c, we can forget about closing them
         here in case of errors:
       *)
      fd_srv_closed := true;
      sys_fd_srv_closed := true;
      self # bind_server rpc sys_rpc c;
      Rpc_server.set_onclose_action rpc 
	(fun _ ->
	   dlogr
	     (fun () ->
		sprintf "Service %s: Container %d: closing"
		  name (Oo.id container));
	   par_thread # watch_shutdown controller#event_system;
	   self # onclose_action c container
	);
      (* Watch the new container. If it does not call [poll] within 60 seconds,
       * drop the process/thread.
       *)
      let startup_timeout = 
	sockserv # socket_service_config # startup_timeout in
      if startup_timeout >= 0.0 then
	Unixqueue.once esys group startup_timeout
	  (fun () ->
	     let is_starting =
	       match c.cont_state with `Starting _ -> true | _ -> false in
	     if List.memq c clist && is_starting then (
	       (* After 60 seconds still starting. This is a bad process! *)
	       controller # logger # log
		 ~component:sockserv#name
		 ~level:`Crit
		 ~message:"Container process/thread does not start up";
	       (* Immediate measure: Remove it from the list of containers *)
	       clist <- List.filter (fun c' -> c' != c) clist;
	       (* [watch_shutdown] will kill the process if possible *)
	       par_thread # watch_shutdown controller#event_system;
	       
	       (* No need to call onclose_action. This _will_ be done if the
                * process is finally dead.
                *)
	     )
	  );
      Some par_thread
    with
      | error ->
	  controller # logger # log
	    ~component:"netplex.controller"
	    ~level:`Crit
	    ~message:("Exception while starting new containers: " ^ 
			Netexn.to_string error);
	  List.iter (fun f -> f()) !onerror;
	  None
	  


  method private onclose_action c container =
    (* Called back when fd_clnt is closed by the container, i.e. when
     * the container process terminates (normally/crashing)
     *)
    let is_starting =
      match c.cont_state with `Starting _ -> true | _ -> false in
    if is_starting then
      n_failures <- n_failures + 1;
    clist <- List.filter (fun c' -> c' != c) clist;
    self # unreg_cont_sockets c;
    dlogr
      (fun () ->
	 sprintf "Service %s: Container %d: post_finish"
	   name (Oo.id container));
    ( try
	sockserv # processor # post_finish_hook 
	  sockserv
	  (controller :> controller)
	  (container :> container_id)
      with
	| error ->
	    controller # logger # log
	      ~component:sockserv#name
	      ~level:`Crit
	      ~message:("post_finish_hook: Exception " ^ 
			  Netexn.to_string error)
    );
    dlogr
      (fun () ->
	 sprintf "Service %s: Container %d: done post_finish"
	   name (Oo.id container));
    controller # plugin_container_finished 
      (container :> container_id) (clist = []);
    (* If a system shutdown is in progress, notify now: *)
    ( match c.shutting_down_system with
	| `None -> ()
	| `Notify f -> f()
	| `Notified f -> f()
    );
    (* Maybe we have to start new containers: *)
    self # adjust();
    (* Maybe the dead container was selected for accepting connections.
     * In this case, reschedule:
     *)
    let reschedule =
      match action with
	| `Selected c' when c == c' -> true
	| `Notified c' when c == c' -> true
	| `Deselected c' when c == c' -> true
	| _ -> false in
    if reschedule then (
      action <- `None;
      self # schedule()
	(* Note: [schedule] is a no-op if the service is not enabled *)
    );
    (* Maybe we are restarting or shutting down. If this is the last
     * container of the service, continue this action:
     *)
    self # nocontainer_action()

  method private nocontainer_action() =
    if clist = [] then (
      match state with
	| `Restarting flag ->
	    (* Set to [`Disabled] and re-enable: *)
	    dlogr
	      (fun () ->
		 sprintf "Service %s: last container exited; about to restart"
		   name);
	    state <- `Disabled;
	    if flag then self # enable()
	| `Down ->
	    dlogr
	      (fun () ->
		 sprintf "Service %s: last container exited; finishing service"
		   name);
	    rm_service 
	      (self : #extended_socket_controller
	       :> extended_socket_controller);
	| _ ->
	    ()
    )
    

  method stop_containers l =
    List.iter
      (fun c ->
	 if List.mem (c.container :> container_id) l then (
	   dlogr
	     (fun () ->
		sprintf "Service %s: Stopping container %d"
		  name (Oo.id c.container));
	   c.shutting_down <- true;
	   c.cont_state <- `Shutting_down;
	   ( match action with
	       | `Notified c' when c' == c ->
		   action <- `Deselected c
	       | _ -> ()
	   );
	   self # check_for_poll_reply c
	 )
      )
      clist

  method private stop_all_containers () =
    action <- `None;
    List.iter
      (fun c ->
	 dlogr
	   (fun () ->
	      sprintf "Service %s: Stopping container %d"
		name (Oo.id c.container));
	 c.shutting_down <- true;
	 c.cont_state <- `Shutting_down;
	 self # check_for_poll_reply c
      )
      clist;
    (* Maybe clist is already empty... *)
    self # nocontainer_action()


  method private adjust() =
    if n_failures >= 10 then (
      controller # logger # log
	~component:sockserv#name
	~level:`Alert
	~message:("Disabling service after 10 startup failures");
      state <- `Disabled;
    )
    else (
      try
	dlogr (fun () -> sprintf "Service %s: Adjusting workload" name);
	wrkmng # adjust 
	  sockserv (self : #socket_controller :> socket_controller)
      with
	| error ->
	    controller # logger # log
	      ~component:sockserv#name
	      ~level:`Crit
	      ~message:("Exception in workload manager, function adjust: " ^ 
			  Netexn.to_string error)
    )

  method private schedule() =
    (* Determine the next container that will have the chance to accept a 
     * connection
     *)
    if state = `Enabled && action = `None then (
      if clist = [] then
	self # adjust();
      let best = ref (None, `Unavailable) in
      let now = Unix.gettimeofday() in
      let have_young_starters = ref false in
      List.iter
	(fun c ->
	   match c.cont_state with
	     | `Busy -> ()  (* ignore *)
	     | `Starting t -> 
		 if now -. t < 1.0 then have_young_starters := true
	     | `Shutting_down -> ()  (* ignore *)
	     | `Accepting(n, t_last) ->
		 let cap = 
		   wrkmng # capacity 
		     (c.container :> container_id) c.cont_state in
		 let greediness_max =
		   match cap with
		     | `Normal_quality(n,g)
		     | `Low_quality(n,g) -> 
			 min 10 (if g then n else min n 1)
		     | _ -> 0 in
		 c.greediness_max <- greediness_max;
		 if cap <> `Unavailable then (
		   match !best with
		     | None, _ -> 
			 best := (Some c, cap)
		     | Some c', cap' ->
			 if cap_gt cap cap' then
			   best := (Some c, cap)
		 )
	)
	clist;
      ( match !best with
	  | None, _ -> 
	      dlogr (fun () -> sprintf "Service %s: All containers busy" name);
	      ()   (* All containers are busy! *)
	  | Some c, best_cap ->
	      (* If there are starting processes that are younger than 1 sec,
               * and the best container is already overloaded, we do not
               * select any container. This choice would be very bad, and
               * we do not have logic to correct it once the starting processes
               * are ready. So defer scheduling for a small period of time.
               *)
	      let bad_best_cap =
		match best_cap with `Low_quality _ -> true | _ -> false in

	      if !have_young_starters && bad_best_cap then (
		dlogr
		  (fun () -> 
		     sprintf "Service %s: Not selecting any container \
                              because of temporary overload"
		       name);
	      )
	      else (
		dlogr
		  (fun () ->
		     sprintf 
		       "Service %s: Selecting container %d in %s (bad=%b, \
                        grdymax=%d)"
		       name (Oo.id c.container)
		       c.par_thread#info_string 
		       bad_best_cap
		       c.greediness_max
		  );
		action <- `Selected c;
		self # check_for_poll_reply c
	      )
      )
    )

  method private bind_server rpc sys_rpc c =
    Netplex_ctrl_srv.Control.V1.bind_async
      ~proc_ping:(fun _ _ reply -> reply())
      ~proc_poll:(self # poll c)
      ~proc_accepted:(self # accepted c)
      rpc;
    Netplex_ctrl_srv.System.V1.bind_async
      ~proc_ping:(fun _ _ reply -> reply())
      ~proc_lookup:(self # lookup c)
      ~proc_send_message:(self # proc_send_message c)
      ~proc_log:(self # log c)
      ~proc_call_plugin:(self # call_plugin c)
      ~proc_register_container_socket:(self # reg_cont_socket c)
      ~proc_lookup_container_sockets:(self # lookup_cont_sockets c)
      ~proc_activate_lever:(self # activate_lever)
      sys_rpc

  method private poll c sess (n,fully_busy) reply =
    (* Last [poll] call still unreplied? If so, send EVENT_NONE: *)
    ( match c.poll_call with
	| None -> ()
	| Some (last_sess, last_reply) ->
	    dlogr 
	      (fun () -> sprintf "Service %s, cont %d: %s <- Event_none"
		 name (Oo.id c.container) c.par_thread#info_string);
	    last_reply `event_none
    );

    dlogr 
      (fun () -> sprintf "Service %s, cont %d: %s -> poll(%d)"
	 name (Oo.id c.container) c.par_thread#info_string n);

    let is_starting =
      match c.cont_state with `Starting _ -> true | _ -> false in
    c.poll_call <- Some (sess, reply);
    if is_starting then
      n_failures <- 0;
    if c.cont_state <> `Shutting_down && not fully_busy then (
      (* If n is updated, we must call [adjust] asap. Before [schedule]! *)
      let old_state = c.cont_state in
      c.cont_state <- `Accepting(n, c.t_accept);
      ( match old_state with
	  | `Accepting(n_old, _) ->
	      if n_old <> n then self # adjust()
	  | _ ->
	      self # adjust()
      );
    );
    self # schedule();
    self # check_for_poll_reply c

  method private check_for_poll_reply c =
    match c.poll_call with
      | None -> ()
      | Some (sess, reply) ->
	  let reply ev =
	    dlogr 
	      (fun () -> sprintf "Service %s, cont %d: %s <- poll returns %s"
		 name (Oo.id c.container) c.par_thread#info_string
		 (Netplex_container.string_of_event ev)
	      );
	    reply ev
	  in
	  let sd_done =
	    match c.shutting_down_system with
	      | `None -> false
	      | `Notify f ->
		  c.shutting_down_system <- `Notified f;
		  reply `event_system_shutdown;
		  c.poll_call <- None;
		  true
	      | `Notified f ->
		  c.shutting_down_system <- `None;
		  f();
		  false
	  in
	  if sd_done then 
	    ()
	  else if not (Queue.is_empty c.messages) then (
	    let msg = Queue.take c.messages in
	    reply (`event_received_message msg);
	    c.poll_call <- None
	  )
	  else if not (Queue.is_empty c.admin_messages) then (
	    let msg = Queue.take c.admin_messages in
	    reply (`event_received_admin_message msg);
	    c.poll_call <- None
	  )
	  else if c.shutting_down then (
	    reply `event_shutdown;
	    c.poll_call <- None;
	    ( match action with
		| `Deselected c' when c' == c ->
		    action <- `None;
		    self # schedule()
		      (* Note: we have here a race condition. I think
                       * it is harmless, however:
                       * It may happen that c and the newly scheduled
                       * container accept connections in parallel.
                       *)
		| _ -> ()
	    )
	  )
	  else 
	    ( match action with
		| `Selected c' when c' == c ->
		    let n_accept =
		      match c.cont_state with
			| `Accepting(n_running,_) ->
			    min 
			      (10 + n_running) 
			      (max c.greediness_max 1)
			| _ -> 1  (* strange *) in
		    reply (`event_accept n_accept);
		    c.poll_call <- None;
		    action <- `Notified c;
		    self # adjust();
		    (* PROBLEM: This adjust call is bogus because the 
                     * number of connections is not yet updated.
                     *)
		| `Deselected c' when c' == c ->
		    reply `event_noaccept;
		    c.poll_call <- None;
		    action <- `None;
		    self # schedule()
		| _ ->
		    ()
	    )
	      
  method private accepted c sess arg reply =
    dlogr 
      (fun () -> sprintf "Service %s, cont %d: %s -> accepted() NOREPLY"
	 name (Oo.id c.container) c.par_thread#info_string);
    match action with
      | `Notified c' when c' == c ->
	  c.t_accept <- Unix.gettimeofday();
	  if c.cont_state <> `Shutting_down then
	    c.cont_state <- `Busy;
	  action <- `None;
	  (* We call [adjust] here even although this can make workload
           * management much harder, because many containers are only
           * `Busy for a short time. However, it would be possible that 
           * required containers are not started if we did not do it.
           *)
	  self # adjust();
	  self # schedule()

      | _ -> ();
	  (* This call is not replied! *)

  method private lookup c sess (srv_name, proto_name) reply =
    let path = ref None in
    List.iter
      (fun (sockserv, _, _) ->
	 if sockserv#name = srv_name then
	   List.iter
	     (fun p ->
		if p#name = proto_name && !path = None then
		  Array.iter
		    (fun addr ->
		       match addr with
			 | `Socket (Unix.ADDR_UNIX p) -> 
			     path := Some p
			 | `Socket_file p ->
			     path := Some p
			 | `W32_pipe_file p ->
			     path := Some p
			 | _ -> ()
		    )
		    p # addresses
	     )
	     sockserv # socket_service_config # protocols
      )
      controller#services;
    reply !path

  method private reg_cont_socket c sess (serv_name, proto_name, path) reply =
    c.cs_paths <- path :: c.cs_paths;
    let already_registered =
      List.exists 
	(fun (sn,pn,p,_) -> sn=serv_name && pn = proto_name && p = path)
	!cs_directory in
    if not already_registered then (
      (* eprintf "Registering: %s %s\n%!" serv_name proto_name; *)
      cs_directory := (serv_name, proto_name, path, c) :: !cs_directory;
    );
    reply ()

  method private unreg_cont_sockets c =
    cs_directory := 
      (List.filter
	 (fun (_,_,_,c') -> c != c')
	 !cs_directory
      );
    List.iter
      (fun p ->
	 (try Sys.remove p with _ -> ())
      )
      c.cs_paths;
    c.cs_paths <- []

  method private lookup_cont_sockets _ sess (serv_name, proto_name) reply =
    (* eprintf "Look up: %s %s\n%!" serv_name proto_name; *)
    let l =
      List.map
	(fun (_,_,path,_) -> path)
	(List.filter
	   (fun (sn,pn,_,_) -> sn = serv_name && pn = proto_name)
	   !cs_directory) in
    reply (Array.of_list l)

  method private proc_send_message c sess (pat, msg) reply =
    controller # send_message pat msg.msg_name msg.msg_arguments;
    reply ()

  method forward_message msg =
    List.iter
      (fun c ->
	 Queue.push msg c.messages;
	 self # check_for_poll_reply c
      )
      clist

  method forward_admin_message msg =
    match msg.msg_name with
      | "netplex.threadlist" ->
	  self # threadlist()
      | "netplex.logger.set_max_level"
	  when sockserv#name = "netplex.controller" ->
	  ( try
	      let s_level = 
		match msg.msg_arguments with
		  | [| s |] -> s
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      let level = Netplex_log.level_of_string s_level in
	      controller # controller_config # set_max_level level
	    with
	      | Failure s ->
		  controller # logger # log 
		    ~component:sockserv#name
		    ~level:`Err
		    ~message:("netplex.logger.set_max_level: " ^ s)
	  )
      | "netplex.debug.enable" when sockserv#name = "netplex.controller" ->
	  ( try
	      let m = 
		match msg.msg_arguments with
		  | [| m |] -> m
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      Netlog.Debug.enable_module m
	    with
	      | Failure s ->
		  controller # logger # log 
		    ~component:sockserv#name
		    ~level:`Err
		    ~message:("netplex.debug.enable: " ^ s)
	  )
      | "netplex.debug.disable" when sockserv#name = "netplex.controller" ->
	  ( try
	      let m = 
		match msg.msg_arguments with
		  | [| m |] -> m
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      Netlog.Debug.disable_module m
	    with
	      | Failure s ->
		  controller # logger # log 
		    ~component:sockserv#name
		    ~level:`Err
		    ~message:("netplex.debug.disable: " ^ s)
	  )
      | _ ->
	  List.iter
	    (fun c ->
	       Queue.push msg c.admin_messages;
	       self # check_for_poll_reply c
	    )
	    clist

  method forward_system_shutdown f_done =
    let n = ref 0 in
    List.iter
      (fun c ->
	 incr n;
	 c.shutting_down_system <- `Notify (fun () ->
					      decr n;
					      if !n = 0 then f_done()
					   );
	 self # check_for_poll_reply c
      )
      clist;
    if !n = 0 then f_done()

  val lev_trans =
    [ log_emerg, `Emerg;
      log_alert, `Alert;
      log_crit, `Crit;
      log_err, `Err;
      log_warning, `Warning;
      log_notice, `Notice;
      log_info, `Info;
      log_debug, `Debug
    ]

  method private log c sess (lev, subchannel, message) reply =
    let level = 
      try List.assoc lev lev_trans 
      with Not_found -> `Emerg in
    controller # logger # log_subch
      ~component:sockserv#name
      ~subchannel
      ~level
      ~message
	  (* This call is not replied! *)

  method private threadlist() =
    List.iter
      (fun c ->
	 let msg = 
	   sprintf "%20s: %s (%s)%s" 
	     c.par_thread#info_string sockserv#name
	     ( match c.cont_state with
		 | `Accepting(n,_) -> string_of_int n ^ " jobs, accepting"
		 | `Busy -> "busy"
		 | `Starting _ -> "starting"
		 | `Shutting_down -> "shutdown"
	     )
	     ( match action with
		 | `Selected c' when c' == c -> " (selected)"
		 | `Notified c' when c' == c -> " (selected*)"
		 | `Deselected c' when c' == c -> " (deselected)"
		 | _ -> ""
	     ) in
	 controller # logger # log
	   ~component:"netplex.controller"
	   ~level:`Notice
	   ~message:msg
      )
      clist

  method private call_plugin c sess (plugin_id,proc_name,proc_arg) reply =
    controller # plugin_receive_call
      (Int64.to_int plugin_id)
      c.container
      proc_name
      proc_arg
      (fun r -> 
	 try reply r
	 with Rpc_server.Connection_lost -> ())
      (fun () -> 
	 try Rpc_server.reply_error sess Rpc.System_err
	 with Rpc_server.Connection_lost -> ())

  method private activate_lever sess (id, arg_str) reply =
    try
      let arg_enc = (Marshal.from_string arg_str 0 : encap) in
      let res_enc = controller # activate_lever id arg_enc in
      let res_str = Marshal.to_string res_enc [] in
      ( try reply res_str
	with Rpc_server.Connection_lost -> ()
      )
    with
      | error ->
	  controller # logger # log
	   ~component:"netplex.controller"
	   ~level:`Err
	   ~message:(sprintf "activate_lever: exception %s"
		       (Netexn.to_string error));
	  ( try Rpc_server.reply_error sess Rpc.System_err
	    with Rpc_server.Connection_lost -> ()
	  )

  method detach () =
    List.iter
      (fun c ->
	 Rpc_server.detach c.rpc;
	 Rpc_server.detach c.sys_rpc;
      )
      clist;
    clist <- []

end


class deferring_logger =
object(self)
  val queue = Queue.create()

  method log_subch ~component ~subchannel ~level ~message =
    Queue.push (component,subchannel,level,message) queue

  method log = self # log_subch ~subchannel:""

  method reopen() = ()

  method max_level : Netplex_types.level = `Debug

  method set_max_level (_ : Netplex_types.level) = ()

  method forward (l : logger) =
    Queue.iter
      (fun (component,subchannel,level,message) ->
	 l # log_subch ~component ~subchannel ~level ~message
      )
      queue;
    Queue.clear queue
end


class admin_par : Netplex_types.parallelizer =
  (* A special parallelizer used for the admin interface *)
object(self)
  method ptype = `Controller_attached

  method init() = ()

  method start_thread : (par_thread -> unit) -> 'x -> 'y -> string -> logger -> par_thread =
    fun f l_close l_share srv_name logger ->
      let pid = Unix.getpid() in
      let throbj =
	( object
	    method ptype = `Controller_attached
	    method info_string = "AttachedToCtrlProcess " ^ string_of_int pid
	    method sys_id = assert false
	    method parallelizer = (self : #parallelizer :> parallelizer)
	    method watch_shutdown _ = ()
	  end
	) in
      f throbj;
      throbj

  method current_sys_id = assert false

  method create_mem_mutex () = assert false

end


class controller_processor setup controller : processor =
  let find_services name =
    List.map
      (fun (sockserv, sockctrl, _) -> (sockserv, sockctrl))
      (List.filter
	 (fun (s,_,_) -> s#name = name) 
	 controller # ext_services)
  in
  let protect f arg =
    try
      f arg;
      `code_ok
    with
      | error ->
	  `code_error (Netexn.to_string error)
  in
object(self)
  inherit Netplex_kit.empty_processor_hooks()

  method supported_ptypes = [ `Controller_attached ]

  method process ~when_done cont fd proto =
    dlogr
      (fun () -> sprintf "Service netplex.controller: creating server");
    let rpc =
      Rpc_server.create2 (`Socket_endpoint(Rpc.Tcp, fd)) cont#event_system in
    Rpc_server.set_exception_handler rpc
      (fun err ->
	 controller # logger # log
	   ~component:"netplex.controller"
	   ~level:`Crit
	   ~message:("Admin server caught exception: " ^ 
		       Netexn.to_string err));
    Netplex_ctrl_srv.Admin.V2.bind
      ~proc_ping:(fun () -> ())
      ~proc_list:(fun () ->
		    Array.map
		      (fun (sockserv,sockctrl,_) ->
			 { srv_name = sockserv#name;
			   srv_protocols =
			     Array.map
			       (fun (proto, fdlist) ->
				  { prot_name = proto;
				    prot_ports =
				      Array.map
					(fun fd ->
					   try
					     let name = Unix.getsockname fd in
					     let domain = 
					       Unix.domain_of_sockaddr name in
					     match name, domain with
					       | Unix.ADDR_UNIX path, _ ->
						   `pf_unix path
					       | Unix.ADDR_INET(addr,port),
						   Unix.PF_INET
						   ->
						   `pf_inet
						     { inet_addr =
							 Unix.string_of_inet_addr
							   addr;
						       inet_port = port
						     }
					       | Unix.ADDR_INET(addr,port),
						     Unix.PF_INET6
						     ->
						   `pf_inet6
						     { inet6_addr =
							 Unix.string_of_inet_addr
							   addr;
						       inet6_port = port
						     }
					       | _ -> `pf_unknown
					   with
					     | _ -> `pf_unknown
					)
					fdlist
				  }
			       )
			       (Array.of_list sockserv#sockets);
			   srv_nr_containers =
			     List.length (sockctrl # container_state);
			   srv_containers =
			     Array.of_list
			       (List.map
				  (fun (cid, par_info, cs, selected) ->
				     { cnt_id = Int64.of_int (Oo.id cid);
				       cnt_sys_id = par_info;
				       cnt_state =
					 if selected then
					   `cstate_selected
					 else
					   match cs with
					     | `Accepting _ ->
						 `cstate_accepting
					     | `Busy ->
						 `cstate_busy
					     | `Starting _ ->
						 `cstate_starting
					     | `Shutting_down ->
						 `cstate_shutdown
				     }
				     
				  )
				  sockctrl # container_state
			       );
			   srv_state = 
			     ( match sockctrl # state with
				 | `Enabled -> state_enabled
				 | `Disabled -> state_disabled
				 | `Restarting _ -> state_restarting
				 | `Down -> state_down
			     )
			 }
		      )
		      (Array.of_list controller#services)
		 )
      ~proc_enable:(protect 
		      (fun name -> 
			 List.iter
			   (fun (_, ctrl) -> ctrl # enable())
			   (find_services name))
		   )
      ~proc_disable:(protect
		       (fun name -> 
			 List.iter
			   (fun (_, ctrl) -> ctrl # disable())
			   (find_services name))
		    )
      ~proc_restart:(protect 
		       (fun name ->
			  List.iter
			    (fun (_, ctrl) -> ctrl # restart())
			    (find_services name))
		    )
      ~proc_restart_all:(protect (fun () ->
				    controller # restart()))
      ~proc_system_shutdown:(protect (fun () ->
				 controller # shutdown()))
      ~proc_reopen_logfiles:(protect (fun () ->
					controller # logger # reopen()))
      ~proc_send_admin_message:(fun (pat, msg) ->
				  controller # send_admin_message
				    pat msg.msg_name msg.msg_arguments
			       )
      rpc;
    Rpc_server.set_onclose_action rpc (fun _ -> 
					 when_done();
				      );
    setup rpc

  method global_exception_handler exn = true

end ;;


class controller_sockserv setup controller : socket_service =
  let processor = new controller_processor setup controller in
  let dir = controller#controller_config#socket_directory in
  let dir' = Filename.concat dir "netplex.controller" in
  let socket_name = Filename.concat dir' "admin" in
  let () = Netplex_util.try_mkdir dir in
  let () = Netplex_util.try_mkdir dir' in
  let addr =
    match Sys.os_type with
      | "Win32" ->
	  `W32_pipe_file socket_name
      | _ ->
	  `Socket(Unix.ADDR_UNIX socket_name) in
  let config : socket_service_config = 
    ( object
	method name = "netplex.controller"
	method protocols =
	  [ object
	      method name = "admin"
	      method addresses = [| addr |]
	      method lstn_backlog = 50
	      method lstn_reuseaddr = true
	      method so_keepalive = true
	      method tcp_nodelay = false
	      method configure_slave_socket _ = ()
	    end
	  ]
	method change_user_to = None
        method startup_timeout = (-1.0)
	method conn_limit = None
	method gc_when_idle = false
	method controller_config = controller#controller_config
      end
    ) in
  let sockserv' = 
    Netplex_sockserv.create_socket_service
      processor config in
object(self)
  method name = sockserv' # name
  method sockets = sockserv' # sockets
  method socket_service_config = sockserv' # socket_service_config
  method processor = processor
  method create_container p s =
    Netplex_container.create_admin_container controller#event_system p s
  method shutdown() =
    sockserv'#shutdown()
  method on_add ctrl = sockserv'#on_add ctrl
  method startup_directory = sockserv'#startup_directory
end


class std_controller_for_esys esys
       (par : parallelizer) (config : controller_config) 
       : extended_controller =
  let dl = new deferring_logger in
  let eps_group = Unixqueue.new_group esys in
  let startup_dir = Unix.getcwd() in
  let ctrl_sys_id = par # current_sys_id in
  let cs_directory = ref [] in
object(self)
  val mutable logger = (dl :> logger)
  val mutable services = []
  val mutable shutting_down = false
  val mutable admin_setups = []
  val mutable message_receivers = []
  val mutable plugins = []
  val mutable socksrv_list = []
  val mutable levers = Hashtbl.create 10
  val mutable next_lever_id = 0

  initializer (
    par # init();
    let l = config # create_logger (self : #controller :> controller) in
    logger <- l;
    dl # forward l;
      (* Forward messages sent to the logger during [create_logger]. *)
    let my_sockserv = 
      new controller_sockserv 
	(fun rpc ->
	   List.iter (fun f -> f rpc) admin_setups
	)
	(self : #extended_controller :> extended_controller) in
    let my_wrkmng =
      Netplex_workload.create_constant_workload_manager 1 in
    (* Cannot use [add_service] because we must use the special parallelizer *)
    let my_sockctrl = 
      new std_socket_controller 
	~no_disable:true
	self#rm_service
	(new admin_par)
	(self : #extended_controller :> extended_controller)
	my_sockserv 
	my_wrkmng 
	cs_directory 
	self#ctrl_detach in
    services <- (my_sockserv, my_sockctrl, my_wrkmng) :: services;
    socksrv_list <- my_sockserv :: socksrv_list;
    my_wrkmng # hello (self : #controller :> controller);
    my_sockctrl # enable();

  )

  method ptype = par # ptype

  method sys_id = ctrl_sys_id

  method controller_config = config

  method services = 
    ( services 
      :> (socket_service * socket_controller * workload_manager) list )

  method ext_services =
    services

  method add_service sockserv wrkmng =
    if not (List.mem par#ptype sockserv#processor#supported_ptypes) then
      failwith "#add_service: the parallelization type is not supported";
    if shutting_down then
      failwith "#add_service: controller is shutting down";
    let sockctrl = 
      new std_socket_controller 
	self#rm_service
	par
	(self : #extended_controller :> extended_controller)
	sockserv 
	wrkmng 
	cs_directory
	self#ctrl_detach in
    services <- (sockserv, sockctrl, wrkmng) :: services;
    sockserv # on_add (self : #controller :> controller);
    wrkmng # hello
      (self : #controller :> controller);
    sockserv # processor # post_add_hook
      sockserv (self : #controller :> controller);
    socksrv_list <- sockserv :: socksrv_list;
    sockctrl # enable();

  method add_admin setup =
    admin_setups <- setup :: admin_setups

  method add_message_receiver recv =
    message_receivers <- recv :: message_receivers

  method add_plugin plugin =
    if not (List.mem plugin plugins) then (
      plugins <- plugin :: plugins;
      plugin # ctrl_added (self :> controller)
    )

  method free_resources () =
    dlog "free_resources";
    admin_setups <- [];
    message_receivers <- [];
    List.iter
      (fun plugin -> plugin # ctrl_unplugged (self :> controller))
      plugins;
    List.iter (fun socksrv -> socksrv#shutdown()) socksrv_list;
    socksrv_list <- [];
    List.iter
      (fun (socksrv,sockctrl,wrkmng) ->
	 if sockctrl#state <> `Down then
	   Netlog.logf `Err
	     "Socket controller not shut down when it should be: %s"
	     socksrv#name;
      )
      services
    

  method private rm_service sockctrl =
    let sockserv = ref None in
    services <- 
      (List.filter 
         (fun (s, c, _) -> 
	    if c = sockctrl then (
	      sockserv := Some s;
	      false
	    )
	    else
	      true
	 ) 
         services);
    match !sockserv with
      | None -> ()   (* strange *)
      | Some s -> 
	  s # processor # post_rm_hook s (self : #controller :> controller);
	  (* We don't immediately shut down s, so it can be re-added again *)

  method logger = logger

  method event_system = esys

  method restart() =
    if shutting_down then
      failwith "#restart: controller is shutting down";
    dlog "Beginning full restart sequence";
    List.iter
      (fun (_, ctrl, _) ->
	 ctrl # restart()
      )
      services;
    dlog "Restart Initiated"


  method shutdown() =
    dlog "Beginning full shutdown sequence";
    shutting_down <- true;
    let real_shutdown() =
      Unixqueue.once esys eps_group 0.0
	(fun () ->
	   dlog "Shutting services down";
	   List.iter
	     (fun (_, ctrl, wrkmng) ->
		ctrl # shutdown();
		wrkmng # shutdown();
	     )
	     services
	)
    in
    dlog "Sending system_shutdown notification to services";
    let n = ref 0 in
    List.iter
      (fun (sserv, ctrl, _) ->
	 ctrl # forward_system_shutdown
	   (fun () ->
	      dlog 
		(sprintf "Received ACK for system_shutdown from %s" sserv#name);
	      decr n;
	      if !n = 0 then   (* all notifications arrived *)
		real_shutdown()
	   );
	 incr n
      )
      services;
    if !n = 0 then
      real_shutdown()
    

  method private matching_services re =
    List.filter
      (fun (sockserv, ctrl, _) ->
	 match Netstring_str.string_match re sockserv#name 0 with
	   | Some _ -> true
	   | None -> false
      )
      services

  method private matching_receivers re =
    List.filter
      (fun recv ->
	 match Netstring_str.string_match re recv#name 0 with
	   | Some _ -> true
	   | None -> false
      )
      message_receivers


  method send_message pat msg_name msg_args =
    let msg = { msg_name = msg_name; msg_arguments = msg_args } in
    let re = regexp_of_pattern pat in
    List.iter
      (fun (sockserv, ctrl, _) -> ctrl # forward_message msg)
      (self # matching_services re);
    List.iter
      (fun recv ->
	 try
	   recv # receive_message
	     (self :> controller)
	     msg_name
	     msg_args
	 with error ->
	   Unixqueue.once esys eps_group 0.0
	     (fun () -> raise error)
      )
      (self # matching_receivers re);


  method send_admin_message pat msg_name msg_args =
    let msg = { msg_name = msg_name; msg_arguments = msg_args } in
    let re = regexp_of_pattern pat in
    List.iter
      (fun (sockserv, ctrl, _) -> ctrl # forward_admin_message msg)
      (self # matching_services re);
    List.iter
      (fun recv ->
	 try
	   recv # receive_admin_message
	     (self :> controller)
	     msg_name
	     msg_args
	 with error ->
	   Unixqueue.once esys eps_group 0.0
	     (fun () -> raise error)
      )
      (self # matching_receivers re);


  method plugin_receive_call plugin_id cid name arg_str reply reply_err =
    let plugin =
      try Some(List.find (fun p -> Oo.id p = plugin_id) plugins) 
      with Not_found -> None in
    match plugin with
      | Some p ->
	  ( try
	      let (_, arg_ty,res_ty) = 
		Rpc_program.signature p#program name in
	      let arg =
		Xdr.unpack_xdr_value ~fast:true arg_str arg_ty [] in
	      p # ctrl_receive_call (self :> controller) cid name arg 
		(function
		   | None ->
		       reply_err()
		   | Some res ->
		       ( try
			   let res_str =
			     Xdr.pack_xdr_value_as_string res res_ty [] in
			   reply res_str
			 with 
			   | error ->
			       logger # log ~component:"netplex.controller"
				 ~level:`Err
				 ~message:("Exception packing plugin response: " ^ 
					     Netexn.to_string error);
			       reply_err()
		       )
		)
	    with
	      | error ->
		  logger # log ~component:"netplex.controller"
		    ~level:`Err
		    ~message:("Exception in plugin call: " ^ 
				Netexn.to_string error);
		  reply_err()
	  )
      | None ->
	  logger # log ~component:"netplex.controller"
	    ~level:`Err
	    ~message:"Received call for unknown plugin";
	  reply_err()

  method plugin_container_finished cid is_last =
    List.iter
      (fun p ->
	 try
	   p # ctrl_container_finished (self :> controller) cid is_last
	 with
	   | error ->
	       logger # log ~component:"netplex.controller"
		 ~level:`Err
		 ~message:("Exception in ctrl_container_finished: " ^ 
			     Netexn.to_string error);
      )
      plugins

  method containers =
    List.flatten
      (List.map
	 (fun (_,sockctrl,_) ->
	    List.map
	      (fun (cid,_,_,_) -> cid)
	      sockctrl#container_state
	 )
	 services
      )

  method containers_for name =
    try
      let _, sockctrl, _ =
	List.find 
	  (fun (sockserv,_,_) -> sockserv#name = name)
	  services in
      List.map
	(fun (cid,_,_,_) -> cid)
	sockctrl#container_state
    with Not_found -> []
      
  method container_count name =
    List.length (self # containers_for name)

  method startup_directory = startup_dir	    

  method register_lever f =
    let id = next_lever_id in
    next_lever_id <- id + 1;
    Hashtbl.replace levers id f;
    id

  method activate_lever id arg =
    let f = 
      try
	Hashtbl.find levers id
      with
	| Not_found ->
	    failwith ("Lever not found: " ^ string_of_int id) in
    f (self :> controller) arg

  method private ctrl_detach() =
    List.iter
      (fun (_,sockctrl,_) ->
	 sockctrl#detach()
      )
      services

end


let create_controller_for_esys esys par config =
  (new std_controller_for_esys esys par config :> controller)


let create_controller par config =
  let esys = Unixqueue.standard_event_system() in
  create_controller_for_esys esys par config


let default_socket_directory = "/tmp/.netplex"

let default_create_logger _ = Netplex_log.channel_logger stderr

let extract_logger ctrl loggers cf logaddr =
  let typ =
    try 
      cf # string_param
	(cf # resolve_parameter logaddr "type") 
    with
      | Not_found ->
	  failwith "Parameter 'type' in 'logging' section is missing" in
  let logger =
    try
      List.find (fun l -> l#name = typ) loggers 
    with
      | Not_found ->
	  failwith ("Logging type not found: " ^ typ) in
  logger # create_logger cf logaddr ctrl


let level_weight = Netplex_log.level_weight

let compound_logger (llist:logger list) cur_max_level : logger =
  ( object
      method log_subch ~component ~subchannel ~level ~message =
	if level_weight level <= level_weight !cur_max_level then 
	  List.iter
	    (fun l -> l # log_subch ~component ~subchannel ~level ~message)
	    llist
      method log ~component ~level ~message =
	if level_weight level <= level_weight !cur_max_level then 
	  List.iter
	    (fun l -> l # log ~component ~level ~message)
	    llist
      method reopen() =
	List.iter (fun l -> l#reopen()) llist
    end
  )


let extract_config (loggers : logger_factory list) (cf : config_file) =
  match cf # resolve_section cf#root_addr "controller" with
    | [] ->
	(* Create a default configuration: *)
	let cur_max_level = ref `Info in
	( object
	    method socket_directory = 
	      default_socket_directory
	    method create_logger ctrl = 
	      compound_logger 
		[ default_create_logger ctrl ] 
		cur_max_level
	    method max_level = !cur_max_level
	    method set_max_level lev = cur_max_level := lev
	  end
	)
    | [ ctrladdr ] ->
	cf # restrict_subsections ctrladdr [ "logging" ];
	cf # restrict_parameters ctrladdr [ "socket_directory"; "max_level" ];
	let socket_directory0 =
	  try 
	    cf # string_param 
	      (cf # resolve_parameter ctrladdr "socket_directory")
	  with
	    | Not_found -> default_socket_directory in
	let socket_directory =
	  if Netsys.is_absolute socket_directory0 then
	    socket_directory0
	  else
	    Filename.concat (Unix.getcwd()) socket_directory0 in
	let create_logger cur_max_level ctrl =
	  ( match cf # resolve_section ctrladdr "logging" with
	      | [] ->
		  default_create_logger ctrl
	      | alist ->
		  let llist =
		    List.map (extract_logger ctrl loggers cf) alist in
		  let logger = 
		    compound_logger llist cur_max_level in
		  logger
	  ) in
	let max_level =
	  try
	    let s = 
	      cf # string_param
		(cf # resolve_parameter ctrladdr "max_level") in
	    if String.lowercase s = "all" then
	      `Debug
	    else
	      Netplex_log.level_of_string s
	  with
	    | Not_found -> `Info   (* default log level *)
	    | _ -> 
		failwith ("In section " ^ cf # print ctrladdr ^ 
			    ": Bad max_level parameter value")
	in
	let cur_max_level = ref max_level in
	( object
	    method socket_directory = socket_directory
	    method create_logger = create_logger cur_max_level
	    method max_level = !cur_max_level
	    method set_max_level lev = cur_max_level := lev
	  end
	)
    | _ ->
	failwith "More than one 'controller' section"
;;
