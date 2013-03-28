(* $Id: netplex_main.ml 1807 2012-11-02 16:22:34Z gerd $ *)

open Netplex_types
open Printf

type cmdline_config =
    { mutable config_filename_opt : string option;
      mutable config_tree_opt : config_tree option;
      mutable pidfile : string option;
      mutable foreground : bool;
    }

let is_win32 =
  match Sys.os_type with
    | "Win32" -> true
    | _ -> false;;

let create ?config_filename
           ?config_tree
           ?(pidfile = None)
           ?(foreground = false) () =
  { config_filename_opt = config_filename;
    config_tree_opt = config_tree;
    pidfile = pidfile;
    foreground = foreground
  }

let modify ?config_filename
           ?config_tree
           ?pidfile
           ?foreground cfg =
  { config_filename_opt = ( match config_filename with
			      | Some f -> Some f
			      | None -> cfg.config_filename_opt
			  );
    config_tree_opt = ( match config_tree with
			  | Some f -> Some f
			  | None -> cfg.config_tree_opt
		      );
    pidfile = ( match pidfile with
		  | Some popt -> popt
		  | None -> cfg.pidfile
	      );
    foreground = ( match foreground with
		     | Some fg -> fg
		     | None -> cfg.foreground
		 )
  }


let args ?(defaults = create()) () =
  let config =
    (* copy of defaults: *)
    modify defaults in

  let spec =
    [ "-conf",
      (Arg.String (fun s -> config.config_filename_opt <- Some s)),
      "<file>  Read this configuration file";
      
      "-pid",
      (Arg.String (fun s -> config.pidfile <- Some s)),
      "<file>  Write this PID file";
      
      "-fg",
      (Arg.Unit (fun () -> config.foreground <- true)),
      "  Start in the foreground and do not run as daemon";
    ] in
  (spec, config)
;;


let config_filename cf = 
  match cf.config_filename_opt with
    | Some f -> f
    | None ->
	let command_name = Sys.argv.(0) in
	( try
	    (Filename.chop_extension command_name) ^ ".conf"
	  with
	    | _ -> command_name ^ ".conf"
	)

let config_filename_opt cf = cf.config_filename_opt

let config_tree_opt cf = cf.config_tree_opt

let pidfile cf = cf.pidfile

let foreground cf = cf.foreground

let daemon f =
  (* Double fork to avoid becoming a pg leader. The outer process waits
     until the most important initializations of the child are done
     (e.g. master sockets are created).
   *)
  if is_win32 then
    failwith "Startup as daemon is unsupported on Win32 - use -fg switch";
  let oldmask = Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigusr1; Sys.sigchld] in
  let pid = Unix.getpid() in
  match Unix.fork() with
    | 0 ->
        ( match Unix.fork() with
            | 0 ->
                ignore(Unix.sigprocmask Unix.SIG_SETMASK oldmask);
                let _ = Unix.setsid() in (* Start new session/get rid of tty *)
                (* Assign stdin/stdout to /dev/null *)
                Unix.close Unix.stdin;
                ignore(Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0);
                Unix.close Unix.stdout;
                ignore(Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0);
                (* Keep stderr open: error messages should appear *)
		Netsys_posix.run_post_fork_handlers();
                f ~init_done:(fun () -> Unix.kill pid Sys.sigusr1)
            | _ ->
                Netsys._exit 0
        )
    | middle_pid ->
        (* Wait for zombie: *)
        ignore(Netsys.restart (Unix.waitpid []) middle_pid);
        (* Wait for SIGUSR1, but ignore SIGCHLD *)
        Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> ()));
        Unix.sigsuspend [ Sys.sigchld ];
        ignore(Unix.sigprocmask Unix.SIG_SETMASK oldmask);
;;


let rec run ctrl =
  try
    Unixqueue.run ctrl#event_system
  with
    | error ->
	ctrl # logger # log
	  ~component:"netplex.controller"
	  ~level:`Crit
	  ~message:("Uncaught exception: " ^ Netexn.to_string error);
	run ctrl
;;


let startup ?(late_initializer = fun _ _ -> ())
            ?(config_parser = Netplex_config.read_config_file)
            par c_logger_cf c_wrkmg_cf c_proc_cf cf =
  let config_file = 
    match cf.config_tree_opt with
      | None ->
	  config_parser (config_filename cf)
      | Some tree ->
	  Netplex_config.repr_config_file (config_filename cf) tree in
  
  let netplex_config =
    Netplex_config.read_netplex_config
      par#ptype
      c_logger_cf c_wrkmg_cf c_proc_cf 
      config_file in

  let maybe_daemonize =
    (if cf.foreground then
       (fun f -> f ~init_done:(fun () -> ()))
     else
       daemon) in
  maybe_daemonize
    (fun ~init_done ->
       let remove_pid_file =
	 match cf.pidfile with
	   | Some file ->
               let f = open_out file in
               fprintf f "%d\n" (Unix.getpid());
               close_out f;
               (fun () ->
		  try Sys.remove file with _ -> ())
	   | None ->
               (fun () -> ())
       in
       try
	 let controller_config = netplex_config # controller_config in
	 
	 let controller = 
	   Netplex_controller.create_controller 
	     par controller_config in

	 Netplex_cenv.register_ctrl controller;

	 (* Change to / so we don't block filesystems without need.
            Do this after controller creation so the controller has a
            chance to remember the cwd
	  *)
	 Unix.chdir "/";  (* FIXME Win32: Something like c:/ *)

	 let old_logger = !Netlog.current_logger in
	 let old_dlogger = !Netlog.Debug.current_dlogger in

	 Netlog.current_logger := 
	   (fun level message ->
	      try
		Netplex_cenv.log level message
		  (* This function also works from the controller thread! *)
	      with
		| Netplex_cenv.Not_in_container_thread ->
		    (* Fall back to something safe: *)
		    old_logger level message
	   );
	 (* hmmm, Netlog.Debug cannot be handled by netplex *)
	 Netlog.Debug.current_dlogger := 
	   (fun mname msg ->
	      Netlog.channel_logger stderr `Debug `Debug (mname ^ ": " ^ msg)
	   );

	 let processors =
	   List.map
	     (fun (sockserv_cfg, 
		   (procaddr, c_proc_cfg), 
		   (wrkmngaddr, c_wrkmng_cfg)
		  ) ->
		c_proc_cfg # create_processor
		  controller_config config_file procaddr)
	     netplex_config#services in
	 (* An exception while creating the processors will prevent the
          * startup of the whole system!
	  *)

	 let services =
	   List.map2
	     (fun (sockserv_cfg, 
		   (procaddr, c_proc_cfg), 
		   (wrkmngaddr, c_wrkmng_cfg)
		  ) 
  		  processor ->
		try
		  let wrkmng =
		    c_wrkmng_cfg # create_workload_manager
		      controller_config config_file wrkmngaddr in
		  let sockserv = 
		    Netplex_sockserv.create_socket_service 
		      processor sockserv_cfg in
		  Some (sockserv, wrkmng)
		with
		  | error ->
		      (* An error while creating the sockets is quite
                       * problematic. We do not add the service, but we cannot
                       * prevent the system startup at that late point in time
                       *)
		      controller # logger # log
			~component:"netplex.controller"
			~level:`Crit
			~message:("Uncaught exception preparing service " ^ 
				    sockserv_cfg#name ^ ": " ^ 
				    Netexn.to_string error);
		      None
	     )
	     netplex_config#services
	     processors in

	 List.iter
	   (function
	      | Some(sockserv,wrkmng) ->
		  ( try
		      controller # add_service sockserv wrkmng
		    with
		      | error ->
			  (* An error is very problematic now... *)
			  controller # logger # log
			    ~component:"netplex.controller"
			    ~level:`Crit
			    ~message:("Uncaught exception adding service " ^ 
					sockserv#name ^ ": " ^ 
					Netexn.to_string error);
		  )
	      | None ->
		  ()
	   )
	   services;

	 ( try
	     late_initializer config_file controller
	   with
	     | error ->
		 (* An error is ... *)
		 controller # logger # log
		   ~component:"netplex.controller"
		   ~level:`Crit
		   ~message:("Uncaught exception in late initialization: " ^ 
			       Netexn.to_string error);
	 );

	 init_done();

	 run controller;
	 Netplex_cenv.unregister_ctrl controller;
	 controller # free_resources();

	 Netlog.current_logger := old_logger;
	 Netlog.Debug.current_dlogger := old_dlogger

       with
	 | error ->
             remove_pid_file();
             raise error
    )
;;
