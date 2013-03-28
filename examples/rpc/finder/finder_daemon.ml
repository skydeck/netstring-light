(* $Id: finder_daemon.ml 1277 2009-10-13 00:03:45Z gerd $ *)

(* This is a RPC server built from the Netplex and RPC components.
 * It is configured in the netplex.cfg file.
 * Note: start program with option "-conf netplex.cfg"
 *
 * This program iterates through the directories every time a search query
 * is solved. One can imagine to improve that, but this is only a simple
 * example without too much optimization.
 *)

(**********************************************************************)
(* The "find" procedure                                               *)
(**********************************************************************)

let proc_find root_dir searched_name =
  
  let rec iterate_dir dir =
    let f = Unix.opendir dir in
    try
      let r = iterate_next dir f in
      Unix.closedir f;
      r
    with
      | End_of_file ->
	  Unix.closedir f;
	  None
      | Unix.Unix_error((Unix.EACCES | Unix.EPERM), _, _) ->
	  (* ignore these *)
	  Unix.closedir f;
	  None
      | error ->
	  Unix.closedir f;
	  raise error

  and iterate_next dir f =
    let file = Unix.readdir f in
    if file <> "." && file <> ".." then (
      let fullname = Filename.concat dir file in
      if file = searched_name then
	Some fullname
      else (
	try
	  let s = Unix.lstat fullname in
	  if s.Unix.st_kind = Unix.S_DIR then
	    let r = iterate_dir fullname in
	    match r with
	      | None -> iterate_next dir f
	      | Some _ -> r
	  else
	    iterate_next dir f
	with
	  | Unix.Unix_error(_,_,_) ->  (* lstat error *)
	      (* ignore *)
	      iterate_next dir f
      )
    )
    else
      iterate_next dir f

  in

  Netplex_sharedvar.set_value "finder.lastquery" searched_name;
  (* Make the last query globally accessible. This is an example how to use
     a plugin only - set_value is not very efficient.
   *)

  match iterate_dir root_dir with
    | None -> `not_found
    | Some fullname -> `found fullname
;;


let proc_lastquery () =
  match Netplex_sharedvar.get_value "finder.lastquery" with
    | None ->
	""
    | Some v ->
	v
;;

(**********************************************************************)
(* Create the RPC server                                              *)
(**********************************************************************)

let configure cf addr =
  let root_dir =
    try
      cf # string_param (cf # resolve_parameter addr "root_dir")
    with
      | Not_found ->
	  failwith "Required parameter root_dir is missing!" in
  root_dir
;;


let setup srv root_dir =
  (* Bind the RPC's: *)
  Finder_service_srv.Finder.V1.bind 
    ~proc_ping:(fun () -> ())
    ~proc_find:(proc_find root_dir)
    ~proc_lastquery:(proc_lastquery)
    ~proc_shutdown:Netplex_cenv.system_shutdown
    srv;
  (* Limit client queries to 1000 bytes: *)
  Rpc_server.set_session_filter 
    srv
    (fun _ -> `Accept_limit_length(1000, `Reject))
;;


let win32_debug_gc = ref false
let debug_fd = ref false


let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  let use_mt = ref false in

  let opt_list' =
    [ "-mt", Arg.Set use_mt,
      "  Use multi-threading instead of multi-processing";
      
      "-debug", Arg.String (fun s -> Netlog.Debug.enable_module s),
      "<module>  Enable debug messages for <module>";

      "-debug-all", Arg.Unit (fun () -> Netlog.Debug.enable_all()),
      "  Enable all debug messages";

      "-debug-list", Arg.Unit (fun () -> 
				 List.iter print_endline (Netlog.Debug.names());
				 exit 0),
      "  Show possible modules for -debug, then exit";

      "-debug-win32", Arg.Unit (fun () -> 
				  win32_debug_gc := true;
				  Netsys_win32.Debug.debug_c_wrapper true),
      "  Special debug log of Win32 wrapper";

      "-debug-fd", Arg.Unit(fun () -> 
			      Netlog.Debug.enable_fd_tracking := true;
			      debug_fd := true;      
			   ),
      "  Special debugging of file descriptors";
   ] @ opt_list in

  Arg.parse
    opt_list'
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";

  if Sys.os_type = "Win32" && not !use_mt then
    failwith "For Win32 multi-processing is unsupported. Use -mt switch";

  let parallelizer =
    if !use_mt then
      Netplex_mt.mt()     (* multi-threading *)
    else
      Netplex_mp.mp() in  (* multi-processing *)
  let finder_factory =
    Rpc_netplex.rpc_factory
      ~configure
      ~name:"finder"
      ~setup
      ~hooks:(fun _ ->
	      object(self)
		inherit Netplex_kit.empty_processor_hooks() 
		method post_add_hook _ ctrl = 
		  ctrl # add_plugin Netplex_sharedvar.plugin
		method post_start_hook _ =
		  let _cr_flag = 
		    Netplex_sharedvar.create_var "finder.lastquery" in
		  ()
	      end
	     )
      () in
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ finder_factory ]           (* make this service type available *)
    cmdline_cfg
;;

Netsys_signal.init();
start();;

if !debug_fd then
  List.iter prerr_endline (Netlog.Debug.fd_table());;

let () =  (* debugging: check that all resources are freed *)
  if !win32_debug_gc then (
    for k = 1 to 10 do
      Gc.full_major();
      Netsys_win32.gc_proxy()
    done
  )
;;

