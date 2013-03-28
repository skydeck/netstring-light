(* $Id: lever.ml 1415 2010-02-15 23:58:25Z gerd $ *)

open Printf

(* --- things we need for creating the lever --- *)

module T = struct
  type s = string    (* argument type. Here, the message string *)
  type r = bool      (* result type. Here, whether the lever was successful *)
end

module L = Netplex_cenv.Make_lever(T)

module LV = Netplex_cenv.Make_var_type(L)

(* --- helper --- *)

let helper_hooks s =
  ( object
      inherit Netplex_kit.empty_processor_hooks () 
      method post_start_hook cont =
	(* Just print the message 10 times *)
	for n = 1 to 10 do
	  Netlog.logf `Info "Helper1: %s" s;
	  Unix.sleep 1
	done;
	(* There is no real way for disabling a helper again. [shutdown]
           will shut the process down (and it is not restarting). The
           service definition will remain active, however.
	 *)
	cont#shutdown();
	
    end
  )

(* --- worker --- *)

let proc_operation s =
  (* Start the helper thread using a lever *)
  let lever = LV.get "sample_lever" in
  let success = lever s in
  let r =
    if success then
      "Could activate lever"
    else
      "Error activating lever" in
  Netlog.log `Info r;
  r


let setup_worker srv _ =
  Operation_srv.P.V.bind
    ~proc_null:(fun () -> ())
    ~proc_operation
    srv


let worker_hooks() =
  ( object(self)
      inherit Netplex_kit.empty_processor_hooks() 

      val mutable helper1_lever = (fun _ -> assert false)

      method post_add_hook socksrv ctrl =
	(* This is run in controller context, right after program startup.
           Register now the lever function, which starts a helper service.
	 *)
        let lever = 
          L.register ctrl
	    (fun ctrl s ->
	       try
                 Netplex_kit.add_helper_service
		   ctrl "helper1" (helper_hooks s);
                 true   (* successful *)
               with error ->
		 Netlog.logf `Err "Cannot start helper service: %s"
		   (Netexn.to_string error);
                 false  (* not successful *)
	    ) in
	(* Remember the created lever until the child forks *)
	helper1_lever <- lever

      method post_start_hook cont =
	(* Make the lever generally available in the child *)
        LV.set "sample_lever" helper1_lever
    end
  )

let worker_factory() =
  Rpc_netplex.rpc_factory
    ~configure:(fun _ _ -> ())
    ~name:"worker"
    ~setup:setup_worker
    ~hooks:(fun _ -> worker_hooks())
    ()


(* --- main --- *)

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
   ] @ opt_list in

  Arg.parse
    opt_list'
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    (sprintf "usage: %s [options]" (Filename.basename Sys.argv.(0)));

  let parallelizer =
    if !use_mt then
      Netplex_mt.mt()     (* multi-threading *)
    else
      Netplex_mp.mp() in  (* multi-processing *)
  
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ worker_factory() ]
    cmdline_cfg


let () =
  Netsys_signal.init();
  start()
