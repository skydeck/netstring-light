(* The main program for the server netplex *)

let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  let opt_list =
    [ "-debug", Arg.String (fun s -> Netlog.Debug.enable_module s),
      "<module>  Enable debug messages for <module>";

      "-debug-all", Arg.Unit (fun () -> Netlog.Debug.enable_all()),
      "  Enable all debug messages";

      "-debug-list", Arg.Unit (fun () -> 
                                 List.iter print_endline (Netlog.Debug.names());
                                 exit 0),
      "  Show possible modules for -debug, then exit";
    ] @ opt_list in

  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";

  let parallelizer = Netplex_mp.mp() in (* multi-processing *)
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ Sort1_worker.worker_factory();
      Sort1_controller.controller_factory()
    ]
    cmdline_cfg


let () =
  Netsys_signal.init();
  start()
