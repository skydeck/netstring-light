(* This netplex can be contacted over fcgi/scgi/ajp.
 * It is configured in the netplex.cfg file. 
 * Note: start program with option "-conf netplex.cfg" 
 *)

let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  let use_mt = ref false in

  let opt_list' =
    [ "-mt", Arg.Set use_mt,
      "  Use multi-threading instead of multi-processing"
    ] @ opt_list in

  Arg.parse 
    opt_list'
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";
  let parallelizer = 
    if !use_mt then
      Netplex_mt.mt()     (* multi-threading *)
    else
      Netplex_mp.mp() in  (* multi-processing *)
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  let output_type = `Transactional buffered in
  let factory = 
    Netcgi_plex.factory
      ~output_type
      (fun container cgi -> Add.generate_page cgi) in
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ factory ]           (* make this service available *)
    cmdline_cfg
;;

Netsys_signal.init();
start();;
