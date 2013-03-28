(* $Id: helper_container.ml 1564 2011-03-08 15:34:23Z gerd $ *)

(* This example demonstrates how to start a helper container (i.e. an 
   additional thread or process). The helper container does not create
   a socket, so we must use internal IPC methods.

   This module is the server program. Use test_client as client.

   The [operation] just returns the passed string in this example (identity).
   As side effect, the number of invocations are counted. The task of the
   helper container is to print this number once each minute to stdout.

   Here, the helper container is socket-less. This is for the purpose
   of this example - in a more real-world scenario it is not uncommon
   that helpers also have sockets, and define RPCs one can call. Note 
   that internal sockets (socketpairs) are always created, but these
   cannot be accessed from outside.

   As every container needs a service definition, the question is how to
   provide it. Here, we simply add it to the controller at program startup,
   i.e. it does not appear in the configuration file. Note that the name
   of the service is nevertheless a global name, and one should be careful
   not to assign the same name twice.

   So we have two services:
   - [worker]: Defined in the configuration file. The containers of this
     service execute [operation], and count the number of invocations.
   - [helper]: Defined programmatically. There is only one container,
     and this container prints the number once every minute.

   For counting, we simply use counters provided by
   [Netplex_semaphore].

   This example is very interesting for multi-threaded programs, because
   it shows how to create threads inside the Netplex environment. There
   is a lot of formalism involved, but the advantage is that the threads
   can use Netplex functions:
   - They can print log messages using [Netplex_cenv.log]
   - They can use Netplex plugins like [Netplex_semaphore]
   - They can be managed by netplex_admin (shutdown, restart, list of
     active containers)
 *)


open Printf

(* --- worker --- *)

let proc_operation s =
  ignore(Netplex_semaphore.increment "worker.operation_count");
  s


let setup srv _ =
  Operation_srv.P.V.bind
    ~proc_null:(fun () -> ())
    ~proc_operation
    srv


(* --- helper --- *)

let set_up_timer() =
  ignore
    (Netplex_cenv.create_timer
       (fun _ ->
	  let v = Netplex_semaphore.get "worker.operation_count" in
	  Netlog.logf `Info "operation count: %Ld" v;
	  true
       )
       60.0)



(* --- main --- *)

let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  let use_mt = ref false in
  let debug = ref false in

  let opt_list' =
    [ "-mt", Arg.Set use_mt,
      "  Use multi-threading instead of multi-processing";
      
      "-debug", Arg.Set debug,
      "  Enable (some) debug messages";
   ] @ opt_list in

  Arg.parse
    opt_list'
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    (sprintf "usage: %s [options]" (Filename.basename Sys.argv.(0)));

  if !debug then (
    ()
  );

  let parallelizer =
    if !use_mt then
      Netplex_mt.mt()     (* multi-threading *)
    else
      Netplex_mp.mp() in  (* multi-processing *)
  let worker_hooks =
    ( object(self)
        inherit Netplex_kit.empty_processor_hooks() 
        method post_add_hook _ ctrl = 
          ctrl # add_plugin Netplex_semaphore.plugin
        method post_start_hook _ =
          let _cr_flag = 
            Netplex_semaphore.create
	      ~protected:false
	      "worker.operation_count" 
	      0L in
          ()
      end
    ) in
  let worker_factory =
    Rpc_netplex.rpc_factory
      ~configure:(fun _ _ -> ())
      ~name:"worker"
      ~setup
      ~hooks:(fun _ -> worker_hooks)
      () in
  let helper_hooks =
    ( object
	inherit Netplex_kit.empty_processor_hooks () 
        method post_add_hook _ ctrl = 
          ctrl # add_plugin Netplex_semaphore.plugin
	method post_start_hook _ =
	  set_up_timer()
      end
    ) in
  Netplex_main.startup
    ~late_initializer:(
      fun _ ctrl -> 
	(* The helper isn't added via a factory, but directly to the
           controller using this special function:
	 *)
	Netplex_kit.add_helper_service ctrl "helper" helper_hooks
    )
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ worker_factory ]           (* make this service type available *)
    cmdline_cfg


let () =
  Netsys_signal.init();
  start()
