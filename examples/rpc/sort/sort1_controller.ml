(* Implementation of the controller process *)

open Sort1_proto_aux

type sort_merge_tree =
  | Sort of int * int * int
      (* partition ID, index start, length *)
  | Merge of int * sort_merge_tree * sort_merge_tree
      (* resulting partition ID, left, right *)


let construct_algorithm n max_depth worker_endpoints ctrl_endpoint =
  let next_part_id = ref 0 in

  let create_part_id() =
    let id = !next_part_id in
    incr next_part_id;
    id
  in

  let worker_index = ref 0 in
  
  let next_worker() =
    let i = !worker_index in
    incr worker_index;
    if !worker_index = Array.length worker_endpoints then worker_index := 0;
    worker_endpoints.(i)
  in

  let rec create_tree k_start k_length depth =
    if k_length <= 1 || depth >= max_depth then (
      let id = create_part_id() in
      Sort(id, k_start, k_length)
    )
    else (
      let id = create_part_id() in
      let k_length' = k_length / 2 in
      let left = 
	create_tree k_start k_length' (depth+1) in
      let right = 
	create_tree (k_start+k_length') (k_length-k_length') (depth+1) in
      Merge(id, left, right)
    )
  in

  let rec create_sort_jobs node cont =
    match node with
      | Sort(id, k_start, k_length) ->
	  let worker = next_worker() in
	  let jobs = [ id, worker, k_start, k_length, cont ] in
	  (id, worker, jobs)

      | Merge(id, left, right) ->
	  let (l_id, l_worker, l_jobs) =
	    create_sort_jobs left `keep in
	  let fwd =
	    { destination = l_worker;
	      merge_with_partition_id = l_id;
	      new_partition_id = id;
	      continuation = cont
	    } in
	  let (r_id, r_worker, r_jobs) =
	    create_sort_jobs right (`forward fwd) in
	  (id, l_worker, l_jobs @ r_jobs)
  in

  let tree = create_tree 0 n 1 in 
  let (_, _, jobs) = create_sort_jobs tree (`return ctrl_endpoint) in
  jobs

  
let rec get_worker_endpoints n_workers =
  (* Loop until at least n_workers are available *)
  let endpoints = 
    Netplex_cenv.lookup_container_sockets "sort_worker" "Worker" in
  if Array.length endpoints < n_workers then (
    Netplex_cenv.logf `Warning
      "Not enough workers found in registry (only %d)- will retry in 1 second"
      (Array.length endpoints);
    Unix.sleep 1;
    get_worker_endpoints n_workers
  )
  else (
    Netplex_cenv.logf `Info
      "Found %d endpoints, using the first %d" 
      (Array.length endpoints) n_workers;
    Array.sub endpoints 0 n_workers
  )


let worker_clients = Hashtbl.create 10
  (* Maps endpoint names to RPC clients *)


let get_worker_client endpoint =
  try
    Hashtbl.find worker_clients endpoint
  with
    | Not_found ->
	let esys = (Netplex_cenv.self_cont())#event_system in
	let connector =
	  Netplex_sockserv.any_file_client_connector endpoint in
	let client =
	  Sort1_proto_clnt.Worker.V1.create_client2
	    ~esys
	    (`Socket(Rpc.Tcp, connector, Rpc_client.default_socket_config)) in
	Hashtbl.replace worker_clients endpoint client;
	client

let running_sort = ref None


let check_for_errors name get_reply =
  try
    let () = get_reply() in
    ()
  with
    | error ->
	Netplex_cenv.logf `Err
	  "Got exception when calling %s: %s"
	  name
	  (Netexn.to_string error)


let proc_sort (n_workers, max_depth) session data emit =
  if !running_sort <> None then
    failwith "Sort already running";  (* can only do one sort at a time *)
  let worker_endpoints =
    get_worker_endpoints n_workers in
  let ctrl_endpoints =
    Netplex_cenv.lookup_container_sockets "sort" "Controller" in
  assert(Array.length ctrl_endpoints = 1);
  let ctrl_endpoint = ctrl_endpoints.(0) in
  let jobs = 
    construct_algorithm 
      (Array.length data) max_depth worker_endpoints ctrl_endpoint in
  Netplex_cenv.logf `Info
    "Constructed algorithm with %d sort jobs" (List.length jobs);
  running_sort := Some emit;
  List.iter
    (fun (id, worker, k_start, k_length, cont) ->
       let client = get_worker_client worker in
       Sort1_proto_clnt.Worker.V1.sort_partition'async
	 client
	 (id, Array.sub data k_start k_length, cont)
	 (check_for_errors "sort_partition")
    )
    jobs


let proc_return_result session data emit =
  match !running_sort with
    | None ->
	failwith "No active sort"
    | Some sort_emit ->
	running_sort := None;
	emit();
	sort_emit data


let configure cf addr =
  let n_workers =
    try cf # int_param(cf # resolve_parameter addr "n_workers") 
    with Not_found -> 1 in
  let max_depth =
    try cf # int_param(cf # resolve_parameter addr "max_depth")
    with Not_found -> 1 in
  Netplex_cenv.logf `Info
    "Using param n_workers=%d" n_workers;
  Netplex_cenv.logf `Info
    "Using param max_depth=%d" max_depth;
  (n_workers, max_depth)

let setup srv (n_workers, max_depth) =
  Sort1_proto_srv.Controller.V1.bind_async
    ~proc_null:(fun _ _ emit -> emit())
    ~proc_return_result
    srv;
  Sort1_proto_srv.Interface.V1.bind_async
    ~proc_null:(fun _ _ emit -> emit())
    ~proc_sort:(proc_sort (n_workers, max_depth))
    srv

let controller_factory() =
  Rpc_netplex.rpc_factory
    ~name:"sort"
    ~configure
    ~setup
    ~hooks:(fun _ ->
	    object(self)
              inherit Netplex_kit.empty_processor_hooks() 
	      method post_start_hook _ =
		let _t =
		  Netplex_cenv.create_timer
		    (fun _ -> 
		       Gc.major();
		       true
		    )
		    1.0 in
		()
	    end
	   )
    ()
