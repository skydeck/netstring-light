(* Implementation of worker processes *)

open Sort1_proto_aux

let kept_data = Hashtbl.create 10
  (* Maps partition ID to sortdata *)

let delayed_actions = ref []
  (* List of tuples (part1_id, part2_id, f): When both partitions are in
     [kept_data], run f
   *)

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


let controller_clients = Hashtbl.create 10
  (* Maps endpoint names to RPC clients. Should only be one *)

let get_controller_client endpoint =
    try
    Hashtbl.find controller_clients endpoint
  with
    | Not_found ->
	let esys = (Netplex_cenv.self_cont())#event_system in
	let connector =
	  Netplex_sockserv.any_file_client_connector endpoint in
	let client =
	  Sort1_proto_clnt.Controller.V1.create_client2
	    ~esys
	    (`Socket(Rpc.Tcp, connector, Rpc_client.default_socket_config)) in
	Hashtbl.replace controller_clients endpoint client;
	client


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


let check_for_delayed_actions() =
  let new_delayed_actions = ref [] in
  List.iter
    (fun (p1,p2,f) ->
       if Hashtbl.mem kept_data p1 && Hashtbl.mem kept_data p2 then (
	 (* We run this within "once" so exceptions can be easily handled *)
	 let esys = (Netplex_cenv.self_cont()) # event_system in
	 let g = Unixqueue.new_group esys in
	 Unixqueue.once esys g 0.0 f
       )
       else
	 new_delayed_actions := (p1,p2,f) :: !new_delayed_actions
    )
    !delayed_actions;
  delayed_actions := List.rev !new_delayed_actions


let execute_op part_id data cont =
  match cont with
    | `keep ->
	Hashtbl.replace kept_data part_id data;
	check_for_delayed_actions()

    | `forward fwd ->
	let client = get_worker_client fwd.destination in
	Sort1_proto_clnt.Worker.V1.merge_partition'async
	  client
	  (fwd.merge_with_partition_id,
	   part_id,
	   data,
	   fwd.new_partition_id,
	   fwd.continuation)
	  (check_for_errors "merge_partition")

    | `return ep ->
	let client = get_controller_client ep in
	Sort1_proto_clnt.Controller.V1.return_result'async
	  client
	  data
	  (check_for_errors "return_result")


let proc_sort_partition session (part_id, data, cont) emit =
  Array.sort String.compare data;
  emit ();
  execute_op part_id data cont


let merge part1_id part2_id partr_id cont () =
  let data1 =
    try Hashtbl.find kept_data part1_id
    with Not_found -> assert false in
  let data2 =
    try Hashtbl.find kept_data part2_id
    with Not_found -> assert false in
  Hashtbl.remove kept_data part1_id;
  Hashtbl.remove kept_data part2_id;
  Gc.major();
  let l1 = Array.length data1 in
  let l2 = Array.length data2 in
  let datar = Array.create (l1 + l2) "" in
  let k1 = ref 0 in
  let k2 = ref 0 in
  let kr = ref 0 in
  while !k1 < l1 && !k2 < l2 do
    if data1.( !k1 ) < data2.( !k2 ) then (
      datar.( !kr ) <- data1.( !k1 );
      incr k1;
      incr kr
    )
    else (
      datar.( !kr ) <- data2.( !k2 );
      incr k2;
      incr kr
    )
  done;
  if !k1 < l1 then
    Array.blit data1 !k1 datar !kr (l1 - !k1);
  if !k2 < l2 then
    Array.blit data2 !k2 datar !kr (l2 - !k2);
  execute_op partr_id datar cont


let proc_merge_partition session
                         (part1_id, part2_id, data2, partr_id, cont)
                         emit =
  Hashtbl.replace kept_data part2_id data2;
  delayed_actions := 
    (part1_id, part2_id, merge part1_id part2_id partr_id cont) :: 
      !delayed_actions;
  emit ();
  check_for_delayed_actions()


let configure cf addr = ()

let setup srv () =
  Sort1_proto_srv.Worker.V1.bind_async
    ~proc_null:(fun _ _ emit -> emit ())
    ~proc_sort_partition
    ~proc_merge_partition
    srv

let worker_factory() =
  Rpc_netplex.rpc_factory
    ~name:"sort_worker"
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

