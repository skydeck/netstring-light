(* Implementation of the controller process *)

open Sort2_proto_aux
open Printf

let worker_clients = Hashtbl.create 10
  (* Maps endpoint names to RPC clients *)


let rec get_worker_endpoints n_workers =
  (* Loop until at least n_workers are available *)
  let endpoints = 
    Netplex_cenv.lookup_container_sockets "sort_worker" "Worker" in
  if Array.length endpoints < n_workers then (
    Netlog.logf `Warning
      "Not enough workers found in registry (only %d)- will retry in 1 second"
      (Array.length endpoints);
    Unix.sleep 1;
    get_worker_endpoints n_workers
  )
  else (
    Netlog.logf `Info
      "Found %d endpoints, using the first %d" 
      (Array.length endpoints) n_workers;
    Array.sub endpoints 0 n_workers
  )


let get_worker_client esys endpoint =
  try
    Hashtbl.find worker_clients endpoint
  with
    | Not_found ->
        let connector =
          Netplex_sockserv.any_file_client_connector endpoint in
        let client =
          Sort2_proto_clnt.Worker.V1.create_client2
            ~esys
            (`Socket(Rpc.Tcp, connector, Rpc_client.default_socket_config)) in
        Hashtbl.replace worker_clients endpoint client;
        client


let get_subarray_info n_workers n =
  let slice_len = n / n_workers in
  let m = n mod n_workers in
  let k = ref 0 in
  Array.init
    n_workers
    (fun i ->
       let k0 = !k in
       let l =
	 if i < m then slice_len+1 else slice_len in
       k := !k + l;
       (k0, l)
    )


let shm_tbl = Hashtbl.create 10
  (* maps name to shm *)


let rec shm_open_excl pid n =
  try
    let name = sprintf "/sort2_%d_%d" pid n in
    let fd = 
      Netsys_posix.shm_open
	name
	[Netsys_posix.SHM_O_RDWR; 
	 Netsys_posix.SHM_O_CREAT;
	 Netsys_posix.SHM_O_EXCL]
	0o600 in
    (name,fd)
  with
    | Unix.Unix_error(Unix.EEXIST,_,_) ->
	shm_open_excl pid (n+1)


let shm_open_existing shm =
  let fd = 
    Netsys_posix.shm_open
      shm.shm_name
      [Netsys_posix.SHM_O_RDWR]
      0o600 in
  let mem = Netsys_mem.memory_map_file fd true (-1) in
  Unix.close fd;
  mem


let free_shm ?(immediate_unmap=false) shm =
  let (_,mem) =
    try
      Hashtbl.find shm_tbl shm.shm_name
    with
      | Not_found ->
	  failwith("Shared mem object not found: " ^ shm.shm_name) in
  Netsys_posix.shm_unlink shm.shm_name;
  if immediate_unmap then
    Netsys_mem.memory_unmap_file mem;   (* dangerous ! *)
  Hashtbl.remove shm_tbl shm.shm_name


let free_all() =
  let shm_list =
    Hashtbl.fold (fun _ (shm,_) acc -> shm::acc) shm_tbl [] in
  List.iter free_shm shm_list    


let dummy_mem =
  Netsys_mem.alloc_memory_pages 1


let sort_subarray esys worker_endpoint data (k,l) when_done when_error =
  (* The following is written as an asynchrounous sequence of RPC calls.
     On success, finally when_done is called. On error, when_error
     is called.
   *)

  let sdata = Array.sub data k l in   (* TODO: avoid this copy *)
  Netlog.logf `Info
    "sort_subarray k=%d l=%d" k l;
  
  (* First find out how large sdata is: *)
  let _,sdata_bytelen =
    Netsys_mem.init_value
      dummy_mem 0 sdata [Netsys_mem.Copy_simulate] in
  Netlog.logf `Info "first init_value done bytelen=%d" sdata_bytelen;
  
  (* Alloc the buffer in the controller: *)
  let ctrl_mem_name, ctrl_mem_fd =
    shm_open_excl (Unix.getpid()) 0 in
  let ctrl_mem =
    Netsys_mem.memory_map_file ctrl_mem_fd true sdata_bytelen in
  Unix.close ctrl_mem_fd;
  let ctrl_shm =
    { shm_name = ctrl_mem_name;
      shm_addr = Int64.of_nativeint(Netsys_mem.memory_address ctrl_mem)
    } in
  Hashtbl.replace shm_tbl ctrl_mem_name (ctrl_shm, ctrl_mem);
  Netlog.logf `Info "addr=%Lx" ctrl_shm.shm_addr;

  (* Alloc the buffer in the worker: *)
  let client = get_worker_client esys worker_endpoint in
  Sort2_proto_clnt.Worker.V1.alloc_shm'async
    client
    (Int64.of_int sdata_bytelen)
    (fun alloc_shm_reply ->
       try
	 let worker_shm = alloc_shm_reply() in
	 (* Got this buffer. Now copy the subarray to this buffer. *)
	 let worker_mem = shm_open_existing worker_shm in
	 let worker_offset,_ =
	   Netsys_mem.init_value
	     ~targetaddr:(Int64.to_nativeint worker_shm.shm_addr)
	     worker_mem 0 sdata [] in
	 Netlog.logf `Info
	   "Second init_value done";
	 (* Request the sort: *)
	 Sort2_proto_clnt.Worker.V1.sort_shm'async
	   client
	   (worker_shm,worker_offset)
	   (fun sort_shm_reply ->
	      try
		let () = sort_shm_reply() in   (* check for errors! *)
		(* Copy the sorted data to the controller buffer, so we can
                   access them
		 *)
		Sort2_proto_clnt.Worker.V1.copy_shm'async
		  client
		  (worker_shm,worker_offset,ctrl_shm)
		  (fun copy_shm_reply ->
		     try
		       Netlog.logf `Info "Got data in ctrl buffer";
		       let ctrl_offset = copy_shm_reply() in
		       (* Get the sorted subarray: *)
		       let sdata_sorted =
			 Netsys_mem.as_value ctrl_mem ctrl_offset in
		       Netsys_mem.value_area ctrl_mem;
		       when_done 
			 sdata_sorted
			 (fun () -> free_shm ~immediate_unmap:true ctrl_shm);
		       (* Do some cleanup. Note that we assume now that
                          we don't access sdata_sorted any longer! 
			*)
		       Sort2_proto_clnt.Worker.V1.free_shm'async
			 client
			 (worker_shm)
			 (fun _ -> ())
		     with
		       | error ->
			   when_error error
		  )
	      with
		| error ->
		    when_error error
	   )
       with
	 | error ->
	     when_error error
    )
  
exception Merge_exit
exception Merge_done_array of int


let rec merge_into_0 out k_out in_arrays k_in =
  let l_in = Array.length in_arrays in

  assert(l_in > 0);

  if l_in = 1 then (
    let a = !(in_arrays.(0)) in
    let l = Array.length a - k_in.(0) in
    Array.blit a k_in.(0) out !k_out l;
    k_out := !k_out + l
  )
  else
    merge_into_1 out k_out in_arrays k_in
  
and merge_into_1 out k_out in_arrays k_in =
  (* at least two in_arrays *)
  let l_in = Array.length in_arrays in
  let l_out = Array.length out in
  try
    while true do
      (* Find the smallest in_array element: *)
      let smallest_v = ref !(in_arrays.(0)).(k_in.(0)) in
      let smallest_p = ref 0 in
      for i = 1 to l_in-1 do
	let v = !(in_arrays.(i)).(k_in.(i)) in
	if v < !smallest_v then (
	  smallest_v := v;
	  smallest_p := i
	)
      done;
      out.( !k_out ) <- !smallest_v;
      incr k_out;
      if !k_out = l_out then raise Merge_exit;
      let k_new = k_in.( !smallest_p ) + 1 in
      k_in.( !smallest_p ) <- k_new;
      if k_new = Array.length !(in_arrays.( !smallest_p )) then
	raise (Merge_done_array !smallest_p)
    done
  with
    | Merge_exit -> 
	()
    | Merge_done_array p ->
	(* We are done with in_array p. Remove it from our structure *)
	let in_arrays' =
	  Array.init
	    (Array.length in_arrays - 1)
	    (fun i ->
	       if i < p then in_arrays.(i) else in_arrays.(i+1)
	    ) in
	let k_in' = 
	  Array.init
	    (Array.length k_in - 1)
	    (fun i ->
	       if i < p then k_in.(i) else k_in.(i+1)
	    ) in
	merge_into_0 out k_out in_arrays' k_in'


let merge_into out in_arrays =
  let t0 = Unix.gettimeofday() in
  Netlog.logf `Info "merge_into";
  let l_in = Array.length in_arrays in
  assert(l_in > 0);
  let k_out = ref 0 in
  let k_in = Array.make l_in 0 in
  merge_into_0 out k_out in_arrays k_in;
  assert (!k_out = Array.length out);
  let t1 = Unix.gettimeofday() in
  Netlog.logf `Info "time for merge_into: %f" (t1-.t0)


let sort esys data worker_endpoints when_done =
  (* sorts asynchronously. Calls [when_done] with [Some sorted_data] on
     success, or [when_done None] on error
   *)
  let n_workers = Array.length worker_endpoints in
  let subarray_info = 
    get_subarray_info n_workers (Array.length data) in
  let to_merge =
    Array.make n_workers (ref [| |]) in
    
  let runcounter = ref 0 in
  let errorflag = ref false in
    
  Array.iteri
    (fun p (k,l) ->
       let worker_endpoint = worker_endpoints.(p) in
       sort_subarray
	 esys worker_endpoint data (k,l) 
	 (fun sdata_sorted cleanup ->  (* when_done *)
	    decr runcounter;
	    if not !errorflag then (
	      (* Note that sdata_sorted is only valid memory until [cleanup]
                 is called. Be careful with that! We cannot attach a finaliser
                 to sdata_sorted directly (it is living outside of the heap)
                 but with some care the ref cell around it will work.

                 Note that in there is a ref to the bigarray in the cleanup
                 closure, and this ref prevents that the shm is unmapped.
	       *)
	      let sdata_sorted_cell = ref sdata_sorted in
	      to_merge.(p) <- sdata_sorted_cell;
	      Gc.finalise 
		(fun _ -> Netlog.logf `Info "Cleanup"; cleanup()) 
		sdata_sorted_cell;
	      (* If all subarrays have arrived, start the merge: *)
	      if !runcounter = 0 then (
		merge_into data to_merge;
		(* and reply: *)
		when_done (Some data)
	      )
	    );
	    if !errorflag && !runcounter = 0 then (
	      when_done None
	    )
	 )
	 (fun error ->         (* when_error *)
	    decr runcounter;
	    Netlog.logf `Err
	      "Got exception: %s" (Netexn.to_string error);
	    errorflag := true;
	    if !runcounter = 0 then (
	      when_done None
	    )
	 );
       incr runcounter
    )
    subarray_info


let emit_error session =
  Rpc_server.reply_error session Rpc.System_err


let proc_sort n_workers session (data,sort_flag) emit =
  if sort_flag then (
    let esys = (Netplex_cenv.self_cont()) # event_system in
    let worker_endpoints =
      get_worker_endpoints n_workers in
    sort 
      esys data worker_endpoints
      (function
	 | Some sorted_data ->
	     emit sorted_data
	 | None ->
	     emit_error session
      )
  )
  else
    emit data


let proc_get_workers n_workers session () emit =
  let worker_endpoints =
    get_worker_endpoints n_workers in
  emit worker_endpoints



let configure cf addr =
  let n_workers =
    try cf # int_param(cf # resolve_parameter addr "n_workers") 
    with Not_found -> 1 in
  Netlog.logf `Info
    "Using param n_workers=%d" n_workers;
  (n_workers)



let setup srv (n_workers) =
  Sort2_proto_srv.Interface.V1.bind_async
    ~proc_null:(fun _ _ emit -> emit ())
    ~proc_sort:(proc_sort n_workers)
    ~proc_get_workers:(proc_get_workers n_workers)
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
		  List.iter
		    (fun signo ->
		       Netsys_signal.register_handler
			 ~name:"Sort2_controller"
			 ~signal:signo
			 ~callback:(fun _ -> free_all())
			 ~keep_default:true
			 ()
		    )
		    [ Sys.sigint; Sys.sigterm ];
                  ()
		method pre_finish_hook _ =
		  free_all()
              end
           )
    ()
