(* Implementation of worker processes *)

open Sort2_proto_aux
open Printf

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


let proc_alloc_shm session size emit =
  if size < 0L || size > Int64.of_int max_int then
    failwith "alloc_shm: bad size parameter";
  let (name,fd) = shm_open_excl (Unix.getpid()) 0 in
  let mem = Netsys_mem.memory_map_file fd true (Int64.to_int size) in
  Unix.close fd;
  let shm = 
    { shm_name = name;
      shm_addr = Int64.of_nativeint(Netsys_mem.memory_address mem) 
    } in
  Hashtbl.replace shm_tbl name (shm, mem);
  Netplex_cenv.logf `Info "addr=%Lx" shm.shm_addr;
  emit shm
  

let proc_sort_shm session (shm,data_offset) emit =
  let (_, mem) =
    try
      Hashtbl.find shm_tbl shm.shm_name
    with
      | Not_found ->
	  failwith("Shared mem object not found: " ^ shm.shm_name) in
  let data =
    (Netsys_mem.as_value mem data_offset : string array) in
  Netsys_mem.value_area mem;
  Netplex_cenv.logf `Info
    "Sorting %d elements" (Array.length data);
  let t0 = Unix.gettimeofday() in
  Array.sort String.compare data;
  let t1 = Unix.gettimeofday() in
  Netplex_cenv.logf `Info "time for sort: %f" (t1-.t0);
  emit ()


let proc_copy_shm session (shm1,data_offset1,shm2) emit =
  let (_, mem1) =
    try
      Hashtbl.find shm_tbl shm1.shm_name
    with
      | Not_found ->
	  failwith("Shared mem object not found: " ^ shm1.shm_name) in
  let mem2, mem2_unmap_flag =
    try
      (snd(Hashtbl.find shm_tbl shm2.shm_name), false)
    with
      | Not_found ->
	  let fd = 
	    Netsys_posix.shm_open
	      shm2.shm_name
	      [Netsys_posix.SHM_O_RDWR]
	      0o600 in
	  let mem = Netsys_mem.memory_map_file fd true (-1) in
	  Unix.close fd;
	  (mem, true) in
  let targetaddr =
    Int64.to_nativeint shm2.shm_addr in
  let data1 =
    (Netsys_mem.as_value mem1 data_offset1 : string array) in
  Netplex_cenv.logf `Info
    "size_mem1=%d size_mem2=%d"
    (Bigarray.Array1.dim mem1)
    (Bigarray.Array1.dim mem2);
  let (data_offset2,_) =
    Netsys_mem.init_value ~targetaddr mem2 0 data1 [] in
  Netplex_cenv.logf `Info
    "Worker init_value done";
  if mem2_unmap_flag then
    Netsys_mem.memory_unmap_file mem2;
  emit data_offset2


let free_shm shm =
  let (_, mem) =
    try
      Hashtbl.find shm_tbl shm.shm_name
    with
      | Not_found ->
	  failwith("Shared mem object not found: " ^ shm.shm_name) in
  Netsys_posix.shm_unlink shm.shm_name;
  Netsys_mem.memory_unmap_file mem;
  Hashtbl.remove shm_tbl shm.shm_name


let proc_free_shm session shm emit =
  free_shm shm;
  emit ()


let free_all() =
  let shm_list =
    Hashtbl.fold (fun _ (shm,_) acc -> shm::acc) shm_tbl [] in
  List.iter free_shm shm_list    


let configure cf addr = ()

let setup srv () =
  Sort2_proto_srv.Worker.V1.bind_async
    ~proc_null:(fun _ _ emit -> emit ())
    ~proc_alloc_shm
    ~proc_sort_shm
    ~proc_copy_shm
    ~proc_free_shm
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
		  List.iter
		    (fun signo ->
		       Netsys_signal.register_handler
			 ~name:"Sort2_worker"
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
