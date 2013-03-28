(* Implementation of the worker *)

open Mm_proto_aux


let jobs_at_once = 100


let proc_run (controller_host, controller_port) () =
  let controller =
    Mm_proto_clnt.Controller.V1.create_client2
      (`Socket(Rpc.Tcp,
	       Rpc_client.Inet(controller_host,controller_port),
	       Rpc_client.default_socket_config)) in

  (* Get the dimension of the matrices, and retrieve them from the ctrl:*)
  let ldim =
    Mm_proto_clnt.Controller.V1.get_dim controller left in
  let rdim =
    Mm_proto_clnt.Controller.V1.get_dim controller right in
  assert(ldim.rows = rdim.columns);

  let lmatrix =
    Array.make ldim.rows [| |] in
  for j = 0 to ldim.rows-1 do
    lmatrix.(j) <- Mm_proto_clnt.Controller.V1.get_row controller (left,j)
  done;
  let rmatrix =
    Array.make rdim.rows [| |] in
  for j = 0 to ldim.rows-1 do
    rmatrix.(j) <- Mm_proto_clnt.Controller.V1.get_row controller (right,j)
  done;
  
  (* Get jobs until there are no more jobs. *)
  let cont = ref true in
  while !cont do
    let jobs = Mm_proto_clnt.Controller.V1.pull_jobs controller jobs_at_once in
    cont := (jobs <> [| |]);
    
    let results = ref [] in
    Array.iter
      (fun job ->
	 let lcol = job.left_col in
	 let rrow = job.right_row in
	 let s = ref 0.0 in
	 for j = 0 to ldim.rows-1 do
	   s := !s +. lmatrix.(j).(lcol) *. rmatrix.(rrow).(j)
	 done;
	 results :=
	   { res_job = job;
	     res_val = !s
	   } :: !results
      )
      jobs;
    
    Mm_proto_clnt.Controller.V1.put_results controller (Array.of_list !results)
  done;

  (* Done: return "()" to caller *)
  ()


let configure cf addr =
  let controller_host =
    try cf # string_param(cf # resolve_parameter addr "controller_host")
    with Not_found ->
      failwith "Required param controller_host is missing" in
  let controller_port =
    try cf # int_param(cf # resolve_parameter addr "controller_port")
    with Not_found ->
      failwith "Required param controller_port is missing" in
  (controller_host, controller_port)
  

let setup srv (controller_host, controller_port) =
  Mm_proto_srv.Worker.V1.bind
    ~proc_ping:(fun () -> ())
    ~proc_run:(proc_run (controller_host, controller_port))
    srv

let worker_factory() =
  Rpc_netplex.rpc_factory
    ~configure
    ~name:"mm_worker"
    ~setup
    ()
