(* N subprocesses solve matrix multiplications 

   Each child has a camlbox where it receives matrices (of type
   float array array) to multiply. The master also has a camlbox where
   it receives the results.

   For simplicity of the example we only support sqare matrices.

   Invoke like:

   ./manymult <n> <size> <n_workers>

   where:
   - <n> = number of matrices to multiply
   - <size> = size of the matrices
   - <n_workers> = number of children 
 *)

open Printf

type request =
    { req_id : int;
      size : int;
      left : float array array;
      right : float array array
    }

type response =
    { resp_id : int;
      worker_id : int;
      result : float array array
    }

type task =
    { request : request;
      mutable response_opt : response option
    }

type worker_arg =
    [ `Multiply of request
    | `Terminate
    ]

type worker_ret =
   [ `Ready of int   (* worker_id *)
   | `Response of response
   ]

let msg_size =
  1024 * 1024  (* 1M fixed size *)

let create_tasks n size =
  Array.init
    n
    (fun k ->
       let left =
	 Array.init size
	   (fun _ -> 
	      Array.init size
		(fun _ ->
		   Random.float 1.0
		)) in
       let right =
	 Array.init size
	   (fun _ -> 
	      Array.init size
		(fun _ ->
		   Random.float 1.0
		)) in
       let req =
	 { req_id = k;
	   size = size;
	   left = left;
	   right = right;
	 } in
       { request = req;
	 response_opt = None
       }
    )

let multiply wid req =
  let r = Array.make_matrix req.size req.size 0.0 in
  for row = 0 to req.size-1 do
    for col = 0 to req.size-1 do
      let s = ref 0.0 in
      for j = 0 to req.size-1 do
	s := !s +. req.left.(j).(col) *. req.right.(row).(j)
      done;
      r.(row).(col) <- !s
    done
  done;
  { resp_id = req.req_id;
    worker_id = wid;
    result = r
  }

let worker wid wbox mname mfd =
  let ms = Netcamlbox.camlbox_sender_of_fd mname mfd in
  Netcamlbox.unlink_camlbox (Netcamlbox.camlbox_addr wbox);
  Netcamlbox.camlbox_send ms (`Ready wid);
  let cont = ref true in
  while !cont do
    let new_list =
      Netcamlbox.camlbox_wait wbox in
    List.iter
      (fun k ->
	 let (msg : worker_arg ref) =
	   Netcamlbox.camlbox_get wbox k in
	 match !msg with
	   | `Multiply req ->
	       let (resp : response) = multiply wid req in
	       Netcamlbox.camlbox_delete wbox k;
	       Netcamlbox.camlbox_send ms (`Response resp)
	   | `Terminate ->
	       cont := false
      )
      new_list
  done;
  exit 0

let wslots = 2

let prepare n_workers =
  (* Create master box: *)
  let mname = "camlbox_" ^ string_of_int(Unix.getpid()) in
  let mbox = Netcamlbox.create_camlbox mname (2*n_workers) msg_size in
  let mfd = Netcamlbox.camlbox_fd mname in
  
  (* Create worker boxes: *)
  let wboxes =
    Array.init n_workers
      (fun k ->
	 let wname = mname ^ "_" ^ string_of_int k in
	 let wbox = Netcamlbox.create_camlbox wname wslots msg_size in
	 let ws = Netcamlbox.camlbox_sender wname in
	 (wbox, ws, ref 2)
      ) in

  (* Fork workers: *)
  let pids = ref [] in
  Array.iteri
    (fun wid (wbox, _, _) ->
       match Unix.fork() with
	 | 0 ->
	     worker wid wbox mname mfd
	 | pid ->
	     pids := pid :: !pids
    )
    wboxes;

  (mbox, wboxes, !pids)


let wait_until_ready n_workers mbox =
  let missing = ref n_workers in
  while !missing > 0 do
    let idx_list = Netcamlbox.camlbox_wait mbox in
    List.iter
      (fun idx ->
         let (msg : worker_ret) = Netcamlbox.camlbox_get mbox idx in
         ( match msg with
             | `Ready _ -> decr missing
             | _ -> assert false
         );
         Netcamlbox.camlbox_delete mbox idx
      )
      idx_list
  done


let master tasks n_workers (mbox, wboxes, pids) =
  (* Loop *)
  let unresponded = ref (Array.length tasks) in
  let todo =
    ref 
      (Array.to_list
	 (Array.init 
	    (Array.length tasks)
	    (fun k -> k))) in
  let free_slots = ref(n_workers * wslots) in
  while !unresponded > 0 do
    if !free_slots > 0 && !todo <> [] then (
      (* Submit new request: *)
      let k_task = List.hd !todo in
      todo := List.tl !todo;
      let task = tasks.(k_task) in
      let submitted = ref false in
      Array.iter
	(fun (_, ws, wfree) ->
	   if not !submitted && !wfree > 0 then (
	     let (msg : worker_arg ref) = ref (`Multiply task.request) in
	     Netcamlbox.camlbox_send ws msg;
	     decr wfree;
	     decr free_slots;
	     submitted := true
	   )
	)
	wboxes;
      assert(!submitted)
    )
    else (
      (* All workers busy: wait for response *)
      let idx_list = Netcamlbox.camlbox_wait mbox in
      List.iter
	(fun idx ->
	   let (msg : worker_ret) = Netcamlbox.camlbox_get_copy mbox idx in
	   Netcamlbox.camlbox_delete mbox idx;
           match msg with
             | `Response resp ->
   	          tasks.( resp.resp_id ).response_opt <- Some resp;
	          let (_, _, wfree) = wboxes.( resp.worker_id ) in
	          incr wfree;
	          incr free_slots;
	          decr unresponded;
             | _ -> assert false
	)
	idx_list
    )
  done;

  (* Check that we've got all results: *)
  Array.iter
    (fun task ->
       assert(task.response_opt <> None)
    )
    tasks;

  (* Ask the children to terminate: *)
  Array.iter
    (fun (_, ws, _) ->
       let (msg : worker_arg ref) = ref `Terminate in
       Netcamlbox.camlbox_send ws msg;
    )
    wboxes;

  (* Collect children: *)
  List.iter
    (fun pid ->
       ignore(Unix.waitpid [] pid)
    )
    pids


let main() =
  let n = int_of_string Sys.argv.(1) in
  let size = int_of_string Sys.argv.(2) in
  let n_workers = int_of_string Sys.argv.(3) in

  printf "Forking children...\n%!";
  let boxtuple = prepare n_workers in
  printf "Creating tasks...\n%!";
  let tasks = create_tasks n size in
  printf "Performing tasks...\n%!";
  let t0 = Unix.gettimeofday() in
  let (mbox,_,_) = boxtuple in
  wait_until_ready n_workers mbox;
  Netcamlbox.unlink_camlbox (Netcamlbox.camlbox_addr mbox);
  master tasks n_workers boxtuple;
  let t1 = Unix.gettimeofday() in
  printf "t = %f\n%!" (t1-.t0)
  


let () =
  main()
