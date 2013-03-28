(* For comparison with manymult - multiply directly 

   Invoke like:

   ./unimult <n> <size>

   where:
   - <n> = number of matrices to multiply
   - <size> = size of the matrices
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
      result : float array array
    }

type task =
    { request : request;
      mutable response_opt : response option
    }

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

let multiply req =
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
    result = r
  }

let main() =
  let n = int_of_string Sys.argv.(1) in
  let size = int_of_string Sys.argv.(2) in

  printf "Creating tasks...\n%!";
  let tasks = create_tasks n size in
  printf "Performing tasks...\n%!";
  let t0 = Unix.gettimeofday() in
  for k = 0 to n-1 do
    tasks.(k).response_opt <- Some(multiply tasks.(k).request)
  done;
  let t1 = Unix.gettimeofday() in
  printf "t = %f\n%!" (t1-.t0)
  


let () =
  main()
