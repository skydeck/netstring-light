(* Test client. Starts one sort operation *)

open Printf

let read_file file =
  let f = open_in file in
  let l = ref [] in
  try
    while true do
      let line = input_line f in
      l := line :: !l
    done;
    assert false
  with
    | End_of_file ->
	close_in f;
	Array.of_list(List.rev !l)

let write_file data =
  Array.iter
    (fun line ->
       print_endline line)
    data


let time f =
  let t0 = Unix.gettimeofday() in
  let r = f() in
  let t1 = Unix.gettimeofday() in
  eprintf "Time elapsed: %f\n%!" (t1 -. t0);
  r


let main() =
  let host = ref "localhost" in
  let port = ref 2021 in
  let mode = ref `Server in
  let file = ref None in
  Arg.parse
    [ "-host", Arg.Set_string host, 
      "<host>   Contact the sort server at this host";

      "-port", Arg.Set_int port, 
      "<port>   Contact the sort server at this port";

      "-local", Arg.Unit (fun () -> mode := `Local),
      "   Just sort in this client (don't contact server)";

      "-gen-random", Arg.Int (fun n ->
				for k = 1 to n do
				  let k = Random.int n in
				  let s = Digest.to_hex
				    (Digest.string (string_of_int k)) in
				  print_endline s
				done;
				exit 0
			     ),
      "<n>  Output n random strings";
				 
    ]
    (fun arg -> file := Some arg)
    (sprintf "usage: %s <options>" Sys.argv.(0));

  match !file with
    | None ->
	failwith "No input file given"
    | Some f ->
	let data = read_file f in
	( match !mode with
	    | `Local ->
		time(fun () -> Array.sort String.compare data);
		write_file data
	    | `Server ->
		let sorter =
		  Sort1_proto_clnt.Interface.V1.create_client2
		    (`Socket(Rpc.Tcp,
			     Rpc_client.Inet(!host,!port),
			     Rpc_client.default_socket_config)) in
		let data' =
		  time
		    (fun () ->
		       Sort1_proto_clnt.Interface.V1.sort sorter data) in
		Rpc_client.shut_down sorter;
		write_file data'
	)


let () =
  Netsys_signal.init();
  main()
