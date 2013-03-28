(* Test client. Starts one sort operation *)

open Sort2_proto_aux
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
  (* Turn off compaction: *)
  let gc = Gc.get() in
  Gc.set { gc with Gc.max_overhead = 1000000 };

  let host = ref "localhost" in
  let port = ref 2021 in
  let mode = ref `Server in
  let file = ref None in
  let sort_flag = ref true in
  Arg.parse
    [ "-host", Arg.Set_string host, 
      "<host>   Contact the sort server at this host";

      "-port", Arg.Set_int port, 
      "<port>   Contact the sort server at this port";

      "-nosort", Arg.Clear sort_flag,
      "   Don't sort - just loop data through server";

      "-local", Arg.Unit (fun () -> mode := `Local),
      "   Just sort in this client (don't contact server)";

      "-xdr-only", Arg.Unit (fun () -> mode := `Xdr_only),
      "   Only XDR-encode and -decode the data";

      "-bypass-controller", Arg.Unit (fun () -> mode := `Bypass_ctrl),
      "   Bypass controller, and contact workers directly";

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
		  Sort2_proto_clnt.Interface.V1.create_client2
		    (`Socket(Rpc.Tcp,
			     Rpc_client.Inet(!host,!port),
			     Rpc_client.default_socket_config)) in
		let data' =
		  time
		    (fun () ->
		       Sort2_proto_clnt.Interface.V1.sort 
			 sorter
			 (data, !sort_flag)) in
		Rpc_client.shut_down sorter;
		write_file data'
	    | `Bypass_ctrl ->
		let sorter =
		  Sort2_proto_clnt.Interface.V1.create_client2
		    (`Socket(Rpc.Tcp,
			     Rpc_client.Inet(!host,!port),
			     Rpc_client.default_socket_config)) in
		let data' =
		  time
		    (fun () ->
		       let workers =
			 Sort2_proto_clnt.Interface.V1.get_workers sorter () in
		       Rpc_client.shut_down sorter;
		       let esys = Unixqueue.create_unix_event_system() in
		       let r = ref None in
		       Sort2_controller.sort
			 esys data workers
			 (fun data_opt -> r := data_opt);
		       Unixqueue.run esys;
		       Hashtbl.iter
			 (fun _ c -> Rpc_client.shut_down c)
			 Sort2_controller.worker_clients;
		       Sort2_controller.free_all();
		       match !r with
			 | None ->
			     failwith "Error"
			 | Some sorted_data ->
			     sorted_data
		    ) in
		ignore(data);   (* don't collect until here *)
		write_file data'
	    | `Xdr_only ->
		let xdrt1 = Xdr.validate_xdr_type xdrt_sortdata in
		let t0 = Unix.gettimeofday() in
		let xdr1 = _of_sortdata data in
		let t1 = Unix.gettimeofday() in
		let s = Xdr.pack_xdr_value_as_string xdr1 xdrt1 [] in
		let t2 = Unix.gettimeofday() in
		let xdr2 = Xdr.unpack_xdr_value ~fast:true s xdrt1 [] in
		let t3 = Unix.gettimeofday() in
		let _data' = _to_sortdata xdr2 in
		let t4 = Unix.gettimeofday() in
		printf "t(lang -> xdr) = %f\n" (t1-.t0);
		printf "t(xdr -> string) = %f\n" (t2-.t1);
		printf "t(string -> xdr) = %f\n" (t3-.t2);
		printf "t(xdr -> lang) ) %f\n%!" (t4-.t3)
	)


let () =
  Netsys_signal.init();
  main()
