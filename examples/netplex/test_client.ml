(* $Id: test_client.ml 1239 2009-05-25 00:30:46Z gerd $ *)

(* A client for all sample servers in this directory *)

open Printf

let start() =
  let host = ref "localhost" in
  let port = ref None in
  let query = ref None in
  let tmo = ref (-1.0) in

  Arg.parse
    [ "-host", Arg.Set_string host,
      "<hostname>  Contact the server on this host (default: localhost)";
      
      "-port", Arg.Int (fun n -> port := Some n),
      "<port>  Contact the server at this port";

      "-timeout", Arg.Set_float tmo,
      "<tmo>  Set a timeout value in seconds";
    ]
    (fun s -> query := Some s)
    "usage: test_client [options] <query>";

  let query_string =
    match !query with
      | None -> 
          failwith "Query is missing on the command-line";
      | Some q -> 
          q in

  try
    let rpc_client =
      match !port with
	| None ->
	    failwith "Port is missing on the command-line"
	| Some p ->
            Operation_clnt.P.V.create_client
              (Rpc_client.Inet(!host,p)) Rpc.Tcp
    in
    Rpc_client.configure rpc_client 0 !tmo;
    
    let r = Operation_clnt.P.V.operation rpc_client query_string  in
    printf "Result: %s\n%!" r;
    
    Rpc_client.shut_down rpc_client;
  with
    | e ->
	printf "Exception: %s\n%!" (Netexn.to_string e)


let () =
  Netsys_signal.init();
  start()


  
