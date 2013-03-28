(* $Id: netplex_util.ml 1302 2009-11-30 23:09:36Z gerd $ *)

open Printf
open Netplex_types

let path_of_container_socket socket_dir sname pname sys_id =
  let dir' = Filename.concat socket_dir sname in
  let thread_name =
    match sys_id with
      | `Process pid -> sprintf "pid%d" pid
      | `Thread tid -> sprintf "thr%d" tid in
  let sock_name = sprintf "%s.%s.rpc" pname thread_name in
  let path = Filename.concat dir' sock_name in
  (dir', path)


let any_file_client_connector_1 fname =
  let st = Unix.stat fname in
  match st.Unix.st_kind with
    | Unix.S_SOCK ->
	(Rpc_client.Unix fname, `UD)
    | Unix.S_REG ->
	let f = open_in fname in
	( try
	    let t = input_line f in
	    ( match t with
		| "socket_file" ->
		    let d = input_line f in
		    let n = int_of_string d in
		    (Rpc_client.Internet(Unix.inet_addr_loopback, n),
		     `Socket_file)
		| "w32_pipe_file" ->
		    let d = input_line f in
		    (Rpc_client.W32_pipe d,
		     `W32_pipe_file)
		| _ ->
		    raise Not_found
	    )
	  with 
	    | _ ->
		close_in f; 
		failwith ("Netplex_sockserv.any_file_connector: Bad file: " ^ 
			    fname)
	)
    | _ ->
	failwith ("Netplex_sockserv.any_file_connector: Bad file type: " ^ 
		    fname)


let any_file_client_connector fname =
  (* reexported by Netplex_sockserv *)
  fst(any_file_client_connector_1 fname)

let client_connector addr =
  (* reexported by Netplex_sockserv *)
  match addr with
    | `Socket s ->
	( match s with
	    | Unix.ADDR_INET(ip,p) ->
		Rpc_client.Internet(ip,p)
	    | Unix.ADDR_UNIX p ->
		Rpc_client.Unix p
	)
    | `Socket_file fname ->
	let (conn, conn_type) = any_file_client_connector_1 fname in
	if conn_type <> `Socket_file then
	  failwith("Netplex_sockserv.client_connector: Unexpected file type: " ^ 
		     fname);
	conn
    | `W32_pipe pname ->
	Rpc_client.W32_pipe pname
    | `W32_pipe_file fname ->
	let (conn, conn_type) = any_file_client_connector_1 fname in
	if conn_type <> `W32_pipe_file then
	  failwith("Netplex_sockserv.client_connector: Unexpected file type: " ^ 
		     fname);
	conn
    | `Container(socket_dir,sname,pname,sys_id_or_any) ->
	( match sys_id_or_any with
	    | `Any ->
		failwith "client_connector: `Container(_,_,`Any) unsupported"
	    | #thread_sys_id as id ->
		let (_,path) =
		  path_of_container_socket socket_dir sname pname id in
		any_file_client_connector path
	)

	
let try_mkdir f =
  try
    Unix.mkdir f 0o777
  with
    | Unix.Unix_error(Unix.EEXIST,_,_) -> ()
;;


let with_fd fd f =
  try f fd
  with error ->
    Unix.close fd;
    raise error


let create_server_socket srvname proto addr =
  (* reexported by Netplex_sockserv *)
  let open_socket proto addr =
    ( match addr with
	| Unix.ADDR_UNIX path ->
	    ( try Unix.unlink path with _ -> () )
	| _ -> ()
    );
    with_fd
      (Unix.socket
	 (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0)
      (fun s ->
	 Unix.setsockopt s Unix.SO_REUSEADDR proto#lstn_reuseaddr;
	 Unix.setsockopt s Unix.SO_KEEPALIVE proto#so_keepalive;
	 Unix.bind s addr;
	 Unix.set_nonblock s;
	 Netsys.set_close_on_exec s;
	 Unix.listen s proto#lstn_backlog;
	 s
      )
  in

  let open_socket_file proto name =
    let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 0) in
    with_fd
      (Unix.socket
	 (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0)
      (fun s ->
	 Unix.setsockopt s Unix.SO_REUSEADDR proto#lstn_reuseaddr;
	 Unix.setsockopt s Unix.SO_KEEPALIVE proto#so_keepalive;
	 Unix.bind s addr;
	 Unix.set_nonblock s;
	 Netsys.set_close_on_exec s;
	 Unix.listen s proto#lstn_backlog;
	 ( match Unix.getsockname s with
	     | Unix.ADDR_INET(_, port) ->
		 let f = open_out name in
		 Unix.chmod name 0o600;
		 output_string f "socket_file\n";
		 output_string f (string_of_int port ^ "\n");
		 close_out f
	     | _ -> ()
	 );
	 s
      )
  in

  let open_w32_pipe proto name =
    let psrv = 
      Netsys_win32.create_local_pipe_server
	name Netsys_win32.Pipe_duplex max_int in
    with_fd
      (Netsys_win32.pipe_server_descr psrv)
      (fun s ->
	 Netsys_win32.pipe_listen psrv proto#lstn_backlog;
	 s
      )
  in

  let open_w32_pipe_file proto file_name =
    let name = 
      Netsys_win32.unpredictable_pipe_name() in
    let psrv = 
      Netsys_win32.create_local_pipe_server
	name Netsys_win32.Pipe_duplex max_int in
    with_fd
      (Netsys_win32.pipe_server_descr psrv)
      (fun s ->
	 Netsys_win32.pipe_listen psrv proto#lstn_backlog;
	 let f = open_out file_name in
	 Unix.chmod file_name 0o600;
	 output_string f "w32_pipe_file\n";
	 output_string f (name ^ "\n");
	 close_out f;
	 s
      )
  in
  match addr with
    | `Socket s -> 
	open_socket proto s
    | `Socket_file f -> 
	open_socket_file proto f
    | `W32_pipe p -> 
	open_w32_pipe proto p
    | `W32_pipe_file f -> 
	open_w32_pipe_file proto f
    | `Container _ ->
	failwith "Netplex_sockserv.open_socket_for: found `Container address"


let close_server_socket_1 ?(release=false) fd =
  let fd_style = Netsys.get_fd_style fd in
  match fd_style with
    | `W32_pipe_server ->
	(* As a special case, we also have to close the connect
           event descriptor
           FIXME: How to avoid that we have to special-case this?
	 *)
	let psrv = Netsys_win32.lookup_pipe_server fd in
	let cn_ev = Netsys_win32.pipe_connect_event psrv in
	let cn_fd = Netsys_win32.event_descr cn_ev in
	Netsys.gclose `W32_event cn_fd;
	if release then Netlog.Debug.release_fd fd;
	Netsys.gclose fd_style fd
    | _ ->
	if release then Netlog.Debug.release_fd fd;
	Netsys.gclose fd_style fd

let close_server_socket fd =
  (* reexported by Netplex_sockserv *)
  close_server_socket_1 fd
