(* $Id: netplex_admin.ml 1774 2012-04-03 21:28:51Z gerd $ *)

open Printf
open Netplex_ctrl_aux

let main() =
  let sockdir = ref "/tmp/.netplex" in
  let cmds = ref [] in
  let admin_cmd = ref [] in
  let admin_target = ref "*" in
  Arg.parse
    [ "-sockdir",
      Arg.String (fun s -> sockdir := s),
      "<dir>  Set the socket directory of the Netplex to administer";

      "-list",
      Arg.Unit (fun () -> cmds := `List :: !cmds),
      "  List available Netplex services";

      "-containers",
      Arg.Unit (fun () -> cmds := `Containers :: !cmds),
      "  List available Netplex services with container details";

      "-enable",
      Arg.String (fun s -> cmds := `Enable s :: !cmds),
      "<name>  Enable service <name>";

      "-disable",
      Arg.String (fun s -> cmds := `Disable s :: !cmds),
      "<name>  Disable service <name>";
      
      "-restart",
      Arg.String (fun s -> cmds := `Restart s :: !cmds),
      "<name>  Restart service <name>";

      "-restart-all",
      Arg.Unit (fun () -> cmds := `Restart_all :: !cmds),
      "  Restart all services";

      "-shutdown",
      Arg.Unit (fun () -> cmds := `Shutdown :: !cmds),
      "  Shutdown the whole Netplex";

      "-reopen-logfiles",
      Arg.Unit (fun () -> cmds := `Reopen_logfiles :: !cmds),
      "  Reopen logfiles (if possible)";

      "-unlink",
      Arg.Unit (fun () -> cmds := `Unlink :: !cmds),
      "  Unlink persistent kernel objects that are only temporarily used";

      "-receiver",
      Arg.String (fun pat -> admin_target := pat),
      "<pat>  Restrict receivers of admin messages to services matching <pat>";
    ]
    (fun arg ->
       admin_cmd := arg :: !admin_cmd)
    "Usage: netplex-admin [ options ] [ admin_cmd arg ... ]";

  let socket =
    Filename.concat !sockdir "netplex.controller/admin" in

  let conn =
    Netplex_sockserv.any_file_client_connector socket in

  let client = ref None in
  let get_client() =
    match !client with
      | None ->
          let c =
            Netplex_ctrl_clnt.Admin.V2.create_client 
              conn
              Rpc.Tcp in
          client := Some c;
          c
      | Some c ->
          c in
          
  let exit_code = ref 0 in

  let check_exn f =
    try f() 
    with
      | Rpc.Rpc_server Rpc.System_err ->
	  prerr_endline "Netplex exception";
	  exit_code := 10
      | error ->
	  prerr_endline ("RPC exception: " ^ Netexn.to_string error);
	  exit_code := 11
  in

  let check_code code =
    match code with
      | `code_ok -> ()
      | `code_error msg ->
	  prerr_endline ("Netplex exception: " ^ msg);
	  exit_code := 10
  in

  let state_list =
    [ state_enabled, "Enabled";
      state_disabled, "Disabled";
      state_restarting, "Restarting";
      state_down, "Down";
    ] in

  let srv_line s =
    printf "%s: %s %d containers\n" 
      s.srv_name
      ( try List.assoc s.srv_state state_list
	with Not_found -> "?"
      )
      s.srv_nr_containers
  in

  let proto_line proto port =
    printf "    %s @ %s\n"
      proto
      port
  in

  let cont_line cinfo =
    printf "    %s: %s\n"
      cinfo.cnt_sys_id
      ( match cinfo.cnt_state with
	  | `cstate_accepting -> "accepting"
	  | `cstate_selected -> "selected"
	  | `cstate_busy -> "busy"
	  | `cstate_starting -> "starting"
	  | `cstate_shutdown -> "shutdown"
      )
  in

  let list with_containers () =
    let l = Netplex_ctrl_clnt.Admin.V2.list (get_client()) () in
    Array.iter
      (fun s ->
	 srv_line s;
	 if s.srv_protocols = [| |] then
	   printf "    no protocols defined\n"
	 else
	   Array.iter
	     (fun p ->
		if p.prot_ports = [| |] then
		  proto_line p.prot_name "-"
		else
		  Array.iter
		    (fun port ->
		       let port_s =
			 match port with
			   | `pf_unknown -> "unknown"
			   | `pf_unix path ->
			       "local:" ^ path
			   | `pf_inet inet ->
			       "inet:" ^ 
				 inet.inet_addr ^ ":" ^ 
				 (string_of_int inet.inet_port)
			   | `pf_inet6 inet ->
			       "inet6:" ^ 
				 inet.inet6_addr ^ ":" ^ 
				 (string_of_int inet.inet6_port)
		       in
		       proto_line p.prot_name port_s
		    )
		    p.prot_ports
	     )
	     s.srv_protocols;
	 if with_containers then
	   if s.srv_containers = [| |] then
	     printf "    no containers\n"
	   else
	     Array.iter cont_line s.srv_containers
      )
      l
  in

  List.iter
    (function
       | `List ->
	   check_exn (list false)
       | `Containers ->
	   check_exn (list true)
       | `Enable pat ->
	   check_exn
	     (fun () ->
		let code = 
                  Netplex_ctrl_clnt.Admin.V2.enable (get_client()) pat in
		check_code code)
       | `Disable pat ->
	   check_exn
	     (fun () ->
		let code = 
                  Netplex_ctrl_clnt.Admin.V2.disable (get_client()) pat in
		check_code code)
       | `Restart pat ->
	   check_exn
	     (fun () ->
		let code = 
                  Netplex_ctrl_clnt.Admin.V2.restart (get_client()) pat in
		check_code code)
       | `Restart_all ->
	   check_exn
	     (fun () ->
		let code = 
                  Netplex_ctrl_clnt.Admin.V2.restart_all (get_client()) () in
		check_code code)
       | `Shutdown ->
	   check_exn
	     (fun () ->
		let code = 
                  Netplex_ctrl_clnt.Admin.V2.system_shutdown (get_client()) () in
		check_code code)
       | `Reopen_logfiles ->
	   check_exn
	     (fun () ->
		let code =
		  Netplex_ctrl_clnt.Admin.V2.reopen_logfiles (get_client()) () in
		check_code code)
       | `Unlink ->
           let path = Filename.concat !sockdir "netplex.pmanage" in
           let pm = Netsys_pmanage.pmanage path in
           pm # unlink()
    )
    (List.rev !cmds);

  ( match List.rev !admin_cmd with
      | [] -> ()
      | name :: args ->
	  let msg =
	    { Netplex_ctrl_aux.msg_name = name;
	      msg_arguments = Array.of_list args
	    } in
	  check_exn
	    (fun () ->
	       Netplex_ctrl_clnt.Admin.V2.send_admin_message 
		 (get_client())
		 (!admin_target, msg))
  );

  ( match !client with
      | None -> ()
      | Some c -> Rpc_client.shut_down c
  );
  
  exit !exit_code
;;


main();;
