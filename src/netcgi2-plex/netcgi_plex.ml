(* $Id: netcgi_plex.ml 1243 2009-05-25 23:45:36Z gerd $ *)

open Netcgi

type mountpoint =
    [ `Mount_dir of string
    | `Mount_at of string
    ]

let wait_for_request timeout fd =
  let fd_style = Netsys.get_fd_style fd in
  let rec wait t =
    let t0 = Unix.gettimeofday() in
    try
      let ok = Netsys.wait_until_readable fd_style fd t in
      if not ok then
	`Conn_close
      else
	`Conn_keep_alive
    with
      | Unix.Unix_error(Unix.EINTR,_,_) ->
	  if timeout < 0.0 then
	    wait t
	  else
	    let t1 = Unix.gettimeofday() in
	    let t' = t -. (t1 -. t0) in
	    if t' > 0.0 then
	      wait t'
	    else
	      `Conn_close
  in
  wait timeout


let rec do_connection timeout do_request fd =
  let cdir =
    wait_for_request timeout fd in
  if cdir = `Conn_keep_alive then (
    let cdir' =
      do_request fd in
    if cdir' = `Conn_keep_alive then
      do_connection timeout do_request fd
    else
      cdir'
  )
  else (
    cdir
  )


let term_connection cont fd cdir =
  match cdir with
    | `Conn_close ->
	Unix.shutdown fd Unix.SHUTDOWN_ALL;
	Unix.close fd;
    | `Conn_close_linger ->
	Unix.setsockopt_optint fd Unix.SO_LINGER (Some 15);
	Unix.shutdown fd Unix.SHUTDOWN_ALL;
	Unix.close fd
    | `Conn_error e ->
	cont # log `Crit ("Exception " ^ Netexn.to_string e);
	Unix.shutdown fd Unix.SHUTDOWN_ALL;
	Unix.close fd;
    | `Conn_keep_alive ->
	assert false


let fcgi_processor ?(config = Netcgi.default_config)
                   ?(output_type = (`Direct "" : Netcgi.output_type))
                   ?(arg_store = fun _ _ _ -> `Automatic)
		   ?(exn_handler = fun _ f -> f())
                   ?(timeout = -1.0) ?mount f : Netplex_types.processor =
  (* TODO: mount *)
  ( object
      inherit Netplex_kit.empty_processor_hooks()

      method process ~when_done cont fd proto =
	let max_conns = 5 in
	  (* TODO: Get max_conns from workload manager *)
	let log msg =
	  cont # log `Err msg in
	let cdir =
	  try 
	    Unix.clear_nonblock fd;
	    do_connection
	      timeout
	      (Netcgi_fcgi.handle_request 
		 config output_type arg_store exn_handler (f cont)
		 ~max_conns ~log:(Some log)
	      )
	      fd
	  with
	    | e -> `Conn_error e in
	term_connection cont fd cdir;
	when_done()

      method supported_ptypes =
	[ `Multi_processing; `Multi_threading ]

    end
  )
;;


let scgi_processor ?(config = Netcgi.default_config)
                   ?(output_type = (`Direct "" : Netcgi.output_type))
                   ?(arg_store = fun _ _ _ -> `Automatic)
		   ?(exn_handler = fun _ f -> f())
                   ?(timeout = -1.0) ?mount f : Netplex_types.processor =
  (* TODO: mount *)
  ( object
      inherit Netplex_kit.empty_processor_hooks()

      method process ~when_done cont fd proto =
	let log msg =
	  cont # log `Err msg in
	let cdir =
	  try 
	    Unix.clear_nonblock fd;
	    do_connection
	      timeout
	      (Netcgi_scgi.handle_request 
		 config output_type arg_store exn_handler (f cont)
		 ~log:(Some log)
	      )
	      fd
	  with
	    | e -> `Conn_error e in
	term_connection cont fd cdir;
	when_done()

      method supported_ptypes =
	[ `Multi_processing; `Multi_threading ]

    end
  )
;;


let ajp_processor ?(config = Netcgi.default_config)
                  ?(output_type = (`Direct "" : Netcgi.output_type))
                  ?(arg_store = fun _ _ _ -> `Automatic)
		  ?(exn_handler = fun _ f -> f())
                  ?(timeout = -1.0) ?mount f : Netplex_types.processor =
  (* TODO: mount *)
  ( object
      inherit Netplex_kit.empty_processor_hooks()

      method process ~when_done cont fd proto =
	let log msg =
	  cont # log `Err msg in
	let cdir =
	  try 
	    Unix.clear_nonblock fd;
	    do_connection
	      timeout
	      (Netcgi_ajp.handle_request 
		 config output_type arg_store exn_handler (f cont)
		 ~log:(Some log)
	      )
	      fd
	  with
	    | e -> `Conn_error e in
	term_connection cont fd cdir;
	when_done()

      method supported_ptypes =
	[ `Multi_processing; `Multi_threading ]

    end
  )
;;


let multi_process ?config ?output_type ?arg_store ?exn_handler ?timeout
                  ?mount ?(enable = [ `FCGI; `SCGI; `AJP ]) 
		  hooks f =
  ( object
      inherit Netplex_kit.processor_base hooks

      method process ~when_done cont fd proto =
	let real_processor =
	  match proto with
	    | "fcgi" when List.mem `FCGI enable ->
		fcgi_processor
		  ?config ?output_type ?arg_store ?exn_handler ?timeout
		  ?mount
		  (fun cont fcgi -> f cont (fcgi : #Netcgi_fcgi.cgi :> cgi))
	    | "scgi" when List.mem `SCGI enable ->
		scgi_processor
		  ?config ?output_type ?arg_store ?exn_handler ?timeout
		  ?mount
		  (fun cont cgi -> f cont cgi)
	    | "ajp" when List.mem `AJP enable ->
		ajp_processor
		  ?config ?output_type ?arg_store ?exn_handler ?timeout
		  ?mount
		  (fun cont cgi -> f cont cgi)
	    | _ ->
		failwith ("Unsupported protocol: " ^ proto)
	in
	real_processor # process ~when_done cont fd proto

      method supported_ptypes =
	[ `Multi_processing; `Multi_threading ]
    end
  )		  
;;


class factory ?config ?enable ?(name = "netcgi")
              ?output_type ?arg_store ?exn_handler
	      ?configure
	      f : Netplex_types.processor_factory =
object
  method name = name
  method create_processor ctrl_cfg cfg addr =
    let timeout_opt =
      try Some(float (cfg#int_param (cfg#resolve_parameter addr "timeout")))
      with
	| Not_found -> None in

    let hooks =
      match configure with
	| Some c ->
	    c cfg addr
	| None ->
	    new Netplex_kit.empty_processor_hooks() in

    (* TODO: parse mount_dir, mount_at *)

    multi_process
      ?config ?output_type ?arg_store ?exn_handler ?timeout:timeout_opt
      (* ?mount *) ?enable 
      hooks f
    
end


let factory = new factory
