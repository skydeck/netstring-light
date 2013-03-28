#use "topfind";;
#require "equeue,equeue-ssl";;

(* This example connects stdin/stdout with a remote SSL service. To test
 * replace the following IP address and port with real numbers, e.g.
 * with an HTTPS server. Then start with
 *
 * ocaml ssl_client.ml
 *
 * Then enter something like:
 * 
 * GET / HTTP/1.0
 *
 * (+ double new lines).
 *)

let remote_ip_addr = "66.249.93.104" ;;  (* This is www.google.com *)
let remote_port = 443 ;;


let maybe_error err_opt =
  match err_opt with
    | None -> ()
    | Some err ->
	raise err
;;


let main() =
  Ssl.init();
  let esys = Unixqueue.create_unix_event_system() in
  let cl = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  prerr_endline "* TCP connected";
  Unix.connect cl 
    (Unix.ADDR_INET
       (Unix.inet_addr_of_string remote_ip_addr, remote_port));
  
  let cl_ctx = Ssl.create_client_context Ssl.TLSv1 in
  let cl_mplex = 
    Uq_ssl.create_ssl_multiplex_controller cl cl_ctx esys in
  cl_mplex # start_ssl_connecting
    ~when_done:
    (fun err_opt ->
       maybe_error err_opt;
       prerr_endline "* SSL connected";
       
       let shutdown_in = ref (fun _ _ _ -> ()) in
       let shutdown_out = ref (fun _ _ _ -> ()) in

       let in_ch = 
	 new Uq_engines.input_async_mplex 
	   ~onshutdown:(`Action (fun ch m s -> !shutdown_in ch m s))
	   ~buffer_size:10
	   (cl_mplex :> Uq_engines.multiplex_controller) in
       let sender =
	 new Uq_engines.sender
	   ~src:in_ch
	   ~dst:Unix.stdout
	   ~close_dst:false
	   esys in
       
       let out_ch =
	 new Uq_engines.output_async_mplex
	   ~onshutdown:(`Action (fun ch m s -> !shutdown_out ch m s))
	   ~buffer_size:10
	   (cl_mplex :> Uq_engines.multiplex_controller) in
       let receiver =
	 new Uq_engines.receiver
	   ~src:Unix.stdin
	   ~dst:out_ch
	   ~close_src:false
	   esys in
       
       (* Shutdown actions: Because we have two channels attached to a
        * single multiplexer, we must synchronize the shutdown.
        *)
       shutdown_in := ( fun ch m s ->
			  (* The SSL connection is terminated. If the out_ch
                           * is already finished, we shut down the multiplexer.
                           * Else we simply abort the output channel.
                           *)
			  match out_ch # state with
			    | `Working _ ->
				out_ch # abort()
			    | _ ->
				m # start_shutting_down
				  ~when_done:(fun _ -> ()) ()
		      );
       shutdown_out := (fun ch m s ->
			  (* The terminal connection is terminated. Now we
                           * have to check whether in_ch is still alive.
                           *)
			  match in_ch # state with
			    | `Working _ ->
				in_ch # abort()
			    | _ ->
				m # start_shutting_down
				  ~when_done:(fun _ -> ()) ()
		       );

       ()
    )
    ();

  esys# run()
;;


(* Unixqueue.set_debug_mode true;*)
main();;

prerr_endline "DONE";
