(* $Id: telnet_client.ml 1614 2011-06-09 15:08:56Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


(* NOTES:
 * - Every Unx or Unix system call wrapped with syscall
 *)


(* TODO:
 * - Sending Synch sequences could be done with higher priority. Currently,
 *   the output_buffer is first processed, then the synch_queue is copied
 *   to the output_buffer (and the output_queue is preempted for the moment).
 *   It is also possible to preempt the output_buffer, but this is much
 *   more complicated.
 *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Ftp_client" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Ftp_client" Debug.enable

let () =
  Netlog.Debug.register_module "Ftp_client" Debug.enable



exception Telnet_protocol of exn;;

let () =
  Netexn.register_printer
    (Telnet_protocol Not_found)
    (fun e ->
       match e with
	 | Telnet_protocol e' ->
	     "Telnet_client.Telnet_protocol(" ^ Netexn.to_string e' ^ ")"
	 | _ ->
	     assert false
    )


type telnet_command =
    Telnet_data of string
  | Telnet_nop
  | Telnet_dm           (* data mark *)
  | Telnet_brk          (* break *)
  | Telnet_ip           (* interrupt process *)
  | Telnet_ao           (* abort output *)
  | Telnet_ayt          (* are you there? *)
  | Telnet_ec           (* erase character *)
  | Telnet_el           (* erase line *)
  | Telnet_ga           (* Go ahead *)
  | Telnet_sb of char   (* Begin of subnegotiation *)
  | Telnet_se           (* End of subnegotation *)
  | Telnet_will of char (* Acknowledges that option is in effect *)
  | Telnet_wont of char (* Acknowledges that option is rejected *)
  | Telnet_do of char   (* Requests to turn on an option *)
  | Telnet_dont of char (* Requests to turn off an option *)
  | Telnet_unknown of char (* Unknown command *)
  | Telnet_eof          (* End of file *)
  | Telnet_timeout      (* Timeout event *)
;;


let prerr_command cmd =
  match cmd with
      Telnet_data s    -> dlog ("command: Data " ^ s)
    | Telnet_nop       -> dlog "command: NOP";
    | Telnet_dm        -> dlog "command: DM";
    | Telnet_brk       -> dlog "command: BRK";
    | Telnet_ip        -> dlog "command: IP";
    | Telnet_ao        -> dlog "command: AO";
    | Telnet_ayt       -> dlog "command: AYT";
    | Telnet_ec        -> dlog "command: EC";
    | Telnet_el        -> dlog "command: EL";
    | Telnet_ga        -> dlog "command: GA";
    | Telnet_sb c      -> dlog ("command: DB " ^ 
				  string_of_int(Char.code c));
    | Telnet_se        -> dlog "command: SE";
    | Telnet_will c    -> dlog ("command: WILL " ^
				  string_of_int(Char.code c));
    | Telnet_wont c    -> dlog ("command: WONT " ^
				  string_of_int(Char.code c));
    | Telnet_do c      -> dlog ("command: DO " ^
				  string_of_int(Char.code c));
    | Telnet_dont c    -> dlog ("command: DONT " ^
				  string_of_int(Char.code c));
    | Telnet_unknown c -> dlog ("command: unknown command " ^
				  string_of_int(Char.code c));
    | Telnet_eof       -> dlog "command: <eof>";
    | Telnet_timeout   -> dlog "command: <timeout>";
;;


type sockstate =
    Down
  | Up_rw 
  | Up_r
;;


type telnet_options =
    { connection_timeout : float;
      verbose_input : bool;
      verbose_output : bool;
    }
;;


type telnet_negotiated_option =
    Telnet_binary       (* see RFC 856 *)
  | Telnet_echo         (* see RFC 857 *)
  | Telnet_suppress_GA  (* see RFC 858 *)
  | Telnet_status       (* see RFC 859 *)
  | Telnet_timing_mark  (* see RFC 860 *)
  | Telnet_ext_opt_list (* see RFC 861 *)
  | Telnet_end_of_rec   (* see RFC 885 *)
  | Telnet_window_size  (* see RFC 1073 *)
  | Telnet_term_speed   (* see RFC 1079 *)
  | Telnet_term_type    (* see RFC 1091 *)
  | Telnet_X_display    (* see RFC 1096 *)
  | Telnet_linemode     (* see RFC 1184 *)
  | Telnet_flow_ctrl    (* see RFC 1372 *)
  | Telnet_auth         (* see RFC 1416 *)
  | Telnet_new_environ  (* see RFC 1572 and 1571 *)
  | Telnet_option of int   (* all other options *)
;;


type telnet_option_state =
    Not_negotiated
  | Accepted
  | Rejected
;;


let rec syscall f =  (* TODO: move to Aux *)
  (* Invoke system call, and handle EINTR *)
  try
    f()
  with
      Unix.Unix_error(Unix.EINTR,_,_) ->
	(* "interrupted system call": A signal happened while the system
	 * blocked.
	 * Simply restart the call.
	 *)
	syscall f
;;


let char_of_option p =
  match p with
    Telnet_binary       -> '\000'
  | Telnet_echo         -> '\001'
  | Telnet_suppress_GA  -> '\003'
  | Telnet_status       -> '\005'
  | Telnet_timing_mark  -> '\006'
  | Telnet_ext_opt_list -> '\255'
  | Telnet_end_of_rec   -> '\025'
  | Telnet_window_size  -> '\031'
  | Telnet_term_speed   -> '\032'
  | Telnet_term_type    -> '\024'
  | Telnet_X_display    -> '\035'
  | Telnet_linemode     -> '\034'
  | Telnet_flow_ctrl    -> '\033'
  | Telnet_auth         -> '\037'
  | Telnet_new_environ  -> '\039'
  | Telnet_option k     -> Char.chr k
;;


let option_of_char c =
  match c with
      '\000' -> Telnet_binary
    | '\001' -> Telnet_echo
    | '\003' -> Telnet_suppress_GA
    | '\005' -> Telnet_status
    | '\006' -> Telnet_timing_mark
    | '\255' -> Telnet_ext_opt_list
    | '\025' -> Telnet_end_of_rec
    | '\031' -> Telnet_window_size
    | '\032' -> Telnet_term_speed
    | '\024' -> Telnet_term_type
    | '\035' -> Telnet_X_display
    | '\034' -> Telnet_linemode
    | '\033' -> Telnet_flow_ctrl
    | '\037' -> Telnet_auth
    | '\039' -> Telnet_new_environ
    | k      -> Telnet_option (Char.code k)
;;


type telnet_connector =
    Telnet_connect of (string * int)
  | Telnet_socket of Unix.file_descr
;;


class telnet_session =
  object (self)
    val mutable connector = Telnet_connect("",0)

    val mutable esys = Unixqueue.create_unix_event_system()

    val mutable callback =
	    (fun _ -> () : bool -> unit)
	(* the argument indicates whether urgent processing has been requested
	 * or not.
	 *)

    val mutable output_queue = Queue.create()
    val mutable synch_queue = Queue.create()
    val mutable input_queue = Queue.create()

    val mutable output_buffer = Netbuffer.create 8192
    val mutable input_buffer = Netbuffer.create 8192
    val mutable primary_buffer = String.create 8192 
    val mutable send_eof = false
    val mutable sending_urgent_data = false

    val mutable group = None
    val mutable socket = Unix.stdin
    val mutable socket_state = Down
    val mutable connecting = None
    val mutable polling_wr = false
    val mutable input_timed_out = false
    val mutable output_timed_out = false

    val mutable options = 
	    { connection_timeout = 300.0;
	      verbose_input = false;
	      verbose_output = false;
	    }

    val mutable enabled_local_options = []
    val mutable offered_local_options = []
    val mutable state_local_options = []       
		(* does not contain options with state Not_negotiated *)

    val mutable enabled_remote_options = []
    val mutable requested_remote_options = []
    val mutable state_remote_options = []
		(* does not contain options with state Not_negotiated *)

    val mutable exn_handler = (fun _ -> ())


    initializer
      exn_handler <-
        (fun x ->
	   self # reset ();
	   match x with
	       Telnet_protocol x' ->
		 raise x          (* Never wrap twice *)
	     | _ ->
		 raise (Telnet_protocol x))


    method set_connection c =
      connector <- c
      

    method set_event_system new_ues =
      esys <- new_ues


    method set_callback cb =
      callback <- cb


    method set_exception_handler xh =
      exn_handler <- xh


    method output_queue = output_queue

    method input_queue = input_queue

    method get_options = options

    method set_options p = 
      options <- p

    method reset() =
      self # abort_connection;
      Queue.clear input_queue;
      Queue.clear output_queue;
      Queue.clear synch_queue;
      Netbuffer.clear input_buffer;
      Netbuffer.clear output_buffer;

    method enable_local_option p =
      if not (List.mem p enabled_local_options) then
	enabled_local_options <- p :: enabled_local_options


    method disable_local_option p =
      if List.mem p enabled_local_options then
	enabled_local_options <- List.filter (fun p' -> p <> p) 
	                                     enabled_local_options;
      if self # get_local_option p = Accepted then
	Queue.add (Telnet_wont (char_of_option p)) output_queue;


    method offer_local_option p = 
      if not (List.mem p offered_local_options) &
	self # get_local_option p <> Accepted  then begin
	offered_local_options <- p :: offered_local_options;
	Queue.add (Telnet_will (char_of_option p)) output_queue
      end


    method enable_remote_option p =
      if not (List.mem p enabled_remote_options) then
	enabled_remote_options <- p :: enabled_remote_options


    method disable_remote_option p =
      if List.mem p enabled_remote_options then
	enabled_remote_options <- List.filter (fun p' -> p <> p) 
	                                       enabled_remote_options;
      if self # get_remote_option p = Accepted then
	Queue.add (Telnet_dont (char_of_option p)) output_queue;


    method request_remote_option p = 
      if not (List.mem p requested_remote_options) &
	 self # get_remote_option p <> Accepted then begin
	requested_remote_options <- p ::requested_remote_options;
	Queue.add (Telnet_do (char_of_option p)) output_queue
      end


    method reset_local_option p =
      state_local_options <- List.filter 
	                       (fun (p',_) -> p <> p') 
	                       state_local_options


    method reset_remote_option p =
      state_remote_options <- List.filter 
	                       (fun (p',_) -> p <> p') 
	                       state_remote_options


    method get_local_option p =
      try
	List.assoc p state_local_options
      with Not_found -> Not_negotiated


    method get_remote_option p =
      try
	List.assoc p state_remote_options
      with Not_found -> Not_negotiated


    method option_negotiation_is_over =
      offered_local_options = [] & requested_remote_options = []


    method process_option_command cmd =
      match cmd with
	  Telnet_will c ->
	    (* If we previously requested the option, it is now in effect. *)
	    let p = option_of_char c in
	    let new_state =
	      if List.mem p requested_remote_options then begin
		requested_remote_options <- List.filter
		                              (fun p' -> p <> p')
		                              requested_remote_options;
		Accepted
	      end
	      else begin
		(* Otherwise accept the option if enabled, and reject if
		 * disabled.
		 *)
		if List.mem p enabled_remote_options then begin
		  Queue.add (Telnet_do c) output_queue;
		  Accepted
		end
		else begin
		  Queue.add (Telnet_dont c) output_queue;
		  Rejected
		end
	      end
	    in
	    state_remote_options <- (p, new_state) ::
	                            List.filter
		                      (fun (p',_) -> p <> p')
		                      state_remote_options;
	| Telnet_wont c ->
	    (* The option is rejected *)
	    let p = option_of_char c in
	    state_remote_options <- (p, Rejected) ::
	                            List.filter
		                      (fun (p',_) -> p <> p')
		                      state_remote_options;
	    requested_remote_options <- List.filter
		                          (fun p' -> p <> p')
		                          requested_remote_options;
	| Telnet_do c ->
	    (* If we previously offered the option, it is now in effect. *)
	    let p = option_of_char c in
	    let new_state =
	      if List.mem p offered_local_options then begin
		offered_local_options <- List.filter
		                           (fun p' -> p <> p')
		                           offered_local_options;
		Accepted
	      end
	      else begin
		(* Otherwise accept the option if enabled, and reject if
		 * disabled.
		 *)
		if List.mem p enabled_local_options then begin
		  Queue.add (Telnet_will c) output_queue;
		  Accepted
		end
		else begin
		  Queue.add (Telnet_wont c) output_queue;
		  Rejected
		end
	      end
	    in
	    state_local_options <- (p, new_state) ::
	                           List.filter
		                     (fun (p',_) -> p <> p')
		                     state_local_options;
	| Telnet_dont c ->
	    (* The option is rejected *)
	    let p = option_of_char c in
	    state_local_options <- (p, Rejected) ::
	                            List.filter
		                      (fun (p',_) -> p <> p')
		                      state_local_options;
	    offered_local_options <- List.filter
		                       (fun p' -> p <> p')
		                       offered_local_options;
	| _ ->
	    ()


    method fetch_subnegotiation =
      if Queue.length input_queue >= 1 then begin
	let para = ref "" in
	let n = ref 0 in
	let ended = ref false in
	begin 
	  try
	    Queue.iter
	      (function 
		   Telnet_data s ->
		     incr n;
		     para := !para ^ s
		 | Telnet_se ->
		     incr n;
		     ended := true;
		     raise Not_found   (* Exit 'iter' *)
		 | _ ->
		     raise Not_found)  (* Exit 'iter' *)
	      input_queue;
		with
		    Not_found -> ()
	end;
	if !ended then begin
	  (* Discard the first n elements of the queue *)
	  for i = 1 to !n do
	    ignore(Queue.take input_queue)
	  done;
	  Some !para
	end
	else None
      end
      else None


    method attach() =
      if group <> None then
	failwith "Telnet_client: already attached";

      let g = Unixqueue.new_group esys in

      let g1 = Unixqueue.new_group esys in  (* group for deferred 'connect' *)
      Unixqueue.once 
	esys
	g1
	0.0
	(fun () -> 
	   self # connect_server
	     (fun () ->
		dlog "Telnet connection: Connected!";
		(* 'group' must not be set earlier, because it is used as
	         * indicator whether a connection is established or not.
		 *)
		group <- Some g;
		let timeout_value = options.connection_timeout in
		Unixqueue.add_resource esys g (Unixqueue.Wait_in socket, 
					       timeout_value);
		Unixqueue.add_handler esys g (self # handler);
		polling_wr <- false;
		self # maintain_polling
	     )
	     (fun err ->
		dlog "Telnet connection: Connection error!";
		exn_handler err
	     )
	)


    method run() = 
      Unixqueue.run esys


    method update() =
      if group <> None then
	self # maintain_polling;


    method send_synch cmds =
      List.iter
	(fun cmd ->
	   Queue.add cmd synch_queue)
	cmds;
      Queue.add Telnet_dm synch_queue;
      self # update()


    method expect_input flag =
      match group with
	| None ->
	    failwith "Telnet_client: not attached"
	| Some g ->
	    Unixqueue.remove_resource esys g (Unixqueue.Wait_in socket);
	    let timeout_value =
	      if flag then 
		options.connection_timeout
	      else
		(-1.0) in
	    Unixqueue.add_resource esys g (Unixqueue.Wait_in socket, 
					   timeout_value)
	    

    method private connect_server f_ok f_err =

      begin match connector with
	| Telnet_connect(hostname, port) ->
	    dlog ("Telnet connection: Connecting to server " ^ 
		    hostname);

	    let g1 = Unixqueue.new_group esys in

	    let eng =
	      Uq_engines.connector 
		(`Socket(`Sock_inet_byname(Unix.SOCK_STREAM,
					   hostname,
					   port),
			 Uq_engines.default_connect_options))
		esys in
	    
	    connecting <- Some eng;

	    Uq_engines.when_state
	      ~is_done:(function
			  | `Socket(s,_) ->
			      Unixqueue.clear esys g1;
			      socket <- s;
			      connecting <- None;
			      syscall
				(fun () -> 
				   Unix.setsockopt s Unix.SO_OOBINLINE true);
			      Netlog.Debug.track_fd
				~owner:"Telnet_client"
				~descr:("connection to " ^ 
					  hostname ^  ":" ^ string_of_int port)
				s;
			      f_ok()
			  | _ -> assert false
		       )
	      ~is_error:(function
			   | err ->
			       Unixqueue.clear esys g1;
			       connecting <- None;
			       f_err err
			)
	      ~is_aborted:(fun () -> 
			     Unixqueue.clear esys g1;
			     connecting <- None
			  )
	      eng;
	    let timeout_value = options.connection_timeout in
	    Unixqueue.once esys g1 timeout_value eng#abort

	| Telnet_socket s ->
	    connecting <- None;
	    syscall(fun () -> Unix.setsockopt s Unix.SO_OOBINLINE true);
	    socket <- s;
	    Netlog.Debug.track_fd
	      ~owner:"Telnet_client"
	      ~descr:("connection to " ^ 
			try Netsys.string_of_sockaddr(Netsys.getpeername s)
			with _ -> "(noaddr)")
	      s;
	    dlog "Telnet connection: Got connected socket";
	    let g1 = Unixqueue.new_group esys in
	    Unixqueue.once esys g1 0.0 f_ok
      end;
      
      socket_state <- Up_rw;
      Netbuffer.clear input_buffer;
      Netbuffer.clear output_buffer;
      Queue.clear input_queue;
      send_eof <- false;
      sending_urgent_data <- false;
      input_timed_out <- false;
      output_timed_out <- false;


    method private shutdown =
      dlog "Telnet connection: Shutdown!";
      begin match socket_state with
	  Down -> ()
	| (Up_rw | Up_r) -> 
	    dlog "Telnet connection: Closing socket!";
	    Netlog.Debug.release_fd socket;
	    try
	      syscall (fun () -> Unix.close socket)
	    with
		_ -> ()           (* ignore failed 'close' *)
      end;
      socket_state <- Down;
      match group with
	  Some g -> 
	    Unixqueue.clear esys g;
	    group <- None;
	| None ->  ()


    method private abort_connection =
      ( match connecting with
	  | None -> ()
	  | Some eng -> eng#abort()
      );
      match group with
	  Some g -> 
	    Unixqueue.remove_resource esys g (Unixqueue.Wait_in socket);
	    if polling_wr then begin
	      Unixqueue.remove_resource esys g (Unixqueue.Wait_out socket);
	      polling_wr <- false;
	    end;
	    self # shutdown;
	    assert (group = None);
        | None -> 
	    ()


    method private maintain_polling =

      (* If one of the following conditions is true, we need not to poll
       * the write side of the socket:
       * - The write_queue is empty and the synch_queue is empty
       *)

      let timeout_value = options.connection_timeout in

      if (Queue.length output_queue = 0 & Queue.length synch_queue = 0)
      then begin
	if polling_wr then begin
	  let g = match group with
	      Some x -> x
	    | None -> assert false
	  in
(*  prerr_endline "REMOVE";   *)
	  Unixqueue.remove_resource esys g (Unixqueue.Wait_out socket);
	end;
	polling_wr <- false
      end;

      (* On the other hand, all of the following conditions must be true
       * to enable polling again:
       * - The write_queue is not empty or the synch_queue is not empty
       *)

      if (Queue.length output_queue > 0 or Queue.length synch_queue > 0)
      then begin
	if not polling_wr then begin
	  let g = match group with
	      Some x -> x
	    | None -> assert false
	  in
(*  prerr_endline "ADD";    *)
	  Unixqueue.add_resource esys g (Unixqueue.Wait_out socket, 
					 timeout_value
					);
	end;
	polling_wr <- true;
      end;


    method private handler  _ _ ev =
      let g = match group with
	  Some x -> x
	| None -> 
	    (* This is possible while shutting down the socket *)
	    raise Equeue.Reject
      in
      match ev with
	| Unixqueue.Input_arrived (g0,fd0) ->
	    if g0 = g then 
	      try self # handle_input with 
		  Unix.Unix_error(_,_,_) as x -> exn_handler x
	    else raise Equeue.Reject
	| Unixqueue.Output_readiness (g0,fd0) ->
	    if g0 = g then 
	      try self # handle_output with 
		  Unix.Unix_error(_,_,_) as x -> exn_handler x
	    else raise Equeue.Reject
	| Unixqueue.Timeout (g0, op) ->
	    if g0 = g then 
	      try self # handle_timeout op with 
		  Unix.Unix_error(_,_,_) as x -> exn_handler x
	    else raise Equeue.Reject
	| _ ->
	    raise Equeue.Reject

    (**********************************************************************)
    (***                    THE TIMEOUT HANDLER                         ***)
    (**********************************************************************)

    method private handle_timeout op =
      begin match op with
	  Unixqueue.Wait_in _  -> input_timed_out  <- true
	| Unixqueue.Wait_out _ -> output_timed_out <- true
	| _ -> ()
      end;

      if input_timed_out & output_timed_out then begin
	(* No network packet arrived for a period of time.
	 * May happen while connecting to a server, or during operation.
	 *)
	if socket_state = Down then
	  raise Equeue.Reject;
	
	Queue.add Telnet_timeout input_queue;
	
	dlog "Telnet connection: Timeout event!";
	
	self # abort_connection;
	
        (* Invoke the callback function: *)

	callback false;
      end


    (**********************************************************************)
    (***                     THE INPUT HANDLER                          ***)
    (**********************************************************************)

    method private handle_input =
      (* Data have arrived on the 'socket'. First we receive as much as we
       * can; then the data are interpreted as sequence of messages.
       *)
      
      (* Ignore this event if the socket is Down (this may happen
       * if the input side is closed while there are still input
       * events in the queue):
       *)
      if socket_state = Down then
	raise Equeue.Reject;

      input_timed_out <- false;

      dlog "Telnet connection: Input event!";

      let _g = match group with
	  Some x -> x
	| None -> assert false
      in
      
      (* Read data into the primary_buffer *)

      let n, eof =
	syscall
	  (fun () ->
	     try
	       let n =
		 Unix.read 
		   socket primary_buffer 0 (String.length primary_buffer) in
	       (n, n=0)
	     with
	       | Unix.Unix_error(Unix.EAGAIN,_,_) -> 
		   (0, false)
	  ) in

      Netbuffer.add_sub_string input_buffer primary_buffer 0 n;

      (* Interpret the octet stream in 'input_buffer' as sequence of
       * commands
       *)
      
      let length = Netbuffer.length input_buffer in
      let buffer = Netbuffer.unsafe_buffer input_buffer in

      let is_urgent = ref false in

      let finish pos =
	Netbuffer.delete input_buffer 0 pos
      in

      let clear_data_path() =
	(* remove any non-urgent data *)
	let new_queue = Queue.create() in
	let within_sb = ref false in
	Queue.iter
	  (fun cmd ->
	     match cmd with
		(Telnet_nop | Telnet_dm | Telnet_brk | Telnet_ip | Telnet_ao |
		 Telnet_ayt | Telnet_ga | 
		 Telnet_will _ | Telnet_wont _ | Telnet_do _ |
		 Telnet_dont _ | Telnet_unknown _ | Telnet_eof | Telnet_timeout)
		 ->
		   Queue.add cmd new_queue
	       | Telnet_sb c ->
		   Queue.add cmd new_queue;
		   within_sb := true
	       | Telnet_se ->
		   Queue.add cmd new_queue;
		   within_sb := false
	       | _ -> 
		   if !within_sb then Queue.add cmd new_queue;
	  )
	  input_queue;
	input_queue <- new_queue
      in
      
      let rec interpret pos =
	if pos >= length then
	  finish length
	else
	  match buffer.[pos] with
	      '\255' -> 
		(* IAC character *)
		if pos+1 < length then begin
		  match buffer.[pos+1] with
		    | '\240' -> Queue.add Telnet_se input_queue;
			        interpret(pos+2);
		    | '\241' -> Queue.add Telnet_nop input_queue;
			        interpret(pos+2)
		    | '\242' -> clear_data_path();
			        Queue.add Telnet_dm input_queue;
				is_urgent := true;
			        interpret(pos+2)
		    | '\243' -> Queue.add Telnet_brk input_queue;
			        interpret(pos+2)
		    | '\244' -> Queue.add Telnet_ip input_queue;
			        interpret(pos+2)
		    | '\245' -> Queue.add Telnet_ao input_queue;
			        interpret(pos+2)
		    | '\246' -> Queue.add Telnet_ayt input_queue;
			        interpret(pos+2)
		    | '\247' -> Queue.add Telnet_ec input_queue;
			        interpret(pos+2)
      		    | '\248' -> Queue.add Telnet_el input_queue;
			        interpret(pos+2)
		    | '\249' -> Queue.add Telnet_ga input_queue;
			        interpret(pos+2)
		    | '\255' -> Queue.add (Telnet_data(String.make 1 '\255'))
			                  input_queue;
			        interpret(pos+2)
		    | ('\250'..'\254') ->
			if pos+2 < length then begin
			  let option = buffer.[pos+2] in
			  match buffer.[pos+1] with
			    | '\250' -> Queue.add (Telnet_sb option) 
				                  input_queue;
				        interpret(pos+3);
			    | '\251' -> Queue.add (Telnet_will option) 
				                  input_queue;
				        interpret(pos+3);
			    | '\252' -> Queue.add (Telnet_wont option) 
				                  input_queue;
				        interpret(pos+3);
			    | '\253' -> Queue.add (Telnet_do option) 
				                  input_queue;
				        interpret(pos+3);
			    | '\254' -> Queue.add (Telnet_dont option) 
				                  input_queue;
				        interpret(pos+3);
			    | _ -> assert false
			end
			else finish pos
		    | _ -> Queue.add (Telnet_unknown(buffer.[pos+1]))
			             input_queue;
			   interpret(pos+2);
		end
		else finish pos
	    | c ->
		begin try 
		  let pos' = Netbuffer.index_from input_buffer pos '\255' in
		  Queue.add 
		    (Telnet_data(Netbuffer.sub input_buffer pos (pos'-pos)))
		    input_queue;
		  interpret pos'
		with
		    Not_found ->
		      Queue.add 
			(Telnet_data
			   (Netbuffer.sub input_buffer pos (length-pos)))
			input_queue;
		      finish length
		end
      in

      if eof then begin
	dlog "got EOF";
	Queue.add Telnet_eof input_queue;
	self # abort_connection;
      end
      else
	interpret 0;

      if !Debug.enable && options.verbose_input then begin
	dlog "Telnet input queue:";
	Queue.iter prerr_command input_queue;
	dlog "<end of queue>";
      end;


      (* TODO: Find out whether urgent data arrived, and remove all non-
       * commands from the queue.
       *)

      (* Invoke the callback function: *)

      callback !is_urgent;

      (* Now there may be new elements on the write queue. *)

      self # maintain_polling;


    (**********************************************************************)
    (***                     THE OUTPUT HANDLER                         ***)
    (**********************************************************************)

    method private handle_output =

      (* Ignore this event if the socket is not Up_rw (this may happen
       * if the output side is closed while there are still output
       * events in the queue):
       *)
      if socket_state <> Up_rw then
	raise Equeue.Reject;

      output_timed_out <- false;

      dlog "Telnet connection: Output event!";

      let _g = match group with
	  Some x -> x
	| None -> assert false
      in

      (* If the write buffer is empty, copy new commands from the write
       * queue to the write buffer.
       *)

      let rec copy_string s pos =
	try
	  let pos' = String.index_from s pos '\255' in
	  Netbuffer.add_string output_buffer (String.sub s pos (pos'-pos));
	  Netbuffer.add_string output_buffer "\255\255";
	  copy_string s (pos'+1)
	with
	    Not_found ->
	      if pos = 0 then
		Netbuffer.add_string output_buffer s
	      else
		let l = String.length s in
		Netbuffer.add_sub_string output_buffer s pos (l-pos)
      in

      let q = 
	if Queue.length synch_queue > 0 then	  
	  synch_queue
	else
	  output_queue in

      let rec copy () =
	match Queue.take q with
	    Telnet_data s ->
	      copy_string s 0;
	      copy()
	  | Telnet_nop ->
	      Netbuffer.add_string output_buffer "\255\241";
	      copy();
	  | Telnet_dm ->
	      Netbuffer.add_string output_buffer "\255\242";
	      copy();
	  | Telnet_brk ->
	      Netbuffer.add_string output_buffer "\255\243";
	      copy();
	  | Telnet_ip ->
	      Netbuffer.add_string output_buffer "\255\244";
	      copy();
	  | Telnet_ao ->
	      Netbuffer.add_string output_buffer "\255\245";
	      copy();
	  | Telnet_ayt ->
	      Netbuffer.add_string output_buffer "\255\246";
	      copy();
	  | Telnet_ec ->
	      Netbuffer.add_string output_buffer "\255\247";
	      copy();
	  | Telnet_el ->
	      Netbuffer.add_string output_buffer "\255\248";
	      copy();
	  | Telnet_ga ->
	      Netbuffer.add_string output_buffer "\255\249";
	      copy();
	  | Telnet_sb c ->
	      Netbuffer.add_string output_buffer "\255\250";
	      Netbuffer.add_string output_buffer (String.make 1 c);
	      copy();
	  | Telnet_se ->
	      Netbuffer.add_string output_buffer "\255\240";
	      copy();
	  | Telnet_will c ->
	      Netbuffer.add_string output_buffer "\255\251";
	      Netbuffer.add_string output_buffer (String.make 1 c);
	      copy();
	  | Telnet_wont c ->
	      Netbuffer.add_string output_buffer "\255\252";
	      Netbuffer.add_string output_buffer (String.make 1 c);
	      copy();
	  | Telnet_do c ->
	      Netbuffer.add_string output_buffer "\255\253";
	      Netbuffer.add_string output_buffer (String.make 1 c);
	      copy();
	  | Telnet_dont c ->
	      Netbuffer.add_string output_buffer "\255\254";
	      Netbuffer.add_string output_buffer (String.make 1 c);
	      copy();
	  | Telnet_unknown c ->
	      Netbuffer.add_string output_buffer "\255";
	      Netbuffer.add_string output_buffer (String.make 1 c);
	      copy();
	  | Telnet_eof ->
	      send_eof <- true;
	      Queue.clear output_queue;
	      Queue.clear synch_queue;
	  | Telnet_timeout ->
	      copy()
      in

      if Netbuffer.length output_buffer = 0 then begin

	if q == synch_queue then begin
	  sending_urgent_data <- true;
	  dlog "Sending urgent data";
	end
	else
	  sending_urgent_data <- false;

	if !Debug.enable && options.verbose_output then begin
	  dlog "Telnet output queue:";
	  Queue.iter prerr_command output_queue;
	  dlog "<end of queue>";
	end;

	try copy() with Queue.Empty -> ()
      end;

      let l = Netbuffer.length output_buffer in
      if l > 0 then begin
	let flags =
	  if sending_urgent_data then [ Unix.MSG_OOB ] else [] in
	let k = 
	  syscall
	    (fun () ->
	       try
		 Unix.send
		   socket (Netbuffer.unsafe_buffer output_buffer) 0 l flags
	       with
		 | Unix.Unix_error(Unix.EAGAIN,_,_) -> 0
	    ) in
	Netbuffer.delete output_buffer 0 k;
      end;

      if Netbuffer.length output_buffer = 0 && send_eof then begin
	dlog "Telnet connection: Sending EOF";
	syscall(fun () -> Unix.shutdown socket Unix.SHUTDOWN_SEND);
	socket_state <- Up_r;
      end;

      self # maintain_polling;
  end
;;

let () =
  Netsys_signal.init()
