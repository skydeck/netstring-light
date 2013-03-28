(* $Id: uq_ssl.ml 1706 2012-02-15 17:30:54Z gerd $ *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Uq_ssl" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Uq_ssl" Debug.enable

let () =
  Netlog.Debug.register_module "Uq_ssl" Debug.enable


open Printf

exception Ssl_error of Ssl.ssl_error

type ssl_socket_state = [ `Unset | `Client | `Server | `Unclean | `Clean ]


let string_of_ssl_error e =
  match e with
    | Ssl.Error_none ->
	"Ssl.Error_none"
    | Ssl.Error_ssl ->
	"Ssl.Error_ssl"
    | Ssl.Error_want_read ->
	"Ssl.Error_want_read"
    | Ssl.Error_want_write ->
	"Ssl.Error_want_write"
    | Ssl.Error_want_x509_lookup ->
	"Ssl.Error_want_x509_lookup"
    | Ssl.Error_syscall ->
	"Ssl.Error_syscall"
    | Ssl.Error_zero_return ->
	"Ssl.Error_zero_return"
    | Ssl.Error_want_connect ->
	"Ssl.Error_want_connect"
    | Ssl.Error_want_accept ->
	"Ssl.Error_want_accept"

let string_of_verify_error e =
  match e with
    | Ssl.Error_v_unable_to_get_issuer_cert ->
	"Ssl.Error_v_unable_to_get_issuer_cert"
    | Ssl.Error_v_unable_to_get_ctl -> 
	"Ssl.Error_v_unable_to_get_ctl"
    | Ssl.Error_v_unable_to_decrypt_cert_signature -> 
	"Ssl.Error_v_unable_to_decrypt_cert_signature"
    | Ssl.Error_v_unable_to_decrypt_CRL_signature -> 
	"Ssl.Error_v_unable_to_decrypt_CRL_signature"
    | Ssl.Error_v_unable_to_decode_issuer_public_key -> 
	"Ssl.Error_v_unable_to_decode_issuer_public_key"
    | Ssl.Error_v_cert_signature_failure -> 
	"Ssl.Error_v_cert_signature_failure"
    | Ssl.Error_v_CRL_signature_failure -> 
	"Ssl.Error_v_CRL_signature_failure"
    | Ssl.Error_v_cert_not_yet_valid -> 
	"Ssl.Error_v_cert_not_yet_valid"
    | Ssl.Error_v_cert_has_expired -> 
	"Ssl.Error_v_cert_has_expired" 
    | Ssl.Error_v_CRL_not_yet_valid -> 
	"Ssl.Error_v_CRL_not_yet_valid" 
    | Ssl.Error_v_CRL_has_expired -> 
	"Ssl.Error_v_CRL_has_expired" 
    | Ssl.Error_v_error_in_cert_not_before_field -> 
	"Ssl.Error_v_error_in_cert_not_before_field"
    | Ssl.Error_v_error_in_cert_not_after_field -> 
	"Ssl.Error_v_error_in_cert_not_after_field" 
    | Ssl.Error_v_error_in_CRL_last_update_field -> 
	"Ssl.Error_v_error_in_CRL_last_update_field"
    | Ssl.Error_v_error_in_CRL_next_update_field -> 
	"Ssl.Error_v_error_in_CRL_next_update_field" 
    | Ssl.Error_v_out_of_mem -> 
	"Ssl.Error_v_out_of_mem"
    | Ssl.Error_v_depth_zero_self_signed_cert -> 
	"Ssl.Error_v_depth_zero_self_signed_cert"
    | Ssl.Error_v_self_signed_cert_in_chain -> 
	"Ssl.Error_v_self_signed_cert_in_chain" 
    | Ssl.Error_v_unable_to_get_issuer_cert_locally -> 
	"Ssl.Error_v_unable_to_get_issuer_cert_locally"
    | Ssl.Error_v_unable_to_verify_leaf_signature -> 
	"Ssl.Error_v_unable_to_verify_leaf_signature" 
    | Ssl.Error_v_cert_chain_too_long -> 
	"Ssl.Error_v_cert_chain_too_long"
    | Ssl.Error_v_cert_revoked -> 
	"Ssl.Error_v_cert_revoked"
    | Ssl.Error_v_invalid_CA -> 
	"Ssl.Error_v_invalid_CA" 
    | Ssl.Error_v_path_length_exceeded -> 
	"Ssl.Error_v_path_length_exceeded"
    | Ssl.Error_v_invalid_purpose -> 
	"Ssl.Error_v_invalid_purpose"
    | Ssl.Error_v_cert_untrusted -> 
	"Ssl.Error_v_cert_untrusted" 
    | Ssl.Error_v_cert_rejected -> 
	"Ssl.Error_v_cert_rejected" 
    | Ssl.Error_v_subject_issuer_mismatch -> 
	"Ssl.Error_v_subject_issuer_mismatch"
    | Ssl.Error_v_akid_skid_mismatch -> 
	"Ssl.Error_v_akid_skid_mismatch"
    | Ssl.Error_v_akid_issuer_serial_mismatch -> 
	"Ssl.Error_v_akid_issuer_serial_mismatch"
    | Ssl.Error_v_keyusage_no_certsign -> 
	"Ssl.Error_v_keyusage_no_certsign"
    | Ssl.Error_v_application_verification -> 
	"Ssl.Error_v_application_verification"
	

let () =
  Netexn.register_printer
    (Ssl_error Ssl.Error_none)
    (fun x ->
       match x with
	 | Ssl_error e -> "Uq_ssl.Ssl_error(" ^ string_of_ssl_error e ^ ")"
	 | _ -> assert false
    );
  Netexn.register_printer
    (Ssl.Connection_error Ssl.Error_none)
    (fun x ->
       match x with
	 | Ssl.Connection_error e -> 
	     "Ssl.Connection_error(" ^ string_of_ssl_error e ^ ")"
	 | _ -> assert false
    );
  Netexn.register_printer
    (Ssl.Accept_error Ssl.Error_none)
    (fun x ->
       match x with
	 | Ssl.Accept_error e -> 
	     "Ssl.Accept_error(" ^ string_of_ssl_error e ^ ")"
	 | _ -> assert false
    );
  Netexn.register_printer
    (Ssl.Read_error Ssl.Error_none)
    (fun x ->
       match x with
	 | Ssl.Read_error e -> "Ssl.Read_error(" ^ string_of_ssl_error e ^ ")"
	 | _ -> assert false
    );
  Netexn.register_printer
    (Ssl.Write_error Ssl.Error_none)
    (fun x ->
       match x with
	 | Ssl.Write_error e -> "Ssl.Write_error(" ^ string_of_ssl_error e ^ ")"
	 | _ -> assert false
    );
  Netexn.register_printer
    (Ssl.Verify_error Ssl.Error_v_out_of_mem)
    (fun x ->
       match x with
	 | Ssl.Verify_error e ->
	     "Ssl.Verify_error(" ^ string_of_verify_error e ^ ")"
	 | _ -> assert false
    )


let string_of_socket_state =
  function
    | `Unset -> "Unset"
    | `Client -> "Client"
    | `Server -> "Server"
    | `Unclean -> "Unclean"
    | `Clean -> "Clean"


class type ssl_multiplex_controller =
object
  inherit Uq_engines.multiplex_controller
  method ssl_socket : Ssl.socket
  method ssl_socket_state : ssl_socket_state
  method ssl_connecting : bool
  method ssl_accepting : bool
  method start_ssl_connecting : 
    when_done:(exn option -> unit) -> unit -> unit
  method start_ssl_accepting :
    when_done:(exn option -> unit) -> unit -> unit
  method inactivate_no_close : unit -> unit
end


let string_of_tag =
  function
    | `Connecting -> "Connecting"
    | `Accepting -> "Accepting"
    | `Reading -> "Reading"
    | `Writing -> "Writing"
    | `Writing_eof -> "Writing_eof"
    | `Shutting_down -> "Shutting_down"


let string_of_exn_opt =
  function
    | None -> "successful"
    | Some x -> Netexn.to_string x


class ssl_mplex_ctrl ?(close_inactive_descr=true)
                     ?(preclose = fun () -> ())
		     ?(initial_state = `Unset)
		     ?timeout
                     fd ssl_sock esys : ssl_multiplex_controller =
  let () = Unix.set_nonblock fd in
  let fdi = Netsys.int64_of_file_descr fd in
object(self)
  val mutable alive = true    (* if false => state in { `Clean, `Unclean } *)
  val mutable read_eof = false
  val mutable wrote_eof = false

  val mutable state = (initial_state : ssl_socket_state)

  val mutable connecting = false   (* true only in state `Unset *)
  val mutable accepting = false    (* true only in state `Unset *)
  val mutable reading = None       (* <> None only in states `Client/`Server *)
  val mutable writing = None       (* <> None only in states `Client/`Server *)
  val mutable writing_eof = None   (* <> None only in states `Client/`Server *)
  val mutable shutting_down = None (* <> None only in states `Client/`Server *)
  val mutable disconnecting = None

  val mutable have_handler = false

  val mutable pending = []
         (* list of pending socket operations *)

  val mutable expecting_input = false
  val mutable expecting_output = false

  val mutable timers = Hashtbl.create 7

  val group = Unixqueue.new_group esys

  method alive = alive
  method ssl_socket = ssl_sock
  method ssl_socket_state = state

  method ssl_connecting = connecting
  method ssl_accepting = accepting
  method reading = reading <> None
  method writing = writing <> None
  method shutting_down = shutting_down <> None
  method read_eof = read_eof
  method wrote_eof = wrote_eof

  method supports_half_open_connection = true

  method mem_supported = false



  method start_ssl_connecting ~when_done () =
    if state <> `Unset then
      failwith "#start_connecting: no longer possible in this state";
    if connecting || accepting then
      failwith "#start_connecting: handshake already in progress";
    dlogr
      (fun () ->
	 sprintf "FD %Ld: start_ssl_connecting" fdi);
    let when_done exn_opt =
      dlogr
	(fun () ->
	   sprintf "FD %Ld: done start_ssl_connecting: %s" 
	     fdi (string_of_exn_opt exn_opt)
	);
      when_done exn_opt in
    self # nonblock_operation
      (ref false)
      `Connecting
      (fun () ->
	 try
	   Ssl.connect ssl_sock;
	   state <- `Client;
	   connecting <- false;
	   (false, false, fun () -> when_done None)
	 with
	   | Ssl.Connection_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl.Connection_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl.Connection_error ssl_err ->
	       state <- `Unclean;
	       connecting <- false;
	       (false, false, 
		fun () -> 
		  self # inactivate_no_close();
		  when_done (Some (Ssl_error ssl_err))
	       )
	   | err ->
	       state <- `Unclean;
	       connecting <- false;
	       (false, false, 
		fun () -> 
		  self # inactivate_no_close();
		  when_done (Some err)
	       )
      )
      (fun x -> 
	 self # inactivate_no_close();
	 when_done (Some x));
    connecting <- true


  method start_ssl_accepting ~when_done () =
    if state <> `Unset then
      failwith "#start_accepting: no longer possible in this state";
    if connecting || accepting then
      failwith "#start_accepting: handshake already in progress";
    dlogr
      (fun () ->
	 sprintf "FD %Ld: start_ssl_accepting" fdi);
    let when_done exn_opt =
      dlogr
	(fun () ->
	   sprintf "FD %Ld: done start_ssl_accepting: %s" 
	     fdi (string_of_exn_opt exn_opt)
	);
      when_done exn_opt in
    self # nonblock_operation
      (ref false)
      `Accepting
      (fun () ->
	 try
	   Ssl.accept ssl_sock;
	   state <- `Server;
	   accepting <- false;
	   (false, false, fun () -> when_done None)
	 with
	   | Ssl.Accept_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl.Accept_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl.Accept_error ssl_err ->
	       state <- `Unclean;
	       accepting <- false;
	       (false, false, 
		fun () -> 
		  self # inactivate_no_close();
		  when_done (Some (Ssl_error ssl_err))
	       )
	   | err ->
	       state <- `Unclean;
	       accepting <- false;
	       (false, false, 
		fun () -> 
		  self # inactivate_no_close();
		  when_done (Some err)
	       )
      )
      (fun x -> 
	 self # inactivate_no_close();
	 when_done (Some x));
    accepting <- true;


  method start_reading ?(peek = fun() -> ()) ~when_done s pos len =
    if pos < 0 || len < 0 || pos + len > String.length s then
      invalid_arg "#start_reading";
    if state <> `Client && state <> `Server then
      failwith "#start_reading: bad state";
    if reading <> None then
      failwith "#start_reading: already reading";
    if shutting_down <> None then
      failwith "#start_reading: already shutting down";
    dlogr
      (fun () ->
	 sprintf "FD %Ld: start_reading" fdi);
    let when_done exn_opt =
      dlogr
	(fun () ->
	   sprintf "FD %Ld: done start_reading: %s" 
	     fdi (string_of_exn_opt exn_opt)
	);
      when_done exn_opt in
    let cancel_flag = ref false in
    self # nonblock_operation
      cancel_flag
      `Reading
      (fun () ->
	 try
	   (* peek(); *)
	   (* [peek] is used by auth-local. It does not work for SSL. *)
	   let n = Ssl_exts.single_read ssl_sock s pos len in
	   reading <- None;
	   assert(n > 0);
	   (false, false, fun () -> when_done None n)
	 with
	   | Ssl.Read_error Ssl.Error_zero_return ->
	       (* Read EOF *)
	       read_eof <- true;  
	       (* Note: read_eof should be consistent with Ssl.read *)
	       (false, false, fun () -> when_done (Some End_of_file) 0)
	   | Ssl.Read_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl.Read_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl.Read_error ssl_err ->
	       state <- `Unclean;
	       reading <- None;
	       (false, false, fun () -> when_done (Some (Ssl_error ssl_err)) 0)
	   | err ->
	       state <- `Unclean;
	       reading <- None;
	       (false, false, fun () -> when_done (Some err) 0)
      )
      (fun x -> self # cancel_reading_with x);
    reading <- Some (when_done, cancel_flag)

  method start_mem_reading ?(peek = fun() -> ()) ~when_done m pos len =
    raise Uq_engines.Mem_not_supported

  method cancel_reading () =
    self # cancel_reading_with Uq_engines.Cancelled

  method private cancel_reading_with x =
    dlogr
      (fun () ->
	 sprintf "FD %Ld: cancel_reading" fdi);
    match reading with
      | None ->
	  ()
      | Some (f_when_done, cancel_flag) ->
	  assert(not !cancel_flag);
	  self # cancel_operation `Reading;
	  cancel_flag := true;
	  reading <- None;
	  f_when_done (Some x) 0


  method start_writing ~when_done s pos len =
    if pos < 0 || len < 0 || pos + len > String.length s then
      invalid_arg "#start_writing";
    if state <> `Client && state <> `Server then
      failwith "#start_writing: bad state";
    if writing <> None || writing_eof <> None then
      failwith "#start_writing: already writing";
    if shutting_down <> None then
      failwith "#start_writing: already shutting down";
    if wrote_eof then
      failwith "#start_writing: already past EOF";
    dlogr
      (fun () ->
	 sprintf "FD %Ld: start_writing" fdi);
    let when_done exn_opt =
      dlogr
	(fun () ->
	   sprintf "FD %Ld: done start_writing: %s" 
	     fdi (string_of_exn_opt exn_opt)
	);
      when_done exn_opt in
    let cancel_flag = ref false in
    self # nonblock_operation
      cancel_flag
      `Writing
      (fun () ->
	 try
	   let n = Ssl_exts.single_write ssl_sock s pos len in
	   writing <- None;
	   (false, false, fun () -> when_done None n)
	 with
	   | Ssl.Write_error Ssl.Error_zero_return ->
	       (false, true, fun () -> ())
	   | Ssl.Write_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl.Write_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl.Write_error ssl_err ->
	       state <- `Unclean;
	       writing <- None;
	       (false, false, fun () -> when_done (Some (Ssl_error ssl_err)) 0)
	   | err ->
	       state <- `Unclean;
	       writing <- None;
	       (false, false, fun () -> when_done (Some err) 0)
      )
      (fun x -> self # cancel_writing_with x);
    writing <- Some (when_done, cancel_flag)


  method start_mem_writing ~when_done m pos len =
    raise Uq_engines.Mem_not_supported


  method start_writing_eof ~when_done () =
    if state <> `Client && state <> `Server && state <> `Unclean then
      failwith "#start_writing_eof: bad state";
    (* N.B. We accept here Unclean because there is still a chance that we
       can at least close the tunnel
     *)
    if writing <> None then
      failwith "#start_writing_eof: already writing";
    if shutting_down <> None then
      failwith "#start_writing_eof: already shutting down";
    if wrote_eof then
      Unixqueue.once esys group 0.0 (fun () -> when_done None)
    else (
      dlogr
	(fun () ->
	   sprintf "FD %Ld: start_writing_eof" fdi);
      let when_done exn_opt =
	dlogr
	  (fun () ->
	     sprintf "FD %Ld: done start_writing_eof: %s" 
	       fdi (string_of_exn_opt exn_opt)
	  );
	when_done exn_opt in
      let n = ref 0 in
      let cancel_flag = ref false in
      self # nonblock_operation
	cancel_flag
	`Writing_eof
	(fun () ->
	   try
	     let (_, sent_shutdown_0) =
	       Ssl_exts.get_shutdown ssl_sock in
	     
	     if not sent_shutdown_0 then
	       Ssl_exts.single_shutdown ssl_sock;

	     let (rcvd_shutdown, sent_shutdown) =
	       Ssl_exts.get_shutdown ssl_sock in

	     if rcvd_shutdown then
	       read_eof <- true;
	     if sent_shutdown then
	       wrote_eof <- true;

	     if !n=1 && not sent_shutdown then (
	       (* Unclean crash *)
	       writing_eof <- None;
	       state <- `Unclean;
	       (false, false, 
		fun () -> when_done(Some(Failure "Unclean SSL shutdown")))
	     )
	     else
	       match sent_shutdown with
		 | false ->
		     (* strange *)
		     (false, true, fun () -> ())
		 | true ->
		     writing_eof <- None;
		     (* The following is unnecessary according to the SSL 
			specs, but actually required by buggy implementations.
		      *)
		     ( try Unix.shutdown fd Unix.SHUTDOWN_SEND with _ -> ());
		     (false, false, fun () -> when_done None)
	   with
	     | Ssl_exts.Shutdown_error Ssl.Error_want_read ->
		 (true, false, fun () -> ())
	     | Ssl_exts.Shutdown_error Ssl.Error_want_write ->
		 (false, true, fun () -> ())
	     | Ssl_exts.Shutdown_error ssl_err ->
		 state <- `Unclean;
		 shutting_down <- None;
		 (false, false, fun () -> when_done (Some (Ssl_error ssl_err)))
	     | err ->
		 state <- `Unclean;
		 shutting_down <- None;
		 (false, false, fun () -> when_done (Some err))
	)
	(fun x -> self # cancel_writing_with x);
      writing_eof <- Some(when_done, cancel_flag)
    )
    

  method cancel_writing () =
    self # cancel_writing_with Uq_engines.Cancelled

  method private cancel_writing_with x =
    dlogr
      (fun () ->
	 sprintf "FD %Ld: cancel_writing" fdi);
    match writing with
      | Some (f_when_done, cancel_flag) ->
	  assert(not !cancel_flag);
	  self # cancel_operation `Writing;
	  cancel_flag := true;
	  writing <- None;
	  f_when_done (Some x) 0
      | None ->
	  ( match writing_eof with
	      | Some(f_when_done, cancel_flag) ->
		  assert(not !cancel_flag);
		  self # cancel_operation `Writing_eof;
		  cancel_flag := true;
		  writing_eof <- None;
		  f_when_done (Some x)
	      | None ->
		  ()
	  )


  method start_shutting_down ?(linger = 60.0) ~when_done () =
    if state <> `Client && state <> `Server && state <> `Unclean then
      failwith "#start_shutting_down: bad state";
    if reading <> None || writing <> None then
      failwith "#start_shutting_down: still reading or writing";
    if shutting_down <> None then
      failwith "#start_shutting_down: already shutting down";
    dlogr
      (fun () ->
	 sprintf "FD %Ld: start_shutting_down" fdi);
    let when_done exn_opt =
      dlogr
	(fun () ->
	   sprintf "FD %Ld: done start_shutting_down: %s" 
	     fdi (string_of_exn_opt exn_opt)
	);
      when_done exn_opt in
    let n = ref 0 in
    let cancel_flag = ref false in
    self # nonblock_operation
      cancel_flag
      `Shutting_down
      (fun () ->
	 try
	   Ssl_exts.single_shutdown ssl_sock;
	   incr n;

	   let (rcvd_shutdown, sent_shutdown) =
	     Ssl_exts.get_shutdown ssl_sock in
	   if rcvd_shutdown then
	     read_eof <- true;
	   if sent_shutdown then
	     wrote_eof <- true;

	   if !n=2 && not (rcvd_shutdown && sent_shutdown) then (
	     (* Unclean crash *)
	     shutting_down <- None;
	     state <- `Unclean;
	     if rcvd_shutdown || sent_shutdown then
	       (false, false, fun () -> when_done None)
	     else
	       (false, false, 
		fun () -> when_done(Some(Failure "Unclean SSL shutdown")))

	   )
	   else
	     match (rcvd_shutdown, sent_shutdown) with
	       | (false, false) ->
		   (* strange *)
		   (false, true, fun () -> ())
	       | (true, false) ->
		   (false, true, fun () -> ())
	       | (false, true) ->
		   (* The following is unnecessary according to the SSL 
		      specs, but actually required by buggy implementations.
		    *)
		   ( try Unix.shutdown fd Unix.SHUTDOWN_SEND with _ -> ());
		   (true, false, fun () -> ())
	       | (true, true) ->
		   shutting_down <- None;
		   state <- `Clean;
		   ( try Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ());
		   (false, false, fun () -> when_done None)
	 with
	   | Ssl_exts.Shutdown_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl_exts.Shutdown_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl_exts.Shutdown_error ssl_err ->
	       state <- `Unclean;
	       shutting_down <- None;
	       (false, false, fun () -> when_done (Some (Ssl_error ssl_err)))
	   | err ->
	       state <- `Unclean;
	       shutting_down <- None;
	       (false, false, fun () -> when_done (Some err))
      )
      (fun x -> self # cancel_shutting_down_with x);
    shutting_down <- Some(when_done, cancel_flag)

  method cancel_shutting_down () =
    self # cancel_shutting_down_with Uq_engines.Cancelled

  method private cancel_shutting_down_with x =
    dlogr
      (fun () ->
	 sprintf "FD %Ld: cancel_shutting_down" fdi);
    match shutting_down with
      | None ->
	  ()
      | Some (f_when_done, cancel_flag) ->
	  assert(not !cancel_flag);
	  self # cancel_operation `Shutting_down;
	  cancel_flag := true;
	  shutting_down <- None;
	  f_when_done (Some x)


  method private start_timer tag f_tmo =
    (* Call f_tmo when operation for tag times out *)
    match timeout with
      | None ->
	  ()
      | Some (tmo, x) ->
	  let tmo_g = Unixqueue.new_group esys in
	  Hashtbl.replace timers tag (tmo_g, f_tmo);
	  Unixqueue.once esys tmo_g tmo 
	    (fun () -> 
	       Hashtbl.remove timers tag;
	       f_tmo x
	    )
	  
  method private stop_timer tag =
    try
      let tmo_g, _ = Hashtbl.find timers tag in
      Unixqueue.clear esys tmo_g;
      Hashtbl.remove timers tag
    with Not_found -> ()

  method private restart_all_timers () =
    let ht = Hashtbl.copy timers in
    Hashtbl.clear timers;
    Hashtbl.iter
      (fun tag (tmo_g, f_tmo) ->
	 Unixqueue.clear esys tmo_g;
	 self # start_timer tag f_tmo
      )
      ht

  method private stop_all_timers() =
    Hashtbl.iter
      (fun tag (tmo_g, f_tmo) ->
	 Unixqueue.clear esys tmo_g;
      )
      timers;
    Hashtbl.clear timers;

  method private nonblock_operation cancel_flag tag f f_tmo =
    (* We use here min_float instead of 0.0 because the latter is handled
       in an optimized way in Unixqueue - and this gets here in the way.
       The optimization implies that it is normally not checked whether
       there are other socket events. However, we exactly want this here -
       so other events can be processed while we are doing our sequence
       of operations.
     *)
    Unixqueue.once
      esys
      group
      (* 0.0 *) min_float
      (fun () ->
	 if not !cancel_flag then (
	   dlogr
	     (fun () ->
		sprintf "FD %Ld: operation: %s" fdi (string_of_tag tag));
	   let (want_rd, want_wr, action) = f() in
	   dlogr
	     (fun () ->
		sprintf "FD %Ld: returning from %s - want_rd=%b want_wr=%b %s"
		  fdi (string_of_tag tag) want_rd want_wr
		  (if want_rd || want_wr then "- queuing op and retrying later"
		   else ""));
	   if want_rd || want_wr then (
	     self # start_timer tag f_tmo;
	     pending <- (tag, want_rd, want_wr, f) :: pending;
	   )
	   else
	     self # restart_all_timers();
	   ( try
	       action();
	       self # setup_queue();
	     with
	       | error ->
		   self # setup_queue(); raise error
	   )
	 )
      )


  method private cancel_operation tag =
    self # stop_timer tag;
    pending <-
      List.filter (fun (t, _, _, _) -> t <> tag) pending;
    self # setup_queue()


  method private retry_nonblock_operations can_read can_write =
    dlogr
      (fun () ->
	 sprintf "FD %Ld: retry_nonblock_operations" fdi);
    let cur_pending = pending in
    pending <- [];    (* maybe new operations are added! *)
    let actions = ref [] in
    let pending' =
      List.flatten
	(List.map
	   (fun (tag, want_rd, want_wr, f) ->
	      if (want_rd && can_read) || (want_wr && can_write)  then (
		dlogr
		  (fun () ->
		     sprintf "FD %Ld: retried operation: %s" 
		       fdi (string_of_tag tag));
		let (want_rd', want_wr', action) = f() in  (* must not fail! *)
		dlogr
		  (fun () ->
		     sprintf "FD %Ld: returning from %s - \
                              want_rd=%b want_wr=%b %s"
		       fdi (string_of_tag tag) want_rd' want_wr'
		       (if want_rd' || want_wr' then
			  "- queuing op and retrying later"
			else ""));
		actions := action :: !actions;
		if want_rd' || want_wr' then
		  [ tag, want_rd', want_wr', f ]   (* try again later *)
		else (
		  self # stop_timer tag;
		  []
		)
	      )		      
	      else
		[ tag, want_rd, want_wr, f ]   (* just keep *)
	   )
	   cur_pending
	) in
    pending <- pending @ pending';

    self # restart_all_timers();

    (* Be careful: We can only return the first error *)
    let first_error = ref None in
    List.iter
      (fun f ->
	 try f()
	 with
	   | e ->
	       ( match !first_error with
		   | None -> first_error := Some e
		   | Some _ ->
		       Netlog.logf `Crit
			 "Uq_ssl hidden exception: %s"
			 (Netexn.to_string e)
			 
	       )
      )
      (List.rev !actions);

    self # setup_queue();

    ( match !first_error with
	| None -> ()
	| Some e -> raise e
    )


  method private setup_queue() =
    if alive then (
      let expecting_input' = 
	List.exists (fun (_, want_rd, _, _) -> want_rd) pending in
      let expecting_output' =
	List.exists (fun (_, _, want_wr, _) -> want_wr) pending in
      
      if expecting_input' || expecting_output' then (
	if not have_handler then (
	  Unixqueue.add_handler esys group (fun _ _ -> self # handle_event);
	  have_handler <- true;
	);
	disconnecting <- None;
      )
      else
	if have_handler && disconnecting = None then (
	  (* It makes only sense to disconnect if all callbacks are cancelled *)
	  if not(accepting || connecting || reading <> None ||
		   writing <> None || shutting_down <> None) then (
	    let wid = Unixqueue.new_wait_id esys in
	    let disconnector = Unixqueue.Wait wid in
	    Unixqueue.add_event esys (Unixqueue.Timeout(group,disconnector));
	    disconnecting <- Some disconnector
	  )
	);
      
      ( match expecting_input, expecting_input' with
	  | (false, true) ->
	      Unixqueue.add_resource esys group (Unixqueue.Wait_in fd, (-1.0))
	  | (true, false) ->
	      Unixqueue.remove_resource esys group (Unixqueue.Wait_in fd)
	  | _ ->
	      ()
      );
	
      ( match expecting_output, expecting_output' with
	  | (false, true) ->
	      Unixqueue.add_resource esys group (Unixqueue.Wait_out fd, (-1.0))
	  | (true, false) ->
	      Unixqueue.remove_resource esys group (Unixqueue.Wait_out fd)
	  | _ ->
	      ()
      );

      expecting_input  <- expecting_input';
      expecting_output <- expecting_output';
    )


  method private handle_event ev =
    match ev with
      | Unixqueue.Input_arrived(g, _) when g = group ->
	  self # retry_nonblock_operations true false

      | Unixqueue.Output_readiness(g, _) when g = group ->
	  self # retry_nonblock_operations false true

      | Unixqueue.Timeout (g, op) when g = group ->
	  ( match disconnecting with
	      | Some op' when op = op' ->
		  disconnecting <- None;
		  have_handler <- false;
		  raise Equeue.Terminate

	      | _ -> raise Equeue.Reject
		  (* Can also be a timeout event from a "once" handler *)
	  )

      | _ ->
	  raise Equeue.Reject


  method inactivate() =
    if alive then (
      alive <- false;
      self # inactivate_no_close();
      if close_inactive_descr then (
	preclose();
	Unix.close fd
      )
    )

  method inactivate_no_close() =
    pending <- [];
    disconnecting <- None;
    have_handler <- false;
    Unixqueue.clear esys group;
    self # stop_all_timers();

  method event_system = esys

end
;;


let create_ssl_multiplex_controller
       ?close_inactive_descr ?preclose ?initial_state ?timeout fd ctx esys =
  let () = Unix.set_nonblock fd in
  let s = Ssl.embed_socket fd ctx in
  let m = Ssl_exts.get_mode s in
  let () = Ssl_exts.set_mode s 
    { m with
	Ssl_exts.enable_partial_write = true; 
	accept_moving_write_buffer = true } in
  new ssl_mplex_ctrl ?close_inactive_descr ?preclose ?initial_state ?timeout
    fd s esys
;;


class ssl_connect_engine (mplex : ssl_multiplex_controller) =
object(self)
  inherit [ unit ] Uq_engines.engine_mixin (`Working 0) mplex#event_system

  initializer
    mplex # start_ssl_connecting
      ~when_done:(fun exn_opt ->
		    match exn_opt with
		      | None ->
			  self # set_state (`Done())
		      | Some err ->
			  self # set_state (`Error err)
		 )
      ()

  method event_system = mplex # event_system

  method abort() =
    match self#state with
      | `Working _ ->
	  mplex # inactivate_no_close();
	  self # set_state `Aborted
      | _ ->
	  ()

end


let ssl_connect_engine = new ssl_connect_engine


class ssl_accept_engine (mplex : ssl_multiplex_controller) =
object(self)
  inherit [ unit ] Uq_engines.engine_mixin (`Working 0) mplex#event_system

  initializer
    mplex # start_ssl_accepting
      ~when_done:(fun exn_opt ->
		    match exn_opt with
		      | None ->
			  self # set_state (`Done())
		      | Some err ->
			  self # set_state (`Error err)
		 )
      ()

  method event_system = mplex # event_system

  method abort() =
    match self#state with
      | `Working _ ->
	  mplex # inactivate_no_close();
	  self # set_state `Aborted
      | _ ->
	  ()

end


let ssl_accept_engine = new ssl_accept_engine
