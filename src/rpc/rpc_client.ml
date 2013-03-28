(* $Id: rpc_client.ml 1676 2011-10-11 10:56:36Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


open Rtypes
open Xdr
open Rpc
open Rpc_common
open Rpc_packer
open Unixqueue
open Printf

exception Message_not_processable


exception Message_lost
exception Message_timeout
exception Response_dropped
exception Communication_error of exn
exception Client_is_down
exception Keep_call
exception Unbound_exception of exn

module type USE_CLIENT = sig
  type t
  val use : t -> Rpc_program.t -> unit
  val unbound_sync_call : 
        t -> Rpc_program.t -> string -> xdr_value -> xdr_value
  val unbound_async_call :
        t -> Rpc_program.t -> string -> xdr_value -> 
        ((unit -> xdr_value) -> unit) -> unit
end

let () =
  Netexn.register_printer
    (Communication_error Not_found)
    (fun e ->
       match e with
	 | Communication_error e' ->
	     "Rpc_client.Communication_error(" ^ Netexn.to_string e' ^ ")"
	 | _ -> assert false
    );
  Netexn.register_printer
    (Unbound_exception Not_found)
    (fun e ->
       match e with
	 | Unbound_exception e' ->
	     "Rpc_client.Unbound_exception(" ^ Netexn.to_string e' ^ ")"
	 | _ -> assert false
    )



module SessionUint4 = struct
  type t = uint4
  let compare = (Pervasives.compare : uint4 -> uint4 -> int)
end

module SessionMap =
  Map.Make(SessionUint4)


type call_state =
  | Delayed    (* call waits for authentication protocol *)
  | Waiting    (* call has not yet been sent *)
  | Pending    (* call has been sent, still no answer *)
  | Done       (* got answer for the call *)

(* Normally, the state of the call is changed from Waiting to Pending to
 * Done.
 * In the case of a retransmission, the call is added to the waiting calls
 * again but its state remains 'Pending' (because the call is still member
 * of the set of pending calls).
 *)


(* The following class types are only preliminary definitions. The type
 * parameter 't is later instantiated with the type of the clients, t.
 *)

type reject_code =
    [ `Fail | `Retry | `Renew | `Next ]

class type ['t] pre_auth_session =
object
  method next_credentials :
     't -> Rpc_program.t -> string -> uint4 ->
     (string * string * string * string * Xdr.encoder option * 
	Xdr.decoder option)
  method server_rejects : 't -> uint4 -> server_error -> reject_code
  method server_accepts : 't -> uint4 -> string -> string -> unit
(*
  method drop_xid : 't -> uint4 -> unit
  method drop_client : 't -> unit
 *)
  method auth_protocol : 't pre_auth_protocol
end


and ['t] pre_auth_protocol =
object
  method state :
         [ `Emit | `Receive of uint4 | `Done of 't pre_auth_session | `Error]
  method emit : uint4 -> uint4 -> uint4 -> Rpc_packer.packed_value
  method receive : Rpc_packer.packed_value -> unit
  method auth_method : 't pre_auth_method
end


and ['t] pre_auth_method =
object
  method name : string
  method new_session : 't -> string option -> 't pre_auth_protocol
end


type regular_call =
    { mutable prog : Rpc_program.t;
      mutable proc : string;
      mutable param : xdr_value;       (* the argument of the call *)
      mutable get_result : (unit -> xdr_value) -> unit;
      mutable decoder : Xdr.decoder option;
      mutable call_user_name : string option;
    }

type call_detail =
    [ `Regular of regular_call
    | `Auth_proto of Rpc_program.t
    ]

type call =
      { mutable detail : call_detail;
	mutable state : call_state;
	mutable retrans_count : int;        (* retransmission counter *)
	mutable xid : uint4;
	mutable destination : Unix.sockaddr option;

	mutable call_timeout : float;
	mutable timeout_group : group option;
	  (* If a timeout handler has been set, this is the corresponding group *)

	mutable call_auth_proto : t pre_auth_protocol;
	  (* calls store the authentication protocol *)

	mutable batch_flag : bool;
	
	mutable request : packed_value option;
	(* The request while the call is waiting *)
      }

and t =
      { mutable ready : bool;
	mutable nolog : bool;

        mutable trans : Rpc_transport.rpc_multiplex_controller option;
	mutable progs :  Rpc_program.t list;
	mutable prot :  protocol;
        mutable esys :  event_system;
	
	mutable est_engine : Rpc_transport.rpc_multiplex_controller Uq_engines.engine option;
	mutable shutdown_connector : t -> Rpc_transport.rpc_multiplex_controller -> (unit->unit) -> unit;

	mutable delayed_calls : (t pre_auth_protocol,call Queue.t) Hashtbl.t;
	  (* delayed: the request cannot be sent - because the authentication
	     protocol is not yet done
	   *)
	mutable waiting_calls : call Queue.t;
	  (* waiting: the request can be sent when the connections allows it *)
	mutable pending_calls : call SessionMap.t;
	  (* pending: the request is sent; waiting for the reply *)

	mutable next_xid : uint4;
	mutable used_xids : unit SessionMap.t;
	mutable last_replier : Unix.sockaddr option;
	mutable last_xid : uint4 option;

	(* configs: *)
	mutable timeout : float;
        mutable max_retransmissions : int;
	mutable next_timeout : float;
	mutable next_max_retransmissions : int;
	mutable next_destination : Unix.sockaddr option;
	mutable next_batch_flag : bool;
	mutable max_resp_length : int option;
	mutable user_name : string option;
	mutable mstring_factories : Xdr_mstring.named_mstring_factories;

	(* authentication: *)
	mutable all_auth_methods : t pre_auth_method list;
	mutable auth_methods : t pre_auth_method list;     
	   (* remaining methods to try *)
	mutable auth_current : (string option,t pre_auth_protocol) Hashtbl.t;
	   (* The protocol used for this user *)

	mutable exception_handler : exn -> unit;
      }

and connector =
    Inet of (string * int)                        (* Hostname, port *)
  | Internet of (Unix.inet_addr * int)
  | Unix of string                                (* path to unix dom sock *)
  | W32_pipe of string
  | Descriptor of Unix.file_descr
  | Dynamic_descriptor of (unit -> Unix.file_descr)
  | Portmapped of string

class type auth_session = [t] pre_auth_session
class type auth_method = [t] pre_auth_method
class type auth_protocol = [t] pre_auth_protocol


let auth_none_session p : auth_session =
object
  method next_credentials _ _ _ _ =
    ("AUTH_NONE", "", "AUTH_NONE", "", None, None)
  method server_rejects _ _ _ = `Next
  method server_accepts _ _ _ _ = ()
  method auth_protocol = p
end


let auth_none_proto m : auth_protocol =
  let session = ref None in
object(self)
  initializer
    session := Some(auth_none_session self)
  method state =
    match !session with
      | None -> assert false
      | Some s -> `Done s
  method emit _ _ _ = assert false
  method receive _ = assert false
  method auth_method = m
end

let auth_none =
object(self)
  method name = "AUTH_NONE"
  method new_session _ _ =
    auth_none_proto self
end

module Debug = struct
  let enable = ref false
  let enable_ptrace = ref false
  let ptrace_verbosity = ref `Name_abbrev_args
  let disable_for_client c = c.nolog <- true
end

let dlog0 = Netlog.Debug.mk_dlog "Rpc_client" Debug.enable
let dlogr0 = Netlog.Debug.mk_dlogr "Rpc_client" Debug.enable

let dlog cl msg =
  if not cl.nolog then dlog0 msg

let dlogr cl getmsg =
  if not cl.nolog then dlogr0 getmsg

let dlog0_ptrace = Netlog.Debug.mk_dlog "Rpc_client.Ptrace" Debug.enable_ptrace
let dlogr0_ptrace = Netlog.Debug.mk_dlogr "Rpc_client.Ptrace" Debug.enable_ptrace

let dlog_ptrace cl msg =
  if not cl.nolog then dlog0_ptrace msg

let dlogr_ptrace cl getmsg =
  if not cl.nolog then dlogr0_ptrace getmsg


let () =
  Netlog.Debug.register_module "Rpc_client" Debug.enable;
  Netlog.Debug.register_module "Rpc_client.Ptrace" Debug.enable_ptrace


let connector_of_sockaddr =
  function
    | Unix.ADDR_INET(ip,p) ->
	Internet(ip,p)
    | Unix.ADDR_UNIX s ->
	Unix s


let connector_of_socksymbol =
  function
    | `Inet(ip,p) -> Internet(ip,p)
    | `Inet_byname(n,p) -> Inet(n,p)
    | `Unix p -> Unix p


  (*****)

let set_auth_methods cl list =
  match list with
    | _ :: _ ->
	Hashtbl.clear cl.auth_current;
	cl.auth_methods <- list;
	cl.all_auth_methods <- list;
    | [] ->
	invalid_arg "Rpc_client.set_auth_methods"

  (*****)

let stop_retransmission_timer cl call =
  match call.timeout_group with
    | None -> ()
    | Some g ->
	Unixqueue.clear cl.esys g

  (*****)

let pass_result cl call f =
  (* for regular calls only! *)

  (* Stop the timer, if any : *)

  stop_retransmission_timer cl call;

  (* Change the state of the call to 'Done': *)

  call.state <- Done;

  (* pass 'f' to the call back function: *)

  try
    dlog cl "Calling back";
    ( match call.detail with
	| `Regular rc ->
	    rc.get_result f;
	| _ ->
	    assert false
    );
    dlog cl "Returned from callback";
  with
    | Keep_call as x ->
	dlog cl "Keep_call";
	raise x
    | Unbound_exception x ->
	dlog cl "Unbound_exception";
	raise x
    | any ->
	begin  (* pass the exception to the exception handler: *)
	  dlogr cl (fun () -> 
		      "Exception from callback: " ^ Netexn.to_string any);
	  cl.exception_handler any
	end


let pass_exception ?(skip_auth=false) cl call x =
  (* Caution! This function does not remove [call] from the set of pending
   * calls.
   * 
   * For authproto messages, the exception is passed to the connected calls
   * instead.
   *)
  if call.state <> Done then (  (* Don't call back twice *)
    try
      dlogr cl
	(fun () ->
	   let sx = Netexn.to_string x in
	   "Passing exception " ^ sx);
      ( match call.detail with
	  | `Regular _ ->
	      pass_result cl call (fun () -> raise x)
	  | `Auth_proto prog ->
	      if not skip_auth then (
		stop_retransmission_timer cl call;
		call.state <- Done;
		let q =
		  try Hashtbl.find cl.delayed_calls call.call_auth_proto
		  with Not_found -> Queue.create() in
		Hashtbl.remove cl.delayed_calls call.call_auth_proto;
		Queue.iter
		  (fun d_call ->
		     pass_result cl d_call (fun () -> raise x)
		  )
		  q
	      )
      )
    with
      | Keep_call -> ()          (* ignore *)
  )

let pass_exception_to_all cl x =
  (* Caution! This function does not erase the set of pending calls.  *)
  dlog cl "Passing exception to all";

  let ht = Hashtbl.create 17 in
  let fn_list = ref [] in
  let add_fn xid call =
    if not (Hashtbl.mem ht xid) then (
      Hashtbl.add ht xid ();
      fn_list := call :: !fn_list
    )
  in

  SessionMap.iter (fun xid call -> add_fn xid call)  cl.pending_calls;
  Queue.iter      (fun call -> add_fn call.xid call) cl.waiting_calls;
  Hashtbl.iter
    (fun auth_proto q ->
       Queue.iter
	 (fun call -> add_fn call.xid call)
	 q
    )
    cl.delayed_calls;

  (* We already included delayed_calls, hence set skip_auth here *)
  List.iter (fun call -> pass_exception ~skip_auth:true cl call x) !fn_list

  (*****)

let close ?error ?(ondown=fun()->()) cl =
  if cl.ready then (
    dlog cl "Closing";
    cl.ready <- false;
    ( match error with
	| None -> pass_exception_to_all cl Message_lost
	| Some e -> pass_exception_to_all cl e
    );
    cl.pending_calls <- SessionMap.empty;
    cl.used_xids <- SessionMap.empty;
    Queue.clear cl.waiting_calls;
    Hashtbl.clear cl.delayed_calls;
    match cl.trans with
      | None -> 
	  ondown()
      | Some trans ->
	  cl.trans <- None;
	  cl.shutdown_connector cl trans ondown
  )
  else
    ondown()
;;

  (*****)

let check_for_input =  (* "forward declaration" *)
  ref (fun _ -> ());;

let check_for_output = (* "forward declaration" *)
  ref (fun _ -> ());;

  (*****)

let find_or_make_auth_protocol cl user_opt =
  try
    Hashtbl.find cl.auth_current user_opt
  with
    | Not_found ->
	let current_auth_method = 
	  match cl.auth_methods with
	    | hd :: _ -> hd
	    | [] -> auth_none in
	let p = 
	  current_auth_method # new_session cl user_opt in
	Hashtbl.add cl.auth_current user_opt p;
	p
;;

  (*****)

let rec next_xid cl =
  if SessionMap.mem cl.next_xid cl.used_xids then
    next_xid cl
  else (
    let xid = cl.next_xid in
    (* xid is uint4, so we increment as int64: *)
    let xid64 = Rtypes.int64_of_uint4 xid in
    let xid64' = Int64.logand (Int64.succ xid64) 0xffff_ffff_L in
    cl.next_xid <- Rtypes.uint4_of_int64 xid64';
    xid
  )

  (*****)

let remove_pending_call cl call =
  cl.pending_calls <- SessionMap.remove call.xid cl.pending_calls;
  cl.used_xids <- SessionMap.remove call.xid cl.used_xids;
  stop_retransmission_timer cl call
;;


let abandon_call cl xid =
  try
    let call = SessionMap.find xid cl.pending_calls in
    remove_pending_call cl call
  with
    | Not_found ->
	let q = Queue.create() in
	Queue.transfer cl.waiting_calls q;
	while not(Queue.is_empty q) do
	  let call = Queue.take q in
	  if call.xid <> xid then Queue.add call cl.waiting_calls
	done;
	cl.used_xids <- SessionMap.remove xid cl.used_xids


  (*****)

let continue_call cl call =
  (* Called when the authentication protocol finishes, and [call] can now
     be added to the [waiting] queue.

     Also called for retransmitted regular requests.
   *)
  let authsess =
    match call.call_auth_proto # state with
      | `Done authsess -> authsess
      | _ -> assert false in
  let rc =
    match call.detail with
      | `Regular rc -> rc
      | _ -> assert false in
  
  call.state <- Waiting;

  let (cred_flav, cred_data, verf_flav, verf_data, enc_opt, dec_opt) =
    authsess # next_credentials 
      cl rc.prog rc.proc call.xid in
  rc.decoder <- dec_opt;
  let request =
    Rpc_packer.pack_call
      ?encoder:enc_opt
      rc.prog
      call.xid
      rc.proc
      cred_flav cred_data verf_flav verf_data
      rc.param in
  call.request <- Some request;

  Queue.add call cl.waiting_calls


(* FIXME: Retransmissions are not yet perfect. We allow here that calls time
   out while the request is still in the Waiting queue (especially TCP
   connect timeouts). If we retransmit with a new encryption key, we will
   send the same request twice ( and severs might not like this).

   Better: do not modify [call] in place, but create a copy.

   Also, it may happen that we get a response to attempt #1 while we already
   dropped it and pushed attempt #2 to Waiting. Now, the response #1 may
   turn out to use a different encryption key, and we cannot decode it.

   Better: the pending table should map xid to a list of possible calls.
   (Still to check how we find then the right one...)
 *)

let retransmit cl call =
  if call.state = Pending || call.state = Waiting then begin
    if call.retrans_count > 0 then begin
      dlog cl "Retransmitting";
      let old_state = call.state in
      (* Make the 'call' waiting again *)
      ( match call.detail with
	  | `Regular _ -> continue_call cl call
	  | `Auth_proto _ -> Queue.add call cl.waiting_calls
      );
      cl.used_xids <- SessionMap.add call.xid () cl.used_xids;
      (* Decrease the retransmission counter *)
      call.retrans_count <- call.retrans_count - 1;
      (* Ensure the call keeps its state: *)
      call.state <- old_state;
      (* Check state of reources: *)
      !check_for_output cl
      (* Note: The [call] remains in state [Pending] (if it is already). 
       * This prevents the [call]
       * from being added to [cl.pending_calls] again.
       *
       * If the [call] is encrypted, it is possible that the client only
       * accepts the response to the second trial, and rejects a late
       * response for the first request, because the encryption code
       * may have changed in the meantime.
       *)
    end
    else begin
      (* still no answer after maximum number of retransmissions *)
      dlog cl "Call timed out!";
      remove_pending_call cl call;
      (* Note that we do not remove the call from waiting_calls for
         performance reasons. We simply skip it there if we find it.
         pass_exception will set call.state to Done.
       *)
      pass_exception cl call Message_timeout;
      (* FIXME: do something with the delayed calls if [call] is 
	 an authproto message
       *)
      (* If we still try to connect the TCP socket, shut the client
         completely down:
       *)
      ( match cl.est_engine with
	  | None -> ()
	  | Some e -> 
	      e#abort();
	      close cl;
      );
      (* Check state of reources: *)
      !check_for_output cl
    end
  end


  (*****)

(* Note: For asynchronous authentication, it would be sufficient that
 * add_call (and add_call_again) are rewritten such that they first
 * schedule the authentication request, and when the request is replied,
 * the call is scheduled.
 *)

let set_timeout cl call =
  if call.call_timeout > 0.0 && call.timeout_group = None then (
    (* Note: Case call_timeout = 0.0 is handled elsewhere *)
    (* CHECK: What happens when the timeout comes before the message
     * is fully written? (Low priority because for stream connections
     * a timeout is usually not set.)
     *)
    let g = new_group cl.esys in
    Unixqueue.once cl.esys g call.call_timeout
      (fun () ->
	 call.timeout_group <- None;
	 dlog cl "Timeout handler";
	 retransmit cl call;
	 (* Maybe we have to cancel reading: *)
	 !check_for_input cl
      );
    call.timeout_group <- Some g
  )


let auth_proto_emit cl prog authproto =
  (* Emit an authproto message *)
  let xid = next_xid cl in
  let prog_nr = Rpc_program.program_number prog in
  let vers_nr = Rpc_program.version_number prog in

  let request = authproto # emit xid prog_nr vers_nr in

  (* THINK: maybe we want to set the call_timeout and the retrans_count
     separately for authproto messages (instead of just taking over the
     values for the regular call)
   *)
  let call =
    { detail = `Auth_proto prog;
      state = Waiting;
      retrans_count = cl.next_max_retransmissions;
      xid = xid;
      destination = cl.next_destination;
      call_timeout = cl.next_timeout;
      timeout_group = None;
      call_auth_proto = authproto;
      batch_flag = false;
      request = Some request;
    } in
  
  Queue.add call cl.waiting_calls;
  cl.used_xids <- SessionMap.add call.xid () cl.used_xids;
  
  (* For TCP and timeout > 0.0 set the timeout handler immediately, so the
     timeout includes connecting
   *)
  if cl.prot = Rpc.Tcp && call.call_timeout > 0.0
  then
    set_timeout cl call


let unbound_async_call_r cl prog procname param receiver authsess_opt =
  (* authsess_opt: if passed, this session is used *)
  if not cl.ready then
    raise Client_is_down;
  if cl.progs <> [] then (
    let prog_id = Rpc_program.id prog in
    if not (List.exists (fun p -> Rpc_program.id p = prog_id) cl.progs) then
      failwith "Rpc_client.unbound_async_call: \
                This client is not bound to the requested program"
  );

  let (_, _, out_type) =
    try Rpc_program.signature prog procname 
    with Not_found ->
      failwith ("Rpc_client.unbound_async_call: No such procedure: " ^
		  procname) in

  if cl.next_batch_flag && Xdr.xdr_type_term out_type <> X_void then
    failwith ("Rpc_client.unbound_async_call: Cannot call in batch mode: " ^
		procname);

  let rc =
    { prog = prog;
      proc = procname;
      param = param;
      get_result = receiver;
      decoder = None;
      call_user_name = cl.user_name;
    } in

  let eff_authsess_opt, eff_authproto =
    match authsess_opt with
      | None ->
	  let authproto = find_or_make_auth_protocol cl cl.user_name in
	  ( match authproto#state with
	      | `Done authsess ->
		  (Some authsess, authproto)
	      | _ ->
		  (None, authproto)
	  )
      | Some authsess ->
	  (Some authsess, authsess#auth_protocol) in

  let new_call =
    match eff_authsess_opt with
      | Some authsess ->
	  let xid = next_xid cl in
	  let (cred_flav, cred_data, verf_flav, verf_data, enc_opt, dec_opt) =
	    authsess # next_credentials 
	      cl prog procname xid in
	  rc.decoder <- dec_opt;
	  let request =
	    Rpc_packer.pack_call
	      ?encoder:enc_opt
	      prog
	      xid
	      procname
	      cred_flav cred_data verf_flav verf_data
	      param in
	  let call =
	    { detail = `Regular rc;
	      state = Waiting;
	      retrans_count = cl.next_max_retransmissions;
	      xid = xid;
	      destination = cl.next_destination;
	      call_timeout = cl.next_timeout;
	      timeout_group = None;
	      call_auth_proto = eff_authproto;
	      batch_flag = cl.next_batch_flag;
	      request = Some request;
	    } in
	  Queue.add call cl.waiting_calls;
	  call
	  
      | None ->
	  dlog cl "starting authentication protocol";
	  ( match eff_authproto#state with
	      | `Done _ -> 
		  assert false

	      | `Error ->
		  assert false

	      | ap_state ->
		  (* Authentication not yet ready. *)
		  if ap_state = `Emit then (
		    dlog cl "emitting new authentication token";
		    auth_proto_emit cl rc.prog eff_authproto;
		  );

		  let xid = next_xid cl in
		  let call =
		    { detail = `Regular rc;
		      state = Delayed;
		      retrans_count = cl.next_max_retransmissions;
		      xid = xid;
		      destination = cl.next_destination;
		      call_timeout = cl.next_timeout;
		      timeout_group = None;
		      call_auth_proto = eff_authproto;
		      batch_flag = cl.next_batch_flag;
		      request = None;
		    } in
		  let q =
		    try Hashtbl.find cl.delayed_calls eff_authproto
		    with Not_found -> Queue.create() in
		  Queue.add call q;
		  Hashtbl.replace cl.delayed_calls eff_authproto q;
		  call
	  )
  in

  cl.last_xid <- Some new_call.xid;
  cl.used_xids <- SessionMap.add new_call.xid () cl.used_xids;
  cl.next_timeout <- cl.timeout;
  cl.next_max_retransmissions <- cl.max_retransmissions;
  cl.next_batch_flag <- false;
  (* We keep next_destination, as required by the API. *)

  (* For TCP and timeout > 0.0 set the timeout handler immediately, so the
     timeout includes connecting
   *)
  if cl.prot = Rpc.Tcp && new_call.state = Waiting && 
     new_call.call_timeout > 0.0
  then
    set_timeout cl new_call;

  !check_for_output cl;

  new_call


let unbound_async_call cl prog procname param receiver =
  ignore(unbound_async_call_r cl prog procname param receiver None)


  (*****)


type 'a threeway =
  | Value of 'a
  | Novalue
  | Error of exn


let call_auth_session call =
  match call.call_auth_proto # state with
    | `Done s -> s
    | _ -> assert false  


let string_of_reject_code =
  function
    | `Fail -> "Fail"
    | `Retry -> "Retry"
    | `Renew -> "Renew"
    | `Next -> "Next"


let process_regular_incoming_message cl message peer sock call rc =
  (* Exceptions in the following block are forwarded to the callback
   * function
   *)
  let auth_sess = call_auth_session call in
  dlogr cl
    (fun () ->
       sprintf "process_regular_incoming_message auth_meth=%s have_decoder=%B"
	 auth_sess#auth_protocol#auth_method#name
	 (rc.decoder <> None)
    );
  let result_opt =
    try
      ( match Rpc_packer.peek_auth_error message with
	  | None ->
	      let (xid,verf_flavour,verf_data,response) =
		Rpc_packer.unpack_reply 
		  ~mstring_factories:cl.mstring_factories
		  ?decoder:rc.decoder
		  rc.prog rc.proc message
		  (* may raise an exception *)
              in
	      auth_sess # server_accepts cl call.xid verf_flavour verf_data;
	      Value response

	  | Some auth_problem ->
	      let code = 
		auth_sess # server_rejects cl call.xid auth_problem in
	      dlogr_ptrace cl
		(fun () ->
		   sprintf
		     "RPC <-- (sock=%s,peer=%s,xid=%Ld) Auth error %s - reaction: %s"
		     (Rpc_transport.string_of_sockaddr sock)
		     (Rpc_transport.string_of_sockaddr peer)
		     (Rtypes.int64_of_uint4 call.xid)
		     (Rpc.string_of_server_error auth_problem)
		     (string_of_reject_code code)
		);
	      remove_pending_call cl call;
	      ( match code with
		  | `Fail ->
		      Error(Rpc.Rpc_server auth_problem)
		  | `Retry ->
		      ignore(
			unbound_async_call_r 
			  cl rc.prog rc.proc rc.param rc.get_result
		          (Some auth_sess));
		      Novalue
		  | `Renew ->
		      (* FIXME: When we send several requests in sequence,
			 we may get here several `Renew codes. That should
			 be merged to a single renewal.
		       *)
		      Hashtbl.remove cl.auth_current rc.call_user_name;
		      unbound_async_call 
			cl rc.prog rc.proc rc.param rc.get_result;
		      Novalue
		  | `Next ->
		      let m = call.call_auth_proto # auth_method in
		      ( match cl.auth_methods with
			  | m0 :: _ :: _ when m0 = m ->
			      cl.auth_methods <- List.tl cl.auth_methods;
			      Hashtbl.remove 
				cl.auth_current rc.call_user_name;
			      (* FIXME: see `Renew *)
			  | _ ->
			      raise(Rpc.Rpc_server Auth_too_weak)
		      );
		      unbound_async_call 
			cl rc.prog rc.proc rc.param rc.get_result;
		      Novalue
	      )
      )
    with
	error ->
	  (* The call_auth_session is simply dropped. *)
	  (* Forward the exception [error] to the caller: *)
	  remove_pending_call cl call;
	  Error error
  in

  match result_opt with
    | Novalue ->
	(* There is no result yet *)
	()

    | Value result ->
	
        (* pass result to the user *)
	
	( try
	    dlogr_ptrace cl
	      (fun () ->
		 sprintf
		   "RPC <-- (sock=%s,peer=%s,xid=%Ld) %s"
		   (Rpc_transport.string_of_sockaddr sock)
		   (Rpc_transport.string_of_sockaddr peer)
		   (Rtypes.int64_of_uint4 call.xid)
		   (Rpc_util.string_of_response
		      !Debug.ptrace_verbosity
		      rc.prog
		      rc.proc
		      result
		   )
	      );
	    let f = (fun () -> result) in
	    pass_result cl call f;      (* may raise Keep_call *)
	    (* Side effect: Changes the state of [call] to [Done] *)
	    remove_pending_call cl call;
	  with
	      Keep_call ->
		call.state <- Pending
	)

    | Error error ->
	( try
	    dlogr_ptrace cl
	      (fun () ->
		 sprintf
		   "RPC <-- (sock=%s,peer=%s,xid=%Ld) Error %s"
		   (Rpc_transport.string_of_sockaddr sock)
		   (Rpc_transport.string_of_sockaddr peer)
		   (Rtypes.int64_of_uint4 call.xid)
		   (Netexn.to_string error)
	      );
	    let f = (fun () -> raise error) in
	    pass_result cl call f;      (* may raise Keep_call *)
	    (* Side effect: Changes the state of [call] to [Done] *)
	    remove_pending_call cl call;
	  with
	      Keep_call ->
		call.state <- Pending
	)


let process_incoming_message cl message peer =

  let sock = 
    match cl.trans with
      | None -> `Implied
      | Some t -> t#getsockname in

  (* Got a 'message' for which the corresponding 'call' must be searched: *)

  let xid = Rpc_packer.peek_xid message in

  let call =
    try
      SessionMap.find xid cl.pending_calls
    with
	Not_found ->
	  (* Strange: Got a message with a session ID that is not pending.
	   * We assume that this is an answer of a very old message that
	   * has been completely timed out.
	   *)
	  raise Message_not_processable
  in
  assert(call.state = Pending);
  
  match call.detail with
    | `Auth_proto prog ->
	dlog cl "continuing authentication protocol";
	( match call.call_auth_proto#state with
	    | `Receive expected_xid when expected_xid = xid ->
		remove_pending_call cl call;

		let err_opt =
		  try
		    dlog cl "receiving authentication token";
		    call.call_auth_proto#receive message; None
		  with error -> Some error in

		let err_opt' =
		  match call.call_auth_proto#state with
		    | `Emit ->
			(* Emit the next message of the protocol: *)
			assert(err_opt = None);
			( try
			    auth_proto_emit cl prog call.call_auth_proto;
			    None
			  with
			    | error -> Some error
			)
		    | `Done authsess ->
			(* We are done - so activate all delayed messages *)
			assert(err_opt = None);
			dlog cl "authentication protocol is done";
			let q =
			  try Hashtbl.find cl.delayed_calls call.call_auth_proto
			  with Not_found -> Queue.create() in
			Hashtbl.remove cl.delayed_calls call.call_auth_proto;
			Queue.iter
			  (fun d_call ->
			     continue_call cl d_call
			  )
			  q;
			None
		    | `Receive _ ->
			assert false
		    | `Error ->
			assert(err_opt <> None);
			err_opt  in

		( match err_opt' with
		    | None ->
			!check_for_output cl
		    | Some err ->
			(* Failed authentication! *)
			let q =
			  try Hashtbl.find cl.delayed_calls call.call_auth_proto
			  with Not_found -> Queue.create() in
			Hashtbl.remove cl.delayed_calls call.call_auth_proto;
			Queue.iter
			  (fun d_call ->
			     pass_exception cl d_call err
			  )
			  q
		)

	    | _ ->
		(* Unexpected messages. We just drop these - assuming these
		   are duplicates
		 *)
		raise Message_not_processable
	)

    | `Regular rc ->
	process_regular_incoming_message cl message peer sock call rc


let drop_response cl message peer =
  let sock = 
    match cl.trans with
      | None -> `Implied
      | Some t -> t#getsockname in
  let xid = Rpc_packer.peek_xid message in
  let call =
    try
      SessionMap.find xid cl.pending_calls
    with
	Not_found ->
	  raise Message_not_processable in
  assert(call.state = Pending);

  dlogr_ptrace cl
    (fun () ->
       sprintf
	 "RPC <-- (sock=%s,peer=%s) Dropping response"
	 (Rpc_transport.string_of_sockaddr sock)
	 (Rpc_transport.string_of_sockaddr peer)
    );
  
  try
    let f = (fun () -> raise Response_dropped) in
    pass_result cl call f;      (* may raise Keep_call *)
    (* Side effect: Changes the state of [call] to [Done] *)
    remove_pending_call cl call;
  with
      Keep_call ->
	call.state <- Pending

  (*****)

let rec handle_incoming_message cl r =
  (* Called when a complete message has been read by the transporter *)
  match r with
    | `Error e ->
	close ~error:(Communication_error e) cl

    | `Ok(msg,addr) ->
	dlog cl "Message arrived";
	( try
	    ( match addr with
		| `Implied -> ()
		| `Sockaddr a ->
		    cl.last_replier <- Some a
	    );
	    ( match msg with
		| `Accept pv ->
		    process_incoming_message cl pv addr
		| `Reject pv ->
		    drop_response cl pv addr
		| _ ->
		    assert false
	    )
	  with
	      Message_not_processable ->
		dlog cl "message not processable";
		()
	);
	(next_incoming_message cl : unit)

    | `End_of_file ->
	dlog cl "End of file";
	close cl

and next_incoming_message cl =
  match cl.trans with
    | None -> ()
    | Some trans -> next_incoming_message' cl trans

and next_incoming_message' cl trans =
  trans # cancel_rd_polling();
  if cl.pending_calls <> SessionMap.empty && not trans#reading then (
    trans # start_reading
      ~before_record:(fun n _ ->
			match cl.max_resp_length with
			  | None -> `Accept
			  | Some m -> if n > m then `Reject else `Accept
		     )
      ~when_done:(fun r ->
		    handle_incoming_message cl r)
      ()
  )
  else
    dlog cl "Stopping reading";
;;


check_for_input := next_incoming_message;;


let rec handle_outgoing_message cl call r =
  (* Called after a complete message has been sent by the transporter *)
  match r with
    | `Error e ->
	close ~error:(Communication_error e) cl

    | `Ok () ->
	dlog cl "message writing finished";
	if call.batch_flag || call.call_timeout = 0.0 then (
	  try
	    if call.batch_flag then
	      pass_result cl call (fun () -> XV_void) (* may raise Keep_call *)
	    else
	      pass_exception cl call Message_timeout;
	    remove_pending_call cl call;
	  with Keep_call ->  (* no removal *)
	    ()
	);
	!check_for_input cl;
	next_outgoing_message cl

and next_outgoing_message cl =
  match cl.trans with
    | None -> ()   (* Not yet initialized *)
    | Some trans -> 
	if not trans#writing then
	  next_outgoing_message' cl trans

and next_outgoing_message' cl trans =
  let call_opt = 
    try Some(Queue.take cl.waiting_calls) with Queue.Empty -> None in

  match call_opt with
    | Some call ->
	(* If the call is already 'Done', skip it. *)
  	(* Change the state of the call. It is now 'pending': *)

	if call.state = Done then (
	  (* That can happen for calls that timeout before they are sent *)
	  dlog cl "found call that has been done";
	  next_outgoing_message cl
	)
	else (
	  let dest =
	    match call.destination with
	      | Some d -> `Sockaddr d
	      | None -> trans#getpeername in
	  ( match call.state with
	      | Done -> assert false
	      | Delayed -> assert false
	      | Waiting ->
		  cl.pending_calls <-
		    SessionMap.add call.xid call cl.pending_calls;
		  (* The xid is already in used_xids *)
		  call.state <- Pending;
		  dlogr_ptrace cl
		    (fun () ->
		       sprintf
			 "RPC --> (sock=%s,peer=%s,xid=%Ld) %s"
			 (Rpc_transport.string_of_sockaddr trans#getsockname)
			 (Rpc_transport.string_of_sockaddr dest)
			 (Rtypes.int64_of_uint4 call.xid)
			 (match call.detail with
			    | `Regular rc ->
				Rpc_util.string_of_request
				  !Debug.ptrace_verbosity
				  rc.prog
				  rc.proc
				  rc.param
			    | `Auth_proto _ ->
				"<authprot>"
			 )
		    )
	      | Pending ->
		  ()
		    (* The call is already member of [pending_calls]
		     * (retransmitted)
		     *)
	  );

	  (* If there should be a timeout handler, add it: *)
	  set_timeout cl call;

	  (* Send the message: *)
	  let m =
	    match call.request with
	      | None -> assert false
	      | Some m -> m in

	  (* If this is a regular message, we can now drop the request buffer *)
	  ( match call.detail with
	      | `Regular _ -> call.request <- None
	      | _ -> ()
	  );

	  dlog cl "start_writing";
	  trans # start_writing
	    ~when_done:(fun r ->
			  handle_outgoing_message cl call r)
	    m
	    dest

	);

    | None ->
	()
;;


check_for_output := next_outgoing_message ;;


(* Shutdown:
 * We first try an orderly shutdown. If that does not work, just inactivate
 * the transport.
 *)


let shutdown_connector cl mplex ondown =
  dlog cl "shutdown_connector";
  mplex # abort_rw();
  ( try
      mplex # start_shutting_down
	~when_done:(fun exn_opt ->
		      ( match exn_opt with 
			  | `Ok _ -> ()
			  | `Error exn ->
			      dlogr cl
				(fun () -> 
				   sprintf "start_shutting_down: exception %s"
				     (Netexn.to_string exn))
		      );
		      mplex # inactivate();
		      ondown()
		   )
	()
    with
      | _ -> mplex # inactivate(); ondown()
  )


let mplex_of_fd ~close_inactive_descr prot fd esys =
  let preclose() =
    Netlog.Debug.release_fd fd in
  match prot with
    | Tcp ->
        Rpc_transport.stream_rpc_multiplex_controller
          ~close_inactive_descr ~preclose fd esys
    | Udp ->
        Rpc_transport.datagram_rpc_multiplex_controller
          ~close_inactive_descr ~preclose fd esys


class type socket_config =
object
  method non_blocking_connect : bool
  method multiplexing :
    close_inactive_descr:bool ->
    protocol -> Unix.file_descr -> Unixqueue.event_system ->
      Rpc_transport.rpc_multiplex_controller Uq_engines.engine
end


class default_socket_config : socket_config =
object
  method non_blocking_connect = true
  method multiplexing ~close_inactive_descr prot fd esys =
    let close() =
      if close_inactive_descr then (
	Netlog.Debug.release_fd fd;
	try
	  match Netsys_win32.lookup fd with
	    | Netsys_win32.W32_pipe ph ->
		Netsys_win32.pipe_shutdown ph;
		Unix.close fd
	    | _ -> 
		()
	with Not_found ->
	  Unix.close fd 
      ) in
    let eng = 
      try
	let mplex = mplex_of_fd ~close_inactive_descr prot fd esys in
	new Uq_engines.epsilon_engine (`Done mplex) esys 
      with
	| error -> 
	    new Uq_engines.epsilon_engine (`Error error) esys in
    Uq_engines.when_state
      ~is_aborted:(fun () -> close())
      ~is_error:(fun _ -> close())
      eng;
    eng
end


class blocking_socket_config : socket_config =
object
  inherit default_socket_config
  method non_blocking_connect = false
end

let default_socket_config = new default_socket_config
let blocking_socket_config = new blocking_socket_config

type mode2 =
    [ `Socket_endpoint of protocol * Unix.file_descr 
    | `Multiplexer_endpoint of Rpc_transport.rpc_multiplex_controller
    | `Socket of protocol * connector * socket_config
    ]


class unbound_async_call cl prog name v =
  let emit = ref (fun _ -> assert false) in
  let call = unbound_async_call_r cl prog name v (fun gr -> !emit gr) None in
object(self)
  inherit [ Xdr.xdr_value ] Uq_engines.engine_mixin (`Working 0) cl.esys

  initializer
    emit := (fun get_result -> 
	       try
		 let r = get_result() in
		 self # set_state (`Done r)
	       with
		 | err ->
		     self # set_state (`Error err)
	    )

  method event_system = cl.esys
  method abort() =
    match self#state with
      | `Working _ ->
	  if call.state <> Done then
	    remove_pending_call cl call;
	  call.state <- Done;
	  self # set_state `Aborted
      | _ -> ()
end


let string_of_file_descr fd =
  Int64.to_string (Netsys.int64_of_file_descr fd)

let rec internal_create initial_xid
                        shutdown
			prog_opt
                        mode esys =

  let id_s_0 =
    match mode with
      | `Socket_endpoint(_,fd) ->
	  "Socket_endpoint(_," ^ string_of_file_descr fd ^ ")"
      | `Multiplexer_endpoint ep ->
	  "Multiplexer_endpoint(" ^ 
	    string_of_int(Oo.id ep) ^ ")"
      | `Socket(_, conn, _) ->
	  let s_conn =
	    match conn with
	      | Inet(h,p) -> 
		  "inet/" ^ h ^ ":" ^ string_of_int p
	      | Internet(ip,p) -> 
		  "inet/" ^ Unix.string_of_inet_addr ip ^ ":" ^ string_of_int p
	      | Unix p ->
		  "unix/" ^ p
	      | W32_pipe p ->
		  "w32_pipe/" ^ p
	      | Descriptor fd ->
		  "fd/" ^ string_of_file_descr fd
	      | Dynamic_descriptor f ->
		  "dyn_fd"
	      | Portmapped h ->
		  "portmapped:" ^ h in
	  "Socket(_," ^ s_conn ^ ",_)" in
  let id_s =
    match prog_opt with
      | None -> id_s_0
      | Some prog ->
	  id_s_0 ^ " for program " ^ 
	    (Int32.to_string 
	       (Rtypes.logical_int32_of_uint4
		  (Rpc_program.program_number prog))) in

  let non_blocking_connect =
    match mode with
      | `Socket(_,_,conf) -> conf # non_blocking_connect
      | _ -> true in

  let cl =
    (* preliminary version of the client *)
    { ready = true;
      trans = None;
      progs = ( match prog_opt with None -> [] | Some p -> [p] );
      prot = Rpc.Udp;
      esys = esys;
      est_engine = None;
      shutdown_connector = shutdown;
      waiting_calls = Queue.create();
      pending_calls = SessionMap.empty;
      delayed_calls = Hashtbl.create 27;
      used_xids = SessionMap.empty;
      next_xid = initial_xid;
      next_destination = None;
      next_timeout = (-1.0);
      next_max_retransmissions = 0;
      next_batch_flag = false;
      max_resp_length = None;
      user_name = None;
      mstring_factories = Hashtbl.create 1;
      last_replier = None;
      last_xid = None;
      timeout = (-1.0);
      max_retransmissions = 0;
      exception_handler = (fun _ -> ());
      all_auth_methods = [ ];
      auth_methods = [ ];
      auth_current = Hashtbl.create 3;
      nolog = false;
    }
  in
  Hashtbl.add cl.mstring_factories "*" Xdr_mstring.string_based_mstrings;
  

  let portmapper_engine prot host prog esys = 
    (* Performs GETPORT for the program on [host]. We use 
     * Rpc_portmapper_aux but not Rpc_portmapper_clnt. The latter is
     * impossible because of a dependency cycle.
     *)
    dlog cl "starting portmapper query";
    let pm_port = Rtypes.int_of_uint4 Rpc_portmapper_aux.pmap_port in
    let pm_prog = Rpc_portmapper_aux.program_PMAP'V2 in
    let pm_client = 
      internal_create
	(Rtypes.uint4_of_int 0)
	shutdown_connector
	(Some pm_prog)
	(`Socket(Rpc.Udp, Inet(host, pm_port), default_socket_config))
	esys
    in
    let v =
      Rpc_portmapper_aux._of_PMAP'V2'pmapproc_getport'arg
	{ Rpc_portmapper_aux.prog = Rpc_program.program_number prog;
	  vers = Rpc_program.version_number prog;
	  prot = ( match prot with
		     | Tcp -> Rpc_portmapper_aux.ipproto_tcp
		     | Udp -> Rpc_portmapper_aux.ipproto_udp
		 );
	  port = Rtypes.uint4_of_int 0;
	} in
    let close_deferred() =
      Unixqueue.once esys (Unixqueue.new_group esys) 0.0 
	(fun() -> close pm_client) in
    new Uq_engines.map_engine
      ~map_done:(fun r ->
		   dlog cl "Portmapper GETPORT done";
		   let addr =
		     match pm_client.trans with
		       | None -> assert false
		       | Some trans ->
			   ( match trans # getpeername with
			       | `Implied -> assert false
			       | `Sockaddr a -> a
			   ) in
		   let port =
		     Rpc_portmapper_aux._to_PMAP'V2'pmapproc_getport'res r in
		   let port = Rtypes.int_of_uint4 port in
		   close_deferred();
		   if port = 0 then
		     `Error (Failure "Program not bound in Portmapper")
		   else
		     `Done(addr, port)
		)
      ~map_error:(fun err ->
		    dlog cl "Portmapper GETPORT error";
		    close_deferred();
		    `Error err)
      (new unbound_async_call pm_client pm_prog "PMAPPROC_GETPORT" v)
  in

  let connect_engine addr esys =
    match addr with
      | `Portmapped(prot,host) ->
	  ( match prog_opt with
	      | None ->
		  failwith 
		    "Rpc_client.unbound_create: Portmapped not supported"
	      | Some prog ->
		  new Uq_engines.seq_engine
		    (portmapper_engine prot host prog esys)
		    (fun (sockaddr, port) ->
		       let inetaddr =
			 match sockaddr with
			   | Unix.ADDR_INET(inet, _) -> inet
			   | _ -> assert false in
		       let stype = 
			 match prot with 
			   | Tcp -> Unix.SOCK_STREAM 
			   | Udp -> Unix.SOCK_DGRAM in
		       let addr = `Sock_inet(stype, inetaddr, port) in
		       let opts = Uq_engines.default_connect_options in
		       Uq_engines.connector (`Socket(addr,opts)) esys
		    )
	  )

      | #Uq_engines.connect_address as addr ->
	  Uq_engines.connector addr esys in

  let track fd =
    Netlog.Debug.track_fd ~owner:"Rpc_client" ~descr:id_s fd in

  let disable_nagle fd =
    try
      Unix.setsockopt fd Unix.TCP_NODELAY true
    with _ -> () in

  let open_socket_non_blocking addr prot conf =
    new Uq_engines.seq_engine
      (connect_engine addr esys)
      (fun status ->
	  dlogr  cl
	    (fun () -> 
	       "Non-blocking socket connect successful for " ^ id_s);
	 let fd = Uq_engines.client_endpoint status in
	 disable_nagle fd;
	 track fd;
	 conf # multiplexing ~close_inactive_descr:true prot fd esys
      ) in

  let open_socket_blocking addr prot conf =
    let conn_esys = Unixqueue.create_unix_event_system() in
    let c = connect_engine addr conn_esys in
    Unixqueue.run conn_esys;
    match c # state with
      | `Done status ->
	  dlogr cl
	    (fun () ->
	       "Blocking socket connect successful for " ^ id_s);
	  let fd = Uq_engines.client_endpoint status in
	  disable_nagle fd;
	  track fd;
	  conf # multiplexing ~close_inactive_descr:true prot fd esys
      | `Error err ->
	  raise err
      | _ ->
	  assert false
  in

  let open_socket =
    if non_blocking_connect then 
      open_socket_non_blocking
    else
      open_socket_blocking in

  let (prot, establish_engine) =
    match mode with
      | `Socket_endpoint(prot,fd) ->
	  disable_nagle fd;
	  track fd;
	  let m = mplex_of_fd ~close_inactive_descr:true prot fd esys in
	  (prot, new Uq_engines.epsilon_engine (`Done m) esys)
      | `Multiplexer_endpoint(mplex) ->
	  if mplex # event_system != esys then
            failwith "Rpc_client.create2: \
                      Multiplexer is attached to the wrong event system";
	  (mplex # protocol,
	   new Uq_engines.epsilon_engine (`Done mplex) esys)
       | `Socket(prot,conn,conf) ->
	   let stype = 
	     match prot with Tcp -> Unix.SOCK_STREAM | Udp -> Unix.SOCK_DGRAM in
	   (match conn with
	      | Inet (host,port) ->
		  let saddr = `Sock_inet_byname(stype, host, port) in
		  let addr = 
		    `Socket(saddr, Uq_engines.default_connect_options) in
		  (prot, open_socket addr prot conf)
	      | Internet (host,port) ->
		  let saddr = `Sock_inet(stype, host, port) in
		  let addr = 
		    `Socket(saddr, Uq_engines.default_connect_options) in
		  (prot, open_socket addr prot conf)
	      | Unix path ->
		  let saddr = `Sock_unix(stype, path) in
		  let addr = 
		    `Socket(saddr, 
			    Uq_engines.default_connect_options) in
		  (prot, open_socket addr prot conf)
	      | W32_pipe path ->
		  if prot <> Rpc.Tcp then
		    failwith "Rpc_client.create2: \
                              Pipe only supported for Rpc.Tcp protocol type";
		  let addr = `W32_pipe(Netsys_win32.Pipe_duplex, path) in
		  (prot, open_socket addr prot conf)
	      |	Descriptor fd -> 
		  (* no track! *)
		  let m = 
		    mplex_of_fd ~close_inactive_descr:false prot fd esys in
		  (prot, new Uq_engines.epsilon_engine (`Done m) esys)
	      |	Dynamic_descriptor f ->
		  let fd = f() in
		  track fd;
		  let m = 
		    mplex_of_fd ~close_inactive_descr:true prot fd esys in
		  (prot, new Uq_engines.epsilon_engine (`Done m) esys)
	      | Portmapped host ->
		  (prot, open_socket (`Portmapped(prot,host)) prot conf)
	   )
  in

  let timeout = if prot = Udp then 15.0 else (-.1.0) in
  let max_retransmissions = if prot = Udp then 3 else 0 in

  (* update cl: *)
  cl.prot <- prot;
  cl.est_engine <- Some establish_engine;
  cl.next_timeout <- timeout;
  cl.timeout <- timeout;
  cl.next_max_retransmissions <- max_retransmissions;
  cl.max_retransmissions <- max_retransmissions;
  cl.exception_handler <- (fun exn -> 
			     if not cl.nolog then
			       Netlog.logf `Crit
				 "Rpc_client: Uncaught exception %s"
				 (Netexn.to_string exn)
			  );

  Uq_engines.when_state
    ~is_done:(fun mplex ->
		dlogr cl
		  (fun () ->
		     sprintf 
		       "Fully connected for %s: (sock=%s,peer=%s)" 
		       id_s
		       (Rpc_transport.string_of_sockaddr mplex#getsockname)
		       (Rpc_transport.string_of_sockaddr mplex#getpeername));
		cl.trans <- Some mplex;
		cl.est_engine <- None;
		(* Maybe we already have messages to send: *)
		!check_for_output cl
	     )
    ~is_error:(fun err ->
		 cl.est_engine <- None;
		 close ~error:(Communication_error err) cl;
		 cl.exception_handler err
	      )
    ~is_aborted:(fun () ->
		   cl.est_engine <- None)
    establish_engine;

  cl
;;


let create2 ?program_number ?version_number ?(initial_xid=0)
            ?(shutdown = shutdown_connector)
            mode prog0 esys =
  
  let prog = Rpc_program.update ?program_number ?version_number prog0 in
  let cl = 
    internal_create
      (Rtypes.uint4_of_int initial_xid) shutdown (Some prog) mode esys in
  cl

let unbound_create ?(initial_xid=0) ?(shutdown = shutdown_connector) 
                   mode esys =
  internal_create (Rtypes.uint4_of_int initial_xid) shutdown None mode esys


let bind cl prog =
  cl.progs <- prog :: cl.progs

let use cl prog =
  let prog_id = Rpc_program.id prog in
  if not(List.exists (fun p -> Rpc_program.id p = prog_id) cl.progs) then
    failwith "Rpc_client.use: This program is not bound by this client"


  (*****)

let is_up cl =
  cl.ready

let configure_next_call cl max_retransmission_trials timeout =
  cl.next_max_retransmissions <- max_retransmission_trials;
  cl.next_timeout <- timeout

let configure cl max_retransmission_trials timeout =
  cl.max_retransmissions <- max_retransmission_trials;
  cl.timeout <- timeout;
  configure_next_call cl max_retransmission_trials timeout

let set_dgram_destination cl addr_opt =
  cl.next_destination <- addr_opt

let set_exception_handler cl xh =
  cl.exception_handler <- xh

let set_batch_call cl =
  cl.next_batch_flag <- true

let set_max_response_length cl n =
  cl.max_resp_length <- Some n

let set_mstring_factories cl fac =
  cl.mstring_factories <- fac

let set_user_name cl n =
  cl.user_name <- n

let gen_shutdown cl is_running run ondown =
  if cl.ready then (
    let b = is_running cl.esys in
    ( match cl.est_engine with
	| None -> ()
	| Some e -> e#abort()
    );
    close ~ondown cl;
    if not b then run cl.esys
  )
  else
    ondown()

let shut_down cl =
  gen_shutdown 
    cl 
    Unixqueue.is_running 
    Unixqueue.run 
    (fun () -> ())

let sync_shutdown cl =
  gen_shutdown 
    cl 
    (fun esys -> 
       if Unixqueue.is_running esys then
	 failwith "Rpc_client.sync_shutdown: called from event loop";
       false)
    Unixqueue.run 
    (fun () -> ())
  
let trigger_shutdown cl ondown =
  gen_shutdown
    cl
    (fun _ -> true)
    Unixqueue.run
    (fun () ->
       let g = Unixqueue.new_group cl.esys in
       Unixqueue.once cl.esys g 0.0 ondown
    )


let event_system cl =
  cl.esys

let program cl =
  List.hd cl.progs

let programs cl =
  cl.progs

let get_socket_name cl =
  match cl.trans with
    | None -> failwith "Rpc_client.get_socket_name: not connected"
    | Some trans ->
	( match trans # getsockname with
	    | `Implied ->
		failwith "Rpc_client.get_socket_name: not applicable"
	    | `Sockaddr a -> a
	)

let get_peer_name cl =
  match cl.trans with
    | None -> failwith "Rpc_client.get_peer_name: not connected"
    | Some trans ->
	( match trans # getpeername with
	    | `Implied ->
		failwith "Rpc_client.get_peer_name: not applicable"
	    | `Sockaddr a -> a
	)

let get_sender_of_last_response cl =
  match cl.last_replier with
    | None -> failwith "Rpc_client.get_sender_of_last_response: nothing received yet or sender's address not available from transport layer"
    | Some addr -> addr


let get_xid_of_last_call cl =
  match cl.last_xid with
    | None ->
	failwith "Rpc_client.get_xid_of_last_call: nothing called"
    | Some xid -> xid


let get_protocol cl =
  cl.prot

let verbose b =
  Debug.enable := b

  (*****)

(* Now synchronous calls: *)

type 'a result =
    No
  | Reply of 'a
  | Error of exn


exception Stop_call

let synchronize esys f_async arg =
  let r = ref No in
  let get_result transmitter =
    try
      r := Reply (transmitter())
    with
      x ->
	r := Error x;
	if x = Message_timeout then 
	  raise (Unbound_exception Stop_call)
  in
  (* push the request onto the queue: *)
  let () = f_async arg get_result in
  (* run through the queue and process all elements: *)
  ( try Unixqueue.run esys with Stop_call -> ());
  (* now a call back of 'get_result' should have happened. *)
  match !r with
    No -> failwith "Rpc_client.synchronize: internal error"
  | Reply x -> x
  | Error e -> raise e



let unbound_sync_call cl prog proc arg =
  synchronize
    cl.esys
    (unbound_async_call cl prog proc)
    arg


  (*****)

(* DEPRECATED FUNCTIONS *)


let add_call cl procname param receiver =
  if not cl.ready then
    raise Client_is_down;
  match cl.progs with
    | [ prog ] ->
	unbound_async_call cl prog procname param receiver
    | _ ->
	failwith "Rpc_client.add_call [deprecated function]: \
                  The client does not have exactly one bound program"


let sync_call cl procname param  =
  if not cl.ready then
    raise Client_is_down;
  match cl.progs with
    | [ prog ] ->
	unbound_sync_call cl prog procname param 
    | _ ->
	failwith "Rpc_client.sync_call [deprecated function]: \
                  The client does not have exactly one bound program"


let create ?program_number ?version_number ?(initial_xid=0) 
           ?(shutdown = shutdown_connector)
           esys c prot prog0 =
  create2 
    ?program_number ?version_number ~initial_xid ~shutdown
    (`Socket(prot, c, (new blocking_socket_config)))
    prog0
    esys

