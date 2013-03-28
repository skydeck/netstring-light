(* $Id: rpc_server.ml 1672 2011-09-23 09:58:59Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

open Rtypes
open Xdr
open Unixqueue
open Uq_engines
open Rpc_common
open Rpc
open Printf


exception Connection_lost

class connection_id sock_name_lz peer_name_lz = 
object 
  method socket_name = (Lazy.force sock_name_lz : Unix.sockaddr)
  method peer_name = (Lazy.force peer_name_lz : Unix.sockaddr)
end ;;


class connection_id_for_mplex mplex =
  let sock_name_lz =
    lazy ( match mplex # getsockname with
	     | `Implied -> failwith "Cannot determine own socket name"
	     | `Sockaddr a -> a
	 ) in
  let peer_name_lz =
    lazy ( match mplex # getpeername with
	     | `Implied -> failwith "Cannot determine peer socket name"
	     | `Sockaddr a -> a
	 ) in
  connection_id sock_name_lz peer_name_lz
;;


class no_connection_id : connection_id = 
object 
  method socket_name = failwith "Cannot determine own socket name"
  method peer_name = failwith "Cannot determine peer socket name"
end ;;


type rule =
    [ `Deny
    | `Drop
    | `Reject
    | `Reject_with of Rpc.server_error
    | `Accept
    | `Accept_limit_length of (int * rule)
    ]

type auth_result =
    Auth_positive of
      (string * string * string * Xdr.encoder option * Xdr.decoder option)
  | Auth_negative of Rpc.server_error
  | Auth_reply of (Xdr_mstring.mstring list * string * string)
  | Auth_drop


type auth_peeker =
    [ `None
    | `Peek_descriptor of Unix.file_descr -> string option
    | `Peek_multiplexer of Rpc_transport.rpc_multiplex_controller -> string option
    ]


exception Late_drop

class type auth_details =
object
  method server_addr : Unix.sockaddr option
  method client_addr : Unix.sockaddr option
  method program : uint4
  method version : uint4
  method procedure : uint4
  method xid : uint4
  method credential : string * string
  method verifier : string * string
  method frame_len : int
  method message : Rpc_packer.packed_value
end


class type ['t] pre_auth_method =
object
  method name : string

  method flavors : string list

  method peek : auth_peeker

  method authenticate :
    't ->
    connection_id ->
    auth_details ->
    (auth_result -> unit) ->
      unit

end


module Uint4 = struct
  type t = uint4
  let compare (a:uint4) (b:uint4) =
    (* avoid calling Pervasives.compare *)
    let a' = logical_int32_of_uint4 a in
    let b' = logical_int32_of_uint4 b in
    if a' = b' then
      0
    else
      if a' < b' then
	-1
      else
	1
end


module Uint4Map = Map.Make(Uint4)


let rec uint4_map_mk f l =
  match l with
      [] -> Uint4Map.empty
    | x :: l' ->
	let (key,value) = f x in
	Uint4Map.add key value (uint4_map_mk f l')
;;


let uint4_min m =
  Uint4Map.fold
    (fun n _ acc ->
       let p =
	 Rtypes.int64_of_uint4 n < Rtypes.int64_of_uint4 acc in
       if p then n else acc)
    m
    (Rtypes.uint4_of_int64 0xffffffffL) ;;
       

let uint4_max m =
  Uint4Map.fold
    (fun n _ acc ->
       let p =
	 Rtypes.int64_of_uint4 n > Rtypes.int64_of_uint4 acc in
       if p then n else acc)
    m
    (Rtypes.uint4_of_int 0) ;;
       


type t =
      { mutable main_socket_name : Rpc_transport.sockaddr;
	mutable dummy : bool;
	mutable service : (Rpc_program.t * binding Uint4Map.t) 
	                    Uint4Map.t Uint4Map.t;
	        (* Program nr/version nr/procedure nr *)
	mutable portmapped : int option;  (* port number *)
	mutable esys : event_system;
	mutable prot : protocol;
	mutable exception_handler : exn -> unit;
	mutable unmap_port : (unit -> unit);
	mutable onclose : (connection_id -> unit) list;
	mutable filter : (Rpc_transport.sockaddr -> connection_id -> rule);
	mutable auth_methods : (string, t pre_auth_method) Hashtbl.t;
	mutable auth_peekers : (auth_peeker * t pre_auth_method) list;
	mutable connections : connection list;
	mutable master_acceptor : server_socket_acceptor option;
	mutable transport_timeout : float;
	mutable nolog : bool;
	mutable get_last_proc : unit->string;
	mutable mstring_factories : Xdr_mstring.named_mstring_factories;
      }

and connection =
    (* For connected streams, but also used for datagram servers. *)
      { whole_server : t;
	conn_id : connection_id;
        mutable trans : Rpc_transport.rpc_multiplex_controller option;
	mutable fd : Unix.file_descr option;

	mutable rule : rule option;
        (* TODO: The rule exists per incoming message, not per connection.
	 * Is it better to put it into Rpc_transport?
	 *)

	mutable next_call_id : int;

	(* replies to do: *)
	mutable replies : session Queue.t;

	mutable close_when_empty : bool;
	(* If true, the connection will be closed when [replies] becomes
         * empty.
         *)

	(* RPC does not define how to check if replies are delivered,
	 * so there is no "re-reply" mechanism. The client has to call
	 * again; but the server cannot identify such repetitions.
	 * (The xid field cannot be used for this purpose!)
	 *)

	mutable peeked :        bool;           (* whether already peeked *)
	mutable peeked_user :   string option;
	mutable peeked_method : t pre_auth_method;
      }

and session =
    (* intentionally immutable to make value sharing possible *)
      { server : connection;
	prog : Rpc_program.t option;  (* None for errors etc. *)
	sess_conn_id : connection_id;
	sockaddr : Unix.sockaddr Lazy.t;   (* own address *)
	peeraddr : Rpc_transport.sockaddr;
	call_id : int;
	client_id : uint4;         (* xid *)
	procname : string;
	parameter : xdr_value;     (* XV_void if not used *)
	result : Rpc_packer.packed_value;
         (* complete result; "" if not used *)
	ptrace_result :string;  (* ptrace only; "" if not used *)
	auth_method : t pre_auth_method;
	auth_user : string;
	auth_ret_flav : string;
	auth_ret_data : string;
	encoder : Xdr.encoder option;
      }

and connector =
      Localhost of int                     (* port, 0: automatically chosen *)
    | Portmapped
    | Internet of (Unix.inet_addr * int)   (* addr, port *)
    | Unix of string                       (* path to unix dom sock *)
    | W32_pipe of string
    | Descriptor of Unix.file_descr
    | Dynamic_descriptor of (unit -> Unix.file_descr)

and binding_sync =
      { sync_name : string;
	sync_proc : xdr_value -> xdr_value
      }

and binding_async =
      { async_name : string;
	async_invoke : session -> xdr_value -> unit
                                            (* invocation of this procedure *)
      }

and binding =
      Sync of binding_sync
    | Async of binding_async

class type auth_method = [t] pre_auth_method ;;

class auth_none : auth_method =
object
  method name = "AUTH_NONE"
  method flavors = [ "AUTH_NONE" ]
  method peek = `None
  method authenticate _ _ _ f = 
    f(Auth_positive("","AUTH_NONE","",None,None))
end

let auth_none = new auth_none

class auth_too_weak : auth_method =
object
  method name = "AUTH_TOO_WEAK"
  method flavors = []
  method peek = `None
  method authenticate _ _ _ f = 
    f(Auth_negative Auth_too_weak)
end

let auth_too_weak = new auth_too_weak

class auth_transport : auth_method =
object
  method name = "AUTH_TRANSPORT"
  method flavors = [ ]
  method peek = 
    `Peek_multiplexer
      (fun mplex ->
	 mplex # peer_user_name
      )
  method authenticate _ _ _ f = 
    f(Auth_negative Auth_too_weak)
end

let auth_transport = new auth_transport

  (*****)

module Debug = struct
  let enable = ref false
  let enable_ctrace = ref false
  let enable_ptrace = ref false
  let ptrace_verbosity = ref `Name_abbrev_args
  let disable_for_server srv = srv.nolog <- true
end

let dlog0 = Netlog.Debug.mk_dlog "Rpc_server" Debug.enable
let dlogr0 = Netlog.Debug.mk_dlogr "Rpc_server" Debug.enable

let dlog srv m = if not srv.nolog then dlog0 m
let dlogr srv m = if not srv.nolog then dlogr0 m

let dlog0_ctrace = Netlog.Debug.mk_dlog "Rpc_server.Ctrace" Debug.enable_ctrace
let dlogr0_ctrace = Netlog.Debug.mk_dlogr "Rpc_server.Ctrace" Debug.enable_ctrace

let dlog_ctrace srv m = if not srv.nolog then dlog0_ctrace m
let dlogr_ctrace srv m = if not srv.nolog then dlogr0_ctrace m


let dlog0_ptrace = Netlog.Debug.mk_dlog "Rpc_server.Ptrace" Debug.enable_ptrace
let dlogr0_ptrace = Netlog.Debug.mk_dlogr "Rpc_server.Ptrace" Debug.enable_ptrace

let dlog_ptrace srv m = if not srv.nolog then dlog0_ptrace m
let dlogr_ptrace srv m = if not srv.nolog then dlogr0_ptrace m


let () =
  Netlog.Debug.register_module "Rpc_server" Debug.enable;
  Netlog.Debug.register_module "Rpc_server.Ctrace" Debug.enable_ctrace;
  Netlog.Debug.register_module "Rpc_server.Ptrace" Debug.enable_ptrace

  (*****)

let connector_of_sockaddr =
  function
    | Unix.ADDR_INET(ip,p) ->
	Internet(ip,p)
    | Unix.ADDR_UNIX s ->
	Unix s

let connector_of_socksymbol sym =
  connector_of_sockaddr
    (Uq_resolver.sockaddr_of_socksymbol sym)


let sockaddrname sa =
  match sa with
    | Unix.ADDR_INET(addr, port) ->
	Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port
    | Unix.ADDR_UNIX path ->
	String.escaped path

let portname fd =
  try 
    sockaddrname (Unix.getsockname fd)
  with
    | _ -> "anonymous"

let portoptname fd_opt =
  match fd_opt with
    | None -> "unknown"
    | Some fd -> portname fd

let mplexname mplex =
  try
    match mplex # getpeername with
      | `Implied -> "implied"
      | `Sockaddr a -> sockaddrname a
  with
    | _ -> "anonymous"

let mplexoptname mplex_opt =
  match mplex_opt with
    | None -> "unknown"
    | Some mplex -> mplexname mplex


let xidname xid =
  Int32.to_string (Rtypes.int32_of_uint4 xid)

let errname =
  function
    | Unavailable_program      -> "Unavailable_program"
    | Unavailable_version(_,_) -> "Unavailable_version"
    | Unavailable_procedure    -> "Unavailable_procedure"
    | Garbage                  -> "Garbage"
    | System_err               -> "System_err"
    | Rpc_mismatch(_,_)        -> "Rpc_mismatch"
    | Auth_bad_cred            -> "Auth_bad_cred"
    | Auth_rejected_cred       -> "Auth_rejected_cred"
    | Auth_bad_verf            -> "Auth_bad_verf"
    | Auth_rejected_verf       -> "Auth_rejected_verf"
    | Auth_too_weak            -> "Auth_too_weak"
    | Auth_invalid_resp        -> "Auth_invalid_resp"
    | Auth_failed              -> "Auth_failed"
    | RPCSEC_GSS_ctxproblem    -> "RPCSEC_GSS_ctxproblem"
    | RPCSEC_GSS_credproblem   -> "RPCSEC_GSS_credproblem"

  (*****)

let null_packed_value =
  Rpc_packer.packed_value_of_string ""

let no_conn_id = new no_connection_id

  (*****)

let check_for_output = ref (fun _ _ -> ())

  (*****)

type reaction =
    Execute_procedure
  | Reject_procedure of server_error

let process_incoming_message srv conn sockaddr_lz peeraddr message reaction =

  let sockaddr_opt =
    try Some(Lazy.force sockaddr_lz) with _ -> None in

  let sockaddr =
    match sockaddr_opt with
      | Some a -> `Sockaddr a
      | None -> `Implied in

  let peeraddr_opt =
    match peeraddr with
      | `Sockaddr a -> Some a
      | `Implied -> None in

  let peeraddr_lz =
    lazy ( match peeraddr with
	     | `Implied -> failwith "Cannot determine peer socket name"
	     | `Sockaddr a -> a
	 ) in

  let make_immediate_answer xid procname result f_ptrace_result =
    srv.get_last_proc <- 
      (fun () -> 
	 if procname = "" then "Unavailable" else "Response " ^ procname);
    { server = conn;
      prog = None;
      sess_conn_id = if srv.prot = Rpc.Tcp then conn.conn_id
                     else new connection_id sockaddr_lz peeraddr_lz;
      sockaddr = sockaddr_lz;
      peeraddr = peeraddr;
      call_id = (-1);          (* not applicable *)
      client_id = xid;
      procname = procname;
      parameter = XV_void;
      result = result;
      auth_method = auth_none;
      auth_user = "";
      auth_ret_flav = "AUTH_NONE";
      auth_ret_data = "";
      encoder = None;
      ptrace_result = (if !Debug.enable_ptrace then f_ptrace_result() else "")
    }
  in

  let schedule_answer answer =
    Queue.add answer conn.replies;
    !check_for_output srv conn
  in

  let protect_protect f =
    try
      f()
    with
	any ->
	  (try srv.exception_handler any with _ -> ());
  in

  let protect ?(ret_flav="AUTH_NONE") ?(ret_data="") f =
    try
      f()
    with
	Rpc_server(Unavailable_program | Unavailable_version(_,_)|
                   Unavailable_procedure | Garbage | System_err
		   as condition) ->
	  protect_protect
	    (fun () ->
	       let xid = Rpc_packer.peek_xid message in
	       let reply = Rpc_packer.pack_accepting_reply xid
			     ret_flav ret_data condition in
	       let answer = 
		 make_immediate_answer xid "" reply 
		   (fun () -> "Error " ^ errname condition) in
	       schedule_answer answer
	    )
      | (Xdr.Xdr_format _
	| Xdr.Xdr_format_message_too_long _ as e
	) ->
          (* Convert to Garbage *)
	   protect_protect
	     (fun () ->
		dlogr srv
		 (fun () ->
		    sprintf "Emitting Garbage after exception: %s"
		      (Netexn.to_string e)
		 );
	       let xid = Rpc_packer.peek_xid message in
	       let reply = Rpc_packer.pack_accepting_reply xid
			     ret_flav ret_data Garbage in
	       let answer = make_immediate_answer xid "" reply 
		 (fun () -> "Error Garbage") in
	       schedule_answer answer
	    )
      | Rpc_server condition ->
	  protect_protect
	    (fun () ->
	       let xid = Rpc_packer.peek_xid message in
	       let reply = Rpc_packer.pack_rejecting_reply xid condition in
	       let answer = make_immediate_answer xid "" reply 
		 (fun () -> "Error " ^ errname condition) in
	       schedule_answer answer
	    )
      | Late_drop ->
	  Netlog.logf `Err
	    "Dropping response message"
      | Abort(_,_) as x ->
	  raise x
      | any ->
	  (* Reply "System_err": *)
	  (try srv.exception_handler any with _ -> ());
	  protect_protect
	    (fun () ->
	       let xid = Rpc_packer.peek_xid message in
	       let reply = Rpc_packer.pack_accepting_reply xid
			     ret_flav ret_data System_err in
	       let answer = make_immediate_answer xid "" reply
		 (fun () -> "Error System_err") in
	       schedule_answer answer
	    )
  in

  protect
    (fun () ->
       match reaction with
	   Execute_procedure ->
	     let
	       xid, prog_nr, vers_nr, proc_nr,
	       flav_cred, data_cred, flav_verf, data_verf, frame_len
	       = Rpc_packer.unpack_call_frame_l message
	     in

	     dlogr_ptrace srv
	       (fun () ->
		  sprintf
		    "Request (sock=%s,peer=%s,xid=%lu) for [0x%lx,0x%lx,0x%lx]"
		    (Rpc_transport.string_of_sockaddr sockaddr)
		    (Rpc_transport.string_of_sockaddr peeraddr)
		    (Rtypes.logical_int32_of_uint4 xid)
		    (Rtypes.logical_int32_of_uint4 prog_nr)
		    (Rtypes.logical_int32_of_uint4 vers_nr)
		    (Rtypes.logical_int32_of_uint4 proc_nr)
	       );
	     
	     let sess_conn_id =
	       if srv.prot = Rpc.Tcp then 
		 conn.conn_id
	       else 
		 new connection_id sockaddr_lz peeraddr_lz
	     in

	     (* First authenticate: *)
	     let (auth_m, authenticate) =
	       match conn.peeked_user with
		 | Some uid ->
		     (conn.peeked_method,
		      (fun _ _ _ cb ->
			 cb (Auth_positive(uid, "AUTH_NONE", "", None, None)))
		     )
		 | None ->
		     ( let m =
			 try Hashtbl.find srv.auth_methods flav_cred
			 with Not_found -> auth_too_weak in
		       (m, m#authenticate)
		     )
	     in

	     let auth_details =
	       ( object
		   method server_addr = sockaddr_opt
		   method client_addr = peeraddr_opt
		   method program = prog_nr
		   method version = vers_nr
		   method procedure = proc_nr
		   method xid = xid
		   method credential = (flav_cred, data_cred)
		   method verifier = (flav_verf, data_verf)
		   method frame_len = frame_len
		   method message = message
		 end
	       ) in

	     (* The [authenticate] method will call the passed function
	      * when the authentication is done. This may be at any time
	      * in the future.
	      *)
	     authenticate
	       srv sess_conn_id auth_details
	       (function 
		  Auth_positive(user,ret_flav,ret_data,enc_opt,dec_opt) ->
		  (* user: the username (method-dependent)
		   * ret_flav: flavour of verifier to return
		   * ret_data: data of verifier to return
		   *)
		  protect ~ret_flav ~ret_data
		    (fun () ->
		       (* Find the right binding *)
		       let prog_map =
			 try
			   Uint4Map.find prog_nr srv.service
			 with Not_found ->
			   raise (Rpc_server Unavailable_program) in

		       let (prog, binding) =
			 try
			   Uint4Map.find vers_nr prog_map
			 with Not_found ->
			   let min_vers = uint4_min prog_map in
			   let max_vers = uint4_max prog_map in
			   raise 
			     (Rpc_server 
				(Unavailable_version (min_vers, max_vers))) in

		       let proc =
			 try
			   Uint4Map.find proc_nr binding
			 with Not_found -> 
			   raise (Rpc_server Unavailable_procedure)
		       in

		       let procname =
			 match proc with
			     Sync p -> p.sync_name
			   | Async p -> p.async_name
		       in

		       let param =
			 Rpc_packer.unpack_call_body
			   ~mstring_factories:srv.mstring_factories
			   ?decoder:dec_opt
			   prog procname message frame_len in

		       srv.get_last_proc <-
			 (fun () ->
			    (* no [string_of_request] - we would keep a
                               reference to param forever!
			     *)
			    "Invoke " ^ procname ^ "()"
			 );

		       dlogr_ptrace srv
			 (fun () ->
			    sprintf
			      "Invoke (sock=%s,peer=%s,xid=%lu): %s"
			      (Rpc_transport.string_of_sockaddr sockaddr)
			      (Rpc_transport.string_of_sockaddr peeraddr)
			      (Rtypes.logical_int32_of_uint4 xid)
			      (Rpc_util.string_of_request
				 !Debug.ptrace_verbosity
				 prog
				 procname
				 param
			      )
			 );

		       begin match proc with
			   Sync p ->
			     let result_value =
			       p.sync_proc param
			     in
			     (* Exceptions raised by the encoder are
				handled by [protect]
			      *)
			     let reply = 
			       Rpc_packer.pack_successful_reply
				 ?encoder:enc_opt
				 prog p.sync_name xid
				 ret_flav ret_data result_value in
			     let answer = make_immediate_answer
			       xid procname reply 
			       (fun () ->
				  Rpc_util.string_of_response
				    !Debug.ptrace_verbosity
				    prog
				    procname
				    result_value
			       )
			     in
			     schedule_answer answer
			 | Async p ->
			     let u, m = match conn.peeked_user with
				 Some uid -> uid, conn.peeked_method
			       | None -> user, auth_m
			     in
			     let this_session =
			       { server = conn;
				 prog = Some prog;
				 sess_conn_id = sess_conn_id;
				 sockaddr = sockaddr_lz;
				 peeraddr = peeraddr;
				 call_id = conn.next_call_id;
				 client_id = xid;
				 procname = p.async_name;
				 parameter = param;
				 result = null_packed_value;
				 auth_method = m;
				 auth_user = u;
				 auth_ret_flav = ret_flav;
				 auth_ret_data = ret_data;
				 ptrace_result = "";  (* not yet known *)
				 encoder = enc_opt;
			       } in
			     conn.next_call_id <- conn.next_call_id + 1;
			     p.async_invoke this_session param
		       end
		    )
		  | Auth_negative code ->
		      protect (fun () -> raise(Rpc_server code))
		  | Auth_reply (data, ret_flav, ret_data) ->
		      let reply = 
			Rpc_packer.pack_successful_reply_raw
			  xid ret_flav ret_data data in
		      let answer = 
			make_immediate_answer
			  xid "" reply 
			  (fun () -> "") in
		      schedule_answer answer
		  | Auth_drop ->
		      dlog srv "auth_drop";
		      ()
		      
	       )
	 | Reject_procedure reason ->
	     srv.get_last_proc <-
	       (fun () ->
		  "Reject " ^ Rpc.string_of_server_error reason
	       );
	     protect (fun () -> raise(Rpc_server reason))
    )
;;

  (*****)

let terminate_any srv conn =
  match conn.trans with
    | None ->
	()
    | Some mplex ->
	dlogr_ctrace srv
	  (fun () ->
	     sprintf "(sock=%s,peer=%s): Closing"
	       (Rpc_transport.string_of_sockaddr mplex#getsockname)
	       (Rpc_transport.string_of_sockaddr mplex#getpeername));
	conn.trans <- None;
	mplex # abort_rw();
	( try
	    mplex # start_shutting_down
	      ~when_done:(fun exn_opt ->
			    (* CHECK: Print exception? *)
			    mplex # inactivate())
	      ();
	  with _ -> mplex # inactivate()
	);
	srv.connections <- 
	  List.filter (fun c -> c != conn) srv.connections;
	if srv.prot = Tcp then (
	  List.iter
	    (fun action ->
	       try
		 action conn.conn_id
	       with
		 | any ->
		     (try srv.exception_handler any with _ -> ())
	    )
	    srv.onclose
	)


let terminate_connection srv conn =
  if srv.prot = Tcp then (
    terminate_any srv conn
  )

  (*****)


let rec unroll_rule r length =
  match r with
    | `Accept_limit_length(limit,r') ->
	if length > limit then unroll_rule r' length else `Accept
    | (`Drop | `Reject | `Reject_with _ | `Deny | `Accept as other) -> 
	other
;;


let rec handle_incoming_message srv conn r =
  (* Called when a complete message has been read by the transporter *)
  match r with
    | `Error e ->
	terminate_connection srv conn;
	raise e

    | `Ok(in_rec,trans_addr) ->
	dlog srv "got message";

	if conn.close_when_empty then (
	    dlog srv "ignoring msg after shutdown";
	) else (

	  (* First check whether the message matches the filter rule: *)
	  
	  let peeraddr = trans_addr in

	  let sockaddr, trans_sockaddr =
	    match conn.trans with
	      | None -> assert false
	      | Some trans ->
		  ( lazy ( match trans # getsockname with
			     | `Sockaddr a -> a
			     | `Implied -> failwith "Address not available" ),
		    trans#getsockname
		  ) in
		  
	  ( match in_rec with
	      | `Deny ->
		  dlogr_ptrace srv
		    (fun () ->
		       sprintf
			 "Request (sock=%s,peer=%s): Deny"
			 (Rpc_transport.string_of_sockaddr trans_sockaddr)
			 (Rpc_transport.string_of_sockaddr peeraddr)
		    );
		  terminate_connection srv conn (* for safety *)
	      | `Drop ->
		  (* Simply forget the message *)
		  dlogr_ptrace srv
		    (fun () ->
		       sprintf
			 "Request (sock=%s,peer=%s): Drop"
			 (Rpc_transport.string_of_sockaddr trans_sockaddr)
			 (Rpc_transport.string_of_sockaddr peeraddr)
		    );
		  ()
	      | `Accept pv ->
		  process_incoming_message
		    srv conn sockaddr peeraddr pv Execute_procedure
	      | `Reject pv ->
		  process_incoming_message
		    srv conn sockaddr peeraddr pv
		    (Reject_procedure Auth_too_weak)
	      | `Reject_with(pv,code) ->
		  process_incoming_message
		    srv conn sockaddr peeraddr pv
		    (Reject_procedure code)
	  );
	  next_incoming_message srv conn  (* if still connected *)
	)

    | `End_of_file ->
        dlog srv "End of file";
	terminate_connection srv conn


and next_incoming_message srv conn =
  match conn.trans with
    | None -> ()
    | Some trans -> next_incoming_message' srv conn trans

and next_incoming_message' srv conn trans =
  let filter_var = ref None in
  trans # start_reading
    ~peek:(fun () -> peek_credentials srv conn)
    ~before_record:(handle_before_record srv conn filter_var)
    ~when_done:(fun r -> handle_incoming_message srv conn r)
    ()

and handle_before_record srv conn filter_var n trans_addr =
  dlog srv "Checking filter before_record";
(*
  let filter = 
    match !filter_var with
      | Some filter -> 
	  filter
      | None ->
 *)
	  let filter = srv.filter trans_addr conn.conn_id in
(*
	  filter_var := Some filter;
	  filter in
 *)
  ( match unroll_rule filter n with
      | `Accept -> `Accept
      | `Deny   -> terminate_connection srv conn; `Deny
      | `Drop   -> `Drop
      | `Reject -> `Reject
      | `Reject_with code -> `Reject_with code
  )

and peek_credentials srv conn =
  if not conn.peeked && (* srv.prot = Tcp && *) srv.auth_peekers <> [] then begin
    (* This is used by AUTH_LOCAL to get the credentials of the peer. Thus
     * we need the file descriptor. Without descriptor, we just cannot
     * authenticate!
     *)
    dlog srv "peek_credentials";
    let u = ref None in
    let m = ref auth_none in
    try
      List.iter
	(fun (peeker, meth) ->
	   match peeker with
	     | `Peek_descriptor p ->
		 ( match conn.fd with
		     | Some fd ->
			 let uid_opt = p fd in
			 if uid_opt <> None then (
			   u := uid_opt; 
			   m := meth; 
			   raise Exit
			 )
		     | None -> ()
		 )
	     | `Peek_multiplexer p ->
		 ( match conn.trans with
		     | Some mplex ->
			 let uid_opt = p mplex in
			 if uid_opt <> None then (
			   u := uid_opt; 
			   m := meth; 
			   raise Exit
			 )
		     | None -> ()
		 )
	     | _ -> ()
	)
	srv.auth_peekers;
    with
	Exit ->
	  conn.peeked <- true;
	  conn.peeked_user <- !u;
	  conn.peeked_method <- !m;
	  dlogr srv (fun () ->
		       sprintf "peek_credentials: user=%s" 
			 ( match !u with
			     | None -> "./."
			     | Some s -> s
			 )
		    );
  end
;;


let rec handle_outgoing_message srv conn r =
  (* Called after a complete message has been sent by the transporter *)
  match r with
    | `Error e ->
	terminate_connection srv conn;
	raise e

    | `Ok () ->
        dlog srv "message writing finished";
	if conn.close_when_empty && Queue.is_empty conn.replies then (
	  dlog srv "closing connection gracefully";
	  terminate_connection srv conn
	)
	else
          next_outgoing_message srv conn

and next_outgoing_message srv conn =
  match conn.trans with
    | None -> ()   (* Not yet initialized *)
    | Some trans ->
        if not trans#writing then
          next_outgoing_message' srv conn trans

and next_outgoing_message' srv conn trans =
  let reply_opt =
    try Some(Queue.take conn.replies) with Queue.Empty -> None in

  match reply_opt with
    | Some reply ->
	dlogr_ptrace srv
	  (fun () ->
	     let sockaddr =
	       try `Sockaddr (Lazy.force reply.sockaddr)
	       with _ -> `Implied in
	     sprintf
	       "Response (sock=%s,peer=%s,cid=%d,xid=%ld): %s"
	       (Rpc_transport.string_of_sockaddr sockaddr)
	       (Rpc_transport.string_of_sockaddr reply.peeraddr)
	       reply.call_id
	       (Rtypes.logical_int32_of_uint4 reply.client_id)
	       reply.ptrace_result
	  );

	dlog srv "next reply";
	trans # start_writing
	  ~when_done:(fun r ->
			handle_outgoing_message srv conn r)
	  reply.result
	  reply.peeraddr
    | None ->
	(* this was the last reply in the queue *)
	dlog srv "last reply"
;;

check_for_output := next_outgoing_message ;;

  (*****)

class type socket_config =
object
  method listen_options : listen_options
  method multiplexing : 
    close_inactive_descr:bool ->
    protocol -> Unix.file_descr -> Unixqueue.event_system ->
      Rpc_transport.rpc_multiplex_controller engine
end


type mode2 =
    [ `Socket_endpoint of protocol * Unix.file_descr
    | `Multiplexer_endpoint of Rpc_transport.rpc_multiplex_controller
    | `Socket of protocol * connector * socket_config
    | `Dummy of protocol
    ]


let create2_srv prot esys =
  let default_exception_handler ex =
    Netlog.log
      `Crit
      ("Rpc_server exception handler: Exception " ^ Netexn.to_string ex)
  in

  let none = Hashtbl.create 3 in
  Hashtbl.add none "AUTH_NONE" auth_none;

  let mf = Hashtbl.create 1 in
  Hashtbl.add mf "*" Xdr_mstring.string_based_mstrings;
  
  { main_socket_name = `Implied;
    dummy = false;
    service = Uint4Map.empty;
    portmapped = None;
    esys = esys;
    prot = prot;
    exception_handler = default_exception_handler;
    unmap_port = (fun () -> ());
    onclose = [];
    filter = (fun _ _ -> `Accept);
    auth_methods = none;
    auth_peekers = [];
    connections = [];
    master_acceptor = None;
    transport_timeout = (-1.0);
    nolog = false;
    get_last_proc = (fun () -> "");
    mstring_factories = mf 
  }  
;;


let connection srv mplex =
  let conn =
    { whole_server = srv;
      fd = None;
      conn_id = 
	( match mplex # protocol with
	    | Tcp -> new connection_id_for_mplex mplex
	    | Udp -> no_conn_id
	);
      rule = None;
      trans = Some mplex;
      next_call_id = 0;
      replies = Queue.create();
      peeked = false;
      peeked_user = None;
      peeked_method = auth_none;
      close_when_empty = false;
    } in
  srv.connections <- conn :: srv.connections;
  conn
;;


let on_trans_timeout srv conn () =
  terminate_any srv conn
;;


let track fd =
  Netlog.Debug.track_fd
    ~owner:"Rpc_server"
    ~descr:(sprintf "RPC connection %s"
	      (Netsys.string_of_fd fd))
    fd


let track_server fd =
  Netlog.Debug.track_fd
    ~owner:"Rpc_server"
    ~descr:(sprintf "RPC server %s" (Netsys.string_of_fd fd))
    fd


let disable_nagle fd =
  try
    Unix.setsockopt fd Unix.TCP_NODELAY true
  with _ -> ()


let create2_multiplexer_endpoint mplex =
  let prot = mplex#protocol in
  let srv  = create2_srv prot mplex#event_system in
  let conn = connection srv mplex in
  srv.main_socket_name <- mplex # getsockname;
  conn.fd <- mplex # file_descr;
  (* Start serving not before the event loop is entered. *)
  Unixqueue.once 
    mplex#event_system
    (Unixqueue.new_group mplex#event_system)
    0.0
    (fun () ->
       (* Try to peek credentials. This can be too early, however. *)
       if conn.trans <> None then (
	 dlogr_ctrace srv
	   (fun () ->
	      sprintf "(sock=%s,peer=%s): Serving connection"
		(Rpc_transport.string_of_sockaddr mplex#getsockname)
		(portoptname mplex#file_descr));
	 if srv.transport_timeout >= 0.0 then
	   mplex # set_timeout 
	     ~notify:(on_trans_timeout srv conn) srv.transport_timeout;
	 peek_credentials srv conn;
	 next_incoming_message srv conn;
       )
	 (* else: server might have been closed *)
    );
  srv
;;


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
;;


class default_socket_config : socket_config = 
object
  method listen_options = default_listen_options

  method multiplexing ~close_inactive_descr prot fd esys =
    let mplex = mplex_of_fd ~close_inactive_descr prot fd esys in
    let eng = new Uq_engines.epsilon_engine (`Done mplex) esys in

    when_state
      ~is_aborted:(fun () -> mplex # inactivate())
      ~is_error:(fun _ -> mplex # inactivate())
      eng;

    eng
end


let default_socket_config = new default_socket_config 


let create2_socket_endpoint ?(close_inactive_descr=true) 
                            prot fd esys =
  disable_nagle fd;
  if close_inactive_descr then track fd;
  let mplex = mplex_of_fd ~close_inactive_descr prot fd esys in
  create2_multiplexer_endpoint mplex 
;;


let create2_socket_server ?(config = default_socket_config)
		          ?override_listen_backlog
		          prot conn esys =
  let srv = create2_srv prot esys in

  let create_multiplexer_eng ?(close_inactive_descr = true) fd prot =
    disable_nagle fd;
    if close_inactive_descr then track fd;
    config # multiplexing ~close_inactive_descr prot fd esys in

  let rec accept_connections acc =  (* for stream sockets *)
    let eng = acc # accept () in
    when_state
      ~is_done:(fun (slave_fd, _) ->
		  let mplex_eng = create_multiplexer_eng slave_fd Tcp in
		  when_state
		    ~is_done:(fun mplex ->
				let conn = connection srv mplex in
				conn.fd <- Some slave_fd;
				dlogr_ctrace srv
				  (fun () ->
				     sprintf "(sock=%s,peer=%s): Serving connection"
				       (Rpc_transport.string_of_sockaddr 
					  mplex#getsockname)
				       (portname slave_fd));
				if srv.transport_timeout >= 0.0 then
				  mplex # set_timeout 
				    ~notify:(on_trans_timeout srv conn) 
				    srv.transport_timeout;
				(* Try to peek credentials. This can be too
                                 * early, however.
				 *)
				peek_credentials srv conn;
				next_incoming_message srv conn;
				accept_connections acc
			     )
		    ~is_error:(fun exn ->
				 srv.exception_handler exn
			      )
		    mplex_eng
	       )
      ~is_error:(fun exn ->
		   srv.exception_handler exn
		)
      eng
  in

  let bind_to_internet addr port =
    let dom = Netsys.domain_of_inet_addr addr in

    let s =
      Unix.socket
	dom
	(if prot = Tcp then Unix.SOCK_STREAM else Unix.SOCK_DGRAM)
	0
    in
    try
      Unix.setsockopt s Unix.SO_REUSEADDR 
	(config#listen_options.lstn_reuseaddr || port <> 0);
      Unix.setsockopt s Unix.SO_KEEPALIVE true;
      Unix.bind
	s
	(Unix.ADDR_INET (addr, port));
      s
    with
	any -> 
	  Unix.close s; raise any
  in

  let bind_to_localhost port =
    bind_to_internet (Unix.inet_addr_of_string "127.0.0.1") port
  in

  let bind_to_w32_pipe name mode =
    let psrv = Netsys_win32.create_local_pipe_server name mode max_int in
    let s = Netsys_win32.pipe_server_descr psrv in
    s
  in

  let get_descriptor() =
    let (fd, close_inactive_descr) =
      match conn with
	| Localhost port ->
	    let s = bind_to_localhost port in
	    (s, true)
	| Internet (addr,port) ->
	    let s = bind_to_internet addr port in
	    (s, true)
	| Portmapped ->
	    let s = bind_to_internet Unix.inet_addr_any 0 in
	    ( try
		let port =
		  match Unix.getsockname s with
		    | Unix.ADDR_INET(_,port) -> port
		    | _ -> assert false in
		dlogr srv
		  (fun () ->
		     sprintf "Using anonymous port %d" port);
		srv.portmapped <- Some port;
		(s, true)
	      with
		  any -> Unix.close s; raise any
	    )
	| Unix path ->
	    ( match Sys.os_type with
		| "Win32" ->
		    let s =
		      Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		    Unix.bind s (Unix.ADDR_INET(Unix.inet_addr_loopback, 0));
		    ( match Unix.getsockname s with
			| Unix.ADDR_INET(_, port) ->
			    let f = open_out path in
			    output_string f (string_of_int port ^ "\n");
			    close_out f
			| _ -> ()
		    );
		    (s, true)
		| _ ->
		    let s =
      		      Unix.socket
			Unix.PF_UNIX
			(if prot = Tcp then Unix.SOCK_STREAM else Unix.SOCK_DGRAM)
			0
		    in
		    begin try
		      Unix.bind s (Unix.ADDR_UNIX path);
		      (s, true)
		    with
			any -> Unix.close s; raise any
		    end
	    )
	| W32_pipe path ->
	    let s = bind_to_w32_pipe path Netsys_win32.Pipe_duplex in
	    (s, true)
	| Descriptor s -> 
	    (s, false)
	| Dynamic_descriptor f -> 
	    let s = f() in
	    (s, true)
    in
    srv.main_socket_name <- ( try
				`Sockaddr (Unix.getsockname fd)
			      with _ -> 
				`Implied 
			    );
    (fd, close_inactive_descr)
  in

  match prot with
    | Udp ->
	let (fd, close_inactive_descr) = get_descriptor() in
	let mplex_eng = create_multiplexer_eng ~close_inactive_descr fd prot in
	when_state
	  ~is_done:(fun mplex ->
		      let conn = connection srv mplex in
		      conn.fd <- Some fd;
		      dlogr_ctrace srv
			(fun () ->
			   sprintf "(sock=%s,peer=%s): Accepting datagrams"
			     (Rpc_transport.string_of_sockaddr 
				mplex#getsockname)
			     (portname fd));
		      if srv.transport_timeout >= 0.0 then
			mplex # set_timeout 
			  ~notify:(on_trans_timeout srv conn) 
			  srv.transport_timeout;
		      (* Try to peek credentials. This can be too early, 
                       * however. 
		       *)
		      peek_credentials srv conn;
		      next_incoming_message srv conn;
		   )
	  ~is_error:(fun exn ->
		       srv.exception_handler exn
		    )
	  mplex_eng;
	srv

    | Tcp ->
	let (fd, close_inactive_descr) = get_descriptor() in

	dlogr_ctrace srv
	  (fun () ->
	     sprintf "(sock=%s): Listening"
	       (portname fd));
	let backlog =
	  match override_listen_backlog with
	    | Some n -> n
	    | None -> config#listen_options.lstn_backlog in
	( match conn with
	    | W32_pipe _ ->
		let psrv = Netsys_win32.lookup_pipe_server fd in
		Netsys_win32.pipe_listen psrv backlog
	    | _ ->
		Unix.listen fd backlog
	);
	if close_inactive_descr then track_server fd;	  
	let acc = 
	  new Uq_engines.direct_acceptor 
	    ~close_on_shutdown: close_inactive_descr
	    ~preclose:(fun () -> Netlog.Debug.release_fd fd)
	    fd esys in
	srv.master_acceptor <- Some acc;
	accept_connections acc;
	srv
;;


let create2 mode esys =
  match mode with
    | `Socket_endpoint(prot,fd) ->
	create2_socket_endpoint prot fd esys
    | `Multiplexer_endpoint mplex ->
	if mplex#event_system != esys then
	  failwith "Rpc_server.create2: Multiplexer is attached \
                    to the wrong event system";
	create2_multiplexer_endpoint mplex 
    | `Socket(prot,conn,config) ->
	create2_socket_server ~config prot conn esys
    | `Dummy prot ->
	let srv = create2_srv prot esys in
	srv.dummy <- true;
	srv
;;


let is_dummy srv = srv.dummy


let bind ?program_number ?version_number prog0 procs srv =
  let prog = Rpc_program.update ?program_number ?version_number prog0 in
  let prog_nr = Rpc_program.program_number prog in
  let vers_nr = Rpc_program.version_number prog in

  let procs =
    uint4_map_mk
      (fun b ->
	 let name =
	   match b with
	     | Sync b' -> b'.sync_name
	     | Async b' -> b'.async_name
	 in
	 Rpc_program.procedure_number prog name, b
      )
      procs in

  let update_service() =
    let old_progbinding =
      try Uint4Map.find prog_nr srv.service
      with Not_found -> Uint4Map.empty in
    
    srv.service <-
      ( Uint4Map.add 
	  prog_nr 
	  ( Uint4Map.add
	      vers_nr
	      (prog, procs)
	      old_progbinding
	  )
	  srv.service 
      ) in

  let pm_mapping port =
    { Rpc_portmapper_aux.prog = prog_nr;
      vers = vers_nr;
      prot = ( match srv.prot with
                 | Tcp -> Rpc_portmapper_aux.ipproto_tcp
                 | Udp -> Rpc_portmapper_aux.ipproto_udp
             );
      port = Rtypes.uint4_of_int port;
    } in

  let pm_error pm error =
    Rpc_client.shut_down pm;
    (try srv.exception_handler error with _ -> ()) in

  let pm_get_old_port pm f =
    Rpc_portmapper_clnt.PMAP.V2.pmapproc_getport'async pm (pm_mapping 0)
      (fun get_result ->
	 try
	   let old_port = get_result() in
	   f (Rtypes.int_of_uint4 old_port)
	 with
	   | error -> pm_error pm error
      ) in

  let pm_unset_old_port pm old_port f =
    dlogr srv
      (fun () ->
	 sprintf "unregistering port: %d" old_port);
    Rpc_portmapper_clnt.PMAP.V2.pmapproc_unset'async pm (pm_mapping old_port)
      (fun get_result ->
	 try
	   let success = get_result() in
	   dlogr srv
	     (fun () ->
		sprintf "portmapper reports %s"
		  (if success then "success" else "failure"));
	   if not success then
	     failwith "Rpc_server.bind: Cannot unregister old port";
	   f ()
	 with
	   | error -> pm_error pm error
      ) in

  let pm_set_new_port pm new_port f =
    dlogr srv
      (fun () ->
	 sprintf "registering port: %d" new_port);
    Rpc_portmapper_clnt.PMAP.V2.pmapproc_set'async pm (pm_mapping new_port)
      (fun get_result ->
	 try
	   let success = get_result() in
	   dlogr srv
	     (fun () ->
		sprintf "portmapper reports %s"
		  (if success then "success" else "failure"));
	   if not success then
	     failwith "Rpc_server.bind: Cannot register port";
	   f ()
	 with
	   | error -> pm_error pm error
      ) in

  let pm_update_service pm () =
    update_service();
    Rpc_client.shut_down pm in

  match srv.portmapped with
    | None ->
	update_service()

    | Some port ->
	let pmap_port =
	  Rtypes.int_of_uint4
	    Rpc_portmapper_aux.pmap_port in
	let pm =
	  Rpc_portmapper_clnt.PMAP.V2.create_client2 
	    ~esys:srv.esys 
	    (`Socket(Rpc.Udp, 
		     Rpc_client.Inet("127.0.0.1", pmap_port), 
		     Rpc_client.default_socket_config)) in
	pm_get_old_port 
	  pm
	  (fun old_port ->
	     if old_port > 0 then (
	       pm_unset_old_port pm old_port
		 (fun () ->
		    pm_set_new_port pm port (pm_update_service pm)
		 )
	     )
	     else
	       pm_set_new_port pm port (pm_update_service pm)
	  )
;;


let unbind' ?(followup = fun () -> ()) 
            prog_nr vers_nr srv =

  let update_service() =
    let old_progbinding =
      try Uint4Map.find prog_nr srv.service
      with Not_found -> Uint4Map.empty in
    
    let exists = 
      try ignore(Uint4Map.find vers_nr old_progbinding); true
      with Not_found -> false in

    let progbinding =
      Uint4Map.remove vers_nr old_progbinding in
    
    srv.service <-
      if progbinding = Uint4Map.empty then
	Uint4Map.remove prog_nr srv.service
      else
	Uint4Map.add prog_nr progbinding srv.service;

    exists
  in

  let pm_mapping port =
    { Rpc_portmapper_aux.prog = prog_nr;
      vers = vers_nr;
      prot = ( match srv.prot with
                 | Tcp -> Rpc_portmapper_aux.ipproto_tcp
                 | Udp -> Rpc_portmapper_aux.ipproto_udp
             );
      port = Rtypes.uint4_of_int port;
    } in

  let pm_error pm error =
    Rpc_client.shut_down pm;
    (try srv.exception_handler error with _ -> ()) in

  let pm_unset_port pm port f =
    dlogr srv
      (fun () ->
	 sprintf "unregistering port: %d" port);
    Rpc_portmapper_clnt.PMAP.V2.pmapproc_unset'async pm (pm_mapping port)
      (fun get_result ->
	 try
	   let success = get_result() in
	   dlogr srv
	     (fun () ->
		sprintf "portmapper reports %s"
		  (if success then "success" else "failure"));
	   if not success then
	     failwith "Rpc_server.unbind: Cannot unregister port";
	   f ()
	 with
	   | error -> pm_error pm error
      ) in

  match srv.portmapped with
    | None ->
	ignore(update_service());
	followup()

    | Some port ->
	let exists = update_service() in

	if exists then (
	  let pmap_port =
	    Rtypes.int_of_uint4
	      Rpc_portmapper_aux.pmap_port in
	  let pm =
	    Rpc_portmapper_clnt.PMAP.V2.create_client2 
	      ~esys:srv.esys 
	      (`Socket(Rpc.Udp, 
		       Rpc_client.Inet("127.0.0.1", pmap_port), 
		       Rpc_client.default_socket_config)) 
	  in
	  pm_unset_port pm port (fun () -> Rpc_client.shut_down pm)
	)
;;


let unbind ?program_number ?version_number prog0 srv =
  let prog = Rpc_program.update ?program_number ?version_number prog0 in
  let prog_nr = Rpc_program.program_number prog in
  let vers_nr = Rpc_program.version_number prog in
  unbind' prog_nr vers_nr srv ;;


let unbind_all srv =
  let rec next l =
    match l with
      | [] -> ()
      | (prog_nr, vers_nr) :: l'->
	  unbind' ~followup:(fun () -> next l') prog_nr vers_nr srv
  in
  let l = ref [] in
  Uint4Map.iter
    (fun prog_nr progbinding ->
       Uint4Map.iter
	 (fun vers_nr _ ->
	    l := (prog_nr, vers_nr) :: !l
	 )
	 progbinding
    )
    srv.service;
  next !l
;;


let bound_programs srv =
  let l = ref [] in
  Uint4Map.iter
    (fun prog_nr progbinding ->
       Uint4Map.iter
	 (fun vers_nr (prog,_) ->
	    l := prog :: !l
	 )
	 progbinding
    )
    srv.service;
  !l
;;


let create ?program_number ?version_number
           esys conn prot mode prog0 procs max_clients =

  (* Backwards-compatible! *)

  let srv =
    match mode with
      | BiPipe ->
	  let fd, close_inactive_descr =
	    match conn with
	      | Descriptor fd -> (fd, false)
	      | Dynamic_descriptor f -> ( f(), true )
	      | _ ->
		  invalid_arg "Rpc_server.create: mode incompatible with connector" in
	  create2_socket_endpoint ~close_inactive_descr prot fd esys
      | Socket ->
	  create2_socket_server
	    ~override_listen_backlog:max_clients
	    prot conn esys 
  in
  bind ?program_number ?version_number prog0 procs srv;
  srv
;;


  (*****)

let get_event_system a_session =
    a_session.server.whole_server.esys

let get_connection_id a_session =
    a_session.sess_conn_id

let get_xid a_session =
    a_session.client_id

let get_socket_name a_session =
    Lazy.force a_session.sockaddr

let get_peer_name a_session =
  match a_session.peeraddr with
    | `Implied -> failwith "Cannot determine peer socket name"
    | `Sockaddr a -> a

let get_conn_socket_name conn_id = conn_id # socket_name

let get_conn_peer_name conn_id = conn_id # peer_name

let get_protocol srv = srv.prot

let get_srv_event_system srv = srv.esys

let get_main_socket_name srv =
  match srv.main_socket_name with
    | `Implied -> failwith "Cannot determine main socket name"
    | `Sockaddr a -> a

let get_server sess = sess.server.whole_server

let get_user sess = sess.auth_user

let get_auth_method sess = sess.auth_method

let get_last_proc_info srv = srv.get_last_proc()

  (*****)

let reply_error a_session condition =
    let conn = a_session.server in
    let srv = conn.whole_server in
    if conn.trans = None then raise Connection_lost;

    let reply =
      match condition with
	  Unavailable_program
	| Unavailable_version(_,_)
	| Unavailable_procedure
	| Garbage
	| System_err ->
	    Rpc_packer.pack_accepting_reply
	      a_session.client_id
	      a_session.auth_ret_flav a_session.auth_ret_data
              condition
	| _ ->
	    Rpc_packer.pack_rejecting_reply
	      a_session.client_id condition
    in

    let reply_session =
      { a_session with
	  parameter = XV_void;
	  result = reply;
	  ptrace_result = (if !Debug.enable_ptrace then
			     "Error " ^ errname condition
			   else ""
			  )

      }
    in

    Queue.add reply_session conn.replies;

    next_outgoing_message srv conn


let reply a_session result_value =
  let conn = a_session.server in
  let srv = conn.whole_server in

  dlogr srv
    (fun () ->
       sprintf "reply xid=%Ld have_encoder=%B"
	 (Rtypes.int64_of_uint4 a_session.client_id)
	 (a_session.encoder <> None)
    );
  
  if conn.trans = None then raise Connection_lost;
  
  let prog =
    match a_session.prog with
      | None -> assert false
      | Some p -> p in

  let f =
    try
      let reply = Rpc_packer.pack_successful_reply
	?encoder:a_session.encoder
	prog a_session.procname a_session.client_id
	a_session.auth_ret_flav a_session.auth_ret_data
	result_value
      in
  
      let reply_session =
	{ a_session with
	    parameter = XV_void;
	    result = reply;
	    ptrace_result = (if !Debug.enable_ptrace then
	  		       Rpc_util.string_of_response
				 !Debug.ptrace_verbosity
				 prog
			       a_session.procname
				 result_value
			     else ""
			    )
	}
      in
      (fun () ->
	 Queue.add reply_session conn.replies;
	 next_outgoing_message srv conn
      )
    with (* exceptions raised by the encoder *)
      | Late_drop ->
	  Netlog.logf `Err
	    "Dropping response message";
	  (fun () -> ())
      | Rpc_server condition ->
	  reply_error a_session condition;
	  (fun () -> ())
  in
  f()
    

let set_exception_handler srv eh =
  srv.exception_handler <- eh

let set_onclose_action srv a =
  srv.onclose <- a :: srv.onclose

let set_session_filter_2 srv f =
  srv.filter <- f

let set_session_filter srv f =
  srv.filter <- (fun addr conn_id -> f addr)

let set_auth_methods srv l =
  let h = Hashtbl.create 20 in
  let p = ref [] in
  List.iter
    (fun m ->
       List.iter (fun name -> Hashtbl.add h name m) m#flavors;
       match m # peek with
	 | `None -> ()
	 | other -> p := !p @ [ other, m ];
    )
    l;
  srv.auth_methods <- h;
  srv.auth_peekers <- !p

let set_timeout srv tmo =
  srv.transport_timeout <- tmo

let set_mstring_factories srv fac =
  srv.mstring_factories <- fac

let stop_server ?(graceful = false) srv =
  dlogr srv
    (fun () ->
       sprintf "Stopping %s" (if graceful then " gracefully" else ""));
  (* Close TCP server socket, if present: *)
  ( match srv.master_acceptor with
      | Some acc -> 
	  acc # shut_down();
	  srv.master_acceptor <- None
      | None -> ()
  );
  unbind_all srv;
  if graceful then (
    let l = srv.connections in
    List.iter
      (fun conn ->
	 if Queue.is_empty conn.replies then
	   terminate_any srv conn;
	 conn.close_when_empty <- true
      )
      l;
  ) else (
    let l = srv.connections in
    srv.connections <- [];
    List.iter
      (fun conn ->
	 terminate_any srv conn
      )
      l
  )

let stop_connection srv conn_id =
  if srv.prot = Tcp then (
    try
      let conn =
	List.find
	  (fun c -> c.conn_id = conn_id)
	  srv.connections in
      terminate_connection srv conn
    with
      | Not_found -> ()
  )

let detach srv =
  (* Detach from connections: *)
    let l = srv.connections in
    srv.connections <- [];
    List.iter
      (fun conn ->
	 conn.fd <- None;
	 match conn.trans with
	   | Some t -> 
	       t#abort_rw();
	       t#cancel_shutting_down()
	   | None -> ()
      )
      l
  
  

let verbose b =
  Debug.enable := b;
  Debug.enable_ctrace := b
