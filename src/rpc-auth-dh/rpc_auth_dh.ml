(* $Id: rpc_auth_dh.ml 1553 2011-02-28 00:12:27Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* See RFC 2695 *)

open Xdr
open Rtypes
open Rpc

module DS = Crypt_des.Cryptsystem;;   (* DS = DES system *)
module DM = Crypt_des.Cryptmodes;;    (* DM = DES modes *)

module A = Rpc_key_aux;;

type state =
    Init
      (* Not yet authenticated *)
  | Resynchronize
      (* like Init, but resynchronize time first *)
  | Auth_fullname_sent of (DS.key * float * int32 * int32)
      (* The fullname has been sent to the server. The parameters are the
       * conversation key, the time when the key was generated, and the
       * timestamp (seconds, microseconds)
       *)
  | Auth_nickname_sent of (DS.key * float * int32 * int32)
      (* The nickname has been sent to the server. The parameters are the
       * conversation key, the time when the key was generated, and the
       *  timestamp (seconds, microseconds)
       *)
  | Auth_accepted of (DS.key * float * int32)
      (* The credentials have been accepted by the server. The parameters
       * are the conversation key, the time when the key was generated, and
       * the nickname to use for further calls.
       *)


let domainname() =
  (* Return the NIS domain name. Just calling "domainname" seems to be the
   * most portable way.
   *)
  (* TODO: Better do not invoke an external program here. May be a security
   * problem with setuid programs
   *)
  if Unix.getuid() <> Unix.geteuid() then
    failwith "Rpc_auth_dh.domainname (setuid)";
  if Unix.getgid() <> Unix.getegid() then
    failwith "Rpc_auth_dh.domainname (setgid)";

  let f = Unix.open_process_in "domainname" in
  try
    let name = input_line f in
    match Unix.close_process_in f with
	Unix.WEXITED 0 ->
	  name
      | _ ->
	  failwith "Rpc_auth_dh.domainname"
    with
	End_of_file ->
	  ignore(Unix.close_process_in f);
	  failwith "Rpc_auth_dh.domainname"
;;


class client_auth_session proto getdeviation ttl key_lifetime ksconn srv_netname
  : Rpc_client.auth_session =
object
  val mutable state = Init
  val mutable n_failures = 0

  val keyserv = lazy (Rpc_key_service.create ?connector:ksconn ())
  val ttl = ttl
  val key_lifetime = key_lifetime

  val mutable current_time_deviation = 0.0
      (* positive means: the time of the server is ahead *)

  val authdh_cred_type =
    Xdr.validate_xdr_type A.xdrt_authdh_cred
  val authdh_fullname_verf_type =
    Xdr.validate_xdr_type A.xdrt_authdh_fullname_verf
  val authdh_nickname_verf_type =
    Xdr.validate_xdr_type A.xdrt_authdh_nickname_verf
  val authdh_server_verf_type =
    Xdr.validate_xdr_type A.xdrt_authdh_server_verf

  method auth_protocol = proto

  method next_credentials client prog proc xid =
    (* Do we need to resynchronize? *)
    if state = Resynchronize then begin
      match Rpc_client.get_peer_name client with
	  Unix.ADDR_UNIX _ ->
	    state <- Init;   (* do not resychronize *)
	| Unix.ADDR_INET(host,_) ->
	    current_time_deviation <- getdeviation host;
	    state <- Init;
    end;
    (* Do we need a new conversation key? *)
    ( match state with
	  Auth_accepted(_,kt,_) ->
	    if (Unix.time() -. kt) > float key_lifetime then
	      state <- Init
	| _ ->
	    ()
    );
    (* Protocol implementation: *)
    match state with
	Resynchronize ->
	  assert false
      | Init
      | Auth_fullname_sent(_,_,_,_)        (* handle this case for robustness *)
      | Auth_nickname_sent(_,_,_,_) ->     (* handle this case for robustness *)
	  (* Send AUTH_DH credentials with fullname: *)
	  (* First, create a new conversation key. *)
	  let keyserv = Lazy.force keyserv in
	  let conv_key = Rpc_key_service.generate keyserv in
	  assert(String.length conv_key = 8);
	  let conv_key' = DS.prepare conv_key in

	  (* Encrypt the timestamp and the ttl value: *)
	  let _now = Unix.gettimeofday() in
	  let now = _now +. current_time_deviation -. float (ttl/2) in
	  let now_secs = Int32.of_float now in
	  let now_usecs = Int32.of_float
			    (now *. 1000000.0 -.
			     Int32.to_float now_secs *. 1000000.0) in
	  let ttl0 = Int32.of_int ttl in
	  let ttl1 = Int32.of_int (ttl-1) in

	  let timestamp = int4_as_string (int4_of_int32 now_secs) ^
			  int4_as_string (int4_of_int32 now_usecs) ^
			  int4_as_string (int4_of_int32 ttl0) ^
			  int4_as_string (int4_of_int32 ttl1) in
	  assert(String.length timestamp = 16);

	  let _, t1t2w1w2 =
	    DM.encrypt_cbc conv_key' (0,0,0,0) timestamp in

	  let t  = String.sub t1t2w1w2 0  8 in
	  let w1 = String.sub t1t2w1w2 8  4 in
	  let w2 = String.sub t1t2w1w2 12 4 in

	  (* Encrypt the conversation key: *)
	  let enc_conv_key = Rpc_key_service.encrypt
			       keyserv srv_netname conv_key in

	  (* Get the client's netname: *)
	  let clnt_netname,_,_ = Rpc_key_service.net_get keyserv in

	  (* Pack the result: *)
	  let fullname = { A.name = clnt_netname;
			   A.key = enc_conv_key;
			   A.w1 = w1;
			 } in
	  let fullname_verf = { A.full_ts = t;
				A.w2 = w2;
			      } in
	  let cred = `adn_fullname fullname in
	  let xdr_cred = A._of_authdh_cred cred in
	  let xdr_verf = A._of_authdh_fullname_verf fullname_verf in

	  state <- Auth_fullname_sent(conv_key', _now, now_secs, now_usecs);

	  ("AUTH_DH", Xdr.pack_xdr_value_as_string
	                xdr_cred authdh_cred_type [],
	   "AUTH_DH", Xdr.pack_xdr_value_as_string
	                xdr_verf authdh_fullname_verf_type [],
	   None,
	   None
	  )

	  (* Note: IMHO, it is wrong that the verifier is not a discriminated
	   * union like the credential. RFC 2695 is very clear about that.
	   * However, existing software assumes that the verifier is either
	   * a fullname verifier or a nickname verifier without discriminator.
	   * (It took several hours to find this out.)
	   *)

      | Auth_accepted(key,keytime,nickname) ->
	  (* Send nickname credentials: *)
	  (* New timestamp: *)
	  let now = Unix.gettimeofday() +. current_time_deviation -.
		      float (ttl/2) in
	  let now_secs = Int32.of_float now in
	  let now_usecs = Int32.of_float
			    (now *. 1000000.0 -.
			     Int32.to_float now_secs *. 1000000.0) in
	  (* Encrypt the timestamp: *)
	  let enc_secs = ref Int32.zero in
	  let enc_usecs = ref Int32.zero in
	  DS.encrypt_ecb_int32 key now_secs now_usecs enc_secs enc_usecs;
	  let enc_timestamp = int4_as_string (int4_of_int32 !enc_secs) ^
			      int4_as_string (int4_of_int32 !enc_usecs) in
	  (* Pass result: *)
	  let nickname = { A.nickname = nickname } in
	  let cred = `adn_nickname nickname in
	  let nickname_verf = { A.nick_ts = enc_timestamp;
				A.w = "\000\000\000\000";
			      } in
	  let xdr_cred = A._of_authdh_cred cred in
	  let xdr_verf = A._of_authdh_nickname_verf nickname_verf in

	  state <- Auth_nickname_sent(key, keytime, now_secs, now_usecs);

	  ("AUTH_DH", Xdr.pack_xdr_value_as_string
	                xdr_cred authdh_cred_type [],
	   "AUTH_DH", Xdr.pack_xdr_value_as_string
	                xdr_verf authdh_nickname_verf_type [],
	   None, None
	  )

  method server_rejects clnt xid err =
    if err = Auth_too_weak then
      `Next
    else (
      n_failures <- n_failures + 1;
      if n_failures >= 3 then raise (Rpc_server err);
      match state with
	  Auth_fullname_sent(_,_,_,_)
	| Auth_nickname_sent(_,_,_,_) ->
	    if err = Auth_rejected_verf then
	      state <- Resynchronize
	    else
	      state <- Init;
	    `Retry
	| _ ->
	    assert false
    )

  method server_accepts clnt xid flav data =
    let check_verifier key kt secs usecs =
      if flav <> "AUTH_DH" then raise(Rpc_server Auth_invalid_resp);
      let xdr = Xdr.unpack_xdr_value ~fast:true data authdh_server_verf_type [] in
      let verf = A._to_authdh_server_verf xdr in
      let enc_ts = verf.A.timestamp_verf in
      (* Decrypt the timestamp: *)
      let enc_ts_msb = Rtypes.mk_int4
			 (enc_ts.[0],enc_ts.[1],enc_ts.[2],enc_ts.[3]) in
      let enc_ts_lsb = Rtypes.mk_int4
			 (enc_ts.[4],enc_ts.[5],enc_ts.[6],enc_ts.[7]) in
      let ts_secs = ref Int32.zero in
      let ts_usecs = ref Int32.zero in
      DS.decrypt_ecb_int32
	key
	(Rtypes.int32_of_int4 enc_ts_msb)
	(Rtypes.int32_of_int4 enc_ts_lsb)
	ts_secs
	ts_usecs;
      (* Check that ts_secs = secs - 1: *)
      if Int32.pred secs <> !ts_secs || usecs <> !ts_usecs then
	raise (Rpc_server Auth_invalid_resp);
      (* Ok, the server has been verified. *)
      state <- Auth_accepted(key, kt, verf.A.new_nickname)
    in
    n_failures <- 0;
    match state with
	Auth_fullname_sent(key,kt,secs,usecs) ->
	  check_verifier key kt secs usecs
      | Auth_nickname_sent(key,kt,secs,usecs) ->
	  check_verifier key kt secs usecs
      | _ ->
	  assert false
end


let getdeviation peer =
  try
    Rpc_time.remote_time peer -. Unix.time()
  with
      Rpc_time.Time_not_available -> 0.0
;;


class client_auth_method ?(ttl = 60)
                         ?(getdeviation = getdeviation)
			 ?(key_lifetime = 3600)
			 ?keyserv
                         srv_netname
  : Rpc_client.auth_method =
object(self)
  method name = "AUTH_DH"
  method new_session clnt user_opt =
    if user_opt <> None then
      failwith "Rpc_auth_dh: cannot set user name freely";
    let s_opt = ref None in
    ( object(iself)
	initializer
	let s =
	  new client_auth_session
	    (iself : Rpc_client.auth_protocol)
	    getdeviation ttl key_lifetime keyserv srv_netname in
	s_opt := Some s

	method state = 
	  match !s_opt with
	    | None -> assert false
	    | Some s -> `Done s
	method emit _ = assert false
	method receive _ = assert false
	method auth_method = (self : #Rpc_client.auth_method :> 
				Rpc_client.auth_method)
      end
    )
    
end


let client_auth_method = new client_auth_method


type session =
    { mutable sess_used : bool;
      mutable sess_time_created : float;     (* server time *)
      mutable sess_ttl : int;
      mutable sess_timestamp_secs : float;   (* the timestamp from the client *)
      mutable sess_timestamp_usecs : int;
      mutable sess_sockaddr : Unix.sockaddr option;
      mutable sess_key : DS.key;
      mutable sess_user : string;
    }


let a_key = DS.prepare "XXXXXXXX";;

let empty_session() =
  { sess_used = false;
    sess_time_created = 0.0;
    sess_ttl = 0;
    sess_timestamp_secs = 0.0;
    sess_timestamp_usecs = 0;
    sess_key = a_key;
    sess_sockaddr = None;
    sess_user = "";
  }


type user_rec =  (* for the attack detector *)
    { mutable current_period : int;
      mutable failures : int;
    }

module CID = struct  (* connection_id as ordered type *)
  type t = Rpc_server.connection_id
  let compare = (Pervasives.compare : t -> t -> int)
end

module CID_Set = Set.Make(CID);;

module TS = struct (* timestamps as ordered type *)
  type t = float * int      (* seconds, microseconds *)
  let compare (a_secs,a_usecs) (b_secs,b_usecs) =
    match Pervasives.compare a_secs b_secs with
	0 -> Pervasives.compare a_usecs b_usecs
      | n -> n
end

module TS_Set = Set.Make(TS);;


let _999999 = Int32.of_string "999999";;
let _max_int = Int32.of_int max_int;;

class server_auth_method ?(max_sessions = 1000)
                         ?(max_ttl = 60)
			 ?(key_lifetime = 3600)
			 ?(attack_detector = true)
                         ?keyserv
                         ()
  : Rpc_server.auth_method =
object(self)
  method name = "AUTH_DH"
  method flavors = [ "AUTH_DH" ]

  val ks = lazy (Rpc_key_service.create ?connector:keyserv ())
  val max_ttl = max_ttl
  val key_lifetime = key_lifetime
  val user_recs = Hashtbl.create 100
  val attack_detector = attack_detector

  val authdh_cred_type =
    Xdr.validate_xdr_type A.xdrt_authdh_cred
  val authdh_fullname_verf_type =
    Xdr.validate_xdr_type A.xdrt_authdh_fullname_verf
  val authdh_nickname_verf_type =
    Xdr.validate_xdr_type A.xdrt_authdh_nickname_verf
  val authdh_server_verf_type =
    Xdr.validate_xdr_type A.xdrt_authdh_server_verf
  val authdh_timestamp_type =
    Xdr.validate_xdr_type A.xdrt_authdh_timestamp

  val sessions = Array.init max_sessions (fun _ -> empty_session())
  val squeue = Queue.create()
		 (* The oldest session index first; the youngest last *)

  val mutable trusted = CID_Set.empty
		   (* The trusted set of connection_id *)
  val mutable n_trusted = 0
			    (* length of [trusted] *)

     (* In [trusted], the connection IDs of authenticated sessions are
      * queued up. The queue grows up to [max_sessions] entries; if further
      * connections are accepted, the smallest ID is removed.
      *
      * It would be much better if we could find out which connections are
      * really alive (TODO). Currently, it is possible that existing connections
      * are deleted from the queue, and that broken connections are contained
      * in the queue. It is only a heuristics to prevent immediate denial
      * of service if the server is under attack.
      *)

  val mutable timestamps = TS_Set.empty
			     (* Set of authenticated timestamps *)

    (* The [timestamps] are stored to prevent replay attacks with
     * fullname credentials. A credential with the same timestamp as
     * an already accepted credential is rejected.
     *)

  initializer
    Array.iteri
      (fun k _ -> Queue.add k squeue)
      sessions


  method private get_session user key addr =
    let k = Queue.take squeue in
    Queue.add k squeue;
    let s = sessions.(k) in
    s.sess_used <- true;
    s.sess_time_created <- Unix.time();
    s.sess_user <- user;
    s.sess_key <- key;
    s.sess_sockaddr <- addr;
    k, s


  method private cleanup_timestamps now =
    (* Delete entries from [timestamps] that are too old *)
    try
      while
	let (ts_secs, ts_usecs) as ts = TS_Set.min_elt timestamps in
	( ts_secs < now -. float max_ttl ) &&
	( timestamps <- TS_Set.remove ts timestamps; true )
      do
	()
      done
    with
	Not_found ->
	  ()

  method peek = `None

  method authenticate
           srv cnid details pass =

    let cred_flav, cred_data = details # credential in
    let verf_flav, verf_data = details # verifier in
    let peername = details # client_addr in

    let protect username f =
      (* If there are more than 10 failures in the current period of 10 seconds,
       * no more calls are accepted.
       * This check is done for every user separetely.
       *)
      let now = truncate (Unix.time() *. 0.1) in
      let user_rec =
	try
	  Hashtbl.find user_recs username
	with
	    Not_found ->
	      let new_user_rec = { current_period = now; failures = 0 } in
	      Hashtbl.add user_recs username new_user_rec;
	      new_user_rec
      in
      if attack_detector &&
	           user_rec.current_period = now && user_rec.failures > 10
      then
	begin
	  (* Last chance: [cnid] is member of [trusted] *)
	  if not (CID_Set.mem cnid trusted) then
	    raise(Rpc_server Auth_bad_cred);
	end;
      try
	f()
      with
	  Rpc_server _ as err ->
	    (* CHECK: We can ignore Auth_rejected_cred here because it
	     * indicates a replay attack, and there is already protection
	     * against this type of attack
	     *)
	    if user_rec.current_period <> now then begin
	      user_rec.current_period <- now;
	      user_rec.failures <- 0
	    end;
	    user_rec.failures <- user_rec.failures + 1;
	    raise err
    in

    let pass_successful_result nickindex s seconds =
      (* Create the verifier to return: *)
      let ts_1 = Int32.pred seconds in
      let enc_ts_msb = ref Int32.zero in
      let enc_ts_lsb = ref Int32.zero in
      DS.encrypt_ecb_int32
	s.sess_key
	ts_1
	(Int32.of_int s.sess_timestamp_usecs)
	enc_ts_msb
	enc_ts_lsb;
      let t =
	Rtypes.int4_as_string (Rtypes.int4_of_int32 !enc_ts_msb) ^
	Rtypes.int4_as_string (Rtypes.int4_of_int32 !enc_ts_lsb) in
      let verf =
	{ A.timestamp_verf = t;
	  A.new_nickname = Int32.of_int nickindex;
	} in
      let ret_data =
	Xdr.pack_xdr_value_as_string
	  (A._of_authdh_server_verf verf) authdh_server_verf_type [] in
      pass(Rpc_server.Auth_positive(s.sess_user,
				    "AUTH_DH",
				    ret_data,None,None))
    in

    let now = Unix.gettimeofday() in
    self # cleanup_timestamps now;
    (* Unpack credentials: *)
    let cred_xdr = Xdr.unpack_xdr_value ~fast:true cred_data authdh_cred_type [] in
    let cred = A._to_authdh_cred cred_xdr in
    if verf_flav <> "AUTH_DH" then raise(Rpc_server Auth_bad_cred);
    if String.length verf_data <> 12 then raise(Rpc_server Auth_bad_cred);
    (* Is it a fullname or a nickname? *)
    begin match cred with
	`adn_fullname fn ->
	  let clnt_netname = fn.A.name in
	  (* Decrypt the conversation key: *)
	  (* TODO: Do this asynchronously *)
	  let conv_key =
	    try
	      Rpc_key_service.decrypt (Lazy.force ks) clnt_netname fn.A.key
	    with
		(* Any problem leads to rejection of the credentials.
		 * Note: This may be difficult to debug
		 *)
		_ -> raise(Rpc_server Auth_bad_cred)
	  in
	  let conv_key' =
	    try DS.prepare conv_key
	    with Failure _ -> raise(Rpc_server Auth_bad_cred)
	  in
	  (* Decrypt timestamp and ttl value: *)
	  let t1t2w1w2 = String.sub verf_data 0 8 ^
			 fn.A.w1 ^
			 String.sub verf_data 8 4 in
	  let _,ts_string =
	    DM.decrypt_cbc conv_key' (0,0,0,0) t1t2w1w2 in
	  let ts = A._to_authdh_timestamp
		     (Xdr.unpack_xdr_value ~fast:true ts_string authdh_timestamp_type [])
	  in
	  (* Check timestamp and ttl values: *)
	  protect clnt_netname
	    (fun () ->
	       let ttl = ts.A.ttl in
	       let ttl_1 = ts.A.ttl_1 in
	       let eff_timestamp_secs =
		 if ts.A.seconds < Int32.zero then
		   4294967296.0 +. Int32.to_float ts.A.seconds
		 else
		   Int32.to_float ts.A.seconds
	       in
	       if ts.A.useconds < Int32.zero || ts.A.useconds > _999999 then
		 raise(Rpc_server Auth_bad_verf);
	       let eff_timestamp_usecs = Int32.to_int (ts.A.useconds) in
	       if TS_Set.mem
		    (eff_timestamp_secs, eff_timestamp_usecs)
		    timestamps then raise (Rpc_server Auth_rejected_cred);
	       if ttl_1 <= Int32.zero || ttl <> Int32.succ ttl_1 then
		 raise(Rpc_server Auth_bad_cred);
	       let eff_ttl =
		 if ttl > _max_int then
		   max_ttl
		 else
		   min max_ttl (Int32.to_int ttl)
	       in
	       let eff_timestamp = eff_timestamp_secs +.
				     (float eff_timestamp_usecs *. 0.000001) in
	       if eff_timestamp > now || eff_timestamp < now -. float eff_ttl
	       then raise(Rpc_server Auth_bad_cred);
	       (* The call is accepted! *)
	       let nickindex, s =
		 self # get_session clnt_netname conv_key' peername in
	       s.sess_ttl <- eff_ttl;
	       s.sess_timestamp_secs <- eff_timestamp_secs;
	       s.sess_timestamp_usecs <- eff_timestamp_usecs;
	       (* Update [trusted] *)
	       trusted <- CID_Set.add cnid trusted;
	       if n_trusted >= max_sessions then
		 trusted <- CID_Set.remove (CID_Set.min_elt trusted) trusted
	       else
		 n_trusted <- n_trusted + 1;
	       (* Update [timestamps] *)
	       timestamps <- TS_Set.add
		               (eff_timestamp_secs, eff_timestamp_usecs)
		               timestamps;
	       (* Return result: *)
	       pass_successful_result nickindex s ts.A.seconds
	    );

      | `adn_nickname nn ->
	  let nn_num = nn.A.nickname in
	  if nn_num < Int32.zero ||
	       nn_num >= Int32.of_int(Array.length sessions) then
		 raise(Rpc_server Auth_bad_cred);
	  let nickindex = Int32.to_int nn_num in
	  let s = sessions.(nickindex) in
	  if not s.sess_used then raise(Rpc_server Auth_bad_cred);
	  if s.sess_sockaddr <> peername then raise(Rpc_server Auth_bad_cred);
	  (* Temporarily set session to unused (to prevent brute-force attacks
	   * on nicknames). If an error occurs, the session remains unused.
	   *)
	  s.sess_used <- false;
	  let verf =
	    A._to_authdh_nickname_verf
	      (Xdr.unpack_xdr_value ~fast:true verf_data authdh_nickname_verf_type []) in
	  let enc_ts = verf.A.nick_ts in
	  if verf.A.w <> "\000\000\000\000" then
	    raise(Rpc_server Auth_bad_cred);  (* not strictly necessary *)
	  (* Decrypt enc_ts: *)
	  let enc_ts_msb = Rtypes.mk_int4
			     (enc_ts.[0],enc_ts.[1],enc_ts.[2],enc_ts.[3]) in
	  let enc_ts_lsb = Rtypes.mk_int4
			     (enc_ts.[4],enc_ts.[5],enc_ts.[6],enc_ts.[7]) in
	  let ts_secs = ref Int32.zero in
	  let ts_usecs = ref Int32.zero in
	  DS.decrypt_ecb_int32
	    s.sess_key
	    (Rtypes.int32_of_int4 enc_ts_msb)
	    (Rtypes.int32_of_int4 enc_ts_lsb)
	    ts_secs
	    ts_usecs;
	  if !ts_usecs < Int32.zero || !ts_usecs > _999999 then
	    raise(Rpc_server Auth_bad_verf);
	  (* Check whether timestamp is in the accepted window: *)
	  if now >= s.sess_time_created +. float key_lifetime then
	    raise(Rpc_server Auth_rejected_verf);  (* session timeout *)
	  let eff_timestamp_secs =
	    if !ts_secs < Int32.zero then
	      4294967296.0 +. Int32.to_float !ts_secs
	    else
	      Int32.to_float !ts_secs
	  in
	  if TS.compare
	        (eff_timestamp_secs, Int32.to_int !ts_usecs)
	        (s.sess_timestamp_secs, s.sess_timestamp_usecs) <= 0 then
            raise(Rpc_server Auth_rejected_verf);
	    (* The same timestamp may arrive again because of retransmissions *)
	  let eff_timestamp = eff_timestamp_secs +.
				(Int32.to_float !ts_usecs *. 0.000001) in
	  let eff_ttl = s.sess_ttl in
	  if eff_timestamp > now || eff_timestamp < now -. float eff_ttl
	    then raise(Rpc_server Auth_rejected_verf);
	  (* The call is accepted! *)
	  (* Set session again to used *)
	  s.sess_used <- true;
	  s.sess_timestamp_secs <- eff_timestamp_secs;
	  s.sess_timestamp_usecs <- Int32.to_int !ts_usecs;
	  pass_successful_result nickindex s !ts_secs

    end
end;;

let server_auth_method = new server_auth_method

