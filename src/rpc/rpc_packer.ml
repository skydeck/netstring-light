(* $Id: rpc_packer.ml 1555 2011-03-03 01:18:51Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* packs procedure calls using XDR *)

open Rtypes
open Xdr
open Rpc
open Rpc_common

(* declaration of the RPC message type system: *)

let auth_flavor_of_pos =
  (* map XV_enum_fast-style _positions_ to names *)
  function
      0 -> "AUTH_NONE"
    | 1 -> "AUTH_SYS"
    | 2 -> "AUTH_SHORT"
    | 3 -> "AUTH_DH"
    | 4 (* sic *) -> "RPCSEC_GSS"
    | _ -> assert false
;;


let rpc_ts_unvalidated =
  [ "auth_flavor",         X_enum [ "AUTH_NONE",     int4_of_int 0;
				       (* also known as AUTH_NULL *)
				    "AUTH_SYS",      int4_of_int 1;
				    "AUTH_SHORT",    int4_of_int 2;
				    "AUTH_DH",       int4_of_int 3;
				       (* also known as AUTH_DES *)
				    "RPCSEC_GSS",    int4_of_int 6;
				  ];

    "opaque_auth",         X_struct [ "flavor", X_type "auth_flavor";
				      "body",   X_opaque (uint4_of_int 400) ];

    "msg_type",            X_enum [ "CALL",          int4_of_int 0;
				    "REPLY",         int4_of_int 1 ];

    "reply_stat",          X_enum [ "MSG_ACCEPTED",  int4_of_int 0;
				    "MSG_DENIED",    int4_of_int 1 ];

    "accept_stat",         X_enum [ "SUCCESS",       int4_of_int 0;
				    "PROG_UNAVAIL",  int4_of_int 1;
				    "PROG_MISMATCH", int4_of_int 2;
				    "PROC_UNAVAIL",  int4_of_int 3;
				    "GARBAGE_ARGS",  int4_of_int 4;
				    "SYSTEM_ERR",    int4_of_int 5;
				  ];

    "reject_stat",         X_enum [ "RPC_MISMATCH",  int4_of_int 0;
				    "AUTH_ERROR",    int4_of_int 1 ];

    "auth_stat",           X_enum [ "AUTH_BADCRED",       int4_of_int 1;
				    "AUTH_REJECTEDCRED",  int4_of_int 2;
				    "AUTH_BADVERF",       int4_of_int 3;
				    "AUTH_REJECTEDVERF",  int4_of_int 4;
				    "AUTH_TOOWEAK",       int4_of_int 5;
				    "AUTH_INVALIDRESP",   int4_of_int 6;
				    "AUTH_FAILED",        int4_of_int 7;
				    "RPCSEC_GSS_CREDPROBLEM", int4_of_int 13;
				    "RPCSEC_GSS_CTXPROBLEM",  int4_of_int 14;
				  ];

    "call_body",           X_struct [ "rpcvers", X_uint;
				      "prog",    X_uint;
				      "vers",    X_uint;
				      "proc",    X_uint;
				      "cred",    X_type "opaque_auth";
				      "verf",    X_type "opaque_auth";
				      "param",   X_param "in" ];

    "call_body_frame",     X_struct [ "rpcvers", X_uint;
				      "prog",    X_uint;
				      "vers",    X_uint;
				      "proc",    X_uint;
				      "cred",    X_type "opaque_auth";
				      "verf",    X_type "opaque_auth";
				      	(* "param",   X_param "in" *) ];

    "call_body_frame_up_to_cred", 
                           X_struct [ "rpcvers", X_uint;
				      "prog",    X_uint;
				      "vers",    X_uint;
				      "proc",    X_uint;
				      "cred",    X_type "opaque_auth" ];

    "accepted_reply",      X_struct [ "verf",    X_type "opaque_auth";
				      "reply_data",
				      X_union_over_enum
					((X_type "accept_stat"),
					 [ "SUCCESS",      X_param "out";
					   "PROG_MISMATCH",
					   X_struct [ "low",  X_uint;
						      "high", X_uint ] ],
					 Some X_void)
				    ];

    "rejected_reply",      X_union_over_enum
                             ((X_type "reject_stat"),
			      [ "RPC_MISMATCH",  X_struct [ "low",  X_uint;
							    "high", X_uint ];
				"AUTH_ERROR",    X_type "auth_stat"
			      ],
			      None);

    "reply_body",          X_union_over_enum
                             ((X_type "reply_stat"),
			      [ "MSG_ACCEPTED", X_type "accepted_reply";
				"MSG_DENIED",   X_type "rejected_reply" ],
			      None);

    "rpc_msg",             X_struct [ "xid",  X_uint;
				      "body",
				      X_union_over_enum
					((X_type "msg_type"),
					 [ "CALL",  X_type "call_body";
					   "REPLY", X_type "reply_body" ],
					 None) ];

    "rpc_msg_call_frame",  X_struct [ "xid",  X_uint;
				      "body",
				      X_union_over_enum
					((X_type "msg_type"),
					 [ "CALL",  X_type "call_body_frame"
					    (* "REPLY", X_type "reply_body"*) ],
					 None) ];

    "rpc_msg_call_frame_up_to_cred",
                           X_struct [ "xid",  X_uint;
				      "body",
				      X_union_over_enum
					((X_type "msg_type"),
					 [ "CALL",  
					     X_type "call_body_frame_up_to_cred"
					 ],
					 None) ];

    "rpc_msg_call_body",   X_param "in";

  ]

(****)

(* validate message type system on demand and expand the "rpc_msg" symbol,
 * i.e. get the generic type of messages. This type has parameters "in"
 * and "out" for the input and output types of concrete messages, resp.
 *)

let rpc_msg =
    ( let rpc_ts = validate_xdr_type_system rpc_ts_unvalidated in
        expanded_xdr_type rpc_ts (X_type "rpc_msg")
    )

let rpc_msg_call_frame =
    ( let rpc_ts = validate_xdr_type_system rpc_ts_unvalidated in
        expanded_xdr_type rpc_ts (X_type "rpc_msg_call_frame")
    )

let rpc_msg_call_frame_up_to_cred =
    ( let rpc_ts = validate_xdr_type_system rpc_ts_unvalidated in
        expanded_xdr_type rpc_ts (X_type "rpc_msg_call_frame_up_to_cred")
    )

let rpc_msg_call_body =
    ( let rpc_ts = validate_xdr_type_system rpc_ts_unvalidated in
        expanded_xdr_type rpc_ts (X_type "rpc_msg_call_body")
    )

let valid_void = validate_xdr_type X_void

(****)

type packed_value =
  | PV of string                       (* as simple string *)
  | PV_ms of Xdr_mstring.mstring list  (* as concatenation of mstrings *)

(****)

let pack_call ?encoder prog xid proc flav_cred data_cred flav_verf data_verf
              proc_parm =

  let prog_nr = Rpc_program.program_number prog in
  let vers_nr = Rpc_program.version_number prog in
  let proc_nr, in_t, out_t = Rpc_program.signature prog proc in

  let message_t = rpc_msg in      (* type of generic message *)

  let message_v =                            (* value of the message *)
    (XV_struct_fast
       [| (* xid *)  XV_uint xid;
	  (* body *) XV_union_over_enum_fast
	  ( (* CALL *)
	    0,
	    XV_struct_fast
	      [| (* rpcvers *) XV_uint (uint4_of_int 2);
		 (* prog *)    XV_uint prog_nr;
		 (* vers *)    XV_uint vers_nr;
		 (* proc *)    XV_uint proc_nr;
		 (* cred *)    XV_struct_fast
			         [| (* flavor *) XV_enum flav_cred;
				    (* Body *)   XV_opaque data_cred
				 |];
                 (* verf *)    XV_struct_fast
			         [| (* flavor *) XV_enum flav_verf;
				    (* body *)   XV_opaque data_verf
				 |];
		 (* param *)   proc_parm
	      |]
            )
       |]) in

  let encode =
    match encoder with
      | None -> []
      | Some e -> [ "in", e ] in

  PV_ms
    (pack_xdr_value_as_mstrings
       ~encode
       message_v            (* the value to pack *)
       message_t            (* the message type... *)
       [ "in", in_t;        (* ...instantiated with input type...*)
	 "out", out_t ]     (* ...and output type *)
    )


let pack_call_gssapi_header prog xid proc flav_cred data_cred =

  let prog_nr = Rpc_program.program_number prog in
  let vers_nr = Rpc_program.version_number prog in
  let proc_nr, in_t, out_t = Rpc_program.signature prog proc in

  let message_t = rpc_msg_call_frame_up_to_cred in

  let message_v =                            (* value of the message *)
    (XV_struct_fast
       [| (* xid *)  XV_uint xid;
	  (* body *) XV_union_over_enum_fast
	  ( (* CALL *)
	    0,
	    XV_struct_fast
	      [| (* rpcvers *) XV_uint (uint4_of_int 2);
		 (* prog *)    XV_uint prog_nr;
		 (* vers *)    XV_uint vers_nr;
		 (* proc *)    XV_uint proc_nr;
		 (* cred *)    XV_struct_fast
			         [| (* flavor *) XV_enum flav_cred;
				    (* Body *)   XV_opaque data_cred
				 |];
	      |]
            )
       |]) in

  PV_ms
    (pack_xdr_value_as_mstrings
       message_v            (* the value to pack *)
       message_t            (* the message type... *)
       [ "in", in_t;        (* ...instantiated with input type...*)
	 "out", out_t ]     (* ...and output type *)
    )


(****)

let unpack_call_frame_l  pv =
  let message_t = rpc_msg_call_frame in

  let message_v, len =
    match pv with
	PV octets ->
	  unpack_xdr_value_l
	    ~fast:true ~prefix:true octets message_t []
      | PV_ms mstrings ->
	  (* FIXME: There is no faster method than this right now: *)
	  let octets = Xdr_mstring.concat_mstrings mstrings in
	  unpack_xdr_value_l
	    ~fast:true ~prefix:true octets message_t []
  in

  match message_v with
    XV_struct_fast
      [| (* xid *)  XV_uint xid;
	 (* body *) XV_union_over_enum_fast
	  ( (* CALL *)
	    0,
	    XV_struct_fast
	      [| (* rpcvers *) XV_uint rpc_version;
		 (* prog *)    XV_uint prog_nr;
	         (* vers *)    XV_uint vers_nr;
	         (* proc *)    XV_uint proc_nr;
	         (* cred *)    XV_struct_fast
		                 [| (* flavor *) XV_enum_fast flav_cred_pos;
				    (* body *)   XV_opaque data_cred
				 |];
	         (* verf *)    XV_struct_fast
				 [| (* flavor *) XV_enum_fast flav_verf_pos;
			            (* body *)   XV_opaque data_verf
				 |]
	     |])
      |] ->
	if rpc_version = uint4_of_int 2 then
	  xid, prog_nr, vers_nr, proc_nr,
	  auth_flavor_of_pos flav_cred_pos, data_cred,
	  auth_flavor_of_pos flav_verf_pos, data_verf,
	  len
        else
	    raise (Rpc_cannot_unpack "RPC version not supported")
  | _ ->
      raise (Rpc_cannot_unpack "strange message")


let unpack_call_frame octets =
  (* compatibility *)
  let (xid, prog_nr, vers_nr, proc_nr,
       flav_cred, data_cred,
       flav_verf, data_verf,
       len) = unpack_call_frame_l octets in
  (xid, prog_nr, vers_nr, proc_nr,
   flav_cred, data_cred,
   flav_verf, data_verf)

(****)

let unpack_call_body ?mstring_factories ?decoder prog proc pv pos =
  let proc_nr, in_t, out_t = Rpc_program.signature prog proc in

  let message_t = rpc_msg_call_body in

  let octets =
    match pv with
	PV octets -> octets
      | PV_ms mstrings -> Xdr_mstring.concat_mstrings mstrings
  in

  let decode =
    match decoder with
      | None -> []
      | Some d -> [ "in", d ] in

  let message_v =                            (* unpack the value *)
    unpack_xdr_value
      ~pos
      ~fast:true
      ?mstring_factories
      ~decode
      octets                                 (* XDR encoded value *)
      message_t                              (* generic type *)
      [ "in", in_t ]                         (* instance for "in" *)
  in

  message_v
;;


let unpack_call_body_raw pv pos =
  let octets =
    match pv with
	PV octets -> octets
      | PV_ms mstrings -> Xdr_mstring.concat_mstrings mstrings
  in

  String.sub octets pos (String.length octets - pos)


(****)

let extract_call_gssapi_header pv =
  let octets = 
    match pv with
      | PV s -> s
      | PV_ms mstrings -> (* FIXME *)
	  Xdr_mstring.concat_mstrings mstrings in
  (* The first 7 words have constant length. The 8th word contains the
     length of the rest (the data part of cred)
   *)
  if String.length octets < 32 then
    failwith "Rpc_packer.extract_call_gssapi_header: too short";
  let n =
    Rtypes.int_of_uint4 (Rtypes.read_uint4 octets 28) in
  let m = if n land 3 = 0 then n else n+4-(n land 3) in
  32 + m


(****)

let unpack_call ?mstring_factories ?decoder prog proc pv =
  (* compatibility *)
  let (xid, prog_nr, vers_nr, proc_nr,
       flav_cred, data_cred,
       flav_verf, data_verf,
       len) = unpack_call_frame_l pv in
  let proc_parm = 
    unpack_call_body ?mstring_factories ?decoder prog proc pv len in
  (xid, prog_nr, vers_nr, proc_nr,
   flav_cred, data_cred,
   flav_verf, data_verf,
   proc_parm)

(****)

let pack_successful_reply ?encoder
       prog proc xid flav_verf data_verf return_value =

  let proc_nr, in_t, out_t = Rpc_program.signature prog proc in

  let message_t = rpc_msg in      (* type of generic message *)

  let message_v =                            (* value of the message *)
    (XV_struct_fast
       [| (* xid *)  XV_uint xid;
	  (* body *) XV_union_over_enum_fast
	  ( (* REPLY *)
	    1,
	    XV_union_over_enum_fast
	      ( (* MSG_ACCEPTED *)
		0,
		XV_struct_fast
		  [| (* verf *)
		       XV_struct_fast [| (* flavor *) XV_enum flav_verf;
	 		                 (* body *)   XV_opaque data_verf |];
		     (* reply_data *)
		       XV_union_over_enum_fast
			 ( (* SUCCESS *) 0, return_value)
		  |] ))
       |] ) in

  let encode =
    match encoder with
      | None -> []
      | Some e -> [ "out", e ] in

  PV_ms
    (pack_xdr_value_as_mstrings
       ~encode
       message_v            (* the value to pack *)
       message_t            (* the message type... *)
       [ "in", in_t;        (* ...instantiated with input type...*)
	 "out", out_t ]     (* ...and output type *)
    )

(****)

let t_void = validate_xdr_type X_void

let pack_successful_reply_raw
       xid flav_verf data_verf return_data =

  let message_t = rpc_msg in      (* type of generic message *)

  let message_v =                            (* value of the message *)
    (XV_struct_fast
       [| (* xid *)  XV_uint xid;
	  (* body *) XV_union_over_enum_fast
	  ( (* REPLY *)
	    1,
	    XV_union_over_enum_fast
	      ( (* MSG_ACCEPTED *)
		0,
		XV_struct_fast
		  [| (* verf *)
		       XV_struct_fast [| (* flavor *) XV_enum flav_verf;
	 		                 (* body *)   XV_opaque data_verf |];
		     (* reply_data *)
		       XV_union_over_enum_fast
			 ( (* SUCCESS *) 0, XV_void)
		  |] ))
       |] ) in

  PV_ms
    (pack_xdr_value_as_mstrings
       message_v            (* the value to pack *)
       message_t            (* the message type... *)
       [ "in", t_void;      (* dummy *)
	 "out", t_void ]    (* ...and output type *)
     @ return_data
    )

(****)

let pack_accepting_reply xid flav_verf data_verf condition =

  let case, explanation =
    match condition with
      Unavailable_program       -> (* PROG_UNAVAIL *) 1, XV_void
    | Unavailable_version (l,h) -> (* PROG_MISMATCH *) 2,
                                    XV_struct_fast [| (* low *)  XV_uint l;
				                      (* high *) XV_uint h |]
    | Unavailable_procedure     -> (* PROC_UNAVAIL *) 3, XV_void
    | Garbage                   -> (* GARBAGE_ARGS *) 4, XV_void
    | System_err                -> (* SYSTEM_ERR *) 5, XV_void
    | _                         -> failwith "pack_accepting_reply"
  in

  let message_t = rpc_msg in      (* type of generic message *)

  let message_v =                            (* value of the message *)
    (XV_struct_fast
       [| (* xid *)  XV_uint xid;
	  (* body *) XV_union_over_enum_fast
	   ( (* REPLY *)
	     1,
	     XV_union_over_enum_fast
	       ( (* MSG_ACCEPTED *)
		 0,
		 XV_struct_fast
		   [| (* verf *) XV_struct_fast
			           [| (* flavor *) XV_enum flav_verf;
				      (* body *)   XV_opaque data_verf |];
		      (* reply_data *) XV_union_over_enum_fast
		                         (case, explanation)
		   |] ))
       |] ) in

  PV_ms
    (pack_xdr_value_as_mstrings
       message_v            (* the value to pack *)
       message_t            (* the message type... *)
       [ "in", valid_void;      (* ...instantiated with input type...*)
	 "out", valid_void ]    (* ...and output type *)
    )

(****)

let pack_rejecting_reply xid condition =

  let case, explanation =
    match condition with
      Rpc_mismatch (l,h) -> (* RPC_MISMATCH *) 0,
                            XV_struct_fast [| (* low *) XV_uint l;
					      (* high *) XV_uint h |]
    | Auth_bad_cred      -> (* AUTH_ERROR *) 1,
                            XV_enum_fast 0 (* AUTH_BADCRED *)
    | Auth_rejected_cred -> (* AUTH_ERROR *) 1,
                            XV_enum_fast 1 (* AUTH_REJECTEDCRED *)
    | Auth_bad_verf      -> (* AUTH_ERROR *) 1,
                            XV_enum_fast 2 (* AUTH_BADVERF *)
    | Auth_rejected_verf -> (* AUTH_ERROR *) 1,
                            XV_enum_fast 3 (* AUTH_REJECTEDVERF *)
    | Auth_too_weak      -> (* AUTH_ERROR *) 1,
                            XV_enum_fast 4 (* AUTH_TOOWEAK *)
    | Auth_invalid_resp  -> (* AUTH_ERROR *) 1,
                            XV_enum_fast 5 (* AUTH_INVALIDRESP *)
    | Auth_failed        -> (* AUTH_ERROR *) 1,
                            XV_enum_fast 6 (* AUTH_FAILED *)
    | RPCSEC_GSS_credproblem ->  1, XV_enum_fast 13
    | RPCSEC_GSS_ctxproblem ->   1, XV_enum_fast 14
  in

(*
  let proc_nr, in_t, out_t = Rpc_program.signature prog proc in
*)

  let message_t = rpc_msg in      (* type of generic message *)

  let message_v =                            (* value of the message *)
    (XV_struct_fast
       [| (* xid *)  XV_uint xid;
	  (* body *) XV_union_over_enum_fast
	  ( (* REPLY *)
	    1,
	    XV_union_over_enum_fast
	      ( (* MSG_DENIED *)
		1,
		XV_union_over_enum_fast (case, explanation)))|]) in

  PV_ms
    (pack_xdr_value_as_mstrings
       message_v            (* the value to pack *)
       message_t            (* the message type... *)
       [ "in", valid_void;        (* ...instantiated with input type...*)
	 "out", valid_void ]      (* ...and output type *)
    )

(****)

let unpack_reply ?mstring_factories ?decoder prog proc pv =

  let proc_nr, in_t, out_t = Rpc_program.signature prog proc in

  let message_t = rpc_msg in      (* type of generic message *)

  let octets =
    match pv with
	PV octets -> octets
      | PV_ms mstrings -> Xdr_mstring.concat_mstrings mstrings
  in

  let decode =
    match decoder with
      | None -> []
      | Some d -> [ "out", d ] in

  let message_v =                            (* unpack the value *)
    unpack_xdr_value
      ~fast:true
      ?mstring_factories
      ~decode
      octets                                 (* XDR encoded value *)
      message_t                              (* generic type *)
      [ "in", in_t;                          (* instance for "in" *)
	"out", out_t ]                       (* instance for "out" type *)
  in

  try
    let
      	XV_struct_fast
        [| (* xid *)  XV_uint xid;
	   (* body *) XV_union_over_enum_fast
	                ( (* REPLY *) 1, XV_union_over_enum_fast
		                           ( reply_stat_pos, reply_part1_v )
                        )
        |]
	= message_v
    in
    match reply_stat_pos with
	0 (* MSG_ACCEPTED *) ->
	  let XV_struct_fast
	    [| (* verf *)       XV_struct_fast
	                          [| (*flavor*) XV_enum_fast verf_flavor_pos;
				     (*body*)   XV_opaque verf_data
				  |];
	       (* reply_data *) XV_union_over_enum_fast
	                          ( accept_stat_pos, reply_part2_v )
            |]
	    = reply_part1_v in
	  begin
	    match accept_stat_pos with
		0 (* SUCCESS *) ->
	          (* return all what we found: *)
		  let verf_flavor = auth_flavor_of_pos verf_flavor_pos in
		  (xid, verf_flavor, verf_data, reply_part2_v)
	      | 1 (* PROG_UNAVAIL *) ->
		  raise (Rpc_server Unavailable_program)
	      | 2 (* PROG_MISMATCH *) ->
		  let XV_struct_fast
		    [| (* low *)  XV_uint l;
		       (* high *) XV_uint h |] = reply_part2_v in
	          raise (Rpc_server (Unavailable_version (l,h)))
	      | 3 (* PROC_UNAVAIL *) ->
		  raise (Rpc_server Unavailable_procedure)
	      | 4 (* GARBAGE_ARGS *) ->
		  raise (Rpc_server Garbage)
	      | 5 (* SYSTEM_ERR *) ->
		  raise (Rpc_server System_err)
	  end
      | 1 (* MSG_DENIED *) ->
	  let XV_union_over_enum_fast
	        ( reject_stat_pos, reply_part2_v ) = reply_part1_v in
	  begin
	    match reject_stat_pos with
		0 (* RPC_MISMATCH *) ->
		  let XV_struct_fast
		    [| (*low*) XV_uint l; (*high*) XV_uint h |] =
		    reply_part2_v in
		  raise (Rpc_server (Rpc_mismatch (l,h)))
	      | 1 (* AUTH_ERROR *) ->
		  begin
		    match reply_part2_v with
			XV_enum_fast 0 ->
			  raise (Rpc_server Auth_bad_cred)
		      | XV_enum_fast 1 ->
			  raise (Rpc_server Auth_rejected_cred)
		      | XV_enum_fast 2 ->
			  raise (Rpc_server Auth_bad_verf)
		      | XV_enum_fast 3 ->
			  raise (Rpc_server Auth_rejected_verf)
		      | XV_enum_fast 4 ->
			  raise (Rpc_server Auth_too_weak)
		      | XV_enum_fast 5 ->
			  raise (Rpc_server Auth_invalid_resp)
		      | XV_enum_fast 6 ->
			  raise (Rpc_server Auth_failed)
		      | XV_enum_fast 13 ->
			  raise (Rpc_server RPCSEC_GSS_credproblem)
		      | XV_enum_fast 14 ->
			  raise (Rpc_server RPCSEC_GSS_ctxproblem)
		  end
	  end
    with
	Match_failure _ ->
	  (* raise x; *) (* DEBUG *)
	  raise (Rpc_cannot_unpack "Unsupported RPC variant")

(****)

let unpack_reply_verifier prog proc pv =

  (* let proc_nr, in_t, out_t = Rpc_program.signature prog proc in *)

  let message_t = rpc_msg in      (* type of generic message *)

  let octets =
    match pv with
	PV octets -> octets
      | PV_ms mstrings -> Xdr_mstring.concat_mstrings mstrings
  in

  let message_v =                            (* unpack the value *)
    unpack_xdr_value
      ~fast:true
      ~prefix:true
      octets                                 (* XDR encoded value *)
      message_t                              (* generic type *)
      [ "in", valid_void;                    (* instance for "in" *)
	"out", valid_void ]                  (* instance for "out" type *)
  in

  try
    let
      	XV_struct_fast
          [| (* xid *)  xid;
	     (* body *) XV_union_over_enum_fast
	     ( (* REPLY *)
	       1,
	       XV_union_over_enum_fast ( reply_stat_pos, reply_part1_v )
                   )
          |]
      = message_v
    in
    match reply_stat_pos with
	0 (* MSG_ACCEPTED *) ->
	  let XV_struct_fast
	    [| (* verf *) XV_struct_fast
		            [| (* flavor *) XV_enum_fast verf_flavor_pos;
			       (* body *)   XV_opaque verf_data
			    |];
	       (* reply_data *)
	       XV_union_over_enum_fast ( accept_stat_pos, reply_part2_v )
            |]
	    = reply_part1_v in
	  auth_flavor_of_pos verf_flavor_pos, verf_data
    with
	Match_failure _ ->
	  (* raise x; *) (* DEBUG *)
	  raise (Rpc_cannot_unpack "Unsupported RPC variant")

(*****)

let peek_xid pv =

  match pv with
      PV octets ->
	if String.length octets < 4 then
	  failwith "peek_xid: message too short [1]";

	Rtypes.mk_uint4 (octets.[0], octets.[1], octets.[2], octets.[3])

    | PV_ms mstrings ->
	if Xdr_mstring.length_mstrings mstrings < 4 then
	  failwith "peek_xid: message too short [2]";

	let s = 
	  Xdr_mstring.prefix_mstrings mstrings 4 in

	Rtypes.mk_uint4 (s.[0], s.[1], s.[2], s.[3])

(*****)

let peek_auth_error pv =
  let len =
    match pv with
	PV octets -> String.length octets
      | PV_ms mstrings -> Xdr_mstring.length_mstrings mstrings in

  if len <> 20 then
    None
  else (
    let octets =
      match pv with
	  PV octets -> octets
	| PV_ms mstrings -> Xdr_mstring.concat_mstrings mstrings in  

    if String.sub octets 4 12 <> 
          "\000\000\000\001\000\000\000\001\000\000\000\001"
    then
      None
    else
      match String.sub octets 16 4 with
	  "\000\000\000\001" -> Some Auth_bad_cred
	| "\000\000\000\002" -> Some Auth_rejected_cred
	| "\000\000\000\003" -> Some Auth_bad_verf
	| "\000\000\000\004" -> Some Auth_rejected_verf
	| "\000\000\000\005" -> Some Auth_too_weak
	| "\000\000\000\006" -> Some Auth_invalid_resp
	| "\000\000\000\007" -> Some Auth_failed
	| "\000\000\000\013" -> Some RPCSEC_GSS_credproblem
	| "\000\000\000\014" -> Some RPCSEC_GSS_ctxproblem
	| _                  -> None
  )
;;

(*****)

let length_of_packed_value pv =
  match pv with
      PV octets -> String.length octets
    | PV_ms mstrings -> Xdr_mstring.length_mstrings mstrings
;;

let string_of_packed_value pv =
  match pv with
      PV octets -> octets
    | PV_ms mstrings -> Xdr_mstring.concat_mstrings mstrings
;;

let packed_value_of_string s = PV s;;

let packed_value_of_mstrings mstrings = PV_ms mstrings

let mstrings_of_packed_value pv =
  match pv with
    | PV octets -> 
	[ Xdr_mstring.string_based_mstrings # create_from_string
	    octets 0 (String.length octets) false ]
    | PV_ms mstrings -> 
	mstrings
;;

let prefix_of_packed_value pv n =
  match pv with
    | PV octets ->
	String.sub octets 0 n
    | PV_ms ms ->
	Xdr_mstring.prefix_mstrings ms n
