(* $Id: rpc_packer.mli 1553 2011-02-28 00:12:27Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* Packing and Unpacking of messages; can be used by both client and
 * server programs.
 * RPC messages consists of a frame and of application data. The packing
 * (and unpacking) of the latter into the frame is done by this module.
 * The frame is an XDR value with some slots that can be filled with
 * application data, normally again an XDR value. The slots are represented
 * using the X_param mechanism provided by the XDR module. As nested X_param
 * usage is not permitted, this feature is not available for application
 * data.
 *)

(* CHANGE:
 * - rpc-0.4: unpacking returns XV_enum_fast, XV_struct_fast, and
 *   XV_union_over_enum_fast values
 *)

open Rtypes
open Xdr
open Rpc

type packed_value

(* General packing and unpacking of messages: *)

val pack_call :
      ?encoder:encoder ->
      Rpc_program.t ->    (* which program *)
      uint4 ->            (* session number "xid" *)
      string ->           (* procedure name *)
      string ->           (* flavour of credentials (authentication method) *)
      string ->           (* data of credentials *)
      string ->           (* flavour of verifier *)
      string ->           (* data of verifier *)
      xdr_value ->        (* the parameter of the procedure call *)
      packed_value        (* the call in XDR representation *)

val pack_call_gssapi_header :
      Rpc_program.t ->    (* which program *)
      uint4 ->            (* session number "xid" *)
      string ->           (* procedure name *)
      string ->           (* flavour of credentials (authentication method) *)
      string ->           (* data of credentials *)
        packed_value
  (* for GSS-API - the call header up to the credentials *)

val unpack_call :
      ?mstring_factories:Xdr_mstring.named_mstring_factories ->
      ?decoder:decoder ->
      Rpc_program.t ->    (* which program to match *)
      string ->           (* which procedure *)
      packed_value ->     (* the call in XDR representation *)
      ( uint4 *           (* session number "xid" *)
	uint4 *           (* program number *)
	uint4 *           (* version number *)
	uint4 *           (* procedure number *)
	string *          (* flavour of credentials *)
	string *          (* data of credentials *)
	string *          (* flavour of verifier *)
	string *          (* data of verifier *)
	xdr_value )       (* the parameter of the procdedure call *)

  (* unpack_call may raise exception Rpc_cannot_unpack. *)
  (* Note: the program and version numbers given with the Rpc_program.t
   * parameter are ignored; the values found in the message are returned
   *)

val unpack_call_frame :
      packed_value ->
      ( uint4 *           (* session number "xid" *)
	uint4 *           (* program number *)
	uint4 *           (* version number *)
	uint4 *           (* procedure number *)
	string *          (* flavour of credentials *)
	string *          (* data of credentials *)
	string *          (* flavour of verifier *)
	string)           (* data of verifier *)
  (* Unpacks only the frame of the call. *)

val unpack_call_frame_l :
      packed_value ->
      ( uint4 *           (* session number "xid" *)
	uint4 *           (* program number *)
	uint4 *           (* version number *)
	uint4 *           (* procedure number *)
	string *          (* flavour of credentials *)
	string *          (* data of credentials *)
	string *          (* flavour of verifier *)
	string *          (* data of verifier *)
        int)              (* length of the frame in bytes *)
  (* Unpacks only the frame of the call, returns also the length of the
   * frame
   *)

val extract_call_gssapi_header :
      packed_value -> int
  (* returns the length of the prefix of the message so that the prefix
     includes the header until (and including) the credentials
   *)

val unpack_call_body :
      ?mstring_factories:Xdr_mstring.named_mstring_factories ->
      ?decoder:decoder ->
      Rpc_program.t ->    (* which program to match *)
      string ->           (* which procedure *)
      packed_value ->     (* the (complete) call in XDR representation *)
      int ->              (* the position in the packed_value where the
			     body begins *)
	xdr_value         (* the parameter of the procdedure call *)
  (* Unpacks only the body (the parameters of the procedure call) of
   * the packed_value. You can pass the length of the call frame as the
   * position of the body.
   *)

val unpack_call_body_raw :
      packed_value -> int -> string

(* More specific messages: *)

(* A successful reply means that the procedure call returned a value. An
 * accepting reply means that authentication was successful but
 * the procedure could not be evaluated. The third catagory of replies
 * are rejecting replies.
 *)


val pack_successful_reply :
      ?encoder:encoder ->
      Rpc_program.t ->    (* which program *)
      string ->           (* which procedure *)
      uint4 ->            (* xid *)
      string ->           (* flavour of verifier *)
      string ->           (* data of verifier *)
      xdr_value ->        (* the return value *)
      packed_value        (* the reply in XDR representation *)

val pack_successful_reply_raw :
      uint4 ->            (* xid *)
      string ->           (* flavour of verifier *)
      string ->           (* data of verifier *)
      Xdr_mstring.mstring list -> (* raw return data *)
      packed_value

val pack_accepting_reply :
      uint4 ->            (* xid *)
      string ->           (* flavour of the verifier *)
      string ->           (* data of verifier *)
      server_error ->     (* one of the possible error conditions *)
      packed_value        (* the reply in XDR representation *)

val pack_rejecting_reply :
(*      Rpc_program.t ->    (* which program *)
      string ->           (* which procedure *)  *)
      uint4 ->            (* xid *)
      server_error ->     (* one of the possible error conditions *)
      packed_value        (* the reply in XDR representation *)

val unpack_reply :
      ?mstring_factories:Xdr_mstring.named_mstring_factories ->
      ?decoder:decoder ->
      Rpc_program.t ->    (* which program *)
      string ->           (* which procedure *)
      packed_value ->     (* the reply in XDR representation *)
      ( uint4 *           (* the xid *)
	string *          (* flavour of the verifier *)
	string *          (* data of the verifier *)
        xdr_value )       (* the return value *)

  (* unpack_reply may raise:
   * Rpc_server: If one of the server_error conditions is true. Note that the
   *   call may be "accepted" even if this exception occurs. In this case
   *   use unpack_reply_verfifier to get the verifier.
   *)


val unpack_reply_verifier :
      Rpc_program.t ->    (* which program *)
      string ->           (* which procedure *)
      packed_value ->     (* the reply in XDR representation *)
      (string * string)   (* the verifier (flavour, data) *)


val peek_xid :
      packed_value ->
      uint4

  (* Extracts the session ID without analyzing the rest of the message. *)


val peek_auth_error :
      packed_value ->
      server_error option

  (* If an authentication error occurs, this function extracts it from the
   * packed message.
   *)


val length_of_packed_value : packed_value -> int
val string_of_packed_value : packed_value -> string
val packed_value_of_string : string -> packed_value

val mstrings_of_packed_value : packed_value -> Xdr_mstring.mstring list
val packed_value_of_mstrings : Xdr_mstring.mstring list -> packed_value 

val prefix_of_packed_value : packed_value -> int -> string
  (** The first n bytes of the packed value *)

(* Currently, the packer works internally with the "mstring list"
   representation, and hence mstrings_of_packed_value is fast.

   The unpacker still works on strings, so one should use packed_value_of_string
   to create the packed_value passed to it.
 *)
