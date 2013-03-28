(* $Id: netmech_scram.ml 1588 2011-04-28 13:59:54Z gerd $ *)

(* Steps:

   client               <->               server
   ----------------------------------------------------------------------
   username, nonce       ->
                         <-               salt, i, nonce'
   clientproof, nonce'   ->
     (=algo(password, salt, i))
                         <-               serversignature
 *)

open Printf

type ptype = [ `GSSAPI ]

type mechanism = [ `SHA_1 ]

type profile =
    { ptype : ptype;
      mechanism : mechanism;
      return_unknown_user : bool;
      iteration_count_limit : int;
    }


type client_first =  (* actually client_first_bare *)
    { c1_username : string;  (* "=xx" encoding not yet applied *)
      c1_nonce : string;     (* anything but comma *)
      c1_extensions : (string * string) list
    }

type server_first =
    { s1_nonce : string;     (* anything but comma *)
      s1_salt : string;      (* decoded *)
      s1_iteration_count : int;
      s1_extensions : (string * string) list
    }

type client_final =
    { cf_chanbind : string;
      cf_nonce : string;     (* anything but comma *)
      cf_extensions : (string * string) list;
      cf_proof : string option;   (* decoded *)
    }

type server_error =
    [ `Invalid_encoding
    | `Extensions_not_supported
    | `Invalid_proof
    | `Channel_bindings_dont_match
    | `Server_does_support_channel_binding
    | `Channel_binding_not_supported
    | `Unsupported_channel_binding_type
    | `Unknown_user
    | `Invalid_username_encoding
    | `No_resources
    | `Other_error
    | `Extension of string
    ]

type server_error_or_verifier =
    [ `Error of server_error
    | `Verifier of string
    ]

type server_final =
    { sf_error_or_verifier : server_error_or_verifier;
      sf_extensions : (string * string) list;
    }

type specific_keys =
    { kc : string;
      ke : string;
      ki : string
    }

type client_session =
    { cs_profile : profile;
      mutable cs_state :
                 [ `Start | `C1 | `S1 | `CF | `SF | `Connected | `Error ];
      mutable cs_c1 : client_first option;
      mutable cs_s1 : server_first option;
      mutable cs_s1_raw : string;
      mutable cs_cf : client_final option;
      mutable cs_sf : server_final option;
      mutable cs_salted_pw : string;
      mutable cs_auth_message : string;
      mutable cs_proto_key : string option;
      cs_username : string;
      cs_password : string;
      mutable cs_chanbind : string;
    }


type server_session =
    { ss_profile : profile;
      mutable ss_state :
                 [ `Start | `C1 | `S1 | `CF | `SF | `Connected | `Error ];
      mutable ss_c1 : client_first option;
      mutable ss_c1_raw : string;
      mutable ss_s1 : server_first option;
      mutable ss_s1_raw : string;
      mutable ss_cf : client_final option;
      mutable ss_cf_raw : string;
      mutable ss_sf : server_final option;
      mutable ss_spw: string option;
      mutable ss_err : server_error option;
      mutable ss_proto_key : string option;
      ss_authenticate_opt : (string -> (string * string * int)) option;
    }

(* Exported: *)
exception Invalid_encoding of string * string
exception Invalid_username_encoding of string * string
exception Extensions_not_supported of string * string
exception Protocol_error of string
exception Invalid_server_signature
exception Server_error of server_error

(* Not exported: *)
exception Invalid_proof of string


module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netmech_scram" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netmech_scram" Debug.enable

let () =
  Netlog.Debug.register_module "Netmech_scram" Debug.enable



let profile ?(return_unknown_user=false) ?(iteration_count_limit=100000) 
	    pt =
  { ptype = pt;
    mechanism = `SHA_1;
    return_unknown_user = return_unknown_user;
    iteration_count_limit = iteration_count_limit;
  }


let saslprep s =
  (* We do not want to implement SASLprep here. It's brainf*ck, because the
     ambiguities it resolves do not occur in practice (people are not as
     dumb as the Unicode guys think). The RFC says we have to limit the
     strings then to US-ASCII.
   *)
  for k = 0 to String.length s - 1 do
    let c = s.[k] in
    if c < '\x20' || c >= '\x7f' then
      raise(Invalid_encoding("Netmech_scram.saslprep: restricted to US-ASCII",
			     s));
  done;
  s


let username_saslprep s =
  try
    saslprep s
  with
    | Invalid_encoding(s1,s2) ->
	raise(Invalid_username_encoding(s1,s2))


let comma_re = Netstring_str.regexp ","

let comma_split s =
  Netstring_str.split_delim comma_re s

let n_value_re = Netstring_str.regexp "\\([a-zA-Z]\\)=\\(.*\\)"

let n_value_split s =
  match Netstring_str.string_match n_value_re s 0 with
    | None -> raise (Invalid_encoding("n_value_split", s))
    | Some r ->
	(Netstring_str.matched_group r 1 s,
	 Netstring_str.matched_group r 2 s)

let check_value_safe_chars s =
  let enc =
    `Enc_subset(`Enc_utf8,
	    fun i -> i <> 0 && i <> 0x2c && i <> 0x3d) in
  try
    Netconversion.verify enc s
  with _ -> raise(Invalid_encoding("check_value_safe_chars",s))

let check_value_chars s =
  let enc =
    `Enc_subset(`Enc_utf8,
		fun i -> i <> 0 && i <> 0x2c) in
  try
    Netconversion.verify enc s
  with _ -> raise(Invalid_encoding("check_value_chars",s))

let check_printable s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
      | '\x21'..'\x2b' -> ()
      | '\x2d'..'\x7e' -> ()
      | _ -> raise(Invalid_encoding("check_printable",s))
  done

let pos_re = Netstring_str.regexp "[1-9][0-9]+$"

let check_positive_number s =
  match Netstring_str.string_match pos_re s 0 with
    | None -> raise(Invalid_encoding("check_positive_number",s))
    | Some _ -> ()

let comma_slash_re = Netstring_str.regexp "[,/]"

let rev_comma_slash_re = Netstring_str.regexp "\\(=2C\\|=3D\\|=\\|,\\)"

let encode_saslname s =
  ( try
      Netconversion.verify `Enc_utf8 s
    with _ -> raise(Invalid_username_encoding("encode_saslname",s))
  );
  Netstring_str.global_substitute
    comma_slash_re
    (fun r s ->
       match Netstring_str.matched_string r s with
	 | "," -> "=2C"
	 | "/" -> "=3D"
	 | _ -> assert false
    )
    s

let decode_saslname s =
  let s' =
    Netstring_str.global_substitute
      rev_comma_slash_re
      (fun r s ->
	 match Netstring_str.matched_string r s with
	   | "=2C" -> ","
	   | "=3D" -> "/"
	   | "=" | "," -> raise(Invalid_username_encoding("decode_saslname",s))
	   | _ -> assert false
      )
      s in
  ( try
      Netconversion.verify `Enc_utf8 s'
    with _ -> raise(Invalid_username_encoding("decode_saslname",s))
  );
  s'


let encode_c1_message c1 =
  (* No gs2-header in GSS-API *)
  "n=" ^ encode_saslname(username_saslprep c1.c1_username) ^ 
  ",r=" ^ c1.c1_nonce ^ 
  (if c1.c1_extensions <> [] then 
      "," ^ 
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) c1.c1_extensions)
   else ""
  )  


let decode_c1_message s =
  let l = List.map n_value_split (comma_split s) in
  match l with
    | [] ->
	raise(Invalid_encoding("decode_c1_mesage: empty", s))
    | ("m",_) :: _ ->
	raise(Extensions_not_supported("decode_c1_mesage: unsupported", s))
    | ("n", username_raw) :: ("r", nonce) :: l' ->
	let username = decode_saslname username_raw in
	let username' = username_saslprep username in
	if username <> username' then
	  raise(Invalid_username_encoding("Netmech_scram.decode_c1_message",
					  s));
	{ c1_username = username;
	  c1_nonce = nonce;
	  c1_extensions = l'
	}
    | _ ->
	raise(Invalid_encoding("decode_c1_mesage", s))


let encode_s1_message s1 =
  "r=" ^ s1.s1_nonce ^
  ",s=" ^ Netencoding.Base64.encode s1.s1_salt ^ 
  ",i=" ^ string_of_int s1.s1_iteration_count ^ 
  ( if s1.s1_extensions <> [] then
      "," ^ 
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) s1.s1_extensions)
   else ""
  )  


let decode_s1_message s =
  let l = List.map n_value_split (comma_split s) in
  match l with
    | [] ->
	raise(Invalid_encoding("decode_s1_mesage: empty", s))
    | ("m",_) :: _ ->
	raise(Extensions_not_supported("decode_s1_mesage: unsupported", s))
    | ("r",nonce) :: ("s",salt_b64) :: ("i",icount_raw) :: l' ->
	let salt =
	  try Netencoding.Base64.decode salt_b64
	  with _ ->
	    raise(Invalid_encoding("decode_s1_message: invalid s", s)) in
	check_positive_number icount_raw;
	let icount = 
	  try int_of_string icount_raw 
	  with _ -> 
	    raise(Invalid_encoding("decode_s1_message: invalid i", s)) in
	{ s1_nonce = nonce;
	  s1_salt = salt;
	  s1_iteration_count = icount;
	  s1_extensions = l'
	}
    | _ ->
	raise(Invalid_encoding("decode_s1_mesage", s))


(* About the inclusion of "c": RFC 5802 is not entirely clear about this.
   I asked the authors of the RFC what to do. The idea is that the 
   GSS-API flavor of SCRAM is obtained by removing the GS2 (RFC 5801)
   part from the description in RFC 5802 for SASL. This leads to the
   interpretation that the "c" parameter is required, and it includes the
   channel binding string as-is, without any prefixed gs2-header.
   (Remember that GS2 is a wrapper around GSS-API, and it can then
   pass the right channel binding string down, i.e. a string that includes
   the gs2-header.)
 *)
	
let encode_cf_message cf =
  "c=" ^ Netencoding.Base64.encode cf.cf_chanbind ^ 
  ",r=" ^ cf.cf_nonce ^ 
  ( if cf.cf_extensions <> [] then
      "," ^ 
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) cf.cf_extensions)
   else ""
  ) ^ 
  ( match cf.cf_proof with
      | None -> ""
      | Some p ->
	  ",p=" ^ Netencoding.Base64.encode p
  )


let decode_cf_message expect_proof s =
  let l = List.map n_value_split (comma_split s) in
  match l with
    | [] ->
	raise(Invalid_encoding("decode_cf_mesage: empty", s))
    | ("c",chanbind_b64) :: ("r",nonce) :: l' ->
	let chanbind =
	  try Netencoding.Base64.decode chanbind_b64 
	  with _ ->
	    raise(Invalid_encoding("decode_cf_mesage: invalid c",
				   s)) in
	let p, l'' =
	  if expect_proof then
	    match List.rev l' with
	      | ("p", proof_b64) :: l''_rev ->
		  let p = 
		    try Netencoding.Base64.decode proof_b64 
		    with _ ->
		      raise(Invalid_encoding("decode_cf_mesage: invalid p",
					     s)) in
		  (Some p, List.rev l''_rev)
	      | _ ->
		  raise(Invalid_encoding("decode_cf_mesage: proof not found",
					 s))
	  else
	    None, l' in
	{ cf_chanbind = chanbind;
	  cf_nonce = nonce;
	  cf_extensions = l'';
	  cf_proof = p
	}
    | _ ->
	raise(Invalid_encoding("decode_cf_mesage", s))

let strip_cf_proof s =
  let l = List.rev (List.map n_value_split (comma_split s)) in
  match l with
    | ("p",_) :: l' ->
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) (List.rev l'))
    | _ ->
	assert false


let string_of_server_error =
  function
    | `Invalid_encoding -> "invalid-encoding"
    | `Extensions_not_supported -> "extensions-not-supported"
    | `Invalid_proof -> "invalid-proof"
    | `Channel_bindings_dont_match -> "channel-bindings-dont-match"
    | `Server_does_support_channel_binding -> 
	"server-does-support-channel-binding"
    | `Channel_binding_not_supported -> "channel-binding-not-supported"
    | `Unsupported_channel_binding_type -> "unsupported-channel-binding-type"
    | `Unknown_user -> "unknown-user"
    | `Invalid_username_encoding -> "invalid-username-encoding"
    | `No_resources -> "no-resources"
    | `Other_error -> "other-error"
    | `Extension s -> s

let server_error_of_string =
  function
    | "invalid-encoding" -> `Invalid_encoding
    | "extensions-not-supported" -> `Extensions_not_supported
    | "invalid-proof" -> `Invalid_proof
    | "channel-bindings-dont-match" -> `Channel_bindings_dont_match
    | "server-does-support-channel-binding" ->
	`Server_does_support_channel_binding
    | "channel-binding-not-supported" -> `Channel_binding_not_supported
    | "unsupported-channel-binding-type" -> `Unsupported_channel_binding_type
    | "unknown-user" -> `Unknown_user
    | "invalid-username-encoding" -> `Invalid_username_encoding
    | "no-resources" -> `No_resources
    | "other-error" -> `Other_error
    | s -> `Extension s


let () =
  Netexn.register_printer
    (Server_error `Invalid_encoding)
    (fun e ->
       match e with
	 | Server_error token ->
	     sprintf "Server_error(%s)" (string_of_server_error token)
	 | _ -> assert false
    )


let encode_sf_message sf =
  ( match sf.sf_error_or_verifier with
      | `Error e ->
	  "e=" ^ string_of_server_error e
      | `Verifier v ->
	  "v=" ^ Netencoding.Base64.encode v
  ) ^
  ( if sf.sf_extensions <> [] then
      "," ^ 
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) sf.sf_extensions)
   else ""
  )


let decode_sf_message s =
  let l = List.map n_value_split (comma_split s) in
  match l with
    | [] ->
	raise(Invalid_encoding("decode_cf_mesage: empty", s))
    | ("v",verf_raw) :: l' ->
	let verf =
	  try Netencoding.Base64.decode verf_raw 
	  with _ -> 
	    raise(Invalid_encoding("decode_sf_message: invalid v", s)) in
	{ sf_error_or_verifier = `Verifier verf;
	  sf_extensions = l'
	}
    | ("e",error_s) :: l' ->
	let error = server_error_of_string error_s in
	{ sf_error_or_verifier = `Error error;
	  sf_extensions = l'
	}
    | _ ->
	raise(Invalid_encoding("decode_sf_mesage", s))


let sha1 s =
  Cryptokit.hash_string (Cryptokit.Hash.sha1()) s

let hmac key str =
  Netauth.hmac
    ~h:sha1
    ~b:64
    ~l:20
    ~k:key
    ~message:str

let int_s i =
  let s = String.make 4 '\000' in
  s.[0] <- Char.chr ((i lsr 24) land 0xff);
  s.[1] <- Char.chr ((i lsr 16) land 0xff);
  s.[2] <- Char.chr ((i lsr 8) land 0xff);
  s.[3] <- Char.chr (i land 0xff);
  s

let hi str salt i =
  let rec uk k =
    if k=1 then
      let u = hmac str (salt ^ int_s 1) in
      let h = u in
      (u,h)
    else (
      let (u_pred, h_pred) = uk (k-1) in
      let u = hmac str u_pred in
      let h = Netauth.xor_s u h_pred in
      (u,h)
    ) in
  snd (uk i)


let lsb128 s =
  (* The least-significant 128 bits *)
  let l = String.length s in
  if l < 16 then
    failwith "Netmech_scram.lsb128";
  String.sub s (l-16) 16


let create_nonce() =
  let s = String.make 16 ' ' in
  Netsys_rng.fill_random s;
  Digest.to_hex s


let create_salt = create_nonce


let create_client_session profile username password =
  ignore(saslprep username);
  ignore(saslprep password);  (* Check for errors *)
  { cs_profile = profile;
    cs_state = `Start;
    cs_c1 = None;
    cs_s1 = None;
    cs_s1_raw = "";
    cs_cf = None;
    cs_sf = None;
    cs_auth_message = "";
    cs_salted_pw = "";
    cs_username = username;
    cs_password = password;
    cs_proto_key = None;
    cs_chanbind = "";
  }
 

let client_emit_flag cs =
  match cs.cs_state with
    | `Start | `S1 -> true
    | _ -> false


let client_recv_flag cs =
  match cs.cs_state with
    | `C1 | `CF -> true
    | _ -> false


let client_finish_flag cs =
  cs.cs_state = `Connected


let client_error_flag cs =
  cs.cs_state = `Error


let catch_error cs f arg =
  try
    f arg
  with
    | error ->
	dlog (sprintf "Client caught error: %s"
		(Netexn.to_string error));
	cs.cs_state <- `Error;
	raise error


let client_protocol_key cs =
  cs.cs_proto_key

let client_user_name cs =
  cs.cs_username

let client_configure_channel_binding cs cb =
  ( match cs.cs_state with
      | `Start | `C1 | `S1 -> ()
      | _ -> failwith "Netmech_scram.client_configure_channel_binding"
  );
  cs.cs_chanbind <- cb

let client_channel_binding cs =
  cs.cs_chanbind

let client_export cs =
  if not (client_finish_flag cs) then
    failwith "Netmech_scram.client_export: context not yet established";
  Marshal.to_string cs []

let client_import s =
  ( Marshal.from_string s 0 : client_session)


let salt_password password salt iteration_count =
  let sp = hi (saslprep password) salt iteration_count in
  (* eprintf "salt_password(%S,%S,%d) = %S\n" password salt iteration_count sp; *)
  sp


let client_emit_message cs =
  catch_error cs
    (fun () ->
       match cs.cs_state with
	 | `Start ->
	     let c1 =
	       { c1_username = cs.cs_username;
		 c1_nonce = create_nonce();
		 c1_extensions = []
	       } in
	     cs.cs_c1 <- Some c1;
	     cs.cs_state <- `C1;
	     let m = encode_c1_message c1 in
	     dlog (sprintf "Client state `Start emitting message: %s" m);
	     m
	       
	 | `S1 ->
	     let c1 =
	       match cs.cs_c1 with None -> assert false | Some c1 -> c1 in
	     let s1 =
	       match cs.cs_s1 with None -> assert false | Some s1 -> s1 in
	     let salted_pw = 
	       salt_password cs.cs_password s1.s1_salt s1.s1_iteration_count in
	     let client_key = hmac salted_pw "Client Key" in
	     let stored_key = sha1 client_key in
	     let cf_no_proof =
	       encode_cf_message { cf_chanbind = cs.cs_chanbind;
				   cf_nonce = s1.s1_nonce;
				   cf_extensions = [];
				   cf_proof = None
				 } in
	     let auth_message =
	       encode_c1_message c1 ^ "," ^ 
		 cs.cs_s1_raw ^ "," ^ 
		 cf_no_proof in
	     let client_signature = hmac stored_key auth_message in
	     let p = Netauth.xor_s client_key client_signature in
	     let cf =
	       { cf_chanbind = cs.cs_chanbind;
		 cf_nonce = s1.s1_nonce;
		 cf_extensions = [];
		 cf_proof = Some p;
	       } in
	     cs.cs_cf <- Some cf;
	     cs.cs_state <- `CF;
	     cs.cs_auth_message <- auth_message;
	     cs.cs_salted_pw <- salted_pw;
	     cs.cs_proto_key <- Some ( lsb128
					 (hmac
					    stored_key
					    ("GSS-API session key" ^ 
					       client_key ^ auth_message)));
	     let m = encode_cf_message cf in
	     dlog (sprintf "Client state `S1 emitting message: %s" m);
	     m
	       
	 | _ ->
	     failwith "Netmech_scram.client_emit_message"
    )
    ()


let client_recv_message cs message =
  catch_error cs
    (fun () ->
       match cs.cs_state with
	 | `C1 ->
	     dlog (sprintf "Client state `C1 receiving message: %s" message);
	     let s1 = decode_s1_message message in
	     let c1 =
	       match cs.cs_c1 with None -> assert false | Some c1 -> c1 in
	     if String.length s1.s1_nonce < String.length c1.c1_nonce then
	       raise (Protocol_error
			"client_recv_message: Nonce from the server is too short");
	     if String.sub s1.s1_nonce 0 (String.length c1.c1_nonce) <> c1.c1_nonce
	     then
	       raise (Protocol_error
			"client_recv_message: bad nonce from the server");
	     if s1.s1_iteration_count > cs.cs_profile.iteration_count_limit then
	       raise (Protocol_error
			"client_recv_message: iteration count too high");
	     cs.cs_state <- `S1;
	     cs.cs_s1 <- Some s1;
	     cs.cs_s1_raw <- message
	       
	 | `CF ->
	     dlog (sprintf "Client state `CF receiving message: %s" message);
	     let sf = decode_sf_message message in
	     ( match sf.sf_error_or_verifier with
		 | `Verifier v ->
		     let salted_pw = cs.cs_salted_pw in
		     let server_key =
		       hmac salted_pw "Server Key" in
		     let server_signature =
		       hmac server_key cs.cs_auth_message in
		     if v <> server_signature then
		       raise Invalid_server_signature;
		     cs.cs_state <- `Connected;
		     dlog "Client is authenticated"
		 | `Error e ->
		     cs.cs_state <- `Error;
		     dlog (sprintf "Client got error token from server: %s"
			     (string_of_server_error e));
		     raise(Server_error e)
	     )
	       
	 | _ ->
	     failwith "Netmech_scram.client_recv_message"
    )
    ()


let create_server_session profile auth =
  (* auth: called as: let (salted_pw, salt, i) = auth username *)
  { ss_profile = profile;
    ss_state = `Start;
    ss_c1 = None;
    ss_c1_raw = "";
    ss_s1 = None;
    ss_s1_raw = "";
    ss_cf = None;
    ss_cf_raw = "";
    ss_sf = None;
    ss_authenticate_opt = Some auth;
    ss_spw = None;
    ss_err = None;
    ss_proto_key = None;
  }


let server_emit_flag ss =
  match ss.ss_state with
    | `C1 | `CF -> true
    | _ -> false

let server_recv_flag ss =
  match ss.ss_state with
    | `Start | `S1 -> true
    | _ -> false

let server_finish_flag ss =
  ss.ss_state = `Connected

let server_error_flag ss =
  ss.ss_state = `Error

let server_protocol_key ss =
  ss.ss_proto_key

let server_export ss =
  if not (server_finish_flag ss) then
    failwith "Netmech_scram.server_export: context not yet established";
  Marshal.to_string { ss with ss_authenticate_opt = None } []

let server_import s =
  ( Marshal.from_string s 0 : server_session)


let catch_condition ss f arg =
  let debug e =
    dlog (sprintf "Server caught error: %s"
	    (Netexn.to_string e)) in
  try
    f arg
  with
    (* After such an error the protocol will continue, but the final
       server message will return the condition
     *)
    | Invalid_encoding(_,_) as e ->
	debug e;
	if ss.ss_err = None then
	  ss.ss_err <- Some `Invalid_encoding
    | Invalid_username_encoding _ as e ->
	debug e;
	if ss.ss_err = None then
	  ss.ss_err <- Some `Invalid_username_encoding
    | Extensions_not_supported(_,_) as e ->
	debug e;
	if ss.ss_err = None then
	  ss.ss_err <- Some `Extensions_not_supported
    | Invalid_proof _ as e ->
	debug e;
	if ss.ss_err = None then
	  ss.ss_err <- Some `Invalid_proof
	

exception Skip_proto


let server_emit_message ss =
  match ss.ss_state with
    | `C1 ->
	let m =
	  try
	    let c1 = 
	      match ss.ss_c1 with
		| None -> raise Skip_proto | Some c1 -> c1 in
	    let (spw, salt, i) = 
	      match ss.ss_authenticate_opt with
		| Some auth -> auth c1.c1_username
		| None -> assert false in
	    let s1 =
	      { s1_nonce = c1.c1_nonce ^ create_nonce();
		s1_salt = salt;
		s1_iteration_count = i;
		s1_extensions = []
	      } in
	    ss.ss_state <- `S1;
	    ss.ss_s1 <- Some s1;
	    ss.ss_spw <- Some spw;
	    let s1 = encode_s1_message s1 in
	    ss.ss_s1_raw <- s1;
	    s1
	  with Not_found | Skip_proto ->
	    (* continue with a dummy auth *)
	    dlog "Server does not know this user";
	    let c1_nonce =
	      match ss.ss_c1 with
		| None -> create_nonce() | Some c1 -> c1.c1_nonce in
	    let s1 =
	      { s1_nonce = c1_nonce ^ create_nonce();
		s1_salt = create_nonce();
		s1_iteration_count = 4096;
		s1_extensions = []
	      } in
	    ss.ss_state <- `S1;
	    ss.ss_s1 <- Some s1;
	    if ss.ss_err = None then
	      ss.ss_err <- Some (if ss.ss_profile.return_unknown_user then
				   `Unknown_user
				 else
				   `Invalid_proof);
	    (* This will keep the client off being successful *)
	    let s1 = encode_s1_message s1 in
	    ss.ss_s1_raw <- s1;
	    s1
	in
	dlog (sprintf "Server state `C1 emitting message: %s" m);
	m
	  
    | `CF ->
	( match ss.ss_err with
	    | Some err ->
		let sf =
		  { sf_error_or_verifier = `Error err;
		    sf_extensions = []
		  } in
		ss.ss_sf <- Some sf;
		ss.ss_state <- `Error;
		let m = encode_sf_message sf in
		dlog (sprintf "Server state `CF[Err] emitting message: %s" m);
		m
		  
	    | None ->
		let spw =
		  match ss.ss_spw with
		    | None -> assert false | Some spw -> spw in
		let cf_no_proof = strip_cf_proof ss.ss_cf_raw in
		let auth_message =
		  ss.ss_c1_raw ^ "," ^ 
		    ss.ss_s1_raw ^ "," ^ 
		    cf_no_proof in
		let server_key =
		  hmac spw "Server Key" in
		let server_signature =
		  hmac server_key auth_message in
		let sf =
		  { sf_error_or_verifier = `Verifier server_signature;
		    sf_extensions = []
		  } in
		ss.ss_sf <- Some sf;
		ss.ss_state <- `Connected;
		let m = encode_sf_message sf in
		dlog (sprintf "Server state `CF emitting message: %s" m);
		m
	)
	  
    | _ ->
	failwith "Netmech_scram.server_emit_message"


let server_recv_message ss message =
  match ss.ss_state with
    | `Start ->
	dlog (sprintf "Server state `Start receiving message: %s" message);

	catch_condition ss
	  (fun () ->
	     let c1 = decode_c1_message message in
	     ss.ss_c1 <- Some c1;
	  ) ();
	ss.ss_c1_raw <- message;
	ss.ss_state <- `C1
	  (* Username is checked later *)
    | `S1 ->
	dlog (sprintf "Server state `S1 receiving message: %s" message);

	catch_condition ss
	  (fun () ->
	     try
	       let s1 =
		 match ss.ss_s1 with
		   | None -> raise Skip_proto | Some s1 -> s1 in
	       let salted_pw =
		 match ss.ss_spw with
		   | None -> raise Skip_proto | Some spw -> spw in
	       let cf = decode_cf_message true message in
	       if s1.s1_nonce <> cf.cf_nonce then
		 raise (Invalid_proof "nonce mismatch");
	       let client_key = hmac salted_pw "Client Key" in
	       let stored_key = sha1 client_key in
	       let cf_no_proof = strip_cf_proof message in
	       let auth_message =
		 ss.ss_c1_raw ^ "," ^ 
		   ss.ss_s1_raw ^ "," ^ 
		   cf_no_proof in
	       let client_signature = hmac stored_key auth_message in
	       let p = Netauth.xor_s client_key client_signature in
	       if Some p <> cf.cf_proof then
		 raise (Invalid_proof "bad client signature");
	       ss.ss_cf <- Some cf;
	       ss.ss_proto_key <- Some ( lsb128
					   (hmac
					      stored_key
					      ("GSS-API session key" ^ 
						 client_key ^ auth_message)));
	     with
	       | Skip_proto -> ()
	  ) ();
	ss.ss_cf_raw <- message;
	ss.ss_state <- `CF
    | _ ->
	failwith "Netmech_scram.server_recv_message"


let server_channel_binding ss =
  match ss.ss_cf with
    | None -> None
    | Some cf -> Some(cf.cf_chanbind)


let server_user_name ss =
  match ss.ss_c1 with
    | None -> None
    | Some c1 -> Some c1.c1_username


let transform_mstrings (trafo:Cryptokit.transform) ms_list =
  (* Like Cryptokit's transform_string, but for "mstring list" *)
  let blen = 256 in
  let s = String.create blen in

  let rec loop in_list out_list =
    match in_list with
      | ms :: in_list' ->
	  let ms_len = ms#length in
	  ( match ms#preferred with
	      | `String ->
		  let (s,start) = ms#as_string in
		  trafo#put_substring s start ms_len;
		  if trafo#available_output > 0 then
		    let o = trafo#get_string in
		    let ms' = Xdr_mstring.string_to_mstring o in
		    loop in_list' (ms' :: out_list)
		  else
		    loop in_list' out_list
	      | `Memory ->
		  let (m,start) = ms#as_memory in
		  let k = ref 0 in
		  let ol = ref out_list in
		  while !k < ms_len do
		    let n = min blen (ms_len - !k) in
		    Netsys_mem.blit_memory_to_string
		      m (start + !k) s 0 n;
		    trafo#put_substring s 0 n;
		    k := !k + n;
		    if trafo#available_output > 0 then (
		      let o = trafo#get_string in
		      let ms' = Xdr_mstring.string_to_mstring o in
		      ol := ms' :: !ol;
		    )
		  done;
		  loop in_list' !ol
	  )
      | [] ->
	  trafo # finish;
	  let out_list' =
	    if trafo#available_output > 0 then
	      let o = trafo#get_string in
	      let ms' = Xdr_mstring.string_to_mstring o in
	      ms' :: out_list
	    else
	      out_list in
	  List.rev out_list' in
  loop ms_list []


let hash_mstrings (hash:Cryptokit.hash) ms_list =
  (* Like Cryptokit's hash_string, but for "mstring list" *)
  let blen = 1024 in
  let s = String.create blen in

  let rec loop in_list =
    match in_list with
      | ms :: in_list' ->
	  let ms_len = ms#length in
	  ( match ms#preferred with
	      | `String ->
		  let (s,start) = ms#as_string in
		  hash#add_substring s start ms_len;
		  loop in_list'
	      | `Memory ->
		  let (m,start) = ms#as_memory in
		  let k = ref 0 in
		  while !k < ms_len do
		    let n = min blen (ms_len - !k) in
		    Netsys_mem.blit_memory_to_string
		      m (start + !k) s 0 n;
		    hash#add_substring s 0 n;
		    k := !k + n;
		  done;
		  loop in_list'
	  )
      | [] ->
	  hash#result in
  loop ms_list
  

let hmac_sha1_mstrings key ms_list =
  let h = Cryptokit.MAC.hmac_sha1 key in
  hash_mstrings h ms_list


(* Encryption for GSS-API *)

module AES_CTS = struct
  (* FIXME: avoid copying strings all the time *)

  let c = 128 (* bits *)

  let m = 1 (* byte *)

  let encrypt key s =
    (* AES with CTS as defined in RFC 3962, section 5. It is a bit unclear
       why the RFC uses CTS because the upper layer already ensures that
       s consists of a whole number of cipher blocks
     *)
    let l = String.length s in
    if l <= 16 then (
      (* Corner case: exactly one AES block of 128 bits or less *)
      let cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.ECB
	  ~pad:Cryptokit.Padding.length (* any padding is ok here *)
	  key Cryptokit.Cipher.Encrypt in
      Cryptokit.transform_string cipher s
    )
    else (
      (* Cipher-text stealing, also see
	 http://en.wikipedia.org/wiki/Ciphertext_stealing
	 http://www.wordiq.com/definition/Ciphertext_stealing
       *)
      (* Cryptokit's padding feature is unusable here *)
      let m = l mod 16 in
      let s_padded = 
	if m = 0 then s else s ^ String.make (16-m) '\000' in
      let cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.CBC
	  key Cryptokit.Cipher.Encrypt in
      let u = Cryptokit.transform_string cipher s_padded in
      let ulen = String.length u in
      assert(ulen >= 32 && ulen mod 16 = 0);
      let v = String.sub u (ulen-16) 16 in
      String.blit u (ulen-32) u (ulen-16) 16;
      String.blit v 0 u (ulen-32) 16;
      String.sub u 0 l
    )

  let encrypt_mstrings key ms_list =
    (* Exactly the same, but we get input as "mstring list" and return output
       in the same way
     *)
    let l = Xdr_mstring.length_mstrings ms_list in
    if l <= 16 then (
      let cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.ECB
	  ~pad:Cryptokit.Padding.length (* any padding is ok here *)
	  key Cryptokit.Cipher.Encrypt in
      transform_mstrings cipher ms_list
    )
    else (
      let m = l mod 16 in
      let ms_padded =
	if m=0 then ms_list else
	  ms_list @ 
	    [ Xdr_mstring.string_to_mstring (String.make (16-m) '\000') ] in
      let cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.CBC
	  key Cryptokit.Cipher.Encrypt in
      let u = transform_mstrings cipher ms_padded in
      let ulen = Xdr_mstring.length_mstrings u in
      assert(ulen >= 32 && ulen mod 16 = 0);

      let u0 = Xdr_mstring.shared_sub_mstrings u 0 (ulen-32) in
      let u1 = Xdr_mstring.shared_sub_mstrings u (ulen-32) 16 in
      let u2 = Xdr_mstring.shared_sub_mstrings u (ulen-16) 16 in

      let u' = u0 @ u2 @ u1 in
      Xdr_mstring.shared_sub_mstrings u' 0 l
    )
    

  let decrypt key s =
    let l = String.length s in
    if l <= 16 then (
      if l <> 16 then
	invalid_arg "Netmech_scram.AES256_CTS: bad length of plaintext";
      let cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.ECB
	  key Cryptokit.Cipher.Decrypt in
      Cryptokit.transform_string cipher s
	(* This string is still padded! *)
    ) else (
      let k_last = ((l - 1) / 16) * 16 in
      let k_last_len = l - k_last in
      let k_second_to_last = k_last - 16 in
      let dn_cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.ECB
	  key Cryptokit.Cipher.Decrypt in
      let c_2nd_to_last = String.sub s k_second_to_last 16 in
      let dn = 
	Cryptokit.transform_string dn_cipher c_2nd_to_last in
      let cn =
	(String.sub s k_last k_last_len) ^ 
	  (String.sub dn k_last_len (16 - k_last_len)) in
      let u = String.create (k_last+16) in
      String.blit s 0 u 0 k_second_to_last;
      String.blit cn 0 u k_second_to_last 16;
      String.blit c_2nd_to_last 0 u k_last 16;
      let cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.CBC
	  key Cryptokit.Cipher.Decrypt in
      let v = Cryptokit.transform_string cipher u in
      String.sub v 0 l
    )


  let decrypt_mstrings key ms_list =
    let l = Xdr_mstring.length_mstrings ms_list in
    if l <= 16 then (
      if l <> 16 then
	invalid_arg "Netmech_scram.AES256_CTS: bad length of plaintext";
      let cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.ECB
	  key Cryptokit.Cipher.Decrypt in
      transform_mstrings cipher ms_list
	(* This string is still padded! *)
    ) else (
      let k_last = ((l - 1) / 16) * 16 in
      let k_last_len = l - k_last in
      let k_second_to_last = k_last - 16 in
      let dn_cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.ECB
	  key Cryptokit.Cipher.Decrypt in
      let c_2nd_to_last = 
	Xdr_mstring.shared_sub_mstrings ms_list k_second_to_last 16 in
      let dn = 
	transform_mstrings dn_cipher c_2nd_to_last in
      let cn0 =
	Xdr_mstring.shared_sub_mstrings ms_list k_last k_last_len in
      let cn1 =
	Xdr_mstring.shared_sub_mstrings dn k_last_len (16-k_last_len) in
      let cn = cn0 @ cn1 in
      let s0 =
	Xdr_mstring.shared_sub_mstrings ms_list 0 k_second_to_last in
      let u =
	s0 @ cn @ c_2nd_to_last in
      let cipher =
	Cryptokit.Cipher.aes
	  ~mode:Cryptokit.Cipher.CBC
	  key Cryptokit.Cipher.Decrypt in
      let v = transform_mstrings cipher u in
      Xdr_mstring.shared_sub_mstrings v 0 l
    )

  (* Test vectors from the RFC (for 128 bit AES): *)

  let k_128 =
    "\x63\x68\x69\x63\x6b\x65\x6e\x20\x74\x65\x72\x69\x79\x61\x6b\x69"

  let v1_in =
    "\x49\x20\x77\x6f\x75\x6c\x64\x20\x6c\x69\x6b\x65\x20\x74\x68\x65\x20"

  let v1_out =
    "\xc6\x35\x35\x68\xf2\xbf\x8c\xb4\xd8\xa5\x80\x36\x2d\xa7\xff\x7f\x97"

  let v2_in =
    "\x49\x20\x77\x6f\x75\x6c\x64\x20\x6c\x69\x6b\x65\x20\x74\x68\x65\x20\
     \x47\x65\x6e\x65\x72\x61\x6c\x20\x47\x61\x75\x27\x73\x20"

  let v2_out =
    "\xfc\x00\x78\x3e\x0e\xfd\xb2\xc1\xd4\x45\xd4\xc8\xef\xf7\xed\x22\
     \x97\x68\x72\x68\xd6\xec\xcc\xc0\xc0\x7b\x25\xe2\x5e\xcf\xe5"

  let v3_in =
    "\x49\x20\x77\x6f\x75\x6c\x64\x20\x6c\x69\x6b\x65\x20\x74\x68\x65\
     \x20\x47\x65\x6e\x65\x72\x61\x6c\x20\x47\x61\x75\x27\x73\x20\x43"

  let v3_out =
    "\x39\x31\x25\x23\xa7\x86\x62\xd5\xbe\x7f\xcb\xcc\x98\xeb\xf5\xa8\
     \x97\x68\x72\x68\xd6\xec\xcc\xc0\xc0\x7b\x25\xe2\x5e\xcf\xe5\x84"

  let v4_in =
    "\x49\x20\x77\x6f\x75\x6c\x64\x20\x6c\x69\x6b\x65\x20\x74\x68\x65\
     \x20\x47\x65\x6e\x65\x72\x61\x6c\x20\x47\x61\x75\x27\x73\x20\x43\
     \x68\x69\x63\x6b\x65\x6e\x2c\x20\x70\x6c\x65\x61\x73\x65\x2c"

  let v4_out =
    "\x97\x68\x72\x68\xd6\xec\xcc\xc0\xc0\x7b\x25\xe2\x5e\xcf\xe5\x84\
     \xb3\xff\xfd\x94\x0c\x16\xa1\x8c\x1b\x55\x49\xd2\xf8\x38\x02\x9e\
     \x39\x31\x25\x23\xa7\x86\x62\xd5\xbe\x7f\xcb\xcc\x98\xeb\xf5"

  let v5_in =
    "\x49\x20\x77\x6f\x75\x6c\x64\x20\x6c\x69\x6b\x65\x20\x74\x68\x65\
     \x20\x47\x65\x6e\x65\x72\x61\x6c\x20\x47\x61\x75\x27\x73\x20\x43\
     \x68\x69\x63\x6b\x65\x6e\x2c\x20\x70\x6c\x65\x61\x73\x65\x2c\x20"

  let v5_out =
    "\x97\x68\x72\x68\xd6\xec\xcc\xc0\xc0\x7b\x25\xe2\x5e\xcf\xe5\x84\
     \x9d\xad\x8b\xbb\x96\xc4\xcd\xc0\x3b\xc1\x03\xe1\xa1\x94\xbb\xd8\
     \x39\x31\x25\x23\xa7\x86\x62\xd5\xbe\x7f\xcb\xcc\x98\xeb\xf5\xa8"

  let v6_in =
    "\x49\x20\x77\x6f\x75\x6c\x64\x20\x6c\x69\x6b\x65\x20\x74\x68\x65\
     \x20\x47\x65\x6e\x65\x72\x61\x6c\x20\x47\x61\x75\x27\x73\x20\x43\
     \x68\x69\x63\x6b\x65\x6e\x2c\x20\x70\x6c\x65\x61\x73\x65\x2c\x20\
     \x61\x6e\x64\x20\x77\x6f\x6e\x74\x6f\x6e\x20\x73\x6f\x75\x70\x2e"

  let v6_out =
    "\x97\x68\x72\x68\xd6\xec\xcc\xc0\xc0\x7b\x25\xe2\x5e\xcf\xe5\x84\
     \x39\x31\x25\x23\xa7\x86\x62\xd5\xbe\x7f\xcb\xcc\x98\xeb\xf5\xa8\
     \x48\x07\xef\xe8\x36\xee\x89\xa5\x26\x73\x0d\xbc\x2f\x7b\xc8\x40\
     \x9d\xad\x8b\xbb\x96\xc4\xcd\xc0\x3b\xc1\x03\xe1\xa1\x94\xbb\xd8"

  let tests =
    [ k_128, v1_in, v1_out;
      k_128, v2_in, v2_out;
      k_128, v3_in, v3_out;
      k_128, v4_in, v4_out;
      k_128, v5_in, v5_out;
      k_128, v6_in, v6_out;
    ]

  let run_tests() =
    List.for_all
      (fun (k, v_in, v_out) ->
	 encrypt k v_in = v_out &&
	  decrypt k v_out = v_in
      )
      tests

  let run_mtests() =
    let j = ref 1 in
    List.for_all
      (fun (k, v_in, v_out) ->
	 prerr_endline("Test: " ^ string_of_int !j);
	 let v_in_ms = Xdr_mstring.string_to_mstring v_in in
	 let v_out_ms = Xdr_mstring.string_to_mstring v_out in
	 let e = 
	   Xdr_mstring.concat_mstrings (encrypt_mstrings k [v_in_ms]) in
	 prerr_endline "  enc ok";
	 let d =
	   Xdr_mstring.concat_mstrings (decrypt_mstrings k [v_out_ms]) in
	 prerr_endline "  dec ok";
	 incr j;
	 e = v_out && d = v_in
      )
      tests
end


module Cryptosystem = struct
  (* RFC 3961 section 5.3 *)

  module C = AES_CTS
    (* Cipher *)

  module I = struct   (* Integrity *)
    let hmac = hmac  (* hmac-sha1 *)
    let hmac_mstrings = hmac_sha1_mstrings
    let h = 12
  end

  exception Integrity_error

  let derive_keys protocol_key usage =
    let k = 8 * String.length protocol_key in
    if k <> 128 && k <> 256 then
      invalid_arg "Netmech_scram.Cryptosystem.derive_keys";
    let derive kt =
      Netauth.derive_key_rfc3961_simplified
	~encrypt:(C.encrypt protocol_key)
	~random_to_key:(fun s -> s)
	~block_size:C.c
	~k
	~usage
	~key_type:kt in
    { kc = derive `Kc;
      ke = derive `Ke;
      ki = derive `Ki;
    }

  let encrypt_and_sign s_keys message =
    let c_bytes = C.c/8 in
    let conf = String.make c_bytes '\000' in
    Netsys_rng.fill_random conf;
    let l = String.length message in
    let p = (l + c_bytes) mod C.m in
    let pad = 
      if p = 0 then "" else String.make (C.m - p) '\000' in
    let p1 = conf ^ message ^ pad in
    let c1 = C.encrypt s_keys.ke p1 in
    let h1 = I.hmac s_keys.ki p1 in
    c1 ^ String.sub h1 0 I.h

  let encrypt_and_sign_mstrings s_keys message =
    let c_bytes = C.c/8 in
    let conf = String.make c_bytes '\000' in
    Netsys_rng.fill_random conf;
    let l = Xdr_mstring.length_mstrings message in
    let p = (l + c_bytes) mod C.m in
    let pad = 
      if p = 0 then "" else String.make (C.m - p) '\000' in
    let p1 =
      ( ( Xdr_mstring.string_to_mstring conf ) :: message ) @
	[ Xdr_mstring.string_to_mstring pad ] in
    let c1 = C.encrypt_mstrings s_keys.ke p1 in
    let h1 = I.hmac_mstrings s_keys.ki p1 in
    c1 @ [ Xdr_mstring.string_to_mstring(String.sub h1 0 I.h) ]

  let decrypt_and_verify s_keys ciphertext =
    let c_bytes = C.c/8 in
    let l = String.length ciphertext in
    if l < I.h then
      invalid_arg "Netmech_scram.Cryptosystem.decrypt_and_verify";
    let c1 = String.sub ciphertext 0 (l - I.h) in
    let h1 = String.sub ciphertext (l - I.h) I.h in
    let p1 = C.decrypt s_keys.ke c1 in
    let h1' = String.sub (I.hmac s_keys.ki p1) 0 I.h in
    if h1 <> h1' then
      raise Integrity_error;
    let q = String.length p1 in
    if q < c_bytes then
      raise Integrity_error;
    String.sub p1 c_bytes (q-c_bytes)
      (* This includes any padding or residue from the lower layer! *)


  let decrypt_and_verify_mstrings s_keys ciphertext =
    let c_bytes = C.c/8 in
    let l = Xdr_mstring.length_mstrings ciphertext in
    if l < I.h then
      invalid_arg "Netmech_scram.Cryptosystem.decrypt_and_verify";
    let c1 = Xdr_mstring.shared_sub_mstrings ciphertext 0 (l - I.h) in
    let h1 = 
      Xdr_mstring.concat_mstrings
	(Xdr_mstring.shared_sub_mstrings ciphertext (l - I.h) I.h) in
    let p1 = C.decrypt_mstrings s_keys.ke c1 in
    let h1' = String.sub (I.hmac_mstrings s_keys.ki p1) 0 I.h in
    if h1 <> h1' then
      raise Integrity_error;
    let q = Xdr_mstring.length_mstrings p1 in
    if q < c_bytes then
      raise Integrity_error;
    Xdr_mstring.shared_sub_mstrings p1 c_bytes (q-c_bytes)
      (* This includes any padding or residue from the lower layer! *)

  let get_ec s_keys n =
    if n < 16 then invalid_arg "Netmech_scram.Cryptosystem.get_ec";
    0

  let get_mic s_keys message =
    String.sub (I.hmac s_keys.kc message) 0 I.h

  let get_mic_mstrings s_keys message =
    String.sub (I.hmac_mstrings s_keys.kc message) 0 I.h

end
