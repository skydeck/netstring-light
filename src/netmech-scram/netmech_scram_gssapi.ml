(* $Id: netmech_scram_gssapi.ml 1562 2011-03-07 16:13:14Z gerd $ *)

(* FIXME:
   - export_sec_context: the token does not include the sequence numbers,
     and it does not include the flags
 *)

open Netgssapi
open Printf

class scram_name (name_string:string) (name_type:oid) =
object
  method otype = ( `Name :  [`Name] )
  method name_string = name_string
  method name_type = name_type
end


type cred =
  | Cred_server                      (* there are no server credentials! *)
  | Cred_client of string * string   (* user name, password *)
  | Cred_none


class scram_cred (name:name) (cred:cred) =
object
  method otype = ( `Credential : [`Credential] )
  method name = name
  method cred = cred
end


type ctx =
  | Ctx_client of Netmech_scram.client_session
  | Ctx_server of Netmech_scram.server_session

class scram_context ctx (init_flags : ret_flag list) =
  let valid = ref true in
  let server_cb = ref "" in
  let specific_keys = ref None in
  let seq_nr = ref 0L in
  let exp_seq_nr = ref None in
  let flags = ref init_flags in
object
  method otype = ( `Context : [ `Context ] )
  method valid = !valid
  method ctx = ctx
  method delete() = valid := false
  method server_cb = server_cb
  method is_acceptor =
    match ctx with
      | Ctx_client _ -> false
      | Ctx_server _ -> true
  method specific_keys =
    match !specific_keys with
      | Some(k_mic_c,k_mic_s,k_wrap_c,k_wrap_s) -> 
	  Some(k_mic_c,k_mic_s,k_wrap_c,k_wrap_s)
      | None ->
	  let proto_key_opt =
	    match ctx with
	      | Ctx_client sess -> 
		  Netmech_scram.client_protocol_key sess
	      | Ctx_server sess -> 
		  Netmech_scram.server_protocol_key sess in
	  (* The usage numbers are defined in RFC 4121 *)
	  (match proto_key_opt with
	     | None -> None
	     | Some proto_key ->
		 let k_mic_c = 
		   Netmech_scram.Cryptosystem.derive_keys
		     proto_key 25 in
		 let k_mic_s = 
		   Netmech_scram.Cryptosystem.derive_keys
		     proto_key 23 in
		 let k_wrap_c = 
		   Netmech_scram.Cryptosystem.derive_keys
		     proto_key 24 in
		 let k_wrap_s = 
		   Netmech_scram.Cryptosystem.derive_keys
		     proto_key 22 in
(*
eprintf "protocol key: %S\n" proto_key;
eprintf "k_mic_c.kc: %S\n" k_mic_c.Netmech_scram.kc;
eprintf "k_mic_s.kc: %S\n" k_mic_s.Netmech_scram.kc;
eprintf "k_wrap_c.ke: %S\n" k_wrap_c.Netmech_scram.ke;
eprintf "k_wrap_c.ki: %S\n" k_wrap_c.Netmech_scram.ki;
eprintf "k_wrap_s.ke: %S\n" k_wrap_s.Netmech_scram.ke;
eprintf "k_wrap_s.ki: %S\n%!" k_wrap_s.Netmech_scram.ki;
 *)
		 specific_keys := Some(k_mic_c,k_mic_s,k_wrap_c,k_wrap_s);
		 !specific_keys
	  )
  method seq_nr = 
    let n = !seq_nr in
    seq_nr := Int64.succ !seq_nr;
    n

  method is_peer_seq_nr_ok n : suppl_status list =
    match !exp_seq_nr with
      | None ->
	  exp_seq_nr := Some n;
	  []
      | Some e ->
	  if n = e then (
	    exp_seq_nr := Some (Int64.succ e);
	    []
	  ) else (
	    if n < e then
	      [ `Unseq_token ]
	    else
	      [ `Gap_token ]
	  )

  method flags = flags
end



module type BACK_COERCE_OBJECT = sig
  type t
  val hide : t -> < >
  val exhibit : < > -> t
end


module Back_coerce_table(T:BACK_COERCE_OBJECT) : sig 
  type table
  val create : unit -> table
  val store : table -> T.t -> unit
  val retrieve : table -> < > -> T.t
end = struct
  module E = struct
    type t = < >
    let equal x y = x = y
    let hash x = Hashtbl.hash x
  end

  module W = Weak.Make(E)

  type table = W.t

  let create() =
    W.create 10

  let store table (x : T.t) =
    ignore(W.merge table (T.hide x))

  let retrieve table (x : < >) : T.t =
    if W.mem table x then
      T.exhibit x
    else
      invalid_arg "Netmech_scram_gssapi: Unknown opaque object"
end

module Credential = struct
  type t = scram_cred
  let hide x = (x :> < >)
  let exhibit x = (Obj.magic x : t)
end

module CredentialBCT = Back_coerce_table(Credential)

module Name = struct
  type t = scram_name
  let hide x = (x :> < >)
  let exhibit x = (Obj.magic x : t)
end

module NameBCT = Back_coerce_table(Name)

module Context = struct
  type t = scram_context
  let hide x = (x :> < > )
  let exhibit x = (Obj.magic x : t)
end


module ContextBCT = Back_coerce_table(Context)


class type client_key_ring =
object
  method password_of_user_name : string -> string
  method default_user_name : string option
end


let empty_client_key_ring : client_key_ring =
object
  method password_of_user_name _ = raise Not_found
  method default_user_name = None
end


class type server_key_verifier =
object
  method scram_credentials : string -> string * string * int
end


let empty_server_key_verifier : server_key_verifier =
object
  method scram_credentials _ = raise Not_found
end

let scram_mech = [| 1; 3; 6; 1; 5; 5; 14 |]


(*
let as_string (sm,pos,len) =
  match sm with
    | `String s ->
	if pos=0 && len=String.length s then
	  s
	else
	  String.sub s pos len
    | `Memory m -> 
	let s = String.create len in
	Netsys_mem.blit_memory_to_string m pos s 0 len;
	s
 *)

(*
let empty_msg = (`String "",0,0)
 *)

exception Calling_error of calling_error
exception Routine_error of routine_error


class scram_gss_api ?(client_key_ring = empty_client_key_ring)
                    ?(server_key_verifier = empty_server_key_verifier)
		    profile
                    : gss_api =
  let scram_ret_flags =
    [ `Mutual_flag; `Conf_flag; `Integ_flag; `Replay_flag; `Sequence_flag ] in

  let credentials = CredentialBCT.create() in
  let names = NameBCT.create() in
  let contexts = ContextBCT.create() in
  let cred_retrieve obj =
    CredentialBCT.retrieve credentials (obj : credential :> < >) in
  let name_retrieve obj =
    NameBCT.retrieve names (obj : name :> < >) in
  let context_retrieve obj =
    ContextBCT.retrieve contexts (obj : context :> < >) in
  let no_cred = 
    ( object
	method otype = `Credential 
	method name = assert false
	method cred = Cred_none
      end
    ) in
  let no_cred_out = (no_cred :> credential) in
  let () = CredentialBCT.store credentials no_cred in
  let no_name =
    ( object
	method otype = `Name
	method name_type = [| |]
	method name_string = ""
      end
    ) in
  let no_name_out = (no_name :> name) in
  let () = NameBCT.store names no_name in
  let default_qop =
    ( object method otype = `QOP end ) in  (* just return something *)
object(self)
  method provider = "Netmech_scram_gssapi.scap_gss_api"

  method no_credential = (no_cred :> credential)

  method no_name = (no_name :> name)

  method accept_sec_context : 
          't . context:context option ->
               acceptor_cred:credential -> 
               input_token:token ->
               chan_bindings:channel_bindings option ->
               out:( src_name:name ->
		     mech_type:oid ->
		     output_context:context option ->
		     output_token:token ->
		     ret_flags:ret_flag list ->
		     time_rec:[ `Indefinite | `This of float] ->
		     delegated_cred:credential ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't 
		   ) -> unit -> 't =
    fun ~context ~acceptor_cred ~input_token ~chan_bindings ~out () ->
      let acc_name =
	new scram_name "@" nt_hostbased_service in
      NameBCT.store names acc_name;
      let src_name = (acc_name :> name) in
      try
	let cb_data =
	  match chan_bindings with
	    | None -> ""
	    | Some (init_addr, acc_addr, cb_data) -> cb_data in
	(* We ignore init_addr and acc_addr... CHECK *)
	let acceptor_cred =
	  cred_retrieve acceptor_cred in
	if acceptor_cred <> no_cred && acceptor_cred#cred <> Cred_server then
	  raise(Routine_error `No_cred);
	let context, sess, is_first =
	  match context with
	    | None ->
		let sess =
		  Netmech_scram.create_server_session
		    profile
		    server_key_verifier#scram_credentials in
		let ctx =
		  Ctx_server sess in
		let context = new scram_context ctx scram_ret_flags in
		(context # server_cb) := cb_data;
		ContextBCT.store contexts context;
		(context, sess, true)
	    | Some c -> 
		let context = context_retrieve c in
		if not context#valid then
		  raise (Routine_error `No_context);
		let sess =
		  match context#ctx with
		    | Ctx_server sess -> sess
		    | Ctx_client _ -> raise (Routine_error `No_context) in
		(context, sess, false) in
	let eff_input_token =
	  if is_first then (* There is a header *)
	    try
	      let k = ref 0 in
	      let (oid, tok) = Netgssapi.wire_decode_token input_token k in
	      if !k <> String.length input_token then
		raise(Routine_error `Defective_token);
	      if oid <> scram_mech then
		raise(Routine_error `Bad_mech);
	      tok
	    with
	      | Failure _ ->
		  raise(Routine_error `Defective_token);
	  else
	    input_token in
	(* The following call usually does not raise exceptions. Error codes
	   are stored inside sess
	 *)
	Netmech_scram.server_recv_message sess eff_input_token;
	let output_context =
	  Some (context :> context) in
	let output_token =
	  Netmech_scram.server_emit_message sess in
	if Netmech_scram.server_error_flag sess then (
	  out
	    ~src_name ~mech_type:scram_mech ~output_context
	    ~output_token
	    ~ret_flags:scram_ret_flags ~time_rec:`Indefinite
	    ~delegated_cred:no_cred_out
	    ~minor_status:0l ~major_status:(`None,`Failure,[]) ()
	)
	else
	  if Netmech_scram.server_finish_flag sess then (
	    (* Finally check channel bindings: *)
	    let scram_cb =
	      match Netmech_scram.server_channel_binding sess with
		| None -> assert false
		| Some d -> d in
	    if scram_cb <> !(context # server_cb) then
	      raise(Routine_error `Bad_bindings);
	    let ret_flags =
	      [`Prot_ready_flag; `Trans_flag] @ scram_ret_flags in 
	    context # flags := ret_flags;
	    out
	      ~src_name ~mech_type:scram_mech ~output_context
	      ~output_token
	      ~ret_flags
	      ~time_rec:`Indefinite
	      ~delegated_cred:no_cred_out
	      ~minor_status:0l ~major_status:(`None,`None,[]) ()
	  )
	  else (
	    out
	      ~src_name ~mech_type:scram_mech ~output_context
	      ~output_token
	      ~ret_flags:scram_ret_flags ~time_rec:`Indefinite
	      ~delegated_cred:no_cred_out
	      ~minor_status:0l ~major_status:(`None,`None,[`Continue_needed])
	      ()
	  )
      with
	| Calling_error code ->
	    out
	      ~src_name ~mech_type:scram_mech ~output_context:None
	      ~output_token:""
	      ~ret_flags:scram_ret_flags ~time_rec:`Indefinite
	      ~delegated_cred:no_cred_out
	      ~minor_status:0l ~major_status:(code,`None,[]) ()
	| Routine_error code ->
	    out
	      ~src_name ~mech_type:scram_mech ~output_context:None
	      ~output_token:""
	      ~ret_flags:scram_ret_flags ~time_rec:`Indefinite
	      ~delegated_cred:no_cred_out
	      ~minor_status:0l ~major_status:(`None,code,[]) ()

  method private get_client_cred user =  (* or Not_found *)
    let pw = client_key_ring # password_of_user_name user in
    let name =
      new scram_name user nt_user_name in
    NameBCT.store names name;
    let cred = 
      new scram_cred (name:>name) (Cred_client(user,pw)) in
    CredentialBCT.store credentials cred;
    cred

  method private get_default_client_cred() = (* or Not_found *)
    match client_key_ring # default_user_name with
      | None -> raise Not_found
      | Some user -> self # get_client_cred user
    
  method acquire_cred :
          't . desired_name:name ->
               time_req:[`None | `Indefinite | `This of float] ->
               desired_mechs:oid_set ->
               cred_usage:cred_usage  ->
               out:( cred:credential ->
		     actual_mechs:oid_set ->
		     time_rec:[ `Indefinite | `This of float] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~desired_name ~time_req ~desired_mechs ~cred_usage ~out ()  ->
      let desired_name = name_retrieve desired_name in
      let error code =
	out
	  ~cred:no_cred_out ~actual_mechs:[] ~time_rec:`Indefinite
	  ~minor_status:0l ~major_status:(`None,code,[]) () in
      match cred_usage with
	| `Initiate ->
	    (* For clients *)
	    if List.mem scram_mech desired_mechs then (
	      let out_client_cred user =
		try
		  let cred = self#get_client_cred user in
		  out
		    ~cred:(cred :> credential)
		    ~actual_mechs:[ scram_mech ]
		    ~time_rec:`Indefinite
		    ~minor_status:0l
		    ~major_status:(`None,`None,[])
		    ()
		with
		  | Not_found -> error `No_cred in
	      (* Expect nt_user_name: *)
	      if desired_name # name_type = Netgssapi.nt_user_name then (
		let user = desired_name # name_string in
		out_client_cred user
	      )
	      else (
		if desired_name = no_name then (
		  (* maybe we have a default: *)
		  match client_key_ring # default_user_name with
		    | None -> error `No_cred
		    | Some user -> out_client_cred user
		)		
		else
		  error `Bad_nametype
	      )
	    )
	    else
	      error `Bad_mech
	| `Accept ->
	    (* For server: Effectively there are no credentials. So we accept
	       any desired_name.
	     *)
	    if List.mem scram_mech desired_mechs then (
	      let server_name =
		new scram_name "@" nt_hostbased_service  in
	      NameBCT.store names server_name;
	      let cred =
		new scram_cred (server_name :> name) Cred_server in
	      CredentialBCT.store credentials cred;
	      out
		~cred:(cred :> credential) 
		~actual_mechs:[ scram_mech ]
		~time_rec:`Indefinite
		~minor_status:0l
		~major_status:(`None,`None,[])
		()
	    )
	    else
	      error `Bad_mech
	| `Both ->
	    (* Not supported - credentials are either for the client or
	       for the server
	     *)
	    out
	      ~cred:no_cred_out ~actual_mechs:[] ~time_rec:`Indefinite
	      ~minor_status:0l ~major_status:(`None,`Bad_nametype,[]) ()
	      
  method add_cred :
          't . input_cred:credential ->
               desired_name:name ->
               desired_mech:oid ->
               cred_usage:cred_usage ->
               initiator_time_req:[`None | `Indefinite | `This of float] ->
               acceptor_time_req:[`None | `Indefinite | `This of float] ->
               out:( output_cred:credential ->
		     actual_mechs:oid_set ->
		     initiator_time_rec:[ `Indefinite | `This of float] ->
		     acceptor_time_rec:[ `Indefinite | `This of float] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~input_cred ~desired_name ~desired_mech ~cred_usage 
        ~initiator_time_req ~acceptor_time_req ~out () 
      ->
	(* More or less it is not possible to add to credentials - we have
	   here only one mechanism. So, the only thing to do here is to
	   create the right error message.
	 *)
	let error code =
	  out
	    ~output_cred:no_cred_out ~actual_mechs:[] 
	    ~initiator_time_rec:`Indefinite ~acceptor_time_rec:`Indefinite
	    ~minor_status:0l ~major_status:(`None,code,[]) () in
	let input_cred = cred_retrieve input_cred in
	let desired_name = name_retrieve desired_name in
	let add cred =
	  if scram_mech = desired_mech then
	    error `Duplicate_element
	  else
	    error `Bad_mech in
	if input_cred = no_cred then (
	  self # acquire_cred 
	    ~desired_name:(desired_name :> name)
	    ~time_req:`None ~desired_mechs:[desired_mech] ~cred_usage
	    ~out:(
	      fun ~cred ~actual_mechs ~time_rec ~minor_status ~major_status() ->
		let (_,code,_) = major_status in
		if code = `None then add cred else error code
	    )
	    ()
	) else
	  add input_cred
	    
  method canonicalize_name :
          't . input_name:name ->
               mech_type:oid ->
               out:( output_name:name ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~input_name ~mech_type ~out () ->
      let error code =
	out 
	  ~output_name:no_name_out ~minor_status:0l
	  ~major_status:(`None,code,[]) ()
      in
      let input_name = name_retrieve input_name in
      if mech_type <> scram_mech then
	error `Bad_mech
      else
	out
	  ~output_name:(input_name :> name) ~minor_status:0l
	  ~major_status:(`None,`None,[]) ()

  method compare_name :
          't . name1:name ->
               name2:name ->
               out:( name_equal:bool ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~name1 ~name2 ~out () ->
      let name1 = name_retrieve name1 in
      let name2 = name_retrieve name2 in
      let equal =
	name1 # name_type <> nt_anonymous &&
	  name2 # name_type <> nt_anonymous &&
	  (name1 = name2 || 
	      (name1#name_type = name2#name_type && 
		  name1#name_string = name2#name_string)) in
      out ~name_equal:equal ~minor_status:0l ~major_status:(`None,`None,[]) ()
	
  method context_time :
          't . context:context ->
               out:( time_rec:[ `Indefinite | `This of float] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~context ~out () ->
      let context = context_retrieve context in
      if not context#valid then
	out ~time_rec:`Indefinite ~minor_status:0l
	  ~major_status:(`None,`No_context,[]) ()
      else
	out
	  ~time_rec:`Indefinite ~minor_status:0l ~major_status:(`None,`None,[])
	  ()

  method delete_sec_context :
          't . context:context ->
               out:( minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~context ~out () ->
      let context = context_retrieve context in
      context#delete();
      out ~minor_status:0l ~major_status:(`None,`None,[]) ()
	
  method display_name :
          't . input_name:name ->
               out:( output_name:string ->
		     output_name_type:oid ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~input_name ~out () ->
      (* We just return the name_string *)
      let input_name = name_retrieve input_name in
      out
	~output_name:input_name#name_string
	~output_name_type:input_name#name_type
	~minor_status:0l
	~major_status:(`None,`None,[])
	()

  method display_minor_status :
          't . minor_status:minor_status ->
               mech_type: oid ->
               out:( status_strings: string list ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~minor_status ~mech_type ~out ()  ->
      out
	~status_strings:["<minor>"] 
	~minor_status:0l ~major_status:(`None,`None,[]) ()
	
  method export_name :
          't . name:name ->
               out:( exported_name:string ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~name ~out () ->
      let name = name_retrieve name in
      let s1 =
	encode_exported_name name#name_type name#name_string in
      let s2 =
      encode_exported_name scram_mech s1 in
      out
	~exported_name:s2
	~minor_status:0l
	~major_status:(`None,`None,[])
	()

  method export_sec_context :
          't . context:context ->
               out:( interprocess_token:interprocess_token ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~context ~out () ->
      (* FIXME: Maybe we should also export the seq_nr *)
      let context = context_retrieve context in
      if not context#valid then
	out
	  ~interprocess_token:"" ~minor_status:0l
	  ~major_status:(`None,`No_context,[]) ()
      else (
	try
	  let interprocess_token =
	    match context#ctx with
	      | Ctx_client sess ->
		  "C" ^ Netmech_scram.client_export sess
	      | Ctx_server sess ->
		  "S" ^ Netmech_scram.server_export sess in
	  out
	    ~interprocess_token ~minor_status:0l
	    ~major_status:(`None,`None,[]) ()
	with
	  | Failure _ ->
	      out
		~interprocess_token:"" ~minor_status:0l
		~major_status:(`None,`Unavailable,[]) ()
      )


  method get_mic :
          't . context:context ->
               qop_req:qop option ->
               message:message ->
               out:( msg_token:token ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~context ~qop_req ~message ~out () ->
      let context = context_retrieve context in
      if not context#valid then
	out
	  ~msg_token:"" ~minor_status:0l
	  ~major_status:(`None,`No_context,[]) ()
      else (
	(* Reject any QOP: *)
	if qop_req <> None && qop_req <> Some default_qop then
	  out
	    ~msg_token:"" ~minor_status:0l
	    ~major_status:(`None,`Bad_QOP,[]) ()
	else (
	  let sk_opt = context # specific_keys in
	  match sk_opt with
	    | None ->
		out
		  ~msg_token:"" ~minor_status:0l
		  ~major_status:(`None,`No_context,[]) ()
	    | Some (k_mic_c,k_mic_s,k_wrap_c,k_wrap_s) ->
		let sk_mic =
		  if context#is_acceptor then k_mic_s else k_mic_c in
		let sequence_number = context # seq_nr in
		let sent_by_acceptor = context # is_acceptor in
		let token =
		  Netgssapi.create_mic_token
		    ~sent_by_acceptor
		    ~acceptor_subkey:false
		    ~sequence_number
		    ~get_mic:(
		      Netmech_scram.Cryptosystem.get_mic_mstrings sk_mic)
		    ~message in
		out
		  ~msg_token:token ~minor_status:0l
		  ~major_status:(`None,`None,[])
		  ()
	)
      )
	
  method import_name :
          't . input_name:string ->
               input_name_type:oid ->
               out:( output_name:name ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~input_name ~input_name_type ~out () ->
      let out_name name_string name_type =
	let n = new scram_name name_string name_type in
	NameBCT.store names n;
	out
	  ~output_name:(n :> name)
	  ~minor_status:0l 
	  ~major_status:(`None,`None,[])
	  () in
      if input_name_type = nt_hostbased_service then
	try
	  let (_service,_host) = parse_hostbased_service input_name in
	  out_name input_name nt_hostbased_service
	with
	  | _ ->
	      out
		~output_name:no_name_out ~minor_status:0l 
		~major_status:(`None,`Bad_name,[]) ()
      else
	if input_name_type = nt_user_name then
	  out_name input_name nt_user_name
	else
	  if input_name_type = nt_export_name then
	    try
	      let k = ref 0 in
	      let (mech_oid,s1) = decode_exported_name input_name k in
	      if !k <> String.length input_name then failwith "too short";
	      if mech_oid <> scram_mech then 
		out
		  ~output_name:no_name_out ~minor_status:0l 
		  ~major_status:(`None,`Bad_name,[]) ()
	      else (
		k := 0;
		let (name_oid,s2) = decode_exported_name s1 k in
		if !k <> String.length input_name then failwith "too short";
		out_name s2 name_oid
	      )
	    with
	      | Failure _ ->
		  out
		    ~output_name:no_name_out ~minor_status:0l 
		    ~major_status:(`None,`Bad_name,[]) ()
	  else
	    if input_name_type = [||] then
	      out_name input_name nt_user_name
	    else
	      out
		~output_name:no_name_out
		~minor_status:0l 
		~major_status:(`None,`Bad_nametype,[])
		() 
	      
		

  method import_sec_context :
          't . interprocess_token:interprocess_token ->
               out:( context:context option ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~interprocess_token ~out () ->
      let error code =
	out ~context:None ~minor_status:0l ~major_status:(`None,code,[]) () in
      let l = String.length interprocess_token in
      if interprocess_token = "" then
	error `Defective_token
      else
	match interprocess_token.[0] with
	  | 'C' ->
	      let t = String.sub interprocess_token 1 (l-1) in
	      let sess =
		Netmech_scram.client_import t in
	      let context = 
		new scram_context (Ctx_client sess) scram_ret_flags in
	      ContextBCT.store contexts context;
	      out
		~context:(Some (context :> context)) 
		~minor_status:0l ~major_status:(`None,`None,[]) ()
	  | 'S' ->
	      let t = String.sub interprocess_token 1 (l-1) in
	      let sess =
		Netmech_scram.server_import t in
	      let context = 
		new scram_context (Ctx_server sess) scram_ret_flags in
	      ContextBCT.store contexts context;
	      out
		~context:(Some (context :> context)) 
		~minor_status:0l ~major_status:(`None,`None,[]) ()
	  | _ ->
	      error `Defective_token
		
  method indicate_mechs :
          't . out:( mech_set:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~out () ->
      out 
	~mech_set:[ scram_mech ]
	~minor_status:0l
	~major_status:(`None, `None, [])
	()

  method init_sec_context :
          't . initiator_cred:credential ->
               context:context option ->
               target_name:name ->
               mech_type:oid -> 
               req_flags:req_flag list ->
               time_rec:float option ->
               chan_bindings:channel_bindings option ->
               input_token:token option ->
               out:( actual_mech_type:oid ->
		     output_context:context option ->
		     output_token:token ->
		     ret_flags:ret_flag list ->
		     time_rec:[ `Indefinite | `This of float ] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun
          ~initiator_cred ~context ~target_name ~mech_type ~req_flags
          ~time_rec ~chan_bindings ~input_token ~out () ->
    let actual_mech_type = scram_mech in
    try
      let cb_data =
	match chan_bindings with
	  | None -> ""
	  | Some (init_addr, acc_addr, cb_data) -> cb_data in
      (* We ignore init_addr and acc_addr... CHECK *)
      let initiator_cred =
	cred_retrieve initiator_cred in
      let eff_init_cred =
	if initiator_cred = no_cred then
	  try self # get_default_client_cred()
	  with
	    | Not_found ->
		raise(Routine_error `No_cred);  (* No default *)
	else
	  initiator_cred in
      let user, pw =
	match eff_init_cred # cred with
	  | Cred_client(user,pw) -> (user,pw)
	  | _ ->
	      raise(Routine_error `No_cred) in
      let context, sess, continuation =
	match context with
	  | None ->
	      let sess =
		Netmech_scram.create_client_session
		  profile user pw in
	      let ctx =
		Ctx_client sess in
	      let context = new scram_context ctx scram_ret_flags in
	      (context # server_cb) := cb_data;
	      ContextBCT.store contexts context;
	      (context, sess, false)
	  | Some c -> 
	      let context = context_retrieve c in
	      if not context#valid then
		raise(Routine_error `No_context);
	      let sess =
		match context#ctx with
		  | Ctx_client sess -> sess
		  | Ctx_server _ -> raise (Routine_error `No_context) in
	      (context, sess, true) in
      if mech_type <> [||] && mech_type <> scram_mech then
	raise(Routine_error `Bad_mech);
      (*
	if List.mem `Deleg_flag req_flags then XXX;
	if List.mem `Anon_flag req_flags then XXX;
       *)
      (* Note that we ignore target_name entirely. It is not needed for
	 SCRAM.
       *)
      if continuation then (  (* this may raise exceptions *)
	try
	  match input_token with
	    | Some intok ->
		Netmech_scram.client_recv_message sess intok
	    | None ->
		raise(Calling_error `Bad_structure)
	with
	  | Netmech_scram.Invalid_encoding(_,_) ->
	      raise(Routine_error `Defective_token)
	  | Netmech_scram.Invalid_username_encoding(_,_) ->
	      raise(Routine_error `Defective_token)
	  | Netmech_scram.Extensions_not_supported(_,_) ->
	      raise(Routine_error `Failure)
	  | Netmech_scram.Protocol_error _ ->
	      raise(Routine_error `Failure)
	  | Netmech_scram.Invalid_server_signature ->
	      raise(Routine_error `Bad_mic)
	  | Netmech_scram.Server_error e ->
	      ( match e with
		  | `Invalid_encoding
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
		  | `Extension _ ->
		      raise(Routine_error `Failure)
	      )
      );
      if Netmech_scram.client_finish_flag sess then (
	let ret_flags =
	  [`Trans_flag; `Prot_ready_flag ] @ scram_ret_flags in
	context # flags := ret_flags;
	out
	  ~actual_mech_type ~output_context:(Some (context :> context))
	  ~output_token:""
	  ~ret_flags
	  ~time_rec:`Indefinite ~minor_status:0l
	  ~major_status:(`None,`None,[]) ()
      )
      else (
	let output_token_1 =
	  Netmech_scram.client_emit_message sess in
	let output_token =
	  if continuation then
	    output_token_1
	  else
	    Netgssapi.wire_encode_token scram_mech output_token_1 in
	let ret_flags =
	  if Netmech_scram.client_protocol_key sess <> None then
	    `Prot_ready_flag :: scram_ret_flags
	  else
	    scram_ret_flags in
	context # flags := ret_flags;
	out
	  ~actual_mech_type ~output_context:(Some (context :> context))
	  ~output_token ~ret_flags
	  ~time_rec:`Indefinite ~minor_status:0l
	  ~major_status:(`None,`None,[`Continue_needed]) ()
      )
    with
      | Calling_error code ->
	  out
	  ~actual_mech_type  ~output_context:None
	    ~output_token:"" ~ret_flags:scram_ret_flags
	  ~time_rec:`Indefinite ~minor_status:0l
	  ~major_status:(code,`None,[]) ()
      | Routine_error code ->
	  out
	  ~actual_mech_type ~output_context:None
	    ~output_token:"" ~ret_flags:scram_ret_flags
	  ~time_rec:`Indefinite ~minor_status:0l
	  ~major_status:(`None,code,[]) ()

  method inquire_context :
          't . context:context ->
               out:( src_name:name ->
                     targ_name:name ->
		     lifetime_req : [ `Indefinite | `This of float ] ->
		     mech_type:oid ->
		     ctx_flags:ret_flag list ->
		     locally_initiated:bool ->
		     is_open:bool ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun
      ~context ~out () ->
    let error code =
      out
	~src_name:no_name_out ~targ_name:no_name_out ~lifetime_req:`Indefinite
	~mech_type:scram_mech ~ctx_flags:scram_ret_flags 
	~locally_initiated:false ~is_open:false 
	~minor_status:0l ~major_status:(`None, code, []) () in
    let context = context_retrieve context in
    if context # valid then
      match context # ctx with
	| Ctx_client sess ->
	    let src_name =
	      new scram_name
		(Netmech_scram.client_user_name sess) nt_user_name in
	    NameBCT.store names src_name;
	    let src_name = (src_name :> name) in
	    let targ_name =
	      new scram_name "@" nt_hostbased_service in
	    NameBCT.store names targ_name;
	    let targ_name = (targ_name :> name) in
	    let is_open = Netmech_scram.client_finish_flag sess in
	    out
	      ~src_name ~targ_name ~lifetime_req:`Indefinite
	      ~mech_type:scram_mech ~ctx_flags:!(context # flags)
	      ~locally_initiated:true ~is_open
	      ~minor_status:0l ~major_status:(`None, `None, []) ()
	      
	| Ctx_server sess ->
	    let src_name =
	      match Netmech_scram.server_user_name sess with
		| None ->
		    no_name
		| Some u ->
		    new scram_name u nt_user_name in
	    NameBCT.store names src_name;
	    let src_name = (src_name :> name) in
	    let targ_name =
	      new scram_name "@" nt_hostbased_service in
	    NameBCT.store names targ_name;
	    let targ_name = (targ_name :> name) in
	    let is_open = Netmech_scram.server_finish_flag sess in
	    out
	      ~src_name ~targ_name ~lifetime_req:`Indefinite
	      ~mech_type:scram_mech ~ctx_flags:!(context # flags)
	      ~locally_initiated:true ~is_open
	      ~minor_status:0l ~major_status:(`None, `None, []) ()
    else
      error `No_context


  method inquire_cred :
          't . cred:credential ->
               out:( name:name ->
		     lifetime: [ `Indefinite | `This of float ] ->
		     cred_usage:cred_usage ->
		     mechanisms:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't = fun ~cred ~out () ->
    let cred = cred_retrieve cred in
    let eff_cred =
      if cred = no_cred then
	try
	  self # get_default_client_cred()
	with
	  | Not_found -> no_cred
	      (* We do not support a default initiator credential *)
      else
	cred in
    if eff_cred = no_cred then
      out
	~name:no_name_out
	~lifetime:`Indefinite
	~cred_usage:`Initiate
	~mechanisms:[]
	~minor_status:0l
	~major_status:(`None, `No_cred, [])
	()
    else
      out
	~name:eff_cred#name
	~lifetime:`Indefinite
	~cred_usage:( match eff_cred#cred with
			| Cred_server -> `Accept
			| Cred_client _ -> `Initiate
			| _ -> assert false
		    )
	~mechanisms:[ scram_mech ]
	~minor_status:0l
	~major_status:(`None, `None, [])
	()
	
  method inquire_cred_by_mech :
          't . cred:credential ->
               mech_type:oid -> 
               out:( name:name ->
		     initiator_lifetime: [ `Indefinite | `This of float ] ->
		     acceptor_lifetime: [ `Indefinite | `This of float ] ->
		     cred_usage:cred_usage ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun
      ~cred ~mech_type ~out () ->
    let cred = cred_retrieve cred in
    let error code =
      out
	~name:no_name_out ~initiator_lifetime:`Indefinite 
	~acceptor_lifetime:`Indefinite ~cred_usage:`Initiate
	~minor_status:0l ~major_status:(`None,code,[]) () in
    if mech_type <> scram_mech then
      error `Bad_mech
	(* CHECK: not documented in RFC 2744 for this function *)
    else
      let eff_cred_opt =
	if cred = no_cred then
	  try Some(self # get_default_client_cred())
	  with Not_found -> None
	else
	  Some cred in
      match eff_cred_opt with
	| Some eff_cred ->
	    out
	      ~name:eff_cred#name
	      ~initiator_lifetime:`Indefinite
	      ~acceptor_lifetime:`Indefinite
	      ~cred_usage:( match eff_cred#cred with
			      | Cred_server -> `Accept
			      | Cred_client _ -> `Initiate
			      | _ -> assert false
			  )
	      ~minor_status:0l
	      ~major_status:(`None, `None, [])
	      ()
	| None ->
	    error `No_cred  (* No default initiator credentials *)
	
  method inquire_mechs_for_name :
          't . name:name ->
               out:( mech_types:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~name ~out () ->
    let name = name_retrieve name in
    let l =
      if name#name_type = nt_hostbased_service ||
	name#name_type = nt_user_name
      then
	[ scram_mech ]
      else
	[] in
    out
      ~mech_types:l ~minor_status:0l ~major_status:(`None,`None,[]) ()

  method inquire_names_for_mech :
          't . mechanism:oid ->
               out:( name_types:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~mechanism ~out () ->
    let l =
      if mechanism = scram_mech then 
	[ nt_hostbased_service; nt_user_name ]
      else
	[] in
    out 
      ~name_types:l
      ~minor_status:0l
      ~major_status:(`None, `None, [])
      ()

  method process_context_token :
          't . context:context ->
               token:token ->
               out:( minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~context ~token ~out () ->
    (* There are no context tokens... *)
    let _context = context_retrieve context in
    out ~minor_status:0l ~major_status:(`None,`Defective_token,[]) ()

  method unwrap :
          't . context:context ->
               input_message:message ->
               output_message_preferred_type:[ `String | `Memory ] ->
               out:( output_message:message ->
		     conf_state:bool ->
		     qop_state:qop ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun  ~context ~input_message ~output_message_preferred_type ~out
      () ->
    let context = context_retrieve context in
    let sk_opt = context # specific_keys in
    let error code =
      out
	~output_message:[] ~conf_state:false ~qop_state:default_qop
	~minor_status:0l ~major_status:(`None,code,[]) () in
    if not context#valid then
      error `No_context
    else
      match sk_opt with
	| None ->
	    error `No_context
	| Some (k_mic_c,k_mic_s,k_wrap_c,k_wrap_s) ->
	    let sk_wrap =
	      if context#is_acceptor then k_wrap_c else k_wrap_s in
	    ( try
		let (sent_by_acceptor, _, _, tok_seq_nr) =
		  Netgssapi.parse_wrap_token_header input_message in
		if sent_by_acceptor = context#is_acceptor then
		  raise Netmech_scram.Cryptosystem.Integrity_error;
		let flags = context#is_peer_seq_nr_ok tok_seq_nr in
		let s =
		  Netgssapi.unwrap_wrap_token_conf
		    ~decrypt_and_verify:(
		      Netmech_scram.Cryptosystem.decrypt_and_verify_mstrings
			sk_wrap)
		    ~token:input_message in
		out
		  ~output_message:s
		  ~conf_state:true
		  ~qop_state:default_qop
		  ~minor_status:0l ~major_status:(`None,`None,flags) ()
	      with
		| Netmech_scram.Cryptosystem.Integrity_error ->
		    error `Bad_mic
		| _ -> (* probable Invalid_argument *)
		    error `Defective_token
	    )


  method verify_mic :
          't . context:context ->
               message:message ->
               token:token ->
               out:( qop_state:qop ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~context ~message ~token ~out () ->
    let context = context_retrieve context in
    let sk_opt = context # specific_keys in
    if not context#valid then
      out
	~qop_state:default_qop ~minor_status:0l
	~major_status:(`None,`No_context,[]) ()
    else
      match sk_opt with
	| None ->
	    out
	      ~qop_state:default_qop ~minor_status:0l
	      ~major_status:(`None,`No_context,[]) ()
	| Some (k_mic_c,k_mic_s,k_wrap_c,k_wrap_s) ->
	    let sk_mic =
	      if context#is_acceptor then k_mic_c else k_mic_s in
	    let (sent_by_acceptor,_,tok_seq_nr) =
	      Netgssapi.parse_mic_token_header token in
	    let flags =
	      context#is_peer_seq_nr_ok tok_seq_nr in
	    let ok =
	      sent_by_acceptor <> context#is_acceptor &&
		(Netgssapi.verify_mic_token
		   ~get_mic:(Netmech_scram.Cryptosystem.get_mic_mstrings sk_mic)
		   ~message
		   ~token) in
	    if ok then
	      out
		~qop_state:default_qop ~minor_status:0l
		~major_status:(`None,`None,flags) ()
	    else
	      out
		~qop_state:default_qop ~minor_status:0l
		~major_status:(`None,`Bad_mic,[]) ()
      
  method wrap :
          't . context:context ->
               conf_req:bool ->
               qop_req:qop option ->
               input_message:message ->
               output_message_preferred_type:[ `String | `Memory ] ->
               out:( conf_state:bool ->
		     output_message:message ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun
          ~context ~conf_req ~qop_req ~input_message 
	  ~output_message_preferred_type ~out () ->
    let context = context_retrieve context in
    if not context#valid then
      out
	~conf_state:false ~output_message:[] ~minor_status:0l
	~major_status:(`None,`No_context,[]) ()
    else
      let sk_opt = context # specific_keys in
      (* Reject any QOP: *)
      if qop_req <> None && qop_req <> Some default_qop then
	out
	  ~conf_state:false ~output_message:[] ~minor_status:0l
	  ~major_status:(`None,`Bad_QOP,[]) ()
      else (
	match sk_opt with
	  | None ->
	      out
		~conf_state:false ~output_message:[] ~minor_status:0l
		~major_status:(`None,`No_context,[]) ()
	  | Some (k_mic_c,k_mic_s,k_wrap_c,k_wrap_s) ->
	      let sk_wrap =
		if context#is_acceptor then k_wrap_s else k_wrap_c in
	      let token =
		Netgssapi.create_wrap_token_conf
		  ~sent_by_acceptor:context#is_acceptor
		  ~acceptor_subkey:false
		  ~sequence_number:context#seq_nr
		  ~get_ec:(
		    Netmech_scram.Cryptosystem.get_ec sk_wrap)
		  ~encrypt_and_sign:(
		    Netmech_scram.Cryptosystem.encrypt_and_sign_mstrings
		      sk_wrap)
		  ~message:input_message in
	      out
		~conf_state:true 
		~output_message:token
		~minor_status:0l
		~major_status:(`None,`None,[])
		()
      )
	

  method wrap_size_limit :
          't . context:context ->
               conf_req:bool ->
               qop_req:qop option ->
               req_output_size:int ->
               out:( max_input_size:int ->
                     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't =
    fun ~context ~conf_req ~qop_req ~req_output_size ~out () ->
    let _context = context_retrieve context in

    (* We have:
       - 12 bytes for the MIC
       - the message is padded to a multiple of 16 bytes
       - the message includes a 16 bytes random header
     *)
    let p_size = (req_output_size - 12) / 16 * 16 in
    let m_size = max 0 (p_size - 16) in
    out 
      ~max_input_size:m_size ~minor_status:0l ~major_status:(`None,`None,[])
      ()
    
end
