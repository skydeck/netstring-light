(* $Id: http_client.ml 1740 2012-02-22 22:31:47Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* TODO:
   - Also support automatic compression of uploads. Be prepared to get
     a 415 response, and fall back to [identity] (or whatever is available)
     Plan:
     * message: set_content_encoding
     * message, private-api: allow automatic compression in open_value_rd
     * Uq_io: implement filter_in_buffer
     * postprocess: intercept 415 responses, and choose a different
       encoding
 *)


(* Reference documents:
 * RFC 2068, 2616:      HTTP 1.1
 * RFC 2069, 2617:      Digest Authentication
 *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Http_client" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Http_client" Debug.enable

let () =
  Netlog.Debug.register_module "Http_client" Debug.enable


open Http_client_conncache
open Printf
open Uq_engines.Operators

exception Bad_message of string;;
exception Http_error of (int * string);;
exception Http_protocol of exn;;
exception Proxy_error of int
exception No_reply;;
exception Too_many_redirections;;
exception Name_resolution_error = Uq_resolver.Host_not_found
exception URL_syntax_error of string
exception Timeout of string
exception Response_too_large

let () =
  Netexn.register_printer
    (Http_protocol Not_found)
    (fun e ->
       match e with
	 | Http_protocol e' ->
	     "Http_client.Http_protocol(" ^ Netexn.to_string e' ^ ")"
	 | _ ->
	     assert false
    )

let() =
  Netsys_signal.init()


type status =
  [ `Unserved
  | `Http_protocol_error of exn
  | `Successful
  | `Redirection
  | `Client_error
  | `Server_error
  ]

type response_body_storage =
    [ `Memory
    | `File of unit -> string
    | `Body of unit -> Netmime.mime_body
    | `Device of unit -> Uq_io.out_device
    ]

type 'message_class how_to_reconnect =
    Send_again
  | Request_fails
  | Inquire of ('message_class -> bool)
  | Send_again_if_idem
;;

type 'message_class how_to_redirect =
    Redirect
  | Do_not_redirect
  | Redirect_inquire of ('message_class -> bool)
  | Redirect_if_idem
;;

type resolver =
    Unixqueue.unix_event_system -> 
    string -> 
    (Unix.inet_addr option -> unit) -> 
      unit
;;

type counters =
    { mutable new_connections : int;
      mutable timed_out_connections : int;
      mutable crashed_connections: int;
      mutable server_eof_connections : int;
      mutable successful_connections : int;
      mutable failed_connections : int;
    }


let better_unix_error f arg =
  try
    f arg
  with
    Unix.Unix_error (e,syscall,param) ->
      let error = Unix.error_message e in
      if param = "" then
	failwith error
      else
	failwith (param ^ ": " ^ error)


let rec syscall f =
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


let hex_digits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
		    '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |];;

let encode_hex s =
  (* encode with lowercase hex digits *)
  let l = String.length s in
  let t = String.make (2*l) ' ' in
  for n = 0 to l - 1 do
    let x = Char.code s.[n] in
    t.[2*n]   <- hex_digits.( x lsr 4 );
    t.[2*n+1] <- hex_digits.( x land 15 );
  done;
  t
;;


type synchronization =
  | Sync
  | Pipeline of int
;;


let max_pipeline = 8 ;;

let pipeline_blacklist =
  (* Stolen from Mozilla *)
  [ Netstring_str.regexp "Microsoft-IIS/";
    Netstring_str.regexp "Netscape-Enterprise/3.";
  ]
;;


type channel_binding_id = int

type http_options = 
    { synchronization : synchronization;
      maximum_connection_failures : int;
      maximum_message_errors : int;
      inhibit_persistency : bool;
      connection_timeout : float;
      number_of_parallel_connections : int;
      maximum_redirections : int;
      handshake_timeout : float;
      resolver : resolver;
      configure_socket : Unix.file_descr -> unit;
      schemes : (string * Neturl.url_syntax * int option * channel_binding_id)
	         list;
      verbose_status : bool;
      verbose_request_header : bool;
      verbose_response_header : bool;
      verbose_request_contents : bool;
      verbose_response_contents : bool;
      verbose_connection : bool;
      verbose_events : bool;
    }
;;

type header_kind = [ `Base | `Effective ]

(*
let http_re =
  Netstring_str.regexp
    "^https?://\
     \\(\
       \\([^/:@]+\\)\
       \\(:\\([^/:@]+\\)\\)?@\
     \\)?\
     \\([^/:@]+\\)\
     \\(:\\([0-9]+\\)\\)?\
     \\([/?].*\\)?$"
 *)

let parse_url_0 ?base_url schemes url =
  try
    let sch = 
      try Neturl.extract_url_scheme url 
      with err ->
	( match base_url with
	    | None -> raise err
	    | Some b -> Neturl.url_scheme b
	) in
    let (_,syn,p_opt,cb) = 
      List.find (fun (sch',_,_,_) -> sch=sch') schemes in
    let ht = Hashtbl.create 1 in
    Hashtbl.add ht sch syn;
    let url' = Neturl.fixup_url_string ~escape_hash:true url in
    let nu1 = 
      Neturl.parse_url 
	?base_syntax:(match base_url with
			| None -> None
			| Some b -> Some(Neturl.url_syntax_of_url b)
		     )
	~schemes:ht ~accept_8bits:true url' in
    let nu2 = 
      Neturl.ensure_absolute_url ?base:base_url nu1 in
    (Neturl.default_url ?port:p_opt nu2, cb)
  with
    | Neturl.Malformed_URL -> raise Not_found

let parse_url ?base_url options url =
  parse_url_0 ?base_url !options.schemes url

(*
  match Netstring_str.string_match http_re url 0 with
    | None ->
	raise Not_found
    | Some m ->
	let is_https =
	  String.sub url 0 5 = "https" in
	let user =
	  try Some(Netstring_str.matched_group m 2 url) 
	  with Not_found -> None in
	let password =
	  try Some(Netstring_str.matched_group m 4 url)
	  with Not_found -> None in
	let host =
	  Netstring_str.matched_group m 5 url in
	let port =
	  try int_of_string(Netstring_str.matched_group m 7 url)
	  with Not_found -> (if is_https then 443 else 80) in
	let path =
	  try Netstring_str.matched_group m 8 url with Not_found -> "" in
	(is_https,user,password,host,port,path)
 *)


let comma_re = Netstring_str.regexp "[ \t\n\r]*,[ \t\n\r]*" ;;

let split_words_by_commas s =
  Netstring_str.split comma_re s ;;

let space_re = Netstring_str.regexp "[ \t\n\r]+" ;;

let split_words s =
  Netstring_str.split space_re s ;;


let sync_resolver esys name reply =
  (* FIXME: Use Uq_resolver also in the async case! *)
  let addr =
    try
      let h = Uq_resolver.get_host_by_name name in
      Some h.Unix.h_addr_list.(0)
    with Uq_resolver.Host_not_found _ -> None in
  reply addr
;;


(**********************************************************************)
(***                          BUFFERS                               ***)
(**********************************************************************)

(* Similar to Buffer, but may be accessed in a more efficient way. *)

module B = Netbuffer;;

type buffer_type = B.t;;


module Q = 
struct
  (* This queue allows one to insert elements. *)

  type 'a t = 'a Queue.t

  exception Empty = Queue.Empty

  let create = Queue.create

  let add = Queue.add

  let push = add 

  let add_at n x q =
    (* Add x behind the n-the last element of q, i.e.
     * add_at 0 x q = Add behind the last element = add x q
     * add_at 1 x q = Add before the last element
     * ...
     * This algorithm is usually called for n ~ length, and is quite
     * efficient in this case.
     *)
    let l = Queue.length q in
    let q' = Queue.create() in
    for k = 1 to l-n do
      Queue.add (Queue.take q) q'
    done;
    Queue.add x q';
    Queue.transfer q q';
    Queue.transfer q' q

  let take = Queue.take
     
  let pop = take

  let peek = Queue.peek

  let top = peek

  let clear = Queue.clear

  let copy = Queue.copy

  let is_empty = Queue.is_empty

  let length = Queue.length

  let iter = Queue.iter

  let transfer = Queue.transfer

  (* fold: omitted *)

end



(**********************************************************************)
(***                  THE HTTP CALL CONTAINER                       ***)
(**********************************************************************)


let dump_header prefix h =
  List.iter
    (fun (n,v) ->
       dlog (prefix ^ n ^ ": " ^ v))
    h



type 'session auth_state =  (* 'session = auth_session, defined below *)
    [ `None
         (* Authentication has not yet been tried *)
    | `In_advance of 'session
         (* This session had been tried before a 401 response was seen *)
    | `In_reply of 'session
         (* This session was tried after a 401 response was seen *)
    ]

class type http_call =
object
  method is_served : bool
  method status : status
  method request_method : string
  method request_uri : string
  method set_request_uri : string -> unit
(* method is_https : bool *)
  method request_header : header_kind -> Netmime.mime_header
  method set_request_header : Netmime.mime_header -> unit
  method set_expect_handshake : unit -> unit
  method set_chunked_request : unit -> unit
  method effective_request_uri : string
  method request_body : Netmime.mime_body
  method set_request_body : Netmime.mime_body -> unit
  method set_request_device : (unit -> Uq_io.in_device) -> unit
  method set_accept_encoding : unit -> unit
  method response_status_code : int
  method response_status_text : string
  method response_status : Nethttp.http_status
  method response_protocol : string
  method response_header : Netmime.mime_header
  method response_body : Netmime.mime_body
  method response_body_storage : response_body_storage
  method set_response_body_storage : response_body_storage -> unit
  method max_response_body_length : int64
  method set_max_response_body_length : int64 -> unit
  method get_reconnect_mode : http_call how_to_reconnect
  method set_reconnect_mode : http_call how_to_reconnect -> unit
  method get_redirect_mode : http_call how_to_redirect
  method set_redirect_mode : http_call how_to_redirect -> unit
  method proxy_enabled : bool
  method set_proxy_enabled : bool -> unit
  method no_proxy : unit -> unit
  method is_proxy_allowed : unit -> bool
  method proxy_use_connect : bool
  method empty_path_replacement : string
  method is_idempotent : bool
  method has_req_body : bool
  method has_resp_body : bool
  method set_channel_binding : channel_binding_id -> unit
  method same_call : unit -> http_call
  method get_req_method : unit -> string
  method get_host : unit -> string
  method get_port : unit -> int
  method get_path : unit -> string
  method get_uri : unit -> string
  method get_req_body : unit -> string
  method get_req_header : unit -> (string * string) list
  method assoc_req_header : string -> string
  method assoc_multi_req_header : string -> string list
  method set_req_header : string -> string -> unit
  method get_resp_header : unit -> (string * string) list
  method assoc_resp_header : string -> string
  method assoc_multi_resp_header : string -> string list
  method get_resp_body : unit -> string
  method dest_status : unit -> (string * int * string)
  method private_api : private_api
end

and private_api =
object
  method parse_request_uri : http_options ref -> unit
  method request_uri_with : ?path:string option -> ?remove_particles:bool ->
                            unit -> Neturl.url

  method channel_binding : http_options ref -> channel_binding_id

  method get_error_counter : int
  method set_error_counter : int -> unit
  method set_error_exception : exn -> unit
  method error_exception : exn option

  method error_if_unserved : bool -> exn -> unit

  method get_redir_counter : int
  method set_redir_counter : int -> unit

  method continue : bool
    (* The value of the continue flag *)
  method set_continue : bool -> unit
    (* Set the continue flag to [true]. The argument says whether a
       "100" code was received or not. (In the latter case the reaction is
       different.)
     *)
  method wait_continue_e : float -> Unixqueue.event_system -> 
                                                         bool Uq_engines.engine
    (* Waits until [set_continue] is called, or until the time is over.
       The bool result says whether a "100" code was received or not.
     *)

  method auth_state : auth_session auth_state
  method set_auth_state : auth_session auth_state -> unit

  method retry_anyway : bool
  method set_retry_anyway : bool -> unit
    (* This flag is set when we retry the request after an authentication
       error. It overrides the reconnect mode - even if we do not want to
       retry the request normally, we do it now exceptionally.
       This is especially important for non-idempotent requests.

       The flag remains active until we get to the point that we can
       write the request line.
     *)

  method set_effective_request_uri : string -> unit

  method prepare_transmission : unit -> unit
    (* Initializes the `Effective request header, and creates the response
     * objects. Sets the status to [`Unserved]. The error and redirection
     * counters are not changed.
     *)
  method set_response_status : int -> string -> string -> unit
    (* code, text, proto *)
  method set_response_header : Netmime.mime_header -> unit
    (* Sets the response header *)
  method response_body_open_wr : Unixqueue.event_system -> Uq_io.out_device
    (* Opens the response body for writing *)
  method request_body_open_rd : Unixqueue.event_system -> Uq_io.in_device
    (* Opens the request body for reading *)
  method finish_request_e : Unixqueue.event_system -> unit Uq_engines.engine
    (* Finish the request part of the call *)
  method finish_response_e : Unixqueue.event_system -> unit Uq_engines.engine
    (* The call is finished. The [status] is set to [`Successful], 
     * [`Redirection], [`Client_error], or [`Server_error] depending on
     * the response.
     *)
  method cleanup : unit -> unit
    (* Release resources *)

  method response_code : int
  method response_proto : string
  method response_header : Netmime.mime_header

  method decompression_enabled : bool

  method dump_status : unit -> unit
  method dump_response_header : unit -> unit
  method dump_response_body : unit -> unit
end

and auth_session =
object
  method auth_scheme : string
  method auth_domain : Neturl.url list
  method auth_realm : string
  method auth_user : string
  method auth_in_advance : bool
  method authenticate : http_call -> (string * string) list
  method invalidate : http_call -> bool
end


class type virtual gen_call =
object
  inherit http_call
  method private virtual fixup_request : unit -> unit
  method private virtual def_request_method : string
  method private virtual def_empty_path_replacement : string
  method private virtual def_is_idempotent : bool
  method private virtual def_has_req_body : bool
  method private virtual def_has_resp_body : bool
end


let new_cb_id () =
  Oo.id (object end)

let http_cb_id = new_cb_id()
let https_cb_id = new_cb_id()
let proxy_only_cb_id = new_cb_id()


class virtual generic_call : gen_call =
object(self)
  val mutable status = (`Unserved : status)
  val mutable finished = false

  val mutable req_uri = None
      (* The req_uri must always include the port number *)
  val mutable req_uri_raw = ""

  val mutable req_cb = http_cb_id
  val mutable req_cb_set = false
  val mutable req_secure = false
  val mutable req_host = ""   (* derived from req_uri *)
  val mutable req_port = 80   (* derived from req_uri *)
  val mutable req_path = ""   (* derived from req_uri *)
  val mutable req_base_header = new Netmime.basic_mime_header []
  val mutable req_work_header = new Netmime.basic_mime_header []
  val mutable req_body = new Netmime.memory_mime_body ""
  val mutable req_dev = None

  val mutable eff_req_uri = ""

  val mutable resp_code = 0
  val mutable resp_text = ""
  val mutable resp_proto = ""
  val mutable resp_header = new Netmime.basic_mime_header []
  val mutable resp_header_set = false
  val mutable resp_body = None
  val mutable resp_body_max = Int64.max_int
  val mutable resp_decompress = false

  val mutable resp_body_storage = (`Memory : response_body_storage)
  val mutable reconn_mode = Send_again_if_idem
  val mutable redir_mode = Redirect_if_idem
  val mutable proxy_enabled = true
  val mutable retry_anyway = false

  val mutable private_api = None

  val mutable resp_handle = None
  val mutable req_handle = None

  val mutable continue = false   (* Whether 100-Continue has been seen *)    
  val mutable continue_e = None
  val mutable continue_aborted = false
  val mutable continue_100 = false

  val mutable error_counter = 0
  val mutable redir_counter = 0
  val mutable auth_state = `None


  method private resp_body =
    match resp_body with
      | None ->
	  let rbody =
	    match resp_body_storage with
	      | `Memory ->
		  new Netmime.memory_mime_body ""
	      | `File f ->
		  let name = f() in
		  new Netmime.file_mime_body name
	      | `Body f ->
		  f () 
	      | `Device _ ->
		  failwith "Http_client: response is forwarded to device - \
                            no accessible response_body" in
	  resp_body <- Some rbody;
	  rbody
      | Some rbody -> rbody 


  method private def_private_api fixup_request =
    ( object(pself) 
	method private release_resources() =
(* eprintf "release_resources call=%d\n%!" (Oo.id self); *)
	  ( match req_handle with
	      | None -> ()
	      | Some d ->
		  Uq_io.inactivate d; req_handle <- None
	  );
	  ( match resp_handle with
	      | None -> ()
	      | Some d ->
		  Uq_io.inactivate d; resp_handle <- None
	  );
	  ( match continue_e with
	      | None -> ()
	      | Some e -> 
		  continue_e <- None;
		  continue_aborted <- true;
		  e#abort()
		  
	  )

	method get_error_counter = error_counter
	method set_error_counter n = error_counter <- n
	method get_redir_counter = redir_counter
	method set_redir_counter n = redir_counter <- n

	method continue = continue
	method set_continue code_100 = 
	  continue <- true;
	  continue_100 <- code_100;
	  match continue_e with
	    | None -> ()
	    | Some e -> 
		continue_e <- None;
		e#abort()

	method wait_continue_e tmo esys =
	  if continue then
	    eps_e (`Done true) esys
	  else (
	    assert(continue_e = None); (* cannot be called multiple times *)
	    let e = 
	      Uq_engines.delay_engine tmo
		(fun () -> eps_e (`Done ()) esys)
		esys in
	    continue_e <- Some e;
	    ( e
	      >> (fun _ -> 
		    continue_e <- None; 
		    if continue_aborted then `Aborted else  `Done continue_100
		 )
	    )
	  )

	method auth_state = auth_state
	method set_auth_state s = auth_state <- s

	method retry_anyway = retry_anyway
	method set_retry_anyway flag = retry_anyway <- flag;

	method set_effective_request_uri s = eff_req_uri <- s

	method parse_request_uri options =
	  if req_uri = None then (
	    try
	      let (nu, cb) = parse_url options req_uri_raw in
	      if (Neturl.url_provides ~user:true nu || 
		    Neturl.url_provides ~password:true nu)
	      then
		failwith "Http_client: URL must not contain user or password";
	      req_uri <- Some nu;
	      req_host <- Neturl.url_host nu;
	      req_port <- Neturl.url_port nu;
	      (* req_path must also contain the parts after the path, i.e.
	         param and query
	       *)
	      req_path <- 
                Neturl.join_path(Neturl.url_path ~encoded:true nu) ^ 
                ( try String.concat "" 
                        (List.map
                          (fun s -> ";" ^ s)
                          (Neturl.url_param ~encoded:true nu))
                  with Not_found -> "") ^
                ( try "?" ^ Neturl.url_query ~encoded:true nu 
                  with Not_found -> "");
	      if not req_cb_set then
		req_cb <- cb;
	    with
		Not_found ->
		  failwith "Http_client: bad URL"
	  )

	method channel_binding options =
	  pself # parse_request_uri options;
	  req_cb

	method request_uri_with ?path ?(remove_particles=false) () =
	  match req_uri with
	    | None ->
		assert false
	    | Some ru ->
		let nu = 
		  Neturl.modify_url
		    ?path:(match path with
			     | None | Some None -> None
			     | Some (Some p) -> Some(Neturl.split_path p)
			  )
		    (Neturl.remove_from_url
		       ~path:(path = Some None)
		       ~query:remove_particles
		       ~param:remove_particles
		       ~fragment:remove_particles
		       ru) in
		nu


	method decompression_enabled = resp_decompress


	method prepare_transmission () =
	  pself # release_resources();
	  status <- `Unserved;
	  req_work_header <- new Netmime.basic_mime_header 
	                             req_base_header#fields;
	  resp_code <- 0;
	  resp_text <- "";
	  resp_proto <- "";
	  resp_header <- new Netmime.basic_mime_header [];
	  resp_header_set <- false;
	  resp_body <- None;
	  fixup_request();
	  ( try ignore(req_work_header # field "Date")
	    with Not_found ->
	      Nethttp.Header.set_date req_work_header (Unix.time())
	  );
	  ( try ignore(req_work_header # field "User-agent")
	    with Not_found ->
	      req_work_header # update_field "User-agent" "Netclient"
	  );
	  continue <- false;
	  continue_e <- None;
	  continue_aborted <- false;
	  continue_100 <- false;

	method request_body_open_rd esys =
	  match req_dev with
	    | Some f ->
		let d = f() in
		req_handle <- Some d;
		d
	    | _ ->
		let ch = req_body # open_value_rd() in
		let d = 
		  `Async_in(new Uq_engines.pseudo_async_in_channel ch,esys) in
		req_handle <- Some d;
		d


	method set_response_status code text proto =
	  resp_code <- code;
	  resp_text <- text;
	  resp_proto <- proto

	method set_response_header h =
	  resp_header <- h;
	  resp_header_set <- true;
	  assert(resp_code <> 0);

	method response_body_open_wr esys =
	  let d0 =
	    match resp_body_storage with
	      | `Device f ->
		  f()
	      | _ ->
		  let rbody = self#resp_body in
		  let ch = rbody # open_value_wr() in
		  `Async_out(new Uq_engines.pseudo_async_out_channel ch,esys) in
	  (* Check for maximum response length: *)
	  let c = ref 0L in
	  let c_max = self # max_response_body_length in
	  let out_count n =
	    c := Int64.add !c (Int64.of_int n);
	    if !c > c_max then raise Response_too_large in
	  let d1 = `Count_out(out_count, d0) in
	  (* Check for decompression: *)
	  let d2 =
	    if pself#decompression_enabled then
	      let algos = 
		try Nethttp.Header.get_content_encoding resp_header
		with Not_found -> [] in
	      match algos with
		| [ algo ] ->
		    ( try 
			let decoder = 
			  Netcompression.lookup_decoder ~iana_name:algo () in
			resp_header # delete_field "Content-Encoding";
			let b = 
			  Uq_io.filter_out_buffer
			    ~max:(Some 4096) decoder d1 in
			`Buffer_out b
		      with
			| Not_found -> d1
		    )
		| _ -> 
		    d1
	    else
	      d1 in
	  resp_handle <- Some d2;
	  d2

	method response_code = resp_code
	method response_proto = resp_proto
	method response_header = resp_header


	method finish_request_e esys =
(* eprintf "finish_request call=%d\n%!" (Oo.id self); *)
	  match req_handle with
	    | None ->
		eps_e (`Done ()) esys
	    | Some d ->
		req_handle <- None;
		Uq_io.shutdown_e d
		>> (fun st -> Uq_io.inactivate d; st)

	method finish_response_e esys =
(* eprintf "finish_response call=%d\n%!" (Oo.id self); *)
	  assert(resp_code <> 0);
	  finished <- true;
	  status <- (if resp_code >= 200 && resp_code <= 299 then
		       `Successful
		     else if resp_code >= 300 && resp_code <= 399 then
		       `Redirection
		     else if resp_code >= 400 && resp_code <= 499 then
		       `Client_error
		     else
		       `Server_error);
	  (* do this last - it can trigger user callback functions: *)
	  match resp_handle with
	    | None ->
		eps_e (`Done ()) esys
	    | Some d ->
(* prerr_endline "Http_cllent SHUTDOWN"; *)
		resp_handle <- None;
		Uq_io.shutdown_e d
		>> (fun st -> Uq_io.inactivate d; st)

	method error_if_unserved verbose error =
	  match status with
	    | `Unserved ->
		if verbose then
		  dlogr (fun () -> 
			   sprintf "Call %d - error %s"
			     (Oo.id self)
			     (Netexn.to_string error));
		pself # set_error_exception error;
	    | _ ->
		()

	method set_error_exception x =
	  finished <- true;
	  ( match x with
		Http_error(_,_) -> assert false   (* not allowed *)
	      | _ ->
		  status <- (`Http_protocol_error x);
	  );
	  (* do this last - it can trigger user callback functions: *)
	  pself # release_resources();

	method error_exception =
	  match status with
	    | `Http_protocol_error x -> Some x
	    | _ -> None

	method cleanup () =
	  pself # release_resources()

	method dump_status () =
	  dlog (sprintf
		  "Call %d - HTTP response code: %d (%s)"
		  (Oo.id self) resp_code resp_text);
	  dlog (sprintf
		  "Call %d - HTTP response protocol: %s"
		  (Oo.id self) resp_proto);

	method dump_response_header () =
	  dump_header 
	    (sprintf 
	       "Call %d - HTTP response "
	       (Oo.id self))
	    (resp_header # fields)

	method dump_response_body () =
	  dlog (sprintf "Call %d - HTTP response body:\n%s\n"
		  (Oo.id self) 
		  (match resp_body with
		     | Some rbody -> rbody#value
		     | None -> ""
		  ))
      end
    )


  (* Call state *)

  method is_served = finished
  method status = status

  (* Accessing the request message (new style) *)

  method request_method = self # def_request_method
  method request_uri = req_uri_raw
  method set_request_uri uri = req_uri_raw <- uri; req_uri <- None

  method set_channel_binding cb = 
    req_cb <- cb;
    req_cb_set <- true


  method request_header (k:header_kind) =
    match k with
      | `Base -> req_base_header
      | `Effective -> req_work_header
  method set_request_header h =
    req_base_header <- h
  method set_expect_handshake() =
    req_base_header # update_field "Expect" "100-continue"
  method set_chunked_request() =
    req_base_header # update_field "Transfer-encoding" "chunked"
  method request_body = 
    if req_dev <> None then
      failwith "Http_client: No request_body - using a device instead";
    req_body
  method set_request_body b = req_body <- b; req_dev <- None
  method set_request_device f = req_dev <- Some f

  method effective_request_uri = eff_req_uri

  method set_accept_encoding() =
    let all_algos =
      Netcompression.all_decoders() in
    Nethttp.Header.set_accept_encoding
      req_base_header
      (if all_algos = [] then ["identity",[]] else
	 (List.map (fun token -> (token,[])) all_algos));
    resp_decompress <- true

  (* Accessing the response message (new style) *)

  method private check_response() =
    match status with
      | `Unserved ->
	  if not resp_header_set then
	    failwith "Http_client: HTTP call is unserved, no response yet"
      | `Http_protocol_error e -> 
	  raise (Http_protocol e)
      | _ -> ()

  method response_status_code = self#check_response(); resp_code
  method response_status_text = self#check_response(); resp_text
  method response_status = 
    self#check_response(); Nethttp.http_status_of_int resp_code
  method response_protocol = self#check_response(); resp_proto
  method response_header = self#check_response(); resp_header
  method response_body = 
    (* [status] is already set after the header is available. The presence
       of the body is indicated by [finished].
     *)
    self#check_response(); 
    if not finished then 
      failwith "Http_client: HTTP call is unserved, response not yet complete";
    self#resp_body

  (* Options *)

  method response_body_storage = resp_body_storage
  method set_response_body_storage s = resp_body_storage <- s
  method max_response_body_length = resp_body_max
  method set_max_response_body_length n = resp_body_max <- n
  method get_reconnect_mode = reconn_mode
  method set_reconnect_mode m = reconn_mode <- m
  method get_redirect_mode = redir_mode
  method set_redirect_mode m = redir_mode <- m
  method proxy_enabled = proxy_enabled
  method set_proxy_enabled e = proxy_enabled <- e
  method no_proxy() = proxy_enabled <- false
  method is_proxy_allowed() = proxy_enabled
  method proxy_use_connect = (req_cb = https_cb_id)

  (* Method characteristics *)

  method empty_path_replacement = self # def_empty_path_replacement
  method is_idempotent = self # def_is_idempotent
  method has_req_body = self # def_has_req_body
  method has_resp_body = self # def_has_resp_body

  (* Repeating calls *)
    
  method same_call() =
    let same =
      {< status = `Unserved;
	 finished = false;
	 resp_header = new Netmime.basic_mime_header [];
	 resp_header_set = false;
	 resp_body = None;
	 eff_req_uri = "";
	 private_api = None;
	 error_counter = 0;
	 redir_counter = 0;
	 continue = false;
	 continue_aborted = false;
	 continue_100 = false;
	 continue_e = None;
	 resp_handle = None;
	 req_handle = None;
	 auth_state = `None;
	 retry_anyway = false
      >} in
    (same : #http_call :> http_call)

  (* Old style access methods *)

  method get_req_method() = self # def_request_method
  method get_host() = req_host
  method get_port() = req_port
  method get_path() = req_path
  method get_uri() = req_uri_raw
  method get_req_body() = req_body # value
  method get_req_header () =
    List.map (fun (n,v) -> (String.lowercase n, v)) req_base_header#fields
  method assoc_req_header n =
    req_base_header # field n
  method assoc_multi_req_header n =
    req_base_header # multiple_field n
  method set_req_header n v =
    req_base_header # update_field n v
  method get_resp_header() =
    self#check_response(); 
    List.map (fun (n,v) -> (String.lowercase n, v)) resp_header#fields
  method assoc_resp_header n =
    self#check_response(); 
    resp_header # field n
  method assoc_multi_resp_header n =
    self#check_response(); 
    resp_header # multiple_field n
  method get_resp_body() =
    self#check_response();
    if resp_code >= 200 && resp_code <= 299 then
      self # resp_body # value
    else
      raise(Http_error(resp_code, self#resp_body#value))
  method dest_status() =
    self#check_response();
    (resp_proto, resp_code, resp_text)

  (* Private *)

  method private_api = 
    match private_api with
      | None ->
	  let api = self # def_private_api self#fixup_request in
	  private_api <- Some api;
	  api
      | Some api ->
	  api

  (* Virtual methods *)

  method virtual private fixup_request : unit -> unit
  method virtual private def_request_method : string
  method virtual private def_empty_path_replacement : string
  method virtual private def_is_idempotent : bool
  method virtual private def_has_req_body : bool
  method virtual private def_has_resp_body : bool
end

(******** SUBCLASSES IMPLEMENTING HTTP METHODS ************************)

class get_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "GET"
  method private def_is_idempotent = true
  method private def_has_req_body = false
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class head_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "HEAD"
  method private def_is_idempotent = true
  method private def_has_req_body = false
  method private def_has_resp_body = false
  method private def_empty_path_replacement = "/"
end

class trace_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "TRACE"
  method private def_is_idempotent = false
  method private def_has_req_body = false
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class options_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "OPTIONS"
  method private def_is_idempotent = false
  method private def_has_req_body = true
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "*"
end

class post_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "POST"
  method private def_is_idempotent = false
  method private def_has_req_body = true
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class put_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "PUT"
  method private def_is_idempotent = true (* sic! *)
  method private def_has_req_body = true
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class delete_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "DELETE"
  method private def_is_idempotent = true
  method private def_has_req_body = false
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class connect_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "CONNECT"
  method private def_is_idempotent = false
  method private def_has_req_body = false
  method private def_has_resp_body = true
    (* This is broken to some degree. See the comment below on when
       CONNECT has a response body
     *)
  method private def_empty_path_replacement = "/"
end


class get the_query =
  object (self)
    inherit get_call
    initializer
      self # set_request_uri the_query
  end


class trace the_query max_hops =
  object (self)
    inherit trace_call
    initializer
      self # set_request_uri the_query
    method private fixup_request() =
      (self # request_header `Effective) # update_field 
	                                "max-forwards" (string_of_int max_hops)
  end


class options the_query =
  object (self)
    inherit options_call
    initializer
      self # set_request_uri the_query
  end


class head the_query =
  object (self)
    inherit head_call
    initializer
      self # set_request_uri the_query
  end


class post query params =
  object (self)
    inherit post_call
    initializer
      self # set_request_uri query
    method private fixup_request() =
      let rh = self # request_header `Effective in
      let is_chunked =
	try rh # field "Transfer-encoding" <> "identity"
	with Not_found -> false in
      rh # update_field "Content-type" "application/x-www-form-urlencoded";
      let l = List.map (fun (n,v) -> n ^ "=" ^ Netencoding.Url.encode v) params in
      let s = String.concat "&" l in
      if not is_chunked then
	rh # update_field "Content-length" (string_of_int (String.length s));
      self # request_body # set_value s
  end
;;


class post_raw the_query s =
  object (self)
    inherit post_call
    initializer
      self # set_request_uri the_query
    method private fixup_request() =
      let rh = self # request_header `Effective in
      let is_chunked =
	try rh # field "Transfer-encoding" <> "identity"
	with Not_found -> false in
      self # request_body # set_value s;
      if not is_chunked then
	rh # update_field "Content-length" (string_of_int (String.length s));
  end
;;


class put the_query s =
  object (self)
    inherit put_call
    initializer
      self # set_request_uri the_query
    method private fixup_request() =
      let rh = self # request_header `Effective in
      let is_chunked =
	try rh # field "Transfer-encoding" <> "identity"
	with Not_found -> false in
      self # request_body # set_value s;
      if not is_chunked then
	rh # update_field "Content-length" (string_of_int (String.length s));
  end
;;


class delete the_query =
  object (self)
    inherit delete_call
    initializer
      self # set_request_uri the_query
  end
;;


class connect the_query (* host:port only *) =
  object (self)
    inherit connect_call
    initializer
      self # set_request_uri ("http://" ^ the_query)
    method effective_request_uri =
      the_query
  end
;;


(**********************************************************************)
(***                  AUTHENTICATION METHODS                        ***)
(**********************************************************************)

class type key =
object
  method user : string
  method password : string
  method realm : string
  method domain : string list
end


let key ~user ~password ~realm ~domain =
  ( object
      method user = user
      method password = password
      method realm = realm
      method domain = domain
    end
  )


class type key_handler =
object
  method inquire_key :
            domain:string list -> realms:string list -> auth:string -> key
  method invalidate_key : key -> unit
end


class key_ring ?uplink () =
object(self)
  val mutable keys = (Hashtbl.create 10 : 
			(string list * string, key * bool) Hashtbl.t)
    (* Maps (domain, realm) to (key, from_uplink) *)

  method inquire_key ~domain ~realms ~(auth:string) =
    let l =
      List.flatten
	(List.map
	   (fun realm ->
	      try
		[ Hashtbl.find keys (domain, realm) ]
	      with
		  Not_found -> [])
	   realms) in
    match l with
      | (key,_) :: _ ->
	  key
      | [] ->
	  ( match uplink with
	      | None -> raise Not_found
	      | Some h ->
		  let key = h # inquire_key ~domain ~realms ~auth in
		  (* or Not_found *)
		  Hashtbl.replace keys (key#domain, key#realm) (key,true);
		  key
	  )

  method invalidate_key (key : key) =
    let domain = key # domain in
    let realm = key # realm in
    try
      let (_, from_uplink) = Hashtbl.find keys (domain, realm) in
      Hashtbl.remove keys (domain, realm);
      if from_uplink then
	( match uplink with
	    | None -> assert false
	    | Some h -> h # invalidate_key key
	)
    with
	Not_found -> ()

  method clear () =
    Hashtbl.clear keys

  method add_key key =
    let domain = key # domain in
    let realm = key # realm in
    Hashtbl.replace keys (domain, realm) (key, false)

  method keys =
    Hashtbl.fold
      (fun _ (key,_) acc -> key :: acc)
      keys
      []

end


class proxy_key_handler user password : key_handler =
object
  method inquire_key ~domain ~realms ~auth =
    try
      let realm = List.hd realms in
      key ~domain ~realm ~user ~password
    with _ -> raise Not_found
  method invalidate_key _ = ()
  
end



class type auth_handler =
object
  method create_session : http_call -> http_options ref -> auth_session option
  method create_proxy_session : http_call -> http_options ref -> auth_session option
end


exception Not_applicable

let get_domain_uri (call : http_call) =
  call # private_api # request_uri_with
    ~path:(Some "/") ~remove_particles:true ()


class basic_auth_session enable_auth_in_advance 
                         key_handler init_call for_proxy
                         : auth_session =
  let domain_uri = if for_proxy then [] else [ get_domain_uri init_call ] in
  let domain = List.map Neturl.string_of_url domain_uri in
  let basic_realms =
    (* Return all "Basic" realms in www-authenticate, or raise Not_applicable *)
    let auth_list = 
      try
	if for_proxy then
	  Nethttp.Header.get_proxy_authenticate init_call#response_header
	else
	  Nethttp.Header.get_www_authenticate init_call#response_header
      with
	| Not_found -> raise Not_applicable
	| Nethttp.Bad_header_field _ -> raise Not_applicable in
    let basic_auth_list =
      List.filter 
	(fun (scheme,_) -> String.lowercase scheme = "basic") auth_list in
    let basic_auth_realm_list =
      List.flatten
	(List.map 
	   (fun (_,params) ->
	      try
		let (_,realm) =
		  List.find (fun (pname,_) -> 
			       String.lowercase pname = "realm") params in
		[realm]
	      with 
		  Not_found -> [])
	   basic_auth_list) in
    if basic_auth_realm_list = [] then
      raise Not_applicable
    else
      basic_auth_realm_list
  in
  let key =
    (* Return the selected key, or raise Not_applicable *)
    try
      key_handler # inquire_key ~domain ~realms:basic_realms ~auth:"basic"
    with
	Not_found -> raise Not_applicable
  in
  (* Check the key: *)
  let () =
    if not (List.mem key#realm basic_realms) then raise Not_applicable;
    if key#domain <> domain then raise Not_applicable;
  in
object(self)
  method auth_scheme = "basic"
  method auth_domain = domain_uri
  method auth_realm = key # realm
  method auth_user = key # user
  method auth_in_advance = enable_auth_in_advance
  method authenticate call =
    let basic_cookie = 
      Netencoding.Base64.encode 
	(key#user ^ ":" ^ key#password) in
    let cred = "Basic " ^ basic_cookie in
    let field_name = 
      if for_proxy then "Proxy-Authorization" else "Authorization" in
    [ field_name, cred ]
  method invalidate call =
    key_handler # invalidate_key key;
    false
end


class basic_auth_handler ?(enable_auth_in_advance=false) 
                         (key_handler : #key_handler)
                         : auth_handler =
object(self)
  method create_session call options =
    try
      Some(new basic_auth_session enable_auth_in_advance key_handler call false)
    with
	Not_applicable ->
	  None
  method create_proxy_session call options =
    try
      Some(new basic_auth_session enable_auth_in_advance key_handler call true)
    with
	Not_applicable ->
	  None
end



let contains_auth v =
  List.mem "auth" (split_words v)


class digest_auth_session enable_auth_in_advance options
                          key_handler (init_call : http_call) for_proxy
                          : auth_session =
  let normalize_domain s =
    try
      let (nu1,_) =
	parse_url
	  ~base_url:(init_call#private_api#request_uri_with())
	  options s in
      Neturl.remove_from_url
	~user:true ~user_param:true ~password:true ~fragment:true nu1
    with
      | Neturl.Malformed_URL -> raise Not_found

(*
    if s <> "" && s.[0] = '/' then
      init_call#private_api#request_uri_with
	~path:None ~remove_particles:true ()
      ^ s
    else
      ( try
	  let (is_https,_,_,host,port,path) = parse_http_url s in
	  let scheme = if is_https then "https" else "http" in
	  scheme ^ "://" ^ host ^ ":" ^ string_of_int port ^ path
	with
	  | Not_found -> s
      )
 *)
  in
  let digest_request =
    (* Return the "Digest" params in www-authenticate, or raise Not_applicable *)
    let auth_list = 
      try
	if for_proxy then
	  Nethttp.Header.get_proxy_authenticate init_call#response_header
	else
	  Nethttp.Header.get_www_authenticate init_call#response_header
      with
	| Not_found -> raise Not_applicable
	| Nethttp.Bad_header_field _ -> raise Not_applicable in
    let digest_auth_list =
      List.filter 
	(fun (scheme,params) -> 
	   String.lowercase scheme = "digest"
	    && List.mem_assoc "realm" params
	    && (try List.mem (List.assoc "algorithm" params) ["MD5";"MD5-sess"]
		with Not_found -> true)
	    && (try contains_auth (List.assoc "qop" params)
		with Not_found -> true)
	    && List.mem_assoc "nonce" params
	) 
	auth_list in
    match  digest_auth_list with
      | [] ->
	  raise Not_applicable
      | (_,params) :: _ ->
	  (* Restriction: only the first request can be processed *)
	  params
  in
  let domain_url =
    if for_proxy then [] else
      try 
	List.map
	  normalize_domain
	  (split_words (List.assoc "domain" digest_request))
      with
	  Not_found -> [ get_domain_uri init_call ] in
  let domain =
    List.map Neturl.string_of_url domain_url in
  let realm =
    try List.assoc "realm" digest_request
    with Not_found -> assert false in
  let key =
    (* Return the selected key, or raise Not_applicable *)
    try
      key_handler # inquire_key ~domain ~realms:[realm] ~auth:"digest"
    with
	Not_found -> raise Not_applicable
  in
  (* Check the key: *)
  let () =
    if key#realm <> realm then raise Not_applicable;
    if key#domain <> domain then raise Not_applicable;
  in
  let algorithm =
    try List.assoc "algorithm" digest_request
    with Not_found -> "MD5" in
  let qop =
    if List.mem_assoc "qop" digest_request then "auth" else "" in
    (* "" = RFC 2069 mode *)
  let nonce =
    try List.assoc "nonce" digest_request
    with Not_found -> assert false in
  let cnonce_init0 = 
    try 
      let s = String.make 8 'X' in
      let () = Netsys_rng.fill_random s in
      s
    with _ -> string_of_float (Unix.gettimeofday()) in
object(self)
  val mutable cnonce_init = cnonce_init0
  val mutable cnonce_incr = 0
  val mutable nc = 0
  val mutable opaque = None
  val mutable a1 = None

  method private first_cnonce =
    Digest.to_hex
      (Digest.string (cnonce_init ^ ":0"))

  method private next_cnonce() =
    let cnonce =
      Digest.to_hex
	(Digest.string (cnonce_init ^ ":" ^ string_of_int cnonce_incr)) in
    cnonce_incr <- cnonce_incr + 1;
    cnonce

  method private next_nc() =
    let r = nc in
    nc <- nc + 1;
    r

  method private fn_h data =
    encode_hex (Digest.string data)

  method private fn_kd secret data =
    encode_hex (Digest.string (secret ^ ":" ^ data))

  method private a1 =
    match a1 with
      | Some v -> v
      | None ->
	  let v =
	    match algorithm with
	      | "MD5" ->
		  key#user ^ ":" ^ realm ^ ":" ^ key#password
	      | "MD5-sess" ->
		  (self # fn_h
		     (key#user ^ ":" ^ realm ^ ":" ^ key#password)) ^ 
		  ":" ^ nonce ^ ":" ^ self#first_cnonce
	      | _ ->
		  assert false
	  in
	  a1 <- Some v;
	  v

  method private a2 call =
    let meth = call # request_method in
    let uri = call # effective_request_uri in
    meth ^ ":" ^ uri

  method authenticate call =
    let cnonce = self#next_cnonce() in
    let nc = self # next_nc() in
    let digest =
      match qop with
	| "auth" ->
	    self#fn_kd
	      (self#fn_h self#a1)
	      (nonce ^ ":" ^ (Printf.sprintf "%08x" nc) ^ ":" ^ cnonce ^ ":" ^ 
		 "auth:" ^ (self#fn_h (self#a2 call)))
	| "" ->
	    self#fn_kd
	      (self#fn_h self#a1)
	      (nonce ^ ":" ^ (self#fn_h (self#a2 call))) 
	| _ ->
	    assert false  (* such digests are not accepted *)
    in
    let creds =
      Printf.sprintf
	"Digest username=\"%s\",realm=\"%s\",nonce=\"%s\",uri=\"%s\",response=\"%s\",algorithm=%s,cnonce=\"%s\",%s%snc=%08d"
	key#user
	realm
	nonce
	call#effective_request_uri
	digest
	algorithm
	cnonce
	(match opaque with
	   | None -> ""
	   | Some s -> "opaque=\"" ^ s ^ "\",")
	(match qop with
	   | "" -> ""
	   | "auth" -> "qop=auth,"
	   | _ -> assert false)
	nc in
    let field_name =
      if for_proxy then "Proxy-Authorization" else "Authorization" in
    [ field_name, creds ]

  method auth_scheme = "digest"
  method auth_domain = domain_url
  method auth_realm = key # realm
  method auth_user = key # user
  method auth_in_advance = enable_auth_in_advance

  method invalidate call =
    (* Check if the [stale] flag is set for our nonce: *)
    let is_stale =
      try
	let auth_list = 
	  Nethttp.Header.get_www_authenticate call#response_header in
	List.exists
	  (fun (scheme,params) -> 
	     String.lowercase scheme = "digest"
	      && (try List.assoc "realm" params = realm
		  with Not_found -> false) 
	      && (try List.assoc "nonce" params = nonce
		  with Not_found -> false)
	      && (try String.lowercase (List.assoc "stale" params) = "true"
		  with Not_found -> false)
	  ) 
	  auth_list
      with
	| Not_found -> false  (* No www-authenticate header *)
	| Nethttp.Bad_header_field _ -> false in
    is_stale || (
      key_handler # invalidate_key key;
      false
    )
end


class digest_auth_handler ?(enable_auth_in_advance=false) 
                         (key_handler : #key_handler)
                         : auth_handler =
object(self)
  method create_session call options =
    try
      Some(new digest_auth_session
	     enable_auth_in_advance options key_handler call false)
    with
	Not_applicable ->
	  None
  method create_proxy_session call options =
    try
      Some(new digest_auth_session 
	     enable_auth_in_advance options key_handler call true)
    with
	Not_applicable ->
	  None
end


class unified_auth_handler (key_handler : #key_handler) : auth_handler =
object(self)
  method create_session call options =
    try Some(new digest_auth_session false options key_handler call false)
    with Not_applicable ->
      try Some(new basic_auth_session false key_handler call false)
      with Not_applicable ->
	None
  method create_proxy_session call options =
    try Some(new digest_auth_session false options key_handler call true)
    with Not_applicable ->
      try Some(new basic_auth_session false key_handler call true)
      with Not_applicable ->
	None
end



let norm_neturl neturl =
  (* Returns the neturl as normalized string (esp. normalized % sequences) *)
  assert(Neturl.url_provides ~port:true neturl);
  let neturl' =
    Neturl.make_url 
      ~encoded:false
      ~scheme:(Neturl.url_scheme neturl)
      ~host:(Neturl.url_host neturl)
      ~path:(try Neturl.url_path neturl with Not_found -> [])
      ?query:(try Some(Neturl.url_query neturl) with Not_found -> None)
      ?fragment:(try Some(Neturl.url_fragment neturl) with Not_found -> None)
      (Neturl.url_syntax_of_url neturl) in
  Neturl.string_of_url neturl'


let prefixes_of_neturl s_url =
  (* Returns a list of all legal prefixes of the absolute URI s.
   * The prefixes are in Neturl format.
   *)
  let rec rev_path_prefixes rev_path =
    match rev_path with
      | [] -> []
      | [ "" ] -> [ rev_path; [] ]
      | [ ""; "" ] -> assert false
      | [ _; "" ] -> rev_path :: rev_path_prefixes [ "" ]
      | "" :: rev_path' ->
	  if rev_path' = [ ""; "" ] then
	    rev_path :: rev_path_prefixes [ "" ]
	  else
	    rev_path :: rev_path_prefixes rev_path'
      | _ :: rev_path' ->
	  rev_path :: (rev_path_prefixes ("" :: rev_path'))
  in
  let path_prefixes path =
    List.map List.rev (rev_path_prefixes (List.rev path)) in
  let s_nofrag_url = Neturl.remove_from_url ~fragment:true s_url in
  let s_noquery_url = Neturl.remove_from_url ~query:true s_nofrag_url in
  let path = Neturl.url_path s_noquery_url in
  s_url :: s_nofrag_url ::
    (List.map
       (fun prefix -> Neturl.modify_url ~path:prefix s_noquery_url
       )
       (path_prefixes path))



class auth_cache =
object(self)
  val mutable auth_handlers = []
  val sessions = Hashtbl.create 10
    (* Only sessions that can be used for authentication in advance. 
     * The hash table maps domain URIs to sessions.
     *)

  method add_auth_handler (h : auth_handler) =
    auth_handlers <- auth_handlers @ [h]

  method create_session (call : http_call) options =
    (* Create a new session after a 401 reply *)
    let rec find l =
      match l with
	| [] -> None
	| h :: l' ->
	    ( match h # create_session call options with
		| None ->
		    find l'
		| Some s ->
		    Some s
	    )
    in
    find auth_handlers

  method tell_successful_session (sess : auth_session) =
    (* Called by [postprocess_complete_message] when authentication was
     * successful. If enabled, [sess] can be used for authentication
     * in advance.
     *)
    if sess # auth_in_advance then (
      List.iter
	(fun dom_uri ->
	   try
	     let dom_uri' = norm_neturl dom_uri in
	     Hashtbl.replace sessions dom_uri' sess
	   with
	     | Neturl.Malformed_URL -> ()
	)
	sess#auth_domain
    )

  method tell_failed_session (sess : auth_session) =
    (* Called by [postprocess_complete_message] when authentication 
     * failed
     *)
    List.iter
      (fun dom_uri ->
	 try
	   let dom_uri' = norm_neturl dom_uri in
	   Hashtbl.remove sessions dom_uri'
	 with
	   | Neturl.Malformed_URL -> ()
      )
      sess#auth_domain;


  method find_session_in_advance (call : http_call) =
    (* Find a session suitable for authentication in advance *)
    let uri = call # private_api # request_uri_with() in
    (* We are not only looking for [uri], but also for all prefixes of [uri] *)
    try
      let prefixes = prefixes_of_neturl uri in
      let prefix =
	List.find (* or Not_found *)
	  (fun prefix ->
	     let s = norm_neturl prefix in
	     Hashtbl.mem sessions s
	  )
	  prefixes in
      Hashtbl.find sessions (norm_neturl prefix)
    with
      | Neturl.Malformed_URL ->
	  raise Not_found
end


(* Backwards compatibility: *)

class key_backing_store =
object(self)
  val db = (Hashtbl.create 10 : (string, (string*string)) Hashtbl.t)
  method set_realm realm user password =
    Hashtbl.replace db realm (user,password)
  method inquire_key ~domain ~realms ~(auth:string) =
    let realm = List.find (fun realm -> Hashtbl.mem db realm) realms in
    let (user, password) = Hashtbl.find db realm in
    ( object
	method user = user
	method password = password
	method realm = realm
	method domain = (domain : string list)
      end
    )
  method invalidate_key (_ : key) = ()
end


class auth_method name (mk_auth_handler : key_ring -> auth_handler) =
  let key_bs =
    new key_backing_store in
  let key_ring = 
    new key_ring ~uplink:key_bs () in
  let auth_handler = 
    mk_auth_handler key_ring in
object(self)
  method name = (name : string)
  method set_realm realm user password =
    key_bs # set_realm realm user password
  method as_auth_handler =
    auth_handler
end


class basic_auth_method =
  auth_method 
    "basic"
    (fun kr -> 
       new basic_auth_handler ~enable_auth_in_advance:true kr)

class digest_auth_method =
  auth_method
    "digest"
    (fun kr -> 
       new digest_auth_handler ~enable_auth_in_advance:true kr)


(**********************************************************************)
(***                 THE CONNECTION CACHE                           ***)
(**********************************************************************)

type connection_cache = Http_client_conncache.connection_cache

let close_connection_cache conn_cache =
  conn_cache # close_all()

let create_restrictive_cache() = new restrictive_cache()

let create_aggressive_cache() = new aggressive_cache()


(**********************************************************************)
(***                       THE I/O BUFFER                           ***)
(**********************************************************************)

(* io_buffer performs the socket I/O.
 *
 * TODO: COMMENT OUT OF DATE
 * The input side is a queue of octets which can be filled by
 * a Unix.read call at its end, and from which octets can be removed
 * at its beginning ("consuming octets").
 * There is also functionality to remove 1XX responses at the beginning
 * of the buffer, and to interpret the beginning of the buffer as HTTP
 * status line.
 * The idea of the buffer is that octets can be added at the end of the
 * buffer while the beginning of the buffer is interpreted as the beginning
 * of the next message. Once enough octets have been added that the message
 * is complete, it is removed (consumed) from the buffer, and the possibly
 * remaining octets are the beginning of the following message.
 *)

exception Garbage_received of string
  (* This exception is raised by [parse_response] when a protocol error
   * occurred before the response status line has been completely received.
   * Such errors are not transferred to the http_call.
   *)


let status_re = 
  Netstring_str.regexp "^\\([^ \t]+\\)\
                        [ \t]+\
                        \\([0-9][0-9][0-9]\\)\
                        \\([ \t]+\\([^\r\n]*\\)\\)?\
                        \r?$" ;;

let chunk_re = 
  Netstring_str.regexp "[ \t]*\
                        \\([0-9a-fA-F]+\\)\
                        [ \t]*\
                        \\(;[^\r\n\000]*\\)?\
                        \r?" ;;

type sockstate =
    Down
  | Up_rw 
  | Up_r
;;


type send_token =
  | Send_header of int * string * string * Netmime.mime_header_ro
      (* call_id, method, url, header *)
  | Send_body of Uq_io.in_device * int64 option
      (* data, length_opt *)
  | Send_body_chunked of Uq_io.in_device
  | Send_eof


let max_line_len = 65536

let input_line_opt_e ?max_len dev =
  Uq_io.input_line_e ?max_len dev >> Uq_io.eof_as_none >>
    (fun st ->
       match st with
	 | `Error Uq_io.Line_too_long ->
	     raise(Garbage_received "Line too long")
	 | _ -> st
    )

let input_opt_e dev s p n =
  Uq_io.input_e dev s p n >> Uq_io.eof_as_none


class type io_buffer =
object
  method socket_state : sockstate
  method socket : Unix.file_descr
  method socket_str : string
  method close : followup:(unit->unit) -> unit -> unit
  method down : unit -> unit
  method status_seen : bool
  method configure_read : fetch_call:(unit -> http_call) -> unit -> unit
  method read_e : Unixqueue.event_system -> http_call option Uq_engines.engine
  method write_activity : bool
  method add : send_token -> unit
  method write_e : Unixqueue.event_system -> unit Uq_engines.engine
end


let io_buffer options fd mplex fd_state : io_buffer =
  let dev = `Multiplex mplex in
  let buf_in_dev = `Buffer_in(Uq_io.create_in_buffer dev) in

  ( object (self)

    (****************************** SOCKET ********************************)
      
      val mutable socket_state = fd_state
      val mutable status_seen = false

      method socket_state = socket_state

      method socket = 
	match socket_state with
	  | Down -> failwith "Socket is down"
	  | _ -> fd
	      
      method socket_str =
	try
	  Int64.to_string (Netsys.int64_of_file_descr self # socket)
	with _ -> "n/a"
	  

      method close ~followup () =
	match socket_state with
	  | Down  -> followup ()
	  | _     -> 
	      let fd_str = self # socket_str in
	      if !options.verbose_connection then 
		dlogr (fun () ->
			 sprintf "FD %s - HTTP connection: Closing socket!"
			   fd_str);
	      mplex # cancel_reading();
	      mplex # cancel_writing();
	      mplex # start_shutting_down
		~when_done:(fun x_opt ->
			      Netlog.Debug.release_fd fd;
			      mplex # inactivate();
			      match x_opt with
				| None -> 
				    followup ()
				| Some err ->
				    if !options.verbose_connection then
				      dlog(sprintf
					     "FD %s - Shutdown error: %s"
					     fd_str (Netexn.to_string err));
				    followup()
			   )
		();
	      socket_state <- Down

      method down() =
	socket_state <- Down  (* our view *)

    (****************************** INPUT ********************************)

    val mutable cfg_fetch_call = (fun () -> raise Not_found)


    method status_seen =
      (* Whether at least one status line (incl. status 1XX) has been seen *)
      status_seen

    method configure_read ~fetch_call () =
      (* fetch_call: This is called when the status line is read to get
	 the corresponding call. This function may raise [Not_found] in
	 which case the status line is an error
       *)
      cfg_fetch_call <- fetch_call


    method read_e esys =
      (* This engine returns the next call read from the input, or
	 returns None for EOF.
       *)
      assert (socket_state = Up_r || socket_state = Up_rw);

      let cur_call = ref None in
      (* remember the call - only for postprocessing on error *)
      
      let rec read_status_line_e call_opt =
	input_line_opt_e ~max_len:max_line_len buf_in_dev
	++ (fun line_opt ->
	      match line_opt with
		| None ->
		    eps_e (`Done None) esys
		| Some line ->
		    ( match Netstring_str.string_match status_re line 0 with
			| None ->
			    raise (Garbage_received "Bad status line")
			| Some m ->
			    let proto = Netstring_str.matched_group m 1 line in
			    let code_str = Netstring_str.matched_group m 2 line in
			    let code = int_of_string code_str in
			    let text =
			      try Netstring_str.matched_group m 4 line
			      with Not_found -> "" in
			    if code < 100 || code > 599 then 
			      raise (Garbage_received "Bad status code");
			    status_seen <- true (* code >= 200 *);
			    let call_opt = 
			      if call_opt = None then (
				let call_opt' = 
				  try Some(cfg_fetch_call())
				  with Not_found -> None in
				cur_call := call_opt';
				call_opt'
			      )
			      else
				call_opt in
			    ( match call_opt with
				| None ->
				    raise(Garbage_received "Spontaneous data")
				| Some call ->
(* eprintf "code=%d text=%s proto=%s\n%!" code text proto; *)
				    if code >= 200 then
				      call # private_api # set_response_status
					code text proto;
				    let hdr_buf = Buffer.create 500 in
				    read_header_e code call hdr_buf
			    )
		    )
	   )

      and read_header_e code call hdr_buf =
	input_line_opt_e ~max_len:max_line_len buf_in_dev
	++ (fun line_opt ->
	      match line_opt with
		| None ->
		    let msg = "EOF where response header expected" in
		    raise (Garbage_received msg)
		| Some line ->
		    Buffer.add_string hdr_buf line;
		    Buffer.add_string hdr_buf "\n";
		    if line = "" || line = "\r" then (
		      parse_header_e code call (Buffer.contents hdr_buf)
		    )
		    else (
		      if Buffer.length hdr_buf > 100000 then (
			let msg ="Response header too long" in
			raise (Garbage_received msg)
		      );
		      read_header_e code call hdr_buf
		    )
	   )
	
      and parse_header_e code call hdr_str =
	let header_l, real_end_pos =
	  try
	    Mimestring.scan_header
	      ~downcase:false ~unfold:true ~strip:true hdr_str
	      ~start_pos:0 ~end_pos:(String.length hdr_str)
	  with
	    | Failure _ ->
		let msg = "Bad response header" in
		raise (Garbage_received msg)
	in
	assert(real_end_pos = String.length hdr_str);
	let header = new Netmime.basic_mime_header header_l in
	call # private_api # set_continue (code < 200);
	(* Calling set_continue may trigger that the send loop
	   in [transmitter] resumes its send task. We need to do it
	   whatever code we see here - not only for code=100.
	 *)
	if code < 200 then (
	  if !options.verbose_events then
	    dlogr (fun () ->
		     sprintf "FD %s - HTTP events: Got 100 Continue line"
		       self#socket_str
		  );
	  read_status_line_e None
	)
	else (
	  call # private_api # set_response_header header;
	  read_body_e code header call
	)

      and read_body_e code header call =
	(* First determine whether a body is expected: 
	   - Normally, the call has either always a body (like GET), or
	     it does not (like HEAD). Exceptions are only the codes 204
	     and 304
	   - The CONNECT method is broken in this respect. If a 200
	     response is emitted, the response body is missing. Any
	     error response includes a body, though.
	 *)
	let have_body =
	  call # has_resp_body && code <> 204 && code <> 304 &&
	    (call # request_method <> "CONNECT" || code >= 300) in
	if have_body then (
	  let out_dev = call # private_api # response_body_open_wr esys in
	  (* Check if we have chunked encoding: *)
	  let is_chunked =
	    try header # field "Transfer-encoding" <> "identity"
	    with Not_found -> false in
	  if is_chunked then
	    read_chunked_body_e code header out_dev call
	  else (
	    let length_opt =
	      try 
		let l = Int64.of_string (header # field "Content-Length") in
		if l < 0L then raise (Bad_message "Bad Content-Length field");
		Some l
	      with
		| Failure _ -> raise (Bad_message "Bad Content-Length field")
		| Not_found -> None in
	    read_plain_body_e code header out_dev length_opt call
	  )
	) else
	  read_end_e call
	
      and read_plain_body_e code header out_dev length_opt call =
	(* Parses a non-chunked HTTP body. If length_opt=None, the message
	 * is terminated by EOF. If length_opt=Some len, the message has
	 * this length.
	 *)
	( Uq_io.copy_e
	    ?len64:length_opt
	    buf_in_dev
	    out_dev
	  >> (function
		| `Done n ->
		    ( match length_opt with
			| None -> ()
			| Some l ->
			    if n <> l then
			      raise(Bad_message "EOF in response message")
		    );
		    `Done n
		| st -> st
	     )   (* out_dev is closed in read_end_e *)
	)
	++ (fun n -> read_end_e call)

      and read_chunked_body_e code header out_dev call =
	(* Parses a chunked HTTP body *)
	input_line_opt_e ~max_len:max_line_len buf_in_dev
	++ (function
	      | Some line ->
		  ( match Netstring_str.string_match chunk_re line 0 with
		      | None ->
			  raise
			    (Bad_message "Cannot parse chunk of response body");
		      | Some m ->
			  let hex_len = Netstring_str.matched_group m 1 line in
			  let len =
			    try Int64.of_string ("0x" ^ hex_len)
			    with Failure _ -> 
			      raise (Bad_message "Chunk too large") in
			  if len = 0L then
			    let trl_buf = Buffer.create 100 in
			    read_trailer_e code header out_dev call trl_buf
			  else
			    read_chunk_data_e code header out_dev len call
		  )
	      | None ->
		  raise (Bad_message "EOF where next response chunk expected")
	   )

      and read_chunk_data_e code header out_dev len call =
	(* Parses the chunk data following the chunk size field *)
	Uq_io.copy_e
	  ?len64:(Some len)
	  buf_in_dev
	  out_dev
	++ (fun n ->
	      if n <> len then
		raise(Bad_message "EOF in response message");
	      read_chunk_end_e code header out_dev call
	   )

      and read_chunk_end_e code header out_dev call =
	input_line_opt_e ~max_len:max_line_len buf_in_dev
	++ (function
	      | Some line ->
		  if line = "" || line = "\r" then
		    read_chunked_body_e code header out_dev call
		  else
		    raise (Bad_message "CR/LF after response chunk is missing")
	      | None ->
		  raise (Bad_message "EOF where next response chunk expected")
	   )

      and read_trailer_e code header out_dev call trl_buf =
	input_line_opt_e ~max_len:max_line_len buf_in_dev
	++ (fun line_opt ->
	      match line_opt with
		| None ->
		    let msg = "EOF where response trailer expected" in
		    raise (Bad_message msg)
		| Some line ->
		    Buffer.add_string trl_buf line;
		    Buffer.add_string trl_buf "\n";
		    if line = "" || line = "\r" then (
		      parse_trailer_e
			code header out_dev call (Buffer.contents trl_buf)
		    )
		    else (
		      if Buffer.length trl_buf > 10000 then (
			let msg ="Response trailer too long" in
			raise (Bad_message msg)
		      );
		      read_trailer_e code header out_dev call trl_buf
		    )
	   )
	
      and parse_trailer_e code header out_dev call trl_str =
	let trailer_l, real_end_pos =
	  try
	    Mimestring.scan_header
	      ~downcase:false ~unfold:true ~strip:true trl_str
	      ~start_pos:0 ~end_pos:(String.length trl_str)
	  with
	    | Failure _ ->
		let msg = "Bad response trailer" in
		raise (Bad_message msg)
	in
	assert(real_end_pos = String.length trl_str);
	let new_header =
	  new Netmime.basic_mime_header (header#fields @ trailer_l) in
	call # private_api # set_response_header new_header;
	(* out_dev is closed in read_end_e *)
	read_end_e call
	  
      and read_end_e call =
	call # private_api # finish_response_e esys  (* will close out_dev *)
	++ (fun () ->
	      eps_e (`Done (Some call)) esys
	   )
      in

      read_status_line_e None
      >> (fun st ->
	    let propagate_garbage = !cur_call <> None in
	    let maybe_cleanup() =
	      match !cur_call with
		| None -> ()
		| Some call -> 
		    call # private_api # cleanup();
		    cur_call := None in
	    match st with
	      | `Error (Garbage_received msg) when propagate_garbage ->
		  maybe_cleanup();
		  `Error (Bad_message msg)
		    (* If we can associate a Garbage_received error with
		       a certain call change the exception to Bad_message.
		       The background is that Garbage_received is not
		       attributed to a specific message by the higher layers.
		     *)
	      | `Error e ->
		  maybe_cleanup();
		  `Error e
	      | _ ->
		  st
	 )

    (****************************** OUTPUT ********************************)

    val send_queue = Q.create()
    val send_buf = String.create 4096

    val mutable sending = false


    method write_activity =
      sending || not(Q.is_empty send_queue)


    method add x =
      Q.add x send_queue


    method write_e esys =
      (* Writes all contents of send_queue *)
      (* It is the task of the caller to close the request body, by
	 calling finish_request_e at the right moment
       *)
      
      let rec write_next_e() =
	if Q.is_empty send_queue then
	  eps_e (`Done()) esys
	else (
	  let token = Q.take send_queue in
	  ( match token with
	      | Send_header (call_id, meth, url, hdr) ->
		  write_header_e call_id meth url hdr
	      | Send_body (dev,length_opt) ->
		  write_body_e dev length_opt
	      | Send_body_chunked dev ->
		  write_body_chunked_e dev
	      | Send_eof ->
		  write_eof_e ()
	  ) 
	  ++ write_next_e
	)

      and write_header_e call_id meth url hdr =
	let buf = B.create 1000 in
	B.add_string buf meth;
	B.add_string buf " ";
	B.add_string buf url;
	B.add_string buf " HTTP/1.1";
	
	if !options.verbose_status then
	  dlogr
	    (fun () ->
	       sprintf "FD %s - Call %d - HTTP request: %s"
		 self#socket_str call_id (B.contents buf));
	
	B.add_string buf "\r\n";
	let ch = new Netchannels.output_netbuffer buf in
	Mimestring.write_header ch hdr#fields;
	ch # close_out();
	      
	if !options.verbose_request_header then
	  dump_header "HTTP request " hdr#fields;
	      
	Uq_io.output_netbuffer_e dev buf

      and write_body_e d expected_length_opt =
	if !options.verbose_request_contents then
	  dlogr
	    (fun () ->
	       sprintf "FD %Ld - HTTP request body fragment %s"
		 (Netsys.int64_of_file_descr fd)
		 (match expected_length_opt with
		    | None -> "(unknown length)"
		    | Some n -> sprintf "(%Ld bytes)" n
		 )
	    );

	(* N.B. We do not close [d] here - task of caller *)
	Uq_io.copy_e ?len64:expected_length_opt d dev
	>> (function
	      | `Done n ->
		  ( match expected_length_opt with
		      | None -> ()
		      | Some exp_n ->
			  if n <> exp_n then
			    failwith "Http_client: announced length of \
                                      request body does not match actual length"
		  );
		  `Done ()
	      | `Error e -> `Error e
	      | `Aborted -> `Aborted
	   )
	  
      and write_body_chunked_e d =
	write_body_next_chunk_e d ()

      and write_body_next_chunk_e d () =
	input_opt_e d (`String send_buf) 0 (String.length send_buf)
	++ (function
	      | Some n ->
		  if !options.verbose_request_contents then
		    dlogr
		      (fun () ->
			 sprintf "FD %Ld - HTTP request body chunk (%d bytes)"
			   (Netsys.int64_of_file_descr fd) n
		      );
		  let s = sprintf "%x\r\n" n in
		  Uq_io.output_string_e dev s
		  ++ (fun () -> 
			Uq_io.really_output_e dev (`String send_buf) 0 n)
		  ++ (fun () -> Uq_io.output_string_e dev "\r\n")
		  ++ write_body_next_chunk_e d
	      | None ->
		  if !options.verbose_request_contents then
		    dlogr
		      (fun () ->
			 sprintf "FD %Ld - HTTP request body chunk (last)"
			   (Netsys.int64_of_file_descr fd)
		      );
		  let s = "0\r\n\r\n" in
		  Uq_io.output_string_e dev s
		    (* N.B. We do not close [d] here - task of caller *)
	   )

      and write_eof_e () =
	if !options.verbose_request_contents then
	  dlogr (fun () ->
		   sprintf "FD %Ld - HTTP request EOF" 
		     (Netsys.int64_of_file_descr fd)
		);
	Uq_io.write_eof_e dev
	++ (fun flag ->
	      if not flag then
		failwith "Http_client: \
                          no support for closing the write side only";
	      (* It is better to set the new state after a successful
		 write_eof operation than before. It is possible that the whole
		 chain of write operations is aborted. In this case
		 an EOF also needs to be written. If we abort between write_eof
		 and the following assignment, the EOF will just be
		 written twice (which is unproblematic). Otherwise, the EOF
		 could be forgotten, and the client would hang.
	       *)
	      if socket_state = Up_rw then
		socket_state <- Up_r;
	      eps_e (`Done()) esys
	   )
      in

      (* If the socket is already closed for writing, do nothing. This
	 can happen if the read side decides to close the socket.
       *)
      if socket_state = Up_rw then (
        sending <- true;
	write_next_e()
	>> (fun st -> sending <- false; st)
      )
      else 
	eps_e (`Done ()) esys

    end
  )
;;

(**********************************************************************)
(***                PROTOCOL STATE OF A SINGLE MESSAGE              ***)
(**********************************************************************)

(* The class 'transmitter' is a container for the message and the
 * associated protocol state, and defines the methods to send
 * the request, and to receive the reply.
 * The protocol state stored here mainly controls the send and receive
 * buffers (e.g. how many octets have already been sent? Are the octets
 * received so far already a complete message or not?)
 *)


type message_state =
    Unprocessed     (* Not even a byte sent or received *)
  | Sending_hdr     (* Sending currently the header *)
  | Handshake       (* The header has been sent, now waiting for status 100 *)
  | Sending_body    (* The header has been sent, now sending the body *)
  | Finishing       (* The body is almost sent - about to finish *)
  | Sent_request    (* The body has been sent; the reply is being received *)
  | Complete        (* The whole reply has been received *)
;;

(* The states Unprocessed...Sent_request are only set by the request
   sender. We enter Complete only if we can receive the corresponding
   response.

   In the case that the response arrives while we are still sending,
   the processing of the response needs to be delayed until the sending
   is done. What can happen then:
   - We get the status while waiting for "100". This is handled in send_e,
     and the status is set to Finishing.
   - We get the status line later while still sending. This is detected
     by the reader, and send_interrupt is called.
   - If we get the status line when already in state Finishing, the 
     request is not aborted, and the remaining bytes are sent.

   Errors are not reflected in the state.
 *)


let string_of_state =
  function
    | Unprocessed  -> "Unprocessed"
    | Sending_hdr  -> "Sending_hdr"
    | Handshake    -> "Handshake"
    | Sending_body -> "Sending_body"
    | Finishing    -> "Finishing"
    | Sent_request -> "Sent_request"
    | Complete     -> "Complete"



let test_conn_close hdr =
  let conn_list = 
    try Nethttp.Header.get_connection hdr
    with _ (* incl. syntax error *) -> [] in
  List.mem "close" (List.map String.lowercase conn_list)


let test_conn_keep_alive hdr =
  let conn_list = 
    try Nethttp.Header.get_connection hdr
    with _ (* incl. syntax error *) -> [] in
  List.mem "keep-alive" (List.map String.lowercase conn_list)


let test_proxy_conn_close hdr =
  let conn_list = 
    try 
      List.map String.lowercase
	(hdr # multiple_field "proxy-connection")
    with _ (* incl. syntax error *) -> [] in
  List.mem "close" (List.map String.lowercase conn_list)


let test_proxy_conn_keep_alive hdr =
  let conn_list = 
    try 
      List.map String.lowercase
	(hdr # multiple_field "proxy-connection")
    with _ (* incl. syntax error *) -> [] in
  List.mem "keep-alive" (List.map String.lowercase conn_list)


let test_http_1_1 proto_str =
  try
    let proto = Nethttp.protocol_of_string proto_str in
    match proto with
      | `Http((1,n),_) when n >= 1 ->  
	  (* HTTP/1.1 <= proto < HTTP/2.0 *)
	  true
      | _ ->
	  false
  with _ -> false


let transmitter
  peer_is_proxy
  proxy_auth_state
  (m : http_call) 
  (f_done : http_call -> unit)
  options
  =
  ( object (self) 
      val mutable state = Unprocessed
      val indicate_done = f_done
      val msg = m
	
(*
      val mutable auth_headers = []
	(* Additional header for _proxy_ authentication *)
 *)

      val mutable send_e_opt = None
      val mutable send_finish_e_opt = None
      val mutable send_interrupted = false
	
      method state = state
	
      method f_done = indicate_done

      method send_interrupted = send_interrupted

      method init() =
	(* Prepare for (re)transmission:
	 * - Set the `Effective request header
	 * - Reset the status info of the http_call
	 * - Initialize transmission state
	 *)
	if !options.verbose_status then
	  dlogr (fun () -> sprintf "Call %d: initialize transmitter" 
		   (Oo.id msg));
	msg # private_api # prepare_transmission();
	(* Set the effective URI. This must happen before authentication. *)
	let eff_uri =
	  if peer_is_proxy then
	    msg # request_uri
	  else
	    let path = msg # get_path() in
	    if path = "" then (
	      msg # empty_path_replacement
	    )
	    else path
	in
	msg # private_api # set_effective_request_uri eff_uri;
	let cah = 
	  match msg # private_api # auth_state with
	    | `None -> []
	    | `In_advance session 
	    | `In_reply session ->
		session # authenticate msg in
	let pah =
	  match !proxy_auth_state with
	    | `None -> []
	    | `In_advance session 
	    | `In_reply session ->
		session # authenticate msg in
	let rh = msg # request_header `Effective in
	List.iter
	  (fun (n,v) ->
	     rh # update_field n v
	  )
	  (cah @ pah);
	if peer_is_proxy then
	  rh # update_field "Proxy-Connection" "keep-alive";
	state <- Unprocessed;
	if not (msg # has_req_body) then (
	  (* Remove certain headers *)
	  rh # delete_field "Expect";
	);
	send_finish_e_opt <- None

      method cleanup() =
	(* release resources *)
	msg # private_api # cleanup()

(*
      method add_auth_header n v =
	auth_headers <- (n,v) :: auth_headers
 *)	
  
      method error_if_unserved error =
	msg # private_api # error_if_unserved !options.verbose_status error

      method send_e (io : io_buffer) 
                    (handshake_cfg : float option)
                    (close_flag : bool)
		    esys =
	(* handshake_cfg: if [Some t], it is waited after the header for the
	   "100 Continue" handshake. [t] is the timeout. The [Expect]
	   header is already set (by the user).
	   close_flag: if true, the "connection:close" header is set
	 *)

	assert (state = Unprocessed);
	assert (not(io # write_activity));
	assert (send_finish_e_opt = None);

	let rec send_header_e() =
	  let rh = msg # request_header `Effective in
	  let host = msg # get_host() in
	  let port = msg # get_port() in
	  let host_str = host ^ (if port = 80 then "" 
				 else ":" ^ string_of_int port) in
	  rh # update_field "Host" host_str;
	  
	  if close_flag then
	    rh # update_field "Connection" "close";

	  io # add (Send_header(self#call_id, 
				msg#request_method,
				msg#effective_request_uri,
				(rh :> Netmime.mime_header_ro)));
	  state <- (if handshake_cfg <> None then Handshake else Sending_hdr);
	  
	  io # write_e esys
	  ++ (match handshake_cfg with
		| Some t when not msg # private_api # continue ->
		    send_handshake_e t
		| _ -> 
		    send_body_e
	     )

	and send_handshake_e tmo () =
	  if !options.verbose_events then
	    dlogr (fun () ->
		     sprintf "Call %d - HTTP events: Waiting for 100 Continue"
		       (Oo.id msg)
		  );
	  msg # private_api # wait_continue_e tmo esys
	  ++ (fun code_100 ->
		if code_100 then
		  send_body_e()
		else
		  (* We got a non-100 response after waiting for 100.
		     We suppress the request body, and instead close the
		     connection on the sender side.
		   *)
		  send_no_body_e()
	     )
	    
	and send_body_e () : unit Uq_engines.engine =
	  if msg # has_req_body then (	  
	    state <- Sending_body;
	    let rh = msg # request_header `Effective in
	    let is_chunked =
	      try rh # field "Transfer-encoding" <> "identity"
	      with Not_found -> false in
	    let d = msg # private_api # request_body_open_rd esys in
	    let tok, need_eof =
	      if is_chunked then
		Send_body_chunked d, false
	      else (
		let length_opt =
		  try 
		    let l = 
		      try Int64.of_string (rh # field "Content-Length") 
		      with
			| Failure _ -> 
			    failwith "Http_client: Bad Content-Length field \
                                      in request" in
		    if l < 0L then
		      failwith "Http_client: Bad Content-Length field in request";
		    Some l
		  with Not_found -> None in
		Send_body(d, length_opt), length_opt = None
	      ) in
	    io # add tok;
	    if need_eof then
	      io # add Send_eof;
	    io # write_e esys
	    ++ (fun () ->
		  state <- Sent_request;
		  msg # private_api # finish_request_e esys
	       )
	  )
	  else (
	    state <- Sent_request;
	    msg # private_api # finish_request_e esys
	  )

	and send_no_body_e () =
	  (* This is only used if we announced a body in the header, but
	     finally do not send it (because we got an error from the server).

	     Normally we just close the connection for writing.
	     If we use the chunked encoding, a better way to indicate
	     the end of the body is to send an empty body 
	   *)
	  state <- Finishing;
	  let rh = msg # request_header `Effective in
	  let is_chunked =
	    try rh # field "Transfer-encoding" <> "identity"
	    with Not_found -> false in
	  if is_chunked then (
	    let ch = new Netchannels.input_string "" in
	    let d = `Async_in(new Uq_engines.pseudo_async_in_channel ch,esys) in
	    io # add (Send_body_chunked d);
	  )
	  else
	    io # add Send_eof;
	  io # write_e esys
	  ++ (fun () ->
		state <- Sent_request;
		eps_e (`Done ()) esys
	     )
	in

	let (fin_e, signal) = Uq_engines.signal_engine esys in
	send_finish_e_opt <- Some fin_e;

	let e = send_header_e() in
	send_e_opt <- Some e;

	let e' =
	  (* This engine handles send interrupts, and returns [true] if an
	     EOF needs to be written
	   *)
	  e >> (fun st ->
		  send_e_opt <- None;
		  match st with
		    | `Aborted when send_interrupted ->
			if !options.verbose_events then
			  dlogr
			    (fun () ->
			       sprintf "Call %d - HTTP event: Send interrupted" 
				 (Oo.id msg));
			`Done true
		    | `Aborted -> `Aborted
		    | `Error err -> `Error err
		    | `Done () -> `Done false
	       ) in
	let e'' =
	  (* This engine writes the EOF if needed *)
	  e' 
	  ++ (fun flag ->
		if flag then (
		  io # add Send_eof;
		  io # write_e esys
		)
		else eps_e (`Done ()) esys
	     ) in

	(* Finally catch errors and signal termination: *)
	e''
	>> (fun st ->
	      ( match st with
		  | `Error err ->
		      if !options.verbose_events then
			dlogr
			  (fun () ->
			     sprintf "Call %d - HTTP event: Send exception: %s" 
			       (Oo.id msg) (Netexn.to_string err)
			  );
		  | _ -> ()
	      );
	      signal st; st
	   )



      method send_interrupt() =
	(* Stop sending immediately. This method is called by the reader
	   when an error status is received while we are still writing
	   the request. The reaction is to close the write side of the
	   connection.

	   For chunked encoding, we could also finish the current chunk,
	   and send an empty chunk. However, this is complicated to
	   get done here, so we always use the close method.
	 *)
	match send_e_opt with
	  | None -> ()
	  | Some e ->
	      send_interrupted <- true;
	      e # abort()

	  
      method send_finish_e esys =
	(* Wait here until the request is completely sent (synchronization with
	   the sender)
	 *)
	match send_finish_e_opt with
	  | None ->
	      eps_e (`Done ()) esys
	  | Some e ->
	      e
	  
	  
      method receive_complete () =
	assert(state = Handshake || state = Sending_body || state = Finishing ||
	    state = Sent_request);
	(* Handshake: this is possible when we were waiting for a 100 response,
	   but got a non-100 instead. In this case, the request is not sent,
	   hence the state remains at Handshake.

	   Sending_body: this is possible when we get a server status while
	   sending the reqest, and stopping the request because of this.
	 *)
	state <- Complete


      method indicate_pipelining =
	(* Return 'true' iff the reply is HTTP/1.1 compliant and does not
	 * contain the 'connection: close' header.
	 *)
	let req_hdr = msg # request_header `Effective in
	let b0 = 
	  if peer_is_proxy then
	    not(test_proxy_conn_close req_hdr) &&
	      not(test_conn_close req_hdr)
	  else
	    not(test_conn_close req_hdr)  in
	b0 && (
	  let resp_header = msg # private_api # response_header in
	  let proto_str = msg # private_api # response_proto in
	  let b1 = 
	    if peer_is_proxy then
	      test_http_1_1 proto_str && not(test_proxy_conn_close resp_header) 
		&& not(test_conn_close resp_header) 
	    else
	      test_http_1_1 proto_str && not(test_conn_close resp_header) in
	  b1 && (
	    try
	      let server = resp_header # field "Server" in
	      not (List.exists 
		     (fun re -> 
			Netstring_str.string_match re server 0 <> None
		     ) 
		     pipeline_blacklist)
	    with
	      | Not_found -> true  (* Nothing known ... Assume the best! *)
	  )
	)
	  
      method indicate_sequential_persistency =
	(* Returns 'true' if persistency without pipelining
	 * is possible.
	 *)
	let b0 = 
	  not(test_conn_close (msg # request_header `Effective)) in
	b0 && (
	  let resp_header = msg # private_api # response_header in
	  let proto_str = msg # private_api # response_proto in
	  let is_http_11 = test_http_1_1 proto_str in
	  let normal_persistency =
	    not peer_is_proxy && 
	      (not (test_conn_close resp_header)) &&
	      (is_http_11 || test_conn_keep_alive resp_header) in
	  let proxy_persistency =
	    peer_is_proxy && 
	      (not (test_conn_close resp_header)) &&
	      (not (test_proxy_conn_close resp_header)) &&
	      (is_http_11 || test_proxy_conn_keep_alive resp_header) in
	  normal_persistency || proxy_persistency
	)
	  
      method message = msg
	
      method call_id = Oo.id msg
	
    end
  )
;;


let drive_postprocessing_e esys options m f_done =
  Uq_engines.delay_engine 0.0
    (fun () ->
       ( try
	   if !options.verbose_status then
	     dlogr (fun () -> 
		      sprintf "Call %d - postprocessing" (Oo.id m));
	   let () = f_done m in ()
	 with
	   | any ->
	       if !options.verbose_status then
		 dlogr (fun () -> 
			  sprintf "Call %d - Exception in postprocessing: %s"
			    (Oo.id m) (Netexn.to_string any));
	       let g = Unixqueue.new_group esys in
	       Unixqueue.once esys g 0.0 (fun () -> raise any);
       );
       eps_e (`Done ()) esys
    )
    esys


let drive_postprocessing_msg esys options m f_done =
  ignore(drive_postprocessing_e esys options m f_done)


(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(***                                                                ***)
(***           THE PROTOCOL STATE OF THE CONNECTION                 ***)
(***                                                                ***)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

type peer =
    [ `Direct of string * int
    | `Http_proxy of string * int
    | `Http_proxy_connect of (string * int) * (string * int)
    | `Socks5 of (string * int) * (string * int)
    ]

let first_hop =
  function
    | `Direct (host,port) -> (host,port)
    | `Http_proxy (host,port) -> (host,port)
    | `Http_proxy_connect ((host,port),_) -> (host,port)
    | `Socks5 ((host,port),_) -> (host,port)

let content_hop =
  function
    | `Direct (host,port) -> (host,port)
    | `Http_proxy (host,port) -> (host,port)  (* This case does not work *)
    | `Http_proxy_connect (_,(host,port)) -> (host,port)
    | `Socks5 (_,(host,port)) -> (host,port)


let rewrite_first_hop s =
  function
    | `Direct (host,port) -> `Direct(s,port)
    | `Http_proxy (host,port) -> `Http_proxy(s,port)
    | `Http_proxy_connect ((host1,port1),(host2,port2)) ->
	`Http_proxy_connect ((s,port1),(host2,port2))
    | `Socks5 ((host1,port1),(host2,port2)) ->
	`Socks5 ((s,port1),(host2,port2))


let proxy_connect_e esys fd fd_open host port options proxy_auth_handler_opt
                    cur_proxy_session tcp_real_connect_e setup_e =
  (* Send the CONNECT line plus header, and wait for 200.

     The continuation of this engine is either setup_e() if successful,
     or tcp_real_connect_e() if another connection to the proxy needs to
     be opened.
   *)
  let mplex =
    Uq_engines.create_multiplex_controller_for_connected_socket
      ~supports_half_open_connection:true
      fd esys in
  (* N.B. No timeout here required because this activity is covered by the
     connect timeout
   *)
  let io = io_buffer options fd mplex Up_rw in
  let host_url = sprintf "%s:%d" host port in
  let msg = new connect host_url in
  let hdr = msg # request_header `Effective in
  hdr # update_field "Host" host_url;
  hdr # update_field "Proxy-Connection" "keep-alive";

  let rec request_e () =
    ( match !cur_proxy_session with   (* authentication *)
	| None -> ()
	| Some sess ->
	    let pah = sess # authenticate msg in
	    List.iter
	      (fun (n,v) ->
		 hdr # update_field n v
	      )
	      pah
    );
    io#add (Send_header(0, msg#request_method, msg#effective_request_uri, 
			( hdr :> Netmime.mime_header_ro )
		       ));
    io#configure_read ~fetch_call:(fun () -> msg) ();
    io#write_e esys
    ++ (fun () ->
	  io#read_e esys
	  ++ (function
		| None ->
		    failwith "EOF from proxy server"
		| Some m ->
		    assert(m = msg);
		    ( try
			if m#response_status_code = 407 && 
			  !cur_proxy_session = None &&
			  proxy_auth_handler_opt <> None
			then (
			  let ah =
			    match proxy_auth_handler_opt with
			      | None -> assert false
			      | Some ah -> ah in
			  let sess =
			    match ah # create_proxy_session msg options with
			      | None -> raise Not_found
			      | Some sess -> sess in
			  cur_proxy_session := Some sess;
			  (* It is now possible that the proxy closes the
			     connection. If so, we do this too, and reopen
			     another one. Otherwise, just go on.
			   *)
			  let rh = m#response_header in
			  let ps = m#response_protocol in
			  let close_flag =
			    test_conn_close rh || test_proxy_conn_close rh ||
			      (not (test_http_1_1 ps) && 
				 not (test_conn_keep_alive rh) &&
				 not (test_proxy_conn_keep_alive rh)) in
			  if close_flag then (
			    Unix.close fd;
			    fd_open := false;
			    tcp_real_connect_e()
			      (* The value of !cur_proxy_session is kept,
				 so we will use the authentication knowlege
				 we already gathered
			       *)
			  )
			  else
			    request_e ()
			)
			else 
			  raise Not_found
		      with Not_found ->
			match m#status with
			  | `Unserved ->
			      assert false
			  | `Http_protocol_error err ->
			      failwith ("Exception from proxy connection: " ^ 
					  Netexn.to_string err)
			  | `Redirection
			  | `Client_error
			  | `Server_error ->
			      raise(Proxy_error m#response_status_code)
			  | `Successful ->
			      setup_e()
		    )
	   )
       )
  in
  request_e ()
  >> (function
	| `Error (Garbage_received msg) ->
	    `Error (Bad_message msg)
	| st -> st
     )


let tcp_connect_e esys tp cb (peer:peer) conn_cache conn_owner options 
                  proxy_auth_handler_opt =
  (* An engine connecting to peer_host:peer_port. If a connection is still
     available in conn_cache, use this instead.

     Output is:
     - `Done(fd, conn_time): fd is the new connection (established in conn_time
       seconds)
   *)
  let timeout_value = !options.connection_timeout in
  
  let resolve_e, signal_res =
    Uq_engines.signal_engine esys in

  let t0 = Unix.gettimeofday() in

  let (hop1_host,hop1_port) = first_hop peer in

  !options.resolver
    esys
    hop1_host
    (function 
       | None ->
	   if !options.verbose_events then
	     dlog "HTTP events: reset after DNS failure";
	   let err = Name_resolution_error hop1_host in
	   signal_res (`Error err)
	     
       | Some addr ->
	   signal_res (`Done addr)
    );

  let descr =
    match peer with
      | `Direct (host,port) -> 
	  sprintf "direct connection to %s:%d" host port
      | `Http_proxy (host,port) ->
	  sprintf "proxy connection via %s:%d" host port
      | `Http_proxy_connect ((host1,port1),(host2,port2)) ->
	  sprintf "proxy connection via %s:%d to %s:%d" host1 port1 host2 port2
      | `Socks5 ((host1,port1),(host2,port2)) ->
	  sprintf "SOCKS connection via %s:%d to %s:%d" host1 port1 host2 port2
  in

  let tmo_x =
    Timeout descr in

  let real_host, real_port = content_hop peer in

  resolve_e
  ++ (fun hop1_ip ->
	let hop1_host_ip =
	  Unix.string_of_inet_addr hop1_ip in
	let cache_peer =
	  rewrite_first_hop hop1_host_ip peer in
	try
	  let fd = conn_cache # find_inactive_connection cache_peer cb in
	  (* Case: reuse old connection *)
	  conn_cache # set_connection_state fd cache_peer (`Active conn_owner);
	  if !options.verbose_events then
	    dlog (sprintf 
		    "FD %Ld - HTTP events: config input (reused fd) - target %s"
		    (Netsys.int64_of_file_descr fd) descr);
	  let mplex = 
	    tp # continue 
	      fd cb !options.connection_timeout tmo_x 
	      real_host real_port esys in
	  eps_e (`Done(fd, mplex, 0.0)) esys
	with
	  | Not_found ->
	      if !options.verbose_connection then
		dlog ("HTTP connection: creating " ^ 
			descr);
	      let proxy_opt = (* SOCKS5 *)
		match peer with
		  | `Socks5 _ ->
		      Some(new Uq_socks5.proxy_client 
			     (`Socket(`Sock_inet(Unix.SOCK_STREAM,
						 hop1_ip,
						 hop1_port),
				      Uq_engines.default_connect_options)))
		  | _ -> None in
	      let sockspec =
		match peer with
		  | `Direct _
		  | `Http_proxy _ 
		  | `Http_proxy_connect _ ->
		      `Sock_inet(Unix.SOCK_STREAM, hop1_ip, hop1_port)
		  | `Socks5 (_,(host2,port2)) ->
		      `Sock_inet_byname(Unix.SOCK_STREAM, host2, port2) in

	      let cur_proxy_session = ref None in

	      let rec tcp_real_connect_e () =
		Uq_engines.connector 
		  ?proxy:proxy_opt
		  (`Socket(sockspec, Uq_engines.default_connect_options))
		  esys 
		++ (function
		      | `Socket(fd,_) ->
			  let fd_open = ref true in
			  !options.configure_socket fd;
			  let setup_e() =
			    tp # setup_e
			      fd cb !options.connection_timeout tmo_x
			      real_host real_port esys
			    >> (function
				  | `Done mplex -> `Done(fd,mplex)
				  | `Error err -> `Error err
				  | `Aborted -> `Aborted
			       ) in
			  ( match peer with
			      | `Http_proxy_connect(_,(host2,port2)) ->
				  proxy_connect_e
				    esys fd fd_open host2 port2 options 
				    proxy_auth_handler_opt
				    cur_proxy_session
				    tcp_real_connect_e
				    setup_e
			      | _ ->
				  setup_e()
			  )
			  >> (fun st ->
				match st with
				  | `Error _ | `Aborted ->
				      if !fd_open then Unix.close fd;
				      fd_open := false;
				      st
				  | _ -> st
			     )
		      | _ -> assert false
		   ) in

	      let eng = tcp_real_connect_e() in

	      Uq_engines.timeout_engine 
		timeout_value
		(Timeout (sprintf
			    "creating %s" descr))
		eng
	      ++ (fun (fd,mplex) ->
		    let t1 = Unix.gettimeofday() in
		    let d = t1 -. t0 in

		    if !options.verbose_connection then
		      dlog (sprintf 
			      "FD %Ld - HTTP %s: Connected!"
			      (Netsys.int64_of_file_descr fd) descr);

		    Netlog.Debug.track_fd
		      ~owner:"Http_client"
		      ~descr:(sprintf 
				"HTTP %s" descr)
		      fd;
		    (* The release_fd is in Http_client_conncache! *)

		    conn_cache # set_connection_state
		      fd cache_peer (`Active conn_owner);
		    eps_e (`Done(fd, mplex, d)) esys
		 )
	      >> (function
		    | `Error err ->
			if !options.verbose_connection then (
			  dlog(sprintf 
				 "HTTP connection: Cannot create %s: \
                                  Exception %s"
				 descr (Netexn.to_string err))
			);
			`Error err
		    | st -> st
		 )
     )		

(**********************************************************************)

class type transport_channel_type =
object
  method setup_e : Unix.file_descr -> channel_binding_id -> float -> exn ->
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller Uq_engines.engine
  method continue : Unix.file_descr -> channel_binding_id -> float -> exn ->
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller
end

let http_transport_channel_type : transport_channel_type =
  ( object(self)
      method continue fd cb tmo tmo_x host port esys =
	Uq_engines.create_multiplex_controller_for_connected_socket
	  ~close_inactive_descr:true
	  ~supports_half_open_connection:true
	  ~timeout:( tmo, tmo_x )
	  fd esys
      method setup_e fd cb tmo tmo_x host port esys =
	let mplex = self # continue fd cb tmo tmo_x host port esys in
	eps_e (`Done mplex) esys
    end
  )


(**********************************************************************)

let fragile_pipeline 
       esys  cb
       peer
       proxy_auth_state proxy_auth_handler_opt
       fd mplex connect_time no_pipelining conn_cache
       auth_cache
       counters options =
  (* Implements a pipeline for an existing connection [fd]. This object
     does not implement any way of recovering after errors.
   *)
  let connection_e, signal_connection = Uq_engines.signal_engine esys in
    (* Indicates that the connection is closed *)
  let queue_e, signal_queue = Uq_engines.signal_engine esys in
    (* Indicates that all processing of queued messages is done *)
  let finished_e =
    Uq_engines.sync_engine connection_e queue_e 
    >> (function 
	  | `Done _ -> `Done() 
	  | `Error e -> `Error e
	  | `Aborted -> `Aborted
       ) in

  let fd_str =
    Int64.to_string (Netsys.int64_of_file_descr fd) in

  let peer_is_proxy =  (* whether we have a normal HTTP proxy *)
    match peer with
      | `Http_proxy _ -> true
      | _ -> false in

  ( object(self)
      val mutable io = io_buffer options fd mplex Up_rw

      val mutable write_queue = Q.create()
      val mutable read_queue = Q.create()
	(* Invariant: write_queue is a suffix of read_queue *)

      (* The following two variables control whether pipelining is enabled or
       * not. The problem is that it is unclear how old servers react if we
       * send to them several requests at once. The solution is that the first
       * "round" of requests and replies is done in a compatibility mode: After
       * the first request has been sent sending stops, and the client waits
       * until the reply is received. If the reply indicates HTTP/1.1 and does
       * not contain a "connection: close" header, all further requests and 
       * replies will be performed in pipelining mode, i.e. the requests are
       * sent independent of whether the replies of the previous requests have
       * been received or not.
       *
       * sending_first_message: 'true' means that the first request has not yet
       *    been completely sent to the server. 
       * done_first_message: 'true' means that the reply of the first request
       *    has been arrived.
       *)
      val mutable sending_first_message = true
      val mutable done_first_message = false
	
      (* 'inhibit_pipelining_byserver': becomes true if the server is able
       * to keep persistent connections but is not able to use pipelining.
       * (HTTP/1.0 "keep alive" connections)
       *)
      val mutable inhibit_pipelining_byserver = no_pipelining
 
(*
      (* Proxy authorization: If 'proxy_user' is non-empty, the variables
       * 'proxy_user' and 'proxy_password' are interpreted as user and
       * password for proxy authentication. More precisely, once the proxy
       * responds with status code 407, 'proxy_credentials_required' becomes
       * true, and all following requests will include the credentials identifying
       * the user (with the "basic" authentication method).
       * If the proxy responds again with code 407, this reaction will not
       * be handled again but will be visible to the outside.
       *)
      val mutable proxy_credentials_required = false
      val mutable proxy_cookie = ""
 *)

      (* Whether this connection never proves to exchange a message: *)
      val mutable total_failure = false

      (* 'close_connection' indicates that a HTTP/1.0 response was received or 
       * that a response contained a 'connection: close' header.
       *)
      val mutable close_connection = false
      
      (* whether the [after_eof] cleanup has already been done (must only happen
	 once
       *)
      val mutable after_eof = false

      (* The vars govern whether there is an engine writing/reading *)
      val mutable drive_output_active = None
      val mutable drive_input_active = None

      
      method length =
	(* Returns the number of open requests (requests without response) *)
	Q.length read_queue


      method active =
	(* Whether something is to be done *)
	self # length > 0


      method iter_unserved_messages f =
	(* Call f with all unserved messages *)
	Queue.iter (fun trans -> f trans) read_queue


      method is_total_failure = total_failure


      method no_pipelining = inhibit_pipelining_byserver


      method finished_e = finished_e
	(* This engine transitions to [`Done()] when the
	   connection is finished, and all messages are processed.

	   At this point, the file descriptor is closed or given back
	   to the cache. The read and write queues are unmodified, and
	   can be used to recover.

	   Errors are reported via the messages.
	 *)


      method add urgent m f_done =
	(* add: adds to the read_queue/write_queue. This must be possible
	   at any time - even after shutting down the socket and stopping
	   any event processing.
	 *)
	(* urgent: whether to insert the message m into the front-most place
	   (to be used after autorization)
	 *)

	if !options.verbose_connection then
	  dlogr (fun () -> 
		   sprintf "HTTP Connection: adding call %d"
		     (Oo.id m));
	
	(* Create the transport container for the message and add it to the
	 * queues:
	 *)
	let trans = 
	  transmitter peer_is_proxy proxy_auth_state m f_done options in
	
(* (* would not work, so leave disabled *)
	if !proxy_auth_state = `None then (
	  match proxy_auth_handler_opt with
	    | None -> ()
	    | Some ah ->
		(* enable in-advance authentication *)
		( match ah # create_proxy_session m with
		    | None -> ()
		    | Some sess ->
			proxy_auth_state := `In_advance sess
		)
	);
 *)

	(* Initialize [trans] for transmission: *)
	trans # init();
	
	let n = Queue.length write_queue in
	if urgent && n > 0 then (
	  (* Insert [trans] at the ealierst possible place *)
	  Q.add_at (n-1) trans write_queue;
	  Q.add_at (n-1) trans read_queue;
	)
	else (
	  Q.add trans write_queue;
	  Q.add trans read_queue;
	)

      method attach() =
	(* Enables event processing. To be called after [add] *)
	sending_first_message <- true;
	done_first_message <- false;
	close_connection <- false;

(* FIXME: reset other vars? *)

	io # configure_read 
	  ~fetch_call:self#drive_input_fetch ();
	self # drive_input();

	if !options.verbose_events then
	  dlog (sprintf 
		  "FD %Ld - HTTP events: config input"
		  (Netsys.int64_of_file_descr fd));
	self # maintain_polling();

      method reset() =
	(* Compare also with after_eof! *)
	self # abort ~reusable:false ~count:`Crashed;

	Q.iter
	  (fun trans ->
	     trans # cleanup();    (* release resources *)
	     trans # error_if_unserved No_reply;
	     self # drive_postprocessing trans
	  )
	  read_queue;

      signal_queue `Aborted


    (**********************************************************************)
    (* End of interface.                                                  *)	
    (**********************************************************************)

    method private maintain_polling() =

      (* If one of the following conditions is true, we need not to poll
       * the write side of the socket:
       * - The write_queue is empty but the read_queue not
       * - The difference between the read and the write queue is too big
       * - We wait for the reply of the first request send to a server
       * - The write side of the socket is closed
       *)

      if !options.verbose_events then
	dlogr
	  (fun () -> 
	     sprintf "FD %s - HTTP events: maintain_polling"
	     fd_str
	  );

      if io # socket_state <> Down then (
	let actual_max_drift =
	  if inhibit_pipelining_byserver then 0 else
	    match !options.synchronization with
	      | Pipeline drift -> min drift max_pipeline
	      | _              -> 0
		  (* 0: Allow no drift if pipelining is not allowed *)
	in
	
	let have_requests =
	  Q.length read_queue - Q.length write_queue <= actual_max_drift
	  && Q.length write_queue > 0 
          && (Q.peek write_queue) # state = Unprocessed
	  && (done_first_message || sending_first_message) in

	(* Note: sending_first_message is true while the first request is sent.
         * done_first_message becomes true when the response arrived. In the
         * meantime both variables are false, and nothing is sent.
         *)

	let do_release =
	  (io # socket_state = Up_rw &&
	      Q.length read_queue = 0 && Q.length write_queue = 0) in
	(* If the socket is still up, we must send EOF. Normally, this 
         * should have happened already, just to be sure we check this here.
	 *)
	
	let do_close_output = close_connection in
	(* close_connection: this is set in update_characteristics after
	 * receiving the first response 
	 *)

	if !options.verbose_events then
	  dlogr
	    (fun () -> 
	       sprintf "FD %s - HTTP events: maintain_polling \
                        n_read=%d n_write=%d \
                        actual_max_drift=%d have_requests=%B \
                        do_close_output=%B do_release=%B"
		 fd_str
		 (Q.length read_queue) (Q.length write_queue)
		 actual_max_drift have_requests do_close_output do_release
	    );
	
	if drive_output_active = None && io#socket_state = Up_rw then (
	  if !options.verbose_events then
	    dlogr
	      (fun () -> 
		 sprintf "FD %s - HTTP events: config output=%s"
		   fd_str
		   (if do_close_output then "close" else
		      if do_release then "release" else
			if have_requests then "enabled" else
			  "none"
		   )
	      );
	  if do_close_output then
	    self # close_output()
	  else
	    if do_release then
	      self # release_io ()
	    else
	      if have_requests then
		self # drive_output()
	)
      )

    method private abort ~reusable ~count =
      (* This method is called when the connection is in a final (maybe
       *  errorneous) state, and the protocol handler decides to stop
       * processing.
       * 
       * reusable: whether it is possible to reuse this connection later
       *)
      if io # socket_state <> Down then (
	if !options.verbose_connection then 
	  dlog (sprintf 
		  "FD %s - HTTP connection: Shutdown!"
		  fd_str);
	let followup() =
	  signal_connection(`Done());
	  if !options.verbose_events then
	    dlogr
	      (fun () ->
		 sprintf "FD %s - HTTP event: Connection processing done" 
		   fd_str) in

	begin match io#socket_state with
	    Down -> 
	      assert false
	  | Up_r -> 
	      io # close
		~followup ()
	  | Up_rw ->
	      if reusable then (
		( try
		    conn_cache # set_connection_state fd peer (`Inactive cb);
		    io # down();
		  with
		    | Not_found ->
			(* We can do an orderly shutdown: *)
			io # close ~followup:(fun () -> ()) ()
		);
		followup()
	      )
	      else
		io # close
		  ~followup ()
	end;
	( match count with
	    | `Timed_out ->
		counters.timed_out_connections <- 
		  counters.timed_out_connections + 1;
	    | `Crashed ->
		counters.crashed_connections <- 
		  counters.crashed_connections + 1;
	    | `Server_eof ->
		counters.server_eof_connections <- 
		  counters.server_eof_connections + 1;
	    | `Successful ->
		counters.successful_connections <- 
		  counters.successful_connections + 1
	    | `Failed ->
		(* By definition of the counter, an abort cannot be failed *)
		assert false;
	);
	if !options.verbose_events then
	  dlog (sprintf "FD %s - HTTP events: reset after shutdown"
		  fd_str);

	self # cancel_output();   (* FIXME: check whether sufficient *)
	self # cancel_input();
      )
 
    (**********************************************************************)
    (***                     THE OUTPUT HANDLER                         ***)
    (**********************************************************************)

    method private drive_output() =
      if !options.verbose_events then
	dlogr (fun() -> 
		 sprintf "FD %s - HTTP events: drive_output" fd_str);

      let can_output =
	drive_output_active = None &&
	io#socket_state = Up_rw &&
	not(Q.is_empty write_queue) in

      assert(can_output);

      if can_output then (
	let trans = Q.peek write_queue in
	assert(trans#state = Unprocessed);

	(* Clear this flag now because we are about to really send a new
	   request
	 *)
	trans # message # private_api # set_retry_anyway false;

	let close_flag =
	  !options.inhibit_persistency in

	let handshake_cfg =
	  try
	    (* Proper parsing not required, because [Expect] is
             * set by the user.
             * [continue]: Already seen status 100
             *)
	    let rh = trans # message # request_header `Effective in
	    if (not (trans # message # private_api # continue) &&
		  String.lowercase(rh # field "expect") = "100-continue")
	    then Some !options.handshake_timeout
	    else None
	  with
	    | Not_found -> None
	in

	let e =
	  trans # send_e io handshake_cfg close_flag esys
	  >> (fun st ->
		drive_output_active <- None;
		if !options.verbose_events then
		  dlogr (fun() -> 
			   sprintf "FD %s - HTTP events: done with output \
                                    state=%s"
			     fd_str (string_of_state trans#state));
		match st with
		  | `Done () ->
		      assert
			(trans#state = Sent_request || trans#state = Complete
			  || trans#state = Handshake
			    || trans#send_interrupted
			);
		      sending_first_message <- false;
		      ignore (Q.take write_queue);
		      self # maintain_polling();
		      st
		  | `Error err ->
		      if !options.verbose_connection then
			dlogr
			  (fun () ->
			     sprintf "FD %s - HTTP connection: Exception %s"
			       fd_str (Netexn.to_string err));
		      self # abort ~reusable:false ~count:`Crashed;
		      self # after_eof (Some err);
		      st
		  | `Aborted -> 
		      if !options.verbose_connection then
			dlogr
			  (fun () ->
			     sprintf "FD %s - HTTP connection: Aborting"
			       fd_str
			  );
		      st
	     ) in

	drive_output_active <- Some e;
      )

    method private close_output() =
      if !options.verbose_events then
	dlogr (fun() -> 
		 sprintf "FD %s - HTTP events: close_output" fd_str);

      let can_close =
	drive_output_active = None &&
	io#socket_state = Up_rw in

      assert(can_close);

      if can_close then (
	assert (not(io # write_activity));      
	io # add Send_eof;
	let e =
	  io # write_e esys
	  >> (fun st ->
		drive_output_active <- None;
		if !options.verbose_events then
		  dlogr (fun() -> 
			   sprintf "FD %s - HTTP events: close_output done"
			     fd_str);
		match st with
		  | `Done () -> 
		      (* The connection closure can be driven by several
			 reasons, so there can still be messages on the
			 queues!
		       *)
		      self # abort ~reusable:false ~count:`Successful;
		      self # after_eof None;
		      st
		  | `Error err ->
		      if !options.verbose_connection then
			dlogr
			  (fun () ->
			     sprintf "FD %s - HTTP connection: Exception %s"
			       fd_str (Netexn.to_string err));
		      self # abort ~reusable:false ~count:`Crashed;
		      self # after_eof (Some err);
		      st
		  | `Aborted -> 
		      if !options.verbose_connection then
			dlogr
			  (fun () ->
			     sprintf "FD %s - HTTP connection: Aborting"
			       fd_str
			  );
		      st
	     )
	in
	drive_output_active <- Some e;
      )


    method private cancel_output() =
      match drive_output_active with
	| None -> ()
	| Some e -> 
	    drive_output_active <- None; e # abort()
      

    method private release_io() =
      (* Give fd/mplex back to the connection cache *)
      assert(io # socket_state = Up_rw);
      self # abort ~reusable:true ~count:`Successful;
      self # after_eof None
	(* FIXME: check whether sufficient *)


    (**********************************************************************)
    (***                     THE INPUT HANDLER                          ***)
    (**********************************************************************)

    method private drive_input() =
      
      let rec read_loop_e() =
	io # read_e esys
	++ (fun call_opt ->
	      match call_opt with
		| None ->
		    if !options.verbose_connection then
		      dlogr
			(fun () ->
			   sprintf "FD %s - HTTP connection: Got EOF!"  fd_str);
		    (* The sender may be still writing, so try to stop it *)
		    if Q.length write_queue > 0 then
		      (Queue.peek write_queue) # send_interrupt();
		    self # abort ~reusable:false ~count:`Server_eof;
		    self # after_eof None;
		    eps_e (`Done()) esys
		| Some call ->
		    if !options.verbose_connection then
		      dlogr
			(fun () ->
			   sprintf "FD %s - HTTP connection: Got Call %d!" 
			     fd_str (Oo.id call));
		    let trans =
		      try Q.peek read_queue
		      with Q.Empty -> assert false in
		    assert(trans#message = call);
		    (* It is possible that the write is still ongoing.
		       By calling send_interrupt we try to stop this,
		       but nevertheless we have to wait until the
		       writer is done! The special state Finishing
		       indicates that it is not worth-while to stop
		       sending because it will end soon anyway.

		       Note that send_finish_e may also report errors
		       of the sending side.
		     *)
		    if trans#state <> Finishing then
		      trans # send_interrupt();
		    trans # send_finish_e esys
		    ++ (fun () ->
			  trans # receive_complete();
			  self # update_characteristics trans;
			  ignore(Q.take read_queue);
			  (* Note that postprocessing may imply a
			     re-initialization of [trans]! So don't access
			     [trans] later than this point.
			   *)
			  self # postprocess_complete_message_e trans
			  ++ (fun () ->
				self # maintain_polling();
				if io#socket_state <> Down then
				  read_loop_e()
				else
				  eps_e (`Done()) esys
			     )
		       )
	   )
      in

      if !options.verbose_events then
	dlogr
	  (fun () ->
	     sprintf "FD %s - HTTP event: Starting read loop" 
	       fd_str);
      let e = 
	read_loop_e() in
      drive_input_active <- Some e;
      ignore(e >> 
	       (fun st -> 
		  drive_input_active <- None;
		  if !options.verbose_events then
		    dlogr
		      (fun () ->
			 sprintf "FD %s - HTTP event: Leaving read loop: %s" 
			   fd_str
			   ( match st with
			       | `Done _ -> "regular"
			       | `Error e -> "exception " ^ Netexn.to_string e
			       | `Aborted -> "aborted"
			   )
		      );
		  ( match st with
		      | `Error err ->
			  self # abort ~reusable:false ~count:`Crashed;
			  self # after_eof (Some err)
		      | _ -> ()
		  );
		  st
	       )
	    )

    method cancel_input() =
      match drive_input_active with
	| None -> ()
	| Some e -> 
	    drive_input_active <- None; e # abort()


    method drive_input_fetch() =
      (* This is called by io when the next status line has been received *)
      if Q.is_empty read_queue then (
	if !options.verbose_connection then (
	  dlogr
	    (fun () ->
	       sprintf 
		 "FD %s - HTTP connection: \
                  Got data of spontaneous response" fd_str);
	);
	raise Not_found
      );
      let trans = Q.peek read_queue in
      match trans # state with
	| Unprocessed ->
	    (* We get response data before we even tried to send
             * the request. We allow this, although this is weird.
             *)
	    if !options.verbose_connection then (
	      dlogr
		(fun () ->
		   sprintf 
		     "FD %s - HTTP connection: \
                     Got response before sending request" fd_str);
	    );
	    trans#message

	| Sending_hdr ->
	    (* We get response data before we finished
             * the request. We allow this for pragmatic reasons.
             *)
	    if !options.verbose_connection then (
	      dlogr
		(fun () ->
		   sprintf
		     "FD %s - HTTP connection: \
                      Got response before finishing request" fd_str)
	    );
	    trans#message

	| Handshake 
	| Sending_body ->
	    (* This is perfectly legal - we get the response after
	       we sent at least the header
	     *)
	    trans#message

	| Sent_request ->
	    (* The normal case *)
	    trans#message

	| _ ->
	    (* Should not happen *)
	    assert false
	    

    method private update_characteristics trans =
      (* After getting a response, check the type, and adapt the
	 transmission style
       *)

      let old_close_connection = close_connection in

      let able_to_pipeline = 
	trans # indicate_pipelining in
      (* able_to_pipeline: is true if we assume that the server
       * is HTTP/1.1-compliant and thus is able to manage pipelined
       * connections.
       * Update first 'close_connection': This variable becomes
       * true if the connection is not assumed to be pipelined
       * which forces that the CLIENT closes the connection 
       * immediately (this is tested in maintain_polling).
       *)
      
      let only_sequential_persistency =
	not able_to_pipeline && 
	  trans # indicate_sequential_persistency in
      (* only_sequential_persistency: is true if the connection is
       * HTTP/1.0, and the server indicated a persistent connection.
       * In this case, pipelining is disabled.
       *)
      
      if only_sequential_persistency then begin
	(* 'close_connection': not set.
	 *)
	if !options.verbose_connection then 
	  dlogr
	    (fun () ->
	       sprintf "FD %s - HTTP connection: \
                               using HTTP/1.0 style persistent connection"
		 fd_str);
	inhibit_pipelining_byserver <- true;
      end
      else
	close_connection  <- close_connection  || not able_to_pipeline;

      if !options.verbose_connection then 
	dlogr
	  (fun () ->
	     sprintf "FD %s - HTTP connection: pipelining=%B persistency=%B close_connection=%B->%B"
	       fd_str able_to_pipeline only_sequential_persistency
	       old_close_connection close_connection
	  );

      (* Remember that the first request/reply round is over: *)
      done_first_message <- true



    method private after_eof err_opt =
      (* Postprocess error information *)
      if not after_eof then (
	(* THINK: We could here process Garbage_received exceptions differently.
	   It is questionable to attribute such exceptions to specific 
	   calls.
	 *)

	(* First check if the connection was a total failure, i.e. if not
	 * even a status line was received. This is just reported to the
	 * robust_pipeline, where the counter for totally failed connections
	 * can be increased.
	 *)
	
	total_failure <- not io#status_seen;

	(* Assertions about queues: write queue is a suffix of read queue *)
	
	let n_read  = Q.length read_queue in
	let n_write = Q.length write_queue in
	assert (n_read >= n_write);
	
	(* Clean up the transmitters: *)
	
	Q.iter
	  (fun trans ->
	     trans # cleanup();    (* release resources *)
	     match err_opt with
	       | None ->
		   if total_failure then
		     trans # error_if_unserved (Bad_message "Protocol error")
	       | Some err ->
		   let err' =
		     match err with
		       | Garbage_received _ ->
			   Bad_message "Protocol error"
		       | No_reply ->
			   Bad_message "Protocol error"
		       | err -> err in
		   trans # error_if_unserved err'
	  )
	  read_queue;
	
	(* Increase the error counter of the head of read_queue: *)
	if n_read > 0 && err_opt <> None then (
	  let trans = Q.peek read_queue in
	  let m = trans # message in
	  let e = m # private_api # get_error_counter in
	  m # private_api # set_error_counter (e+1);
	);
	
	(* We have reached the logical end: *)
	signal_queue (`Done());
	if !options.verbose_events then
	  dlogr
	    (fun () ->
	       sprintf "FD %s - HTTP event: Queue processing done" 
		 fd_str);

	after_eof <- true
      )


    (**********************************************************************)
    (***                     AUTHENTICATION                             ***)
    (**********************************************************************)

    method private postprocess_complete_message_e trans =
      (* This method is invoked for every complete reply. The following
       * cases are handled at this stage of processing:
       *
       * - Status code 407: The proxy demands authorization. If the request
       *   already contains credentials for the proxy, this status code
       *   isn't handled here. Otherwise, the request is added again onto
       *   the queue, and a flag ('proxy_credentials_required') is set which 
       *   forces that the proxy credentials must be added for every new 
       *   request.
       *   Note: The necessary authentication header fields are added in
       *   the 'add' method.
       *
       * - Status code 401: XXX
       *
       * All other status codes are not handled here. Note that it is not
       * possible to react on the redirection codes because this object
       * represents the connection to exactly one server.
       * As default behaviour, the method 'postprocess' of the 
       * transmitter object is invoked; this method incorporates
       * all the intelligence not coded here.
       *)

      let default_action_e() =
	trans#cleanup();
	drive_postprocessing_e esys options trans#message trans#f_done in

      let msg = trans # message in
      let code = msg # private_api # response_code in
      let _req_hdr = msg # request_header `Effective in
      let _resp_hdr = msg # private_api # response_header in
      match code with
	| 407 when peer_is_proxy ->
	    (* --------- Proxy authorization required: ---------- *)
	    let try_again =
	      match !proxy_auth_state with
		| `None
		| `In_advance _ -> 
		    true
		| `In_reply sess ->
		    (* A previous attempt failed. *)
		    let continue = sess # invalidate msg in
		    if not continue then proxy_auth_state := `None;
		    continue
	    in
	    if try_again then (
	      match proxy_auth_handler_opt with
		| None ->
		    default_action_e()
		| Some ah ->
		    (match ah # create_proxy_session msg options with
		       | None ->
			   (* Authentication failed immediately *)
			   proxy_auth_state := `None;
			   default_action_e()
		       | Some sess ->
			   (* Remember the new session: *)
			   proxy_auth_state := `In_reply sess;
			   (* Force that even a new connection will be
			      established independently of reconnect_mode:
			    *)
			   msg # private_api # set_retry_anyway true;
			   ignore (self # add true trans#message trans#f_done);
			   eps_e (`Done()) esys
		    )
    	    )
	    else
	      default_action_e()
	| 401 ->
	    (* -------- Content server authorization required: ---------- *)
	    (* Unless a previous authentication attempt failed, just create
             * a new session, and repeat the request.
             *)
	    let try_again =
	      match msg # private_api # auth_state with
		| `None
		| `In_advance _ -> 
		    true
		| `In_reply sess ->
		    (* A previous attempt failed. *)
		    let continue = sess # invalidate msg in
		    if not continue then auth_cache # tell_failed_session sess;
		    continue
	    in
	    if try_again then (
	      match auth_cache # create_session msg options with
		| None ->
		    (* Authentication failed immediately *)
		    default_action_e()
		| Some sess ->
		    (* Remember the new session: *)
		    msg # private_api # set_auth_state (`In_reply sess);
		    (* Force that even a new connection will be
		       established independently of reconnect_mode:
		     *)
		    msg # private_api # set_retry_anyway true;
		    ignore (self # add true trans#message trans#f_done);
		    eps_e (`Done()) esys
    	    )
	    else
	      default_action_e()
	| n when n >= 200 && n < 400 ->
	    (* Check whether authentication was successful *)
	    ( match msg # private_api # auth_state with
		| `None -> ()
		| `In_advance _ -> ()
		| `In_reply session ->
		    auth_cache # tell_successful_session session
	    );
	    default_action_e()
	| _ ->
	    default_action_e()

    (**********************************************************************)
    (***    POSTPROCESS = INVOKE USER CALLBACK FUNCTION                 ***)
    (**********************************************************************)
	
    method private drive_postprocessing trans =
      trans#cleanup();
      drive_postprocessing_msg esys options trans#message trans#f_done
     
    end
  )

(**********************************************************************)

let robust_pipeline 
      esys tp cb
      peer
      proxy_auth_handler_opt
      conn_cache conn_owner 
      auth_cache
      counters options =
  (* Implements a pipeline that connects to the peer after the first
     message is added, and that is able to reconnect as often as
     necessary
   *)

  let proxy_auth_state =  ref `None in

  ( object(self)
      val mutable fp_opt = None
	(* The fragile_pipeline, if any *)

      val queue = Q.create()
	(* Queued requests before being connected *)

      (* 'connecting' may be set to [Some e] where [e] is the connecting engine.
       *)
      val mutable connecting = None
	
      (* 'connect_pause': seconds to wait until the next connection is tried.
       * There seems to be a problem with some operating systems (namely
       * Linux 2.2) which do not like immediate reconnects if the previous
       * connection did not end in a sane state.
       * A value of 0.5 seems to be sufficient for reconnects (see below).
       *)			      
      val mutable connect_pause = 0.0
	
      (* no_pipelining: true if the server has somehow indicated that it
	 is better not to rely on pipelining
       *)
      val mutable no_pipelining = false
	
      (* 'connect_time': how many seconds the last 'connect' took
       *)
      val mutable connect_time = 0.0
	
      (* If a connection does not lead to any type of response, the client 
       * simply re-opens a new connection and tries again. The following 
       * variables limit the number of attempts until the client gives up.
       *
       * 'totally_failed_connections': counts the number of failed connections
       *      without any response from the server.
       *)
      val mutable totally_failed_connections = 0
	
    method length =
      match fp_opt with
	| None -> 0
	| Some fp -> fp#length


    method active =
      match fp_opt with
	| None -> false
	| Some fp -> fp#active


    method add urgent (m : http_call) f_done =
      (* Check whether we can authenticate in advance: *)
      if m # private_api # auth_state = `None then (
	try
	  let sess = auth_cache # find_session_in_advance m in
	  m # private_api # set_auth_state (`In_advance sess)
	with
	    Not_found -> ()
      );
      match fp_opt with
	| None ->
	    Q.add (urgent,m,f_done) queue;
	    if connecting = None then
	      self # reconnect()

	| Some fp ->
	    (* Even if fp is already shut down! This only means that fp#finish_e
	       has not yet signalled that we are actually finished. This will
	       happen soon, though, and the added message will be picked up
	       then
	     *)
	    fp # add urgent m f_done


    method reset () =
      if !options.verbose_connection then
	dlog "HTTP connection: reset";

      ( match connecting with
	  | None -> ()
	  | Some e ->
	      e#abort();
	      connecting <- None
      );

      ( match fp_opt with
	  | None -> ()
	  | Some fp ->
	      fp # reset();
	      fp_opt <- None
		(* note that [fp#reset] causes that finished_e transitions
		   to `Aborted. Hence, conn_is_done is not called 

		   The remaining messages in fp are already postprocessed.
		 *)
      );

      Q.iter
	(fun (_,m,f_done) ->
	   m # private_api # error_if_unserved 
	     !options.verbose_connection No_reply;
	   self # drive_postprocessing_msg m f_done
	)
	queue;
      Q.clear queue;

      totally_failed_connections <- 0



    method private reconnect() =
      assert(connecting = None);
      assert(fp_opt = None);
      let e =
	Uq_engines.delay_engine
	  connect_pause
	  (fun () ->
	     tcp_connect_e 
	       esys tp cb peer conn_cache conn_owner options 
	       proxy_auth_handler_opt
	  )
	  esys in
      connecting <- Some e;
      Uq_engines.when_state
	~is_error:(fun err ->
		     connecting <- None;
		     self#conn_is_error err
		  )
	~is_done:(fun (fd,mplex,t) ->
		    connect_time <- t;
		    connect_pause <- 0.0;
		    connecting <- None;
		    let fp =
		      fragile_pipeline
			esys cb peer
			proxy_auth_state proxy_auth_handler_opt
			fd mplex t no_pipelining conn_cache
			auth_cache
			counters options in
		    fp_opt <- Some fp;
		    Q.iter
		      (fun (urgent,m,f_done) -> 
			 fp # add urgent m f_done
		      )
		      queue;
		    Q.clear queue;
		    fp # attach();
		    Uq_engines.when_state
		      ~is_done:(self#conn_is_done fp)
		      fp#finished_e
		 )
	e

    method private conn_is_error error =
      (* It was not possible to reach the peer *)

      counters.crashed_connections <- 
	counters.crashed_connections + 1;

      let q = Q.create() in
      Q.transfer queue q;

      self # conn_retry true error q;

    method private conn_is_done fp () =
      (* Check the result of fp, and move the requests back to [queue] that
	 can be tried again
       *)
      fp_opt <- None;
      no_pipelining <- fp#no_pipelining;  (* remember that *)

      let q = Q.create() in
      fp # iter_unserved_messages
	(fun trans ->
	   trans # cleanup();
	   Q.add (false, trans#message, trans#f_done) q
	);

      self # conn_retry fp#is_total_failure No_reply q


    method private conn_retry is_total_failure subst_error q =
      (* Maybe we have a "total failure" - got no response from server *)
      let too_many_total_failures =
	if is_total_failure then (
	  totally_failed_connections <- totally_failed_connections + 1;
	  if !options.verbose_connection then
	    dlog "HTTP connection: total failure";
	  
	  totally_failed_connections >= !options.maximum_connection_failures 
	)
	else (
	  totally_failed_connections <- 0;
	  false
	) in

      if !options.verbose_connection then
	dlog "HTTP connection: checking remaining pipeline requests";

      if too_many_total_failures then
	counters.failed_connections <- 
	  counters.failed_connections + 1;

      (* Check all requests individually *)
      Q.iter
	(fun (urgent,m,f_done) ->
	   let e = m # private_api # get_error_counter in
(*dlog (sprintf "e=%d idem=%B" e m#is_idempotent);*)
	   let try_again =
	     not too_many_total_failures &&
	       e <= !options.maximum_message_errors &&
	       (m # private_api # error_exception <> Some Response_too_large) &&
	       (m # private_api # retry_anyway ||
		  ( match m # get_reconnect_mode with
		      | Send_again -> true
		      | Request_fails -> false
		      | Send_again_if_idem -> m # is_idempotent
		      | Inquire f ->
			  (* Ask the function 'f' whether to reconnect or not: *)
			  ( try f m    (* returns true or false *)
			    with
				(* The invocation of 'f' may raise an exception.
				 * It is printed to stderr (there is no other
				 * way to report it).
				 *)
				x ->
				  dlog ("Exception caught in Http_client: " 
					^ (Netexn.to_string x));
				  false
			  )
		  )
	       ) in
	   if try_again then (
	     (* Ok, this request is tried again. *)
	     if !options.verbose_status then
	       dlogr (fun () -> 
			sprintf "Call %d - rescheduling" (Oo.id m));
	     Q.add (urgent,m,f_done) queue;
	   )
	   else (
	     m # private_api # error_if_unserved
	       !options.verbose_connection subst_error;
	     self # drive_postprocessing_msg m f_done
	   )
	)
	q;

      (* Try again: *)
      if Q.length queue > 0 then (

	if !options.verbose_connection then
	  dlog "HTTP connection: retrying after failure";
	
	connect_pause <- 1.0;
	self#reconnect()      
      )


    (**********************************************************************)
    (***    POSTPROCESS = INVOKE USER CALLBACK FUNCTION                 ***)
    (**********************************************************************)

    (* Same as for fragile_pipeline *)
	
    method private drive_postprocessing_msg m f_done =
      drive_postprocessing_msg esys options m f_done

    end
  )


(**********************************************************************)


(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(***                                                                ***)
(***                 THE PIPELINE INTERFACE                         ***)
(***                                                                ***)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

(* The following class, 'pipeline' defines the interface for the outside
 * world.
 *)

type proxy_type = [`Http_proxy | `Socks5 ] 

let parse_proxy_setting url =
  let syn = Neturl.ip_url_syntax in
  let (nu,cb) = 
    try parse_url_0 [ "http", syn, Some 80, http_cb_id ] url
    with Not_found -> failwith ("Invalid proxy URL: " ^ url) in
  let auth =
    try
      let u = Neturl.url_user nu in       (* may raise Not_found *)
      let p = Neturl.url_password nu in   (* may raise Not_found *)
      Some(u,p)
    with Not_found -> None in
  (Neturl.url_host nu, Neturl.url_port nu, auth)

let parse_no_proxy =
  split_words_by_commas


class pipeline =
  object (self)
    val mutable esys = Unixqueue.create_unix_event_system()

    val mutable proxy = ""
    val mutable proxy_port = 80
    val mutable proxy_auth = false
    val mutable proxy_user = ""
    val mutable proxy_password = ""
    val mutable proxy_type = `Http_proxy
    val tproxy = Hashtbl.create 5  (* transport-specific proxy *)

    val mutable no_proxy_for = []

    val mutable connections = Hashtbl.create 10

    val mutable open_messages = 0

    val mutable open_connections = 0

    val options =
      let http_syn =
        { (Hashtbl.find Neturl.common_url_syntax "http") with
            Neturl.url_enable_query = Neturl.Url_part_not_recognized
        } in
      let https_syn =
        { (Hashtbl.find Neturl.common_url_syntax "https") with
            Neturl.url_enable_query = Neturl.Url_part_not_recognized
        } in
      let ipp_syn =
        { (Hashtbl.find Neturl.common_url_syntax "ipp") with
            Neturl.url_enable_query = Neturl.Url_part_not_recognized
        } in
      ref
	{ (* Default values: *)
	  synchronization = Pipeline 5;
	  maximum_connection_failures = 2;
	  maximum_message_errors = 2;
	  inhibit_persistency = false;
	  connection_timeout = 300.0;
	  number_of_parallel_connections = 2;
	  maximum_redirections = 10;
	  handshake_timeout = 1.0;
	  resolver = sync_resolver;
	  configure_socket = (fun _ -> ());
	  schemes = [ "http", http_syn, Some 80, http_cb_id;
		      "https", https_syn, Some 443, https_cb_id;
		      "ipp", ipp_syn, Some 631, http_cb_id;
		    ];
	  verbose_status = true;
	  verbose_request_header = false;
	  verbose_response_header = false;
	  verbose_request_contents = false;
	  verbose_response_contents = false;
	  verbose_connection = true;
	  verbose_events = false;
	}

    val auth_cache = new auth_cache

    val mutable conn_cache = create_restrictive_cache()

    val counters =
      { new_connections = 0;
	timed_out_connections = 0;
	crashed_connections = 0;
	server_eof_connections = 0;
	successful_connections = 0;
	failed_connections = 0;
      }

    val transports = Hashtbl.create 5

    initializer (
      Hashtbl.add transports http_cb_id http_transport_channel_type;
      Hashtbl.add transports proxy_only_cb_id http_transport_channel_type;
    )

    method event_system = esys

    method set_event_system new_esys =
      esys <- new_esys;
      Hashtbl.clear connections;

    method connection_cache = conn_cache

    method set_connection_cache cc = conn_cache <- cc

    method add_authentication_method ( m : auth_method ) =
      self # add_auth_handler (m # as_auth_handler)

    method add_auth_handler (h : auth_handler) =
      auth_cache # add_auth_handler h

    method set_proxy the_proxy the_port =
      (* proxy="": disables proxy *)
      proxy       <- the_proxy;
      proxy_port  <- the_port;
      proxy_type  <- `Http_proxy;
      ()

    method set_proxy_auth user passwd =
      (* sets 'user' and 'password' if demanded by a proxy *)
      proxy_auth     <- user <> "";
      proxy_user     <- user;
      proxy_password <- passwd


    method avoid_proxy_for l =
      (* l: List of hosts or domains *)
      no_proxy_for <- l


    method set_proxy_from_environment() =
      (* Is the environment variable "http_proxy" set? *)
      let http_proxy =
	try Sys.getenv "http_proxy" with Not_found -> "" in
      if http_proxy <> "" then (
	let (host,port,auth) = parse_proxy_setting http_proxy in
	self # set_proxy host port;
	match auth with
	  | None -> ()
	  | Some(u,p) ->
	      self # set_proxy_auth u p
      );

      (* Is the environment variable "no_proxy" set? *)
      let no_proxy =
	try Sys.getenv "no_proxy" with Not_found -> "" in
      let no_proxy_list =
	parse_no_proxy no_proxy in
      self # avoid_proxy_for no_proxy_list;


    method set_transport_proxy_from_environment l =
      List.iter
	(fun (var_name, cb_id) ->
	   let var =
	     try Sys.getenv var_name with Not_found -> "" in
	   if var <> "" then (
	     let (host,port,auth) = parse_proxy_setting var in
	     self # set_transport_proxy cb_id host port auth `Http_proxy
	   );
	)
	l;

      (* Is the environment variable "no_proxy" set? *)
      let no_proxy =
	try Sys.getenv "no_proxy" with Not_found -> "" in
      let no_proxy_list =
	parse_no_proxy no_proxy in
      self # avoid_proxy_for no_proxy_list
      

    method set_socks5_proxy host port =
      proxy       <- host;
      proxy_port  <- port;
      proxy_type  <- `Socks5


    method configure_transport (cb:int) (tp:transport_channel_type) =
      Hashtbl.replace transports cb tp

    method set_transport_proxy cb host port auth (pt:proxy_type) =
      Hashtbl.replace tproxy cb (host,port,auth,pt)

    method reset () =
      (* deletes all pending requests; closes connection *)

      (* Reset all connections: *)
      Hashtbl.iter
	(fun _ cl ->
	   List.iter
	     (fun c ->
		c # reset())
	     !cl)
	connections;

(*
   - well, this _should_ do nothing
      List.iter
	(fun fd -> 
	   conn_cache # forget_connection fd;
	   Unix.close fd;
	)
	(conn_cache # find_my_connections (self :> < >));
 *)

      self # reset_counters()


    method private peer_of_call request =
      request # private_api # parse_request_uri options;

      let host = request # get_host() in
      let port = request # get_port() in
      let cb = request # private_api # channel_binding options in

      let peer_of_proxy h p pt =
      	match pt with
	  | `Http_proxy ->
	      if request # proxy_use_connect then
		`Http_proxy_connect((h,p),(host,port))
	      else
		`Http_proxy(h,p)
	  | `Socks5 ->
	      `Socks5((h,p),(host,port)) in

      let proxy_possible =
	request # proxy_enabled &&
	  (not
             (List.exists
		(fun dom ->
		   if dom <> "" &&
                     dom.[0] = '.' &&
		     String.length host > String.length dom
		   then
                     let ld = String.length dom in
                     String.lowercase(String.sub 
					host 
					(String.length host - ld) 
					ld)
                     = String.lowercase dom
		   else
                     dom = host)
		no_proxy_for
	     )) in
      try
	if proxy_possible then ( 
	  try
	    let (h,p,a,pt) = Hashtbl.find tproxy cb in
	    (peer_of_proxy h p pt, a)
	  with
	    | Not_found ->
		if proxy = "" then raise Not_found;
		(peer_of_proxy proxy proxy_port proxy_type, 
		 if proxy_auth then
		   Some(proxy_user, proxy_password)
		 else
		   None
		)
	)
	else raise Not_found
      with Not_found ->
	(`Direct(host,port), None)


    method proxy_type_of_call request =
      let peer, auth = self # peer_of_call request in
      match peer with
	| `Http_proxy _ -> Some `Http_proxy
	| `Http_proxy_connect _ -> Some `Http_proxy
	| `Socks5 _ -> Some `Socks5
	| `Direct _ -> None


    method proxy_type url : proxy_type option =
      let request = new get url in  (* pseudo request *)
      self # proxy_type_of_call request


    method channel_binding (req : http_call) =
      req # private_api # channel_binding options


    method private add_with_callback_no_redirection (request : http_call) f_done =

      let cb = request # private_api # channel_binding options in

      (* find out the effective peer: *)
      let peer, auth = self # peer_of_call request in
      let use_proxy =
	match peer with
	  | `Direct _ -> false
	  | _ -> true in

      (* Find out if there is already a connection to this peer: *)

      let conn = 
	let connlist = 
	  try
	    Hashtbl.find connections (peer, cb) 
	  with
	      Not_found ->
		let new_connlist = ref [] in
		Hashtbl.add
		  connections (peer, cb) new_connlist;
		new_connlist
	in
	if List.length !connlist < !options.number_of_parallel_connections 
	  then begin
	    let tp =
	      try 
		if cb = proxy_only_cb_id && not use_proxy then raise Not_found;
		Hashtbl.find transports cb
	      with Not_found ->
		failwith "Http_client: No transport for this channel binding" in
	    let proxy_auth_handler_opt =
	      match auth with
		| Some(u,p) ->
		    let kh = new proxy_key_handler u p in
		    Some(new unified_auth_handler kh)
		| None -> 
		    None in
	    let new_conn = robust_pipeline
	                     esys tp cb
	                     peer
	                     proxy_auth_handler_opt
			     conn_cache
			     (self :> < >)
			     auth_cache
			     counters
			     options in
	    open_connections <- open_connections + 1;
	    counters.new_connections <- counters.new_connections + 1;
	    connlist := new_conn :: !connlist;
	    new_conn
	  end 
	  else begin
	    (* Find the connection with the lowest number of queue entries: *)
	    List.fold_left
	      (fun best_conn a_conn ->
		 if a_conn # length < best_conn # length then
		   a_conn
		 else
		   best_conn)
	      (List.hd !connlist)
	      (List.tl !connlist)
	  end
      in
      
      (* Add the request to the queue of this connection: *)

      conn # add false request 
	(fun m ->
	   (* Update 'open_connections', 'connections', and 'open_messages' *)
	   if not conn#active then begin
	     (* Check whether the connection is still in the [connections]
              * hash. It is possible that it is already deleted here.
              *)

	     let connlist =
	       try
		 Hashtbl.find connections (peer, cb);
	       with
		   Not_found -> ref []
	     in
	     if List.exists (fun c -> c == conn) !connlist then (
	       open_connections <- open_connections - 1;
	       connlist := List.filter (fun c -> c != conn) !connlist;
	       if !connlist = [] then
		 Hashtbl.remove connections (peer, cb);
	     )
	   end;
	   self # update_open_messages;
	   (* Do user action: *)
	   f_done m;
	);

      open_messages <- open_messages + 1;

    method private update_open_messages =
      open_messages <- 0;
      Hashtbl.iter
	(fun _ cl ->
	   List.iter
	     (fun c ->
		if c # active then 
		  open_messages <- open_messages + (c # length))
	     !cl)
	connections;


    method add_with_callback (request : http_call) f_done =
      request # private_api # parse_request_uri options;
      self # add_with_callback_no_redirection
	request
	(fun m ->
	   try
	     let (_,code,_) = m # dest_status() in
	     match code with
		 (301|302) ->
		   (* Simply repeat the request with a different URI *)
		   let do_redirection =
		     match m # get_redirect_mode with
			 Redirect -> true
		       | Do_not_redirect -> false
		       | Redirect_if_idem -> m # is_idempotent
		       | Redirect_inquire f ->
			   (* Ask the function 'f' whether to redirect: *)
			   begin 
			     try f m    (* returns true or false *)
			     with
			     (* The invocation of 'f' may raise an exception.
			      * It is printed to stderr (there is no other
			      * way to report it).
			      *)
				 x ->
				   dlog (sprintf 
					   "Call %d - \
                                            Exception caught in Http_client %s"
					   (Oo.id m)
					   (Netexn.to_string x));
				   false
			   end
		   in

		   if do_redirection then begin
		     (* Maybe the redirection limit is exceeded: *)
		     let rc = m # private_api # get_redir_counter in
		     if rc >= !options.maximum_redirections
		     then (
		       m # private_api # set_error_exception Too_many_redirections;
		       f_done m
		     )
		     else (
		       let location = m # assoc_resp_header "location" in
		       (* or raise Not_found *)
		       let location' =
			 if location <> "" && location.[0] = '/' then
			   (* Problem: "Location" header must be absolute due
			    * to RFC specs. Now it is relative (with full path).
			    * Workaround: Interpret relative to old server
			    *)
			   ( try
			       let (nu,_) =
				 parse_url 
				   ~base_url:(m # private_api # request_uri_with
					      ())
				   options
				   location in
			       Neturl.string_of_url nu
			     with _ -> location
			   )
			 else
			   location in
		       let ok =
			 try
			   m # set_request_uri location';
			   true
			 with
			   | _ ->
			       (* Bad URL! *)
			       let e = URL_syntax_error location' in
			       m # private_api # set_error_exception e;
			       false
		       in
		       if ok then (
			 m # private_api # set_redir_counter (rc+1);
			 m # private_api # set_error_counter 0;
			 self # add_with_callback m f_done
		       )
		       else f_done m
		     )
		   end
		     else f_done m

	       | _ -> 
		   f_done m
	     with
		 (Http_protocol _ | Not_found) -> 
		   f_done m
	)


    method add request =
      self # add_with_callback request (fun _ -> ())

    method add_e request =
      let (e, signal) = Uq_engines.signal_engine esys in
      self # add_with_callback request (fun c -> signal (`Done c));
      e

    method run () =
	 Unixqueue.run esys

    method get_options = !options

    method set_options p =
      options := p

    method number_of_open_messages = open_messages

    method number_of_open_connections = open_connections

    method connections =
      let l = ref [] in
      Hashtbl.iter
	(fun (peer, _) conns ->
	   List.iter
	     (fun conn ->
		let host, port = first_hop peer in
		l := (host, port, conn#length) :: !l
	     )
	     !conns
	)
	connections;
      !l

    method cnt_new_connections = counters.new_connections

    method cnt_timed_out_connections = counters.timed_out_connections

    method cnt_crashed_connections = counters.crashed_connections

    method cnt_server_eof_connections = counters.server_eof_connections

    method cnt_successful_connections = counters.successful_connections

    method cnt_failed_connections = counters.failed_connections

    method reset_counters() =
      counters.new_connections <- 0;
      counters.timed_out_connections <- 0;
      counters.crashed_connections <- 0;
      counters.server_eof_connections <- 0;
      counters.successful_connections <- 0;
      counters.failed_connections <- 0;


  end
;;

(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(***                                                                ***)
(***                 THE CONVENIENCE MODULE                         ***)
(***                                                                ***)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

(* This module is intended for beginners and for simple applications
 * of this HTTP implementation.
 *)

let omtp = !Netsys_oothr.provider
let mutex = omtp # create_mutex()

let serialize f arg =
  mutex # lock();
  try
    let r = f arg in
    mutex # unlock();
    r
  with
      err -> mutex # unlock(); raise err
;;
	

module Convenience =
  struct

    let http_trials = ref 3
    let http_user = ref ""
    let http_password = ref ""

    let this_user = ref ""
    let this_password = ref ""

    let conv_verbose = ref false

    class simple_key_handler : key_handler =
    object
      method inquire_key ~domain ~realms ~auth =
	if !this_user <> "" then
	  ( object
	      method user = !this_user
	      method password = !this_password
	      method realm = List.hd realms 
	      method domain = domain
	    end )
	else
	  if !http_user <> "" then
	    ( object
		method user = !http_user
		method password = !http_password
		method realm = List.hd realms 
		method domain = domain
	      end )
	  else
	    raise Not_found
      method invalidate_key (_ : key) = ()
    end


    let auth_basic =
      new basic_auth_handler 
	~enable_auth_in_advance:true (new simple_key_handler)

    let auth_digest =
      new digest_auth_handler 
	~enable_auth_in_advance:true (new simple_key_handler)

    let get_default_pipe() =

      let p = new pipeline in

      p # set_proxy_from_environment();

      (* Add authentication methods: *)
      p # add_auth_handler auth_basic;
      p # add_auth_handler auth_digest;

      (* That's it: *)
      p


    let pipe = lazy (get_default_pipe())
    let pipe_empty = ref true


    let configure_pipeline f =
      let p = Lazy.force pipe in
      f p

    let request m =
      serialize
	(fun trials ->
	   let p = Lazy.force pipe in
	   m # set_accept_encoding();
	   p # add m;
	   try
	     p # run()
	   with
	     | error -> p # reset(); raise error
	)

    let prepare_url =
      serialize
	(fun url ->
	   let p = Lazy.force pipe in
	   try
	     this_user := "";
	     let (nu, _) = parse_url (ref p#get_options) url in
	     if Neturl.url_provides ~user:true nu then (
	       this_user := Neturl.url_user nu;
	       this_password := "";
	       if Neturl.url_provides ~password:true nu then
		 this_password := Neturl.url_password nu;
	     );
	     Neturl.string_of_url
	       (Neturl.remove_from_url
		  ~user:true ~user_param:true ~password:true
		  nu)
	   with
	     | Not_found | Neturl.Malformed_URL -> 
		 url
	)

    let http_get_message url =
      let m = new get (prepare_url url) in
      request m !http_trials;
      m

    let http_get url = (http_get_message url) # get_resp_body()

    let http_head_message url =
      let m = new head (prepare_url url) in
      request m !http_trials;
      m

    let http_post_message url params =
      let m = new post (prepare_url url) params in
      request m 1;
      m

    let http_post url params = (http_post_message url params) # get_resp_body()

    let http_put_message url content =
      let m = new put (prepare_url url) content in
      request m !http_trials;
      m

    let http_put url content = (http_put_message url content) # get_resp_body()

    let http_delete_message url =
      let m = new delete (prepare_url url) in
      request m 1;
      m

    let http_delete url = (http_delete_message url) # get_resp_body()


    let http_verbose 
         ?(verbose_status=true)
         ?(verbose_request_header=true)
         ?(verbose_response_header=true)
         ?(verbose_request_contents=true)
         ?(verbose_response_contents=true)
         ?(verbose_connection=true)
         ?(verbose_events=true) =
      serialize
	(fun () ->
	   let p = Lazy.force pipe in
	   let opt = p # get_options in
	   p # set_options
	     { opt with verbose_status = verbose_status;
	         verbose_request_header = verbose_request_header;
		 verbose_response_header = verbose_response_header;
		 verbose_request_contents = verbose_request_contents;
		 verbose_response_contents = verbose_response_contents;
		 verbose_connection = verbose_connection;
		 verbose_events = verbose_events
             };
	   conv_verbose := true;
	   Debug.enable := true;
	)
  end
