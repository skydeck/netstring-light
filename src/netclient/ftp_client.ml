(* $Id: ftp_client.ml 1692 2012-02-05 18:44:00Z gerd $ *)

open Telnet_client
open Ftp_data_endpoint
open Printf
open Uq_engines.Operators   (* ++, >>, eps_e *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Ftp_client" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Ftp_client" Debug.enable

let () =
  Netlog.Debug.register_module "Ftp_client" Debug.enable


exception FTP_error of exn
exception FTP_protocol_violation of string
exception FTP_timeout of string

let proto_viol s =
  raise(FTP_protocol_violation s)

let () =
  Netexn.register_printer
    (FTP_error Not_found)
    (fun e ->
       match e with
	 | FTP_error e' ->
	     "Ftp_client.FTP_error(" ^ Netexn.to_string e' ^ ")"
	 | _ -> assert false
    )


type cmd_state =
    [ `Not_connected
    | `Init
    | `Success
    | `Proto_error
    | `Temp_failure
    | `Perm_failure
    | `Rename_seq
    | `Restart_seq
    | `User_pass_seq
    | `User_acct_seq
    | `Pass_acct_seq
    | `Preliminary
    ]

type port =
    [ `Active of string * int * Unix.file_descr
    | `Passive of string * int
    | `Ext_active of string * int * Unix.file_descr
    | `Ext_passive of int
    | `Unspecified
    ]

type form_code =
    [ `Non_print | `Telnet | `ASA ]

type representation =
    [ `ASCII of form_code option
    | `EBCDIC of form_code option
    | `Image
    ]

type structure =
    [ `File_structure
    | `Record_structure
    ]

type transmission_mode =
    Ftp_data_endpoint.transmission_mode

type ftp_state =
    { cmd_state : cmd_state;
      ftp_connected : bool;
      ftp_data_conn : bool;
      ftp_user : string option;
      ftp_password : string option;
      ftp_account : string option;
      ftp_logged_in : bool;
      ftp_port : port;
      ftp_repr : representation;
      ftp_structure : structure;
      ftp_trans : transmission_mode;
      ftp_dir : string list;
      ftp_features : (string * string option) list option;
      ftp_options : (string * string option) list;
    }


type cmd =
    [ `Connect of string * int
    | `Disconnect
    | `Dummy
    | `USER of string
    | `PASS of string
    | `ACCT of string
    | `CWD of string
    | `CDUP
    | `SMNT of string
    | `QUIT
    | `REIN
    | `PORT
    | `PASV
    | `TYPE of representation
    | `STRU of structure
    | `MODE of transmission_mode
    | `RETR of string * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `STOR of string * (ftp_state -> Ftp_data_endpoint.local_sender)
    | `STOU of (ftp_state -> Ftp_data_endpoint.local_sender)
    | `APPE of string * (ftp_state -> Ftp_data_endpoint.local_sender)
    | `ALLO of int * int option
    | `REST of string
    | `RNFR of string
    | `RNTO of string
    | `DELE of string
    | `RMD of string
    | `MKD of string
    | `PWD
    | `LIST of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `NLST of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `SITE of string
    | `SYST
    | `STAT of string option
    | `HELP of string option
    | `NOOP
    | `FEAT
    | `OPTS of string * string option
    | `EPRT 
    | `EPSV of [ `AF of Unix.socket_domain | `ALL ] option
    | `LANG of string option
    | `MDTM of string
    | `SIZE of string
    | `MLST of string option
    | `MLSD of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    ]

let string_of_cmd_nolf =
  function
    | `Connect _ -> ""
    | `Disconnect -> ""
    | `Dummy -> ""
    | `USER s -> "USER " ^ s
    | `PASS s -> "PASS " ^ s
    | `ACCT s -> "ACCT " ^ s
    | `CWD s  -> "CWD " ^ s
    | `CDUP   -> "CDUP"
    | `SMNT s -> "SMNT " ^ s
    | `QUIT   -> "QUIT"
    | `REIN   -> "REIN"
    | `PORT   -> assert false   (* not done here *)
    | `PASV   -> "PASV"
    | `TYPE t -> "TYPE " ^ 
	         ( match t with
		     | `ASCII None -> "A"
		     | `ASCII (Some `Non_print) -> "A N"
		     | `ASCII (Some `Telnet) -> "A T"
		     | `ASCII (Some `ASA) -> "A C"
		     | `EBCDIC None -> "E"
		     | `EBCDIC (Some `Non_print) -> "E N"
		     | `EBCDIC (Some `Telnet) -> "E T"
		     | `EBCDIC (Some `ASA) -> "E C"
		     | `Image -> "I"
		 )
    | `STRU `File_structure -> "STRU F"
    | `STRU `Record_structure -> "STRU R"
    | `MODE `Stream_mode -> "MODE S"
    | `MODE `Block_mode -> "MODE B"
    | `RETR (s,_) -> "RETR " ^ s
    | `STOR (s,_) -> "STOR " ^ s
    | `STOU _     -> "STOU\r\n"
    | `APPE (s,_) -> "APPE " ^ s
    | `ALLO(n,r)  -> "ALLO " ^ string_of_int n ^ 
	             (match r with
			| None -> ""
			| Some m -> " R " ^ string_of_int m)
    | `REST s -> "REST " ^ s
    | `RNFR s -> "RNFR " ^ s
    | `RNTO s -> "RNTO " ^ s
    | `DELE s -> "DELE " ^ s
    | `RMD  s -> "RMD " ^ s
    | `MKD  s -> "MKD " ^ s
    | `PWD    -> "PWD"
    | `LIST(None,_) -> "LIST"
    | `LIST(Some s,_) -> "LIST " ^ s
    | `NLST(None,_) -> "NLST"
    | `NLST(Some s,_) -> "NLST " ^ s
    | `SITE s -> "SITE " ^ s
    | `SYST   -> "SYST"
    | `STAT None -> "STAT"
    | `STAT(Some s) -> "STAT " ^ s
    | `HELP None -> "HELP"
    | `HELP(Some s) -> "HELP " ^ s
    | `NOOP -> "NOOP"
    | `FEAT -> "FEAT"
    | `OPTS (cmd,None) -> "OPTS " ^ cmd
    | `OPTS (cmd,Some param) -> "OPTS " ^ cmd ^ " " ^ param
    | `EPRT -> "EPRT"
    | `EPSV None -> "EPSV"
    | `EPSV (Some (`AF dom)) ->
	let n =
	  match dom with
	    | Unix.PF_INET -> 1
	    | Unix.PF_INET6 -> 2
	    | _ -> failwith "no such address family" in
	"EPSV " ^ string_of_int n
    | `EPSV (Some `ALL) -> "EPSV ALL"
    | `LANG None -> "LANG"
    | `LANG (Some tok) -> "LANG " ^ tok
    | `MDTM s -> "MDTM " ^ s
    | `SIZE s -> "SIZE " ^ s
    | `MLST None -> "MLST"
    | `MLST (Some n) -> "MLST " ^ n
    | `MLSD (None, _) -> "MLSD"
    | `MLSD (Some n,_) -> "MLSD " ^ n


let string_of_cmd =
  function
    | `Connect _ -> ""
    | `Disconnect -> ""
    | `Dummy -> ""
    | cmd -> string_of_cmd_nolf cmd ^ "\r\n"

let pasv_re = 
  Netstring_str.regexp 
    ".*[^0-9]\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),\
             \\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\)"

let extract_pasv s =
  match Netstring_str.string_match pasv_re s 0 with
    | None ->
	proto_viol "Cannot parse specification of passive port"
    | Some m ->
	let h1 = Netstring_str.matched_group m 1 s in
	let h2 = Netstring_str.matched_group m 2 s in
	let h3 = Netstring_str.matched_group m 3 s in
	let h4 = Netstring_str.matched_group m 4 s in
	let p1 = Netstring_str.matched_group m 5 s in
	let p2 = Netstring_str.matched_group m 6 s in
	let p = int_of_string p1 * 256 + int_of_string p2 in
	(h1 ^ "." ^ h2 ^ "." ^ h3 ^ "." ^ h4, p)


let epsv_re =
  Netstring_str.regexp
    ".*(\\([^)]+\\))"

let extract_epsv s =
  try
    match Netstring_str.string_match epsv_re s 0 with
      | None ->
	  raise Not_found
      | Some m ->
	  let p = Netstring_str.matched_group m 1 s in
	  if String.length p < 5 then
	    raise Not_found;
	  let d = p.[0] in
	  if p.[1] <> d || p.[2] <> d || p.[String.length p - 1] <> d then
	    raise Not_found;
	  let u = String.sub p 3 (String.length p - 4) in
	  let n = try int_of_string u with _ -> raise Not_found in
	  if n < 1 || n > 65535 then raise Not_found;
	  n
  with
    | Not_found ->
	proto_viol "Cannot parse specification of extended passive port"

let addr_re = 
  Netstring_str.regexp
    "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$"

let format_port (addr,p) =
  match Netstring_str.string_match addr_re addr 0 with
    | None ->
	failwith "Bad IP address"
    | Some m ->
	let h1 = Netstring_str.matched_group m 1 addr in
	let h2 = Netstring_str.matched_group m 2 addr in
	let h3 = Netstring_str.matched_group m 3 addr in
	let h4 = Netstring_str.matched_group m 4 addr in
	let p1 = string_of_int(p lsr 8) in
	let p2 = string_of_int(p land 0xff) in
	h1 ^ "," ^ h2 ^ "," ^ h3 ^ "," ^ h4 ^ "," ^ p1 ^ "," ^ p2


let format_eprt (addr,p) =
  let ip = Unix.inet_addr_of_string addr in
  let dom = Netsys.domain_of_inet_addr ip in
  let af = 
    match dom with
      | Unix.PF_INET -> 1
      | Unix.PF_INET6 -> 2
      | _ -> assert false in
  sprintf "|%d|%s|%d|" af addr p

let set_ftp_port state value =
  ( match state.ftp_port with
      | `Active(_,_,fd) | `Ext_active(_,_,fd) ->
	  Netlog.Debug.release_fd fd;
	  Unix.close fd
      | _ ->
	  ()
  );
  { state with ftp_port = value }


let feature_line_re = 
  Netstring_str.regexp "^ \\([\x21-\xff]+\\)\\( \\(.*\\)\\)?$"

let line_re = Netstring_str.regexp "\r?\n"

let parse_features s =
  let lines = Netstring_str.split line_re s in
  List.flatten
    (List.map
       (fun line ->
	  match Netstring_str.string_match feature_line_re line 0 with
	    | None -> []
	    | Some m ->
		let label = Netstring_str.matched_group m 1 line in
		let param = 
		  try Some(Netstring_str.matched_group m 3 line)
		  with Not_found -> None in
		[ label, param ]
       )
       lines)


let semi_re =
  Netstring_str.regexp ";"

let fact_re =
  Netstring_str.regexp "\\([-a-zA-Z0-9,.!@#$%^&()_+?/\\'\"]+\\)[*]?"

let parse_facts s =
  let l = Netstring_str.split semi_re s in
  List.map
    (fun word ->
       match Netstring_str.string_match fact_re word 0 with
	 | None ->
	     failwith "Cannot parse facts of MLST feature"
	 | Some m ->
	     let name = Netstring_str.matched_group m 1 word in
	     let enabled = word.[String.length word - 1] = '*' in
	     (String.lowercase name, enabled)
    )
    l


type reply = int * string
    (* Reply code plus text *)


let string_of_state =
  function
    | `Not_connected -> "Not_connected"
    | `Init -> "Init"
    | `Success -> "Success"
    | `Proto_error -> "Proto_error"
    | `Temp_failure -> "Temp_failure"
    | `Perm_failure -> "Perm_failure"
    | `Rename_seq -> "Rename_seq"
    | `Restart_seq -> "Restart_seq"
    | `User_pass_seq -> "User_pass_seq"
    | `User_acct_seq -> "User_acct_seq"
    | `Pass_acct_seq -> "Pass_acct_seq"
    | `Preliminary -> "Preliminary"


let nc_state () =
  { cmd_state = `Not_connected;
    ftp_connected = false;
    ftp_data_conn = false;
    ftp_user = None;
    ftp_password = None;
    ftp_account = None;
    ftp_logged_in = false;
    ftp_port = `Unspecified;
    ftp_repr = `ASCII None;
    ftp_structure = `File_structure;
    ftp_trans = `Stream_mode;
    ftp_dir = [];
    ftp_features = None;
    ftp_options = [];
  }

let init_state s =
  let _s_name =
    match Unix.getsockname s with
      | Unix.ADDR_INET(addr,_) ->
	  Unix.string_of_inet_addr addr
      | _ ->
	  failwith "Not an internet socket"
  in
  { cmd_state = `Init;
    ftp_connected = true;
    ftp_data_conn = false;
    ftp_user = None;
    ftp_password = None;
    ftp_account = None;
    ftp_logged_in = false;
    ftp_port = `Unspecified;
    ftp_repr = `ASCII None;
    ftp_structure = `File_structure;
    ftp_trans = `Stream_mode;
    ftp_dir = [];
    ftp_features = None;
    ftp_options = [];
  }


let start_reply_re = Netstring_str.regexp "^[0-9][0-9][0-9]-"
let end_reply_re = Netstring_str.regexp "^[0-9][0-9][0-9] "


let is_active state =
  match state.ftp_port with
    | `Active _ -> true
    | `Ext_active _ -> true
    | _ -> false

let is_passive state =
  match state.ftp_port with
    | `Passive _ -> true
    | `Ext_passive _ -> true
    | _ -> false


class work_engine e =
object
  method is_working =
    match e # state with
      | `Working _ -> true
      | _ -> false
  method abort() =
    match e # state with
      | `Working _ -> e # abort()
      | _ -> ()
end



class type ftp_client_pi =
object
  method exec_e : ?prelim:(ftp_state -> reply -> unit) ->
                  cmd -> (ftp_state * reply) Uq_engines.engine
  method send_abort : unit -> unit
  method request_notification : (unit -> bool) -> unit
  method run : unit -> unit
  method ftp_state : ftp_state
  method state : unit Uq_engines.engine_state
  method abort : unit -> unit
  method event_system : Unixqueue.event_system
  method is_empty : bool
  method need_ip6 : bool
  method supports_tvfs : bool
  method supports_mdtm : bool
  method supports_size : bool
  method supports_mlst : bool
  method mlst_facts : string list
  method mlst_enabled_facts : string list
  method supports_utf8 : bool
end

type ftp_method =
    ftp_client_pi -> unit Uq_engines.engine



exception Dummy
exception Next
exception Abort


class ftp_client_pi_impl
        ?(event_system = Unixqueue.create_unix_event_system())
	?(timeout = 300.0)
	?proxy
        () =
  let ctrl_input_buffer = Netbuffer.create 500 in
  let ctrl_input, ctrl_input_shutdown = 
    Netchannels.create_input_netbuffer ctrl_input_buffer in
object(self)

  inherit [unit] Uq_engines.engine_mixin (`Working 0) event_system as mix

  val queue = Queue.create()

  val mutable ftp_state = nc_state()

  val mutable data_engine = None
  val mutable work_engines = ( [] : work_engine list)

  val ctrl = new telnet_session
  val mutable ctrl_attached = false
  val mutable sock_opt = None

  val reply_text = Buffer.create 200
  val mutable reply_code = (-1)
  val mutable reply_callback = (fun _ _ -> ())

  val mutable error_callback = (fun _ -> ())

  val mutable interaction_state = 
    ( `Not_connected
	: [ `Ready 
          | `Connecting_pasv of cmd * Uq_engines.connect_status Uq_engines.engine
	  | `Listening_actv of cmd * Unixqueue.event Uq_engines.engine
          | `Transfer of cmd
	  | `Transfer_replied of cmd * int * string
	  | `Waiting of cmd
	  | `Not_connected
	  ] )
      (* `Ready: another command can be sent now
       * `Connecting_pasv: In passive mode, we are connecting to the
       *    remote port (done by the argument engine). This state is
       *    skipped when we are still connected.
       * `Listening_actv: In active mode, we are listening for the
       *    connect (done by the argument engine). This state is
       *    skipped when we are still connected.
       * `Transfer: a data transfer is in progress
       * `Transfer_replied: while the rest of the transfer is not yet
       *    done, the server already sent a reply
       * `Waiting: it is waited for the reply
       * `Not_connected
       *)


  initializer (
    ctrl # set_event_system event_system;
    ctrl # set_callback self#receive_ctrl_reply;
    ctrl # set_exception_handler self#catch_telnet_exception;
    let opts = ctrl # get_options in
    ctrl # set_options
      { opts with
	  Telnet_client.connection_timeout = timeout
      }
  )

  method private sock =
    match sock_opt with
      | None -> assert false
      | Some sock -> sock

  method private peer_str =
    match sock_opt with
      | None -> "n/a"
      | Some sock ->
	  ( try Netsys.string_of_sockaddr (Netsys.getpeername sock)
	    with _ -> "(noaddr)" 
	  )

  method ftp_state = ftp_state

  method is_empty = Queue.is_empty queue

  method abort() =
    ctrl # reset();
    self # close_connection();
    self # set_state `Aborted;
    let e = Abort in
    error_callback e;
    Queue.iter
      (fun (_,_,cb) -> cb e)
      queue;
    Queue.clear queue
    

  method private catch_telnet_exception e =
    self # set_error (Telnet_protocol e)

  method private set_error e =
    ctrl # reset();
    self # close_connection();
    self # set_state (`Error e);
    error_callback e;
    Queue.iter
      (fun (_,_,cb) -> cb e)
      queue;
    Queue.clear queue

  method private protect f =
    (* Run [f ()] and catch exceptions *)
    try
      f()
    with
      | err -> self # set_error err

  method private clean_work_engines() =
    work_engines <- List.filter (fun e -> e # is_working) work_engines


  method private receive_ctrl_reply got_synch =
    while not (Queue.is_empty ctrl#input_queue) do
      let tc = Queue.take ctrl#input_queue in
      match tc with
	| Telnet_data data ->
	    Netbuffer.add_string ctrl_input_buffer data;
	    self # protect (self # parse_ctrl_reply)

	| Telnet_nop ->
	    ()

	| Telnet_will _
	| Telnet_wont _
	| Telnet_do _
	| Telnet_dont _ ->
	    ctrl # process_option_command tc

	| Telnet_sb _ 
	| Telnet_se ->
	    ()    (* Ignore subnegotiation *)

	| Telnet_eof ->
	    ctrl_input_shutdown();
	    self # protect (self # parse_ctrl_reply)

	| Telnet_timeout ->
	    self # set_error (FTP_timeout self#peer_str)

	| _ ->
	    (* Unexpected telnet command *)
	    self # protect (fun () ->
			      proto_viol "Unexpected command on Telnet level")
    done

  method private parse_ctrl_reply() =
    try
      while true do
	let line = ctrl_input # input_line() in  (* or exception! *)
	dlogr (fun () ->
		 sprintf "ctrl received: %s" line);
	if Netstring_str.string_match start_reply_re line 0 <> None then (
	  let code = int_of_string (String.sub line 0 3) in
	  if reply_code <> (-1) && reply_code <> code then 
	    proto_viol "Parse error of control message";
	  reply_code <- code;
	  Buffer.add_string reply_text line;
	  Buffer.add_string reply_text "\n";
	)
	else
	  if Netstring_str.string_match end_reply_re line 0 <> None then (
	    let code = int_of_string (String.sub line 0 3) in
	    if reply_code <> (-1) && reply_code <> code then
	      proto_viol "Parse error of control message";
	    Buffer.add_string reply_text line;
	    Buffer.add_string reply_text "\n";
	    let text = Buffer.contents reply_text in
	    reply_code <- (-1);
	    Buffer.clear reply_text;
	    self # interpret_ctrl_reply code text;
	  )
	  else (
	    if reply_code = (-1) then
	      proto_viol "Parse error of control message";
	    Buffer.add_string reply_text line;
	    Buffer.add_string reply_text "\n";
	  )
      done
    with
      | Netchannels.Buffer_underrun ->
	  (* No complete line yet *)
	  ()
      | End_of_file ->
	  ftp_state <- nc_state();
	  self # set_state (`Done ())

  method private interpret_ctrl_reply code text =
    (* This method is called whenever a reply has been completely received.
     * This may happen in a number of situations:
     * - As greeting message
     * - As regular response to a sent FTP command
     * - When the data transfer is over. Note that the control response
     *   may be received before the end of the transfer is seen by the
     *   client.
     * - Within the data transfer
     * - At any other point in time, but this is regarded as protocol error
     *)
    let reply st cmd_state =
      let st' = { st with cmd_state = cmd_state } in
      ftp_state <- st';
      dlogr (fun () ->
	       sprintf "state: %s" (string_of_state cmd_state));
      if cmd_state <> `Preliminary then
	ctrl # expect_input false;
      reply_callback st' (code,text)
    in
    let ready() =
      interaction_state <- `Ready in
    let unexpected cmdname =
      proto_viol ("Unexpected control message (code=" ^ 
		    string_of_int code ^ " after command " ^ cmdname ^ ")") in
    ( match interaction_state with
	| `Ready ->
	    proto_viol "Spontaneous control message"
	| `Waiting (`Connect _) ->
	    (match code with
	       | 120 -> reply ftp_state `Preliminary
	       | 220 -> ready(); reply ftp_state `Success
	       | n when n >= 400 && n <= 499 ->
		   ready(); reply ftp_state `Temp_failure
	       | n when n >= 500 && n <= 599 ->
		   ready(); reply ftp_state `Perm_failure
	       | _   -> proto_viol "Unexpected control message"
	    )
	| `Waiting `Dummy ->
	    ready(); reply ftp_state `Success
	| `Waiting (`USER s) ->
	    ( match code with
		| 230 -> 
		    ready(); 
                    reply { ftp_state with 
			      ftp_user = Some s;
			      ftp_password = None;
			      ftp_account = None;
			      ftp_logged_in = true } `Success
		| 530 -> 
		    ready();
		    reply { ftp_state with
			      ftp_logged_in = false } `Perm_failure
		| 331 -> 
		    ready(); 
		    reply { ftp_state with 
			      ftp_user = Some s;
			      ftp_password = None;
			      ftp_account = None;
			      ftp_logged_in = false } `User_pass_seq
		| 332 -> 
		    ready();
		    reply { ftp_state with 
			      ftp_user = Some s;
			      ftp_password = None;
			      ftp_account = None;
			      ftp_logged_in = false } `User_acct_seq
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "USER"
	    )
	| `Waiting (`PASS s) ->
	    ( match code with
		| 202 | 230 -> 
		    ready(); 
		    reply { ftp_state with
			      ftp_password = Some s;
			      ftp_account = None;
			      ftp_logged_in = true } `Success
		| 530 ->
		    ready();
		    reply { ftp_state with
			      ftp_logged_in = false } `Perm_failure
		| 332 ->  
		    ready();
		    reply { ftp_state with 
			      ftp_password = Some s;
			      ftp_account = None;
			      ftp_logged_in = false } `Pass_acct_seq
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "PASS"
	    )
	| `Waiting (`ACCT s) ->
	    ( match code with
		| 202 | 230 -> 
		    ready(); 
		    reply { ftp_state with
			      ftp_account = Some s;
			      ftp_logged_in = true } `Success
		| 530 -> 
		    ready();
		    reply { ftp_state with
			      ftp_logged_in = false } `Perm_failure
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "ACCT"
	    )
	| `Waiting (`CWD s) ->
	    ( match code with
		| 200 | 250 -> 
		    ready(); 
		    let ftp_state' =
		      { ftp_state with ftp_dir = s :: ftp_state.ftp_dir } in
		    reply ftp_state' `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "CWD"
	    )
	| `Waiting `CDUP ->
	    ( match code with
		| 200 | 250 -> 
		    ready();
		    let ftp_state' =
		      match ftp_state.ftp_dir with
			| [] -> ftp_state
			| _ :: dir ->
			    { ftp_state with ftp_dir = dir } in
		    reply ftp_state' `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "CDUP"
	    )
	| `Waiting `REIN ->
	    ( match code with
		| 120 -> 
		    reply ftp_state `Preliminary
		| 220 -> 
		    ready(); reply (init_state self#sock) `Success
		    (* CHECK: Close data connection? *)
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "REIN"
	    )
	| `Waiting `PASV ->
	    ( match code with
		| 227 ->
		    let (addr,port) = extract_pasv text in
		    ready();
		    self # close_data_connection();
		    reply (set_ftp_port ftp_state (`Passive(addr,port))) `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "PASV"
	    )
	| `Waiting (`EPSV _) ->
	    ( match code with
		| 229 ->
		    let port = extract_epsv text in
		    ready();
		    self # close_data_connection();
		    reply (set_ftp_port ftp_state (`Ext_passive port)) `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "EPSV"
	    )
	| `Waiting (`TYPE t) ->
	    ( match code with
		| n when n >= 200 && n <= 299 ->
		    ready();
		    reply { ftp_state with ftp_repr = t } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "TYPE"
	    )
	| `Waiting (`MODE m) ->
	    ( match code with
		| n when n >= 200 && n <= 299 ->
		    ready();
		    reply { ftp_state with ftp_trans = m } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "MODE"
	    )
	| `Waiting (`STRU s) ->
	    ( match code with
		| n when n >= 200 && n <= 299 ->
		    ready();
		    reply { ftp_state with ftp_structure = s } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "STRU"
	    )
	| `Waiting (`REST _) ->
	    ( match code with
		| 350 ->
		    ready();
		    reply ftp_state `Restart_seq
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "REST"
	    )
	| `Waiting (`RNFR _) ->
	    ( match code with
		| 350 ->
		    ready(); reply ftp_state `Rename_seq
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "RNFR"
	    )
	| `Waiting `FEAT ->
	    ( match code with
		| 211 ->
		    let l = parse_features text in
		    ready(); 
		    reply { ftp_state with ftp_features = Some l } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "FEAT"
	    )
	| `Waiting (`OPTS(cmd,param_opt)) ->
	    ( match code with
		| n when n >= 200 && n <= 299 ->
		    let l =
		      (cmd, param_opt) ::
			(List.filter
			   (fun (cmd',_) -> 
			      String.lowercase cmd <> String.lowercase cmd')
			   ftp_state.ftp_options) in
		    ready(); 
		    reply { ftp_state with ftp_options = l } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    unexpected "OPTS"
	    )
	| `Waiting (( `SMNT _
		    | `QUIT
		    | `PORT
		    | `ALLO _
		    | `RNTO _
		    | `DELE _
		    | `RMD _
		    | `MKD _
		    | `PWD 
 		    | `SYST
 		    | `STAT _
		    | `HELP _
		    | `SITE _
		    | `MDTM _
		    | `SIZE _
		    | `NOOP
		    | `MLST _) as cmd) ->
	    ( match code with
		| n when n >= 100 && n <= 199 ->
		    reply ftp_state `Preliminary
		| n when n >= 200 && n <= 299 ->
		    ready(); reply ftp_state `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   ->
		    unexpected (string_of_cmd_nolf cmd)
	    )

	| `Connecting_pasv(cmd, conn_engine) ->
	    (* This is just a very early response *)
	    ( match code with
		| 125 | 150 ->
		    reply ftp_state `Preliminary
		| n when n >= 400 && n <= 499 ->
		    conn_engine # abort();
		    ready();
		    reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    conn_engine # abort();
		    ready();
		    reply ftp_state `Perm_failure
		| _ ->
		    interaction_state <- `Transfer_replied(cmd,code,text)
	    )
	| `Listening_actv(cmd, acc_engine) ->
	    (* This is just a very early response *)
	    ( match code with
		| 125 | 150 ->
		    reply ftp_state `Preliminary
		| n when n >= 400 && n <= 499 ->
		    acc_engine # abort();
		    ready();
		    reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    acc_engine # abort();
		    ready();
		    reply ftp_state `Perm_failure
		| _ ->
		    interaction_state <- `Transfer_replied(cmd,code,text)
	    )
	| `Transfer cmd ->
	    (* The transfer probably ends in the near future, just record
             * the reply, and wait for the end of the transfer.
             *)
	    ( match code with
		| 125 | 150 ->
		    reply ftp_state `Preliminary
		| n when n >= 400 && n <= 499 ->
		    self # close_data_connection();
		    ready();
		    reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    self # close_data_connection();
		    ready();
		    reply ftp_state `Perm_failure
		| _ ->
		    interaction_state <- `Transfer_replied(cmd,code,text)
	    )
	| `Transfer_replied (cmd,_,_) ->
	    (* Another reply! This is an error. *)
	    unexpected (string_of_cmd_nolf cmd ^ ", second reply")
	| `Waiting ( (`RETR(_,_)
		     |`LIST(_,_)
		     |`NLST(_,_)
		     |`MLSD(_,_)
		     |`STOR(_,_)
		     |`STOU _
		     |`APPE(_,_) ) as cmd ) ->
	    (* This state is only possible when the transfer has already
             * been completed.
             *)
	    ( match data_engine with
		| None -> ()  (* strange *)
		| Some e -> 
		    if e # descr_state <> `Clean then self # close_data_connection()
	    );
	    ( match code with
		| 125 | 150 ->
		    reply ftp_state `Preliminary
		| 226 ->
		    self # close_data_connection();
 		    ready();
		    reply ftp_state `Success
		| 250 ->
 		    ready();
		    reply ftp_state `Success
		| n when n >= 400 && n <= 499 ->
		    self # close_data_connection();
		    reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    self # close_data_connection();
		    reply ftp_state `Perm_failure
		| _ ->
		    unexpected (string_of_cmd_nolf cmd)
	    )
	| _ -> assert false

    );
    self # send_command_when_ready()


  method private send_command_when_ready() =
    if interaction_state = `Ready then (
      try
	assert(reply_code = (-1));
	let (cmd, onreply, onerror) = Queue.take queue in  (* or Queue.Empty *)
	interaction_state <- `Waiting cmd;
	error_callback <- onerror;
	( match cmd with
	    | `Connect _ -> 
		failwith "Ftp_client: Already connected"
	    | `RETR(_,f)
	    | `LIST(_,f)
	    | `NLST(_,f)
	    | `MLSD(_,f) ->
		let h _ = assert false in
		( match ftp_state.ftp_port with
		    | `Passive(_,_) | `Ext_passive _ ->
			(* In passive mode, connect now: *)
			self # setup_passive_endpoint `Receiver cmd h f
			
		    | `Active(_,_,_) | `Ext_active(_,_,_) ->
			(* In active mode, accept the connection now *)
			self # setup_active_endpoint `Receiver cmd h f

		    | `Unspecified ->
			failwith "Ftp_client: Usage error, one must send \
                                  `PORT or `PASV before the transfer"
		)
	    | `STOR(_,f)
	    | `STOU f
	    | `APPE(_,f) ->
		let h _ = assert false in
		( match ftp_state.ftp_port with
		    | `Passive(_,_) | `Ext_passive _  ->
			(* In passive mode, connect now: *)
			self # setup_passive_endpoint `Sender cmd f h
			
		    | `Active(_,_,_) | `Ext_active(_,_,_) ->
			(* In active mode, accept the connection now *)
			self # setup_active_endpoint `Sender cmd f h

		    | `Unspecified ->
			failwith "Ftp_client: Usage error, one must send \
                                  `PORT or `PASV before the transfer"
		)
	    | _ -> ()
	);
	let line = 
	  match cmd with
	    | `PORT | `EPRT ->
		let addr =  (* of control connection *)
		  match Unix.getsockname self#sock with
		    | Unix.ADDR_INET(addr,_) -> addr
		    | _ -> assert false in
		let addr_str = Unix.string_of_inet_addr addr in
		let dom = Netsys.domain_of_inet_addr addr in
		let server_sock = Unix.socket dom Unix.SOCK_STREAM 0 in
		Unix.bind server_sock (Unix.ADDR_INET(addr,0));
		Unix.listen server_sock 1;
		Netlog.Debug.track_fd
		  ~owner:"Ftp_client"
		  ~descr:("Data server (active mode) for " ^ 
			    self#peer_str)
		  server_sock;
		let port =
		  match Unix.getsockname server_sock with
		    | Unix.ADDR_INET(_,port) -> port
		    | _ -> assert false in
		dlogr (fun () ->
			 sprintf "created data server (active mode) \
                                  listening for %s:%d"
			   addr_str port);
		let p =
		  if cmd = `PORT then
		    `Active(addr_str,port,server_sock)
		  else
		    `Ext_active(addr_str,port,server_sock) in
		ftp_state <- set_ftp_port ftp_state p;
		if cmd = `PORT then (
		  let port_str = format_port (addr_str,port) in
		  "PORT " ^ port_str ^ "\r\n"
		) else (
		  let eprt_str = format_eprt (addr_str,port) in
		  "EPRT " ^ eprt_str ^ "\r\n"
		)
	    | _ -> string_of_cmd cmd in
	reply_callback <- onreply;
	if cmd = `Disconnect then (
	  self # close_connection();
	  onreply ftp_state (221, "Disconnected");
	)
	else (
	  if line <> "" then (
	    dlogr (fun () ->
		     sprintf "ctrl sent: %s" line);
	    Queue.push (Telnet_data line) ctrl#output_queue;
	    ctrl # expect_input true;
	    ctrl # update();
	  ) else (
	    dlog "ctrl sent: DUMMY";
	    raise Dummy
	  )
	)
      with
	| Queue.Empty ->
	    ()
	| Dummy ->
	    self # interpret_ctrl_reply 200 "200 DUMMY"
    )


  method private maybe_open_connection() =
    if interaction_state = `Not_connected then (
      try 
	let (cmd,onreply,onerror) = Queue.take queue in  (* or Not_found *)
	error_callback <- onerror;
	match cmd with
	  | `Connect(host,port) ->
	      dlogr (fun () ->
		       sprintf "connecting to %s:%d" host port);
	      let conn_engine =
		Uq_engines.timeout_engine
		  timeout
		  (FTP_timeout (sprintf "%s:%d" host port))
		  (Uq_engines.connector ?proxy
		     (`Socket(
			`Sock_inet_byname(Unix.SOCK_STREAM,
					  host,
					  port),
			Uq_engines.default_connect_options))
		     event_system
		  ) in
	      Uq_engines.when_state
		~is_done:(function
			    | `Socket(sock,_) ->
				(* N.B. This socket is fd-tracked by Telnet_client *)
				dlogr (fun () ->
					 sprintf "connected to %s:%d"
					   host port);
				sock_opt <- Some sock;
				ftp_state <- init_state sock;
				ctrl # set_connection (Telnet_socket sock);
				if not ctrl_attached then (
				  ctrl # attach();
				  ctrl_attached <- true
				);
				reply_callback <- onreply;
				self # clean_work_engines();
			    | _ ->
				assert false
			 )
		~is_error:self#set_error
		conn_engine;
	      work_engines <- new work_engine conn_engine :: work_engines;
	      interaction_state <- `Waiting cmd;

	  | `Disconnect | `Dummy ->
	      raise Next
	  | _ -> 
	      failwith "Ftp_client: Not connected"
	      
      with 
	| Queue.Empty -> ()
	| Next -> self # maybe_open_connection()
    )


  method private close_data_connection() =
    ( match data_engine with
	| None -> ()
	| Some e -> 
	    dlogr (fun () -> "aborting data transfer");
	    ( match e # state with
		| `Working _ -> e # abort()
		| _ -> ()
	    );
	    let data_sock = e # descr in
	    Netlog.Debug.release_fd data_sock;
	    Unix.close data_sock;
	    data_engine <- None;
    );
    ftp_state <- set_ftp_port ftp_state `Unspecified;
    List.iter (fun e -> e # abort()) work_engines;
    work_engines <- [];

  method private close_connection() =
    (* Terminates any transfer immediately and closes the connection *)
    interaction_state <- `Not_connected;
    self # close_data_connection();
    ctrl # reset();
    sock_opt <- None;
    ftp_state <- nc_state()


  method private setup_passive_endpoint typ cmd f_send f_receive =
    assert(is_passive ftp_state);
    let setup =
      match typ with
	| `Receiver -> self#setup_receiver f_receive
	| `Sender   -> self#setup_sender f_send in
    ( match data_engine with
	| Some e ->
	    (* Continue with the old connection *)
	    if e # descr_state <> `Clean then
	      proto_viol "Data connection not clean";
	    (* Create a new engine taking the connection over *)
	    let data_sock = e # descr in
	    dlogr (fun () ->
		     sprintf "reusing passive-mode data connection");
	    ctrl # expect_input false;
	    setup
	      ~is_done:(fun () ->
			  match interaction_state with
			    | `Transfer_replied(_,c,t) ->
				interaction_state <-
				  `Waiting cmd;
				self # interpret_ctrl_reply c t
			    | `Transfer cmd ->
				ctrl # expect_input true;
				interaction_state <-
				  `Waiting cmd
			    | _ -> assert false
		       )
	      ~is_error:(fun err ->
			   self # set_error (FTP_error err))
	      data_sock;
	    interaction_state <- `Transfer cmd
	| None ->
	    (* Indicates that a connection is to be opened *)
	    let addr,port =
	      match ftp_state.ftp_port with
		| `Passive(a,p) -> a,p 
		| `Ext_passive p ->
		    let sock = self#sock in
		    let sockname = 
		      try Netsys.getpeername sock
		      with _ -> failwith "Cannot determine socket address" in
		    let a =
		      match sockname with
			| Unix.ADDR_INET(ip,_) -> Unix.string_of_inet_addr ip
			| _ -> failwith "Bad socket family" in
		    a,p
		| _ -> assert false in
	    let conn_engine =
	      Uq_engines.timeout_engine
		timeout
		(FTP_timeout (sprintf "%s:%d" addr port))
		(Uq_engines.connector ?proxy
		   (`Socket(
		      `Sock_inet(Unix.SOCK_STREAM,
				 Unix.inet_addr_of_string addr,
				 port),
		      Uq_engines.default_connect_options))
		   event_system
		) in
	    Uq_engines.when_state
	      ~is_done:(function
			  | `Socket(data_sock,_) ->
			      dlogr (fun () ->
				       sprintf "passive-mode data connection \
                                                to %s:%d established"
					 addr port);
			      Netlog.Debug.track_fd
				~owner:"Ftp_client"
				~descr:("Data connection (passive mode) for "^ 
					  self#peer_str)
				data_sock;
			      ctrl # expect_input false;
			      setup
				~is_done:(fun () ->
					    match interaction_state with
					      | `Transfer_replied(_,c,t) ->
						  interaction_state <-
						    `Waiting cmd;
						  self # interpret_ctrl_reply c t
					      | `Transfer cmd ->
						  ctrl # expect_input true;
						  interaction_state <-
						    `Waiting cmd
					      | _ -> assert false
					 )
				~is_error:(fun err ->
					     self # set_error (FTP_error err))
				data_sock;
			      interaction_state <- `Transfer cmd;
			      self # clean_work_engines();
			  | _ -> assert false
		       )
	      ~is_error:(fun err -> 
			   let rep_err =
			     match err with
			       | FTP_timeout _ -> err
			       | _ -> FTP_error err in
			   self # clean_work_engines();
			   self # set_error rep_err)
	      conn_engine;
	    work_engines <- new work_engine conn_engine :: work_engines;
	    interaction_state <- `Connecting_pasv(cmd,conn_engine);
    )

  method private setup_active_endpoint typ cmd f_send f_receive =
    assert(is_active ftp_state);
    let setup =
      match typ with
	| `Receiver -> self#setup_receiver f_receive
	| `Sender   -> self#setup_sender f_send in
    ( match data_engine with
	| Some e ->
	    (* Continue with the old connection *)
	    if e # descr_state <> `Clean then
	      proto_viol "Data connection not clean";
	    (* Create a new engine taking the connection over *)
	    dlogr (fun () ->
		     sprintf "reusing active-mode data connection");
	    let data_sock = e # descr in
	    setup
	      ~is_done:(fun () ->
			  match interaction_state with
			    | `Transfer_replied(_,c,t) ->
				interaction_state <-
				  `Waiting cmd;
				self # interpret_ctrl_reply c t
			    | `Transfer cmd ->
				interaction_state <-
				  `Waiting cmd
			    | _ -> assert false
		       )
	      ~is_error:(fun err ->
			   self # set_error (FTP_error err))
	      data_sock;
	    interaction_state <- `Transfer cmd
	| None ->
	    (* Indicates that a connection is to be opened *)
	    let addr,port,server_sock =
	      match ftp_state.ftp_port with
		| `Active(a,p,fd) -> a,p,fd | _ -> assert false in
	    let acc_engine =
	      Uq_engines.timeout_engine
		timeout
		(FTP_timeout (sprintf "%s:%d" addr port))
		(new Uq_engines.poll_engine 
		   [ (Unixqueue.Wait_in server_sock), (-1.0) ] event_system
		 :> _ Uq_engines.engine
		) in
	    Uq_engines.when_state
	      ~is_done:(function
			  | Unixqueue.Input_arrived(_,_) ->
			      let data_sock, _ = Unix.accept server_sock in
			      dlogr (fun () ->
				       sprintf "accepted new active-mode \
                                                data connection");
			      Netlog.Debug.track_fd
				~owner:"Ftp_client"
				~descr:("Data connection (active mode) for " ^ 
					  self#peer_str)
				data_sock;
			      setup
				~is_done:(fun () ->
					    match interaction_state with
					      | `Transfer_replied(_,c,t) ->
						  interaction_state <-
						    `Waiting cmd;
						  self # interpret_ctrl_reply c t
					      | `Transfer cmd ->
						  interaction_state <-
						    `Waiting cmd
					      | _ -> assert false
					 )
				~is_error:(fun err ->
					     self # set_error (FTP_error err))
				data_sock;
			      interaction_state <- `Transfer cmd;
			      self # clean_work_engines()
			  | _ ->
			      assert false
		       )
	      ~is_error:(fun err -> 
			   let rep_err =
			     match err with
			       | FTP_timeout _ -> err
			       | _ -> FTP_error err in
			   self # clean_work_engines();
			   self # set_error rep_err)
	      acc_engine;
            let acc_engine = (acc_engine :> Unixqueue.event Uq_engines.engine) in
	    work_engines <- new work_engine acc_engine :: work_engines;
	    interaction_state <- `Listening_actv(cmd,acc_engine);
    )


  method private setup_receiver f_receive ~is_done ~is_error data_sock =
    let data_peer = 
      try Netsys.string_of_sockaddr (Unix.getpeername data_sock)
      with _ -> "n/a" in
    let local = f_receive ftp_state in
    let de = 
      new ftp_data_receiver
	~esys:event_system
	~mode:ftp_state.ftp_trans
	~local_receiver:local
	~descr:data_sock () in
    let e = 
      Uq_engines.watchdog timeout de
      >> (function
	    | `Error Uq_engines.Watchdog_timeout ->
		`Error (FTP_timeout data_peer)
	    | st -> st
	 ) in
    data_engine <- Some (de :> ftp_data_engine);
    Uq_engines.when_state 
      ~is_done
      ~is_error
      e

	
  method private setup_sender f_send ~is_done ~is_error data_sock =
    let data_peer = 
      try Netsys.string_of_sockaddr (Unix.getpeername data_sock)
      with _ -> "n/a" in
    let local = f_send ftp_state in
    let de = 
      new ftp_data_sender
	~esys:event_system
	~mode:ftp_state.ftp_trans
	~local_sender:local
	~descr:data_sock () in
    let e = 
      Uq_engines.watchdog timeout de
      >> (function
	    | `Error Uq_engines.Watchdog_timeout ->
		`Error (FTP_timeout data_peer)
	    | st -> st
	 ) in
    data_engine <- Some (de :> ftp_data_engine);
    Uq_engines.when_state 
      ~is_done
      ~is_error
      e

	
  method exec_e ?(prelim = fun _ _ -> ()) cmd =
    match self#state with
      | `Working _ ->
	  let e, signal = Uq_engines.signal_engine event_system in
	  let onreply st code =
	    if st.cmd_state = `Preliminary then
	      prelim st code
	    else
	      signal(`Done(st,code)) in
	  let onerror x =
	    if x = Abort then
	      signal `Aborted
	    else
	      signal (`Error x) in
	  Queue.push (cmd, onreply, onerror) queue;
	  self # protect (self # maybe_open_connection);
	  self # protect (self # send_command_when_ready);
	  e
      | _ ->
	  failwith "Ftp_client.ftp_client_pi: Connection already terminated"
    
  method send_abort () = ()
    (* TODO *)

  method run () = Unixqueue.run event_system

  method need_ip6 =
    try
      let addr =  (* of control connection *)
	match Unix.getsockname self#sock with
	  | Unix.ADDR_INET(addr,_) -> addr
	  | _ -> raise Not_found in
      let dom = Netsys.domain_of_inet_addr addr in
      match dom with
	| Unix.PF_INET6 -> true
	| _ -> false
    with _ -> false
    
  method supports_tvfs =
    match ftp_state.ftp_features with
      | None -> false
      | Some l -> List.mem_assoc "TVFS" l

  method supports_mdtm =
    match ftp_state.ftp_features with
      | None -> false
      | Some l -> List.mem_assoc "MDTM" l

  method supports_size =
    match ftp_state.ftp_features with
      | None -> false
      | Some l -> List.mem_assoc "SIZE" l

  method supports_mlst =
    match ftp_state.ftp_features with
      | None -> false
      | Some l -> List.mem_assoc "MLST" l

  method mlst_facts =
    match ftp_state.ftp_features with
      | None -> []
      | Some l -> 
	  ( try
	      let mlst_param =
		List.assoc "MLST" l in
	      ( match mlst_param with
		  | None -> []
		  | Some p ->
		      List.map fst (parse_facts p)
	      )
	    with
	      | Not_found -> []
	  )

  method mlst_enabled_facts =
    match ftp_state.ftp_features with
      | None -> []
      | Some l -> 
	  ( try
	      let mlst_param =
		List.assoc "MLST" l in
	      ( match mlst_param with
		  | None -> []
		  | Some p ->
		      List.map fst 
			( List.filter (fun (_,en) -> en) (parse_facts p) )
	      )
	    with
	      | Not_found -> []
	  )

  method supports_utf8 =
    match ftp_state.ftp_features with
      | None -> false
      | Some l -> List.mem_assoc "UTF8" l

end


exception FTP_method_temp_failure of int * string
exception FTP_method_perm_failure of int * string
exception FTP_method_unexpected_reply of int * string

let connect_method ~host ?(port = 21) () (pi:ftp_client_pi) =
  pi # exec_e (`Connect(host,port))
  ++ (fun _ -> eps_e (`Done()) pi#event_system)


let errorcheck_e pi (st,(rcode,rtext)) =
  match st.cmd_state with
    | `Success -> 
	eps_e (`Done()) pi#event_system
    | `Temp_failure ->
	eps_e (`Error(FTP_method_temp_failure(rcode,rtext))) pi#event_system
    | `Perm_failure ->
	eps_e (`Error(FTP_method_perm_failure(rcode,rtext))) pi#event_system
    | _ ->
	eps_e (`Error(FTP_method_unexpected_reply(rcode,rtext))) pi#event_system


let login_method ~user ~get_password ~get_account () (pi:ftp_client_pi) =
  pi # exec_e (`USER user)
  ++ (fun (st,r) ->
	match st.cmd_state with
	  | `Success -> 
	      eps_e (`Done()) pi#event_system
	  | `User_pass_seq ->
	      pi # exec_e (`PASS (get_password()))
	      ++ (fun (st2,r2) ->
		    match st.cmd_state with
		      | `Pass_acct_seq ->
			  pi # exec_e (`ACCT (get_account()))
			  ++ (errorcheck_e pi)
		      | _ ->
			  errorcheck_e pi (st2,r2)
		 )
	  | `User_acct_seq ->
	      pi # exec_e (`ACCT (get_account()))
	      ++ (errorcheck_e pi)
	  | _ ->
	      errorcheck_e pi (st,r)
     )


let slash_re = Netstring_str.regexp "/+";;


let rec basename l =
  match l with
    | [] -> failwith "Bad filename"
    | [name] -> name
    | _ :: l' -> basename l'


let rec dirname l =
  match l with
    | [] -> []
    | [name] -> []
    | dir :: l' -> dir :: dirname l'


let rec is_prefix l1 l2 =
  match (l1, l2) with
    | (x1::l1'), (x2::l2') ->
	x1 = x2 && is_prefix l1' l2' 
    | [], _ ->
	true
    | (_::_), [] ->
	false


let rec without_prefix l1 l2 =
  match (l1, l2) with
    | (x1::l1'), (x2::l2') ->
	if x1 = x2 then without_prefix l1' l2' else failwith "without_prefix"
    | [], _ ->
	l2
    | (_::_), [] ->
	failwith "without_prefix"


let walk_method (destination : [ `File of string | `Dir of string | `Stay ] )
                (pi:ftp_client_pi) =
  let rec walk_to_directory_e path =
    let ftp_state = pi#ftp_state in
    let cur_dir = List.rev ftp_state.ftp_dir in  (* ftp_dir is in rev order *)
    if is_prefix cur_dir path then
      match without_prefix cur_dir path with
	| [] ->
	    eps_e (`Done()) pi#event_system
	| dir :: _  ->
	    pi # exec_e (`CWD dir)
	    ++ (fun (st,r) ->
		  if st.cmd_state = `Success then
		    walk_to_directory_e path
		  else
		    errorcheck_e pi (st,r)
	       )
    else
      pi # exec_e `CDUP
      ++ (fun (st,r) ->
	    if st.cmd_state = `Success then
	      walk_to_directory_e path
	    else
	      errorcheck_e pi (st,r)
	 )
  in

  match destination with
    | `File name ->
	let rpath = List.rev (Netstring_str.split slash_re name) in
	( match rpath with
	    | _ :: dir -> walk_to_directory_e (List.rev dir)
	    | [] -> failwith "Bad filename"
	)
    | `Dir name ->
	let path = Netstring_str.split slash_re name in
	walk_to_directory_e path
	>> (function
	      | `Error e -> `Error e
	      | st -> st
	   )
    | `Stay ->
	eps_e (`Done()) pi#event_system



type filename =
    [ `NVFS of string
    | `TVFS of string
    | `Verbatim of string
    ]


let destination_of_file = 
  function
    | `NVFS name -> `File name
    | `TVFS _ -> `Stay
    | `Verbatim _ -> `Stay

let destination_of_dir = 
  function
    | `NVFS name -> `Dir name
    | `TVFS _ -> `Stay
    | `Verbatim _ -> `Stay

let norm_tvfs name =
  let l = Netstring_str.split slash_re name in
  let n = String.concat "/" l in
  if name <> "" && name.[0] = '/' then
    "/" ^ n
  else
    n


let ftp_filename =
  function
    | `NVFS name -> basename (Netstring_str.split slash_re name)
    | `TVFS name -> norm_tvfs name
    | `Verbatim name -> name


let file_e (file : filename) (pi:ftp_client_pi) =
  (* Internal, not exported *)
  let walk = walk_method (destination_of_file file) in
  let filename = ftp_filename file in
  walk pi 
  ++ (fun () ->
	eps_e (`Done filename) pi#event_system
     )


let dir_e (dir : filename) (pi:ftp_client_pi) =
  (* Internal, not exported *)
  let walk = walk_method (destination_of_dir dir) in
  let filename_opt = 
    match dir with
      | `NVFS name -> None
      | `TVFS name -> Some (norm_tvfs name)
      | `Verbatim name -> Some name in
  walk pi 
  ++ (fun () ->
	eps_e (`Done filename_opt) pi#event_system
     )

let quit_method () (pi:ftp_client_pi) =
  pi # exec_e `QUIT
  ++ errorcheck_e pi
  ++ (fun () ->
	(pi :> _ Uq_engines.engine)
     )

let invoke_method ~command () (pi:ftp_client_pi) =
  pi # exec_e command
  ++ errorcheck_e pi

let set_structure_method structure =
  invoke_method 
    ~command:(`STRU structure) 
    ()


let set_mode_method mode =
  invoke_method 
    ~command:(`MODE mode) 
    ()


let mkdir_method file (pi:ftp_client_pi) =
  file_e file pi
  ++ (fun filename ->
	invoke_method 
	  ~command:(`MKD filename) () pi
     )


let rmdir_method file (pi:ftp_client_pi) =
  file_e file pi
  ++ (fun filename ->
	invoke_method 
	  ~command:(`RMD filename) () pi
     )


let delete_method file (pi:ftp_client_pi) =
  file_e file pi 
  ++ (fun filename ->
	invoke_method 
	  ~command:(`DELE filename) () pi
     )

(* MDTM response is YYYYMMDDHHMMSS[.s+] (GMT) *)

let time_re = 
  Netstring_str.regexp
    ".*[^0-9]\
     \\([0-9][0-9][0-9][0-9]\\)\
     \\([0-9][0-9]\\)\
     \\([0-9][0-9]\\)\
     \\([0-9][0-9]\\)\
     \\([0-9][0-9]\\)\
     \\([0-9][0-9]\\)\
     \\(\\.\\([0-9]+\\)\\)?"

let extract_time s =
  match Netstring_str.string_match time_re s 0 with
  | None ->
      failwith ("Cannot parse time-val: " ^ s)
  | Some m ->
      let nanos =
        try 
	  let ns0 = Netstring_str.matched_group m 8 s in
	  let ns1 = if String.length ns0 > 9 then String.sub ns0 0 9 else ns0 in
	  let d = String.length ns1 in
	  int_of_string ns1 * Netlog.ten_power (9-d)
	with Not_found -> 0 in
      let get_int i = int_of_string (Netstring_str.matched_group m i s) in
      Netdate.since_epoch
        { Netdate.year = get_int 1;
          Netdate.month = get_int 2;
          Netdate.day = get_int 3;
          Netdate.hour = get_int 4;
          Netdate.minute = get_int 5;
          Netdate.second = get_int 6;
	  Netdate.nanos = nanos;
          Netdate.zone = 0;  (* GMT *)
          Netdate.week_day = -1 }


let mdtm_method ~file ~process_result () (pi:ftp_client_pi) =
  file_e file pi
  ++ (fun filename ->
	pi # exec_e (`MDTM filename)
	++ (fun (st, (code,text)) ->
	      match st.cmd_state with
		| `Success ->
		    let t = extract_time text in
		    process_result t;
		    eps_e (`Done ()) pi#event_system
		| _ ->
		    errorcheck_e pi (st, (code,text))
	   )
     )

let size_re = Netstring_str.regexp "^213 \\([0-9]+\\)"

let extract_size s =
  match Netstring_str.string_match size_re s 0 with
    | None ->
	failwith ("Cannot parse size: " ^ s)
    | Some m ->
	( try
	    Int64.of_string (Netstring_str.matched_group m 1 s)
	  with
	    | _ -> failwith ("Too large: " ^ s)
	)


let size_method ~file ~representation ~process_result () (pi:ftp_client_pi) =
  file_e file pi
  ++ (fun filename ->
	pi # exec_e (`TYPE representation)
	++ errorcheck_e pi
	++ (fun () -> pi # exec_e (`SIZE filename))
	++ (fun (st, (code,text)) ->
	      match st.cmd_state with
		| `Success ->
		    let t = extract_size text in
		    process_result t;
		    eps_e (`Done ()) pi#event_system
		| _ ->
		    errorcheck_e pi (st, (code,text))
	   )
     )



let feat_method ?(process_result = fun _ -> ()) () (pi:ftp_client_pi) =
  pi # exec_e `FEAT
  ++ errorcheck_e pi
  ++ (fun () ->
	match (pi # ftp_state).ftp_features with
	  | None -> assert false
	  | Some l -> process_result l; eps_e (`Done ()) pi#event_system
     )


let rename_method' filename_from filename_to (pi:ftp_client_pi) =
  pi # exec_e (`RNFR filename_from)
  ++ (fun (st,r) ->
	match st.cmd_state with
	  | `Rename_seq ->
	      pi # exec_e (`RNTO filename_to) ++ errorcheck_e pi
	  | _ ->
	      errorcheck_e pi (st,r)
     )


let rename_method ~file_from ~(file_to : filename) () (pi:ftp_client_pi) =
  (* Check arguments: *)
  let filename_to =
    match (file_from, file_to) with
      | (`NVFS p1), (`NVFS p2) ->
	  (* p1 and p2 must point to the same directory. Return basename of p2 *)
	  let p1' = Netstring_str.split slash_re p1 in
	  let p2' = Netstring_str.split slash_re p2 in
	  let d1 = dirname p1' in
	  let d2 = dirname p2' in
	  if d1 <> d2 then invalid_arg "Ftp_client.rename_method";
	  basename p2'
      | (`Verbatim _), (`Verbatim s) -> s
      | (`TVFS _), (`TVFS s) -> norm_tvfs s
      | _ -> invalid_arg "Ftp_client.rename_method"
  in
  file_e file_from pi
  ++ (fun filename_from ->
	rename_method' filename_from filename_to pi
     )


let transfer_method ~command ~representation () (pi:ftp_client_pi) =
  let passive_cmd = if pi#need_ip6 then `EPSV None else `PASV in
  let active_cmd = if pi#need_ip6 then `EPRT else `PORT in
  pi # exec_e (`TYPE representation)
  ++ errorcheck_e pi
  ++ (fun () ->
	pi # exec_e passive_cmd
	++ (fun (st,r) ->
	      match st.cmd_state with
		| `Perm_failure ->
		    pi # exec_e active_cmd ++ errorcheck_e pi
		| _ ->
		    errorcheck_e pi (st,r)
	   )
     )
  ++ (fun () ->
	pi # exec_e command ++ errorcheck_e pi
     )


let get_method ~file ~representation ~store () (pi:ftp_client_pi) =
  file_e file pi
  ++ (fun filename ->
	transfer_method ~command:(`RETR(filename,store)) ~representation () pi
     )


let put_method ?(meth=`STOR) 
               ~file ~representation ~store () (pi:ftp_client_pi) =
  file_e file pi
  ++ (fun filename ->
	let command =
	  match meth with
	    | `STOR -> `STOR(filename,store)
	    | `APPE -> `APPE(filename,store) in
	transfer_method ~command ~representation () pi
     )


let list_method ~dir ~representation ~store () (pi:ftp_client_pi) =
  dir_e dir pi
  ++ (fun filename_opt ->
	transfer_method
	  ~command:(`LIST(filename_opt,store)) ~representation () pi
     )


let nlst_method ~dir ~representation ~store () (pi:ftp_client_pi) =
  dir_e dir pi
  ++ (fun filename_opt ->
	transfer_method
	  ~command:(`NLST(filename_opt,store)) ~representation () pi
     )

let crlf_re = Netstring_str.regexp "\r?\n"

let parse_nlst_document s =
  Netstring_str.split crlf_re s


type entry = string * (string * string) list

type entry_type =
    [ `File | `Cdir | `Pdir | `Dir | `Other ]

type entry_perm =
    [ `Append | `Create | `Delete | `Enter | `Rename | `List | `Mkdir
    | `Delete_member | `Read | `Write 
    ]

let entry_re =
  Netstring_str.regexp "\\([^ ]*\\) \\(.*\\)$"

let factvalue_re =
  Netstring_str.regexp "\\([-a-zA-Z0-9,.!@#$%^&()_+?/\\'\"]+\\)=\\(.*\\)$"

let parse_entry_line s =
  match Netstring_str.string_match entry_re s 0 with
    | None ->
	failwith "Ftp_client.parse_entry_line"
    | Some m ->
	let facts = Netstring_str.matched_group m 1 s in
	let name = Netstring_str.matched_group m 2 s in
	let fact_l = Netstring_str.split semi_re facts in
	let parsed_facts =
	  List.map
	    (fun fact ->
	       match Netstring_str.string_match factvalue_re fact 0 with
		 | None ->
		     failwith "Ftp_client.parse_entry_line"
		 | Some m ->
		     let factname = Netstring_str.matched_group m 1 fact in
		     let value = Netstring_str.matched_group m 2 fact in
		     (String.lowercase factname, value)
	    )
	    fact_l in
	(name, parsed_facts)

let mlsd_method ~dir ~store () (pi:ftp_client_pi) =
  dir_e dir pi
  ++ (fun filename_opt ->
	transfer_method
	  ~command:(`MLSD(filename_opt,store)) ~representation:`Image () pi
     )

let parse_mlsd_document s =
  let lines = Netstring_str.split crlf_re s in
  List.map parse_entry_line lines
  

let mlst_method ~file ~process_result () (pi:ftp_client_pi) =
  file_e file pi
  ++ (fun filename ->
	pi # exec_e (`MLST (Some filename))
	++ (fun (st,(code,text)) ->
	      match st.cmd_state with
		| `Success ->
		     let lines = Netstring_str.split crlf_re text in
		     let lines' =
		       List.filter
			 (fun line ->
			    line <> "" && line.[0] = ' '
			 )
			 lines in
		     let lines'' =
		       List.map
			 (fun line ->
			    String.sub line 1 (String.length line - 1)
			 )
			 lines' in
		     let entries =
		       List.map parse_entry_line lines'' in
		     process_result entries;
		     eps_e (`Done ()) pi#event_system
  		| _ ->
		    errorcheck_e pi (st,(code,text))
	   )
     )

let get_size (_,e) =
  Int64.of_string (List.assoc "size" e)

let get_modify (_,e) =
  extract_time (" " ^ List.assoc "modify" e)

let get_create (_,e) =
  extract_time (" " ^ List.assoc "create" e)

let get_type (_,e) =
  match String.lowercase(List.assoc "type" e) with
    | "file" -> `File
    | "cdir" -> `Cdir
    | "pdir" -> `Pdir
    | "dir" -> `Dir
    | _ -> `Other

let get_unique (_,e) =
  List.assoc "unique" e

let get_perm (_,e) =
  let p = List.assoc "perm" e in
  let l = ref [] in
  String.iter
    (fun c ->
       match c with
	 | 'a' -> l := `Append :: !l
	 | 'c' -> l := `Create :: !l
	 | 'd' -> l := `Delete :: !l
	 | 'e' -> l := `Enter :: !l
	 | 'f' -> l := `Rename :: !l
	 | 'l' -> l := `List :: !l
	 | 'm' -> l := `Mkdir :: !l
	 | 'p' -> l := `Delete_member :: !l
	 | 'r' -> l := `Read :: !l
	 | 'w' -> l := `Write :: !l
	 | _ -> ()
    )
    p;
  List.rev !l

let get_lang (_,e) =
  List.assoc "lang" e

let get_media_type (_,e) =
  List.assoc "media-type" e

let get_charset (_,e) =
  List.assoc "charset" e

let get_unix_mode (_,e) =
  int_of_string ("0o" ^ List.assoc "unix.mode" e)

let get_unix_uid (_,e) =
  List.assoc "unix.uid" e

let get_unix_gid (_,e) =
  List.assoc "unix.gid" e

let get_name (n,_) = n


exception Esys_exit


class ftp_client 
        ?(event_system = Unixqueue.create_unix_event_system())
        () =
  let proxy = ref None in
  let pi_opt = ref None in
  let timeout = ref 300.0 in

  let get_pi() =
    match !pi_opt with
      | None ->
	  let pi =
	    new ftp_client_pi_impl
	      ~event_system 
	      ~timeout:!timeout
	      ?proxy:!proxy
	      () in
	  pi_opt := Some pi;
	  pi
      | Some pi ->
	  pi 
  in

object(self)
  method event_system = event_system

  method configure_timeout t =
    timeout := t

  method set_socks5_proxy h p =
    proxy := Some(new Uq_socks5.proxy_client 
                    (`Socket(`Sock_inet_byname(Unix.SOCK_STREAM,
					       h,p),
                               Uq_engines.default_connect_options)))

  method reset() =
    match !pi_opt with
      | None -> ()
      | Some pi ->
	  pi#abort();
	  pi_opt := None

  method run () = Unixqueue.run event_system

  method exec_e (m : ftp_method) =
    ( let pi = get_pi() in
      match pi#state with
	| `Done _ | `Error _ | `Aborted ->
	    self # reset()
	| _ ->
	    ()
    );
    m (get_pi())
    >> (function
	  | `Error e -> `Error e
	  | st -> st
       )

  method exec (m : ftp_method) =
    let throw x =
      let g = Unixqueue.new_group event_system in
      Unixqueue.once event_system g 0.0 (fun () -> raise x) in

    let e = self#exec_e m in
    ignore(
      (* We use ">>" and not [when_state] for observing, because the latter
	 does not catch the case that [e] is immediately entering a final
	 state.
       *)
      e >>
	(function
	   | `Done () -> throw Esys_exit; `Done ()
	   | `Error e -> throw e; `Done ()
	   | `Aborted -> throw (Failure "engine has been aborted"); `Done ()
	)
    );
    try
      Unixqueue.run event_system;
    with
      | Esys_exit -> ()

  method pi =
    match !pi_opt with
      | None -> failwith "Ftp_client: no protocol interpreter active"
      | Some pi -> pi
end


let () =
  Netsys_signal.init()
