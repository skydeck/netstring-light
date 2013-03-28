(* 
 * $Id: uq_socks5.ml 1662 2011-08-29 23:05:06Z gerd $
 *)

open Uq_engines;;

exception Socks_error of string;;

let socks_error s =
  raise (Socks_error s);;

let addr_of_name name =
  let entry = Uq_resolver.get_host_by_name name in
  entry.Unix.h_addr_list.(0)
;;

class [ 't ] engine_mixin (init_state : 't engine_state) =
object(self)
  val mutable notify_list = []
  val mutable notify_list_new = []
  val mutable state = init_state

  method state = state

  method request_notification f =
    notify_list_new <- f :: notify_list_new
    
  method private set_state s =
    if s <> state then (
      state <- s;
      self # notify();
    )

  method private notify() =
    notify_list <- notify_list @ notify_list_new;
    notify_list_new <- [];
    notify_list <- List.filter (fun f -> f()) notify_list
end ;;


class message_receiver src message_callback ues =
object(self)
  (* This engine reads messages of a fixed length from the descriptor
   * [src].
   *
   * The method [expect] defines how long the next message will be. If
   * such a message has arrived, the [message_callback] is called.
   * You can get the message by invoking [message].
   *
   * At EOF (within the message) the engine goes to error state.
   *
   * The engine is never done.
   *)

  inherit [unit] engine_mixin (`Working 0 : unit engine_state)

  val group = Unixqueue.new_group ues
  val mutable in_eof = false
  val mutable in_polling = false

  val mutable message = ""
    (* The message buffer *)

  val mutable message_length = 0
    (* How many bytes of the message buffer are used *)

  initializer
    (* Define the event handler: *)
    Unixqueue.add_handler ues group (fun _ _ -> self # handle_event);
    (* Define the abort (exception) handler: *)
    Unixqueue.add_abort_action ues group (fun _ -> self # handle_exception);
    (* Initially, we do not poll *)

  method message =
    if message_length = String.length message then
      message
    else
      failwith "incomplete message"

  method expect n =
    (* Reset the message, and expect that the next message will have 
     * n bytes 
     *)
    message <- String.create n;
    message_length <- 0;
    self # check_input_polling()

  method abort() =
    match state with
	`Working _ ->
	  in_eof <- true;
	  self # set_state `Aborted;
	  Unixqueue.clear ues group
      | _ ->
	  ()

  method event_system = ues

  method private count() =
    match state with
	`Working n -> 
	  self # set_state (`Working (n+1))
      | _ ->
	  ()

  method private handle_event ev =
    match ev with
	Unixqueue.Input_arrived(g,_) when g = group ->
	  self # handle_input();
	  (* Note: the engine can now be in the state `Aborted, because the
	   * message_callback was invoked
	   *)
	  self # check_input_polling();
      | _ ->
	  raise Equeue.Reject


  method private handle_exception exn =
    (* Unixqueue already ensures that the whole group will be deleted,
     * so we need not to do it here
     *)
    in_eof <- true;
    self # set_state (`Error exn)


  method private handle_input() =
    let message_expected = String.length message in
    if not in_eof && message_length < message_expected then
      try
	let n = Unix.read 
		  src message message_length (message_expected-message_length)
	in
	message_length <- message_length + n;
	in_eof <- (n = 0);
	if message_length = message_expected then
	  message_callback()
	else if in_eof then 
	  self # set_state (`Error (Sys_error "Unexpected end of file"));
	self # count();
      with
	  Unix.Unix_error(Unix.EAGAIN,_,_)
	| Unix.Unix_error(Unix.EWOULDBLOCK,_,_)
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    (* These exceptions are expected, and can be ignored *)
	    ()
	| error ->
	    (* Any other exception stops the engine *)
	    raise(Unixqueue.Abort(group,error))
	    

  method private check_input_polling() =
    (* It is possible that this method is called when the engine is already
     * aborted. In this case, just do nothing.
     *)
    match state with
	`Working _ ->
	  let message_expected = String.length message in
	  let need_polling = not in_eof && message_length < message_expected in
	  ( if need_polling && not in_polling then
	      Unixqueue.add_resource ues group (Unixqueue.Wait_in src, -1.0)
	    else
	      if not need_polling && in_polling then
		Unixqueue.remove_resource ues group (Unixqueue.Wait_in src);
	  );
	  in_polling <- need_polling
      | _ ->
	  ()

end
;;


let create_socks_request rq_type sockspec =
  let dst_addr_len = 
    match sockspec with
	`Sock_inet(_, ip, port) ->
	  if port < 0 || port > 65535 then socks_error "Illegal port";
	  ( match Netsys.domain_of_inet_addr ip with
	      | Unix.PF_INET -> 4
	      | Unix.PF_INET6 -> 16
	      | _ -> assert false
	  )
      | `Sock_inet_byname(_, name, port) ->
	  if port < 0 || port > 65535 then socks_error "Illegal port";
	  let l = String.length name in
	  if l > 255 then socks_error "Name too long";
	  1 + l
      | _ ->
	  assert false
	  (* should be caught earlier *)
  in
  let len = 6 + dst_addr_len in
  let rq = String.create len in
  rq.[0] <- '\005';
  rq.[1] <- (match rq_type with
		 `Connect       -> '\001'
	       | `Bind          -> '\002'
	       | `UDP_associate -> '\003'
	    );
  rq.[2] <- '\000';
  ( match sockspec with
	`Sock_inet(_, addr, port) ->
	  ( match Netsys.domain_of_inet_addr addr with
	      | Unix.PF_INET ->
		  rq.[3] <- '\001';
		  let p = Netsys.protostring_of_inet_addr addr in
		  String.blit p 0 rq 4 4;
		  rq.[8] <- Char.chr (port lsr 8);
		  rq.[9] <- Char.chr (port land 0xff);
	      | Unix.PF_INET6 ->
		  rq.[3] <- '\004';
		  let p = Netsys.protostring_of_inet_addr addr in
		  String.blit p 0 rq 4 16;
		  rq.[20] <- Char.chr (port lsr 8);
		  rq.[21] <- Char.chr (port land 0xff);
	      | _ -> assert false
	  )
       | `Sock_inet_byname(_, name, port) ->
	  rq.[3] <- '\003';
	  let l = String.length name in
	  rq.[4] <- Char.chr l;
	  String.blit name 0 rq 5 l;
	  rq.[5+l] <- Char.chr (port lsr 8);
	  rq.[6+l] <- Char.chr (port land 0xff);
      | _ ->
	  assert false
  );
  rq
;;


let fill_udp_header buf sockspec =
  let dst_addr_len = 
    match sockspec with
	`Sock_inet(_, ip, port) ->
	  if port < 0 || port > 65535 then socks_error "Illegal port";
	  ( match Netsys.domain_of_inet_addr ip with
	      | Unix.PF_INET -> 4
	      | Unix.PF_INET6 -> 16
	      | _ -> assert false
	  )
      | `Sock_inet_byname(_, name, port) ->
	  if port < 0 || port > 65535 then socks_error "Illegal port";
	  let l = String.length name in
	  if l > 255 then socks_error "Name too long";
	  1 + l
      | _ ->
	  assert false
	  (* should be caught earlier *)
  in
  let len = 6 + dst_addr_len in
  buf.[0] <- '\000';
  buf.[1] <- '\000';
  buf.[2] <- '\000';   (* no fragmentation *)
  ( match sockspec with
	`Sock_inet(_, addr, port) ->
	  ( match Netsys.domain_of_inet_addr addr with
	      | Unix.PF_INET ->
		  buf.[3] <- '\001';
		  let p = Netsys.protostring_of_inet_addr addr in
		  String.blit p 0 buf 4 4;
		  buf.[8] <- Char.chr (port lsr 8);
		  buf.[9] <- Char.chr (port land 0xff);
	      | Unix.PF_INET6 ->
		  buf.[3] <- '\004';
		  let p = Netsys.protostring_of_inet_addr addr in
		  String.blit p 0 buf 4 16;
		  buf.[20] <- Char.chr (port lsr 8);
		  buf.[21] <- Char.chr (port land 0xff);
	      | _ -> assert false
	  )
       | `Sock_inet_byname(_, name, port) ->
	  buf.[3] <- '\003';
	  let l = String.length name in
	  buf.[4] <- Char.chr l;
	  String.blit name 0 buf 5 l;
	  buf.[5+l] <- Char.chr (port lsr 8);
	  buf.[6+l] <- Char.chr (port land 0xff);
      | _ ->
	  assert false
  );
  len
;;


exception Fragmented

let read_udp_header buf len =
  if buf.[0] <> '\000' || buf.[1] <> '\000' then
    socks_error "Protocol error";

  if buf.[2] <> '\000' then raise Fragmented;

  match buf.[3] with
      '\001' ->
	(* IP version 4 address *)
	if len < 10 then socks_error "Protocol error";
	let addr = Netsys.inet_addr_of_protostring (String.sub buf 4 4) in
	let p1 = Char.code buf.[8] in
	let p0 = Char.code buf.[9] in
	let port = (p1 lsl 8) lor p0 in
	(10, `Sock_inet(Unix.SOCK_DGRAM, addr, port))
    | '\004' ->
	(* IP version 6 address *)
	if len < 22 then socks_error "Protocol error";
	let addr = Netsys.inet_addr_of_protostring (String.sub buf 4 16) in
	let p1 = Char.code buf.[20] in
	let p0 = Char.code buf.[21] in
	let port = (p1 lsl 8) lor p0 in
	(10, `Sock_inet(Unix.SOCK_DGRAM, addr, port))
    | '\003' ->
	(* Domainname *)
	let namelen = Char.code buf.[4] in
	if len < 7+namelen then socks_error "Protocol error";
	let name = String.sub buf 5 namelen in
	let p1 = Char.code buf.[5+namelen] in
	let p0 = Char.code buf.[6+namelen] in
	let port = (p1 lsl 8) lor p0 in
	(7+namelen, `Sock_inet_byname(Unix.SOCK_DGRAM, name, port))
    | _ ->
	socks_error "Protocol error";
;;



class socks5_datagram_socket socks_sock udp_sock 
      : wrapped_datagram_socket =
object(self)
  val buf = String.create 8192
	      (* Sufficient for UDP *)

  val mutable active = true

  method descriptor = udp_sock

  method sendto s p n flags addrspec =
    if p < 0 || n < 0 || p+n > String.length s then
      invalid_arg "sendto";
    if self#check_eof() then self#shut_down();
    if not active then socks_error "Proxy connection broken";
    let k = fill_udp_header buf addrspec in
    if k+n > String.length buf then
      (* socks_error "Datagram too large" *)
      raise(Unix.Unix_error(Unix.EMSGSIZE, "sendto(SOCKS)", ""));
    String.blit s p buf k n;
    let m = Unix.send udp_sock buf 0 (k+n) flags in
    m - k
    
  method recvfrom s p n flags =
    if p < 0 || n < 0 || p+n > String.length s then
      invalid_arg "recvfrom";
    if self#check_eof() then self#shut_down();
    if not active then socks_error "Proxy connection broken";
    try
      let m = Unix.recv udp_sock buf 0 (String.length buf) flags in
      let (k, addrspec) = read_udp_header buf m in
      let m' = min (m-k) n in
      String.blit buf k s p m';
      (m', addrspec)
    with
	Fragmented ->
	  raise(Unix.Unix_error(Unix.EAGAIN, "recvfrom(SOCKS)", ""))

  method private check_eof() =
    (* Is there an EOF? - Assumes that socks_sock is non-blocking *)
    let b = String.make 1 ' ' in
    try
      let n = Unix.read socks_sock b 0 1 in
      n = 0
    with
	Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
	  false

  method shut_down () =
    if active then (
      Unix.close socks_sock;
      Unix.close udp_sock;
      active <- false;
    )
      (* We silently ignore multiple shut_down calls *)

  method datagram_type = `Inet_udp
  method socket_domain = Unix.PF_INET
  method socket_type = Unix.SOCK_DGRAM
  method socket_protocol = 0
end;;


(* The automaton:
 * - socks_state is the current transition. The transition takes place when
 *   the next message arrives. The transition returns the next transition
 *   (Trans t'), or Finish.
 * - socks_out: Strings written to this buffer are sent to the SOCKS
 *   server (asynchronously). For convenience, use self#output.
 * - socks_in: The message buffer receiving messages from the SOCKS server
 *   of a certain length.
 * - The automaton is also an engine. The state goes to 
 *   [`Done status] when the proxy connection is established and 
 *   configured, where [status] is the connect_status.
 * - socks_socket is never closed
 *
 * TODO:
 * - For simplicity, the engine state counts transitions, not packets.
 *)

type 't transition =
    Finish of 't
  | Trans of (unit -> 't transition)
;;


class virtual ['t] automaton socks_socket ues =
object(self)
  inherit ['t] engine_mixin (`Working 0)

  val mutable socks_state = Trans(fun () -> assert false)

  val mutable socks_out = lazy (assert false)
  val mutable socks_in = lazy (assert false)

  initializer
    socks_out <- lazy(new output_async_descr 
			~close_dst:false ~dst:socks_socket ues);
    socks_in <- lazy(new message_receiver 
		       socks_socket self#msg_received ues);
    let s_out = Lazy.force socks_out in
    let s_in  = Lazy.force socks_in in

    (* Add some error handling: *)
    when_state
      ~is_error:(fun x -> 
		   s_out # abort();
		   self # set_state (`Error x);
		)
      s_in;

    when_state
      ~is_error:(fun x -> 
		   s_in # abort();
		   self # set_state (`Error x);
		)
      s_out;


  method private msg_received () =
    try
      ( match state with
	    `Working n ->
	      self # set_state (`Working (n+1))
	  | _ ->
	      assert false
      );
      let next_state = 
	match socks_state with
	    Trans f  -> f() 
	  | Finish _ -> assert false
      in
      socks_state <- next_state;
      ( match next_state with
	    Finish arg ->
	      (* Important: By aborting these two engines, the underlying 
	       * resources are removed from the event system. This MUST happen
	       * before the finishing action is carried out, which usually 
	       * triggers another callback that expects that the resources are
	       * available.
	       *)
	      (Lazy.force socks_out) # abort();
	      (Lazy.force socks_in) # abort();
	      self # finish arg;
	  | _ -> 
	      ()
      )
    with
	error ->
	  (Lazy.force socks_out) # abort();
	  (Lazy.force socks_in) # abort();
	  self # set_state (`Error error);

  method private virtual finish : 't -> unit
    
  method private output s =
    let n = (Lazy.force socks_out) # output s 0 (String.length s) in
    assert(n = String.length s)
      (* because socks_out is unconstrained *)

  (* The engine interface: *)

  method abort () =
    (Lazy.force socks_out) # abort();
    (Lazy.force socks_in) # abort();
    self # set_state `Aborted

  method event_system = ues

end
;;


let hello_and_authentication output socks_in followup =
  let rec start_state() =
    let s = "\005\001\000" in
    (* SOCKS Version 5; One auth method; Auth method 0
     * = no authentication
     *)
    output s;
    socks_in # expect 2;
    Trans got_auth_method_state

  and got_auth_method_state() =
    (* Interpret incoming message: *)
    let msg = socks_in # message in
    assert(String.length msg = 2);
    if msg.[0] <> '\005' then
      socks_error "Bad SOCKS protocol version";
    if msg.[1] <> '\000' then
      socks_error "Authentication method not supported";
    followup()

  in
  start_state
;;


let receive_socks_reply socks_in stype followup =
  (* Initially expects a four byte message *)

  let rec got_reply_1_state() =
    (* Interpret incoming message: The first four bytes
     * of the reply (until ATYP field) 
     *)
    let msg = socks_in # message in
    assert(String.length msg = 4);
    if msg.[0] <> '\005' then
      socks_error "Bad SOCKS protocol version";
    if msg.[1] <> '\000' then (
      match msg.[1] with
	  '\001' -> socks_error "General SOCKS server failure"
	| '\002' -> socks_error "Connection not allowed by ruleset"
	| '\003' -> socks_error "Network is unreachable"
	| '\004' -> socks_error "Host is unreachable"
	| '\005' -> socks_error "Connection refused"
	| '\006' -> socks_error "TTL expired"
	| '\007' -> socks_error "Command not supported"
	| '\008' -> socks_error "Address type not supported"
	| _      -> socks_error "Protocol error (1)"
    );
    match msg.[3] with
	'\001' ->  (* IPV4 address *)
	  socks_in # expect 4;
	  Trans got_reply_ipv4_state
      | '\002' ->  (* domainname *)
	  socks_in # expect 1;  (* length of domainname *)
	  Trans got_reply_domainname_state
      | '\003' ->  (* IPV6 address *)
	  socks_in # expect 16;
	  Trans got_reply_ipv6_state
      | _ ->
	  socks_error "Protocol error (2)";

  and got_reply_ipv4_state() =
    (* Interpret incoming message: Four bytes forming an IPv4 address *)
    let msg = socks_in # message in
    assert(String.length msg = 4);
    let addr = Netsys.inet_addr_of_protostring msg in
    let bnd_spec = `Sock_inet(stype, addr, 0) in
    socks_in # expect 2;
    Trans (got_reply_port_state bnd_spec)

  and got_reply_ipv6_state() =
    (* Interpret incoming message: 16 bytes forming an IPv6 address *)
    let msg = socks_in # message in
    assert(String.length msg = 16);
    let addr = Netsys.inet_addr_of_protostring msg in
    let bnd_spec = `Sock_inet(stype, addr, 0) in
    socks_in # expect 2;
    Trans (got_reply_port_state bnd_spec)

  and got_reply_domainname_state() =
    (* Interpret incoming message: The length of the domainname *)
    let msg = socks_in # message in
    assert(String.length msg = 1);
    let n = Char.code msg.[0] in
    socks_in # expect n;
    Trans (got_reply_dnstring_state n)

  and got_reply_dnstring_state n () =
    (* Interpret incoming message: The domainname string *)
    let msg = socks_in # message in
    assert(String.length msg = n);
    let bnd_spec = `Sock_inet_byname(Unix.SOCK_STREAM, msg, 0) in
    socks_in # expect 2;
    Trans (got_reply_port_state bnd_spec)

  and got_reply_port_state bnd_prelim_spec () =
    (* Interpret incoming message: The port number *)
    let msg = socks_in # message in
    assert(String.length msg = 2);
    let port = 
      (Char.code msg.[0] lsl 8) lor (Char.code msg.[1]) in
    let bnd_spec =
      match bnd_prelim_spec with
	  `Sock_inet(stype, addr, _) -> 
	    `Sock_inet(stype, addr, port)
	| `Sock_inet_byname(stype, name, _) -> 
	    `Sock_inet_byname(stype, name, port)
	| _ -> bnd_prelim_spec
    in
    followup bnd_spec

  in
  got_reply_1_state
;;


class socks5_datagram_automaton socks_socket udp_socket ues =
object(self)
  inherit [ wrapped_datagram_socket ] automaton socks_socket ues

  initializer
    let socks_in = Lazy.force socks_in in

    let rec start_state() =
      hello_and_authentication self#output socks_in send_udpassoc_request ()

    and send_udpassoc_request() =
      (* Create next request: *)
      let client_spec =
	match Unix.getsockname udp_socket with
	    Unix.ADDR_INET(_, client_port) ->
	      (* We do not use the inet addr of udp_socket because it
	       * is 0.0.0.0! Instead, take the address of socks_socket.
	       * This implicitly assumes that both remote sockets can
	       * be reached via the same interface.
	       *)
	      ( match Unix.getsockname socks_socket with
		    Unix.ADDR_INET(client_addr, _) ->
		      `Sock_inet(Unix.SOCK_DGRAM, client_addr, client_port)
		  | _ ->
		      assert false
	      )
	  | _ ->
	      assert false in
      let s =
	create_socks_request `UDP_associate client_spec in
      self # output s;
      socks_in # expect 4;
      Trans(receive_socks_reply socks_in Unix.SOCK_DGRAM got_udpassoc_reply)

    and got_udpassoc_reply bnd_spec =
      let bnd_addr =
	match bnd_spec with
	    `Sock_inet(_, addr, port) ->
	      Unix.ADDR_INET(addr,port)
	  | `Sock_inet_byname(_, name, port) ->
	      let addr = addr_of_name name in
	      Unix.ADDR_INET(addr,port)
	  | _ ->
	      assert false in
      Unix.connect udp_socket bnd_addr;
      let wsock = new socks5_datagram_socket socks_socket udp_socket in
      Finish wsock

    in
    socks_state <- start_state();

    method private finish arg =
      self # set_state (`Done arg)

end
;;



(* PROBLEM: ~proxy does not work for datagrams *)

class socks5_datagram_provider ?proxy socks_connaddr 
      : datagram_socket_provider =
object(self)

  method create_datagram_socket dgtype ues =
    if dgtype <> `Inet_udp then 
      raise Addressing_method_not_supported;

    (* First connect to the SOCKS proxy; if connected, invoke
     * the SOCKS state automaton
     *)
    let proxy_conn_eng = connector ?proxy socks_connaddr ues in
    new seq_engine
      proxy_conn_eng
      (fun conn_stat ->
	 let socks_socket = client_endpoint conn_stat in
	 let udp_socket = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
	 Unix.set_nonblock udp_socket;
	 Netsys.set_close_on_exec udp_socket;
	 Unix.bind udp_socket (Unix.ADDR_INET(Unix.inet_addr_any,0));
	 let eng =
	   new socks5_datagram_automaton socks_socket udp_socket ues in
	 let close_both _ = Unix.close socks_socket; Unix.close udp_socket in
	 when_state
	   ~is_error:close_both
	   ~is_aborted:close_both
	   eng;
	 eng
      )
end
;;


class socks5_connect_automaton socks_socket connect_spec ues =
object(self)
  inherit [connect_status] automaton socks_socket ues

  initializer
    let socks_in = Lazy.force socks_in in

    let rec start_state() =
      hello_and_authentication self#output socks_in send_connect_request ()

    and send_connect_request() =
      (* Create next request: *)
      let s =
	create_socks_request `Connect connect_spec in
      self # output s;
      socks_in # expect 4;
      Trans(receive_socks_reply socks_in Unix.SOCK_STREAM got_connect_reply)

    and got_connect_reply bnd_spec =
      Finish(`Socket(socks_socket, bnd_spec))

    in
    socks_state <- start_state();

    method private finish arg =
      self # set_state (`Done arg)

end
;;


class socks5_socket_connector ?proxy socks_connaddr : client_socket_connector =
object(self)

  method connect connaddr ues =
    match connaddr with
	`Socket (sockspec, opts) ->
	  (* Check whether sockspec is supported: *)
	  ( match sockspec with
		`Sock_inet(Unix.SOCK_STREAM, _, _) -> ()
	      | `Sock_inet_byname(Unix.SOCK_STREAM, _, _) -> ()
	      | _ ->
		  raise Addressing_method_not_supported
	  );

	  (* opts.conn_bind is ignored: not supported by SOCKS *)

	  (* First connect to the SOCKS proxy; if connected, invoke
	   * the SOCKS state automaton
	   *)
	  let proxy_conn_eng = connector ?proxy socks_connaddr ues in
	  new seq_engine
	    proxy_conn_eng
	    (fun conn_stat ->
	       let socks_socket = client_endpoint conn_stat in
	       let eng =
		 new socks5_connect_automaton socks_socket sockspec ues in
	       when_state
		 ~is_error:(fun _ -> Unix.close socks_socket)
		 ~is_aborted:(fun _ -> Unix.close socks_socket)
		 eng;
	       eng
	    )

      | _ ->
	  raise Addressing_method_not_supported

end
;;


class socks5_accept_automaton notify socks_socket ues =
object(self)
  inherit [Unix.file_descr * inetspec option] automaton socks_socket ues

  initializer
    let socks_in = Lazy.force socks_in in

    let rec continue() =
      (* Wait for the next reply of the SOCKS proxy: *)
      socks_in # expect 4;
      Trans(receive_socks_reply socks_in Unix.SOCK_STREAM got_accept_reply)

    and got_accept_reply client_spec =
      notify socks_socket;
      Finish(socks_socket, Some client_spec)

    in
    socks_state <- continue();

    method private finish arg =
      self # set_state (`Done arg)

end
;;


class socks5_socket_acceptor socks_socket actual_bind_spec ues
      : server_socket_acceptor =
  (* actual_bind_spec: The bind address the SOCKS proxy really uses *)
object(self)

  val mutable acc_engine = None
			     (* The engine currently accepting connections *)

  val mutable responsible_for_socks_socket = true

  method server_address = `Socket(actual_bind_spec,
				  Uq_engines.default_connect_options)

  method multiple_connections = false

  method accept () =
    if acc_engine <> None then
      failwith "Uq_socks5.socks5_socket_acceptor:  Resource busy";
    let eng = new socks5_accept_automaton self#release_socket socks_socket ues in
    acc_engine <- Some eng;
    eng

      (* Note: If eng generates errors, we do not close socks_socket
       * for compatibility with multi-connection listeners
       *)


  method private release_socket _ =
    responsible_for_socks_socket <- false

  method shut_down() =
    if responsible_for_socks_socket then
      Unix.close socks_socket;
    match acc_engine with
	None -> 
	  ()
      | Some acc -> 
	  acc # abort()
end 
;;


class socks5_listen_automaton socks_socket bind_spec ues =
object(self)
  inherit [server_socket_acceptor] automaton socks_socket ues

  initializer
    let socks_in = Lazy.force socks_in in

    let rec start_state() =
      hello_and_authentication self#output socks_in send_bind_request ()

    and send_bind_request() =
      (* Create next request: *)
      let s =
	create_socks_request `Bind bind_spec in
      self # output s;
      socks_in # expect 4;
      Trans(receive_socks_reply socks_in Unix.SOCK_STREAM got_bind_reply)

    and got_bind_reply bnd_spec =
      let acceptor = new socks5_socket_acceptor socks_socket bnd_spec ues in
      Finish acceptor

    in
    socks_state <- start_state();

    method private finish arg =
      self # set_state (`Done arg)
end
;;


class socks5_socket_listener ?proxy socks_connaddr : server_socket_listener =
object(self)
  method listen lstnaddr ues =
    match lstnaddr with
	`Socket (sockspec, opts) ->
	  (* Check whether sockspec is supported: *)
	  ( match sockspec with
		`Sock_inet(Unix.SOCK_STREAM, _, _) -> ()
	      | `Sock_inet_byname(Unix.SOCK_STREAM, _, _) -> ()
	      | _ ->
		  raise Addressing_method_not_supported
	  );

	  (* First connect to the SOCKS proxy; if connected, invoke
	   * the SOCKS state automaton
	   *)
	  let proxy_conn_eng = connector ?proxy socks_connaddr ues in
	  new seq_engine
	    proxy_conn_eng
	    (fun conn_stat ->
	       let socks_socket = client_endpoint conn_stat in
	       let eng = new socks5_listen_automaton socks_socket sockspec ues
	       in
	       when_state
		 ~is_error:(fun _ -> Unix.close socks_socket)
		 ~is_aborted:(fun _ -> Unix.close socks_socket)
		 eng;
	       eng
	    )
	  

      | _ ->
	  raise Addressing_method_not_supported
end 
;;
    

class proxy_client socks_connaddr =
object(self)
  inherit socks5_socket_connector  socks_connaddr
  inherit socks5_socket_listener   socks_connaddr
  inherit socks5_datagram_provider socks_connaddr
end
;;
