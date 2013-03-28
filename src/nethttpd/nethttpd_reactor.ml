(* $Id: nethttpd_reactor.ml 1623 2011-06-13 12:06:50Z gerd $
 *
 *)

(*
 * Copyright 2005 Baretta s.r.l. and Gerd Stolpmann
 *
 * This file is part of Nethttpd.
 *
 * Nethttpd is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Nethttpd is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Nethttpd; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Nethttpd_reactor" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Nethttpd_reactor" Debug.enable

let () =
  Netlog.Debug.register_module "Nethttpd_reactor" Debug.enable


open Nethttp
open Nethttp.Header
open Nethttpd_types
open Nethttpd_kernel
open Netchannels
open Printf

class type http_processor_config =
object
  inherit Nethttpd_kernel.http_protocol_config
  method config_timeout_next_request : float
  method config_timeout : float
  method config_cgi : Netcgi.config
  method config_error_response : error_response_params -> string
  method config_log_error : request_info -> string -> unit
  method config_log_access : full_info -> unit
end

class type http_reactor_config =
object
  inherit http_processor_config
  method config_reactor_synch : [ `Connection | `Close | `Flush | `Write ]
end


class type internal_environment =
object
  inherit extended_environment

  method unlock : unit -> unit
  method req_method : http_method
  method response : http_response
  method log_access : unit -> unit
end


class type http_reactive_request =
object
  method environment : extended_environment
  method accept_body : unit -> unit
  method reject_body : unit -> unit
  method finish : unit -> unit
  method finish_request : unit -> unit
end


let get_this_host addr =
  match addr with
    | Unix.ADDR_UNIX path ->
	("", None)   (* questionable *)
    | Unix.ADDR_INET(ia,port) ->
	(Unix.string_of_inet_addr ia, Some port)


let logged_error_response fd_addr peer_addr req_meth_uri_opt 
                          in_cnt req_rej status hdr_opt msg_opt env_opt 
                          resp_opt config =
  let unopt = 
    function Some x -> x | None -> raise Not_found in
  let msg, have_msg =
    match msg_opt with
      | Some msg -> msg, true
      | None -> "", false in
  let code = int_of_http_status status in
  let info =
    ( object
	method server_socket_addr = fd_addr
	method remote_socket_addr = peer_addr
	method request_method = fst(unopt req_meth_uri_opt)
	method request_uri = snd(unopt req_meth_uri_opt)
	method input_header = (unopt env_opt) # input_header
	method cgi_properties = (unopt env_opt) # cgi_properties
	method input_body_size = in_cnt
	method response_status_code = code
	method error_message = msg
      end
    ) in
  if have_msg then
    config # config_log_error (info:>request_info) msg;
  match resp_opt with
    | None -> ()
    | Some resp ->
	let body = config # config_error_response info in
	let eff_header = 
	  match hdr_opt with
	    | None ->
		new Netmime.basic_mime_header []
	    | Some hdr ->
		hdr in
	Nethttpd_kernel.send_static_response 
	  resp status (Some eff_header) body;
	let full =
	  new create_full_info
	    ~response_status_code:code
	    ~request_body_rejected:req_rej
	    ~output_header:eff_header
	    ~output_body_size:(Int64.of_int (String.length body))
	    (info:>request_info) in
	config # config_log_access full


let no_info =
  ( object
      method server_socket_addr = raise Not_found
      method remote_socket_addr = raise Not_found
      method request_method = raise Not_found
      method request_uri = raise Not_found
      method input_header = raise Not_found
      method cgi_properties = raise Not_found
      method input_body_size = raise Not_found
    end
  )


class http_environment (proc_config : #http_processor_config)
                       req_meth req_uri req_version req_hdr 
                       fd_addr peer_addr
                       in_ch in_cnt 
                       out_ch output_state resp after_send_file
		       reqrej fdi
                      : internal_environment =

  (* Decode important input header fields: *)
  let (in_host, in_port_opt) =
    (* Host and port of the [Host] header *)
    try get_host req_hdr
    with 
      | Not_found -> 
	  (* For HTTP/1.1 and later this is not allowed. For earlier protocols, we
	   * just fill in the IP address that accepted the request. 
	   *)
	  ( match req_version with
	      | `Http((1,n),_) when n>= 1 ->
		  raise(Standard_response(`Bad_request, 
				        None,
				        Some "Nethttpd: Bad request: [Host] header is missing"))
	      | _ ->
		  get_this_host fd_addr
	  )
      | Bad_header_field _ ->
	  raise(Standard_response(`Bad_request,
                                   None,
			        Some "Nethttpd: Bad request: Cannot decode [Host] header")) in

  let (script_name, query_string) = decode_query req_uri in

(*
  let full_uri =
    "http://" ^ in_host ^ 
    (match in_port with Some n -> ":" ^ string_of_int n | None -> "") ^ 
    req_uri
 *)

object(self)
  inherit empty_environment

  val mutable locked = true

  val mutable logged_props = []

  val out_state = (output_state : output_state ref)

  initializer (
    config <- proc_config # config_cgi;
    in_header <- req_hdr;
    in_channel <- in_ch;
    out_channel <- out_ch;
    protocol <- req_version;
    properties <- [ "GATEWAY_INTERFACE", "Nethttpd/0.0";
		  "SERVER_SOFTWARE",   "Nethttpd/0.0";
		  "SERVER_NAME",       in_host;
		  "SERVER_PROTOCOL",   string_of_protocol req_version;
		  "REQUEST_METHOD",    req_meth;
		  "SCRIPT_NAME",       script_name;
		  (* "PATH_INFO",         ""; *)
		  (* "PATH_TRANSLATED",   ""; *)
		  "QUERY_STRING",      query_string;
		  (* "REMOTE_HOST",       ""; *)
		  "REMOTE_ADDR",       fst(get_this_host peer_addr);
		  (* "AUTH_TYPE",         ""; *)
		  (* "REMOTE_USER",       ""; *)
		  (* "REMOTE_IDENT",      ""; *)
		  "HTTPS",             "off";
		  "REQUEST_URI",       req_uri;
		  ] @
                  ( match in_port_opt with
		      | Some p -> [ "SERVER_PORT", string_of_int p ]
		      | None   -> [] );
    logged_props <- properties
  )

  method unlock() =
    dlogr (fun () -> sprintf "FD=%Ld: env.unlock" fdi);
    locked <- false

  method server_socket_addr = fd_addr
  method remote_socket_addr = peer_addr

  method response = resp
  method req_method = (req_meth, req_uri)

  val mutable sent_status = 0
  val mutable sent_resp_hdr = new Netmime.basic_mime_header []

  method send_output_header() =
    dlogr (fun () -> sprintf "FD=%Ld: env.send_output_header out_state=%s"
	     fdi (string_of_output_state !out_state));
    if locked then failwith "Nethttpd_reactor: channel is locked";
    if !out_state = `Start then (
      (* The response status is encoded in the [Status] pseudo header *)
      let (code, phrase) = status_of_cgi_header out_header in
      resp # send (`Resp_status_line(code, phrase));
      (* Create a copy of the header without [Status]: *)
      let h = new Netmime.basic_mime_header out_header#fields in
      h # delete_field "Status";
      sent_status <- code;   (* remember what has been sent for access logging *)
      sent_resp_hdr <- h;
      resp # send (`Resp_header h);
      out_state := `Sending;
    )
      (* netcgi2 may call send_output_header several times, so we have to 
         ignore the later calls after the first one
       *)

  method send_file fd length =
    dlogr (fun () -> sprintf "FD=%Ld: env.send_file out_state=%s" 
	     fdi (string_of_output_state !out_state));
    if locked then failwith "Nethttpd_reactor: channel is locked";
    if !out_state <> `Start then
      failwith "send_file";
    (* The response status is encoded in the [Status] pseudo header *)
    let (code, phrase) = status_of_cgi_header out_header in
    let status = http_status_of_int code in
     (* Create a copy of the header without [Status]: *)
    let h = new Netmime.basic_mime_header out_header#fields in
    h # delete_field "Status";
    sent_status <- code;   (* remember what has been sent for access logging *)
    sent_resp_hdr <- h;
    send_file_response resp status (Some h) fd length;
    out_state := `Sending;
    after_send_file()

  method log_error s =
    let info =
      ( object 
	  method server_socket_addr = fd_addr
	  method remote_socket_addr = peer_addr
	  method request_method = req_meth
	  method request_uri = req_uri
	  method input_header = req_hdr
	  method cgi_properties = logged_props
	  method input_body_size = !in_cnt
	end
      ) in
    proc_config # config_log_error info s

  method log_props l =
    logged_props <- l


  val mutable access_logged = false

  method log_access () =
    (* Called when the whole response is written. Do now access logging *)
    if not access_logged then (
      let full_info =
	( object
	    method server_socket_addr = fd_addr
	    method remote_socket_addr = peer_addr
	    method request_method = req_meth
	    method request_uri = req_uri
	    method input_header = req_hdr
	    method cgi_properties = logged_props
	    method input_body_size = !in_cnt
	    method response_status_code = sent_status
	    method request_body_rejected = !reqrej
	    method output_header = sent_resp_hdr
	    method output_body_size = resp#body_size
	  end
	) in
      proc_config # config_log_access full_info;
      access_logged <- true
    )


  method input_body_size = !in_cnt
  method request_body_rejected = !reqrej

  method output_state = out_state
end


class http_reactor_input next_token in_cnt fdi =
  (* an extension of rec_in_channel *)
object(self)
  val mutable current_chunk = None
  val mutable eof = false
  val mutable closed = false
  val mutable locked = true

  method private refill() =
    dlogr (fun () -> sprintf "FD=%Ld: input.refill" fdi);
    match next_token() with
      | `Req_body(s,pos,len) ->
	  assert(len > 0);
	  in_cnt := Int64.add !in_cnt (Int64.of_int len);
	  current_chunk <- Some(s,pos,len)
      | `Req_trailer _ ->
	  self # refill ()   (* ignore *)
      | `Req_end ->
	  current_chunk <- None;
	  eof <- true;
	  raise End_of_file;
      | _ ->
	  (* Something else... Handle this as `Req_end! *)
	  current_chunk <- None;
	  eof <- true;
	  raise End_of_file;
	  

  method input s spos slen =
    dlogr (fun () -> sprintf "FD=%Ld: input.input" fdi);
    if closed then raise Closed_channel;
    if locked then failwith "Nethttpd_reactor: channel is locked";
    if eof then raise End_of_file;
    if current_chunk = None then self#refill();  (* may raise End_of_file *)
    match current_chunk with
      | Some(u,upos,ulen) ->
	  (* We have [ulen] data, copy that to [s] *)
	  let len = min slen ulen in
	  String.blit u upos s spos len;
	  let ulen' = ulen - len in
	  if ulen' = 0 then
	    current_chunk <- None
	  else
	    current_chunk <- Some(u,upos+len,ulen');
	  len
      | None ->
	  (* After [refill] this is not possible *)
	  assert false

  method close_in() =
    dlogr (fun () -> sprintf "FD=%Ld: input.close_in" fdi);
    if not closed then (
      if locked then failwith "Nethttpd_reactor: channel is locked";
      (* It is no problem to ignore further arriving tokens. These will be "eaten" by
       * [finish_request] later. (Of course, we could call [finish_request] here,
       * but that would probably defer the generation of responses.)
       *)
      closed <- true;
    )

  method unlock() =
    dlogr (fun () -> sprintf "FD=%Ld: input.unlock" fdi);
    locked <- false;

  method drop() =
    dlogr (fun () -> sprintf "FD=%Ld: input.drop" fdi);
    locked <- false;
    eof <- true

end


class http_reactor_output config resp synch out_state 
                          (f_access : unit -> unit) fdi =   
  (* an extension of rec_out_channel *)
object
  val mutable closed = false
  val mutable locked = true
  val mutable access_logged = false

  method output s spos slen =
    dlogr (fun () -> sprintf "FD=%Ld: output.output" fdi);
    if closed then raise Closed_channel;
    if locked then failwith "Nethttpd_reactor: channel is locked";
    if !out_state <> `Sending then 
      failwith "output channel: Cannot output now";
    let u = String.sub s spos slen in
    resp # send (`Resp_body(u, 0, String.length u));
    ( match config#config_reactor_synch with
	| `Write ->
	    synch()
	| _ ->
	    ()
    );
    slen

  method flush() =
    dlogr (fun () -> sprintf "FD=%Ld: output.flush" fdi);
    if closed then raise Closed_channel;
    if locked then failwith "Nethttpd_reactor: channel is locked";
    if !out_state = `Sending then 
      match config#config_reactor_synch with
	| `Write
	| `Flush ->
	    synch()
	| _ ->
	    ()
	      
  method close_out() =
    dlogr (fun () -> sprintf "FD=%Ld: output.close_out" fdi);
    if not closed then (
      if locked then failwith "Nethttpd_reactor: channel is locked";
      closed <- true;
      if !out_state = `Sending then
	resp # send `Resp_end;
      out_state := `End;
      ( match config#config_reactor_synch with
	  | `Write
	  | `Flush
	  | `Close ->
	      synch()
	  | _ ->
	      ()
      );
      (* This is the right time for writing the access log entry: *)
      if not access_logged then (
	f_access();
	access_logged <- true
      )
    )
      


  method after_send_file() =
    (* Flush after send_file. This method does not count as close_out,
       though. It is possible that more flushes follow.
     *)
    dlogr (fun () -> sprintf "FD=%Ld: output.after_send_file" fdi);
    out_state := `End;
    ( match config#config_reactor_synch with
	| `Write
	| `Flush
	| `Close ->
	    synch()
	| _ ->
	    ()
    );
    (* This is the right time for writing the access log entry: *)
    if not access_logged then (
      f_access();
      access_logged <- true
    )

  method unlock() =
    dlogr (fun () -> sprintf "FD=%Ld: output.unlock" fdi);
    locked <- false

end


class http_reactive_request_impl config env inch outch resp expect_100_continue
                                 finish_request reqrej
                                 : http_reactive_request =
  (* NB. inch and outch must only be used here to control the locks in these
     channels, not for doing real I/O
   *)
object(self)

  initializer (
    let (m,u) = env # req_method in
    dlogr (fun () ->
	     sprintf "req-%d: %s %s" 
	       (Oo.id self) m u
	  );
  )

  method environment = 
    (env : internal_environment :> extended_environment)

  method accept_body() =
    dlogr (fun () -> sprintf "req-%d: accept_body" (Oo.id self));
    if expect_100_continue then
      resp # send resp_100_continue;
    (* We need not to synch here! The attempt to read the body will synchronize
     * implicitly.
     * We should keep in mind, however, that when the existing body isn't read
     * the "100 Continue" might be transmitted very late. This is no disadvantage,
     * I think.
     *)
    inch # unlock();
    outch # unlock();
    env # unlock()

  method reject_body() =
    dlogr (fun () -> sprintf "req-%d: reject_body" (Oo.id self));
    inch # drop();
    outch # unlock();
    env # unlock();
    reqrej := true    (* for access logging only *)

  val mutable fin_req = false

  method finish_request() =
    if not fin_req then (    (* Do this only once *)
      dlogr (fun () -> sprintf "req-%d: finish_request" (Oo.id self));
      fin_req <- true;
      inch # drop();
      outch # unlock();
      env # unlock();
      finish_request();   (* Read the rest of the request until `Req_end *)
    )

  method finish() =
    self # finish_request();
    dlogr (fun () -> sprintf "req-%d: finish" (Oo.id self));
    match !(env#output_state) with
      | `Start ->
	  (* The whole response is missing! Generate a "Server Error": *)
	  dlogr (fun () -> 
		   sprintf "req-%d: no response, generating server error"
		     (Oo.id self));
	  output_std_response config env `Internal_server_error None 
	    (Some "Nethttpd: Missing response, replying 'Server Error'");
	  (env#output_state) := `End
      | `Sending ->
	  (* The response body is probably incomplete or missing. Try to close
	   * the channel.
	   *)
	  dlogr (fun () -> 
		   sprintf "req-%d: incomplete response, trying to fix"
		     (Oo.id self));
	  ( try env # output_ch # close_out() with Closed_channel -> () );
	  (env#output_state) := `End;
      | `End ->
	  (* Everything ok, just to be sure... *)
	  ( try env # output_ch # close_out() with Closed_channel -> () );
end



class http_reactor (config : #http_reactor_config) fd =
  (* note that "new http_reactor" can already raise exceptions, e.g.
     Unix.ENOTCONN
   *)
object(self)
  val proto = new http_protocol config fd
  val fd_addr = Unix.getsockname fd
  val peer_addr = Netsys.getpeername fd

  initializer (
    Netlog.Debug.track_fd
      ~owner:"Nethttpd_reactor"
      ~descr:(sprintf "HTTP %s->%s"
		(Netsys.string_of_sockaddr peer_addr)
		(Netsys.string_of_sockaddr fd_addr))
      fd
  )

  method private cycle() =
    let block = 
      if proto # waiting_for_next_message then 
	config#config_timeout_next_request
      else
	config#config_timeout in
    dlogr (fun () -> 
	     sprintf "FD %Ld: cycle block_tmo=%f" 
	       (Netsys.int64_of_file_descr fd) block);
    proto # cycle ~block ();

  method private next_token() =
    if proto # recv_queue_len = 0 then (
      self # cycle();
      self # next_token()
    )
    else (
      let tok = proto # receive() in
      dlogr (fun () ->
	       sprintf "FD %Ld: next_token=%s"
		 (Netsys.int64_of_file_descr fd)
		 (Nethttpd_kernel.string_of_req_token tok));
      tok
    )

  method private peek_token() =
    if proto # recv_queue_len = 0 then (
      self # cycle();
      self # peek_token()
    )
    else (
      let tok = proto # peek_recv() in
      dlogr (fun () ->
	       sprintf "FD %Ld: peek_token=%s"
		 (Netsys.int64_of_file_descr fd)
		 (Nethttpd_kernel.string_of_req_token tok));
      tok
    )

  method private finish_request() =
    (* Read the rest of the previous request, ignoring it *)
    dlogr (fun () -> 
	     sprintf "FD %Ld: reactor_finish_request" 
	       (Netsys.int64_of_file_descr fd));
    match self # peek_token() with
      | `Req_header _
      | `Eof
      | `Fatal_error _
      | `Bad_request_error _
      | `Timeout ->
	  (* Everything ok, do nothing *)
	  ()
      | `Req_end ->
	  (* Just drop this token, the next token starts the new request *)
	  ignore(proto # receive ())
      | `Req_expect_100_continue
      | `Req_body _
      | `Req_trailer _ ->
	  (* Continue to read this request until its end *)
	  while
	    match self # peek_token () with
	      | `Req_header _
	      | `Eof 
	      | `Fatal_error _
	      | `Bad_request_error _ 
	      | `Timeout ->
		  false   (* Do not read further *)
	      | _ ->
		  ignore(self # next_token());
		  true    (* Continue *)
	  do
	    ()
	  done


  method private synch() =
    (* Ensure that all written data are actually transmitted: *)
    dlogr (fun () -> 
	     sprintf "FD %Ld: synch loop" (Netsys.int64_of_file_descr fd));
    while proto # do_output do
      self # cycle();
    done;
    dlogr (fun () -> 
	     sprintf "FD %Ld: leaving synch loop"
	       (Netsys.int64_of_file_descr fd));
    (* CHECK: Maybe we have to throw away the remaining tokens of the current request! *)


  method next_request () =
    let tok = self # next_token() in
    match tok with
      | `Req_header (req, req_hdr, resp) ->
	  (* Ok, we have a new request. Initialize the new environment processing
	   * it
	   *)
	  dlogr (fun () -> 
		   sprintf "FD %Ld: reactor_next_request: got header" 
		     (Netsys.int64_of_file_descr fd));
	  let expect_100_continue =
	    try
	      proto # peek_recv() = `Req_expect_100_continue
	    with
		Recv_queue_empty -> false in
	  if expect_100_continue then
	    ignore(proto # receive());

	  let ((req_meth, req_uri), req_version) = req in

	  let env_opt = ref None in  (* set below *)
	  let in_cnt = ref 0L in
	  let input_ch = 
	    new http_reactor_input self#next_token in_cnt
	      (Netsys.int64_of_file_descr fd)  in
	  let output_state = ref `Start in
	  let output_ch = 
	    new http_reactor_output config resp self#synch 
	      output_state 
	      (fun () -> 
		 match !env_opt with
		   | None -> ()
		   | Some env -> env#log_access()
	      )
	      (Netsys.int64_of_file_descr fd) in
	  let lifted_input_ch = 
	    lift_in ~buffered:false (`Rec (input_ch :> rec_in_channel)) in
	  let lifted_output_ch = 
	    lift_out (`Rec (output_ch :> rec_out_channel)) in
	  (* The input channel needs no additional buffer here. The httpd kernel
	   * already implements a good buffer.
	   *
	   * The output channel profits from a buffer. The effect is that the
	   * kernel is driven with output chunks of uniform size. Furthermore,
	   * `Write synchronization is only performed after every of these chunks,
	   * and not after every output method invocation.
	   *)

	  let after_send_file() =
	    output_ch # after_send_file();
	    lifted_output_ch # close_out() in

	  let reqrej = ref false in
	  ( try
	      let env = new http_environment 
			      config 
                              req_meth req_uri req_version req_hdr 
                              fd_addr peer_addr
		              lifted_input_ch in_cnt 
			      lifted_output_ch output_state
			      resp after_send_file reqrej
			      (Netsys.int64_of_file_descr fd)
	      in
	      env_opt := Some env;
	      let req_obj = new http_reactive_request_impl 
			      config env input_ch output_ch resp expect_100_continue 
			      self#finish_request reqrej
	      in
	      dlogr (fun () -> 
		       sprintf 
			 "FD %Ld: reactor_next_request: returning req-%d" 
			 (Netsys.int64_of_file_descr fd)
			 (Oo.id req_obj));
	      Some req_obj
	    with
		Standard_response(status, hdr_opt, msg_opt) ->
		  (* Probably a problem when decoding a header field! *)
		  dlogr (fun () -> 
			   sprintf 
			     "FD %Ld: reactor_next_request: standard response" 
			     (Netsys.int64_of_file_descr fd));
		  logged_error_response
		    fd_addr peer_addr (Some(req_meth,req_uri))
		    !in_cnt !reqrej status hdr_opt msg_opt !env_opt
		    (Some resp) config;
		  self # synch();
		  self # finish_request();
		  self # next_request()
	  )

      | `Eof ->
	  dlogr (fun () -> 
		   sprintf 
		     "FD %Ld: reactor_next_request: got eof" 
		     (Netsys.int64_of_file_descr fd));
	  self # synch();
	  None
	  
      | `Fatal_error e ->
	  (* The connection is already down. Just log the incident: *)
	  dlogr (fun () -> 
		   sprintf 
		     "FD %Ld: reactor_next_request: got fatal error" 
		     (Netsys.int64_of_file_descr fd));
	  if e <> `Broken_pipe_ignore then (
	    let msg = Nethttpd_kernel.string_of_fatal_error e in
	    let status = `Internal_server_error in
	    logged_error_response 
	      fd_addr peer_addr None 0L false status None (Some msg)
	      None None config;
	  );
	  None

      | `Bad_request_error (e, resp) ->
	  (* Log the incident, and reply with a 400 response: *)
	  dlogr (fun () -> 
		   sprintf 
		     "FD %Ld: reactor_next_request: got bad request" 
		     (Netsys.int64_of_file_descr fd));
	  let msg = string_of_bad_request_error e in
	  let status = status_of_bad_request_error e in
	  logged_error_response
	    fd_addr peer_addr None 0L false status None (Some msg)
	    None (Some resp) config;
	  self # next_request()

      | `Timeout ->
	  (* Just ignore. The next token will be `Eof *)
	  dlogr (fun () -> 
		   sprintf 
		     "FD %Ld: reactor_next_request: got timeout" 
		     (Netsys.int64_of_file_descr fd));
	  self # next_request()

      | _ ->
	  (* Everything else means that we lost synchronization, and this is a
	   * fatal error!
	   *)
	  dlogr (fun () -> 
		   sprintf 
		     "FD %Ld: reactor_next_request: out of sync" 
		     (Netsys.int64_of_file_descr fd));
	  logged_error_response
	    fd_addr peer_addr None 0L false `Internal_server_error
	    None (Some "Nethttpd: Reactor out of synchronization")
	    None None config;
	  proto # abort `Server_error;
	  self # next_request()

  method close () =
    dlogr (fun () -> 
	     sprintf "FD %Ld: reactor close" (Netsys.int64_of_file_descr fd));
    ( try
	self # synch();
      with
	| err -> 
	    Netlog.Debug.release_fd fd;
	    Unix.close fd; 
	    raise err
    );
    proto # shutdown();
    if proto # need_linger then (
      dlogr (fun () -> 
	       sprintf "FD %Ld: lingering" (Netsys.int64_of_file_descr fd));
      let lc = 
	new Nethttpd_kernel.lingering_close
	  ~preclose:(fun () -> Netlog.Debug.release_fd fd)
	  fd in
      while lc # lingering do
	lc # cycle ~block:true ()
      done
    )
    else (
      Netlog.Debug.release_fd fd;
      Unix.close fd
    )
end


exception Redirect_response_legal of string * http_header

type x_reaction = 
    [ http_service_reaction
    | `Redirect_request of string * http_header
    ]


let process_connection config fd (stage1 : 'a http_service) =

  let fd_addr_str =
    try Netsys.string_of_sockaddr (Unix.getsockname fd)
    with _ -> "(noaddr)" in
  let peer_addr_str =
    try Netsys.string_of_sockaddr (Netsys.getpeername fd) 
    with _ -> "(noaddr)" in

  dlogr
    (fun () ->
       sprintf "FD %Ld (%s -> %s) processing connection"
	 (Netsys.int64_of_file_descr fd) peer_addr_str fd_addr_str 
    );

  let protect env f arg =
    try
      f arg
    with
      | Redirect_response_legal(_,_) as e -> raise e

      | Standard_response(status, hdr_opt, errmsg_opt) 
	  when !(env#output_state) = `Start -> (
	    output_std_response config env status hdr_opt errmsg_opt
	  )
 
      | err when !(env#output_state) = `Start ->
	  output_std_response config env `Internal_server_error None 
	    (Some("Nethttpd: Handler exception: " ^ Netexn.to_string err));
  in

  let do_stage3 req env stage3 =
    dlogr (fun () -> sprintf "req-%d: stage3" (Oo.id req));
    try
      stage3 # generate_response env;
      dlogr (fun () -> sprintf "req-%d: stage3 done" (Oo.id req));
    with
      | Redirect_request(_,_) ->
	  failwith "Caught Redirect_request in stage 3, \
                    but it is only allowed in stage 1"
      | Redirect_response(uri,hdr) ->
	  if !(env#output_state) <> `Start then
	    failwith "Caught Redirect_response, \
                      but it is too late for redirections";
	  dlogr (fun () -> sprintf "req-%d: stage3 redirect_response"
		   (Oo.id req));
	  raise (Redirect_response_legal(uri,hdr))
  in

  let do_stage2 req env stage2 =
    dlogr (fun () -> sprintf "req-%d: stage2" (Oo.id req));
    let stage3 = 
      try
	stage2 # process_body env
      with
	| Redirect_request(_,_) ->
	    failwith "Caught Redirect_request in stage 2, \
                      but it is only allowed in stage 1"
	| Redirect_response(_,_) ->
	    failwith "Caught Redirect_response in stage 2, \
                      but it is only allowed in stage 3"
    in
    dlogr (fun () -> sprintf "req-%d: stage2 done" (Oo.id req));
    req # finish_request();
    do_stage3 req env stage3
  in

  let rec process_request req (redir_env:extended_environment) redir_count =
    (* [redir_env]: The environment of the request, possibly rewritten by redirects.
     * [redir_count]: The number of already performed redirections
     * [req]: Contains always the original environment
     *)
    dlogr 
      (fun () ->
	 sprintf "req-%d: process_request redir_count=%d"
	   (Oo.id req) redir_count);
    if redir_count > 10 then
      failwith "Too many redirections";
    dlogr (fun () -> sprintf "req-%d: stage1" (Oo.id req));
    let reaction = 
      try (stage1 # process_header 
	     (redir_env :> extended_environment) :> x_reaction)
      with 
	| Redirect_request(new_uri, new_hdr) ->
	    `Redirect_request(new_uri, new_hdr)
	| Redirect_response(_,_) ->
	    failwith "Caught Redirect_response in stage 1, \
                      but it is only allowed in stage 3"
    in
    dlogr
      (fun () ->
	 let s_reaction =
	   match reaction with
	     | `Accept_body stage2 -> "Accept_body (next stage: stage2)"
	     | `Reject_body stage3 -> "Reject_body (next stage: stage3)"
	     | `Static _ -> "Static"
	     | `File _ -> "File"
	     | `Std_response _ -> "Std_response"
	     | `Redirect_request _ -> "Redirect_request" in
	 sprintf "req-%d: stage1 results in: %s" (Oo.id req) s_reaction
      );
    ( try
	( match reaction with
	    | `Accept_body stage2 ->
		req # accept_body();
		protect redir_env (do_stage2 req redir_env) stage2
	    | `Reject_body stage3 ->
		req # reject_body();
		protect redir_env (do_stage3 req redir_env) stage3
	    | `Static(status, resp_hdr_opt, resp_str) ->
		req # reject_body();
		output_static_response redir_env status resp_hdr_opt resp_str
	    | `File(status, resp_hdr_opt, resp_filename, pos, length) ->
		req # accept_body();
		protect
		  redir_env 
		  (output_file_response 
		     redir_env status resp_hdr_opt resp_filename pos) 
		  length
	    | `Std_response(status, resp_hdr_opt, errlog_opt) ->
		req # reject_body();
		output_std_response 
		  config redir_env status resp_hdr_opt errlog_opt
	    | `Redirect_request(new_uri, new_hdr) ->
		dlogr (fun () -> sprintf "req-%d: redirect_request to: %s"
			 (Oo.id req) new_uri);
		let (new_script_name, new_query_string) = 
		  decode_query new_uri in
		new_hdr # update_multiple_field 
		  "Content-length" 
		  (redir_env # multiple_input_header_field "Content-length");
		let new_properties =
		  update_alist 
		    [ "REQUEST_URI", new_uri;
		      "SCRIPT_NAME", new_script_name;
		      "QUERY_STRING", new_query_string ] 
		    redir_env#cgi_properties in
		let new_env =
		  new redirected_environment 
		    ~properties:new_properties
		    ~in_header:new_hdr
		    ~in_channel:req#environment#input_channel
		    redir_env in
		process_request req new_env (redir_count+1)
	)
      with
	| Redirect_response_legal(new_uri, new_hdr) ->
	    dlogr (fun () -> sprintf "req-%d: redirect_response to: %s"
		     (Oo.id req) new_uri);
	    if !(redir_env#output_state) <> `Start then
	      failwith "Redirect_response is not allowed after \
                        output has started";
	    let (new_script_name, new_query_string) = decode_query new_uri in
	    new_hdr # update_field "Content-length" "0";
	    let new_properties =
	      update_alist 
		[ "REQUEST_URI", new_uri;
		  "SCRIPT_NAME", new_script_name;
		  "QUERY_STRING", new_query_string;
		  "REQUEST_METHOD", "GET"
		] 
		redir_env#cgi_properties in
	    let new_env =
	      new redirected_environment 
		~properties:new_properties
		~in_header:new_hdr
		redir_env in
	    process_request req new_env (redir_count+1)
	    
    );
    dlogr (fun () -> sprintf "req-%d: finish" (Oo.id req));
    req # finish();
    dlogr (fun () -> sprintf "req-%d: done" (Oo.id req));
  in

  let rec fetch_requests reactor =
    match reactor # next_request() with
      | None ->
	  dlogr (fun () -> sprintf "FD %Ld - no next request"
		   (Netsys.int64_of_file_descr fd));
	  ()
      | Some req ->
	  dlogr (fun () -> sprintf "FD %Ld - next request req-%d"
		   (Netsys.int64_of_file_descr fd) (Oo.id req)
		);
	  process_request req req#environment 0;
	  fetch_requests reactor
  in
  
  let reactor = 
    try
      new http_reactor config fd 
    with
	err ->
	  (* An exception means here that getsockname or getpeername failed.
             We can only close the descriptor!
           *)
	  Unix.close fd;
	     (* No need for release_fd - the descriptor has not yet been
                tracked!
	      *)
	  raise err
  in
  ( try
      fetch_requests reactor
    with
	err ->
	  let msg = Netexn.to_string err in
	  dlogr (fun () ->
		   sprintf "Exception forwarding to error log: %s" msg);
	  config # config_log_error no_info
	    ("Nethttpd: Protocol exception: " ^ msg);
  );
  ( try
      reactor # close()
    with
	err ->
	  let msg = Netexn.to_string err in
	  dlogr (fun () ->
		   sprintf "Exception forwarding to error log: %s" msg);
	  config # config_log_error no_info
	    ("Nethttpd: Protocol exception: " ^ msg)
  )
;;


let default_http_processor_config =
  ( object
      inherit Nethttpd_kernel.modify_http_protocol_config
	        Nethttpd_kernel.default_http_protocol_config
      method config_timeout_next_request = 15.0
      method config_timeout = 300.0
      method config_cgi = Netcgi.default_config
      method config_error_response = Nethttpd_util.std_error_response
      method config_log_error p msg =
	let s = Nethttpd_util.std_error_log_string p msg in
	Netlog.log `Err s
      method config_log_access p = ()
    end
  )


let override v opt =
  match opt with
    | None -> v
    | Some x -> x


class modify_http_processor_config
      ?modify_http_protocol_config:(m1 = fun cfg -> cfg)
      ?config_timeout_next_request
      ?config_timeout
      ?config_cgi
      ?config_error_response
      ?config_log_error
      ?config_log_access
      (config : http_processor_config) : http_processor_config =
  let config_timeout_next_request =
    override config#config_timeout_next_request config_timeout_next_request in
  let config_timeout =
    override config#config_timeout config_timeout in
  let config_cgi =
    override config#config_cgi config_cgi in
  let config_error_response =
    override config#config_error_response config_error_response in
  let config_log_error =
    override config#config_log_error config_log_error in
  let config_log_access =
    override config#config_log_access config_log_access in
object
  inherit modify_http_protocol_config (m1 (config:>http_protocol_config))
  method config_timeout_next_request = config_timeout_next_request
  method config_timeout = config_timeout
  method config_cgi = config_cgi
  method config_error_response = config_error_response
  method config_log_error = config_log_error
  method config_log_access = config_log_access
end


let default_http_reactor_config =
  ( object
      inherit modify_http_processor_config default_http_processor_config
      method config_reactor_synch = `Write
    end
  )


class modify_http_reactor_config
      ?modify_http_protocol_config
      ?modify_http_processor_config:(m2 = fun cfg -> cfg)
      ?config_reactor_synch
      (config : http_reactor_config) : http_reactor_config =
  let config_reactor_synch =
    override config#config_reactor_synch config_reactor_synch in
object
  inherit modify_http_processor_config
            ?modify_http_protocol_config
            (m2 (config:>http_processor_config))
  method config_reactor_synch = config_reactor_synch
end
