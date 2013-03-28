(* netcgi_ajp.ml

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

(* Implementation of the Apache JServ Protocol version 1.3 (ajp13) as
   described at
   http://tomcat.apache.org/connectors-doc/common/ajpv13a.html
*)

open Netcgi_common
open Printf

(* Specialize [min] to integers for performance reasons (> 150% faster). *)
let min x y = if (x:int) <= y then x else y

let ajp_log_error msg =
  let zone = Netdate.localzone (* log local time *) in
  let date = Netdate.format "%c" (Netdate.create ~zone (Unix.gettimeofday())) in
  prerr_endline ("[" ^ date ^ "] [Netcgi_ajp] " ^ msg)


(* Property file
 ***********************************************************************)

let rm_htspace =
  Netcgi_common.rm_htspace (fun c -> c = ' ' || c = '\t')


let props_of_file fname =
  let props = ref [] in
  let fh = open_in fname in
  try
    while true do
      let line = input_line fh in
      (* Index of comment sign, to remove it *)
      let comment = (try String.index line '#'
	             with Not_found -> String.length line) in
      try
	let i = String.index line '=' in
	let name = rm_htspace line 0 i
	and value = rm_htspace line (i + 1) comment in
	props := (name,value) :: !props
      with Not_found ->
	() (* No '=' sign; ignore silently *)
    done;
    assert false
  with
  | End_of_file -> close_in fh; List.rev !props
  | err -> close_in fh;	raise err


let arg_parse speclist anonf usage =
  let specs =
    Arg.align (("-classpath", Arg.String(fun _ -> ()),
	       " The option [-classpath <path>] is ignored")
	        :: speclist) in
  let f_call = ref 0 in
  let property_file = ref "" in
  let f s =
    incr f_call;
    if !f_call = 1 then () (* ignore *)
    else if !f_call = 2 then property_file := s
    else anonf s in
  Arg.parse specs f usage;
  (* Parse the property file *)
  try props_of_file !property_file
  with Invalid_argument _ ->
    Arg.usage specs usage;
    failwith "Netcgi_ajp.arg_parse: property file \
      (second anonymous argument) cannot be read."


(* Output
 ***********************************************************************)

(* Max packet size according the the spec. *)
let max_packet_size = 8192 (* 8Kb *)


let buffer_add_int b n =
  assert(0 <= n && n <= 0xFFFF);
  let sn = String.create 2 in
  sn.[0] <- Char.unsafe_chr(n lsr 8);
  sn.[1] <- Char.unsafe_chr(n land 0xFF);
  Netbuffer.add_string b sn

let buffer_add_string b s =
  buffer_add_int b (String.length s);
  Netbuffer.add_string b s;
  Netbuffer.add_string b "\x00"


(* [buffer_add_header b 0 fields] add the header [fields] to the
   netbuffer [b] encoded according to the AJP spec.  It returns the
   number of fields that were added.  If the number of headers or the
   buffer becomes too large, we discard the remaining fields. *)
let rec buffer_add_header b num_headers = function
  | [] -> num_headers
  | (f,v) :: tl ->
      if num_headers >= 0xFFFF then
        num_headers (* too many header fields; stop *)
      else
        (* Ok, can add one more field (2 bytes to code the number of
           headers). *)
        match String.lowercase f with
        | "content-type" ->     add_code_value b num_headers 0xA001 v tl
        | "content-language" -> add_code_value b num_headers 0xA002 v tl
        | "content-length" ->   add_code_value b num_headers 0xA003 v tl
        | "date" ->             add_code_value b num_headers 0xA004 v tl
        | "last-modified" ->    add_code_value b num_headers 0xA005 v tl
        | "location" ->         add_code_value b num_headers 0xA006 v tl
        | "set-cookie" ->       add_code_value b num_headers 0xA007 v tl
        | "set-cookie2" ->      add_code_value b num_headers 0xA008 v tl
        | "servlet-engine" ->   add_code_value b num_headers 0xA009 v tl
        | "status" ->           add_code_value b num_headers 0xA00A v tl
        | "www-authenticate" -> add_code_value b num_headers 0xA00B v tl
        | _ ->
            let f_len = String.length f
            and v_len = String.length v in
            if 6 + f_len + v_len + Netbuffer.length b > max_packet_size
            then num_headers
            else begin
                (* Skip (unlikely) too long names or values *)
                if f_len < 0xA000 && v_len <= 0xFFFF then (
                    buffer_add_string b f;
                    buffer_add_string b v;
                    buffer_add_header b (num_headers + 1) tl
                  )
                else buffer_add_header b num_headers tl
              end

and add_code_value b num_headers code value tl =
  let len = String.length value in
  if 5 + len + Netbuffer.length b > max_packet_size then num_headers
  else begin
      (* skip if [value] is too long *)
      if len <= 0xFFFF then (
          buffer_add_int b code;
          (* encode string [value] *)
          buffer_add_int b len;
          Netbuffer.add_string b value;
          Netbuffer.add_string b "\x00";
          buffer_add_header b (num_headers + 1) tl
        )
      else buffer_add_header b num_headers tl
    end


(* In the following, we stack several netchannels. The goal is to reduce
 * the number of system calls (Unix.write) as much as possible. These
 * calls are much more expensive than the additional overhead of stacking.
 *
 * In particular, the stack looks as follows:
 *
 * - TOP: user_out_channel
 *   Data written to this channel is encoded as AJP packet. No buffer.
 *   Maybe it would be better to have another buffer before the encoding
 *   happens to make the packets as large as possible. However, we usually
 *   have already a transactional buffer on top of the user_out_channel.
 * - ajp_out_channel
 *   Data written to this channel must already be formatted as AJP packets.
 *   Auxiliary methods generate such packets from user data. No buffer.
 * - Netchannels.buffered_raw_out_channel
 *   Implements a buffer of size max_packet_size.
 * - prim_out_channel
 *   Only purpose is to disable close_out. The file descriptor is not
 *   closed by the channel stack
 * - BOTTOM: Netchannels.output_descr
 *   Maps channel methods to system calls. No buffer.
 *
 * For the input direction stacking is similar.
 *)


(* This channel writes to the descriptor, but does not forward the
 * close request (fd is kept open)
 *)
class prim_out_channel fd =
object(self)
  inherit Netchannels.output_descr fd

  method close_out() = ()
    (* don't close fd *)
end


(* This channel includes a buffer and some additional ajp primitives *)
class ajp_out_channel fd =
  let prim_ch = new prim_out_channel fd in
object (self)
  inherit Netchannels.buffered_raw_out_channel
            ~buffer_size:max_packet_size
	    prim_ch

  inherit Netchannels.augment_raw_out_channel
    (* reduces additional methods like really_output to the more
       primitive ones from Netchannels.buffered_raw_out_channel
     *)


  method send_user_data s pos len =
    (* outputs user data as ajp packet *)
    if len > 0 then (
      let h = String.create 7 in  (* packet header *)
      let chunk_len = min len (max_packet_size - 8) in
      let payload_len = chunk_len + 4 in
      h.[0] <- 'A';
      h.[1] <- 'B';
      h.[2] <- Char.unsafe_chr(payload_len lsr 8);
      h.[3] <- Char.unsafe_chr(payload_len land 0xFF);
      h.[4] <- '\x03'; (* prefix_code *)
      h.[5] <- Char.unsafe_chr(chunk_len lsr 8);
      h.[6] <- Char.unsafe_chr(chunk_len land 0xFF);
      self # output_string h;
      self # really_output s pos chunk_len;
      self # output_char '\000'; (* to terminate the chunk *)
      self # send_user_data s (pos+chunk_len) (len-chunk_len);
    )


  (* AJP13_END_RESPONSE.  Signals on [fd] the end of the
     request-handling cycle.  We specify that [fd] can be reused to
     handle new incoming requests.  *)
  method send_end_response() =
    (* (packet prefix "AB") (length=2) (prefix_code=5) (reuse=true) *)
    self # output_string "AB\x00\x02\x05\x01"

  (* Ask for more data from the request body.  Ask for the maximum of
     8186 bytes (8Kb - 6).  *)
  method send_get_body_chunk() =
    (* (packet prefix "AB") (length=3) (prefix_code=6) (size=0x1FFA) *)
    self # output_string "AB\x00\x03\x06\x1F\xFA"

  (* Send a CPong reply on [fd]. *)
  method send_cpong() =
    (* (packet prefix "AB") (payload length=1) (prefix_code=9) *)
    self # output_string "AB\x00\x01\x09"

  (* AJP13_SEND_HEADERS *)
  method send_headers (header: Netmime.mime_header) =
    let b = Netbuffer.create 0x1000 in
    Netbuffer.add_string b "AB??\x04"; (* "AB" length[to fill] prefix_code *)
    (* Status *)
    let st = try int_of_string(header#field "Status") with _ -> 200 in
    buffer_add_int b st;
    let st_msg = Nethttp.string_of_http_status(Nethttp.http_status_of_int st) in
    buffer_add_string b st_msg;
    (* Headers *)
    let pos = Netbuffer.length b in
    Netbuffer.add_string b "??"; (* # of headers, to set *)
    let num_headers = buffer_add_header b 0 header#fields in
    (* Set the length of the payload and the number of headers and output *)
    let buf = Netbuffer.unsafe_buffer b
    and len = Netbuffer.length b in
    let payload_len = len - 4 in
    buf.[2] <- Char.unsafe_chr(payload_len lsr 8);
    buf.[3] <- Char.unsafe_chr(payload_len land 0xFF);
    buf.[pos] <- Char.unsafe_chr(num_headers lsr 8);
    buf.[pos+1] <- Char.unsafe_chr(num_headers land 0xFF);
    self # really_output buf 0 len
end


(* This channel wraps user payload into ajp packets *)
class user_out_channel ajp_ch =
object(self)
  val mutable pos_out = 0

  inherit Netchannels.augment_raw_out_channel
    (* reduces additional methods like really_output to the more
       primitive ones implemented below
     *)

  method output s pos len =
    ajp_ch # send_user_data s pos len;
    pos_out <- pos_out + len;
    len

  method flush() =
    ajp_ch # flush()

  method close_out() =
    try
      ajp_ch # flush()
    with
      | error ->
	  Netlog.logf `Err
	    "Netcgi_ajp: Suppressed error in close_out: %s"
	    (Netexn.to_string error)

  method pos_out = pos_out

end


(* Input object
 ***********************************************************************)

exception Invalid of string
  (** Exception raised if the protocol is not respected.  The string
      is an explanation to be logged.  *)

exception Shutdown
  (** Special exception raised if the connector is requested to
      shutdown.  *)


(* This channel reads from the descriptor, but does not forward the
 * close request (fd is kept open)
 *)
class prim_in_channel fd =
object(self)
  inherit Netchannels.input_descr fd

  method close_in() = ()
    (* don't close fd *)
end


(* On the input side, this channel includes a buffer and some additional
   ajp primitives. The output side is inherited from ajp_out_channel,
   i.e. this channel is bidirectional.
 *)
class ajp_channel fd =
  let prim_ch = new prim_in_channel fd in
object(self)
  inherit ajp_out_channel fd
    (* This is both an input and output channel *)

  inherit Netchannels.buffered_raw_in_channel
            ~buffer_size:max_packet_size
	    prim_ch

  inherit Netchannels.augment_raw_in_channel
    (* reduces additional methods like really_input to the more
       primitive ones from Netchannels.buffered_raw_in_channel.
       Note that input_line is slow (but not used here)
     *)

  val buf = String.create max_packet_size
  val mutable buf_len = 0
  val mutable scan_pos = 0   (* for packet scanner below *)

  method packet_buffer = buf
  method packet_buffer_length = buf_len

  (* Input a whole packet data into [buf=packet_buffer].  Return the length of the
     payload.  Thus the data is in [buf.[0 .. length-1]].
     @raise Invalid if the packet is larger than the buffer. *)
  method input_packet() =
    let s = String.create 4 in
    self # really_input s 0 4;
    if s.[0] <> '\x12' || s.[1] <> '\x34' then
      raise(Invalid "Packets must start with 0x1234");
    let len = (Char.code(s.[2]) lsl 8) lor (Char.code(s.[3])) in
    if len > String.length buf then
      raise(Invalid(sprintf "Packet data length = %i bytes > allowed = %i."
                      len (String.length buf)));
    self # really_input buf 0 len;
    buf_len <- len;
    scan_pos <- 0;
    len


  (* User payload: *)

  val mutable first_filling = true

  method input_user_data() =
    (* We need to ask for the next data packet. *)
    if not first_filling then (
      self # send_get_body_chunk();
      self # flush()
    );
    first_filling <- false;

    (* Get packet: *)
    let payload_len = self # input_packet() in
    if payload_len < 2 then
      raise(Invalid "Data packet payload too small");

    let data_len = (Char.code(buf.[0]) lsl 8) lor (Char.code(buf.[1])) in
    if data_len+2 > payload_len then
      raise(Invalid "Length exceeds payload length");

    (* Return (pos,len) of buf *)
    (2, data_len)


  (* Packet scanner: *)

  method scan_bool () =
    let p = scan_pos in
    if p >= buf_len then
      raise(Invalid "Scanning beyond end of packet");
    scan_pos <- p+1;
    buf.[p] <> '\000'

  method scan_byte() =
    let p = scan_pos in
    if p >= buf_len then
      raise(Invalid "Scanning beyond end of packet");
    scan_pos <- p+1;
    Char.code buf.[p]

  method scan_int() =
    let p = scan_pos in
    if p+1 >= buf_len then
      raise(Invalid "Scanning beyond end of packet");
    scan_pos <- p+2;
    (Char.code(buf.[p]) lsl 8) lor (Char.code(buf.[p+1]))

  method scan_string() =
    let l = self # scan_int() in
    self # scan_string1 l

  method scan_string1 l =
    if l = 0xFFFF then
      (* BLOODY HELL: A length of 0xFFFF means "null" and there is no
         terminating '\000'.  This is not documented in the spec! *)
      ""
    else (
      let p = scan_pos in
      if p + l >= buf_len then
	raise(Invalid "Scanning beyond end of packet");
      scan_pos <- p+l+1;
      String.sub buf p l
    )

end


(* This channel unwraps user payload from ajp packets. Must only be used
   after the FORWARD_REQUEST packet has been received
 *)
class user_in_channel ajp_ch =
object(self)
  inherit Netchannels.augment_raw_in_channel
    (* Define derived method. Note that input_line is slow. This is ok
       because the MIME scanner does not rely on it.
     *)

  val mutable buf_pos = 0
  val mutable buf_len = 0

  val mutable pos_in = 0

  method input s pos len =
    if buf_len <= 0 then (
      let (p,l) = ajp_ch # input_user_data() in
      if l = 0 then
	raise End_of_file;
      buf_pos <- p;
      buf_len <- l
    );
    let l = min len buf_len in
    String.blit (ajp_ch # packet_buffer) buf_pos s pos l;
    buf_pos <- buf_pos + l;
    buf_len <- buf_len - l;
    pos_in <- pos_in + l;
    l

  method close_in() = ()

  method pos_in = pos_in

end




(* AJP13_FORWARD_REQUEST
 ***********************************************************************)

let http_headers =
  [| "accept"; "accept-charset"; "accept-encoding"; "accept-language";
     "authorization"; "connection"; "content-type"; "content-length";
     "cookie"; "cookie2"; "host"; "pragma"; "referer"; "user-agent" |]

(** Input [num] headers from [ajp_ch] and prepend them to [headers].
    Headers are represented as (string * string) list *)
let rec scan_request_headers ajp_ch num headers =
  if num = 0 then
    headers
  else
    let l = ajp_ch # scan_int() in
    let name =
      if l >= 0xA001 && l <= 0xA00E then
        (* The header name is encoded as 0xA0?? *)
        http_headers.(l - 0xA001)
      else
        (* The two bytes read form the length of the string *)
        ajp_ch # scan_string1 l in
    let value = ajp_ch # scan_string () in
    scan_request_headers ajp_ch (num - 1) ((name, value) :: headers)


let attributes =
  [| "context"; "servlet_path"; "remote_user"; "auth_type";
     "QUERY_STRING"; "jvm_route"; "ssl_cert"; "ssl_cipher";
     "ssl_session"; "req_attribute"; "ssl_key_size" |]

(** Input all attributes and prepend them to the list [attr]. *)
let rec scan_attributes ajp_ch attr =
  let b = ajp_ch # scan_byte () in
  if 0x01 <= b && b <= 0x0B then
    let name =
      if b = 0x0A then  (* req_attribute *)
	ajp_ch # scan_string()
      else
	attributes.(b - 1) in
    let value = ajp_ch # scan_string () in
    scan_attributes ajp_ch ((name, value) :: attr)
  else
    attr (* ends *)


let request_methods =
  [| "OPTIONS"; "GET"; "HEAD"; "POST"; "PUT"; "DELETE"; "TRACE";
     "PROPFIND"; "PROPPATCH"; "MKCOL"; "COPY"; "MOVE"; "LOCK"; "UNLOCK";
     "ACL"; "REPORT"; "VERSION-CONTROL"; "CHECKIN"; "CHECKOUT";
     "UNCHECKOUT"; "SEARCH"; "MKWORKSPACE"; "UPDATE"; "LABEL";
     "MERGE"; "BASELINE_CONTROL"; "MKACTIVITY" |]

(** Assumes that the first byte of the AJP13_FORWARD_REQUEST packet
    is already read, and returns [(props, inheader)].
*)
let scan_props_inheader ?script_name log_error ajp_ch =
  let m = ajp_ch # scan_byte () in
  let req_method =
    if 1 <= m && m <= Array.length request_methods then
      request_methods.(m - 1)
    else
      (* Do not raise an exc because the spec explicitely says
         additional methods will be transported. *)
      "Unknown method " ^ string_of_int m in
  let protocol = ajp_ch # scan_string() in
  let req_uri =  ajp_ch # scan_string() in
  let remote_addr = ajp_ch # scan_string() in
  let remote_host = ajp_ch # scan_string() in
  let server_name = ajp_ch # scan_string() in
  let server_port = ajp_ch # scan_int() in
  let is_ssl = ajp_ch # scan_bool() in
  let num_headers = ajp_ch # scan_int() in
  (* Fix: AJP does not transmit the compulsory CGI properties
     PATH_INFO and SCRIPT_NAME.  Unfortunately there is not enough
     information to deduce them from [req_uri] (but it is important to
     get [req_uri] by concatenating the two).  That's why we use the
     optional [script_name].  *)
  let script_name, path_info =
    match script_name with
    | None ->
        begin
          (* Cut at the first '/' not at the beginning of the string. *)
          try
            let i = String.index_from req_uri 1 '/' in
            (String.sub req_uri 0 i,
            String.sub req_uri i (String.length req_uri - i))
          with Not_found | Invalid_argument _ ->
            (req_uri, "") (* fallback *)
        end
    | Some s ->
        let s_len = String.length s in
        let is_script_name = is_prefix s req_uri (* => req_uri longer *)
          && (s_len = String.length req_uri
              || req_uri.[s_len] = '/') in
        if is_script_name then
          (s, String.sub req_uri s_len (String.length req_uri - s_len))
        else (
          log_error(sprintf "The given script_name=%S is not a prefix \
		of the REQUEST_URI=%S" s req_uri);
          (req_uri, "")
        ) in
  let props =
    [ ("REQUEST_METHOD", req_method);
      ("SERVER_PROTOCOL", protocol);
      ("REQUEST_URI", req_uri);
      ("REMOTE_ADDR", remote_addr);
      ("REMOTE_HOST", remote_host);
      ("SERVER_NAME", server_name);
      ("SERVER_PORT", string_of_int server_port);
      ("HTTPS", if is_ssl then "on" else "off");
      ("PATH_INFO", path_info);
      ("SCRIPT_NAME", script_name);
    ] in
  (* HTTP headers *)
  let inheader = scan_request_headers ajp_ch num_headers [] in
  (* Attributes *)
  let props = scan_attributes ajp_ch props in
  (* @return *)
  (props, inheader)


(** [input_forward_request fd] loops, reading packets on [fd], until
    it finds a AJP13_FORWARD_REQUEST in which case it returns the
    corresponding pair [(props, inheader)].  Other type of requests
    are handled automatically.

    This function will potentially run a great number of times so must
    be tail rec. *)
let rec input_forward_request ?script_name log_error ajp_ch =
  let payload_len = ajp_ch # input_packet () in
  if payload_len = 0 then begin
      log_error "Packet with empty payload!";
      input_forward_request ?script_name log_error ajp_ch
    end
  else
    let b = ajp_ch # scan_byte() in
    match b with
      | 7 -> raise Shutdown
      | 8 ->
          (* Ping: the web server asks the container to take control
             (secure login phase). *)
          (* FIXME: ignore it -- do not know how to respond!! *)
          log_error "Packet \"Ping\" received.  IGNORING.";
          input_forward_request ?script_name log_error ajp_ch
      | 10 ->
          (* CPing: the web server asks the container to respond quickly
             with a CPong. *)
          ajp_ch # send_cpong();
          input_forward_request ?script_name log_error ajp_ch
      | 2 ->
          (* Forward Request: Begin the request-processing cycle with the
             following data *)
          scan_props_inheader ?script_name log_error ajp_ch
      | b ->
          (* Unknown, skip packet *)
          log_error("Unknown packet code " ^ string_of_int b ^ ".  Skipped.");
          input_forward_request ?script_name log_error ajp_ch



(************************************************************************)
(* Environment *)

class ajp_environment ?log_error ~config ~properties ~input_header ajp_ch =
  let user_ch = new user_out_channel ajp_ch in
object(self)
  inherit
    Netcgi_common.cgi_environment ~config ~properties ~input_header user_ch

  (* @override *)
  (* AJP has a special packet type (different from the output one) to
     send the headers. *)
  method send_output_header () =
    if header_not_sent then begin
        ajp_ch # send_headers self#output_header;
        header_not_sent <- false (* One output header per request *)
      end

  (* Override to use the correct channel *)
  method log_error msg =
    match log_error with
      | None -> ajp_log_error msg
      | Some f -> f msg
end

(************************************************************************)
(* Requests *)

let dbg_out msg =
  prerr_endline ("PID " ^ string_of_int(Unix.getpid()) ^ ": " ^ msg)


let handle_request ?script_name config output_type arg_store exn_handler f ~log fd =
  try
    (* The ajp_ch is a bidirectional channel that reads/writes AJP packets *)
    let ajp_ch = new ajp_channel fd in
    let log_error = match log with Some f -> f | None -> ajp_log_error in

    (* Read input packets until the FORWARD_REQUEST packet arrives *)
    let (properties, input_header) =
      input_forward_request ?script_name log_error ajp_ch in

    (* Creates the environment. ajp_ch will be wrapped as user_out_channel *)
    let env =
      new ajp_environment
	?log_error:log ~config ~properties ~input_header ajp_ch in

    (* Now that one knows the environment, one can warn about exceptions *)
    exn_handler_default env ~exn_handler
      (fun () ->
         try
	   (* This channel decodes user data from ajp packets: *)
	   let in_ch = new user_in_channel ajp_ch in

           let cgi =
	     cgi_with_args (new cgi) env output_type in_ch arg_store in
           (try
              f(cgi: Netcgi.cgi);
              cgi#out_channel#commit_work();
              cgi#finalize()
            with e when config.default_exn_handler ->
              cgi#finalize(); raise e);
           None
         with Shutdown -> Some Shutdown
      )
      ~finally:(fun () ->
                  ajp_ch # send_end_response();
		  ajp_ch # flush()
	       );
    `Conn_keep_alive
  with
    | Shutdown ->
	`Conn_close
    | error ->
	`Conn_error error


let rec handle_connection_1 fd ~config ?script_name output_type arg_store
    exn_handler f =

  let log = Some ajp_log_error in
  let fd_style = Netsys.get_fd_style fd in

  (* FIXME: Although the debug info says the connection is
     recycled, apache does not close the sockect and tries to
     reconnect.  That leaves unused processes running.  Until I
     understand what is going on, I close the connection handler
     if I have to wait more and 1 sec for the next request. *)
  let cdir =
    try
      let ok = Netsys.restart_tmo
	(Netsys.wait_until_readable fd_style fd) 1.0 in
      if not ok then (
	(* ajp_log_error "Timeout waiting for the next connection.  Closing.";*)
	`Conn_close
      )
      else
	handle_request config output_type arg_store exn_handler f ~log fd
    with
      | error -> `Conn_error error in

  match cdir with
    | `Conn_keep_alive ->
	handle_connection_1 fd ~config ?script_name output_type arg_store
	  exn_handler f
    | `Conn_close ->
	Netlog.Debug.release_fd fd;
	Unix.close fd
    | `Conn_error error ->
	Netlog.Debug.release_fd fd;
	Unix.close fd;
	raise error
    (* other cdir not possible *)

let handle_connection fd ~config ?script_name output_type arg_store
    exn_handler f =
  Netlog.Debug.track_fd
    ~owner:"Netcgi_ajp"
    ~descr:("connection from " ^
	      try Netsys.string_of_sockaddr(Netsys.getpeername fd)
	      with _ -> "(noaddr)")
    fd;
  handle_connection_1 fd ~config ?script_name output_type arg_store
    exn_handler f


let run
    ?props
    ?(config=Netcgi.default_config)
    ?script_name
    ?(allow=fun _ -> true)
    ?(output_type=(`Direct "": Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic))
    ?(exn_handler=(fun _ f -> f()))
    ?sockaddr
    ?(port=8009)
    f =
  (* Socket to listen to *)
  let sockaddr = match sockaddr with
    | None -> Unix.ADDR_INET(Unix.inet_addr_loopback, port)
    | Some sockaddr -> sockaddr in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 5;
  Netlog.Debug.track_fd
    ~owner:"Netcgi_ajp"
    ~descr:("master " ^ Netsys.string_of_sockaddr sockaddr)
    sock;
  while true do
    let (fd, server) = Unix.accept sock in
    try
      if allow server then
        handle_connection fd ~config ?script_name output_type arg_store
          exn_handler f;
    with
      | e when config.default_exn_handler ->
	  (* Any exception occurring here is fatal *)
	  ajp_log_error("FATAL: " ^ Netexn.to_string e);
  done

