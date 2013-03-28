(* netcgi_fcgi.ml

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


(* FIXME: how do we handle SIGTERM ? *)

(* FastCGI protocol (version 1) according to the specification found
   at http://www.fastcgi.com/devkit/doc/fcgi-spec.html
*)
(** There is a bug in mod_fastcgi with the following symptom:

    "If the answer to a request to the server has more than 8192 Bytes
    and less then about 16000 Bytes of payload, the HTTP header is not
    counted, the request will fail with an idle timeout.  But the FCGI
    server has delivered the complete response to the apache server in
    time."

    A fix is to close the socket at the end of the request -- in
    addition the spec. sec. 3.5 says "A simple application gets a
    significant performance boost by closing the transport connection
    when it has finished writing its response."

    This of course disallows the multiplexing of requests.  As moreover
    this library will deal with 1 request at a time, there is no much
    point in queuing the packets for the other requests, better
    DISALLOW MULTIPLEXING.

    TODO: If the need is felt, a multithreaded connector running
    requests in separated threads makes sense.
*)


(* Monomorphic version for speed *)
let min (i:int) j = if i <= j then i else j


open Netcgi_common

let fcgi_version = '\001'
let fcgi_listensock = Unix.stdin

(* FCGI record types *)
type fcgi_type = char
let fcgi_begin_request = '\001'
let fcgi_abort_request = '\002'
let fcgi_end_request   = '\003'
let fcgi_params        = '\004'
let fcgi_stdin         = '\005'
let fcgi_stdout        = '\006'
let fcgi_stderr        = '\007'
let fcgi_data          = '\008'
let fcgi_get_values        = '\009'
let fcgi_get_values_result = '\010'
let fcgi_unknown_type      = '\011'

type role = [`Responder | `Authorizer | `Filter]

(* Values for protocolStatus component of FCGI_EndRequestBody *)
type protocol_status =
  | REQUEST_COMPLETE
  | CANT_MPX_CONN
  | OVERLOADED
  | UNKNOWN_ROLE

exception Abort of int
  (* [Abort id] is raised when a FCGI_ABORT_REQUEST record is
     received. *)

let fcgi_keep_conn = 0x1
  (* Mask for flags component of FCGI_BeginRequestBody *)

(* Get the list of valid IP addresses for the Web server or [] if not set. *)
let fcgi_web_server_addrs =
  try
    let is_comma c = (c = ',') in
    let addrs = rev_split is_comma (Unix.getenv "FCGI_WEB_SERVER_ADDRS") in
    List.map Unix.inet_addr_of_string addrs
  with
    Not_found | Failure _ -> []


(************************************************************************)
(* Output *)

let set_length4 s ofs n =
  (* 4 bytes encoding of the length [n] in the string [s] from
     position [ofs]. *)
  s.[ofs+3] <- Char.unsafe_chr(n land 0xFF);
  let n = n lsr 8 in
  s.[ofs+2] <- Char.unsafe_chr(n land 0xFF);
  let n = n lsr 8 in
  s.[ofs+1] <- Char.unsafe_chr(n land 0xFF);
  s.[ofs] <- Char.chr((n lsr 8) lor 0x80)

(* [lengths_of_key_val k v] returns a string encoding the lengths
   of the key-value pair [(k,v)] according to fcgi spec: 3.4
   Name-Value Pairs. *)
let lengths_of_key_val k v =
  let klen = String.length k
  and vlen = String.length v in
  if klen < 128 then
    if vlen < 128 then begin
      let s = String.create 2 in
      s.[0] <- Char.chr klen;
      s.[1] <- Char.chr vlen;
      s
    end
    else begin
      let s = String.create 5 in
      s.[0] <- Char.chr klen;
      set_length4 s 1 vlen;
      s
    end
  else
    if vlen < 128 then begin
      let s = String.create 5 in
      set_length4 s 0 klen;
      s.[4] <- Char.chr vlen;
      s
    end
    else begin
      let s = String.create 8 in
      set_length4 s 0 klen;
      set_length4 s 4 vlen;
      s
    end

(* Add the key-value pair [k], [v] to the buffer [buf]. *)
let add_key_val buf k v =
  Buffer.add_string buf (lengths_of_key_val k v);
  Buffer.add_string buf k;
  Buffer.add_string buf v


(* In the following, we stack several netchannels. The goal is to reduce
 * the number of system calls (Unix.write) as much as possible. These
 * calls are much more expensive than the additional overhead of stacking.
 *
 * In particular, the stack looks as follows:
 *
 * - TOP: user_out_channel
 *   Data written to this channel is encoded as fcgi packet. No buffer.
 *   Maybe it would be better to have another buffer before the encoding
 *   happens to make the packets as large as possible. However, we usually
 *   have already a transactional buffer on top of the user_out_channel.
 * - fcgi_out_channel
 *   Data written to this channel must already be formatted as fcgi packets.
 *   Auxiliary methods generate such packets from user data. No buffer.
 * - Netchannels.buffered_raw_out_channel
 *   Implements a buffer.
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


(* This channel includes a buffer and some additional fcgi primitives *)
class fcgi_out_channel fd =
  let prim_ch = new prim_out_channel fd in
object (self)
  inherit Netchannels.buffered_raw_out_channel 
            ~buffer_size:8192
	    prim_ch

  inherit Netchannels.augment_raw_out_channel
    (* reduces additional methods like really_output to the more 
       primitive ones from Netchannels.buffered_raw_out_channel
     *)

  (* [output_packet ty id s ofs len] sends a request [id],
     a record of type [ty] whose data is the substring [s.[ofs .. ofs +
     len - 1]] where [len <= 65535].  *)
  method output_packet ty id s ofs len =
    assert(len <= 0xFFFF);
    let padding_len = 
      let r = len mod 8 in
      if r = 0 then 0 else 8 - r in
    (* We keep total size a multiple of 8 bytes so the web server can
       easily align the data for efficency. *)
    let r = String.create 8 in
    r.[0] <- fcgi_version;
    r.[1] <- ty;
    r.[2] <- Char.chr(id lsr 8);
    r.[3] <- Char.chr(id land 0xFF);  (* requestId *)
    r.[4] <- Char.chr(len lsr 8);
    r.[5] <- Char.chr(len land 0xFF); (* contentLength *)
    r.[6] <- Char.chr(padding_len);  (* paddingLength *)
    r.[7] <- '\000';
    self # output_string r;
    self # really_output s ofs len;
    self # really_output r 0 padding_len  (* Padding (garbage) *)

  (* [send_user_data ty id s pos len] sends a request [id],
     a record of type [ty] whose data is [s]. If necessary, data is split
     into several packets
   *)
  method send_user_data ty id s pos len =
    let rec loop pos len =
      let l = min (min len (String.length s - pos)) 0xFFFF in
      if l > 0 then (
	self # output_packet ty id s pos l;
	loop (pos+l) (len-l)
      )
    in
    if len > 0 then
      loop pos len

  (* End the request, either because the handling script ended or
     because it was rejected.  [exit_code] is an application-level
     status code (the meaning depends on the role).  [status] is a
     protocol-level status code. *)
  method send_end_request id exit_code status =
    let r = String.make 8 '\000' in
    r.[3] <- Char.chr(exit_code land 0xFF); (* appStatus (4 bytes) *)
    let exit_code = exit_code lsr 8 in
    r.[2] <- Char.chr(exit_code land 0xFF);
    let exit_code = exit_code lsr 8 in
    r.[1] <- Char.chr(exit_code land 0xFF);
    r.[0] <- Char.chr(exit_code lsr 8);
    r.[4] <- Char.chr(match status with
			| REQUEST_COMPLETE -> 0
			| CANT_MPX_CONN ->    1
			| OVERLOADED ->       2
			| UNKNOWN_ROLE ->     3); (* protocolStatus *)
    self # output_packet fcgi_end_request id r 0 8

  (* Response to a managment record of type [t] that this library does
     not understand. *)
  method send_unknown_type t =
    let r = String.make 8 '\000' in
    r.[0] <- t;
    self # output_packet fcgi_unknown_type 0 r 0 8

  method send_stdout_end_response id =
    (* Close the stream FCGI_STDOUT by sending an empty record *)
    self # output_packet fcgi_stdout id "" 0 0

  method send_stderr_end_response id =
    (* Close the stream FCGI_STDERR by sending an empty record *)
    self # output_packet fcgi_stderr id "" 0 0

  (* [send_values props ~max_conns] send back (on [fd])
   an appropriate FCGI_GET_VALUES_RESULT response for a
   FCGI_GET_VALUES record [r]. *)
  method send_values (props : (string * string) list) ~max_conns =
    let buf = Buffer.create 64 in
    if List.mem_assoc "FCGI_MAX_CONNS" props then
      add_key_val buf "FCGI_MAX_CONNS" (string_of_int max_conns);
    if List.mem_assoc "FCGI_MAX_REQS" props then
      add_key_val buf "FCGI_MAX_REQS" "1"; (* no multiplexing! *)
    if List.mem_assoc "FCGI_MPXS_CONNS" props then
      add_key_val buf "FCGI_MPXS_CONNS" "0"; (* no multiplexing! *)
    let s = Buffer.contents buf in
    self # output_packet fcgi_get_values_result 0 s 0 (String.length s)
end


(* This channel wraps user payload into fcgi packets. The [id] comes from
   the previously received request
  *)
class user_out_channel fcgi_ch id =
object(self)
  val mutable pos_out = 0

  inherit Netchannels.augment_raw_out_channel
    (* reduces additional methods like really_output to the more 
       primitive ones implemented below
     *)

  method output s pos len =
    fcgi_ch # send_user_data fcgi_stdout id s pos len;
    pos_out <- pos_out + len;
    len

  method flush() =
    fcgi_ch # flush()

  method close_out() =
    try
      fcgi_ch # flush()
    with
      | error ->
	  Netlog.logf `Err
	    "Netcgi_fcgi: Suppressed error in close_out: %s"
	    (Netexn.to_string error);
    
  method pos_out = pos_out

end


(************************************************************************)
(* Input *)


(* FCGI dialog takes the form of records (spec. section 3.3).  The
   incoming ones will be decoded to the following structure.  *)
type record = {
  version : int;	(* at present = 1 *)
  ty : char;		(* type, see the constants above *)
  id : int;		(* FastCGI request id; 0 = management *)
  length : int;
  (* length (<= 65535) is the number of bytes of data.  The data
     itself is not part of this record, input functions will take a
     buffer [data] and return the record.  [data.[0 .. length-1]] will
     be the actual data read.  This scheme has been chosen in order to
     be able to reuse a given string as buffer. *)
}


(* [get_length data ofs] returns the [(l, o)] where [l] is length at
   offset [ofs] encoded according to the Name-Value Pairs specs
   (section 3.4) and [o] is the next offset.
   It is assumed that [0 <= ofs < String.length s].
   @raise Failure if the spec is not respected.  *)
let get_length data ofs =
  let b = Char.code(data.[ofs]) in
  if b lsr 7 = 0 then
    (b, ofs + 1)
  else begin
    if ofs + 3 >= String.length data then
      failwith "Netcgi_fcgi.update_props_inheader";
    let b2 = Char.code(data.[ofs + 1])
    and b1 = Char.code(data.[ofs + 2])
    and b0 = Char.code(data.[ofs + 3]) in
    (* Note: this must also work on 64 bit platforms! *)
    (((b land 0x7F) lsl 24) + (b2 lsl 16) + (b1 lsl 8) + b0, ofs + 4)
  end

(** [get_props_inheader data datalen props_inheader] adds to
    [props_inheader] the key-value pairs contained in [data].  The
    keys are uppercased (CGI/1.1 says they must be treated case
    insensitively).

    @raise Failure if the key or val lengths exceed the length of the
    string [data]. *)
let get_props_inheader =
  let rec add data ofs datalen props_inheader =
    if ofs < datalen then begin
      let namelen, ofs = get_length data ofs in
      if ofs >= datalen then failwith "Netcgi_fcgi.get_props_inheader";
      let valuelen, ofs = get_length data ofs in
      let ofs_value = ofs + namelen in
      let ofs_next = ofs_value + valuelen in
      if  ofs_next > datalen then failwith "Netcgi_fcgi.get_props_inheader";
      let name = String.uppercase(String.sub data ofs namelen)
      and value = String.sub data ofs_value valuelen in
      let props_inheader =
	Netcgi_common.update_props_inheader (name, value) props_inheader in
      add data ofs_next datalen props_inheader
    end
    else props_inheader 
  in
  (fun data datalen props_inheader -> 
     add data 0 datalen props_inheader
  )


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
   fcgi primitives. The output side is inherited from fcgi_out_channel,
   i.e. this channel is bidirectional.
 *)
class fcgi_channel fd =
  let prim_ch = new prim_in_channel fd in
object(self)
  inherit fcgi_out_channel fd
    (* This is both an input and output channel *)

  inherit Netchannels.buffered_raw_in_channel
            ~buffer_size:8192
	    prim_ch

  inherit Netchannels.augment_raw_in_channel
    (* reduces additional methods like really_input to the more 
       primitive ones from Netchannels.buffered_raw_in_channel.
       Note that input_line is slow (but not used here)
     *)


  val padding_buffer = String.create 0xFF

  method input_padding len =
    self # really_input padding_buffer 0 len


  val record_buffer = String.create 65536  (* 64K *)

  method record_buffer = record_buffer


  (** [let r = input_record ()] reads the next record from the socket [fd],
      returns the information as a record [r] and puts the data into
     [record_buffer]
   *)
  method input_record() =
    let header = String.create 8 in
    self # really_input header 0 8;
    let version = Char.code(header.[0])
    and id =  Char.code(header.[2]) lsl 8 + Char.code(header.[3])
    and len = Char.code(header.[4]) lsl 8 + Char.code(header.[5])
    and padding = Char.code(header.[6]) in
    self # really_input record_buffer 0 len;
    self # input_padding padding;
    { version = version;
      ty = header.[1];
      id = id;
      length = len }

  (* This function returns the next application record.  Management
     records (of id=0) are dealt with automatically.  (This is very
     useful to get them out of the way for several functions below.) *)
  method input_app_record ~max_conns =
    let r = self # input_record () in
    if r.id = 0 then (
      (* null request ID => management record *)
      if r.ty = fcgi_get_values then (
	let props = 
	  try fst(get_props_inheader record_buffer r.length ([], []))
	  with Failure _ -> [] in
	self # send_values props ~max_conns;
	self # input_app_record ~max_conns
      )
      else (
	self # send_unknown_type r.ty;
	self # input_app_record ~max_conns
      )
    )
    else r

(* Get the next input record of type [ty] and id [id], put the data
   into [record_buffer] and return the record.  

   @raise Abort if th server send a FCGI_ABORT_REQUEST for the current
   request id.

   This function is useful because we do not allow multiplexing and
   because the protocol is nearly sequential.

   BEWARE: you should not call this function after receiving an empty
   record (indicating that the stream is closed), otherwise it will
   loop indefinitely waiting for a record that will never come.
*)
  method input_stream_record ty id ~max_conns =
    let r = self # input_app_record ~max_conns in
    if r.id <> id then (
      (* Another id -- and not a management record.  Close the new
         request and try again. *)
      self # send_end_request r.id 0 CANT_MPX_CONN;
      self # input_stream_record ty id ~max_conns
    )
    else if r.ty = fcgi_abort_request then
      raise(Abort id)
    else if r.ty <> ty then
      (* Not the expected type; ignore the record (who knows, it may be
         some filter data that we never read). *)
      self # input_stream_record ty id ~max_conns
    else
      r (* Record of the desired type *)

  (* [input_begin_request ~max_conns] handles management records and
     skip the other ones till a FCGI_BEGIN_REQUEST is received, in which
     case [(id, role, flags)] is returned. *)
  method input_begin_request ~max_conns : (int * role * int) =
    let r = self # input_app_record ~max_conns in
    if r.ty = fcgi_begin_request && r.length = 8 then (
      let role = 
	Char.code(record_buffer.[0]) lsl 8 + Char.code(record_buffer.[1])
      and flags = 
	Char.code(record_buffer.[2]) in
      match role with
	| 1 -> (r.id, `Responder, flags)
	| 2 -> (r.id, `Authorizer, flags)
	| 3 -> (r.id, `Filter, flags)
	| _ ->
	    (* Rejecting this request that has an unknown role and waiting
   	       for the next one. *)
	    self # send_end_request r.id 0 UNKNOWN_ROLE;
	    self # input_begin_request ~max_conns
    )
    else
      self # input_begin_request ~max_conns
	
  (* Accumulate the stream of params into a list of properties and
     input_header. *)
  method input_props_inheader id ~max_conns props_inheader =
    let r = self # input_stream_record fcgi_params id ~max_conns in
    if r.length = 0 then
      (* End of stream *)
      props_inheader
    else
      let props_inheader = 
	get_props_inheader record_buffer r.length props_inheader in
      self # input_props_inheader id ~max_conns props_inheader

end


(* This channel unwraps user payload from fcgi packets of type [ty]
   (either fcgi_stdin or fcgi_data). 
 *)
class user_in_channel fcgi_ch ty id ~max_conns =
object(self)
  inherit Netchannels.augment_raw_in_channel
    (* Define derived methods. Note that input_line is slow. This is ok
       because the MIME scanner does not rely on it.
     *)

  val mutable buf_pos = 0
  val mutable buf_len = 0

  val mutable pos_in = 0
    
  method input s pos len =
    if buf_len <= 0 then (
      let r = fcgi_ch # input_stream_record ty id ~max_conns in
      if r.length = 0 then
	raise End_of_file;
      buf_pos <- 0;
      buf_len <- r.length
    );
    let l = min len buf_len in
    String.blit (fcgi_ch # record_buffer) buf_pos s pos l;
    buf_pos <- buf_pos + l;
    buf_len <- buf_len - l;
    pos_in <- pos_in + l;
    l


  method close_in() = ()

  method pos_in = pos_in

end



(* When no input of FCGI_DATA is required, we need a dummy object to
   play the input channel role.  *)
class closed_in_obj : Netchannels.in_obj_channel =
object
  method input (_:string) (_:int) (_:int) = raise Netchannels.Closed_channel
  method close_in () = ()
  method pos_in = raise Netchannels.Closed_channel
  method really_input (_:string) (_:int) (_:int) =
    raise Netchannels.Closed_channel
  method input_char () = raise Netchannels.Closed_channel
  method input_byte () = raise Netchannels.Closed_channel
  method input_line () = raise Netchannels.Closed_channel
end


(************************************************************************)
(** Environment *)

(* Creates the environment including the output channel *)
class fcgi_environment ?log_error ~config ~properties ~input_header fcgi_ch id
  : Netcgi_common.cgi_environment =

  let user_ch = new user_out_channel fcgi_ch id in

  let fcgi_log_error id msg : unit =
    fcgi_ch # send_user_data fcgi_stderr id msg 0 (String.length msg) in

object
  inherit cgi_environment ~config ~properties ~input_header user_ch

  (* Override to use the correct channel *)
  method log_error msg = 
    match log_error with
      | None -> fcgi_log_error id msg
      | Some f -> f msg
end


(************************************************************************)
(** CGI abstraction and request handling *)

class type cgi =
object
  inherit Netcgi.cgi
  method role : role
  method data : Netchannels.in_obj_channel
  method data_length : int
  method data_mtime : float
end


class fcgi (role:role) id data_ch ~max_conns env op req_meth args : cgi =
object
  inherit Netcgi_common.cgi env op req_meth args

  val data_stream = data_ch
  val role = role
  val data_length =
    try int_of_string(env#cgi_property "FCGI_DATA_LENGTH") with _ -> 0
  val data_mtime =
    try float_of_string(env#cgi_property "FCGI_DATA_LAST_MOD") with _ -> 0.

  method role = role
  method data = data_stream
  method data_length = data_length
  method data_mtime = data_mtime
end


let handle_request config 
                   output_type arg_store exn_handler f ~max_conns ~log fd =
  let fcgi_ch = new fcgi_channel fd in

  try
    let (id, role, flags) = 
      fcgi_ch # input_begin_request ~max_conns in
      
    let (properties, input_header) =
      fcgi_ch # input_props_inheader id ~max_conns ( [], [] ) in

    let env = 
      new fcgi_environment
	?log_error:log ~config ~properties ~input_header fcgi_ch id in

    (* Now that one knows the environment, one can deal with exn. *)
    exn_handler_default env ~exn_handler
      (fun () ->
         try
	   let input_ch  = 
	     new user_in_channel fcgi_ch fcgi_stdin id ~max_conns in

	   let data_ch =
	     if role = `Filter then
	       new user_in_channel fcgi_ch fcgi_data id ~max_conns 
	     else 
	       new closed_in_obj in

	   (* Note that this will read [input_ch] before [data_ch] *)
	   let cgi = 
	     cgi_with_args 
	       (new fcgi role id data_ch ~max_conns) 
	       env output_type input_ch arg_store in

	   (try
	      f cgi;
              cgi#out_channel#commit_work();
	      cgi#finalize()
	    with e when config.default_exn_handler ->
              cgi#finalize(); raise e);
           None
         with 
	   | Abort _ as e -> Some e
	   | End_of_file as e -> Some e
      )
      ~finally:
      (fun () ->
	 fcgi_ch # send_stderr_end_response id;
	 fcgi_ch # send_stdout_end_response id;
	 fcgi_ch # send_end_request id 0 REQUEST_COMPLETE;
	 fcgi_ch # flush()
      );
    if flags land fcgi_keep_conn = 0 then
      `Conn_close_linger
    else
      `Conn_keep_alive
  with 
    | Abort id ->
	(* FCGI_ABORT_REQUEST received.  The exit_status should come from
	   the application the spec says.  However, in general, the
	   application will not have yet started, so just return 0.
	   (Since we so not allow multiplexed requests, it is more likely
	   that the web server just closes the connection but we handle it
	   anyway.) *)
	`Conn_close
    | End_of_file ->
	`Conn_close
    | error ->
	`Conn_error error
	    

(* [handle_connection fd .. f] handle an accept()ed connection,
   reading incoming records on the file descriptor [fd] and running
   [f] for each incoming request. *)
let rec handle_connection_1 fd ~max_conns ~external_server ~config
    output_type arg_store exn_handler f =
  let fd_cmd =
    handle_request config output_type arg_store exn_handler f ~max_conns 
      ~log:None fd in
  (* FIXME: because of the bug explained in the beginning of this
     file, we would like to close the file descriptor when the request
     is complete.

     If we do nothing, one sees the entire output but get an "idle
     timeout".  If we close [fd], one sometimes get "Broken pipe:
     ... write failed"!  So we just shutdown.  Do we risk fd
     shortage???  

     [gerd]: We need to do a lingering close.
  *)

  let fd_cmd' =
    if external_server then
      fd_cmd 
    else
      if fd_cmd = `Conn_keep_alive then
	`Conn_close_linger   (* Close anyway in this case *) 
      else
	fd_cmd in

  match fd_cmd' with
    | `Conn_close ->
        (try Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ());
	Netlog.Debug.release_fd fd;
	Unix.close fd
    | `Conn_close_linger ->
	Unix.setsockopt_optint fd Unix.SO_LINGER (Some 15);
        (try Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ());
	Netlog.Debug.release_fd fd;
	Unix.close fd
    | `Conn_keep_alive ->
	(* The server is supposed to take care of closing [fd].  (Tail
	   recursiveness is important as many requests may be handled by
	   this fun.)  *)
	handle_connection_1 fd ~external_server ~max_conns
	  ~config output_type arg_store exn_handler f
    | `Conn_error e ->
        (try Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ());
	Netlog.Debug.release_fd fd;
	Unix.close fd;
	raise e

let handle_connection fd ~max_conns ~external_server ~config
    output_type arg_store exn_handler f =
  Netlog.Debug.track_fd 
    ~owner:"Netcgi_fcgi"
    ~descr:("connection from " ^ 
	      try Netsys.string_of_sockaddr(Netsys.getpeername fd)
	      with _ -> "(noaddr)")
    fd;
  handle_connection_1 fd ~max_conns ~external_server ~config
    output_type arg_store exn_handler f


let default_allow server =
  match server with
  | Unix.ADDR_UNIX _ -> true
  | Unix.ADDR_INET(addr,_) ->
      fcgi_web_server_addrs = [] || List.mem addr fcgi_web_server_addrs


let run ?(config=Netcgi.default_config)
    ?(allow=default_allow)
    ?(output_type=(`Direct "":Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic))
    ?(exn_handler=(fun _ f -> f()))
    ?sockaddr
    ?port
    f =
  (* FIXME: Under M$win, the web server communicates with a FCGI script
     that it launches by means of a named pipe [fd] (contrarily to the
     spec).  The requests are all sent through that pipe.  Thus there is
     a single connection. *)

  let sockaddr1 =
    match port with
      | None -> None
      | Some p -> Some(Unix.ADDR_INET(Unix.inet_addr_loopback,p)) in
  let sock = match sockaddr1 with
    | None ->
	(* FastCGI launched by the web server *)
	fcgi_listensock
    | Some sockaddr ->
	(* FastCGI on a distant machine, listen on the given socket. *)
	let sock =
	  Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
	Unix.setsockopt sock Unix.SO_REUSEADDR true;
	Unix.bind sock sockaddr;
	Unix.listen sock 5;
	Netlog.Debug.track_fd
	  ~owner:"Netcgi_fcgi"
	  ~descr:("master " ^ Netsys.string_of_sockaddr sockaddr)
	  sock;
	sock
  in
  let max_conns = 1 (* single process/thread *) in
  while true do
    let (fd, server) = Netsys.restart Unix.accept sock in
    try
      if allow server then (
        let external_server = (match server with
                               | Unix.ADDR_UNIX _ -> false
                               | Unix.ADDR_INET _ -> true) in
	handle_connection fd ~max_conns ~external_server
	  ~config output_type arg_store exn_handler f
      );
    with
    | e when config.default_exn_handler ->
	(* We cannot log the error because [fd] is already closed, and
           we can assume anyway that the connection has crashed before.
           Don't do anything.
         *)
	()
  done


(* To ckeck:

   http://mapserver.gis.umn.edu/cgi-bin/wiki.pl?FastCGIOnWin32
 *)
