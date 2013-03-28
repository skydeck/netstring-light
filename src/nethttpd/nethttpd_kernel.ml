(* $Id: nethttpd_kernel.ml 1641 2011-07-20 19:42:58Z gerd $
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

let dlog = Netlog.Debug.mk_dlog "Nethttpd_kernel" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Nethttpd_kernel" Debug.enable

let () =
  Netlog.Debug.register_module "Nethttpd_kernel" Debug.enable


open Nethttpd_types
open Nethttp
open Nethttp.Header
open Printf

type fatal_error =
    [ `Broken_pipe
    | `Broken_pipe_ignore
    | `Message_too_long
    | `Timeout
    | `Unix_error of Unix.error
    | `Server_error
    ]

let string_of_fatal_error =
  function
    | `Broken_pipe -> "Nethttpd: Broken pipe"
    | `Broken_pipe_ignore -> "Nethttpd: Ignorable broken pipe"
    | `Message_too_long -> "Nethttpd: Message too long, dropping it"
    | `Timeout -> "Nethttpd: Connection timed out"
    | `Unix_error ue -> ("Nethttpd: System error: " ^ Unix.error_message ue)
    | `Server_error -> "Nethttpd: Terminating connection because of internal error"
	

type bad_request_error =
    [ `Bad_header_field of string
    | `Bad_header
    | `Bad_trailer
    | `Bad_request_line
    | `Request_line_too_long
    | `Protocol_not_supported
    | `Unexpected_eof
    | `Format_error of string
    ]

let string_of_bad_request_error =
  function
    | `Bad_header_field s -> "Nethttpd: Bad request header field: " ^ s
    | `Bad_header -> "Nethttpd: Bad request header"
    | `Bad_trailer -> "Nethttpd: Bad request trailer"
    | `Bad_request_line -> "Nethttpd: Bad request line"
    | `Request_line_too_long -> "Nethttpd: Request line too long"
    | `Protocol_not_supported -> "Nethttpd: Prototol not supported"
    | `Unexpected_eof -> "Nethttpd: Bad request: unexpected EOF"
    | `Format_error s -> "Nethttpd: Bad request: " ^ s
	
let status_of_bad_request_error =
  function
    | `Request_line_too_long -> `Request_uri_too_long
    | `Protocol_not_supported -> `Http_version_not_supported
    | _ -> `Bad_request


type data_chunk = string * int * int
type status_line = int * string

type transfer_coding =
    [ `Identity
    | `Chunked
    ]

type resp_token =
    [ `Resp_info_line of (status_line * http_header)
    | `Resp_status_line of status_line
    | `Resp_header of http_header
    | `Resp_body of data_chunk
    | `Resp_trailer of http_trailer
    | `Resp_end
    | `Resp_action of (unit -> unit)
    ]

let resp_100_continue = 
  `Resp_info_line((100, "Continue"), new Netmime.basic_mime_header [])

exception Send_queue_empty

type front_token = 
   [ `Resp_wire_data of data_chunk    (* everything else *)
   | `Resp_end 
   ]
type front_token_x = 
   [ front_token
   | `Resp_wire_action of (unit->unit)
   ]
type front_token_opt = [ `None | front_token_x ]
type resp_state =
    [ `Inhibited | `Queued | `Active | `Processed | `Error | `Dropped ]

type announcement =
    [`Ignore | `Ocamlnet | `Ocamlnet_and of string | `As of string ]


let string_of_front_token_x =
  function
    | `Resp_wire_data(s,pos,len) ->
	let n = min len 20 in
	sprintf 
	  "Resp_wire_data(%s%s)"
	  (String.escaped (String.sub s pos n))
	  (if len > n then "..." else "")
    | `Resp_end ->
	"Resp_end"
    | `Resp_wire_action _ ->
	"Resp_wire_action"



class type http_response  =
object
  method state : resp_state
  method set_state : resp_state -> unit
  method bidirectional_phase : bool
  method set_callback : (unit -> unit) -> unit
  method send : resp_token -> unit
  method send_queue_empty : bool
  method protocol : protocol
  method close_connection : bool
  method transfer_encoding : transfer_coding
  method front_token : front_token
  method advance : int -> unit
  method body_size : int64
end


let string_of_state =
  function
    | `Inhibited -> "Inhibited"
    | `Queued    -> "Queued"
    | `Active    -> "Active"
    | `Processed -> "Processed"
    | `Error     -> "Error"
    | `Dropped   -> "Dropped"


(* TODO:
 * - make http_repsonse_impl thread-safe
 * - implement trailers
 *)
class http_response_impl ?(close=false) ?(suppress_body=false) fdi
                         (req_version : protocol) 
                         (ann_server : announcement) : http_response =
  (* - [close]: If true, the connection will be closed after this response
   * - [suppress_body]: If true, the body will not be transmitted (e.g. in response
   *   to a HEAD request)
   * - [req_version]: The version of the request. Used to limit features in the
   *   response to what the client understands.
   *)
object(self)
  val resp_version = `Http((1,1),[])
  val mutable state = `Queued
  val mutable accept = (`Status : [ `Status | `Header | `Body | `End | `None] )
  val mutable suppress_body = suppress_body
  val mutable front_token = (`None : front_token_opt)
  val mutable queue = (Queue.create() : front_token_x Queue.t)
  val mutable close_connection = close
  val mutable transfer_encoding = `Identity
  val mutable announced_content_length = None
  val mutable real_length = 0L   (* int64 *)
  val mutable callback = (fun () -> ())
  val mutable bidirectional_phase = false

  method state = state

  method bidirectional_phase = bidirectional_phase

  method set_state s = 
    let old_state = state in
    state <- s;
    dlogr (fun () ->
	     sprintf "FD %Ld resp-%d: response state: %s"
	       fdi (Oo.id self) (string_of_state s));
    if s <> old_state && (s = `Processed || s = `Error || s = `Dropped) then (
      (* do all actions on the queue *)
      try
	while true do
	  match Queue.take queue with
	    | `Resp_wire_action f -> ( try f() with _ -> () )
	    | _ -> ()
	done
      with
	  Queue.Empty -> ()
    );
    if s <> old_state then callback();

  method set_callback f = callback <- f

  method send tok =
    match tok with
      | `Resp_info_line((code, phrase), info_header) ->
	  if code < 100 || code > 199 then 
	    failwith "Nethttpd_kernel.http_response: Bad informational status code";
	  if accept <> `Status then
	    failwith "Nethttpd_kernel.http_response: Cannot send status line now";
	  ( match req_version with
	      | `Http((1,n),_) when n >= 1 ->
		  let s = 
		    Printf.sprintf "%s %3d %s\r\n" 
		      (string_of_protocol resp_version) code phrase in
		  Queue.push (`Resp_wire_data (s, 0, String.length s)) queue;
		  (* Convert the header to a data chunk: *)
		  let b = Netbuffer.create 256 in 
		     (* Expect a short/empty header in most cases *)
		  let ch = new Netchannels.output_netbuffer b in
		  Mimestring.write_header ch info_header#fields;
		  Queue.push 
		    (`Resp_wire_data (Netbuffer.unsafe_buffer b, 0, Netbuffer.length b))
		    queue;
		  Queue.push
		    (`Resp_wire_action (fun () -> bidirectional_phase <- true))
		    queue
	      | _ ->
		  ()   (* Suppress this for HTTP 1.0 and lower *)
	  )
      | `Resp_status_line (code, phrase) ->
	  if code < 200 || code > 999 then 
	    failwith "Nethttpd_kernel.http_response: Bad final status code";
	  if accept <> `Status then
	    failwith "Nethttpd_kernel.http_response: Cannot send status line now";
	  let s = 
	    Printf.sprintf "%s %03d %s\r\n" 
	      (string_of_protocol resp_version) code phrase in
	  Queue.push
	    (`Resp_wire_action (fun () -> bidirectional_phase <- false))
	    queue;
	  Queue.push (`Resp_wire_data (s, 0, String.length s)) queue;
	  accept <- `Header
      | `Resp_header resp_header ->
	  if accept <> `Header then 
	    failwith "Nethttpd_kernel.http_response: Cannot send header now";
	  (* Set [announced_content_length]: *)
	  ( try
	      let len = get_content_length resp_header in  (* or Not_found *)
	      announced_content_length <- Some len;
	      dlogr (fun () ->
		       sprintf "FD %Ld resp-%d: response anncounced_content_length=%Ld"
			 fdi (Oo.id self) len)
	    with
		Not_found -> 
		  announced_content_length <- None
	  );
	  (* Update the values for [close_connection] and [transfer_encoding]: *)
	  ( match req_version with
	      | `Http((1,n),_) when n >= 1 ->
		  transfer_encoding <- ( match announced_content_length with
					   | Some _ -> `Identity
					   | None -> `Chunked );
		  dlogr (fun () ->
			   sprintf "FD %Ld resp-%d: response transfer_encoding=%s"
			     fdi (Oo.id self)
			     (match transfer_encoding with
				| `Identity -> "identity"
				| `Chunked -> "chunked"
			     ))
	      | _ ->
		  (* Other protocol version: fall back to conservative defaults *)
		  close_connection <- true;
		  transfer_encoding <- `Identity;
		  dlogr (fun () ->
			   sprintf "FD %Ld resp-%d: response \
                             transfer_encoding=identity; close_connection=true"
			     fdi (Oo.id self))
	  );
	  (* Update the header: *)
	  ( match transfer_encoding, suppress_body with
	      | (`Identity, false)
	      | (_, true) ->
		  resp_header # delete_field "Transfer-Encoding"
	      | (`Chunked, false) -> 
		  set_transfer_encoding resp_header ["chunked", []]
	  );
	  resp_header # delete_field "Trailer";
	  set_date resp_header (Unix.time());
	  ( match close_connection with
	      | false -> resp_header # delete_field "Connection"
	      | true  -> set_connection resp_header ["close"]
	  );
	  resp_header # delete_field "Upgrade";
	  ( match ann_server with
	      | `Ignore -> ()
	      | `Ocamlnet ->
		  let sh = "Ocamlnet/" ^ Netconst.ocamlnet_version in
		  set_server resp_header sh
	      | `Ocamlnet_and s ->
		  let sh = s ^ " Ocamlnet/" ^ Netconst.ocamlnet_version in
		  set_server resp_header sh
	      | `As sh ->
		  set_server resp_header sh
	  );
	  (* Convert the header to a data chunk: *)
	  let b = Netbuffer.create 4096 in
	  let ch = new Netchannels.output_netbuffer b in
	  Mimestring.write_header ch resp_header#fields;
	  Queue.push 
	    (`Resp_wire_data (Netbuffer.unsafe_buffer b, 0, Netbuffer.length b)) queue;
	  (* What is accepted next: *)
	  accept <- `Body
      | `Resp_body ((s,pos,len) as data) ->
	  if accept <> `Body then 
	    failwith "Nethttpd_kernel.http_response: Cannot send body now";
	  if pos < 0 || len < 0 || pos + len > String.length s then
	    invalid_arg "Nethttpd_kernel.http_response#send";
	  if not suppress_body then ( 
	    match transfer_encoding with
	      | `Identity ->
		  (* Check whether the length fits to the announced length: *)
		  let len' =
		    match announced_content_length with
		      | None -> len
		      | Some ann_len ->
			  Int64.to_int
			    (min (Int64.of_int len) (Int64.sub ann_len real_length))
		  in
		  if len' > 0 then
		    Queue.push (`Resp_wire_data (s,pos,len')) queue;
		  if len > 0 && len' = 0 then
		    dlogr (fun () ->
			     sprintf "FD %Ld resp-%d: response warning: \
                                  response is longer than announced" 
			       fdi (Oo.id self))

	      | `Chunked ->
		  if len > 0 then (
		    (* Generate the chunk header: *)
		    let u = 
		      Printf.sprintf "%x\r\n" len in
		    Queue.push (`Resp_wire_data(u,0,String.length u)) queue;
		    (* Output the chunk: *)
		    Queue.push (`Resp_wire_data data) queue;
		    (* Framing: *)
		    Queue.push (`Resp_wire_data ("\r\n", 0, 2)) queue;
		  )
	  );
	  real_length <- Int64.add real_length (Int64.of_int len);
	  if real_length < 0L then   (* Check for wrap around *)
	    failwith "Nethttpd_kernel: response too long";
      | `Resp_trailer resp_trailer ->
	  if accept <> `Body then 
	    failwith "Nethttpd_kernel.http_response: Cannot send trailer now";
	  accept <- `End;
	  (* trailers are ignored for now *)
      | `Resp_end ->
	  if accept <> `Body && accept <> `End then
	    failwith "Nethttpd_kernel.http_response: Cannot finish response now";
	  if not suppress_body then ( 
	    match transfer_encoding with
	      | `Identity ->
		  (* Check whether the length fits to the announced length: *)
		  ( match announced_content_length with
		      | None -> ()
		      | Some ann_len ->
			  if ann_len > real_length then
			    close_connection <- true
		  );
		  Queue.push `Resp_end queue
	      | `Chunked ->
		  (* Add the last-chunk: *)
		  let s = "0\r\n\r\n" in
		  Queue.push (`Resp_wire_data(s,0,String.length s)) queue;
		  Queue.push `Resp_end queue;
	  );
	  accept <- `None
      | `Resp_action f ->
	  Queue.push (`Resp_wire_action f) queue

  method send_queue_empty = 
    (state = `Inhibited) ||
    ( (front_token = `None) && (Queue.is_empty queue) )

  method protocol = resp_version

  method close_connection = close_connection

  method transfer_encoding = transfer_encoding

  method body_size = real_length

  method front_token : front_token =
    if state = `Inhibited then raise Send_queue_empty;
    match front_token with
      | `None ->
	  ( try
	      let tok = Queue.take queue in
	      front_token <- (tok :> front_token_opt);
	      dlogr (fun () ->
		       sprintf "FD %Ld resp-%d: response new front_token: %s"
			 fdi (Oo.id self) (string_of_front_token_x tok));
	      self # front_token
	    with Queue.Empty ->
	      raise Send_queue_empty
	  )
      | `Resp_wire_action f ->
	  front_token <- `None;
	  dlogr (fun () -> sprintf "FD %Ld resp-%d: response Resp_wire_action" 
		   fdi (Oo.id self));
	  f();
	  self # front_token

      | #front_token as other ->
	  other


  method advance n =
    if n > 0 then (
      ignore(self # front_token);  (* such that we can assert front_token <> `None *)
      match front_token with
	| `Resp_wire_data (s,pos,len) ->
	    if n > len then 
	      invalid_arg "Nethttpd_kernel#http_response: Cannot advance past the current data chunk";
	    let len' = len - n in
	    front_token <- if len'=0 then `None else `Resp_wire_data(s,pos+n,len');
	    if front_token = `None && Queue.is_empty queue then callback()
	| `Resp_end ->
	    failwith "Nethttpd_kernel#http_response: Cannot advance past the end of the response"
	| `Resp_wire_action _ -> assert false
	| `None -> assert false
    )
end

let send_static_response resp status hdr_opt body =
  let code = int_of_http_status status in
  let text = string_of_http_status status in
  let h = ( match hdr_opt with 
	      | None -> new Netmime.basic_mime_header []
	      | Some h -> h ) in
  ( try ignore(h # field "Content-Type")
    with Not_found -> h # update_field "Content-type" "text/html";
  );
  h # update_field "Content-Length" (string_of_int (String.length body));
  resp # send (`Resp_status_line(code, text));
  resp # send (`Resp_header h);
  resp # send (`Resp_body(body, 0, String.length body));
  resp # send `Resp_end
;;


let send_file_response resp status hdr_opt fd length =
  Netlog.Debug.track_fd
    ~owner:"Nethttpd_kernel"
    ~descr:"file response"
    fd;
  let hdr =
    match hdr_opt with
      | None ->
	  new Netmime.basic_mime_header []
      | Some h -> h in
  ( match status with
      | `No_content
      | `Reset_content
      | `Not_modified -> ()
      | _ ->
	  ( try ignore(hdr # field "Content-Type")
	    with Not_found -> hdr # update_field "Content-type" "text/html";
	  );
  );
  hdr # update_field "Content-Length" (Int64.to_string length);
  let code = int_of_http_status status in
  let phrase = string_of_http_status status in
  resp # send (`Resp_status_line (code, phrase));
  resp # send (`Resp_header hdr);
  let fd_open = ref true in
  let buf = String.create 8192 in
  let len = ref length in
  let rec feed() =
    match resp # state with
      | `Inhibited | `Queued -> assert false
      | `Active ->
	  ( try
	      let m = min 8192L !len in
	      let n = Unix.read fd buf 0 (Int64.to_int m) in  (* or Unix_error *)
	      if n > 0 then (
		len := Int64.sub !len (Int64.of_int n);
		resp # send (`Resp_body(buf, 0, n));  (* no copy of [buf]! *)
		resp # send (`Resp_action feed)       (* loop *)
	      )
	      else (
		resp # send `Resp_end;
		fd_open := false;
		Netlog.Debug.release_fd fd;
		Unix.close fd;
	      )
	    with
	      | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK),_,_) ->
		  assert false    (* Cannot happen when [fd] is a file! *)
	      | Unix.Unix_error(Unix.EINTR, _, _) ->
		  feed()
	  )
      | `Processed | `Error | `Dropped ->
	  if !fd_open then (
	    Netlog.Debug.release_fd fd;
	    Unix.close fd;
	  );
	  fd_open := false
  in
  resp # send (`Resp_action feed)
;;


type request_line = http_method * protocol

type req_token =
    [ `Req_header of request_line * http_header * http_response
    | `Req_expect_100_continue
    | `Req_body of data_chunk
    | `Req_trailer of http_trailer
    | `Req_end
    | `Eof
    | `Fatal_error of fatal_error
    | `Bad_request_error of bad_request_error * http_response
    | `Timeout
    ]

let string_of_req_token =
  function
    | `Req_header(((req_method, req_uri), req_proto), hd, resp) ->
	sprintf "Req_header(%s %s)" req_method req_uri
    | `Req_expect_100_continue ->
	"Req_expect_100_continue"
    | `Req_body(s,pos,len) ->
	let n = min len 20 in
	sprintf "Req_body_data(%s%s)"
	  (String.escaped (String.sub s pos n))
	  (if len > n then "..." else "")
    | `Req_trailer tr ->
	"Req_trailer"
    | `Req_end ->
	"Req_end"
    | `Eof ->
	"Eof"
    | `Fatal_error e ->
	sprintf "Fatal_error(%s)" (string_of_fatal_error e)
    | `Bad_request_error (e, resp) ->
	sprintf "Bad_request_error(%s)" (string_of_bad_request_error e)
    | `Timeout -> 
	"Timeout"

exception Recv_queue_empty

exception Buffer_exceeded
  (* Internally used by HTTP implementation: The buffer was not large enough for the
   * current token
   *)

exception Timeout
  (* Internally used by HTTP implementation: socket blocks for too long *)

exception Fatal_error of fatal_error
  (* Internally used by HTTP implementation: Indicate fatal error *)

exception Bad_request of bad_request_error
  (* Internally used by HTTP implementation: Indicate bad request *)


class type http_protocol_config =
object
  method config_max_reqline_length : int
  method config_max_header_length : int
  method config_max_trailer_length : int
  method config_limit_pipeline_length : int
  method config_limit_pipeline_size : int
  method config_announce_server : announcement
  method config_suppress_broken_pipe : bool
end


let http_find_line_start s pos len =
  try Mimestring.find_line_start s pos len
  with Not_found -> raise Buffer_exceeded


let http_find_double_line_start s pos len =
  try Mimestring.find_double_line_start s pos len
  with Not_found -> raise Buffer_exceeded


let parse_req_line s pos len =
  (* Parses a request line: "WORD WORD WORD\r\n", where \r is optional.
     The words are separated by spaces or TABs. Once we did this with
     a regexp, but this caused stack overflows in the regexp interpreter
     for long request lines.

     Raises Not_found if not parseable.

     We don't support HTTP/0.9. One could recognize it easily because the third
     word is missing.
   *)
  let e = pos+len in
  let rec next_sep p =
    if p >= e then raise Not_found else
      let c = s.[p] in
      if c = ' ' || c = '\t' then p else
	if c = '\r' || c = '\n' then raise Not_found
	else next_sep(p+1) in
  let rec next_end p =
    if p >= e then raise Not_found else
      let c = s.[p] in
      if c = '\r' || c = '\n' then p 
      else next_end(p+1) in
  let rec skip_sep p =
    if p >= e then raise Not_found else
      let c = s.[p] in
      if c = ' ' || c = '\t' 
      then skip_sep(p+1)
      else p in
  let p1 = next_sep pos in
  let q1 = skip_sep p1 in
  let p2 = next_sep q1 in
  let q2 = skip_sep p2 in
  let p3 = next_end q2 in
  if s.[p3] = '\n' then (
    if p3+1 <> e then raise Not_found
  ) else (
    if s.[p3] <> '\r' then raise Not_found;
    if p3+2 <> e then raise Not_found;
    if s.[p3+1] <> '\n' then raise Not_found;
  );
  let w1 = String.sub s pos (p1-pos) in
  let w2 = String.sub s q1 (p2-q1) in
  let w3 = String.sub s q2 (p3-q2) in
  (w1, w2, w3)

  
let is_hex =
  function
    | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
    | _ -> false


let parse_chunk_header s pos len =
  (* Parses "HEXNUMBER OPTIONAL_SEMI_AND_IGNORED_EXTENSION CRLF",
     or raises Not_found.
   *)
  let e = pos+len in
  let rec skip_hex_number p =
    if p >= e then raise Not_found else
      let c = s.[p] in
      if is_hex c then skip_hex_number(p+1)
      else p in
  let p1 = skip_hex_number pos in
  if p1 = pos || p1 >= e then raise Not_found;
  let c1 = s.[p1] in
  if c1=';' then (
    let p2 =
      Mimestring.find_line_start s p1 (e - p1) in
    if p2 <> e then raise Not_found
  )
  else (
    if c1 = '\n' then (
      if p1+1 <> e then raise Not_found
    ) else (
      if p1+2 <> e then raise Not_found;
      if s.[p1+1] <> '\n' then raise Not_found
    )
  );
  String.sub s pos (p1-pos)


type cont =
    [ `Continue of unit -> cont | `Restart | `Restart_with of unit -> cont ]


module StrSet = Set.Make(String)

class http_protocol (config : #http_protocol_config) (fd : Unix.file_descr) =
  let pa = Netsys_posix.create_poll_array 1 in
  let fdi = Netsys.int64_of_file_descr fd in
object(self)
  val mutable resp_queue = Queue.create()
    (* The queue of [http_response] objects. The first is currently being transmitted *)

  val mutable recv_buf = Netbuffer.create 8192
    (* The buffer of received data that have not yet been tokenized *)

  val mutable recv_eof = false
    (* Whether EOF has been seen. This is also set if the protocol engine is no
     * longer interested in any input because of processing errors
     *)

  val mutable recv_queue = (Queue.create() : (req_token * int) Queue.t)
    (* The queue of received tokens. The integer is the estimated buffer size *)

  val mutable recv_cont = (fun () -> `Restart)
    (* The continuation processing the beginning of [recv_buf] *)

  val mutable test_coverage = StrSet.empty
    (* Only used for certain tests: This set contains tokens for cases the program
     * ran into.
     *)

  val mutable pipeline_len = 0

  val mutable recv_queue_byte_size = 0

  val mutable waiting_for_next_message = true
    (* Updated by the input acceptor as side effect *)

  val mutable need_linger = true
    (* Whether we need a lingering close to reliable close the connection from the
     * server side.
     *)

  val linger_buf = String.create 256
    (* A small buffer for data thrown away *)


  initializer (
    recv_cont <- self # accept_header 0;
    Unix.set_nonblock fd
  )

  method cycle ?(block=0.0) () = 
    try
      (* Block until we have something to read or write *)
      if block <> 0.0 then
	self # block block;
      (* Accept any arriving data, and process that *)
      self # accept_data();
      (* Transmit any outgoing data *)
      self # transmit_response();


    with
      | Fatal_error e ->
	  dlog (sprintf "FD %Ld: fatal error" fdi);
	  self # abort e;
      | Timeout ->
	  dlog (sprintf "FD %Ld: timeout" fdi);
	  self # timeout()
      | Bad_request e ->
	  (* Stop only the input side of the engine! *)
	  dlog (sprintf "FD %Ld: bad request" fdi);
	  self # stop_input_acceptor();
	  let resp = 
	    new http_response_impl ~close:true fdi (`Http((1,0),[])) 
	      config#config_announce_server in
	  self # push_recv (`Bad_request_error(e, resp), 0);
	  self # push_recv (`Eof, 0);
	  resp # set_state `Queued;   (* allow response from now on *)
	  Queue.push resp resp_queue

  method private block d =
    (* If d < 0 wait for undefinite time. If d >= 0 wait for a maximum of d seconds.
     * On expiration, raise [Timeout].
     *)
    dlogr (fun () -> sprintf "FD %Ld: block %f" fdi d);
    let f_input = self#do_input in
    let f_output = self#do_output in
    if not f_input && not f_output then raise Timeout;
    Netsys_posix.set_poll_cell pa 0
      { Netsys_posix.poll_fd = fd;
	poll_req_events = Netsys_posix.poll_req_events f_input f_output false;
	poll_act_events = Netsys_posix.poll_null_events()
      };
    let t = Unix.gettimeofday() in
    try
      let n = Netsys_posix.poll pa 1 d in
      if n = 0 then raise Timeout;
      (* Check for error: *)
      let c = Netsys_posix.get_poll_cell pa 0 in
      let have_error = 
	Netsys_posix.poll_err_result c.Netsys_posix.poll_act_events in
      if have_error then (
	(* Now find out which error. Unfortunately, a simple Unix.read on
           the socket seems not to work.
	 *)
(*
 -- interpreting POLL_HUP here is difficult. Generally, POLL_HUP can also
    be set when the other side sends EOF, i.e. a harmless condition

	if Netsys_posix.poll_hup_result c.Netsys_posix.poll_act_events then
	  raise(Fatal_error `Broken_pipe);
 *)
	let code =
	  Unix.getsockopt_int fd Unix.SO_ERROR in
	let error =
	  Netsys.unix_error_of_code code in
	let exn_arg =
	  match error with
	    | Unix.EPIPE 
	    | Unix.ECONNRESET -> `Broken_pipe
	    | _ -> `Unix_error error in
	raise(Fatal_error exn_arg)
      )
	
    with
	Unix.Unix_error(Unix.EINTR,_,_) ->
	  dlog (sprintf "FD %Ld: block: EINTR" fdi);
	  if d < 0.0 then
	    self # block d
	  else (
	    let t' = Unix.gettimeofday() in
	    self # block (max 0.0 (t' -. t))
	  )


  method private case name =
    test_coverage <- StrSet.add name test_coverage

  method test_coverage =
    StrSet.elements test_coverage

  (* ---- Process received data ---- *)

  method private stop_input_acceptor() =
    recv_cont <- (fun () -> `Restart);
    recv_eof <- true

  method private accept_data () =
    (* Check whether the socket is ready and we can receive input data. New data
     * are appended to [recv_buf]. Continue with the next acceptor 
     *)
    let continue =
      try
	if recv_eof then (
	  if need_linger then (
	    (* Try to linger in the background... *)
	    dlogr
	      (fun () ->
		 sprintf "FD %Ld: lingering for remaining input" fdi);
	    let n = Unix.recv fd linger_buf 0 (String.length linger_buf) [] in
	       (* or Unix_error *)
	    if n=0 then need_linger <- false  (* that's it! *)
	  );
	  false  (* no new data *)
	)
	else (
	  if self # do_input then (
	    dlogr (fun () -> sprintf "FD %Ld: recv" fdi);
	    let n = Netbuffer.add_inplace   (* or Unix_error *)
	      recv_buf
	      (fun s pos len -> Unix.recv fd s pos len []) in
	    if n=0 then (
	      recv_eof <- true;
	      need_linger <- false;
	      dlog (sprintf "FD %Ld: got EOF" fdi);
	    )
	    else
	      dlogr (fun () -> sprintf "FD %Ld: got %d bytes" fdi n);
	    true
	  )
	  else
	    false (* no new data *)
	)
      with
	| Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK), _,_)->
	    dlogr (fun () -> sprintf "FD %Ld: recv EWOULDBLOCK" fdi);
	    false             (* socket not ready *)
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    dlogr (fun () -> sprintf "FD %Ld: recv EINTR" fdi);
	    false             (* got signal *)
	| Unix.Unix_error(Unix.ECONNRESET, _,_) ->
	    dlogr (fun () -> sprintf "FD %Ld: recv ECONNRESET" fdi);
	    self # abort `Broken_pipe;
	    false
	| Unix.Unix_error(e, _, _) ->
	    dlogr (fun () -> sprintf "FD %Ld: recv ERROR" fdi);
	    self # abort (`Unix_error e);
	    false
    in
    if continue then 
      self # accept_loop()

  method private accept_loop () =
    let next_cont_opt = recv_cont() in
    ( match next_cont_opt with
	| `Continue next_cont -> 
	    recv_cont <- next_cont; 
	    self # accept_loop()
	| `Restart -> 
	    ()    (* Stop here for now, restart the last function the next time *)
	| `Restart_with next_cont ->
	    recv_cont <- next_cont
	      (* Stop here, too, but use [next_cont] the next time *)
    )


  (* The following methods are only called by [accept_loop] when [recv_cont]
   * is set to one of the methods. They return [`Continue] when another method
   * should continue to parse the message. They return [`Restart] when they could not
   * finish parsing, and they want to be called again the next time [cycle] is
   * invoked. They return [`Restart_with] when they could not finish parsing,
   * but another method is scheduled for the next time.
   *
   * Programming rules:
   * - When the buffer boundary is hit (often indicated by [Buffer_exceeded]),
   *   check on EOF. Furthermore, delete processed parts from the input buffer.
   *
   * - After parts of the buffer have been deleted, [`Restart] must not be returned
   *   (the position would be wrong)
   *)

  method private accept_header pos () : cont =
    (* Check whether the beginning of [recv_buf] contains the full request line and
     * the full header. If so, process that. If not, still check on premature EOF.
     *)
    (* (1) Skip any CRLF sequences at the beginning
     * (2) Check there is at least one character
     * (2a) Try to parse the request line
     * (3) Search the next occurence of CRLF CRLF
     *     CHECK: Maybe use a faster algorithm, e.g. Knuth-Morris-Pratt
     * (4) Try to parse this block
     * (5) Create the corresponding response object, and put the token onto the queue
     * (6) Go on with body parsing
     *
     * If we ever hit the bounding of the buffer, raise Buffer_exceeded. This means
     * we don't have the header block yet.
     *)
    IFDEF Testing THEN self # case "accept_header" ELSE () END;
    waiting_for_next_message <- true;
    let l = Netbuffer.length recv_buf in
    let s = Netbuffer.unsafe_buffer recv_buf in
    let block_start = Mimestring.skip_line_ends s pos (l - pos) in  (* (1) *)
    try
      (* (2) *)
      if block_start = l || (block_start+1 = l && s.[block_start] = '\013') then (
	IFDEF Testing THEN self # case "accept_header/1" ELSE () END;
	raise Buffer_exceeded;
      );
      (* (2a) *)
      let reqline_end =
	try
	  http_find_line_start s block_start (l - block_start)
	with
	    Buffer_exceeded ->
	      IFDEF Testing THEN self # case "accept_header/reqline_ex" ELSE () END;
	      waiting_for_next_message <- false;
	      if l-block_start > config#config_max_reqline_length then
		raise (Bad_request `Request_line_too_long);
	      raise Buffer_exceeded
      in
      if reqline_end-block_start > config#config_max_reqline_length then
	raise (Bad_request `Request_line_too_long);
      waiting_for_next_message <- false;
      let ((meth,uri),req_version) as request_line =
	try
	  let (meth, uri, proto_s) = 
	    parse_req_line s block_start (reqline_end - block_start) in
	    (* or Not_found *)
	  IFDEF Testing THEN self # case "accept_header/4" ELSE () END;
	  let proto = protocol_of_string proto_s in
	  ( match proto with
	      | `Http((1,_),_) -> ()
	      | _ -> raise (Bad_request `Protocol_not_supported)
	  );
	  ((meth, uri), proto)
	with
	  | Not_found ->
	      (* This is a bad request. Response should be "Bad Request" *)
	      IFDEF Testing THEN self # case "accept_header/3" ELSE () END;
	      raise (Bad_request `Bad_request_line)
      in
      (* (3) *)
      let config_max_header_length = config # config_max_header_length in
      let block_end =
	try
	  http_find_double_line_start s block_start (l - block_start)
	with
	    Buffer_exceeded -> 
	      IFDEF Testing THEN self # case "accept_header/2" ELSE () END;
	      if l-block_start > config_max_header_length then
		raise (Fatal_error `Message_too_long);
	      raise Buffer_exceeded
      in
      if block_end - block_start > config_max_header_length then
	raise (Fatal_error `Message_too_long);
      (* (4) *)
      (* For simplicity, we create an in_obj_channel reading the portion of the
       * buffer.
       *)
      let ch = new Netchannels.input_string 
		 ~pos:reqline_end ~len:(block_end - reqline_end) s in
      let str = new Netstream.input_stream ch in
      (* TODO: This is quite expensive. Create a new netstream class for cheaper access 
       * in this case where we only read from a constant string.
       *)
      let req_h = 
	try Netmime.read_mime_header str 
	with Failure _ -> 
	  IFDEF Testing THEN self # case "accept_header/5" ELSE () END;
	  raise(Bad_request `Bad_header) in
      (* (5) *)
      let close = 
	match req_version with
	  | `Http((1,0),_) -> false   (* Ignore "Connection" header *)
	  | `Http((1,n),_) when n >= 1 ->
	      (try List.mem "close" (get_connection req_h) with Not_found -> false)
	  | _ -> false in
      let suppress_body = (meth = "HEAD") in
      let resp = 
	new http_response_impl ~close ~suppress_body fdi (snd request_line)
	  config#config_announce_server in
      self # push_recv 
	(`Req_header (request_line, req_h, resp), block_end-block_start);
      Queue.push resp resp_queue;
      (* (6) *)
      `Continue(self # accept_body_start meth req_version req_h resp block_end)

    with
      | Buffer_exceeded ->
	  IFDEF Testing THEN self # case "accept_header/exceeded" ELSE () END;
	  if recv_eof then (
	    if l = block_start then (   (* Regular EOF *)
	      IFDEF Testing THEN self # case "accept_header/regeof" ELSE () END;
	      self # push_recv (`Eof, 0);
	      `Restart_with (fun () -> `Restart)
	    )
	    else (
	      IFDEF Testing THEN self # case "accept_header/eof" ELSE () END;
	      raise(Bad_request `Unexpected_eof)
	    )
	  )
	  else (
	    IFDEF Testing THEN self # case "accept_header/restart" ELSE () END;
	    Netbuffer.delete recv_buf 0 block_start;
	    `Restart_with (self # accept_header 0)
	  )

  method private accept_body_start meth req_version req_h resp pos () : cont =
    (* Parse the start of the body at byte [pos] of [recv_buf]. This function
     * only checks the transfer encoding, and passes over to
     * [accept_body_identity] or [accept_body_chunked].
     *)
    IFDEF Testing THEN self # case "accept_body_start" ELSE () END;
    let is_http_1_1 =
      function
	| `Http((1,1),_) -> true
	| _ -> false in
    try
      ( match req_version with
	  | `Http((1,n),_) when n>=1 ->
	      let expect_list = 
		try get_expect req_h with Not_found -> []  in
	      let rfc2068_expect = 
		(is_http_1_1 req_version && (meth = "POST" || meth = "PUT")) in
	      let rfc2616_expect = 
		List.exists (fun (tok,_,_) -> tok = "100-continue") expect_list in
	      if rfc2068_expect || rfc2616_expect then (
		IFDEF Testing THEN self # case "accept_body_start/100-continue" ELSE () END;
		self # push_recv (`Req_expect_100_continue, 0);
		if resp#state = `Inhibited then
		  resp # set_state `Queued   (* allow response from now on *)
	      )
	  | _ -> ()
      );
      let enc_list = try get_transfer_encoding req_h with Not_found -> [] in
      let chunked_encoding =
	(* The RFC talks about "non-identity transfer encoding"... *)
	match enc_list with
	    [] | ["identity",_] -> false
	  | _ -> true in
      if chunked_encoding then (
	IFDEF Testing THEN self # case "accept_body_start/chunked" ELSE () END;
	`Continue (self # accept_body_chunked req_h resp pos)
      )
      else (
	let remaining_length = 
	  try Some(get_content_length req_h) with Not_found -> None in
	IFDEF Testing THEN 
	  if remaining_length = None then
	    self # case "accept_body_start/empty" 
	  else
	    self # case "accept_body_start/identity" 
	ELSE () END;
	`Continue (self # accept_body_identity req_h resp pos remaining_length)
      )
    with
	Bad_header_field name ->
	  IFDEF Testing THEN self # case "accept_body_start/bad_header_field" ELSE () END;
	  raise(Bad_request (`Bad_header_field name))

  method private accept_body_identity req_h resp pos remaining_length () : cont =
    (* Accept a body with no transfer encoding. The body continues at byte [pos].
     * In [remaining_length], the number of missing bytes is remembered until
     * the body is complete. [None] means there was neither [Content-length] nor
     * [Transfer-Encoding], so the body is empty (e.g. for GET).
     *)
    IFDEF Testing THEN self # case "accept_body_identity" ELSE () END;
    let l = Netbuffer.length recv_buf in
    match remaining_length with
      | Some rl ->
	  let have_length = Int64.of_int (l - pos) in
	  let take_length = min rl have_length in
	  let n = Int64.to_int take_length in
	  if n > 0 then
	    self # push_recv (`Req_body(Netbuffer.sub recv_buf pos n, 0, n), n);
	  let rl' = Int64.sub rl take_length in
	  if rl' > 0L then (
	    IFDEF Testing THEN self # case "accept_body_identity/exceeded" ELSE () END;
	    (* We hit the buffer boundary *)
	    if recv_eof then (
	      (* This request was prematurely terminated by EOF. Simply drop it. *)
	      IFDEF Testing THEN self # case "accept_body_identity/eof" ELSE () END;
	      raise(Bad_request `Unexpected_eof)
	    )
	    else (
	      (* Need to read the remaining part of the request: *)
	      IFDEF Testing THEN self # case "accept_body_identity/restart" ELSE () END;
	      Netbuffer.clear recv_buf;
	      `Restart_with(self # accept_body_identity req_h resp 0 (Some rl'))
	    )
	  )
	  else (
	    (* This was the last part of the message. *)
	    IFDEF Testing THEN self # case "accept_body_identity/last" ELSE () END;
	    self # push_recv (`Req_end, 0);
	    pipeline_len <- pipeline_len + 1;
	    if resp#state = `Inhibited then
	      resp # set_state `Queued;   (* allow response from now on *)
	    `Continue(self # accept_header (pos+n))
	  )
      | None ->
	  self # push_recv (`Req_end, 0);
	  pipeline_len <- pipeline_len + 1;
	  if resp#state = `Inhibited then
	    resp # set_state `Queued;   (* allow response from now on *)
	  `Continue(self # accept_header pos)

  method private accept_body_chunked req_h resp pos () : cont =
    (* Check for a chunk header at byte position [pos]. If complete, parse the number of
     * bytes the chunk will consist of, and continue with [accept_body_chunked_contents].
     *)
    IFDEF Testing THEN self # case "accept_body_chunked" ELSE () END;
    let l = Netbuffer.length recv_buf in
    let s = Netbuffer.unsafe_buffer recv_buf in
    try
      let p = http_find_line_start s pos (l - pos) (* or Buffer_exceeded *) in
      ( try
	  let hex_digits = parse_chunk_header s pos (p-pos) in
	    (* or Not_found *)
	  let chunk_length =
	    try int_of_string("0x" ^ hex_digits)
	    with Failure _ -> 
	      IFDEF Testing THEN self # case "accept_body_chunked/ch_large" ELSE () END;
	      raise(Bad_request (`Format_error "Chunk too large")) in
	  (* Continue with chunk data or chunk end *)
	  if chunk_length > 0 then (
	    IFDEF Testing THEN self # case "accept_body_chunked/go_on" ELSE () END;
	    `Continue(self # accept_body_chunked_contents req_h resp p chunk_length)
	  )
	  else (
	    IFDEF Testing THEN self # case "accept_body_chunked/end" ELSE () END;
	    `Continue(self # accept_body_chunked_end req_h resp p)
	  )
	with Not_found ->
	  IFDEF Testing THEN self # case "accept_body_chunked/invalid_ch" ELSE () END;
	  raise(Bad_request (`Format_error "Invalid chunk"))
      )
    with
      | Buffer_exceeded -> 
	  IFDEF Testing THEN self # case "accept_body_chunked/exceeded" ELSE () END;
	  if recv_eof then (
	    IFDEF Testing THEN self # case "accept_body_chunked/eof" ELSE () END;
	    raise(Bad_request `Unexpected_eof);
	  );
	  if pos > 0 then
	    Netbuffer.delete recv_buf 0 pos;
	  if Netbuffer.length recv_buf > 500 then (
	    IFDEF Testing THEN self # case "accept_body_chunked/ch_hdr_large" ELSE () END;
	    raise(Bad_request (`Format_error "Chunk header too large"));
	  );
	  IFDEF Testing THEN self # case "accept_body_chunked/restart" ELSE () END;
	  `Restart_with(self # accept_body_chunked req_h resp 0)

  method private accept_body_chunked_contents req_h resp pos remaining_length () : cont =
    (* Read the chunk body at [pos], at most [remaining_length] bytes *)
    IFDEF Testing THEN self # case "accept_body_chunked_contents" ELSE () END;
    let l = Netbuffer.length recv_buf in
    let s = Netbuffer.unsafe_buffer recv_buf in
    if remaining_length > 0 then (
      IFDEF Testing THEN self # case "accept_body_chunked_contents/data" ELSE () END;
      (* There are still data to read *)
      let have_length = l - pos in
      let take_length = min have_length remaining_length in
      let rem_length' = remaining_length - take_length in
      if take_length > 0 then
	self # push_recv
	  (`Req_body(Netbuffer.sub recv_buf pos take_length, 0, take_length), take_length);
      if take_length = remaining_length then
	`Continue
	  (self # accept_body_chunked_contents req_h resp (pos+take_length) 0)
      else (
	IFDEF Testing THEN self # case "accept_body_chunked_contents/exceeded" ELSE () END;
	(* We hit the buffer boundary. Delete the buffer *)
	if recv_eof then (
	  IFDEF Testing THEN self # case "accept_body_chunked_contents/eof" ELSE () END;
	  raise(Bad_request `Unexpected_eof);
	);
	Netbuffer.clear recv_buf;
	IFDEF Testing THEN self # case "accept_body_chunked_contents/restart" ELSE () END;
	`Restart_with 
	  (self # accept_body_chunked_contents req_h resp 0 rem_length')
      )
    ) else (
      IFDEF Testing THEN self # case "accept_body_chunked_contents/end" ELSE () END;
      (* End of chunk reached. There must a (single) line end at the end of the chunk *)
      if (l > pos && s.[pos] = '\010') then (
	IFDEF Testing THEN self # case "accept_body_chunked_contents/lf" ELSE () END;
	`Continue(self # accept_body_chunked req_h resp (pos+1))
      )
      else
	if (l > pos+1 && s.[pos] = '\013' && s.[pos+1] = '\010') then (
	  IFDEF Testing THEN self # case "accept_body_chunked_contents/crlf" ELSE () END;
	  `Continue(self # accept_body_chunked req_h resp (pos+2))
	)
	else
	  if l > pos+1 then (
	    IFDEF Testing THEN self # case "accept_body_chunked_contents/no_eol" ELSE () END;
	    raise (Bad_request (`Format_error "Chunk not followed by line terminator"))
	  )
	  else (
	    IFDEF Testing THEN self # case "accept_body_chunked_contents/e_exceeded" ELSE () END;
	    (* We hit the buffer boundary *)
	    if recv_eof then (
	      IFDEF Testing THEN self # case "accept_body_chunked_contents/e_eof" ELSE () END;
	      raise(Bad_request `Unexpected_eof);
	    );
	    Netbuffer.delete recv_buf 0 pos;
	    IFDEF Testing THEN self # case "accept_body_chunked_contents/e_restart" ELSE () END;

	    `Restart_with (self # accept_body_chunked_contents req_h resp 0 0)
	  )
    )
    
  method private accept_body_chunked_end req_h resp pos () : cont =
    (* Read the trailer *)
    IFDEF Testing THEN self # case "accept_body_chunked_end" ELSE () END;
    let l = Netbuffer.length recv_buf in
    let s = Netbuffer.unsafe_buffer recv_buf in
    let config_max_trailer_length = max 2 (config # config_max_trailer_length) in
    try
      (* Check if there is a trailer *)
      if l > pos && s.[pos] = '\010' then (
	IFDEF Testing THEN self # case "accept_body_chunked_end/lf" ELSE () END;
	self # push_recv (`Req_end, 0);
	pipeline_len <- pipeline_len + 1;
	if resp#state = `Inhibited then
	  resp # set_state `Queued;   (* allow response from now on *)
	`Continue(self # accept_header (pos+1))
      )
      else 
	if l > pos+1 && s.[pos] = '\013' && s.[pos+1] = '\010' then (
	  IFDEF Testing THEN self # case "accept_body_chunked_end/crlf" ELSE () END;
	  self # push_recv (`Req_end, 0);
	  pipeline_len <- pipeline_len + 1;
	  if resp#state = `Inhibited then
	    resp # set_state `Queued;   (* allow response from now on *)
	  `Continue(self # accept_header (pos+2))
	)
	else (
	  IFDEF Testing THEN self # case "accept_body_chunked_end/trailer" ELSE () END;
	  (* Assume there is a trailer. *)
	  let trailer_end = http_find_double_line_start s pos (l-pos) in
              (* or Buf_exceeded *)
	  IFDEF Testing THEN self # case "accept_body_chunked_end/tr_found" ELSE () END;
	  (* Now we are sure there is a trailer! *)
	  if trailer_end - pos > config_max_trailer_length then (
	    IFDEF Testing THEN self # case "accept_body_chunked_end/tr_long" ELSE () END;
	    raise(Bad_request (`Format_error "Trailer too long"));
	  );
	  let ch = new Netchannels.input_string 
		     ~pos:pos ~len:(trailer_end - pos) s in
	  let str = new Netstream.input_stream ch in
	  let req_tr =
	    try Netmime.read_mime_header str 
	    with Failure _ -> 
	      IFDEF Testing THEN self # case "accept_body_chunked_end/bad_tr" ELSE () END;
	      raise(Bad_request `Bad_trailer) in
	  self # push_recv (`Req_trailer req_tr, trailer_end-pos);
	  self # push_recv (`Req_end, 0);
	  pipeline_len <- pipeline_len + 1;
	  if resp#state = `Inhibited then
	    resp # set_state `Queued;   (* allow response from now on *)
	  `Continue(self # accept_header trailer_end)
	)

    with
	Buffer_exceeded ->
	  IFDEF Testing THEN self # case "accept_body_chunked_end/exceeded" ELSE () END;
	  if recv_eof then (
	    IFDEF Testing THEN self # case "accept_body_chunked_end/eof" ELSE () END;
	    raise(Bad_request `Unexpected_eof);
	  );
	  if l-pos > config_max_trailer_length then (
	    IFDEF Testing THEN self # case "accept_body_chunked_end/tr_long" ELSE () END;
	    raise(Bad_request (`Format_error "Trailer too long"));
	  );
	  Netbuffer.delete recv_buf 0 pos;
	  IFDEF Testing THEN self # case "accept_body_chunked_end/restart" ELSE () END;
	  `Restart_with (self # accept_body_chunked_end req_h resp 0)

  (* ---- Process responses ---- *)

  method resp_queue_len = 
    Queue.length resp_queue

  method private transmit_response() =
    (* Try to transmit the response. Do nothing if the socket is not ready for
     * transmission. If a fatal error happens, the connection is aborted.
     *)
    if not (Queue.is_empty resp_queue) then
      let resp = Queue.peek resp_queue in
      try
	resp # set_state `Active;
	match resp # front_token with      (* or Send_queue_empty, Unix_error *)
	  | `Resp_wire_data (s,pos,len) ->
	      (* Try to write: *)
	      dlogr (fun () -> sprintf "FD %Ld: send" fdi);
	      let n = Unix.send fd s pos len [] in  (* or Unix.error *)
	      dlogr (fun () -> sprintf "FD %Ld: sent %d bytes" fdi n);
	      (* Successful. Advance by [n] *)
	      resp # advance n;

	  | `Resp_end ->
	      dlogr
		(fun () -> sprintf "FD %Ld: found Resp_end in the queue" fdi);
	      pipeline_len <- pipeline_len - 1;
	      resp # set_state `Processed;
	      (* Check if we have to close the connection: *)
	      if resp # close_connection then
		self # shutdown_sending();
	      (* Continue with the next response, if any, and if possible: *)
	      let next_resp = Queue.take resp_queue in
	      next_resp # set_state `Active;   (* ... unless dropped *)
	      (* If the queue is still non-empty, and if the connection is closed,
	       * the remaining, already computed responses, cannot be sent at all.
	       * Drop the responses in this case.
	       *)
	      if resp # close_connection then
		self # drop_remaining_responses()

      with
	| Send_queue_empty ->
	    ()    (* nothing to do *)
	| Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK),_,_) ->
	    ()    (* socket not ready *)
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    ()    (* Signal happened, try later again *)
	| Unix.Unix_error((Unix.EPIPE | Unix.ECONNRESET), _,_) ->
	    resp # set_state `Error;
	    ignore(Queue.take resp_queue);
	    self # abort `Broken_pipe
	| Unix.Unix_error(e, _, _) ->
	    resp # set_state `Error;
	    ignore(Queue.take resp_queue);
	    self # abort (`Unix_error e)

  method private drop_remaining_responses() =
    (* Set the state to [`Dropped] for all responses in the [resp_queue]: *)
    Queue.iter
      (fun resp -> resp # set_state `Dropped)
      resp_queue;
    Queue.clear resp_queue

  (* ---- Queue management ---- *)

  method receive () =
    try
      let (tok, size) = Queue.take recv_queue in
      recv_queue_byte_size <- recv_queue_byte_size - size;
      tok
    with
	Queue.Empty -> raise Recv_queue_empty

  method peek_recv () =
    try
      fst(Queue.peek recv_queue)
    with
	Queue.Empty -> raise Recv_queue_empty

  method private push_recv ( (token,size) as qelem ) =
    Queue.push qelem recv_queue;
    recv_queue_byte_size <- recv_queue_byte_size + size

  method recv_queue_len =
    Queue.length recv_queue

  method recv_queue_byte_size =
    recv_queue_byte_size 


  (* ---- Socket stuff ---- *)

  val mutable fd_down = false

  method shutdown () =
    (* The shutdown issue is discussed here:
     * http://ftp.ics.uci.edu/pub/ietf/http/draft-ietf-http-connection-00.txt
     *
     * Recommendation: 
     * - Only shutdown for sending
     * - Keep receiving data for a while ("lingering"). In principle, until the client has 
     *   seen the half shutdown, but we do not know when. Apache lingers for 30 seconds.
     *)
    dlogr (fun () -> sprintf "FD %Ld: shutdown" fdi);
    self # stop_input_acceptor();
    self # drop_remaining_responses();
    self # shutdown_sending()

  method private shutdown_sending() =
    dlogr (fun () -> sprintf "FD %Ld: shutdown_sending" fdi);
    if not fd_down then (
      try
	Unix.shutdown fd Unix.SHUTDOWN_SEND;
      with
	  Unix.Unix_error(Unix.ENOTCONN,_,_) -> 
	    need_linger <- false   (* the peer has already shut down in the meantime *)

    );
    fd_down <- true

  method waiting_for_next_message =
    waiting_for_next_message

  method input_timeout_class : [ `Normal | `Next_message | `None ] =
    (* Do we have an active response object? In this case, it might be the case the
     * connection is output-driven, and no timeout applies:
     *)
    try
      let first = Queue.peek resp_queue in  (* or Queue.Empty *)
      if first#state <> `Active then raise Queue.Empty;
      (* If the response object is in the bidirectional phase, the normal input
       * timeout applies nevertheless.
       *)
      if first#bidirectional_phase then
	`Normal
      else
	`None
    with
	Queue.Empty ->
	  if waiting_for_next_message then
	    `Next_message
	  else
	    `Normal
	  
  method timeout () =
    if waiting_for_next_message then (
      (* Indicate a "soft" timeout. Processing is nevertheless similar to [abort]: *)
      dlogr (fun () -> sprintf "FD %Ld: soft timeout" fdi);
      need_linger <- false;
      self # shutdown();
      self # push_recv (`Timeout, 0);
      self # push_recv (`Eof, 0);
    )
    else 
      self # abort `Timeout   (* "hard" timeout *)
      

  method abort (err : fatal_error) = 
    dlogr (fun () -> 
	     sprintf "FD %Ld: abort %s" fdi (string_of_fatal_error err));
    need_linger <- false;
    self # shutdown();
    let err' =
      if err=`Broken_pipe && config#config_suppress_broken_pipe then
	`Broken_pipe_ignore
      else
	err in
    self # push_recv (`Fatal_error err', 0);
    self # push_recv (`Eof, 0)

  method fd = fd
  method config = (config :> http_protocol_config)

  method pipeline_len = pipeline_len


  method do_input =
    not recv_eof && 
    (pipeline_len <= config#config_limit_pipeline_length) &&
    (recv_queue_byte_size <= config#config_limit_pipeline_size)

  method do_output =
    not (Queue.is_empty resp_queue) &&
    not ((Queue.peek resp_queue) # send_queue_empty)

  method need_linger = 
    need_linger

end


class lingering_close ?(preclose = fun () -> ()) fd =
  let fd_style = Netsys.get_fd_style fd in
object(self)
  val start_time = Unix.gettimeofday()
  val timeout = 60.0
  val junk_buffer = String.create 256
  val mutable lingering = true

  method cycle ?(block=false) () =
    try
      if block then (
	let now = Unix.gettimeofday() in
	let sel_time = max (timeout -. (now -. start_time)) 0.0 in
	if sel_time = 0.0 then raise Not_found;
	ignore(Netsys.wait_until_readable fd_style fd sel_time);
	()
      );

      let n = Unix.recv fd junk_buffer 0 (String.length junk_buffer) [] in
      if n = 0 then (
	lingering <- false;
	preclose();
	Unix.close fd
      )
    with
      | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK), _,_)->
	  ()             (* socket not ready *)
      | Unix.Unix_error(Unix.EINTR,_,_) ->
	  ()             (* got signal *)
      | Unix.Unix_error(_, _,_) 
      | Not_found ->  (* Any other error means we are done! *)
	  lingering <- false;
	  preclose();
	  Unix.close fd

  method lingering = lingering

  method fd = fd
end


let default_http_protocol_config =
  ( object
      method config_max_reqline_length = 32768
      method config_max_header_length = 65536
      method config_max_trailer_length = 32768
      method config_limit_pipeline_length = 5
      method config_limit_pipeline_size = 65536
      method config_announce_server = `Ocamlnet
      method config_suppress_broken_pipe = false
    end
  )

let override v opt =
  match opt with
    | None -> v
    | Some x -> x

class modify_http_protocol_config 
        ?config_max_reqline_length
        ?config_max_header_length
        ?config_max_trailer_length
        ?config_limit_pipeline_length
        ?config_limit_pipeline_size
        ?config_announce_server
        ?config_suppress_broken_pipe 
        (config : http_protocol_config) : http_protocol_config =
  let config_max_reqline_length = 
    override config#config_max_reqline_length config_max_reqline_length in
  let config_max_header_length =
    override config#config_max_header_length config_max_header_length in
  let config_max_trailer_length =
    override config#config_max_trailer_length config_max_trailer_length in
  let config_limit_pipeline_length =
    override config#config_limit_pipeline_length config_limit_pipeline_length in
  let config_limit_pipeline_size =
    override config#config_limit_pipeline_size config_limit_pipeline_size in
  let config_announce_server =
    override config#config_announce_server config_announce_server in
  let config_suppress_broken_pipe =
    override config#config_suppress_broken_pipe config_suppress_broken_pipe in
object
  method config_max_reqline_length = config_max_reqline_length
  method config_max_header_length = config_max_header_length
  method config_max_trailer_length = config_max_trailer_length
  method config_limit_pipeline_length = config_limit_pipeline_length
  method config_limit_pipeline_size = config_limit_pipeline_size
  method config_announce_server = config_announce_server
  method config_suppress_broken_pipe = config_suppress_broken_pipe
end
