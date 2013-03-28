(* $Id: nethttpd_kernel.mli 1410 2010-02-14 19:44:28Z gerd $
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

(** {1 The protocol kernel of the HTTP daemon}
  *
  * This module mainly defines the [http_protocol] class which implements the
  * exchange of messages with a HTTP client. The request messages are represented
  * as sequence of [req_token] values. The response is encapsulated in a separate
  * [http_response] class. The contents of the response are represented as sequence
  * of [resp_token] values.
 *)

(* HTTP protocol kernel for daemon *)

open Nethttp
open Nethttpd_types

type fatal_error =
    [ `Broken_pipe
    | `Broken_pipe_ignore
    | `Message_too_long
    | `Timeout
    | `Unix_error of Unix.error
    | `Server_error
    ]
    (** These are the serious protocol violations after that the daemon stops
      * any further processing.
      *
      * Note that [`Timeout] refers to a timeout in the middle of a request.
      * [`Broken_pipe_ignore] is the "harmless" version of [`Broken_pipe]
      * (see [config_suppress_broken_pipe]).
      *
      * Long messages are fatal because it is suspected that they are denial
      * of service attacks. The kernel generates [`Message_too_long] only for
      * long headers, not for long bodies.
      *
      * Fatal server errors can happen when exceptions are not properly handled.
      * As last resort the HTTP daemon closes the connection without notifying
      * the client.
     *)

val string_of_fatal_error : fatal_error -> string
  (** Convert error to a string, for logging *)

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
    (** A bad request is a violation where the current request cannot be
      * decoded, and it is not possible to accept further requests over the
      * current connection.
     *)

val string_of_bad_request_error : bad_request_error -> string
  (** Convert error to a string, for logging *)

val status_of_bad_request_error : bad_request_error -> http_status
  (** Returns the best response code for the error *)


type data_chunk = string * int * int
    (** A [data_chunk] is a substring of a string. The substring is described by
      * the triple [(s, pos, len)] where [s] is the container, [pos] is the
      * position where the substring begins, and [len] its length.
     *)

type status_line = int * string
    (** = (code, phrase) *)

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
    (** The [resp_token] represents a textual part of the response to send:
      * - [`Resp_info_line] is an informational status line (code=100..199). There can
      *   be several informational lines, and they can be accompanied with their own
      *   headers. Such lines are only sent to HTTP/1.1 clients.
      * - [`Resp_status_line] is the final status line to send (code >= 200)
      * - [`Resp_header] is the whole response header to send
      * - [`Resp_body] is the next part of the response body to send.
      * - [`Resp_trailer] is the whole response trailer to send (currently ignored)
      * - [`Resp_action] is special because it does not directly represent a token
      *   to send. The argument is a function which is called when the token is
      *   the next token on the active event queue. The function is also called when
      *   the event queue is dropped because of an error (the state of the
      *   response object indicates this). The function must not raise exceptions
      *   except [Unix_error], and it must not block.
     *)

val resp_100_continue : resp_token
  (** The predefined token for the "100 Continue" response *)

type resp_state =
    [ `Inhibited | `Queued | `Active | `Processed | `Error | `Dropped ]
    (** The response state:
      * - [`Inhibited] = it is not yet allowed to start the response
      * - [`Queued] = the response waits on the queue for activation
      * - [`Active] = the response is currently being transmitted
      * - [`Processed] = the response has been completely sent
      * - [`Error] = an error occurred during the transmission of this response
      * - [`Dropped] = an earlier response forced to close the connection, and
      *   this response is dequeued
     *)

type front_token = 
   [ `Resp_wire_data of data_chunk
   | `Resp_end 
   ]
   (** Tokens generated by [http_response]:
     * - [`Resp_wire_data] are data tokens.
     * - [`Resp_end] indicates the end of the response.
    *)

exception Send_queue_empty


type announcement =
    [`Ignore | `Ocamlnet | `Ocamlnet_and of string | `As of string ]
  (** See config *)


(** Encapsultation of the HTTP response for a single request *)
class type http_response  =
object
  (** Represents the action of sending the response
    *
    * This class has an internal
    * queue of response tokens that are not yet processed. One can easily add
    * new tokens to the end of the queue ([send]). 
    *
    * The class is responsible for determining the transfer encoding:
    * - When the HTTP version is 1.0, the encoding is always "identity", and the
    *   connection will be closed after the response.
    * - When the HTTP version is 1.1, and there is a Content-length header,
    *   the encoding will be selected as "identity". It is checked whether the
    *   body has really this length. If too short, it is suggested to close
    *   the connection. If too long, the extra part of the message is silently
    *   dropped.
    * - When the HTTP version is 1.1, and there is no Content-length header,
    *   the encoding will be selected as "chunked".
    *
    * Currently, the [TE] request header is not taken into account. The trailer
    * is always empty.
    *
    * The following headers are set (or removed) by this class:
    * - [Transfer-Encoding]
    * - [Trailer]
    * - [Date]
    * - [Connection]
    * - [Upgrade]
    * - [Server] (it is appended to this field)
    *
    * Responses for HEAD requests have the special behaviour that the body is silently
    * dropped. The calculation of header fields is not affected by this. This means
    * that HEAD can be easily implemented by doing the same as for GET.
    *
    * Responses for other requests that must not include a body must set
    * [Content-Length] to 0.
   *)

  (** These methods can be called by the content provider: *)

  method state : resp_state
    (** Reports the state. The initial state is [`Inhibited] *)

  method bidirectional_phase : bool
    (** The bidrectional phase starts after "100 Continue" has been sent to the
      * client, and stops when the response body begins. The bidirectional phase
      * is special for the calculation of timeout values (input determines the timeout
      * although the response has started).
     *)

  method send : resp_token -> unit
    (** Add token to the end of the send queue *)

  method send_queue_empty : bool
    (** Return whether the send queue is empty. When the state is [`Inhibited], this
     * method fakes an empty queue.
     *)

  method protocol : protocol
    (** The HTTP version of the response. This is currently always HTTP/1.1, but maybe
      * we need to fake lower versions for buggy clients. Let's see what comes.
     *)

  method close_connection : bool
    (** Returns whether the connection should be closed after this response.
      * This flag should be evaluated when the [`Resp_end] front token has been
      * reached.
     *)

  method transfer_encoding : transfer_coding
    (** Returns the selected transfer encoding. This is valid after the header
      * has been passed to this object with [send].
     *)

  method front_token : front_token
    (** The first token of the queue, represented as [data_chunk]. Raises 
      * [Send_queue_empty] when there is currently no front token, or the state
      * is [`Inhibited].
      * If there is a front token, it will never have length 0.
      *
      * Note that [Unix_error] exceptions can be raised when [`Resp_action]
      * tokens are processed.
     *)

  method set_callback : (unit -> unit) -> unit
    (** The function will be called when either [set_state] changes the state,
      * or when the send queue becomes empty. Note that the callback must never
      * fail, it is called in situations that make it hard to recover from errors.
     *)

  method body_size : int64
    (** Accumulated size of the response body *)

  (** These methods must only be called by the HTTP protocol processor: *)

  method set_state : resp_state -> unit
    (** Sets the state *)

  method advance : int -> unit
    (** Tell this object that [n] bytes of the front token could be really
      * sent using [Unix.write]. If this means that the whole front token
      * has been sent, the next token is pulled from the queue and is made
      * the new front token. Otherwise, the data chunk representing the
      * front token is modified such that the position is advanced by
      * [n], and the length is reduced by [n].
     *)

end

class http_response_impl : ?close:bool -> ?suppress_body:bool -> int64 -> protocol -> announcement -> http_response
  (** Exported for debugging and testing only *)

val send_static_response : 
      http_response -> 
      http_status -> http_header option -> string -> unit
  (** Sends the string argument as response body, together with the given status and
    * the header (optional). Response header fields are set as follows:
    * - The [Content-Length] is set to the length of the string.
    * - The [Content-Type] is set to "text/html" unless given by the header.
    * If the header object is passed in, these modifications are done 
    * directly in this object as side effect.
   *)

val send_file_response : http_response -> 
                         http_status -> http_header option ->
                            Unix.file_descr -> int64 -> unit
  (** Sends the contents of a file as response body, together with the given status and
    * the header (optional). The descriptor must be a file descriptor (that cannot
    * block). The int64 number is the length
    * of the body.  Response header fields are set as follows:
    * - The [Content-Length] is set to the length of the string.
    * - The [Content-Type] is set to "text/html" unless given by the header.
    *
    * Note that [Content-Range] is not set automatically, even if the file is only
    * partially transferred.
    *
    * If the header object is passed in, these modifications are done 
    * directly in this object as side effect.
    *
    * The function does not send the file immediately, but rather sets the [http_response]
    * object up that the next chunk of the file is added when the send queue becomes
    * empty. This file will be closed when the transfer is done.
   *)

type request_line = http_method * protocol
  (** The method (including the URI), and the HTTP version *)

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
    (** A [req_token] represents a textual part of the received request:
      * - [`Req_header] is the full received header. Together with the header,
      *   the corresponding [http_response] object is returned which must
      *   be used to transmit the response.
      * - [`Req_expect_100_continue] is generated when the client expects that the
      *   server sends a "100 Continue" response (or a final status code) now.
      *   One should add [`Resp_info_line resp_100_continue] to the send queue
      *   if the header is acceptable, or otherwise generate an error response. In any
      *   case, the rest of the request must be read until [`Req_end].
      * - [`Req_body] is a part of the request body. The transfer-coding, if any,
      *   is already decoded.
      * - [`Req_trailer] is the received trailer
      * - [`Req_end] indicates the end of the request (the next request may begin
      *   immediately).
      * - [`Eof] indicates the end of the stream
      * - [`Bad_request_error] indicates that the request violated the HTTP protocol
      *   in a serious way and cannot be decoded. It is required to send a 
      *   "400 Bad Request" response. The following token will be [`Eof].
      * - [`Fatal_error] indicates that the connection crashed. 
      *   The following token will be [`Eof].
      * - [`Timeout] means that nothing has been received for a certain amount
      *   of time, and the protocol is in a state that the next request can begin.
      *   The following token will be [`Eof].
      *
      * Note that it is always allowed to [send] tokens to the client. The protocol
      * implementation takes care that the response is transmitted at the right point
      * in time.
     *)

val string_of_req_token : req_token -> string
  (** For debugging *)

exception Recv_queue_empty

(** Configuration values for the HTTP kernel *)
class type http_protocol_config =
object
  method config_max_reqline_length : int
    (** Maximum size of the request line. Longer lines are immediately replied with
      * a "Request URI too long" response. Suggestion: 32768.
     *)

  method config_max_header_length : int
    (** Maximum size of the header, {b including} the request line. Longer headers
      * are treated as attack, and cause the fatal error [`Message_too_long].
      * Suggestion: 65536.
     *)

  method config_max_trailer_length : int
    (** Maximum size of the trailer *)

  method config_limit_pipeline_length : int
    (** Limits the length of the pipeline (= unreplied requests). A value of 0
      * disables pipelining. A value of n allows that another request is
      * received although there are already n unreplied requests.
     *)

  method config_limit_pipeline_size : int
    (** Limits the size of the pipeline in bytes. If the buffered bytes in the
      * input queue exceed this value, the receiver temporarily stops reading
      * more data. The value 0 has the effect that even the read-ahead of
      * data of the current request is disabled. The value (-1) disables the
      * receiver completely (not recommended).
     *)

  method config_announce_server : announcement
    (** Whether to set the [Server] header:
     * - [`Ignore]: The kernel does not touch the [Server] header.
     * - [`Ocamlnet]: Announce this web server as "Ocamlnet/<version>"
     * - [`Ocamlnet_and s]: Announce this web server as [s] and append
     *   the Ocamlnet string.
     * - [`As s]: Announce this web server as [s]
     *)

  method config_suppress_broken_pipe : bool
    (** Whether to suppress [`Broken_pipe] errors. Instead 
      * [`Broken_pipe_ignore] is reported.
     *)

end


val default_http_protocol_config : http_protocol_config 
  (** Default config:
      - [config_max_reqline_length = 32768]
      - [config_max_header_length = 65536]
      - [config_max_trailer_length = 32768]
      - [config_limit_pipeline_length = 5]
      - [config_limit_pipeline_size = 65536]
      - [config_announce_server = `Ocamlnet]
      - [config_suppress_broken_pipe = false]
   *)

class modify_http_protocol_config : 
        ?config_max_reqline_length:int ->
        ?config_max_header_length:int ->
        ?config_max_trailer_length:int ->
        ?config_limit_pipeline_length:int ->
        ?config_limit_pipeline_size:int ->
        ?config_announce_server:announcement ->
        ?config_suppress_broken_pipe:bool ->
        http_protocol_config -> http_protocol_config 
  (** Modifies the passed config object as specified by the optional
      arguments
   *)

(** The core event loop of the HTTP daemon *)
class http_protocol : #http_protocol_config -> Unix.file_descr ->
object
  (** Exchange of HTTP messages
    *
    * In [fd] one must pass the already connected socket. It must be in non-
    * blocking mode.
    *
    * How to use this class: Basically, one invokes [cycle] until the whole
    * message exchange on [fd] is processed. [cycle] receives data from the
    * socket and sends data to the socket. There are two internal queues:
    *
    * The receive queue stores parts of received requests as [req_token].
    * One can take values from the front of this queue by calling [receive].
    *
    * The response queue stores [http_response] objects. Each of the objects
    * corresponds to a request that was received before. This queue is handled
    * fully automatically, but one can watch its length to see whether all responses
    * are actually transmitted over the wire.
    *
    * The basic algorithm to process messages is:
    *
    * {[
    * let rec next_token () =
    *   if proto # recv_queue_len = 0 then (
    *     proto # cycle ();
    *     next_token()
    *   )
    *   else
    *     proto # receive()
    *
    * let cur_token = ref (next_token()) in
    * while !cur_token <> `Eof do
    *   (* Process first token of next request: *)
    *   match !cur_token with
    *    | `Req_header(req_line, header, resp) ->
    *         (* Depending on [req_line], read further tokens until [`Req_end] *)
    *         ...
    *         (* Switch to the first token of the next message: *)
    *         cur_token := next_token()
    *    | `Timeout -> ...
    *    | `Bad_request_error(e,resp) -> 
    *          (* Generate 400 error, send it to [resp] *)
    *          ...
    *          (* Switch to the first token of the next message: *)
    *          cur_token := next_token()
    *    | `Fatal_error e -> failwith "Crash"
    *    | _ -> assert false
    * done;
    * while proto # resp_queue_len > 0 do
    *   proto # cycle ();
    * done;
    * proto # shutdown()
    * ]}
    *
    * See the file [tests/easy_daemon.ml] for a complete implementation of this.
    *
    * As one can see, it is essential to watch the lengths of the queues in order
    * to figure out what has happened during [cycle].
    *
    * When the body of the request is empty, [`Req_body] tokens are omitted.
    * Note that for requests like [GET] that always have an empty body, it is
    * still possible that an errorneous client sends a body, and that [`Req_body]
    * tokens arrive. One must accept and ignore these tokens.
    *
    * Error handling: For serious errors, the connection is immediately aborted.
    * In this case, [receive] returns a [`Fatal_error] token. Note that the
    * queued responses cannot be sent! An example of this is [`Broken_pipe].
    *
    * There is a large class of non-serious errors, esp. format errors
    * in the header and body. It is typical of these errors that one cannot determine
    * the end of the request properly. For this reason, the daemon stops reading
    * further data from the request, but the response queue is still delivered.
    * For these errors, [receive] returns a [`Bad_request_error] token.
    * This token contains a [http_response] object that must be filled with a
    * 400 error response.
   *)

  method cycle : ?block:float -> unit -> unit
    (** Looks at the file descriptor. If there is data to read from the descriptor,
      * and there is free space in the input buffer, additional data is read into
      * the buffer. It is also tried to interpret the new data as [req_token]s,
      * and if possible, new [req_token]s are appended to the receive queue.
      *
      * If the response queue has objects, and there is really data one can send,
      * and if the socket allows one to send data, it is tried to send as much
      * data as possible.
      *
      * The option [block] (default: 0) can be set to wait until data
      * can be exchanged with the socket. This avoids busy waiting. The number
      * is the duration in seconds to wait until the connection times out
      * (0 means not to wait at all, -1 means to wait infinitely). When a timeout
      * happens, and there is nothing to send, and the last request was fully
      * processed, [receive] will simply return [`Timeout] (i.e. when 
      * [waiting_for_next_message] is [true]). Otherwise, the
      * fatal error [`Timeout] is generated.
     *)

  method receive : unit -> req_token
    (** Returns the first [req_token] from the receive queue. Raises
      * [Recv_queue_empty] when the queue is empty (= has no new data)
     *)

  method peek_recv : unit -> req_token
    (** Peeks the first token, but leaves it in the queue.
      * Raises [Recv_queue_empty] when the queue is empty.
     *)

  method recv_queue_len : int
    (** Returns the length of the receive queue (number of tokens) *)

  method resp_queue_len : int
    (** Returns the length of the internal response queue (number of [http_response]
      * objects that have not yet fully processed)
     *)

  method pipeline_len : int
    (** Returns the number of unanswered requests = Number of received [`Req_end] tokens
      * minus number of responses in state [`Processed]. Note that [pipeline_len]
      * can become [-1] when bad requests are responded.
     *)

  method recv_queue_byte_size : int
    (** Returns the (estimated) size of the input queue in bytes *)

  method waiting_for_next_message : bool
    (** Whether the kernel is currently waiting for the beginning of a new
      * arriving HTTP request. This is [false] while the request is being
      * received.
     *)

  method input_timeout_class : [ `Normal | `Next_message | `None ]
    (** Suggests the calculation of a timeout value for input:
      * - [`Normal]: The normal timeout value applies
      * - [`Next_message]: The timeout value applies while waiting for the next message
      * - [`None]: The connection is output-driven, no input timeout value
     *)

  method shutdown : unit -> unit
    (** Shuts the socket down. Note: the descriptor is not closed.
     *)

  method timeout : unit -> unit
    (** Process a timeout condition as [cycle] does *)

  method abort : fatal_error -> unit
    (** Stops the transmission of data. The receive queue is cleared and filled
      * with the two tokens [`Fatal_error] and [`Eof]. 
      * The response queue is cleared. The [cycle]
      * method will return immediately without doing anything.
     *)

  method fd : Unix.file_descr
    (** Just returns the socket *)

  method do_input : bool
    (** Returns [true] iff the protocol engine is interested in new data from the
      * socket. Returns [false] after EOF and after errors.
     *)

  method do_output : bool
    (** Returns [true] iff the protocol engine has data to output to the socket *)

  method need_linger : bool
    (** Returns [true] when a lingering close operation is needed to reliably shut
      * down the socket. In many cases, this expensive operation is not necessary.
      * See the class [lingering_close] below.
     *)

  method config : http_protocol_config
    (** Just returns the configuration *)

  method test_coverage : string list
    (** For testing: returns a list of tokens indicating into which cases the program
      * ran.
     *)

end


(** Closes a file descriptor using the "lingering close" algorithm.
    The optional [preclose] function is called just before [Unix.close].
 *)
class lingering_close : ?preclose:(unit->unit) -> Unix.file_descr ->
object
  (** Closes a file descriptor using the "lingering close" algorithm
    * 
    * Usage:
    * {[ while lc # lingering do lc # cycle ~block:true () done ]}
   *)

  method cycle : ?block:bool -> unit -> unit
    (** Reads data from the file descriptor until EOF or until a fixed timeout
      * is over. Finally, the descriptor is closed. If [block] is set, the method
      * blocks until data is available. (Default: [false])
     *)

  method lingering : bool
    (** Whether the socket is still lingering *)

  method fd : Unix.file_descr
    (** The file descriptor *)
end


(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module
     *)
end
