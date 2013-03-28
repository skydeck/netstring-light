(* $Id: nethttpd_engine.mli 1411 2010-02-14 19:49:46Z gerd $
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

(** {1 The event-based encapsulation of the HTTP daemon}
  * 
  * This is a user-friendlier encapsulation of the HTTP daemon. It uses
  * the engine module defined in [Uq_engines].
 *)

(* Integration into event-based server design *)

open Nethttpd_types

type engine_req_state =
    [ `Received_header
    | `Receiving_body
    | `Received_request 
    | `Finishing
    ]

class type http_engine_config =
object
  inherit Nethttpd_reactor.http_processor_config

  method config_input_flow_control : bool
    (** If [true], the engine stops reading input tokens from the HTTP kernel when
      * there is data in the input channel of the engine not yet read. If [false],
      * all available input tokens are fetched from the kernel and buffered up
      * in the input channel.
      *
      * In general, this {b should} be set to [true]. However, this is only possible
      * when the user of the engine is prepared for flow control. In particular,
      * all data contained in the input channel must be immediately read, or else
      * the engine blocks. By calling [input_ch_async # request_notification], the
      * user can be notified when there is data to read.
      *
      * When set to [false], the engine never blocks, but the price is that the
      * input channel may become as large as needed to store the whole request.
      *
      * The option [config_limit_pipeline_size] does not have any effect for engines.
     *)

  method config_output_flow_control : bool
    (** If [true], the engine signals the user when there is already enough data
      * to output, and no more output should be generated. The user can query
      * this state by calling [output_ch_async # can_output], and react
      * accordingly. The user can also ignore this signal, and the output channel
      * buffers all data.
      *
      * If [false], the mentioned method [can_output] returns always [true]. This
      * turns off flow control in the case it is implemented by the user of the
      * engine, but actually not wanted.
      *
      * The internal processing of data is not affected by this configuration option.
      * In doubt, set it to [true].
     *)

end


val default_http_engine_config : http_engine_config
  (** The default config:
       - [config_input_flow_control=false]
       - [config_output_flow_control=true]
   *)

class modify_http_engine_config :
        ?modify_http_protocol_config:
           (Nethttpd_kernel.http_protocol_config -> 
              Nethttpd_kernel.http_protocol_config) ->
        ?modify_http_processor_config:
           (Nethttpd_reactor.http_processor_config -> 
              Nethttpd_reactor.http_processor_config) ->
        ?config_input_flow_control:bool ->
        ?config_output_flow_control:bool ->
         http_engine_config ->  http_engine_config
  (** Modifies the passed config object as specified by the optional
      arguments.

      [modify_http_protocol_config] and [modify_http_processor_config]:
      These functions can be used to modify the
      parts of the config object that are inherited from [http_protocol_config]
      and [http_processor_config], respectively:
      For example:

      {[
        let new_cfg =
          new modify_http_engine_config
            ~modify_http_protocol_config:
               (new Nethttpd_kernel.modify_http_protocol_config
                  ~config_suppress_broken_pipe:true)
            ~modify_http_processor_config:
               (new Nethttpd_reactor.modify_http_processor_config
                  ~config_timeout:15.0)
            old_cfg
      ]}
   *)


class type extended_async_environment =
object
  inherit extended_environment

  (** Environment also providing asynchronous views to I/O *)

  method input_ch_async : Uq_engines.async_in_channel
    (** This is the [input_ch] channel taken as asynchonous channel. This type of
      * channel indicates when data is available to read, and also sends notifications.
      * Note that this is only an alternate interface of the [input_ch] object.
      *
      * The method [can_input] returns true when there is at least one byte of
      * the body to read, or the EOF has been seen. The channel buffers any arriving
      * data (which can be limited in amount by [config_pipeline_size]).
      *
      * The behaviour of this channel is influenced by the configuration option
      * [config_input_flow_control].
     *)

  method output_ch_async : Uq_engines.async_out_channel
    (** This is the [output_ch] channel taken as asynchronous channel. This type of
      * channel indicates when space is available for further output, and also sends
      * notifications. 
      * Note that this is only an alternate interface of the [output_ch] object.
      *
      * The method [can_output] returns [true] only when the internal buffer is empty,
      * i.e. all data have been transmitted to the client. Independent of this, the
      * channel buffers all data written to it.
      *
      * The behaviour of this channel is influenced by the configuration option
      * [config_output_flow_control].
     *)
end


class type http_request_header_notification =
object
  (** Notification that a new request header has arrived
    *
    * This object notifies the user that a new request header has arrived.
    * The header is accessible by the [environment] object. The channels
    * also contained in this object are locked at this moment. The user must
    * now either call [schedule_accept_body] or [schedule_reject_body]. The
    * user will get a second notification (a [http_request_notification], below)
    * when the request body has completely arrived (in case of acceptance), or
    * immediately (in case of rejection). One can also call [schedule_finish]
    * at any time to drop the current request.
   *)
  
  method req_state : engine_req_state
    (** Returns the request processing state which is [`Received_header] at the
      * moment when this notification is delivered.
     *)

  method environment : extended_async_environment
    (** The request environment. Depending on the request processing state, parts
      * of the environment are already set up or still unaccessible ("locked").
      * In the state [`Received_header] only the request header and the 
      * derived fields are accessible, and the input and output channels are
      * locked. In the state [`Receiving_body] the input channel is unlocked,
      * but it is not yet filled (reading from it may cause the exception 
      * [Buffer_underrun]). The output channel remains locked.
      * In the state [`Received_request], the input channel is unlocked and filled
      * with data, and the output channel is unlocked, too.
      *
      * This environment is not fully CGI-compatible. In particular, the following
      * differences exist:
      * - There is no [cgi_path_info] and no [cgi_path_translated].
      * - The user is always unauthenticated.
      * - The [Status] response header works as in CGI. The [Location] header, however,
      *   must be a full URL when set (only browser redirects)
      * - When the request body is transmitted by chunked encoding, the header
      *   [Content-Length] is not set. In CGI this is interpreted as missing body.
      *   It is unlikely that clients send requests with chunked encoding, as this
      *   may cause interoperability problems anyway.
      *   
     *)

  method schedule_accept_body : on_request:(http_request_notification -> unit) ->
                               ?on_error:(unit -> unit) -> 
                               unit -> unit
    (** Schedules that the request body is accepted. In terms of HTTP, this sends the
      * "100 Continue" response when necessary. One can reply with a positive or
      * negative message.
      *
      * This method returns immediately, and sets callbacks for certain events.
      * When the body has completely arrived (or is empty), the function 
      * [on_request] is called back. The argument is the full request notification
      * (see below).
      *
      * When the request is dropped for some reason, [on_error] is called back instead.
      * This can be used to free resources, for example.
      *
      * Neither of the callbacks must raise exceptions.
     *)

  method schedule_reject_body : on_request:(http_request_notification -> unit) ->
                               ?on_error:(unit -> unit) -> 
                               unit -> unit
    (** Schedules that the request body is rejected. In terms of HTTP, this prevents
      * sending the "100 Continue" response. Any arriving request body is silently
      * discarded. One should immediately reply with an error mesage.
      * negative message.
      *
      * This method returns immediately, and sets callbacks for certain events.
      * When the body has completely arrived (or is empty), the function 
      * [on_request] is called back. The argument is the full request notification
      * (see below).
      *
      * When the request is dropped for some reason, [on_error] is called back instead.
      * This can be used to free resources, for example.
      *
      * Neither of the callbacks must raise exceptions.
     *)

  method schedule_finish : unit -> unit
    (** Schedules that the request is finished. This method should be called after
      * the regular processing of the request to ensure that the HTTP protocol
      * is fulfilled. If the request body has not been
      * fully read, this is now done, and its data are dropped. If the response
      * is incomplete, it is completed. If the error is not recoverable, a "Server
      * Error" is generated.
     *)

end


and http_request_notification =
object
  (** Notification that the whole request has arrived
    *
    * This object notifies the user that the request has fully arrived (including
    * the body if accepted), and can now be responded. The [environment] is the
    * same as in the request header notification, but the channels are now
    * unlocked.
   *)

  method req_state : engine_req_state
    (** Returns the request processing state which is [`Received_request] at the
      * moment when this notification is delivered.
     *)

  method environment : extended_async_environment
    (** The request environment. See above. *)

  method schedule_finish : unit -> unit
    (** Schedules that the request is finished. See above. *)

end


class http_engine : on_request_header:(http_request_header_notification -> unit) ->
                    unit -> 
                    #http_engine_config -> Unix.file_descr -> 
                    Unixqueue.unix_event_system ->
                      [unit] Uq_engines.engine
  (** This engine processes the requests arriving on the file descriptor using
    * the Unix event system. Whenever a new request header arrives, the function
    * [on_request_header] is called back, and must handle the request.
    *
    * Unless aborted using the [abort] method, this engine is always successful.
    * Errors are logged, but not returned as result.
    *
    * The file descriptor is closed after processing all HTTP requests and
    * responses. It is also closed on error and when the engine is aborted.
    *
    * An aborted engine does not try to clean up resources external to the 
    * engine, e.g. by calling the [on_error] functions. This is up to the user.
   *)


class type http_engine_processing_config =
object
  method config_synch_input : 
           (Netchannels.in_obj_channel -> unit) ->
           Uq_engines.async_in_channel ->
           unit
    (** The "input synchronizer": It is called as [obj # config_synch_input f ch]
      * to create a synchronous input channel from an asynchronous one, [ch].
      * The function [f] must be called back by the synchronizer when synchronisation
      * is established, and with the synchronous channel [ch'] as argument.
      * In particular, the task of the synchronizer is to turn blocking reads of
      * [ch'] into non-blocking reads of [ch]. In general there are two ways of
      * implementation:
      * - Buffer all input from [ch] until the end of the channel is reached,
      *   then call [f] with a wrapper channel [ch'] that just reads from the
      *   buffer.
      * - Run [f] in a different thread that blocks whenever there is nothing to
      *   read from [ch]. 
      *
      * Both implementations are allowed, i.e. {b it is allowed that [f] runs in
      * a different thread}.
      *
      * CHECK: How to handle exceptions raised from [f]? Idea: [f] is obliged to
      * close [ch'] in this case, even if [ch] is not yet at the end. The rest of
      * exception handling is up to the user. - The complementary must also be true:
      * When there is an error in the engine, [ch] must be closed to signal the
      * other thread that we have a problem.
     *)

  method config_synch_output : 
           (Netchannels.out_obj_channel -> unit) ->
           Uq_engines.async_out_channel ->
           unit
    (** The "output synchronizer": It is called as [obj # config_synch_output f ch]
      * to create a synchronous output channel from an asynchronous one, [ch].
      * The function [f] must be called back by the synchronizer when synchronisation
      * is established, and with the synchronous channel [ch'] as argument.
      * In particular, the task of the synchronizer is to turn blocking writes to
      * [ch'] into non-blocking writes to [ch]. In general there are two ways of
      * implementation:
      * - Call [f], then buffer all output to [ch'] until the end of the channel is
      *   reached, and finally output the contents of the buffer in an asynchronous
      *   way.
      * - Run [f] in a different thread that blocks whenever there is no space to
      *   write to [ch]. 
      *
      * Both implementations are allowed, i.e. {b it is allowed that [f] runs in
      * a different thread}.
      *
      * CHECK: Exceptions.
     *)
end

class buffering_engine_processing_config : http_engine_processing_config
  (** Implements the synchronisation by buffering *)

class type http_engine_processing_context =
object

  method engine : unit Uq_engines.engine
    (** The engine doing HTTP *)

end

val process_connection :
      #Nethttpd_reactor.http_processor_config ->
      #http_engine_processing_config ->
      Unix.file_descr ->
      Unixqueue.unix_event_system ->
      'a http_service ->
        http_engine_processing_context
  (** Sets up an engine that processes all requests using the service description.
    * This function returns immediately, one needs to [Unixqueue.run] the event
    * system to start the engine.
    *
    * The passed [http_engine_processing_config] is crucial for good performance.
    * XXX
   *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module
     *)
end
