(* $Id: nethttpd_types.mli 1410 2010-02-14 19:44:28Z gerd $
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
 * along with WDialog; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Type definitions for the HTTP daemon
  *
  * {b Contents}
  *
  * - {!Nethttpd_types.exceptions}
  * - {!Nethttpd_types.environment}
  * - {!Nethttpd_types.service}
  * - {!Nethttpd_types.helpers}
 *)

(** Many types can also be found in the [Nethttp] module (part of netstring).
  * Furthermore, [Netcgi_env] and [Netcgi_types] are of interest (part of cgi).
 *)

(** {1:exceptions Exceptions} *)

open Nethttp

exception Standard_response of http_status * http_header option * string option
  (** Some HTTP containers allow you to raise this exception. The standard
    * response corresponding to [http_status] is sent back to the client.
    * If the third argument exists, an entry into the error log
    * is written.
   *)

(**********************************************************************)

(** {1:environment Environment} *)

type output_state =
    [ `Start
    | `Sending
    | `End
    ]

val string_of_output_state : output_state -> string
  (** Debugging *)

(** An extension of [cgi_environment] for use with the daemon. The methods
  * retrieving the socket addresses are virtual.
 *)
class type virtual v_extended_environment =
object
  inherit Netcgi.cgi_environment

  method virtual server_socket_addr : Unix.sockaddr
  method virtual remote_socket_addr : Unix.sockaddr
    (** These are always the physical IP addresses and ports of the two endpoints
      * of the current connection.
     *)

  method cgi_request_uri : string
    (** The full request URI. Identical to the CGI property "REQUEST_URI" *)

  method log_props : (string * string) list -> unit
    (** Remember this version of [cgi_properties] as the one sent to the
        [config_log_access] function
     *)

  method input_channel : Netchannels.in_obj_channel
    (** The input channel for reading the body of the request *)

  method input_body_size : int64
    (** so far known, or 0L *)

  method request_body_rejected : bool
    (** so far known, or false *)

  method send_file : Unix.file_descr -> int64 -> unit
    (** Sends the output header with a file as body. The file must already be open,
     * and positioned where the transmission begins. The number is the length
     * of the transmission.
     *
     * This method may return immediately when it is possible to open the file, and
     * to set the kernel up for file transmission. Otherwise a [Unix_error] is
     * raised. It is also allowed that this method blocks until the file is actually
     * transmitted.
     *
     * It is not allowed to print to the output channel and to call [send_file].
     * Only one transmission method must be invoked.
     *)

  method virtual output_state : output_state ref
    (** Reflects the state of the output generation:
        - [`Start]: Nothing is generated yet
        - [`Sending]: Output is already being sent
        - [`End]: The response (for a single request) has been fully sent
     *)

end



(** Same as [v_extended_environment], but no virtual methods 
 *)
class type extended_environment =
object
  inherit v_extended_environment
  method server_socket_addr : Unix.sockaddr
  method remote_socket_addr : Unix.sockaddr
  method output_state : output_state ref
end


(** {2 Construction of environments} *)

class virtual empty_environment :
object
  inherit v_extended_environment
  val mutable config : Netcgi.config
  val mutable protocol : Nethttp.protocol
  val mutable in_header : http_header
  val mutable out_header : http_header
  val mutable properties : (string * string) list
  val mutable in_channel : Netchannels.in_obj_channel
  val mutable out_channel : Netchannels.out_obj_channel
end
  (** This class implements an environment with defined internal containers.
    * These containers are empty, but fully functional.
    * The following methods are empty and should be redefined:
    * - [send_output_header]
    * - [send_file]
    * - [log_error]
    * - [log_props]
    *
    * The virtual methods, of course, must be defined!
   *)

class redirected_environment : 
        ?in_header : http_header ->
        ?properties : (string * string) list ->
        ?in_channel : Netchannels.in_obj_channel ->
        extended_environment ->
          extended_environment
  (** This class overlays the input-side containers of an existing environment.
    * The output-side containers ([out_header], and [out_channel])
    * are physically identical with the existing environment.
    *
    * If one of the argument is not passed on class instantiation, the corresponding
    * overlay container is initialized with the current value of the passed
    * environment. As exception of this rule, the input channel is initialized with
    * an empty input channel.
   *)

(** {2 Auxiliary Functions for Environments} *)

val output_static_response : #extended_environment ->
                             http_status -> http_header option -> string -> unit
  (** Outputs the string argument as response body, together with the given status and
    * the header (optional). Response header fields are set as follows:
    * - The [Content-Length] is set to the length of the string.
    * - The [Content-Type] is set to "text/html" unless given by the header.
    *
    * If the header is not passed, the header of the environment is taken. If the header
    * argument exists, however, it overrides the header of the environment.
   *)

val output_file_response : #extended_environment ->
                           http_status -> http_header option ->
                           string -> int64 -> int64 -> unit
  (** Outputs the contents of a file as response body, together with the given status and
    * the header (optional). The string is the file name. The first int64 number is
    * the position in the file where to start, and the second number is the length
    * of the body.  Response header fields are set as follows:
    * - The [Content-Length] is set to the length of the string.
    * - The [Content-Type] is set to "text/html" unless given by the header.
    *
    * Note that [Content-Range] is not set automatically, even if the file is only
    * partially transferred.
    *
    * If the header is not passed, the header of the environment is taken. If the header
    * argument exists, however, it overrides the header of the environment.
    *
    * The function raises [Sys_error] when the file cannot be read.
   *)

(** {1 Generating error responses, logging} *)

class type request_info =
object
  method server_socket_addr : Unix.sockaddr
    (** The socket address of this server. May raise [Not_found] if
        there is no such address *)
  method remote_socket_addr : Unix.sockaddr
    (** The socket address of the client. May raise [Not_found] if
        there is no such address *)
  method request_method : string
    (** The method like [GET]. May raise [Not_found] *)
  method request_uri : string
    (** The URI of the client request. This is often without the
        server designation, i.e. just [/path?query].
        May raise [Not_found] *)
  method input_header : Nethttp.http_header
    (** The request header. May raise [Not_found] *)
  method cgi_properties : (string * string) list
    (** The distilled CGI properties *)
  method input_body_size : int64
    (** The size of the input body. May raise [Not_found] *)
end


class type full_info =
object
  inherit request_info
  method response_status_code : int
    (** The HTTP status code to response *)
  method request_body_rejected : bool
    (** Whether the request body was rejected *)
  method output_header : Nethttp.http_header
    (** The response header *)
  method output_body_size : int64
    (** The size of the output body. *)
end


class create_full_info : response_status_code:int ->
                         request_body_rejected:bool ->
                         output_header:Nethttp.http_header ->
                         output_body_size:int64 ->
                         request_info -> full_info
  (** Creates a [full_info] object by adding to a [request_info] object *)


class type error_response_params =
object
  inherit request_info

  method response_status_code : int
    (** The HTTP status code to response *)

  method error_message : string
    (** The error message explaining the detail that went wrong *)
end


class type min_config =
object
  method config_error_response : error_response_params -> string
  method config_log_error : request_info -> string -> unit
end
  (** Minimal configuration needed for [output_std_response] *)

val output_std_response : #min_config -> #extended_environment ->
                          http_status -> http_header option -> string option -> unit
  (** Outputs a "standard response" for the [http_status]. The string argument
    * is an optional entry into the error log.
    *
    * If the header is not passed, an empty header is taken. If the header argument
    * exists, this header is taken. The header of the environment is never taken.
   *)


(**********************************************************************)

(** {1:service Service Providers}
  *
  * Service providers are defined using the three class types:
  * - [http_service]: The service provider as such. When a HTTP header has been
  *   received, and the service provider is invoked, it returns an object
  *   fitting to the next class type, [http_service_receiver]. This object
  *   is tagged with [`Accept_body]; at this point there are also alternate ways
  *   of processing, see below.
  * - [http_service_receiver]: The task of this object is to receive the request
  *   body. When the body has been completely received, the object is notified,
  *   and returns a third object of type [http_service_generator].
  * - [http_service_generator]: The task of this object is to generate the
  *   response.
  *
  * An implementor is free to define only one class that satisfies all three
  * class types at once. However, this is only an option.
  *
  * The three objects reflect three stages of HTTP processing. The stages have
  * made explicit to allow the implementor of services to intercept the points
  * in time when the processing of the next stage begins. Furthermore, in multi-threaded
  * environments it is allowed that the stages are performed in the contexts of
  * different threads.
  *
  * In addition to the three-stage model there also several faster paths of 
  * processing:
  * - [`Reject_body] can be used to refuse the acceptance of the request body when
  *   it is already clear that an error response is sent back. This path skips
  *   the stage [http_service_receiver].
  * - [`Static] can be used to send a constant string back (only to be used
  *   when the string needs not to be computed)
  * - [`File] can be used to send the contents of a file back (only to be used
  *   when the file already exists)
 *)

exception Redirect_request of string * http_header
  (** The "early" redirect is only allowed in stage 1 of HTTP processing.
    * The string argument is the new URI path of the request. The header can also
    * be exchanged except the fields that are needed to decode the request
    * body. It is not possible to change the method.
   *)

exception Redirect_response of string * http_header
  (** The "late" redirect is only allowed in stage 3 of HTTP processing.
    * The string argument is the new URI path of the request.  The header can also
    * be exchanged except the fields that are needed to decode the request
    * body. {b The method is always changed to [GET].}
   *)


class type http_service_generator =
object
  method generate_response : extended_environment -> unit
    (** Third stage of HTTP processing:
      * This method is called when the HTTP request has been completely received,
      * and the response is going to be generated. This method can again be called
      * from a different thread than the previous stages. It is allowed to spend
      * any necessary time to compute the response.
      *
      * When the method returns, the request processing is finished. No more data
      * is allowed to be written to the output channel past this moment.
      *
      * The method may raise [Standard_response] to generate one of the
      * standard messages.
     *)
end


class type http_service_receiver =
object
  method process_body : extended_environment -> http_service_generator
    (** Second stage of HTTP processing:
      * This method is called when the body is expected to be arriving. Note that
      * the main difference to [process_header] is that this method can be
      * called from a different thread. It is allowed (and expected) that this method
      * blocks while reading the input. Of course, the input and output
      * channels of the environment are unlocked, and can be used.
      * 
      * When the method returns, the request processing continues with stage 3.
      * Any body data not read is dropped.
      *
      * It is allowed that this method generates a response (or part of it),
      * although this should be better done in stage 3.
      *
      * The method may raise [Standard_response] to generate one of the
      * standard messages.
      *
      * One way of implementing this method is to instantiate [Netcgi.std_activation].
     *)

end


type http_service_reaction =
    [ `Accept_body of http_service_receiver
    | `Reject_body of http_service_generator
    | `Static of http_status * http_header option * string
    | `File of http_status * http_header option * string * int64 * int64
    | `Std_response of http_status * http_header option * string option
    ]
    (** Indicates the immediate reaction upon an arriving HTTP header:
      * - [`Accept_body] is the regular way of processing requests. If necessary,
      *   the client is told to continue sending the rest of the request.
      * - [`Reject_body] can be used when the body of the request is not needed,
      *   and the response will be negative.
      * - [`Static] means to send the header and a constant string back as response.
      *   The header is taken from the environment if not explicitly passed,
      *   Note: The [Content-Length] header is automatically added. The [Content-Type]
      *   defaults to "text/html".
      * - [`File] is similar to this, but the data come from a file. The file
      *   is specified by an absolute pathname. The range of the file is given
      *   by the start position and the length of the range.
      *   The header is taken from the environment if not explicitly passed,
      *   Note: The [Content-Length] header is automatically added. The [Content-Type]
      *   defaults to "text/html".
      * - [`Std_response] is similar to [`Static], however the body is the standard
      *   text for the status code. If the header is omitted, it is taken as empty.
      *   The third argument is an optional entry into the error log.
      *   Note: The [Content-Length] header is automatically added. The [Content-Type]
      *   defaults to "text/html".
     *)

class type ['a] http_service =
object
  method name : string
    (** The name of the type of the service provider *)

  method def_term :'a
    (** The definition term *)

  method print : Format.formatter -> unit
    (** Outputs the definition term to a formatter *)

  method process_header : extended_environment -> http_service_reaction
    (** First stage of HTTP processing:
      * This method is called when the HTTP header has been received. This method
      * must return quickly without blocking the thread how to go on. For example,
      * this could look as follows:
      * - Check whether the client is allowed to access this resource. If this
      *   can be done immediately, it should be done now. (If an external service
      *   must be queried, the check must not be done now, but deferred to the
      *   second or third stage.) If the access is denied, an error response can
      *   be sent back using [`Static], [`File], or, if computed, using [`Reject_body].
      * - Check whether the request is delegated to another service provider
      *   (e.g. lookup by hostname, by port number, or by URI). In this case,
      *   the result of this [process_header] call is simply the result of the
      *   [process_header] call of the other service provider.
      * - If this service provider generates the contents, there are mainly two
      *   types of reaction. If the contents are stored in a file, one can simply
      *   return [`File]. Otherwise, return [`Accept_body] to continue with the
      *   second stage. Note that even if no body is expected to arrive, one must
      *   go through the second stage, and drop any unexpected body.
      *
      * The argument of this function is the Netcgi environment. The header is
      * complete, including the request method. One cannot access the input and
      * output channels at this stage of processing.
     *)
end

(**********************************************************************)

(** {1:helpers Helpers} *)

val update_alist : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list
  (** [update_alist updl l]: Returns the alist with all elements of [updl]
    * and all elements of [l] that are not member of [updl].
   *)
