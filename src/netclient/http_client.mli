(* $Id: http_client.mli 1740 2012-02-22 22:31:47Z gerd $ *)

(** HTTP 1.1 client *)

(**********************************************************************)
(* HTTP/1.1 client                                                    *)
(* written by Gerd Stolpmann                                          *)
(**********************************************************************)

(** {b Note for beginners:} There is a simplified interface called
  * {!Http_client.Convenience}.
 *)

(** 
 * Implements the following advanced features:
 *  - chunked message transport
 *  - persistent connections
 *  - connections in pipelining mode ("full duplex" connections)
 *  - modular authentication methods, currently Basic and Digest
 *  - event-driven implementation; allows concurrent service for
 *    several network connections 
 *  - HTTP proxy support, also with Basic and Digest
 *    authentication
 *  - SOCKS proxy support (1)
 *  - HTTPS support via extension module {!Https_client} (in library
 *    [equeue-ssl]). HTTPS proxies are also supported (CONNECT method) (1)
 *  - Automatic and configurable retry of failed idempotent requests
 *  - Redirections can be followed automatically 
 *  - Compressed message bodies can be automatically decoded (gzip only,
 *    method [set_accept_encoding]) (1)
 *
 * Left out:
 *  - multipart messages, including multipart/byterange
 *  - content digests specified by RFC 2068 and 2069   (2)
 *  - content negotiation   (2)
 *  - conditional and partial GET   (2)
 *  - client-side caching   (2)
 *  - HTTP/0.9 compatibility
 *
 * (1) Since Ocamlnet-3.3
 *
 * (2) These features can be implemented on top of this module if really needed,
 *     but there is no special support for them.
 *
 * Related modules/software:
 * - {!Http_fs} allows you to access HTTP servers in the style of filesystems
 * - WebDAV: If you are looking for WebDAV there is an extension of this module:
 *   {{:http://oss.wink.com/webdav/} Webdav}, which is separately available.
 *)

(** {b Thread safety}
  *
  * The module can be compiled such that it is thread-safe. In particular,
  * one has to link the http_client_mt.cm[xo] object, and thread-safety is
  * restricted to the following kinds of usage:
  * - The golden rule is that threads must not share pipeline objects.
  *   If every thread uses its own pipeline, every thread will have its own
  *   set of state variables.
  *   It is not detected if two threads errornously share a pipeline,
  *   neither by an error message nor by implicit serialization. Strange
  *   things may happen.
  * - The same applies to the other objects, e.g. http_call objects
  * - The [Convenience] module even serializes; see below.
  *)



(** {1 Types and Exceptions} *)

exception Bad_message of string;;
  (** The server sent a message which cannot be interpreted. The string
   * indicates the reason.
   *)

exception No_reply;;
  (** There was no response to the request because some other request failed
   * earlier and it was not allowed to send the request again.
   *)

exception Too_many_redirections
  (** While following redirections the limit has been reached *)

exception Name_resolution_error of string
  (** Could not resolve this name - same as {!Uq_engines.Host_not_found} *)

exception URL_syntax_error of string
  (** This URL cannot be parsed after a redirection has been followed.
   *)

exception Timeout of string
  (** A timeout. The string explains which connection is affected.
      {b New since Ocamlnet-3.3.}
   *)

exception Proxy_error of int
  (** An error status from a proxy. This is only used when extra proxy messages
      are used to configure the proxy (e.g. the CONNECT message).
   *)

exception Response_too_large
  (** The length of the response exceeds the configured maximum *)

exception Http_protocol of exn;;
  (** The request could not be processed because the exception condition 
   * was raised. The inner exception is one of the above defined.
   *)

exception Http_error of (int * string);;
  (** {b Deprecated.} 
   * The server sent an error message. The left component of the pair is
   * the error code, the right component is the error text.
   * This exception is only used by [get_resp_body], and by the
   * {!Http_client.Convenience} module.
   *)

type status =
  [ `Unserved
  | `Http_protocol_error of exn
  | `Successful
  | `Redirection
  | `Client_error
  | `Server_error
  ]
  (** Condensed status information of a HTTP call:
    * - [`Unserved]: The call has not yet been finished
    * - [`HTTP_protocol_error e]: An error on HTTP level occurred. Corresponds
    *   to the exception [Http_protocol].
    * - [`Successful]: The call is successful, and the response code is between
    *   200 and 299.
    * - [`Redirection]: The call is successful, and the response code is
    *   between 300 and 399.
    * - [`Client_error]: The call failed with a response code between 400 and
    *   499.
    * - [`Server_error]: The call failed for any other reason.
   *)

type 'message_class how_to_reconnect =
    Send_again         (** Send the request automatically again *)
  | Request_fails      (** Drop the request *)
  | Inquire of ('message_class -> bool)
                       (** If the function return [true] send again, otherwise
			* drop the request.
			*)
  | Send_again_if_idem (** Default behaviour: [Send_again] for idempotent 
			* methods (GET, HEAD), [Request_fails] for the rest
			*)
      (** How to deal with automatic reconnections, especially when the
        * connection crashes.
       *)
;;


type 'message_class how_to_redirect =
    Redirect           (** Perform the redirection *)
  | Do_not_redirect    (** No redirection *)
  | Redirect_inquire of ('message_class -> bool)
                       (** If the function return [true] redirect, otherwise
			* do not redirect. It is legal to set the [Location]
                        * header as part of the action performed by the
                        * function. (Should be an absolute http URL.)
			*)
  | Redirect_if_idem   (** Default behaviour: [Redirect] for idempotent 
			* methods (GET, HEAD), [Do_not_redirect] for the rest
			*)
;;


type private_api ;;
  (** The private part of the [http_call] class type *)


type response_body_storage =
    [ `Memory
    | `File of unit -> string
    | `Body of unit -> Netmime.mime_body
    | `Device of unit -> Uq_io.out_device
    ]
  (** How to create the response body:
    * - [`Memory]: The response body is in-memory
    * - [`File f]: The response body is stored into the file whose name
    *   is returned by [f()]
    * - [`Body f]: The response body is stored into the object returned
    *   by [f()]
    * - [`Device f]: The response is directly forwarded to the device
    *   obtained by [f()] (new since Ocamlnet-3.3)
    *
    * When the function [f] is called in the latter cases the response
    * header has already been received, and can be retrieved with the
    * [response_header] method of the call object. Also, [response_status_text],
    * [response_status_code], and [response_status] return meaningful
    * values.
   *)


type synchronization =
  | Sync
      (** The next request begins after the response of the last request has
       * been received.
       *)
  | Pipeline of int
      (** The client is allowed to send several requests without waiting
       * for responses. The number is the maximum number of unreplied
       * requests that are allowed. A typical value: 5.
       * If you increase this value, the risk becomes higher that requests
       * must be repeatedly sent to the server in the case the connection
       * crashes. Increasing is recommended if you send a bigger number of
       * GET or HEAD requests to the server. Decreasing is recommended if you
       * send large POST or PUT requests to the server.
       *
       * Values > 8 are interpreted as 8.
       *)
(** This type determines whether to keep requests and responses 
  * synchronized or not.
  *
  * The first request/response round is always done in
  * Sync mode, because the protocol version of the other side
  * is not known at that moment. [Pipeline] requires HTTP/1.1.
  *
  * In previous versions of netclient there was a third option,
  * [Sync_with_handshake_before_request_body]. This option is no
  * longer necessary because the HTTP specification has been updated
  * in the meantime, and there is a better mechanism now (the
  * [Expect] header is set).
  *)
;;


type resolver =
    Unixqueue.unix_event_system -> 
    string -> 
    (Unix.inet_addr option -> unit) -> 
      unit
	(** A name resolver is a function [r] called as [r esys name reply].
          * As [name] the name to resolve is passed. The resolver must 
          * finally call [reply] with either the resolved address or
          * with [None], indicating an error in the latter case.
          * The event system [esys] can be used to carry out the resolution
          * process in an asynchronous way, but this is optional.
          *
          * Only 1:1 resolution is supported, 1:n resolution not.
         *)

type channel_binding_id = int
    (** A channel binding identifies a requirement for the transport
	channel, especially whether plain HTTP is sufficient, or HTTPS
	needs to be used, and if so, whether there are further requirements
	for the SSL context. There are the predefined IDs:

	- {!Http_client.http_cb_id} for HTTP connections
	- {!Http_client.https_cb_id} for HTTPS connections without user
	  certificates
        - {!Http_client.proxy_only_cb_id} for the restriction that this
	  protocol can only be used via a web proxy
     *)


type http_options = 
    { synchronization : synchronization;
        (** Default: [Pipeline 5]. *)
      maximum_connection_failures : int;
        (** This option limits the number of connection attempts.
	 * Default: 2
	 *)
      maximum_message_errors : int;
        (** This option limits the number of protocol errors tolerated per
	 * request. If a request leads to a protocol error, the connection
	 * is shut down, the server is connected again, and the request is
	 * tried again (if the kind of the message allows retransmission).
	 * If a request repeatedly fails, this option limits the number
	 * of retransmissions.
	 * Default: 2
	 *)
      inhibit_persistency : bool;
        (** This option turns persistent connections off.
	 * Default: [false]
	 * It is normally not necessary to change this option.
	 *)
      connection_timeout : float;
        (** If there is no network transmission for this period of time,
	 * the connection is shut down, and tried again.
	 * Default: 300.0 (seconds)
	 * It may be necessary to increase this value if HTTP is used for
	 * batch applications that contact extremely slow services.
	 *)
      number_of_parallel_connections : int;
	(** The client keeps up to this number of parallel connections to
	 * a single content server or proxy.
	 * Default: 2
	 * You may increase this value if you are mainly connected with
	 * an HTTP/1.0 proxy.
	 *)
      maximum_redirections : int;
        (** The maximum number of redirections per message *)
      handshake_timeout : float;
        (** The timeout when waiting for "100 Continue". Default: 1.0 *)
      resolver : resolver;
        (** The function for name resolution *)
      configure_socket : Unix.file_descr -> unit;
        (** A function to configure socket options *)
      schemes : (string * Neturl.url_syntax * int option * channel_binding_id) 
                   list;
        (** The list of supported URL schemes. The tuples mean
	    [(scheme, syntax, default_port, cb)]. By default, the
	    schemes "http", "https", and "ipp" are supported.
	 *)
      verbose_status : bool;
      verbose_request_header : bool;
      verbose_response_header : bool;
      verbose_request_contents : bool;
      verbose_response_contents : bool;
      verbose_connection : bool;
      verbose_events : bool;
        (** Enable various debugging message types.
	 * - [verbose_status]: reports about status of received documents
	 * - [verbose_request_header]: prints the header sent to the server
	 * - [verbose_request_contents]: prints the document sent to the server
	 * - [verbose_response_header]: prints the header of the answer from the server
	 * - [verbose_response_contents]: prints the document received from the server
	 * - [verbose_connection]: reports many connection events; authentication,
	 *   too.
         * - [verbose_events]: everything about the interaction with [Unixqueue]
         *
         * By default, [verbose_status] and [verbose_connection] are enabled.
         * Note that you also have to set [Debug.enable] to [true] to see
         * any log message at all!
	 *)
    }
      (** Options for the whole pipeline. It is recommended to change options
	* the following way:
	* {[
	*    let opts = pipeline # get_options in
        *    let new_opts = { opts with <field> = <value>; ... } in
        *    pipeline # set_options new_opts
        * ]}
        *
        * New fields can be added anytime to this record, and this style
        * of changing options is transparent to field additions.
       *)
;;


type header_kind = [ `Base | `Effective ]
  (** The [`Base] header is set by the user of [http_call] and is never
    * changed during processing the call. The [`Effective] header is a copy
    * of the base header at the time the request is sent. The effective header
    * contains additions like [Content-length] and authentication info.
   *)

(** The container for HTTP calls *)
class type http_call =
object
  (** [http_call] is the runtime container for HTTP method calls. It contains
      * the request message, the response message, and the current transmission
      * status.
      *
      * In previous versions of netclient, this class type was called 
      * [message]. This was quite inexact because this class embraces both
      * messages that are part of a call.
      *
      * {b Incompatible changes}:
      * - [using_proxy] has been removed. This simply isn't a property of
      *   an individual call.
      * - [get_req_uri] has been removed from the public interface for similar
      *   reasons.
      * - The request and response messages are now primarily stored as
      *   [Netmime.mime_header] and [Netmime.mime_body] objects. The old
      *   style access methods remain in the API for now. The advantage is
      *   that it is possible to use external files as body containers.
      * - There are two request headers, [`Base] and [`Effective].
   *)

  (** {2 Call State} *)

  method is_served : bool
    (** [true] if request/response cycle(s) have been finished, i.e. the
	* call was successful, or a final error state has been reached.
     *)

  method status : status
    (** The condensed status *)

  (** {2 Accessing the request message (new style) } *)

  method request_method : string
    (** The HTTP method as string *)

  method request_uri : string
    (** The request URI as string. This is always an absolute URI in the
	* form "http://server/path" or "https://server/path".
     *)

  method set_request_uri : string -> unit
    (** Sets the request URI. This implicitly also sets the channel binding
	ID (see below).	

	{b Changed in Ocamlnet-3.3:} The URI is no longer immediately parsed,
	but first when the call is submitted to a pipeline. This means that
	parsing errors will first be reported by the [add] method. The
	background is that parsing now depends on pipeline options.
     *)

  method request_header : header_kind -> Netmime.mime_header
    (** The whole header of the request. Users of this class should only
	* modify the [`Base] header. After the call has been processed,
	* the [`Effective] header contains the version of the header that has
	* actually been transmitted.
	*
	* The user should set the following headers:
	* - [Content-length]: Set this to the length of the request body
	*   if known. (The client falls back to HTTP 1.0 if not set!)
	* - [Content-type]: Set this to the media type of the request body
	* - [Expect]: Set this to "100-continue" to enable a handshake before
	*   the body is sent. Recommended for large bodies. (See also
	*   [set_expect_handshake] below.)
	*
	* The following headers may be set, but there are reasonable defaults:
	* - [Date]
	* - [User-agent]
	*
	* The following headers must not be set:
	* - [Connection]
     *)

  method set_request_header : Netmime.mime_header -> unit
    (** Replaces the whole [`Base] header object *)

  method set_expect_handshake : unit -> unit
    (** Configures that a handshake is done before sending the request body.
	This is useful when the request body can be large, and
	authentication or response indirections are possible.
	{b New since Ocamlnet-3.3.}
     *)

  method set_chunked_request : unit -> unit
    (** Configures that the request is transmitted using chunked encoding.
	This has advantages when the size of the request is not known
	in advance. However, this works only for true HTTP/1.1 servers.
	{b New since Ocamlnet-3.3.}
     *)

  method effective_request_uri : string
    (** The URI actually sent to the server in the request line of the
	* protocol.
     *)

  method request_body : Netmime.mime_body
    (** The whole body of the request. This method fails after
	[set_request_device] has been called (no body then).
     *)

  method set_request_body : Netmime.mime_body -> unit
    (** Replaces the whole body object *)

  method set_request_device : (unit -> Uq_io.in_device) -> unit
    (** Sets that the request data is read from the passed device instead
	of taking it from a body object.  The device must be connected with
	the same event loop as the HTTP client.
     *)

  method set_accept_encoding : unit -> unit
    (** Sets the [Accept-Encoding] field in the request header, and 
	includes all decompression algorithms registered in
	{!Netcompression}. Additionally, the automatic decompression of
	the response body is enabled.

	Note that you need to ensure that the algorithms are really
	registered at {!Netcompression}. For example, to get [gzip]
	support, run {[ Netgzip.init() ]}, and include [netzip] as
	library.
     *)


  (** {2 Accessing the response message (new style) }
      *
      * These methods will fail if the call has not yet been served! 
      * If the call has been finished, but a hard error (e.g. socket error)
      * occurred, the
      * exception [Http_protocol] is raised. When the server only
      * sent an error code, no exception is raised - but the user can
      * manually test for such codes (e.g. with [repsonse_status] or
      * [status]).
   *)

  method response_status_code : int
    (** The three-digit status code *)

  method response_status_text : string
    (** The text transmitted along with the code *)

  method response_status : Nethttp.http_status
    (** The decoded code. Unknown codes are mapped to the generic status
	* values [`Ok], [`Multiple_choices], [`Bad_request], and 
	* [`Internal_server_error].
     *)

  method response_protocol : string
    (** The HTTP version indicated in the response *)

  method response_header : Netmime.mime_header
    (** The whole header of the response. If the call has not succeeded, 
	* [Http_protocol] will be raised.
     *)

  method response_body : Netmime.mime_body
    (** The whole body of the response. If the call has not succeeded, 
	* [Http_protocol] will be raised. If the call has succeeded, 
	* but no body has been transmitted, the empty body is substituted.
	*
	* If the response is directly forwarded to a device (after
	* calling [set_response_body_storage (`Device d)]), there is
	* no accessible response body, and this method will fail.
     *)


  (** {2 Options} *)

  method response_body_storage : response_body_storage
    (** How to create the response body. Defaults to [`Memory]. *)

  method set_response_body_storage : response_body_storage -> unit
    (** Sets how to create the response body *)

  method max_response_body_length : int64
    (** Returns the current maximum length (initially [Int64.max_int]) *)

  method set_max_response_body_length : int64 -> unit
    (** Sets a new maximum length. When the body exceeds this maximum
	by more than the size of the internal buffer, the reception of
	the response is interrupted, and the error is set to
	[Response_too_large].
     *)

  method get_reconnect_mode : http_call how_to_reconnect
    (** Get what to do if the server needs to be reconnected, i.e.
	* if the request must be sent repeatedly.
	* By default, this is [Send_again_if_idem].
     *)

  method set_reconnect_mode : http_call how_to_reconnect -> unit
    (** Sets the reconnect mode *)

  method get_redirect_mode : http_call how_to_redirect
    (** By default, the redirect mode is [Redirect_if_idem]. *)

  method set_redirect_mode : http_call how_to_redirect -> unit
    (** Sets the redirect mode *)
    
  method proxy_enabled : bool
    (** Returns the proxy mode *)

  method set_proxy_enabled : bool -> unit
    (** Sets the proxy mode *)

  method no_proxy : unit -> unit
    (** Same as [set_proxy_enabled false] *)

  method is_proxy_allowed : unit -> bool
    (** {b Deprecated.} Same as [proxy_enabled] *)

  (** {2 Method characteristics}
      *
      * These properties describe the HTTP method 
   *)

  method proxy_use_connect : bool
    (** Whether to use the CONNECT method if the connection is made via a
	web proxy. This is normally true if the channel binding is
	{!Http_client.https_cb_id}
     *)

  method empty_path_replacement : string
    (** The string to substitute in the request line for the empty
	* path. This is usually "/", and for OPTIONS it is "*".
     *)

  method is_idempotent : bool
    (** Whether the method is to be considered as idempotent ( = repeated
	* invocations have the same result and no side effect). This is
	* true for GET and HEAD.
     *)

  method has_req_body : bool
    (** Whether the method allows to send a request body *)

  method has_resp_body : bool
    (** Whether the method allows to reply with a body. This is true
        * except for HEAD.
     *)

  (** {2 Channel bindings} 

      Channel bindings are used to distinguish between security requirements.
      There are normally only two types of requirements:

      - The ID {!Http_client.http_cb_id} is used for messages that can only
        be sent over HTTP connections, i.e. unencrypted TCP. It is automatically
        set when the URL of the message starts with "http://".
      - The ID {!Http_client.https_cb_id} describes the requirement that the
        message can only be sent over HTTPS connections, i.e. TLS-protected
        TCP. It is automatically  set when the URL of the message starts with
        "https://".

      It is possible to change the channel binding to establish further
      types of security requirements (e.g. that certain client certificates
      are used), or even other details of the transport connection.

      The method [channel_binding] is gone from this object type.
      It is now available as part of [pipeline].
   *)

  method set_channel_binding : channel_binding_id -> unit
    (** Sets the channel binding. Note that [set_request_uri] also sets
	the channel binding, but always to the default for the type of URL.
     *)

  (** {2 Repeating calls} *)

  method same_call : unit -> http_call
    (** This method returns a new object that will perform the same call
	* as this object (this function is called "reload" in browsers).
	* The new object is initialized as follows:
	* - The state is set to [`Unserved]
	* - The request method remains the same (the class of the returned
	*   object remains the same)
	* - The request URI is the same string as the original URI
	* - The channel binding ID is the same
	* - The base request header is the same object
	* - The request body is the same object
	* - Options like reconnect, redirect mode, and proxy mode are
	*   copied.
     *)

  (** {2 Old style access methods}
      *
      * These method were introduced in previous versions of netclient,
      * but are quite limited. Some questionable methods are now deprecated
      * and will be removed in future versions of netclient.
   *)

  method get_req_method : unit -> string
    (** Get the name of the request method. Same as [request_method]. *)

  method get_host : unit -> string
    (** The host name of the content server, extracted from the URI.
	
	{b Changed in Ocamlnet-3.3:} The host can first be extracted after
	the call is submitted to a pipeline.
     *)

  method get_port : unit -> int
    (** The port number of the content server, extracted from the URI.
	
	{b Changed in Ocamlnet-3.3:} The port can first be extracted after
	the call is submitted to a pipeline.
     *)

  method get_path : unit -> string
    (** The path (incl. query, if any) extracted from the URI.
	
	{b Changed in Ocamlnet-3.3:} The path can first be extracted after
	the call is submitted to a pipeline.
     *)
    
  method get_uri : unit -> string
    (** the full URI of this message: http://server:port/path. If the
	* path is empty, it is omitted. - Same as [request_uri].
     *)

  method get_req_body : unit -> string
    (** What has been sent as body in the (last) request. Same as
	* [request_body # value].
     *)

  method get_req_header : unit -> (string * string) list
    (** {b Deprecated.}
	* What has been sent as header in the (last) request. Returns
	* (key, value) pairs, where the keys are all in lowercase.
	*
	* In new code, the [request_header] object should be accessed instead.
     *)

  method assoc_req_header : string -> string
    (** Query a specific header entry, or raise [Not_found] *)

  method assoc_multi_req_header : string -> string list
    (** Return all header values for a given field name (header entries
	* which allow several values separated by commas can also be 
	* transmitted by several header lines with the same name).
     *)

  method set_req_header : string -> string -> unit
    (* Set the request header entry with given "name" to "value". *)

  method get_resp_header : unit -> (string * string) list
    (** {b Deprecated.}
	* Get the header of the last response. The keys are in lowercase 
	* characters again.
     *)

  method assoc_resp_header : string -> string
    (** Query a specific header entry of the response. *)

  method assoc_multi_resp_header : string -> string list
    (** Return all response header values for a given field name (header 
	* entries which allow several values separated by commas can also be 
	* transmitted by several header lines with the same name).
     *)

  method get_resp_body : unit -> string
    (** {b Deprecated.}
	* Returns the body of the last response if the response status
	* is OK (i.e. the code is in the range 200 to 299).
	*
	* Otherwise, Http_error (code, body) is raised where 'code' is
	* the response code and 'body' is the body of the (errorneous)
	* response.
     *)

  method dest_status : unit -> (string * int * string)
    (** Returns the status line of the last response (but status lines
	* with code 100 are ignored).
	* The returned triple is (http_string, code, text)
     *)

  (** {2 Private} *)

  method private_api : private_api
end
;;


class type transport_channel_type =
object
  method setup_e : Unix.file_descr -> channel_binding_id -> float -> exn ->
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller Uq_engines.engine
  (** [setup fd cb tmo tmo_x host port esys]: Create or configure a communication
      circuit over the file descriptor [fd] that can be driven by the
      returned multiplex controller object.

      [tmo] is the timeout. After inactivity the  exception [tmo_x] must be
      raised.

      [host] is the name of the machine to connect to. [port] is the port
      number. The descriptor [fd] is already connected to this port, directly
      or via a proxy.
   *)

  method continue : Unix.file_descr -> channel_binding_id -> float -> exn ->
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller
  (** [continue] is called when an already established circuit needs to
      be continued.

      Note that the event system can be different now.
   *)
end


(** {1 HTTP methods} *)



(** This class is an implementation of [http_call]. A subclass must
  * define the virtual methods that mostly define the general properties
  * of the HTTP method.
 *)
class virtual generic_call : 
object
  inherit http_call

  method private virtual fixup_request : unit -> unit
    (** This method is called just before sending the request. It can
      * fix up things in the [`Effective] request header.
     *)

  (** The following methods define the values for the corresponding 
    * methods without the [def_] prefix:
   *)
  method private virtual def_request_method : string
  method private virtual def_empty_path_replacement : string
  method private virtual def_is_idempotent : bool
  method private virtual def_has_req_body : bool
  method private virtual def_has_resp_body : bool
end
;;


(** The following classes are implementations for the various HTTP
  * methods. These classes do not initialize the call object.
 *)

class get_call : http_call
class trace_call : http_call
class options_call : http_call
class head_call : http_call
class post_call : http_call
class put_call : http_call
class delete_call : http_call


(** The following classes initialize the request message of the
  * call (header and body). 
  * These classes are also backward compatible to the classes
  * found in earlier versions of netclient.
 *)

class get : string -> http_call
  (** Argument: URI *)

class trace : string -> int -> http_call
  (** Arguments: URI, maximum number of hops *)

class options : string -> http_call
  (** Argument: URI or "*" *)

class head : string -> http_call
  (** Argument: URI *)

class post : string -> (string * string) list -> http_call
  (** Arguments: URI, parameter list to be transferred as
    * application/x-www-form-urlencoded body
   *)

class post_raw : string -> string -> http_call
  (** Arguments: URI, body *)

class put : string -> string -> http_call
  (** Arguments: URI, body *)

class delete : string -> http_call
  (** Argument: URI *)


(** {1 Authentication} *)

(** A [key] is a user/password combination for a certain realm *)
class type key =
object
  method user : string
    (** The user name *)
  method password : string
    (** The password in cleartext *)
  method realm : string
    (** The realm *)
  method domain : string list
    (** The domain URIs defining the protection space. The domain URIs
     * are absolute URIs. The list must not be empty for content accesses.
     * For proxy keys the list must be empty.
     *)
end


val key : user:string -> password:string -> realm:string -> 
          domain:string list -> key
  (** Create a key object *)


class type key_handler =
object
  method inquire_key :
            domain:string list -> realms:string list -> auth:string -> key
    (** The method is called when a new session must be authenticated.
      * The [domain] is the URI list describing the protection space. URIs
      * currently have the form "http://host:port/path", i.e. the port is
      * always written out. The [realms] parameter is a list
      * of realm identifiers. In [auth] the name of the authentication 
      * method is passed (lowercase characters). The method must
      * search (or query for) a key, and return it. The key must refer to
      * one of the passed realms. The domain of the key must be exactly
      * the same as the passed [domain]. If the method raises [Not_found],
      * authentication will fail.
     *)
  method invalidate_key : key -> unit
    (** The handler is notified that authentication failed for this key *)
end



(** The [key_ring] is a cache for keys. The optional [uplink] handler
  * is invoked when no matching key is found in the cache.
 *)
class key_ring : ?uplink : #key_handler -> unit ->
object
  inherit key_handler
  method clear : unit -> unit
    (** Deletes all keys from this key ring *)
  method add_key : key -> unit
    (** Adds a key to the ring *)
  method keys : key list
    (** List of all known keys *)
end



(** An [auth_session] represents an authenticated session *)
class type auth_session =
object
  method auth_scheme : string
    (** The authentication scheme, e.g. "basic" *)
  method auth_domain : Neturl.url list
    (** The list of domain URIs defines the protection space.

	{b Change:} Since Ocamlnet-3.3, this is a list of {!Neturl.url},
	and no longer a list of strings.
     *)
  method auth_realm : string
    (** The realm *)
  method auth_user : string
    (** The user identifier *)
  method auth_in_advance : bool
    (** Whether "authentication in advance" is enabled *)
  method authenticate : http_call -> (string * string) list
    (** Returns a list of additional headers that will authenticate 
      * the passed call for this session. (This is usually only one
      * header, [authorization].)
      *
      * If the call is authenticated in advance, it does not contain
      * any authentication information. If the call is authenticated
      * in reaction to a 401 status, the response header contains 
      * the [www-authenticate] field(s).
     *)
  (* Maybe future addition: method post_authenticate
   * Needed for auth-int
   *)
  method invalidate : http_call -> bool
    (** The session is notified that authentication failed. (This
      * method is not called for authentication-in-advance, but only
      * if an authentication attempt after a 401 status failed.)
      * The method can return [true] if another authentication should
      * be started immediately.
      *
      * Note: By returning [true] the session can indicate a "stale"
      * condition in the sense of RFC 2617.
     *)
end


(** An authentication handler has the capability of adding the necessary
  * headers to messages.
 *)
class type auth_handler =
object
  method create_session : http_call -> http_options ref -> auth_session option
    (** Create a new authentication session. The passed call has status 401.
     *)
  method create_proxy_session : http_call -> http_options ref -> auth_session option
    (** Same for proxy authentication *)
end

class basic_auth_handler : 
        ?enable_auth_in_advance:bool -> #key_handler -> auth_handler
  (** Basic authentication. Authentication information is obtained by
    * the passed key_handler.
    *
    * [enable_auth_in_advance]: If set to [true], authentication can be
    * done in advance, i.e. before the server requests authentication.
    * This reduces the number of messages exchanged with the server, but
    * may be an additional security risk.
   *)

class digest_auth_handler : 
        ?enable_auth_in_advance:bool -> #key_handler -> auth_handler
  (** Digest authentication. Authentication information is obtained by
    * the passed key_handler.
    *
    * This handler is compatible with RFC 2069 and RFC 2617. In particular,
    * the following protocol options are available:
    * - The algorithms MD5 and MD5-sess are implemented
    * - The quality of protection mode "auth" is implemented. The optional
    *   mode "auth-int" has been omitted.
    * - The information of the [Authentication-Info] header is completely
    *   ignored
    *
    * [enable_auth_in_advance]: If set to [true], authentication can be
    * done in advance, i.e. before the server requests authentication.
    * This reduces the number of messages exchanged with the server, but
    * may be an additional security risk.
   *)

class unified_auth_handler : #key_handler -> auth_handler
  (** Support both digest and basic authentication, with preference to
      digest.

      Note that there is no way of authenticating in advance, as it is
      not known in advance which mechanism is used.
   *)


(** {b Deprecated.} For (limited) backwards compatibility: *)
class basic_auth_method :
  object
    method name : string 
    method set_realm : string -> string -> string -> unit
	(* set_realm realm user password:
	 * adds that (user,password) should be used for the given realm
	 *)
    method as_auth_handler : auth_handler
  end

(** {b Deprecated.} For (limited) backwards compatibility: *)
class digest_auth_method : basic_auth_method


(** {1 Transport} *)

(** A connection cache is an object that keeps connections open that
  * are currently unused. A connection cache can be shared by several
  * pipelines.
 *)

type connection_cache = Http_client_conncache.connection_cache

val close_connection_cache : connection_cache -> unit
  (** Closes all descriptors known to the cache *)

val create_restrictive_cache : unit -> connection_cache
  (** A restrictive cache closes connections as soon as there are no
    * pending requests.
   *)

val create_aggressive_cache : unit -> connection_cache
  (** This type of cache tries to keep connections as long open as
    * possible. The consequence is that users are responsible for
    * closing the descriptors (by calling [close_connection_cache]) when the
    * cache is no longer in use.
   *)

val http_cb_id : channel_binding_id
  (** Identifies a channel binding to pure HTTP (without SSL), with or
      without web proxies
   *)

val https_cb_id : channel_binding_id
  (** Identifies a channel binding to anonymous HTTPS (i.e. no client
      certificates), with or without web proxies.
   *)

val proxy_only_cb_id : channel_binding_id
  (** Identifies a channel binding to web proxy connections. Use this to
      e.g. send an FTP URL to a web proxy via HTTP
   *)

val new_cb_id : unit -> channel_binding_id
  (** Allocates and returns a new ID *)

val http_transport_channel_type : transport_channel_type
  (** Transport via HTTP *)

type proxy_type = [`Http_proxy | `Socks5 ] 


(** {1 Pipelines} *)

(** A pipeline is a queue of HTTP calls to perform *)
class pipeline :
  object
    (** A [pipeline] object is a FIFO queue of HTTP calls. It is called
     * "pipeline" because it is processed asynchronously: Requests may be
     * sent to the HTTP server independently of whether responses of the
     * previous requests already arrived or not.
     *
     * Furthermore, a [pipeline] object may keep connections to several
     * servers at once. (More exactly, it has a FIFO queue for every
     * server it is connected with.)
     *
     * The [pipeline] object keeps track what is happening, so you need
     * not to care about the details of communications. The API is
     * simple: Create a [pipeline] object, do some setup (add authentication
     * methods; configure the proxy to use), add the requests, and 
     * [run] the pipeline. The rest is done automatically. To get the results,
     * you can either memorize the requests you wanted to know yourself
     * and ask every request object about the reply of the server; or
     * you can specify that a callback function should be called once
     * the request is processed (with positive or negative result).
     * It is possible to add further requests to the pipeline from within
     * these callback functions.
     *
     * If you want to have several pipelines, or some cooperation with
     * other network services, you may specify a [Unixqueue.event_system].
     * For example, to have two pipelines working concurrently:
     *
     * {[
     * let ues = Unixqueue.create_unix_event_system() in
     * let p1 = new pipeline in
     * let p2 = new pipeline in
     * p1 # set_event_system ues;
     * p2 # set_event_system ues;
     * Unixqueue.run ues             (* run p1 and p2 in parallel *)
     * ]}
     *
     * This works not only with pipelines, but with every network client
     * or server which is compatible with the [Unixqueue] design.
     *)

    method event_system : Unixqueue.event_system
      (** Returns the event system *)

    method set_event_system : Unixqueue.event_system -> unit
      (** Sets the event system. Must be called before the first call
        * is added
        *)

    method connection_cache : connection_cache
      (** The current connection cache. By default, a private
          * restrictive cache is used.
         *)

    method set_connection_cache : connection_cache -> unit
        (** Set the connection cache. This must happen before the first
          * call is added.
          *)

    method add_authentication_method : basic_auth_method -> unit
	(** adds an old-style authentication method *)

    method add_auth_handler : auth_handler -> unit
	(** adds a new-style authentication handler *)

    method set_proxy : string -> int -> unit
	(** [set_proxy name port]:
	 * sets that an HTTP proxy [name] listening on [port] is to be used
	 *)

    method set_proxy_auth : string -> string -> unit
	(** sets user and password for the proxy. Works for both "digest"
	    and "basic" mechanisms. Any realm is acceptable.
	 *)

    method avoid_proxy_for : string list -> unit
	(** sets a list of host names or domain suffixes for which no proxy
	 * should be used. 
	 * e.g. [ "localhost"; ".our.net" ]
	 *)

    method set_proxy_from_environment : unit -> unit
	(** Inspect the environment variables [http_proxy] and [no_proxy]
	 * and set the proxy options from them.
	 *)

    method set_socks5_proxy : string -> int -> unit
      (** Sets that a SOCKS version 5 proxy is used at this host and port.
	  There is no authentication. The [avoid_proxy_for] setting is
	  honoured.
       *)

    method configure_transport : 
             channel_binding_id -> transport_channel_type -> unit
       (** [configure_transport id transport]: Configures that messages with
	   channel binding ID [id] are exchanged on [transport].

	   By default, there is only a configuration for
	   {!Http_client.http_cb_id}, i.e. for normal unencrypted channels.
	*)

    method set_transport_proxy : channel_binding_id ->
                                 string ->
                                 int ->
                                 (string * string) option ->
                                 proxy_type -> unit
      (** [set_transport_proxy id host port auth ptype]: Sets a special
	  proxy for the transport identified by [id]. This overrides
	  [set_proxy], [set_proxy_auth], and [set_socks5_proxy] for the
	  given transport.
       *)

    method set_transport_proxy_from_environment : 
             (string * channel_binding_id) list -> unit
      (** Like [set_proxy_from_environment], this method inspects environment
	  variables and configures the proxy settings. This function, however,
	  is more flexible, and can use different environment variables for
	  different transports.

	  The argument list has pairs [(var_name, id)] meaning that the
	  environment variable [var_name] configures the proxy for [id].
	  For instance, 
	  {[ [("http_proxy", http_cb_id); ("https_proxy", https_cb_id)] ]}
	  means that these two variables are used for the respective
	  transports.

	  The variable ["no_proxy"] is interpreted anyway.
       *)

    method reset : unit -> unit
	(** Empties the pipeline and inactivates any open connection.
	 * The currently active operation is interrupted, and every request
	 * with response is set to [No_reply] (i.e. you get the exception
	 * [No_reply] if you try to access the response).
	 * If there are callbacks for these requests, the callback
	 * functions are invoked.
	 * The queues of open requests and replies are cleared. All
	 * connections to all servers are inactivated.
         *
         * Inactivation means that open connections are given back
         * to the connection cache for further reuse if the state
         * of the connection allows this; otherwise the connections are
         * closed.
	 *)

    method add : http_call -> unit
	(** Adds the call to the end of the pipeline. 
         * One must not add calls that have already been served.
	 *)

    method add_with_callback : http_call -> (http_call -> unit) -> unit
	(** Adds the call to the end of the pipeline.
         *
	 * After the call has been processed, the callback function
	 * is called. This function is called for every call that
	 * leaves the pipeline, it does not matter whether processing
	 * was successful or not. Invoke [status] on the message
	 * to get what happened; either some status information from the
	 * server is available (perhaps OK status), or an exception is
	 * indicated.
	 *)

    method add_e : http_call -> http_call Uq_engines.engine
      (** The same as engine: The call [c] is added to the pipeline, and
	  when it is processed, the returned engine transitions to the
	  state [`Done c].
       *)

    method proxy_type : string -> proxy_type option
      (** [proxy_type url] returns [Some pt] if a proxy would be used for this
	  [url], and [None] if a direct connection would be made.
       *)

    method proxy_type_of_call : http_call -> proxy_type option
      (** Same for an already created call object *)

    method channel_binding : http_call -> channel_binding_id
      (** Reports the current channel binding of this call *)


    method run : unit -> unit
      (** Runs through the requests in the pipeline. If a request can be
       * fulfilled, i.e. the server sends a response, the state of the
       * request is set and the request is removed from the pipeline.
       * If a request cannot be fulfilled (no response, bad response, 
       * network error), the exception is stored in the [http_call]
       * object and will be raised once the state of the object is
       * queried.
       *
       * Under certain conditions (serious network errors) [run] does
       * not catch the exception; it simply cleans its own state up
       * (aborting the errorneous network connection). In this case,
       * simply invoke [run] again to continue.
       * [run] terminates normally if the pipeline becomes empty.
       *
       * The engine handles the following HTTP return codes itself:
       * - 100: This is an intermediate return code 
       * - 301: The redirection is followed if configured
       * - 302: The redirection is followed if configured
       * - 401: Content server authentication
       * - 407: Proxy server authentication
       *
       * All other return codes remain uninterpreted, it is up to the
       * caller of this function to react on them.
       *
       *)

    method get_options : http_options
    method set_options : http_options -> unit
      (** Get/Set the available options for the HTTP engine. 
       * The new options will take into effect immediately.
       *)

    method number_of_open_messages : int
      (** Returns the number of messages which are still in the pipeline. *)

    method number_of_open_connections : int
      (** Returns the number of connections which are open at the same time
        * and currently being used by this object (i.e. connections 
        * returned to the cache do not count)
       *)

    method connections : (string * int * int) list
      (** Reports which connections exist: [ (host, port, queue_length) ] *)

    method cnt_new_connections : int
      (** Counts new connections (or better: attempts to establish connections)
        *)

    method cnt_timed_out_connections : int
      (** Counts connections given up because of timeouts *)

    method cnt_crashed_connections : int
      (** Counts connections with network or protocol errors *)

    method cnt_server_eof_connections : int
      (** Counts connections the server terminated with EOF *)

    method cnt_successful_connections : int
      (** Counts connections closed because pipelines become empty *)

    method cnt_failed_connections : int
      (** Counts totally failed connections (no more reconnects allowed) *)

    method reset_counters : unit -> unit

      (** Notes on counters:
        *
        * - [cnt_new_connections]: Is increased when a new connection attempt
        *   is started (that may fail or timeout in the future). Reconnects
        *   do not count.
        * - [cnt_timed_out_connections]: Is increased whenever an established
        *   connection times out. Usually, it is immediately reconnected.
        * - [cnt_crashed_connections]: Is increased whenever an established
        *   connection crashes. Usually, it is immediately reconnected.
        * - [cnt_failed_connections]: Is increased when a timed out or
        *   crashed connection exceeds the maximum number of errors, and it is
        *   not tried to reconnect.
        * - [cnt_successful_connections]: Is increased when all HTTP calls
        *   have been replied.
        *
        * When the client has done all of its jobs, we have
        *
        * {[ cnt_new_connections = cnt_failed_connections + cnt_successful_connections ]}
        *
       *)

  end
;;


(** {b Example using the pipeline:}
  *
  * {[
  * let call = new get "http://server/path" in
  * let pipeline = new pipeline in
  * pipeline # add call;
  * pipeline # run();    (* Now the HTTP client is working... *)
  * match call # status with
  * | `Successful -> ...
  * | ...
  * ]}
 *)

(** {2 Auxiliary pipeline functions} *)

val parse_proxy_setting : string -> (string * int * (string * string) option)
  (** Parses the value of an environment variable like [http_proxy],
      i.e. an HTTP URL. The argument is the URL.
      Returns [(host,port,auth)] where [auth] may include user name and
      password.
   *)

val parse_no_proxy : string -> string list
  (** Parses the value of an environment variable like [no_proxy]. Returns
      the list of domains.
   *)


(** {1 Convenience module for simple applications} *)

(** Do [open Http_client.Convenience] for simple applications. *)


module Convenience :
sig
 
  (** Convenience module for simple applications *)

  (** Do [open Http_client.Convenience] for simple applications. *)

  (** The functions of this module share the following behaviour:
    *
    * The environment variables [http_proxy] and [no_proxy] determine 
    * the proxy settings. [http_proxy] must be an http-URL that contains
    * the proxy's name, its port, and optionally user and password.
    * E.g. "http://eric:ericspassword\@proxy:8080/".
    * The variable [no_proxy] is a comma-separated list of hosts and
    * domains for which no proxy must be used.
    * E.g. "localhost, sun, moon, .intra.net"
    *
    * There is a default behaviour for authentication. Both "basic" and 
    * "digest" methods are enabled. Two global variables, [http_user] and
    * [http_password] set the user and password if the URL does not specify
    * them. In the case that user and password are included in the URL,
    * these values are always
    * used.
    *
    * There is a default error behaviour. If a request fails, it is
    * automatically repeated. The variable [http_trials] specifies the number
    * of times a request is submitted at most.
    * Requests are not repeated if there is a HTTP return code that indicates
    * a normal operating condition.
    * POST and DELETE requests are never repeated.
    *
    * Error codes are reported as {!Http_client.Http_error}. Note that
    * this is different than what the pipeline core does.
    *)

  (** {b Thread safety}
   *
   * The Convenience module is fully thread-safe with the exception of the
   * exported variables (http_trials, http_user, and http_password). Note
   * that all threads share the same pipeline, and access to the pipeline
   * is serialized.
   * The latter simply means that it always works, but that threads may 
   * block each other (i.e. the program slows down if more than one thread
   * wants to open http connections at the same time).
   *)

  val http_trials : int ref
    (** number of times every request is tried. Default: 3 *)

  val http_user : string ref
    (** The default user if authentication is required *)

  val http_password : string ref
    (** The default password if authentication is required *)

  val configure_pipeline : (pipeline -> unit) -> unit
    (** This function will be called before the pipeline is used. This
	is intended for fine-grained configuration.
     *)

  val http_get_message : string -> http_call
    (** Does a "GET" request with the given URL and returns the message *)
	  
  val http_head_message : string -> http_call
    (** Does a "HEAD" request with the given URL and returns the reply. *)

  val http_post_message : string -> (string * string) list -> http_call
    (** Does a "POST" request with the given URL and returns the reply.
      * The list contains the parameters sent with the POST request.
      *)

  val http_put_message : string -> string -> http_call
    (** Does a "PUT" request with the given URL and returns the reply.
      * The second argument contains the contents to be put.
     *)

  val http_delete_message : string -> http_call
    (** Does a "DELETE" request with the given URL and returns the reply. *)

  val http_get : string -> string
    (** Does a "GET" request with the given URL and returns the message
      * body
     *)

  val http_post : string -> (string * string) list -> string
    (** Does a "POST" request with the given URL and returns the response body.
      * The list contains the parameters send with the POST request.
     *)

  val http_put : string -> string -> string
    (** Does a "PUT" request with the given URL and returns the response body.
      * The second argument contains the contents to be put.
     *)

  val http_delete : string -> string
    (** Does a "DELETE" request with the given URL and returns the response
      * body.
     *)

  val http_verbose : 
        ?verbose_status:bool ->
        ?verbose_request_header:bool ->
        ?verbose_response_header:bool ->
        ?verbose_request_contents:bool ->
        ?verbose_response_contents:bool ->
        ?verbose_connection:bool ->
        ?verbose_events:bool ->
        unit -> unit
    (** Turns on debug messages on stderr. By default, all options are 
	turned on.
     *)

end

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module *)
end
