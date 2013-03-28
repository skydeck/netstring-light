(* $Id: rpc_client.mli 1709 2012-02-16 01:17:04Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** RPC clients *)

(** This module implements an RPC client, i.e. provides means to connect
 * to an RPC service and call remote procedures.
 * In general, this module works in an asynchronous way and is implemented
 * event-driven. All events are handled by an event queue of type
 * Unixqueue.t that must already exist and to which this module adds its
 * own event handlers and event resources. This means that this module
 * can co-exist with other services and share the same event queue with
 * them.
 *
 * You can push several procedure calls on the event queue at once.
 * The queue serves then as a pipeline; the calls are sent to the
 * server as long as the server accepts new calls. Replies are received
 * in any order, and the return values of the remote procedures are
 * delivered using a callback function.
 *
 * You can set timeouts and force automatic retransmission if you want
 * this; these features are enabled by default if the underlying transport
 * mechanism is UDP. Timeouts and other exceptions are delivered to the
 * callback functions, too.
 *
 * The whole mechanism is designed to allow maximum parallelism without
 * needing to use the multi-threading features of O'Caml. Especially,
 * the following parallelisms can be done:
 * - Call several procedures of the same server in parallel. Note that
 *   this does not necessarily mean that the procedures are run in
 *   parallel since the server is free to decide whether to work
 *   in a synchronous or asynchronous way.
 * - Call several procedures of different servers in parallel. To do so,
 *   simply add several RPC clients to the same event queue.
 * - Call a procedure and do something completely different in the
 *   background; this works well as long as the other task can be
 *   programmed using file descriptor events, too.
 *
 * However, there are still some restrictions concerning asynchronous
 * calls. Some of them will be removed in the future, but others are
 * difficult to tackle:
 * - Authentication methods requiring RPC calls or other network services are
 *   performed in an synchronous way, too.
 * - Name service lookups are synchronous, too.
 *
 * {b Multi-threading:} Only a single thread may use an RPC client at a
 * time. There is a way so that several threads can share the same client
 * without giving up concurrency, see {!Uq_mt.rpc_client} for details.
 *)

open Rpc
open Xdr
open Rtypes

(* The following exceptions are delivered to the callback function: *)

exception Message_lost
  (** got EOF when some pending procedure calls were not replied or even sent *)

exception Message_timeout
  (** After all retransmissions, there was still no reply *)

exception Response_dropped
  (** Drop reason: The response exceeded the configured maximum message size *)

exception Communication_error of exn
  (** an I/O error happened *)

exception Client_is_down
  (** The RPC call cannot be performed because the client has been shut down
   * in the meantime. You can get this exception if you begin a new call,
   * but the connection is closed now.
   *)

exception Keep_call
  (** This exception can be raised by the callback function that is invoked
   * when the server response arrives. It causes that the RPC call record
   * is kept in the housekeeping structure of the client. If the server
   * sends another response, the callback function will be invoked again.
   * I.e. one call can be replied several times (server-driven batching).
   *)

exception Unbound_exception of exn
  (** This exception can be raised by the callback function that is invoked
   * when the server response arrives. It simply causes that the inner
   * exception bypasses the exception handler, and falls through to the 
   * caller of [Unixqueue.run]. This is useful to jump out of the running RPC
   * routines.
   *)


type t
  (** The type of RPC clients *)


type connector =
    Inet of (string * int)                    
      (** Hostname or IP address, port *)
  | Internet of (Unix.inet_addr * int)
      (** The address plus port *)
  | Unix of string
      (** Path to unix dom sock. Not supported on Win32.
       *)
  | W32_pipe of string
      (** Path to named pipe (only Win32) *)
  | Descriptor of Unix.file_descr
      (** Pass an already open socket descriptor. The descriptor will not
        * be closed when the client is done! On Win32, the proxy descriptors
        * as returned by {!Netsys_win32.pipe_descr} are also accepted.
        *)
  | Dynamic_descriptor of (unit -> Unix.file_descr)
      (** The function is called to get the socket descriptor. 
        * Unlike [Descriptor], the descriptor will be closed when the
        * client is done (unless it is a proxy descriptor)
        *)
  | Portmapped of string
      (** The portmapper on this host is queried to get address information *)

val connector_of_sockaddr : Unix.sockaddr -> connector
  (** Converts the socket address into a connector *)

val connector_of_socksymbol : Netsockaddr.socksymbol -> connector
  (** Converts the {!Netsockaddr.socksymbol} into a connector *)

val shutdown_connector : 
  t -> Rpc_transport.rpc_multiplex_controller -> (unit -> unit) -> unit
  (** The default implementation to shut down the connector. Actions are
    * triggered that will take the connector down at some time in the future.
    * At this time, the callback function is invoked.
    *
    * For [Descriptor] connector the socket is shut down but not closed.
    * For the other connector types the socket is also closed. 
    * Win32 named pipes are shut down.
   *)

class type socket_config =
object
  method non_blocking_connect : bool
   (** [non_blocking_connect]: Whether the remote service is connected
    *   in the background. In this case, [create2] immediately returns,
    *   and it is already possible to add procedure calls. However, these
    *   calls are deferred until the connection is established.
    *)
  method multiplexing :
    close_inactive_descr:bool ->
    protocol -> Unix.file_descr -> Unixqueue.event_system ->
      Rpc_transport.rpc_multiplex_controller Uq_engines.engine
    (* close_inactive_descr: also implies that release_fd is called *)
end
  (** Configuration for [`Socket] (see below). *)


val default_socket_config : socket_config
  (** Default configuration with [non_blocking_connect] = true *)

class default_socket_config : socket_config
  (** Default configuration as class *)

val blocking_socket_config : socket_config
  (** Configuration with [non_blocking_connect] = false *)

class blocking_socket_config : socket_config
  (** blocking [connect] configuration as class *)

type mode2 =
    [ `Socket_endpoint of protocol * Unix.file_descr 
    | `Multiplexer_endpoint of Rpc_transport.rpc_multiplex_controller
    | `Socket of protocol * connector * socket_config
    ]
  (** Determines the type of the client for [create2]:
    *
    * - [`Socket_endpoint(proto,fd)]: Socket [fd] is a connected socket
    *   descriptor used for communication. [proto] determines the
    *   encapsulation; should be [Tcp] for stream sockets and [Udp] for
    *   datagram sockets. The descriptor will be closed when the client
    *   terminates.
    *
    * - [`Multiplexer_endpoint m]: [m] is an RPC multiplex controller.
    *
    * - [`Socket(proto, conn, config)]: Creates and connect a client
    *   socket according to [conn]. [proto] determines the
    *   encapsulation; should be [Tcp] for stream sockets and [Udp] for
    *   datagram sockets. [config] specifies configuration details.
   *)

val create2 :
      ?program_number:uint4 ->
      ?version_number:uint4 ->
      ?initial_xid:int ->
      ?shutdown:(t -> Rpc_transport.rpc_multiplex_controller -> 
		   (unit -> unit) -> unit) ->
      mode2 ->
      Rpc_program.t ->
      Unixqueue.event_system ->
      t
  (** New style clients:
   * Opens a connection to the server specified by [mode2].
   * The server is assumed to implement an RPC program as specified by
   * the [Rpc_program.t] argument. (You can override the program and version
   * numbers stored in this argument by the optional parameters
   * [program_number] and [version_number]. If you need to call several
   * programs/versions with the same client, use [unbound_create] instead.)
   *
   * All communication to the server is handled using the given queue
   * [Unixqueue.event_system]. There is a limit of 2GB per message
   * or [Sys.max_string_length], whatever is lower.
   *
   * If the protocol (passed along with [mode2]) is Tcp, the communication 
   * will be handled stream-oriented. In this case, no timeout is detected
   * and no retransmissions are done.
   *
   * If the protocol is Udp, a datagram-oriented communication style is
   * used. This works only for Internet UDP sockets because these are
   * bidirectional (Unix domain sockets are unidirectional and do not
   * work). For Udp, there is a timeout of 15 seconds and a maximum
   * of 3 retransmissions (i.e. a total of 4 transmission trials).
   * For connected UDP sockets there is a limit of 64K per message
   * (max. size of an Internet packet). For unconnected UDP sockets
   * there is a limit of 16K per message due to restrictions in the
   * OCaml runtime.
   *
   *
   * @param program_number Overrides the program number in [Rpc_program.t]
   *
   * @param version_number Overrides the version number in [Rpc_program.t]
   *
   * @param initial_xid The initial value for the session identifier.
   *
   * @param shutdown This function is called when the client is shut down
   *   to close the client socket. By default, [shutdown_connector] is
   *   called.
   *
   *)

val unbound_create :
      ?initial_xid:int ->
      ?shutdown:(t -> Rpc_transport.rpc_multiplex_controller -> 
		   (unit -> unit) -> unit) ->
      mode2 ->
      Unixqueue.event_system ->
      t
  (** Creates an unbound client. This is like [create2], but the client is
      not restricted to a particular RPC program.

      One can convert an unbound client into a bound client by calling
      [bind], see below. It is possible to bind several times, so several
      programs can be called with the same client (provided the server is
      also capable of dealing with several programs).

      This function does not support [Portmapped] connectors.
   *)

val bind : t -> Rpc_program.t -> unit
  (** Binds this program additionally *)

val use : t -> Rpc_program.t -> unit
  (** If there are no bound programs, this is a no-op. Otherwise it is 
      checked whether the passed program is bound. If not, an exception
      is raised.

      Programs are compared by comparing {!Rpc_program.id}. The program
      must be the same value, but it is also allowed to 
      {!Rpc_program.update} it in the meantime, i.e. to change program
      and version numbers.
   *)

val configure : t -> int -> float -> unit
  (** [configure client retransmissions timeout]:
   * sets the number of retransmissions and the timeout for the next calls.
   * (These values are defaults; the actual values are stored with each
   * call.)
   *
   * Values of [retransmissions > 0] are semantically only valid if the
   * called procedures are idempotent, i.e. invoking them several times
   * with the same values has the same effect as only one invocation.
   * Positive values for [retransmissions] should only be used for Udp-style
   * communication.
   *
   * The timeout value determines how long the client waits until the
   * next retransmission is done, or, if no more retransmissions are
   * permitted, a [Message_timeout] exception is delivered to the receiving
   * callback function. A [timeout] value of 0.0 means immediate timeout
   * (see next paragraph). A negative [timeout] value means 'no timeout'.
   * Positive [timeout] values are possible for both Udp and Tcp connections.
   * Timeout values are measured in seconds.
   *
   * There is a special application for the timeout value 0.0: If you
   * don't expect an answer from the server at all ("batch mode"), this
   * timeout value will cause that the message handler will get
   * a [Message_timeout] exception immediately. You should ignore this
   * exception for batch mode. The positive effect from the timeout is that
   * the internal management routines will remove the remote call from
   * the list of pending calls such that this list will not become too long.
   * (You can get a similar effect by calling [set_batch_call], however.)
   *
   * Note that the meaning of timeouts for TCP connections is unclear.
   * The TCP stream may be in an undefined state. Because of this, the
   * client does not make any attempt to clean the state up for TCP.
   * The user is advised to shut down the client, and reconnect.
   *
   * There is another subtle difference between UDP and TCP. For UDP,
   * the timer is started when the packet is sent. For TCP, however,
   * the timer is already started when the RPC call is added to the
   * queue, i.e. much earlier. This means that the time for connecting
   * to the remote service is also bound by the timeout. The rationale
   * is that TCP timeouts are usually set to catch total service failures
   * rather than packet losses, and this behaviour is best for this purpose.
   *)

val configure_next_call : t -> int -> float -> unit
  (** Same as [configure], but it only affects the next call *)

val set_dgram_destination : t -> Unix.sockaddr option -> unit
  (** [set_dgram_destination client addr_opt]: This function is required
    * for using the client in conjunction with unconnected UDP sockets.
    * For connected sockets, the destination of datagrams is implicitly
    * given. For unconnected sockets, one has to set the destination
    * explicitly. Do so by calling [set_dgram_destination] with
    * [Some addr] as [addr_opt] argument before calling.
    * Passing [None] as [addr_opt] removes the explicit destination again.
    * Note that unconnected sockets differ from connected sockets also in
    * the relaxation that they can receive messages from any IP address,
    * and not only the one they are connected to.
    *
    * The current destination is used for all following calls. It is
    * not automatically reset to [None] after the next call.
   *)

val set_batch_call : t -> unit
  (** The next call will be a batch call. The client does not wait for the
      response of a batch call. Instead, the client immediately fakes the
      response of a "void" return value.

      It is required that the batch call has a "void" return type. Otherwise,
      the client raises an exception, and ignores the call.

      This setting only affects the next call.
   *)

val set_user_name : t -> string option -> unit
  (** Sets the user name, or [None] (the default user name). This is only
      meaningful for authentication.
   *)

val set_max_response_length : t -> int -> unit
  (** Sets the maximum length of responses. By default, there is only the
      implicit maximum of [Sys.max_string_length].

      If the maximum is exceeded, the exception [Response_dropped] is raised.
   *)

val set_exception_handler : t -> (exn -> unit) -> unit
  (** sets an exception handler (the default prints the exception 
   * with [`Crit] level to the logger set in {!Netlog}).
   * Only exceptions resulting from invocations of a
   * callback function are forwarded to this handler (unless wrapped
   * by [Unbound_exception]).
   *
   * Exceptions occuring in the handler itself are not caught, and will
   * fall through.
   *)

val set_mstring_factories : t -> Xdr_mstring.named_mstring_factories -> unit
  (** Sets the mstring factory configuration that is used for decoding
      responses containing managed strings.
   *)

val event_system : t -> Unixqueue.event_system
  (** Returns the unixqueue to which the client is attached *)

val programs : t -> Rpc_program.t list
  (** Returns the list of all bound programs *)

val get_socket_name : t -> Unix.sockaddr
val get_peer_name : t -> Unix.sockaddr
  (** Return the addresses of the client socket and the server socket, resp.
    * Note that these are only available when the client is already connected.
    * The function calls fail otherwise. It is also possible that the
    * underlying transport mechanism does not know these data.
   *)

val get_sender_of_last_response : t -> Unix.sockaddr
  (** Return the address of the sender of the last received response. *)

val get_xid_of_last_call : t -> Rtypes.uint4
  (** Returns the session identifier used in the just made call *)

val get_protocol : t -> Rpc.protocol
  (** Get the protocol flavour *)

val abandon_call : t -> Rtypes.uint4 -> unit
  (** To be used in conjunction with {!Rpc_client.Keep_call}: The call
      with this session identifier is no longer expected, and removed
      from the internal data structures.

      Restriction: for now, this does not work when there is authentication.
   *)

val is_up : t -> bool
  (** Return whether the client is up *)

val unbound_sync_call : 
      t -> Rpc_program.t -> string -> xdr_value -> xdr_value
  (** [unbound_sync_call client pgm proc arg]: Invoke the remote procedure
      [proc] of the program [pgm] via [client]. The input arguments are
      [arg]. The result arguments are returned (or an error is raised)
   *)

val unbound_async_call :
      t -> Rpc_program.t -> string -> xdr_value -> 
      ((unit -> xdr_value) -> unit) -> unit
  (** [unbound_ssync_call client pgm proc arg emit]: Invoke the remote 
      procedure
      [proc] of the program [pgm] via [client]. The input arguments are
      [arg]. When the result [r] is available, the client will call
      [emit (fun () -> r)] back. When an exception [e] is available, the
      client will call [emit (fun () -> raise e)] back.
   *)

class unbound_async_call :
      t -> Rpc_program.t -> string -> xdr_value -> [xdr_value] Uq_engines.engine
  (** Same as [unbound_async_call], but with an engine API. The engine
      is initially in state [`Working 0]. When the call is finished, the
      engine transitions to [`Done r] where [r] is the response value.
      If an error happens, it transitions to [`Error e] where [e] is the
      exception.

      One can [abort] the engine, but one caveat: This does not stop
      the transmission of the current message (the underlying RPC transporter
      doing this is not aborted). Aborting can only prevent that a
      message is sent before it is sent, and it can remove the call from the
      housekeeping data structures before the response arrives. Of course,
      one can shut the client down to achieve immediate stop of data
      transmission.
   *)

val synchronize : Unixqueue.event_system ->
                  ('a -> ((unit -> 'b) -> unit) -> unit) ->
                  'a -> 'b
  (** Turns an async call into a synchronous call *)


val shut_down : t -> unit
  (** Shuts down the connection. Any unprocessed calls get the exception
   * [Message_lost]. It is no error to shut down a client that is already
   * down - nothing happens in this case.
   *
   * Shutdowns can be complex operations. For this reason, this function
   * implements some magic that is usually the right thing, but may also
   * be wrong:
   *  - If called outside the event loop, it is assumed that a synchronous
   *    shutdown is desired, and the event loop is started to complete the
   *    shutdown immediately. This is right
   *    when the only task connected with the event loop is the shutdown,
   *    which is then done, and this function returns finally to the caller. If
   *    there are other tasks on the event loop, these tasks are also run,
   *    however, which may lead to side effects and infinite delay. This can
  *     be wrong.
   *  - If called from within the event loop, the shutdown is only triggered
   *    but not immediately done. When the caller returns to the event loop
   *    the shutdown will be performed. This case is problematic when you
   *    pass the file descriptor explicitly with [Descriptor] to the client.
   *    You don't know when the client is finally down, and the descriptor
   *    can be closed.
   *
   * The following functions allow more fine grained control of the shutdown.
   *)

val sync_shutdown : t -> unit
  (** Enforces a synchronous shutdown of the connection. This is only
    * possible if called from outside the event loop. This function fails
    * if called from within the event loop.
    *
    * You can be sure that the shutdown is completely done when this
    * function returns normally.
   *)

val trigger_shutdown : t -> (unit -> unit) -> unit
  (** Triggers the shutdown, and calls the passed function back when it is
    * done.
    *
    * The function is not only called when the client has to be taken
    * down, but also if the client is already down.
   *)


type reject_code =
    [ `Fail | `Retry | `Renew | `Next ]
  (** Reaction on authentication problems:
      - [`Fail]: Stop here, and report to user
      - [`Retry]: Just try again with current session
      - [`Renew]: Drop the current session, and get a new session from
        the current [auth_method]
      - [`Next]: Try the next authentication method
   *)
  

class type auth_session =
object
  method next_credentials : t ->
                            Rpc_program.t ->
                            string ->
                            uint4 ->
                            (string * string * string * string *
			       Xdr.encoder option *
			       Xdr.decoder option
			    )
         (** Called with [client prog proc xid].
	     Returns [(cred_flavour, cred_data, verifier_flavor, verifier_data,
	     enc_opt, dec_opt)].
	     
	     Changed in Ocamlnet-3.3: Additional arguments [prog], [proc],
	     [xid]. New return values [enc_opt] and [dec_opt].
	  *)
  method server_rejects : t -> uint4 -> server_error -> reject_code
         (** Called if the server rejects the credentials or the verifier
	  * (Auth_xxx). This method indicates how to react on errors.
	  * 
	  * Changed in Ocamlnet-3.3: Additional arg [xid]. New return value.
	  *)
  method server_accepts : t -> uint4 -> string -> string -> unit
         (** Called if the server accepts the credentials. The two strings
	  * are the returned [verifier_flavor] and [verifier_data].
	  * This method may raise [Rpc_server Rpc_invalid_resp] to indicate
	  * that the returned verifier is wrong.
	  * 
	  * Changed in Ocamlnet-3.3: Additional arg [xid]
	  *)
(*
  method drop_xid : t -> uint4 -> unit
    (** Called when the client drops this xid *)
  method drop_client : t -> unit
    (** Called when the client is closed *)
 *)
  method auth_protocol : auth_protocol
    (** Return the corresponding protocol *)
end
  (** An [auth_session] object is normally created for every client instance.
   * It contains the current state of authentication. The methods are only
   * interesting for implementors of authentication methods.
   *
   * This class type might be revised in the future to allow asynchronous
   * authentication (authentication often uses some network service).
   *)


and auth_protocol =
object
  method state : [ `Emit | `Receive of uint4 | `Done of auth_session | `Error]
    (** The state of the authentication protocol:
	- [`Emit]: The client needs to emit another token
	- [`Receive xid]: The client waits for another token (with
	  session identifier [xid])
	- [`Done session]: The protocol is finished and [session] can
	  be used for authenticating
	- [`Error]: Something went wrong.
     *)

  method emit : uint4 -> uint4 -> uint4 -> Rpc_packer.packed_value
    (** Emits a token for this [xid], [prog_nr] and [vers_nr]. 
	The returned packed value
	should have been created with {!Rpc_packer.pack_value}. It is 
	possible that [emit] is called several times with different
	xid values. In this case, the returned packed value should
	be identical except that the new xid is included in the message.

	After emission, the state must change to [`Receive].
     *)

  method receive : Rpc_packer.packed_value -> unit
    (** Receive a token for the [xid] announced in [state]. The passed
	packed value is the full RPC message. The message may also contain
	a server error - which may be processed by the protocol, or which
	may cause the reaction that [receive] raises an {!Rpc.Rpc_server}
	exception.

	After [receive], the state can change to [`Emit], [`Done] or
	[`Error]. The latter is obligatory when [receive] raises an
	exception. It is also possible not to raise an exception but
	silently switch to [`Error].

	Design limitation: there is right now no way to indicate that the
	next authentication method should be used instead.
     *)

  method auth_method : auth_method

end
  (** An authentication protocol is used for creating an authentication
      session.
   *)


and auth_method =
object
  method name : string
         (** The name of this method, used for errors etc. *)
  method new_session : t -> string option -> auth_protocol
         (** Request a new session. The 2nd argument is the user name, or [None]
	     if the default is to be used (whatever this is). Some
	     authenticators only support [None].

	     It is allowed that the returned [auth_protocol] object is already
	     in state [`Done], i.e. that actually no protocol is run.

	     Changed in Ocamlnet-3.3: different signature. The user name is
	     now an argument, and the method returns [auth_protocol] instead
	     of [auth_session]. There can now be a separate session for
	     each user (plus for the default user [None]).
	  *)
end
  (** An [auth_method] object represents a method of authentication. Such an
   * object can be shared by several clients.
   *)


val auth_none : auth_method
  (** The authentication method that does not perform authentication. *)


val set_auth_methods : t -> auth_method list -> unit
  (** Set the authentication methods for this client. The passed methods
   * are tried in turn until a method is accepted by the server.
   * The default is [ auth_none ]
   *
   * When the methods are set for an active client, the ongoing calls
   * are continued with the old method. First new calls are ensured to
   * use the new list.
   *)

(** This module type is what the generated "clnt" module assumes about the
    client interface
 *)
module type USE_CLIENT = sig
  type t
    (** The client type *)

  val use : t -> Rpc_program.t -> unit
    (** Announcement that this program will be used. The client may
        reject this by raising an exception.
     *)

  val unbound_sync_call : 
        t -> Rpc_program.t -> string -> xdr_value -> xdr_value
    (** [unbound_sync_call client pgm proc arg]: Invoke the remote procedure
        [proc] of the program [pgm] via [client]. The input arguments are
        [arg]. The result arguments are returned (or an error is raised)
     *)

  val unbound_async_call :
        t -> Rpc_program.t -> string -> xdr_value -> 
        ((unit -> xdr_value) -> unit) -> unit
    (** [unbound_ssync_call client pgm proc arg emit]: Invoke the remote 
        procedure
        [proc] of the program [pgm] via [client]. The input arguments are
        [arg]. When the result [r] is available, the client will call
        [emit (fun () -> r)] back. When an exception [e] is available, the
        client will call [emit (fun () -> raise e)] back.
     *)

end


(** {2 Deprecated Interfaces} *)


val create :
      ?program_number:uint4 ->
      ?version_number:uint4 ->
      ?initial_xid:int ->
      ?shutdown:(t -> Rpc_transport.rpc_multiplex_controller -> 
		   (unit->unit) -> unit) ->
      Unixqueue.event_system ->
      connector ->
      protocol ->
      Rpc_program.t ->
      t
  (** Opens a connection to the server specified by the [connector].
   * The server is assumed to implement an RPC program as specified by
   * the [Rpc_program.t] argument. (You can override the program and version
   * numbers stored in this argument by the optional parameters
   * [program_number] and [version_number].)
   *
   * All communication to the server is handled using the given queue
   * [Unixqueue.event_system].
   *
   * If the protocol is Tcp, the communication will be handled stream-
   * oriented. In this case, no timeout is detected and no retransmissions
   * are done.
   *
   * If the protocol is Udp, a datagram-oriented communication style is
   * used. This works only for Internet UDP sockets because these are
   * bidirectional (Unix domain sockets are unidirectional and do not
   * work). For Udp, there is a timeout of 15 seconds and a maximum
   * of 3 retransmissions (i.e. a total of 4 transmission trials).
   *
   * Unlike [create2], servers made with [create] always use blocking
   * [connect] for backwards compatibility.
   *
   * @deprecated This function should not be used any more in new programs.
   *    Use [create2] or [unbound_create].
   *
   * @param program_number Overrides the program number in [Rpc_program.t]
   *
   * @param version_number Overrides the version number in [Rpc_program.t]
   *
   * @param initial_xid The initial value for the session identifier.
   *
   * @param shutdown This function is called when the client is shut down
   *   to close the client socket. By default, [shutdown_connector] is
   *   called.
   *
   *)

val program : t -> Rpc_program.t
  (** Returns the program the client represents.

      @deprecated This is the same as [List.hd (Rpc_client.programs client)]
   *)

val add_call :
    t ->
    string ->
    xdr_value ->
    ((unit -> xdr_value) -> unit) ->
       unit
  (** [add_call client proc_name arg f]: add the call to the procedure [name]
    * with argument [arg] to the queue of unprocessed calls.
    *
    * When the reply has arrived or an error situation is detected, the
    * function [f] is called back. The argument of [f] is another function
    * that will return the result or raise an exception:
    *
    * {[ let my_f get_result =
    *      try
    *        let result = get_result() in
    *        ...
    *      with
    *         exn -> ...
    *    in
    *    add_call client name arg my_f
    * ]}
    *
    * If [f] does not catch the exception, the pluggable exception handler
    * of the client is called (see [set_exception_handler]). Exceptions are
    * either [Message_lost], [Message_timeout], or [Communication_error].
    *
    * The function [f] can raise the exception [Keep_call] to indicate
    * the special handling that a further reply of the call is expected
    * (batching).
    *
    * @deprecated [add_call] is restricted to the case that there is only
    *   one bound program. It will fail in other cases. Use 
    *   [unbound_async_call] instead. Note also that there is no longer
    *   the optional [when_sent] argument. Use [set_batch_call] instead
   *)

val sync_call :
    t ->            (* which client *)
    string ->       (* which procedure (name) *)
    xdr_value ->    (* the parameter of the procedure *)
    xdr_value       (* the result of the procedure *)
  (** Calls the procedure synchronously.
   * Note that this implies that the underlying unixqueue is started and that
   * all events are processed regardless of whether they have something to do
   * with this call or not.
   *
   * @deprecated [sync_call] is restricted to the case that there is only
   *   one bound program. It will fail in other cases. Use 
   *   [unbound_sync_call] instead.
   *)

val verbose : bool -> unit
  (** set whether you want debug messages or not (same as setting
      {!Rpc_client.Debug.enable})
   *)


(** {2 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Whether debug messages are enabled. 
        See {!Netlog.Debug} for more information.
     *)

  val enable_ptrace : bool ref
    (** Whether the procedure trace is enabled as debug messages.
        The procedure trace outputs for every RPC call and response
        a debug message. [ptrace_verbosity] says how verbose.
     *)

  val ptrace_verbosity : Rpc_util.verbosity ref
    (** How verbose the ptrace is. Defaults to [`Name_abbrev_args] *)

  val disable_for_client : t -> unit
    (** Disables all log messages for this client (internally used) *)

end
