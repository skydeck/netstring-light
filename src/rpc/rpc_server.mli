(* $Id: rpc_server.mli 1651 2011-08-03 16:38:17Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** RPC servers *)

(** Like the client, the RPC server module is programmed on top of the
 * Unixqueue event system. It pushes itself on an existing Unixqueue
 * as a new service that accepts RPC calls, forwards them to configurable
 * functions, and sends the replies back.
 *
 * The server module can manage two kinds of RPC functions: synchronous
 * and asynchronous. Synchronous functions compute their result immediately
 * and thus the result can be sent back just after the evaluation of the
 * function has finished. In contrast to this, asynchronous functions only
 * get noticed about the call and need not to know immediately what should
 * be answered. Typically, an asynchronous function initiates a second
 * communication channel and its result depends on what happens on the
 * second channel. The communication on this channel is done in an
 * asynchronous way, too, and can be managed by the same event system that
 * carries out the RPC service. After several input or output events,
 * the result has somehow been computed, and the answer can be sent
 * back to the original caller. To do so, the asynchronous RPC function
 * invokes 'reply' together with the necessary session IDs that identify
 * the answer among all answers.
 *)

open Rtypes
open Xdr
open Rpc

exception Connection_lost
  (** raised by the 'reply' function if the connection to the original caller
   * has been lost in the meantime.
   *)

type t
  (** represents a server for an RPC program *)

type session
  (** identifies a pair of a call and a reply *)

type connection_id
  (** identifies the connection of a session. For connectionless servers,
   * every session gets a new connection_id.
   * You can compare connection_ids to find out whether two sessions
   * belong to the same connection. Use "=" for equality.
   *)

type connector =
  | Localhost of int
	(** The service is installed on 'localhost' and listens on the
	 * given port number. A number of 0 means that the port is
	 * chosen by the operating system.
	 * Note: The service is only locally reachable.
	 *)
  | Portmapped
        (** The service is installed on every network interface; the port is
	 * chosen by the operating system; the program is registered with the
	 * portmapper
	 *)
  | Internet of (Unix.inet_addr * int)
        (** The service is installed on the passed interface/port combination.
	 * Use Unix.inet_addr_any to listen on all network interfaces.
	 * Use port 0 to automatically choose the port number.
	 *)
  | Unix of string
	(** The service is installed on a Unix domain socket.
	 * Note: the socket path must not exist when the server is started,
	 * and the socket must be unlinked when the server terminates.
         * Note Win32: Unix domain sockets are emulated by writing the
         * inet4 port number into a one-line file.
	 *)
  | W32_pipe of string
      (** The service is installed for a named pipe. (Only for Win32.) *)
  | Descriptor of Unix.file_descr
	(** The service listens on the given file descriptor. *)
  | Dynamic_descriptor of (unit -> Unix.file_descr)
	(** The service listens on the returned file descriptor. *)

type binding_sync =
    { sync_name : string;                  (** procedure name *)
      sync_proc : xdr_value -> xdr_value   (** the function that implements the
					    * procedure
					    *)
    }

type binding_async =
    { async_name : string;                 (** procedure name *)
      async_invoke : session -> xdr_value -> unit
	  (** A function that is called when the procedure is called *)
    }

type binding =
    Sync of binding_sync     (** bind a synchonous procedure *)
  | Async of binding_async   (** bind an asynchonous procedure *)


val connector_of_sockaddr : Unix.sockaddr -> connector
  (** Converts the socket address into a connector *)

val connector_of_socksymbol : Netsockaddr.socksymbol -> connector
  (** Converts the {!Netsockaddr.socksymbol} into a connector *)

val create :
    ?program_number:uint4 ->  (* Override program number *)
    ?version_number:uint4 ->  (* Override version number *)
    Unixqueue.event_system -> (* the underlying event queue *)
    connector ->           (* the address of the service *)
    protocol ->            (* Tcp: stream-oriented; Udp: datagram-oriented *)
    mode ->                (* Socket: serve multiple connections/datagrams;
			    * BiPipe: serve only a single connection
			    *)
    Rpc_program.t ->       (* The specification of the program *)
    binding list ->        (* The procedures *)
    int ->                 (* maximum number of waiting connections (backlog) *)
      t
  (** Deprecated creation of an RPC server. For new programs, use [create2]
    * or one of its variants.
    *
    * Creates a new server that is pushed onto the event queue.
    * The [connector], [protocol] and [mode] values control the network
    * type of the server. Note that not all combinations are valid; the
    * following can be used:
    * - any [connector], [protocol=Tcp], [mode=Socket]:
    *     creates a classic TCP server socket that allows multiple
    *     stream connections at the same time
    * - [connector=Descriptor s], [protocol=Tcp], [mode=BiPipe]:
    *     (where [s] is one half of a socketpair)
    *     creates a stream socket that is the endpoint of a point-to-point
    *     stream connection (bidirectional pipe)
    * - any Internet namespace connector, [protocol=Udp], [mode=Socket]:
    *     creates a UDP server socket that allows serving multiple datagrams
    *
    * Note: If [connector = Descriptor _] the file descriptor is not opened by
    * this module and not closed. The other [connector]s work automatically
    * regarding this point, i.e. descriptors are opened and closed as
    * necessary.
    *
    * [connector = Dynamic_descriptor]: The open descriptor is closed after use.
    *
    * The [Rpc_program.t] specifies the procedures that are available and
    * their signatures. The [binding list] should contain for every procedure
    * name the function that handles calls of the procedures.
    *
    * The remaining integer is the maximum number of waiting connections
    * if a classic Tcp server socket is used; other connection types ignore
    * this number.
    *
    * The optional arguments [?program_number] and [?version_number] override
    * the numbers specified in the passed program.
    *
    * {b Notes on servers:}
    * - servers that allow multiple connections never terminate by themselves
    * - servers for only one connection (endpoint of a bidirectional pipe)
    *   terminate if they see an EOF on the stream; in this case the stream
    *   is closed by the server
    * - the [create] function may block if the connector is Portmapped
    *
    * {b Note for UDP servers:} Due to limitations of the ocaml runtime
    * there is a limit of 16K per message.
    *)

class type socket_config =
object
  method listen_options : Uq_engines.listen_options
  method multiplexing : 
    close_inactive_descr:bool ->
    protocol -> Unix.file_descr -> Unixqueue.event_system ->
      Rpc_transport.rpc_multiplex_controller Uq_engines.engine
end

val default_socket_config : socket_config
class default_socket_config : socket_config

type mode2 =
    [ `Socket_endpoint of protocol * Unix.file_descr
    | `Multiplexer_endpoint of Rpc_transport.rpc_multiplex_controller
    | `Socket of protocol * connector * socket_config
    | `Dummy of protocol
    ]
  (** Determines the type of the server for [create2]:
    *
    * - [`Socket_endpoint(proto,fd)]: Socket [fd] is a connected socket
    *   descriptor used for communication. [proto] determines the 
    *   encapsulation; should be [Tcp] for stream sockets and [Udp] for
    *   datagram sockets.
    *
    * - [`Multiplexer_endpoint m]: [m] is an RPC multiplex controller.
    *
    * - [`Socket(proto, conn, config)]: Opens or uses a server socket 
    *   according to [conn]. [proto] determines the 
    *   encapsulation; should be [Tcp] for stream sockets and [Udp] for
    *   datagram sockets. [config] specifies configuration details.
    *
    * Despite their names, [`Socket_endpoint] and [`Socket] also support
    * Win32 named pipes.
   *)

val create2 : 
      mode2 ->
      Unixqueue.event_system ->
        t
   (** Creates a server according to the [mode2] argument. This kind of server
     * does initially not have any bindings.
    *)

val bind :
      ?program_number:uint4 ->  (* Override program number *)
      ?version_number:uint4 ->  (* Override version number *)
      Rpc_program.t ->
      binding list ->
      t -> 
        unit
  (** Binds the program as specified by the [binding list]. If the portmapper
    * must be informed, this action is started (and continued in the
    * background). One can bind several programs in several versions to the
    * same server.
   *)

val unbind :
      ?program_number:uint4 ->  (* Override program number *)
      ?version_number:uint4 ->  (* Override version number *)
      Rpc_program.t ->
      t -> 
        unit
  (** Unbinds the program if it is bound by the server *)

val bound_programs : t -> Rpc_program.t list
  (** Returns the bound programs *)

val get_event_system : session -> Unixqueue.event_system
  (** Find out the event system that contains the 'session' *)

val get_connection_id : session -> connection_id
  (** Get the connection_id *)

val get_xid : session -> Rtypes.uint4
  (** Returns the session ID.
   * Important note: This number identifies the session from the caller's
   * view, not from the server's view!
   *)

val get_socket_name : session -> Unix.sockaddr
val get_peer_name : session -> Unix.sockaddr
  (** Return the address of the socket serving the session, and the client
   * socket, resp. These functions fail if the server is not running on
   * a socket.
   *)

val get_conn_socket_name : connection_id -> Unix.sockaddr
val get_conn_peer_name : connection_id -> Unix.sockaddr
  (** Return the address of the socket serving the connection, and the client
   * socket, resp. These functions fail if the server is not running on
   * a socket.
   *)

val get_server : session -> t
  (** Returns the server instance of the session *)

val get_main_socket_name : t -> Unix.sockaddr
  (** Returns the address of the server socket, or the address of the
   * bidirectional pipe.
   * This function fails if the main file descriptor is not a socket.
   *)

val get_protocol : t -> protocol
  (** Return whether Tcp or Udp *)

val get_srv_event_system : t -> Unixqueue.unix_event_system
  (** Returns the event system *)

val get_last_proc_info : t -> string
  (** Get a debug string describing the last invoked procedure *)

val is_dummy : t -> bool
  (** Whether this is a server in [`Dummy] mode. These servers cannot be
      used for communication
   *)

type rule =
    [ `Deny
    | `Drop
    | `Reject
    | `Reject_with of Rpc.server_error
    | `Accept
    | `Accept_limit_length of (int * rule)
    ]
  (* similar to Rpc_transport.in_rule *)

val set_session_filter : t -> (Rpc_transport.sockaddr -> rule) -> unit
  (** If set, the filter function is invoked every time the beginning of a new
   * RPC call is received, and the result of the filter function determines
   * what to do with the call:
   *
   * `Deny: TCP connections are immediately closed; UDP packets are dropped
   * `Drop: The call is dropped (it does not allocate memory)
   * `Reject_with: A response is sent back that the call is rejected. The
   *   parameter specified the error code
   * `Reject: The same as [`Reject_with Rpc.Auth_too_weak]
   * `Accept: The call is accepted without limitation (the default if no
   *   filter is installed)
   * `Accept_limit_length(n,r): If the call is longer than n bytes, the rule
   *   r will be applied
   *
   * The parameter of the filter function is the socket address of the
   * client.
   *
   * The intention of filters is to prevent denial of service attacks.
   * A simple but good filter for TCP servers is
   *   set_filter srv (fun _ -> (`Accept_limit_length(n,`Deny))
   * which accepts messages up to n bytes without limit, and denies longer
   * messages. n is the length of the longest sensible message.
   *
   * For UDP servers, there is an implicit limit of 16K, so it is
   * not necessary to care about this.
   *
   * Another application is to restrict which systems can contact this
   * server, based on the IP address of the client.
   *
   * Note that this is not a protection against distributed denial of service
   * attacks.
   *)

val set_session_filter_2 : t -> (Rpc_transport.sockaddr -> connection_id -> rule) -> unit
  (** Same as [set_session_filter], but the filter gets as second argument the
    * connection ID.
   *)

val set_mstring_factories : t -> Xdr_mstring.named_mstring_factories -> unit
  (** Sets the mstring factories to use for decoding requests containing
      managed strings
   *)

val reply : session -> xdr_value -> unit
  (** Asynchronous procedures can reply their results with this function.
   *
   * NOTES:
   * - As with synchronous procedures, the transfer is not reliable since
   *   the connection may be broken at any time
   * - If it is already known that the connection is down, a Connection_lost
   *   exception is raised.
   * - If you don't want to reply to a certain call, just don't [reply].
   *   Unreplied calls do not allocate memory.
   * - It is possible to reply several times ("batch mode"), but the client
   *   must support it, too. Just call [reply] several times for the same
   *   session.
   *)

val reply_error : session -> Rpc.server_error -> unit
  (** Like [reply], but an error condition is sent back to the caller. *)

val set_exception_handler : t -> (exn -> unit) -> unit
  (** Sets the exception handler for the server.
   * The exception handler gets most exceptions raised by the functions that
   * are bound to procedures. The exception handler does not get Abort
   * exceptions and any exceptions resulting from I/O problems.
   *
   * NOTES ABOUT EXCEPTIONS:
   * - The default exception handler logs a [`Crit] message using {!Netlog}.
   * - I/O problems usually lead to an 'Abort' of the whole server.
   *)

val set_onclose_action : t -> (connection_id -> unit) -> unit
  (** Every time a connection is closed, the onclose function is called
   * with the closed connection.
   * The default onclose action is to do nothing. The function is also
   * called for [Descriptor] connectors when the socket should be closed
   * (for these connectors the socket is not closed by this module).
   *
   * Note that this action only applies to closed connections. It will
   * not be executed for closed sockets in general (closed master socket,
   * closed datagram socket).
   *
   * If several onclose actions are set, they will be executed in reverse
   * order.
   *)

val set_timeout : t -> float -> unit
  (** Sets the timeout for the transport. *)

val stop_server : ?graceful:bool -> t -> unit
  (** Stops the server: If a TCP server socket is listening, it is immediately
    * closed. The shutdown procedure for the connections is initiated.
    * Pending result messages are dropped.
    *
    * [graceful]: If true, the shutdown procedure is deferred until all
    * responses have been transferred back to the caller. This includes
    * any responses added to the message queue in the current callback.
    * New calls are not accepted.
   *)

val stop_connection : t -> connection_id -> unit
  (** Schedules a special event that causes the connection to be stopped in the
   * very near future. The function has only an effect for stream-oriented
   * servers (mode = Tcp). The connection socket will be closed (unless it
   * was passed using [Descriptor]). Nothing happens for datagram-oriented
   * servers (mode = Udp).
   *)


(************************* authentication **************************)

type auth_result =
    Auth_positive of (string * string * string * 
			Xdr.encoder option * Xdr.decoder option)
      (** Successful authentication:
          [(username, returned_verifier_flavour, returned_verifier_data, 
	    enc_opt, dec_opt
	  )]

	  Encoders and decoders are allowed to raise the exceptions
	  {!Rpc_server.Late_drop} and {!Rpc.Rpc_server}.
       *)
  | Auth_negative of Rpc.server_error
      (** Failed authentication *)
  | Auth_reply of (Xdr_mstring.mstring list * string * string)
      (** The authentication method generates the positive response
	  of this RPC call:
	  [(data, verf_flavor, verf_data)]
	  (new in Ocamlnet-3.3)
       *)
  | Auth_drop
      (** Authentication demands to drop the message *)


exception Late_drop
  (** This can be raised in encryption/decryption functions to prevent
      that a response is sent.
   *)

type auth_peeker =
    [ `None
    | `Peek_descriptor of Unix.file_descr -> string option
    | `Peek_multiplexer of Rpc_transport.rpc_multiplex_controller -> string option
    ]

class type auth_details =
object
  method server_addr : Unix.sockaddr option
  method client_addr : Unix.sockaddr option
  method program : uint4
  method version : uint4
  method procedure : uint4
  method xid : uint4
  method credential : string * string
  method verifier : string * string
  method frame_len : int
  method message : Rpc_packer.packed_value
end


class type auth_method =
object
  method name : string
    (** The name of the authentication method *)

  method flavors : string list
    (** Which credential flavors are handled by this method *)

  method peek : auth_peeker
    (** If available, this function is called for every accepted connection.
     * It may return the user name.
     * Notes:
     * - peeked user names override [authenticate]
     * - [peek] is only called once after the stream connection has been
     *   accepted
     *)

  method authenticate :
           t ->
	   connection_id ->
           auth_details ->
	   (auth_result -> unit) ->
	     unit
    (** [authenticate srv conn_id details f]:
     *	This method is called when a remote call has arrived. Its task is
     * to determine the client user and pass the user name (and the verifier)
     * back. If the user cannot be authenticated, the call must be rejected.
     * When the method has done the authentication, it calls the passed
     * function [f] with the [auth_result]. This function can be called
     * immediately or asynchronously.
     *
     * Changed in Ocamlnet-3.3: the arguments [program], [version],
     * [procedure], and [xid] are new. Added new [auth_result] of
     * [Auth_reply].
     *)
end

val set_auth_methods : t -> auth_method list -> unit
  (** Sets the available authentication methods.
   * By default, the list is set to [ auth_none ].
   * If none of the methods apply, the call is rejected (Auth_too_weak).
   *)

val auth_none : auth_method
  (** The authentication method "AUTH_NONE", i.e. no user name is passed.
   * The function [get_user] will return "".
   *)

val auth_too_weak : auth_method
  (** The method that always rejects. *)

val auth_transport : auth_method
  (** Authenticate by trusting the transport layer. The user returned by
    * the multiplexer's method peer_user_name is taken.
    *)

val get_user : session -> string
  (** Returns the user name as returned by the authentication method. See
   * the description of the method for the format of the user name string.
   *)

val get_auth_method : session -> auth_method
  (** Returns the method that was used to authenticate the user. *)

val verbose : bool -> unit
  (** {b Deprecated.}
      Set whether you want debug messages to stderr or not
   *)

val detach : t -> unit
  (** {b Internal function.} Cancels all pending I/O operations, and
      deallocates buffers. This function has only one purpose: The
      RPC servers inherited by a Netplex child process return memory.
      The RPC server is unusable after this.
   *)

module Debug : sig
  val enable : bool ref
    (** Whether debug messages of general kind are enabled. 
        See {!Netlog.Debug} for more information.
     *)

  val enable_ctrace : bool ref
    (** Whether debug messages are enabled that trace connection events.
        See {!Netlog.Debug} for more information.

        The "module name" for these messages is "Rpc_server.Ctrace".
     *)

  val enable_ptrace : bool ref 
    (** Whether the procedure trace is enabled.
        The procedure trace outputs for every RPC call and response
        a debug message. [ptrace_verbosity] says how verbose.
     *)

  val ptrace_verbosity : Rpc_util.verbosity ref
    (** How verbose the ptrace is. Defaults to [`Name_abbrev_args] *)

  val disable_for_server : t -> unit
    (** Disables logging for this server *)


end
