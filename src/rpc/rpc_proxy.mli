(* $Id: rpc_proxy.mli 1557 2011-03-04 14:43:05Z gerd $ *)

(** RPC proxies *)

(** The [Rpc_proxy] module provides an improved reliability layer on
    top of {!Rpc_client}. This layer especially features:
     - automatic connection management: TCP connections are started
       and terminated as needed
     - multiple connections can be held in parallel to a remote
       server to increase concurrency on the server
     - failover to other servers when the orignal servers time out
     - support for an initial ping at connection establishment time
       to test the availability of the connection
     - retransmission of idempotent RPC calls
 
    Proxies can only handle stream connections (TCP and Unix Domain).
    Also, the remote endpoints must already be specified by socket
    addresses. (No portmapper and other indirect lookup methods.)

    The proxy functionality is implemented in two layers, the managed
    clients, and the managed sets. The former layer can handle only
    one TCP connection (with reconnect), whereas the latter is able to
    manage a bunch of connections to the same service.  Both layers
    can profit from a reliability cache that knows which services had
    errors in the past.

    See below for a tutorial.

    There is also a blog article explaining RPC proxies:
    {{:http://blog.camlcity.org/blog/ocamlnet3_ha.html} The next server,
    please!}
 *)

module ReliabilityCache : sig
  (** The reliability cache stores information about the availability
      of remote servers. The managed clients put information about
      recent failures into the cache.

      It is advantegeous to have only one cache per process, because
      this maximizes the usefulness. The cache is thread-safe.

      A server endpoint is disabled when too many errors occur in
      sequence. For a disabled endpoint the functions [host_is_enabled]
      and/or [sockaddr_is_enabled] return [false]. The endpoint is
      automatically enabled again after some timeout; this is initially
      [disable_timeout_min], but is increased exponentially until
      [disable_timeout_max] when further errors occur.

      Independently of this machinery the functions [host_is_enabled]
      and [sockaddr_is_enabled] may also return [false] when an
      external availability checker says that the endpoint is down.
      This information is not entered into the cache, and will also
      not trigger the disable timeout. Instead, the hook function
      getting the availability will be simply called again.
   *)

  type rcache
    (** The cache *)

  type rcache_policy =
      [ `Independent
      | `Failing_port_disables_host of int
      | `Any_failing_port_disables_host
      | `None
      ]
    (** How failures of individual ports are interpreted:
        - [`Independent]: When a connection to a remote port repeatedly fails, 
          only this port is disabled
        - [`Failing_port_disables_host p]: When a connection to the TCP
          port [p] repeatedly fails, the whole remote host is disabled.
          Other ports do not disable the host, but are treated as in 
          [`Independent].
        - [`Any_failing_port_disables_host]: When a connection to any TCP
          port repeatedly fails, the whole remote host is disabled
        - [`None]: Nothing is disabled

        Note that the [rcache_availability] hook is not affected by the
        policy; this hook is called anyway. The policy only determines
        how the internal error counter is interpreted.
     *)

  type rcache_config =
      { rcache_policy : rcache_policy;   (** The policy, see above *)
	rcache_disable_timeout_min : float;  (** For how long ports and hosts 
                                             are disabled *)
	rcache_disable_timeout_max : float;  (** For how long ports and hosts 
                                             are disabled at most *)
	rcache_threshold : int;   (** How many errors are required 
                                      for disabling a port *)
	rcache_availability : rcache -> Unix.sockaddr -> bool;  (** External
            availability checker. Called  by [sockaddr_is_enabled] before
            the result is calculated *)
      }

  val create_rcache_config : ?policy:rcache_policy -> 
                             ?disable_timeout_min:float ->
                             ?disable_timeout_max:float ->
                             ?threshold:int ->
                             ?availability:(rcache -> Unix.sockaddr -> bool) ->
                             unit -> rcache_config
    (** Create a config record. The optional arguments set the config
        components with the same name. The arguments default to:
         - [policy = `None]
         - [disable_timeout_min = 1.0]
         - [disable_timeout_max = 64.0]
         - [threshold = 1]
         - [availability = fun _ _ -> true]
     *)

  val create_rcache : rcache_config -> rcache
    (** Creates a new cache object. The same cache can be used by several
        managed clients, even by totally unrelated ones
     *)

  val rcache_config : rcache -> rcache_config
    (** Return the config *)

  val global_rcache_config : unit -> rcache_config
    (** Returns the global config:
         - [policy = `None]
         - [disable_timeout_min = 1.0]
         - [disable_timeout_max = 64.0]
         - [threshold = 1]
         - [availability = fun _ _ -> true]
     *)

  val set_global_rcache_config : rcache_config -> unit
    (** Sets the new global config. This is only possible as long as
        neither [default_global_config] nor [global_rcache] have been called.
     *)

  val global_rcache : unit -> rcache
    (** The global cache. Initially, this cache has the default config.
        It is possible to change the default config before using the
        global cache for the first time.
     *)

  val derive_rcache : rcache -> rcache_config -> rcache
    (** [derive_cache parent config]: Returns a new cache that shares the
        error counters with [parent]. The interpretation of the counters,
        however, may be differently configured in [config].

        Because it is advantageous to share the error information as much
        as possible, the recommended way to create a new cache object is
        to derive it from the global cache.

        What [derive_rcache] actually does (and this is not yet
        optimal): Any [incr] and [reset] of an error counter is also
        forwarded to the parent cache. The tests whether hosts and ports
        are enabled do an AND of the results for the cache and its parent
        (i.e. both must be ok to enable). This allows some information
        sharing, but only in vertical direction.
     *)

  val incr_rcache_error_counter : rcache -> Unix.sockaddr -> unit
    (** Increase the error counter for this sockaddr. If the threshold
        is reached and there is a disable policy, the sockaddr will be disabled.

        This function is to be called after an RPC call times out, or
        runs into a socket error.
     *)

  val reset_rcache_error_counter : rcache -> Unix.sockaddr -> unit
    (** Reset the error counter for this sockaddr. If disabled, the
        sockaddr is set to enabled again.

        This function is to be called when an RPC call is successful.
     *)

  val sockaddr_is_enabled : rcache -> Unix.sockaddr -> bool
    (** Returns whether the sockaddr is enabled. This also calls the
        [rcache_availability] hook.
     *)

  val host_is_enabled : rcache -> Unix.inet_addr -> bool
    (** Returns whether the host is enabled *)

end

module ManagedClient : sig
  (** Managed clients are {!Rpc_client} clients with the ability to
      reconnect in the case of errors. 

      Additional features:
       - they can also be disabled, either based on a  time criterion or
         a customizable hook. This encodes the assumption that failing
         servers need some time to recover
       - unused connections are closed (driven by a timeout)
       - support for the initial ping after establishing the connection

      Initial pings are useful to test whether the connection is
      really working. Servers normally accept new TCP connections without
      knowing whether there are resources for processing the connections
      (i.e. whether there is a process or thread waiting for serving
      it). Because of this, the client cannot assume that the TCP
      connection is really up only because the [connect] system call
      said the connection is there. The initial ping fixes this problem:
      The null procedure is once called after the TCP connection has
      been established. Only when this works the client believes the
      connection is really up. It is required that [mclient_programs]
      is configured with at least one program, and this program must
      have a procedure number 0 of type [void -> void].

      In multi-threaded programs, threads must not share managed clients.

      Managed clients can be used together with ocamlrpcgen-generated
      modules. Provided the generated module [M_clnt] contains the
      client code for program [P] and version [V], one can do

      {[
         module MC = M_clnt.Make'P(Rpc_proxy.ManagedClient)
      ]}

      and call RPCs [f] as in

      {[
         let res = MC.V.f mc arg
      ]}

      (if [mc] is the managed client, and [arg] the argument).
   *)
 
  type mclient
    (** A managed client *)

  type mclient_config =
      { mclient_rcache : ReliabilityCache.rcache;  (** The rcache *)
	mclient_socket_config : Rpc_client.socket_config;
	  (** The socket configuration *)
	mclient_idle_timeout : float;
	  (** After how many seconds unused connections are closed.
              A negative value means never. 0 means immediately. A positive
              value is a point in time in the future.
	   *)
	mclient_programs : Rpc_program.t list;
	  (** The programs to bind *)
	mclient_msg_timeout : float;
	  (** After how many seconds the reply must have been arrived.
              A negative value means there is no timeout. 0 means immediately. 
              A positive
              value is a point in time in the future.
	   *)
	mclient_msg_timeout_is_fatal : bool;
	  (** Whether a message timeout is to be considered as fatal error
              (the client is shut down, and the error counter for the endpoint
              is increased)
	   *)
	mclient_exception_handler : (exn -> unit) option;
	  (** Whether to call {!Rpc_client.set_exception_handler} *)
	mclient_auth_methods : Rpc_client.auth_method list;
	  (** Set these authentication methods in the client *)
	mclient_user_name : string option;
	  (** The user name for authentication, None = default user *)
	mclient_initial_ping : bool;
	  (** Whether to call procedure 0 of the first program after
              connection establishment (see comments above)
	   *)
	mclient_max_response_length : int option;
	  (** The maximum response length. See 
              {!Rpc_client.set_max_response_length}.
	   *)
	mclient_mstring_factories : Xdr_mstring.named_mstring_factories option
	  (** The factories to use for decoding managed strings *)
      }

  exception Service_unavailable
    (** Procedure calls may end with this exception when the reliability
        cache disables the service
     *)

  val create_mclient_config : ?rcache:ReliabilityCache.rcache ->
                              ?socket_config:Rpc_client.socket_config ->
                              ?idle_timeout:float ->
                              ?programs:Rpc_program.t list ->
                              ?msg_timeout:float ->
                              ?msg_timeout_is_fatal:bool ->
                              ?exception_handler:(exn -> unit) ->
                              ?auth_methods:Rpc_client.auth_method list ->
                              ?user_name:string option ->
                              ?initial_ping:bool ->
                              ?max_response_length:int ->
                              ?mstring_factories:Xdr_mstring.
                                                    named_mstring_factories ->
                              unit -> mclient_config
                                        
    (** Create a config record. The optional arguments set the config
        components with the same name. The defaults are:
         - [rcache]: Use the global reliability cache
         - [socket_config]: {!Rpc_client.default_socket_config}
         - [programs]: The empty list. It is very advisable to fill this!
         - [msg_timeout]: (-1), i.e. none
         - [msg_timeout_is_fatal]: false
         - [exception_handler]: None
         - [auth_methods]: empty list
         - [user_name]: None
         - [initial_ping]: false
         - [max_response_length]: None
         - [mstring_factories]: None
     *)

  val create_mclient : mclient_config -> 
                       Rpc_client.connector ->
                       Unixqueue.event_system ->
                         mclient
    (** Create a managed client for this config connecting to this
        connector. Only [Internet] and [Unix] connectors are supported.
     *)

  type state = [ `Down | `Connecting | `Up of Unix.sockaddr option]
      (** The state:
           - [`Down]: The initial state, and also reached after a socket
             error, or after one of the shutdown functions is called.
             Although [`Down], there might still some cleanup to do.
             When RPC functions are called, the client is automatically
             revived.
           - [`Connecting]: This state is used while the initial ping is
             done. It does not reflect whether the client is really 
             TCP-connected. Without initial ping, this state cannot occur.
           - [`Up s]: The client is (so far known) up and can be used.
             [s] is the socket address of the local socket
       *)

  val mclient_state : mclient -> state
    (** Get the state *)

  val mclient_serial : mclient -> int
    (** Get the serial number of the connection. The serial number is
	increased when the client is reconnected. If the client is down
	the serial number of the next connection attempt is returned.
     *)

  val pending_calls : mclient -> int
    (** Returns the number of pending calls *)

  val event_system : mclient -> Unixqueue.event_system
    (** Return the event system *)

  val shut_down : mclient -> unit
  val sync_shutdown : mclient -> unit
  val trigger_shutdown : mclient -> (unit -> unit) -> unit
    (** Shut down the managed client. See the corresponding functions
        {!Rpc_client.shut_down}, {!Rpc_client.sync_shutdown}, and
        {!Rpc_client.trigger_shutdown}
     *)

  val record_unavailability : mclient -> unit
    (** Increases the error counter in the reliability cache for this
        connection. The only effect can be that the error counter
        exceeds the [rcache_threshold] so that the server endpoint
        is disabled for some time. However, this only affects new 
        connections, not existing ones.

        For a stricter interpretation of errors see 
        [enforce_unavailability].

        The error counter is increased anyway when a socket error
        happens, or an RPC call times out and [msg_timeout_is_fatal]
        is set. This function can be used to also interpret other
        misbehaviors as fatal errors.
     *)
    (* This is a strange function. Maybe it should go away. One could
       call it after a successful RPC call when the result of this call
       indicates that the server is not good enough for further use
       (although it is still able to respond). However, after a successful
       RPC the error counter is reset, and this cannot be prevented by
       this function (too late)
     *)

  val enforce_unavailability : mclient -> unit
    (** Enforces that all pending procedure calls get the
        [Service_unavailable] exception, and that the client is shut down.
        The background is this: When the reliability cache discovers an
        unavailable port or host, only the new call is stopped with this
        exception, but older calls remain unaffected. This function
        can be used to change the policy, and to stop even pending calls.

        The difference to [trigger_shutdown] is that the pending RPC
        calls get the exception [Service_unavailable] instead of
        {!Rpc_client.Message_lost}, and that it is enforced that the
        shutdown is recorded as fatal error in the reliability cache.
     *)

  val set_batch_call : mclient -> unit
    (** The next call is a batch call. See {!Rpc_client.set_batch_call} *)

  val rpc_engine : mclient -> 
                   (mclient -> 'a -> ((unit -> 'b) -> unit) -> unit) ->
                   'a ->
                     'b Uq_engines.engine
    (** Call an RPC function in engine style:

        {[ let e = rpc_engine mc f_rpc ]}

        where [f_rpc] is one of the generated client functions (async
	signature). The engine reaches [`Done r] when the result [r]
	has arrived.

	The engine is not abortable (shut the client down instead).
     *)

  val compare : mclient -> mclient -> int
    (** [ManagedClient] can be used with [Set.Make] and [Map.Make] *)

  include Rpc_client.USE_CLIENT with type t = mclient
    (** We implement the [USE_CLIENT] interface for calling procedures *)
end

module ManagedSet : sig
  (** Manages a set of clients *)

  type mset
    (** a managed set *)

  type mset_policy =
      [ `Failover | `Balance_load ]
	(** Sets in which order managed clients are picked from the 
            [services] array passed to [create_mset]:
             - [`Failover]: Picks an element from the first service
               in [services] that is enabled and has free capacity.
               That means that the first service is preferred until it is
               maxed out or it fails, then the second service is preferred,
               and so on.
             - [`Balance_load]: Picks an element from the service in 
               [services] that is enabled and has the lowest load.
	 *)

  type mset_config =
      { mset_mclient_config : ManagedClient.mclient_config;  
	   (** The mclient config *)
	mset_policy : mset_policy;
           (** The policy *)
	mset_pending_calls_max : int;
	  (** When an mclient processes this number of calls at the same time,
              it is considered as fully busy. (Value must by > 0).
	   *)
	mset_pending_calls_norm : int;
	  (** When an mclient processes less than this number of calls,
              its load is considered as too light, and it is tried to put
              more load on this client before opening another one
	   *)
	mset_idempotent_max : int;
	  (** How often idempotent procedures may be tried to be called.
              A negative value means infinite.
	   *)
        mset_idempotent_wait : float;
          (** Wait this number of seconds before trying again *)
      }

  exception Cluster_service_unavailable
    (** Raised by [mset_pick] when no available endpoint can be found,
        or all available endpoints have reached their maximum load.
     *)

  val create_mset_config : ?mclient_config:ManagedClient.mclient_config ->
                           ?policy:mset_policy ->
                           ?pending_calls_max:int ->
                           ?pending_calls_norm:int ->
                           ?idempotent_max:int ->
                           ?idempotent_wait:float ->
                           unit -> mset_config
    (** Create a config record. The optional arguments set the config
        components with the same name. The defaults are:
         - [mclient_config]: The default mclient config
         - [policy]: [`Balance_load]
         - [pending_calls_max]: [max_int]
         - [pending_calls_norm]: 1
         - [idempotent_max]: 3
         - [idempotent_wait]: 5.0
     *)

  val create_mset : mset_config ->
                    (Rpc_client.connector * int) array ->
                    Unixqueue.event_system ->
                    mset
    (** [create_mset config services]: The mset is created with [config],
        and the [services] array describes which ports are available,
        and how often each port may be contacted (i.e. max number of
        connections).
     *)

  val mset_pick : ?from:int list -> mset -> ManagedClient.mclient * int
    (** Pick an mclient for another call, or raise [Cluster_service_unavailable].
        The returned int is the index in the [mset_services] array.

        If [from] is given, not all specified mclients qualify for this
        call. In [from] one can pass a list of indexes pointing into
        the [mset_services] array, and only from these mclients the 
        mclient is picked. For [`Failover] policies, the order given
        in [from] is respected, and the mclients are checked from left
        to right.
     *)

  val mset_services : mset -> (Rpc_client.connector * int) array
    (** Returns the service array *)

  val mset_load : mset -> int array
    (** Returns the number of pending calls per service *)

  val event_system : mset -> Unixqueue.event_system
    (** Return the event system *)

  val shut_down : mset -> unit
  val sync_shutdown : mset -> unit
  val trigger_shutdown : mset -> (unit -> unit) -> unit
    (** Shut down the managed set. See the corresponding functions
        {!Rpc_client.shut_down}, {!Rpc_client.sync_shutdown}, and
        {!Rpc_client.trigger_shutdown}
     *)

  val idempotent_async_call :
       ?from:int list -> 
       mset -> 
       (ManagedClient.mclient -> 'a -> ((unit -> 'b) -> unit) -> unit) ->
       'a ->
       ((unit -> 'b) -> unit) -> 
         unit
    (** [idempotent_async_call
           mset async_call arg emit]: Picks a new
        [mclient] and calls [async_call mclient arg emit].
        If the call leads to a fatal error, a new [mclient]
        is picked, and the call is repeated. In total, the call may be
        tried [mset_idempotent_max] times. It is recommended to set
        [rcache_threshold] to 1 when using this function because this
        enforces that a different mclient is picked when the first one
        fails.

        Note that a timeout is not considered as a fatal error by default;
        one has to enable that by setting [mclient_msg_timeout_is_fatal].

        Note that this form of function is compatible with the 
        generated [foo'async] functions of the language mapping.

        [from] has the same meaning as in [mset_pick].
     *)

  val idempotent_sync_call :
       ?from:int list -> 
       mset -> 
       (ManagedClient.mclient -> 'a -> ((unit -> 'b) -> unit) -> unit) ->
       'a ->
       'b
    (** Synchronized version. Note that you have to pass an asynchronous
        function as second argument. The result is synchronous, however.
     *)

end


(**
    {1:tut The [Rpc_proxy] tutorial}

    {2:mclient Managed clients}

    A normal RPC client has a very limited lifecylce: It is created,
   then a connection is made to an RPC service, messages are exchanged,
   and finally the connection is terminated. After that the client
   becomes unusable. In short, it is "use once" client.

   In contrast to this, managed clients can be recycled. This is
   especially useful for dealing with socket errors, and 
   connection terminations triggered by the RPC server.

   {b How to use managed clients:} For a {i normal} RPC client the
   generator [ocamlrpcgen] creates all required glue code to easily
   start RPC calls. For example, if a file [proto.x] is taken as input
   for [ocamlrpcgen], a piece of code doing a call could look like:

   {[ 
      let client =
        Proto_clnt.PROG.VERS.create_client connector protocol
      let result =
        Proto_clnt.PROG.VERS.procedure client argument
   ]}

   (Here, [PROG], [VERS], [procedure] are just placeholders for the
   name of the program, the version identifier, and the procedure name.)

   For RPC proxies, however, this is slightly more complicated. [ocamlrpcgen]
   does not produce a managed client that is ready for use. Instead,
   only a functor is provided that can take the
   {!Rpc_proxy.ManagedlClient} module as input:

   {[
      module M = Proto_clnt.Make'PROG(Rpc_proxy.ManagedClient)

      let esys =
        Unixqueue.create_unix_event_system()
      let mclient_config =
        Rpc_proxy.ManagedClient.create_mclient_config
          ~programs:[ Proto_clnt.PROG.VERS._program ]
          () in
      let mclient =
        Rpc_proxy.ManagedClient.create_mclient mclient_config connector esys
      let result =
        M.VERS.procedure mclient argument
   ]}

   (The functor approach has been chosen, because it gives the
   user more flexibility - it is possible to apply the functor
   on other implementations of improved clients than 
   {!Rpc_proxy.ManagedClient}.)

   Note that [esys] is always explicit, even in the case the
   user only performs synchronous calls - the user should create
   a new [esys] then, pass it to [mclient], and ignore it otherwise.

   Now, how does the recycling feature work? The managed client can be
   in one of three states:
    - [`Down]: The client is not connected. This is the initial state,
      and the state after errors and terminated connections (no matter
      whether triggered by the client or by the server)
    - [`Connecting]: The client is busy (re)connecting (only used in
      some cases)
    - [`Up sockaddr]: The client is connected and has the socket address
      [sockaddr]

   The state can be queried with {!Rpc_proxy.ManagedClient.mclient_state}.
   When it is [`Down], the next RPC call automatically starts the
   reconnect to the service. When the connection is established, the
   call is done, and the messages are exchanged that are representing
   the call. After that, the state remains [`Up] after the call.

   When the call stops because of an error, the error is reported to
   the user in the normal way, and the client is shut down, i.e. after
   an error the state is [`Down]. If the user decides to try the call
   again, the client automatically reconnects following the outlined
   rules. Note that managed clients never automatically retry calls
   by themselves.

   When the TCP connection is regularly shut down (either by the server
   or by the client calling {!Rpc_proxy.ManagedClient.shut_down}), the
   client state is changed to [`Down] at the next opportunity. Especially
   a server-driven shutdown may first be detected when the next RPC call
   is tried on the connection. This may or may not lead to an error 
   depending on the exact timing. In any way, the connection is finally
   established again.

   Of course, managed clients must be shut down after use, because
   there is no other (automatic) way of recognizing that they are no
   longer used. Call {!Rpc_proxy.ManagedClient.shut_down} for this.

   Managed client also have a few more features that can be
   enabled in [mclient_config], especially:
    - {b Initial ping}: This means that the TCP connection is tested
      before being used for user operations. The test is done by pinging
      the service once (via the RPC null procedure). This is recommended
      because some connectivity problems can first be detected when the
      TCP connection is actually used.
    - {b Idle timeout}: The TCP connection is closed after it is
      idle for some period of time. "Idle" means here that nothing is
      being transmitted, and that no response from the server is expected.
      The connection is closed at the first opportunity. The user should
      be aware that this can only happen when the event loop for [esys]
      is running. Especially for synchronous calls this is typically
      not the case, so one would have to call [Unixqueue.run esys] now 
      and then to create opportunities for detecting the idle timeout.
    - {b Reliability cache}: The cache object counts errors, and can
      disable certain service endpoints if they only produce errors.
      This mostly makes sense when there are alternative endpoints,
      i.e. in the context of a managed set (see below).
 *)

(** {2 Managed Sets}

    Managed sets are another layer on top of the managed
    clients. These sets are able to manage several connections where
    each is implemented as managed client. The connections can go to
    the same server endpoint in order to parallelize RPCs at the
    client side, or to several server endpoints that provide the same
    service.  The latter can be used for client-driven load balancing,
    and for client-driven failover management of HA setups (HA = high
    availability).

    For creating a managed set, the code looks like

    {[
      module M = Proto_clnt.Make'PROG(Rpc_proxy.ManagedClient)

      let esys =
        Unixqueue.create_unix_event_system()
      let mclient_config =
        Rpc_proxy.ManagedClient.create_mclient_config
          ~programs:[ Proto_clnt.PROG.VERS._program ]
          () in
      let mset_config =
        Rpc_proxy.ManagedSet.create_mset_config
          ~mclient_config
          () in
      let services =
        [| connector, n_connections; ... |] in
      let mset =
        Rpc_proxy.ManagedSet.create_mset 
          mset_config 
          services
          esys in
      let mclient, idx =
        Rpc_proxy.ManagedSet.mset_pick mset in
      let result =
        M.VERS.procedure mclient argument
    ]}

    The managed clients are internally created by the set - one
    only should pass in [mclient_config] so the set knows what kind of
    client is preferred. For the simple application of maintaining
    several connections to the same server, one would create the [mset]
    with a one-element service array:

    {[
       let services =
          [| connector, n_connections |]
    ]}

    where [connector] describes the server port, and [n_connections] is
    the maximum number of connections to create and maintain. 
    The {!Rpc_proxy.ManagedSet.mset_pick}
    function creates internally up to [n_connections] managed clients,
    and returns one of them. By default, it is not guaranteed that the
    client is idle (meaning no previous call is pending)  - 
    if the connections are all already busy, [mset_pick]
    starts returning busy connections (but the least busy one first).

    There are a number of options allowing to modify the default
    behavior:
     - One can enforce that only idle clients are returned by [mset_pick].
       To do this, pass the argument [~mset_pending_calls_max:1] to
       {!Rpc_proxy.ManagedSet.create_mset_config}. It can then happen
       that no client is idle, and [mset_pick] will raise
       {!Rpc_proxy.ManagedSet.Cluster_service_unavailable}.
     - If the [services] array has more than one element, they are
       considered as equivalent service endpoints. [mset_pick] will
       pick one of the endpoints. There are two policies controlling
       the selection: With [~policy:`Balance_load] it is aimed at
       sending roughly the same number of calls to all endpoints. With
       [~policy:`Failover] the services are assigned precedences by the position
       in the array (i.e. the first service is used as long as possible,
       then the second service is used, etc.). The [policy] argument
       is again to be passed to {!Rpc_proxy.ManagedSet.create_mset_config}.

   Of course, managed sets must be shut down after use, because
   there is no other (automatic) way of recognizing that they are no
   longer used. Call {!Rpc_proxy.ManagedSet.shut_down} for this.

   {2 Caching reliability data}

   The cache allows to disable certain hosts or ports when the error
   counter reaches a limit. The service is disabled for a limited time span.
   This is especially useful when there is an alternate port that can
   jump in for the failing one, i.e. when the [services] array of a
   managed set has two or more elements.

   There is a single global cache object, but one can also create
   specific cache objects. Generally, cache objects can be shared by
   many managed clients and managed sets. The hope is that sharing
   is useful because more data can be made available to users of
   services. If you do not want to use the global cache object, you
   can create your own, and configure it in [mclient_config].

   The global cache object is automatically used when nothing else
   is specified. The global cache object is by default configured in
   a way so it does not have any effect, though. So we have to
   change this in order to enable the cache:

   {[
     let rcache_config =
       Rpc_proxy.ReliabilityCache.create_rcache_config
        ~policy:`Independent
        ~threshold:3
        () in
     Rpc_proxy.ReliabilityCache.set_global_rcache_config rcache_config
   ]}

   This means that 3 errors in sequence disable a service port. [`Independent]
   means that each port is handled independently in this respect.

    At the first time, the port is only disabled for one second. The
    duration of the time span is increased by each additional error
    until it reaches 64 seconds. These durations can be changed, of
    course.

    As the impact of changing the global cache object is sometimes
    unpredictable, one can also create a private cache object
    ({!Rpc_proxy.ReliabilityCache.create_rcache}). Another way is
    to derive a semi-private object from the global one. This means
    that the error counters are global, but the interpretation can
    be set individually in each use. This would look like:

    {[
    let rcache_config =
      Rpc_proxy.ReliabilityCache.create_rcache_config
        ~policy:`Independent
        ~threshold:3
        () in
    let rcache =
      Rpc_proxy.ReliabilityCache.derive_rcache
        (Rpc_proxy.ReliabilityCache.global_rcache())
        rcache_config in
    ...
    let mclient_config =
      Rpc_proxy.ManagedClient.create_mclient_config
        ...
        ~rcache
        ...
        ()
    ]}

  {2 Idempotent calls}

    In the layer of managed sets there is some limited support for
    automatically repeating failing idempotent RPC calls.

    Instead of calling the RPC with

    {[
      let mclient, idx =
        Rpc_proxy.ManagedSet.mset_pick mset in
      let result =
        M.VERS.procedure mclient argument
    ]}

    one uses

    {[
      let result =
        Rpc_proxy.ManagedSet.idempotent_sync_call
          mset
          M.VERS.procedure'async
          argument
    ]}

    The effet is that {!Rpc_proxy.ManagedSet.idempotent_sync_call}
    repeats automatically the call when an error occurs. It is
    assumed that the call is idempotent so it can be repeated
    without changing the meaning.

    The call may be repeated several times. This is configured in
    the managed set [mset] (parameter [mset_idempotent_max]).

    Note that one has to pass the asynchronous version (suffix ['async])
    of the RPC wrapper even when doing a synchronous call.

    Also see the documentation for
    {!Rpc_proxy.ManagedSet.idempotent_async_call}.

 *)
