(* $Id: netplex_types.mli 1718 2012-02-21 14:59:45Z gerd $ *)

(** Types for [Netplex] *)


type encap = Netplex_encap.encap

type param_value =
    [ `String of string
    | `Int of int
    | `Float of float
    | `Bool of bool
    ]

type param_value_or_any =
    [ param_value
    | `Any of exn
    | `Encap of encap
    ]

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]
  (** Log levels, modeled after syslog *)

(** A logger receives log messages *)
class type logger =
object
  (** A logger receives log messages *)

  method log_subch : component:string -> subchannel:string -> 
                     level:level -> message:string -> unit
    (** Receive a log [message] of [level] from [component]. The component
        string is the name of the socket service emitting the message.
        Optionally, one can specify a [subchannel] when a single component
        needs to write to several log files. For example, a [subchannel]
        of ["access"] could select the access log of a webserver.
        The main log file is selected by passing the empty string as
        [subchannel].
     *)

  method log : component:string -> level:level -> message:string -> unit
    (** Same as [log_subch] when [subchannel] is set to the empty string.
        This means that the message is sent to the main log file of the
        component.
     *)

  method reopen : unit -> unit
    (** Reopen the log files *)
end

type parallelization_type =
    [ `Multi_processing
    | `Multi_threading
    | `Controller_attached
    ]
  (** Type of parallelization:
    * - [`Multi_processing] on a single host
    * - [`Multi_threading] on a single host
    * - [`Controller_attached] means that the service runs within the 
    *   controller. This is (for now) only allowed for controller-internal
    *   services.
   *)

type thread_sys_id =
    [ `Thread of int | `Process of int ]
  (** A system-specific identifier of the thread/process *)

type socket_state =
    [ `Enabled | `Disabled | `Restarting of bool | `Down ]
  (** The state of a socket:
    * - [`Enabled]: The controller allows containers to accept connections.
    *   Note that this does not necessarily means that there such containers.
    * - [`Disabled]: It is not allowed to accept new connections. The
    *   socket is kept open, however.
    * - [`Restarting b]: The containers are being restarted. The boolean
    *   argument says whether the socket will be enabled after that.
    * - [`Down]: The socket is down/closed
   *)

type container_id = < socket_service_name : string >
  (** Such objects identify containers. As additional info, the method
      [socket_service_name] returns the name of the socket service the
      container implements
   *)

type container_state =
    [ `Accepting of int * float
    | `Busy
    | `Starting of float
    | `Shutting_down
    ]
  (** The container state for workload management:
    * - [`Accepting(n,t)]: The container is accepting further connections.
    *   It currently processes [n] connections. The last connection was
    *   accepted at time [t] (seconds since the epoch).
    * - [`Busy]: The container does not accept connections
    * - [`Starting t]: The container was started at time [t] and is not
    *   yet ready.
    * - [`Shutting_down]: The container is being shutted down.
   *)

type capacity =
    [ `Normal_quality of int * bool
    | `Low_quality of int * bool
    | `Unavailable
    ]
  (** How many connections a container can accept in addition to the
    * existing connections:
    * - [`Normal_quality(n,greedy)]: It can accept n connections with normal
    *   service quality, [n > 0]
    * - [`Low_quality(n,greedy)]: It can accept n connections with low
    *   service quality (e.g. because it is already quite loaded), [n > 0]
    * - [`Unavailable]: No capacity free
    *
    * The [greedy] flag sets whether greedy accepts are allowed.
   *)

type extended_address =
    [ `Socket of Unix.sockaddr
    | `Socket_file of string
    | `W32_pipe of string
    | `W32_pipe_file of string
    | `Container of string * string * string * [ thread_sys_id | `Any ]
    ]
  (** Possible addresses:
       - [`Socket s]: The socket at this socket address
       - [`Socket_file f]: The file [f] contains the (anonymous) port number
         of a socket bound to [127.0.0.1] (This is meant as substitute for
         Unix Domain sockets on Win32.)
       - [`W32_pipe name]: The Win32 pipe with this [name] which must
         be of the form "\\.\pipe\<pname>"
       - [`W32_pipe_file f]: The file [f] contains the (random) name of a 
         Win32 pipe
       - [`Container(socket_dir,service_name,proto_name,thread_id)]: 
         The special endpoint
         of the container for [service_name] that is running as [thread_id].
         It is system-dependent what this endpoint "is" in reality, usually
         a socket or named pipe. If any container of a service is meant,
         the value [`Any] is substituted as a placeholder for a not yet
         known [thread_id].
   *)

(** The controller is the object in the Netplex master process/thread
    that manages the containers, logging, and service definitions
 *)
class type controller = 
object
  method ptype : parallelization_type
    (** The actually effective parallelization type *)

  method sys_id : thread_sys_id
    (** The thread running the controller *)

  method controller_config : controller_config

  method services : (socket_service * socket_controller * workload_manager) list
    (** The list of controlled services *)

  method add_service : socket_service -> workload_manager -> unit
    (** Adds a new service. Containers for these services will be started
      * soon. It is allowed to add several services with the same name
      * (but it will be hard to distinguish them later).
     *)

  method add_message_receiver : ctrl_message_receiver -> unit
    (** Adds a message receiver. This receiver runs in the context of the
        controller and receives all messages sent to it. The [name] method
        must return the name.
     *)

  method add_plugin : plugin -> unit
    (** Adds a plugin. If the plugin object has already been added, this
        is a no-op.

        Plugins must have been added before the first container is started.
        This is not checked, however. You are on the safe side when the
        plugin is added in the [create_processor] factory method, or in
        the [post_add_hook] of the processor.
     *)

  method add_admin : (Rpc_server.t -> unit) -> unit
    (** [add_admin setup]: Allows to bind another RPC program to the admin
      * socket. The function [setup] will be called whenever a connection
      * to the admin socket is established, and this function can call
      * [Rpc_server.bind] to bind another RPC program. By default, only
      * the [Admin] interface is available as described in [netplex_ctrl.x].
      *
      * Note that this RPC server runs in the scope of the controller! No
      * additional process or thread is created.
     *)

  method logger : logger
    (** The logger *)

  method event_system : Unixqueue.unix_event_system
    (** The event system used by the controller. It {b must not} be used
     * from a container.
     *)

  method restart : unit -> unit
    (** Initiates a restart of all containers: All threads/processes are
      * terminated and replaced by newly initialized ones.
     *)

  method shutdown : unit -> unit
    (** Initiates a shutdown of all containers. It is no longer possible
      * to add new services. When the shutdown has been completed, 
      * the controller will terminate itself. Note that the shutdown is
      * performed asynchronously, i.e. this method returns immediately,
      * and the messaging required to do the shutdown is done in the 
      * background.
     *)

  method send_message : string -> string -> string array -> unit
    (** [send_message destination msgname msgargs]: Sends a message to
        [destination]. When this method returns, it is only ensured that
        the receivers registered in the controller have been notified about
        the message (so it can be made sure that any newly forked containers
        know about the message). It is not guaranteed that the existing
        containers are notified when this method returns. This can (and
        usually will) happen at any time in the future.
     *)

  method send_admin_message : string -> string -> string array -> unit
    (** [send_message destination msgname msgargs]: Sends an admin message to
        [destination].
 
        See [send_message] for the notification guarantees.
     *)

  method register_lever : (controller -> encap -> encap) -> int
    (** [let id = register_lever f]: It is possible to register a function [f]
        in the controller, and run it over the internal RPC interface from
        any container. These functions are called levers. See
        [activate_lever] below. See also
        {!Netplex_cenv.Make_lever} for a convenient way to create
        and use levers.
     *)

  method activate_lever : int -> encap -> encap
    (** Runs the registered lever directly *)

  method containers : container_id list
    (** Lists the containers *)

  method containers_for : string -> container_id list
    (** Lists the containers for a certain socket service name *)

  method container_count : string -> int
    (** The number of containers for a certain socket service name *)

  method free_resources : unit -> unit
    (** Should be called when the controller is finished, in order to
        free resources again. E.g. plugins are unplugged, and the master
        sockets are closed.
     *)

  method startup_directory : string
    (** The current directory at startup time *)

end

and controller_config =
object
  method socket_directory : string
    (** The directory where Unix domain sockets are created. For every
      * service a subdirectory is created, and the socket has the name
      * of the protocol.
      *
      * This is always an absolute path, even if it is only given as
      * relative path in the config file.
     *)

  method create_logger : controller -> logger
    (** Create a logger to be used for the whole Netplex system. The
      * controller is already initialized which makes it possible to
      * write the logger as Netplex service. Messages arriving during the
      * creation are queued up and sent afterwards to the new logger.
     *)

  method max_level : level
    (** Return the maximum global log level *)

  method set_max_level : level -> unit
    (** Set the maximum global log level *)
end
	  
and socket_service =
object
  method name : string
    (** The name of the [socket_service] is used to identify the service
      * in the whole netplex process cluster. Names are hierarchical;
      * name components are separated by dots (e.g. "company.product.service").
      * The prefix "netplex." is reserved for use by Netplex. The name
      * "netplex.controller" refers to the service provided by the
      * controller.
     *)

  method sockets : (string * Unix.file_descr array) list
    (** A [socket_service] consists of a list of supported protocols
      * which are identified by a name. Every protocol is available 
      * on a list of sockets (which may be bound to different addresses).
      * The sockets corresponding to [`Container] addresses are missing
      * here.
     *)

  method socket_service_config : socket_service_config
    (** The configuration *)

  method processor : processor
    (** A user-supplied object to process incoming connections *)

  method shutdown : unit -> unit
    (** Shuts down the master sockets *)

  method create_container : parallelization_type -> socket_service -> container
    (** {b Internal method.} Called by the controller to create a new
      * container. The container must match the parallelization type of
      * the controller. This call is already done in the process/thread
      * provided for the container.
      *)

  method on_add : controller -> unit
    (** Get some runtime configuration aspects from this controller. This
	is called when the socket service is added to the controller
     *)

  method startup_directory : string
    (** The current directory at Netplex startup time (same view as controller)
     *)
end

and socket_service_config =
object
  method name : string
    (** The proposed name for the [socket_service] *)

  method protocols : protocol list
    (** This list describes the sockets to create in detail *)

  method change_user_to : (int * int) option
    (** Instructs the container to change the user of the process after
      * starting the service. This is only possible in multi-processing mode.
      * In multi-threading mode, this parameter is ignored.
     *)

  method startup_timeout : float
    (** After this many seconds the container must have finished the
        [post_start_hook]. It is usually 60 seconds.
     *)

  method conn_limit : int option
    (** An optional limit of the number of connections this container
	can accept. If the limit is reached, the container will not
	accept any further connections, and shut down when all connections
	are processed.
     *)

  method gc_when_idle : bool
    (** If set, idle containers run a [Gc.full_major] cycle. *)

  method controller_config : controller_config
    (** Make this config accessible here too, for convenience *)
end

and protocol =
object
  method name : string
    (** The protocol name is an arbitrary string identifying groups of
      * sockets serving the same protocol for a [socket_service].
     *)
  method addresses : extended_address array
    (** The addresses of the master sockets. (The socket type is always
      * SOCK_STREAM.) The list must be non-empty.
     *)
  method lstn_backlog : int
    (** The backlog (argument of Unix.listen) *)
  method lstn_reuseaddr : bool
    (** Whether to reuse ports immediately *)
  method so_keepalive : bool
    (** Whether to set the keep-alive socket option *)
  method tcp_nodelay : bool
    (** Whether to set the TCP_NODELAY option *)
  method configure_slave_socket : Unix.file_descr -> unit
    (** A user-supplied function to configure slave sockets (after [accept]).
      * The function is called from the process/thread of the container.
     *)
end

and socket_controller =
object
  method state : socket_state
    (** The current state *)
  method enable : unit -> unit
    (** Enables a disabled socket service again *)
  method disable : unit -> unit
    (** Disable a socket service temporarily *)
  method restart : unit -> unit
    (** Restarts the containers for this socket service only *)
  method shutdown : unit -> unit
    (** Closes the socket service forever, and initiates a shutdown of all
      * containers serving this type of service.
     *)
  method container_state : (container_id * string * container_state * bool) list
    (* (cid, par_info, cstate, selected)
     * par_info: the info_string of the par_thread
     * selected: says whether the container is selected to accept the
     * next connection
     *)

  method start_containers : int -> int
    (* Arg: #containers to start. Return val: actual started #containers *)

  method stop_containers : container_id list -> unit

end

and ctrl_message_receiver =
object
  method name : string
    (** The name of this receiver *)

  method receive_message :
            controller -> string -> string array -> unit
    (** This function is called when a broadcast message is received.
      * The first string is the name of the message, and the array are
      * the arguments.
     *)

  method receive_admin_message :
            controller -> string -> string array -> unit
    (** This function is called when a broadcast admin message is received.
      * The first string is the name of the message, and the array are
      * the arguments.
     *)
end

(** Processor hooks can be used to modify the behavior of a processor.
    See {!Netplex_intro.servproc} for some documentation about the hooks.
 *)
and processor_hooks =
object
  method post_add_hook : socket_service -> controller -> unit
    (** A user-supplied function that is called after the service has been
      * added to the controller 
     *)

  method post_rm_hook : socket_service  -> controller -> unit
    (** A user-supplied function that is called after the service has been
      * removed from the controller 
     *)

  method pre_start_hook : socket_service -> controller -> container_id -> unit
    (** A user-supplied function that is called before the container is
      * created and started. It is called from the process/thread of the
      * controller.
     *)

  method post_start_hook : container -> unit
    (** A user-supplied function that is called after the container is
      * created and started, but before the first service request arrives.
      * It is called from the process/thread of the
      * container.
     *)

  method pre_finish_hook : container -> unit
    (** A user-supplied function that is called just before the container is
      * terminated. It is called from the process/thread of the
      * container.
     *)

  method post_finish_hook : socket_service -> controller -> container_id -> unit
    (** A user-supplied function that is called after the container is
      * terminated. It is called from the process/thread of the
      * controller.
     *)

  method workload_hook : container -> bool -> int -> unit
    (**  A user-supplied function that is called when the workload
	 changes, i.e. a new connection has been accepted, or an
	 existing connection could be completely processed.
	 The [bool] argument is [true] if the reason is a new
	 connection. The [int] argument is the number of connections.
	 This function is called from the process/thread of the container.
     *)

  method receive_message :
            container -> string -> string array -> unit
    (** This function is called when a broadcast message is received.
      * The first string is the name of the message, and the array are
      * the arguments.
     *)

  method receive_admin_message :
            container -> string -> string array -> unit
    (** This function is called when a broadcast admin message is received.
      * The first string is the name of the message, and the array are
      * the arguments.
     *)

  method system_shutdown : unit -> unit
    (** A user-supplied function that is called when a system shutdown
      * notification arrives. This notification is just for information
      * that every container of the system will soon be shut down. The
      * system is still completely up at the time this notification 
      * arrives, so if the services of other components are required to
      * go down this is the right point in time to do that (e.g. send
      * important data to a storage component).
     *)

  method shutdown : unit -> unit
    (** A user-supplied function that is called when a shutdown notification
      * arrives. That means that the container should terminate ASAP.
      * There is, however, no time limitation. The termination is started
      * by calling the [when_done] function passed to the [process] method.
     *)

  method global_exception_handler : exn -> bool
    (** This method is called when an uncaught exception would otherwise
      * terminate the container. It can return [true] to indicate that
      * the container continues running.
     *)

  method container_event_system : unit -> Unixqueue.event_system
    (** This method is called to get the event systems for containers.
	This is normally a {!Unixqueue.standard_event_system}, but
	users can override it.
     *)

  method container_run : Unixqueue.event_system -> unit
    (** [container_run esys]: By default, it just runs [esys#run()].
	This method is called to run the event system of the containers.
	Users can override it.
     *)

end

(** The processor is the object that is notified when a new TCP connection
    is accepted. The processor has to include the protocol interpreter that
    reads and write data on this connection. See {!Netplex_intro.defproc}
    for an example how to define a processor.
 *)
and processor =
object
  inherit processor_hooks

  method process : 
           when_done:(unit -> unit) ->
           container -> Unix.file_descr -> string -> unit
    (** A user-supplied function that is called when a new socket connection
      * is established. The function can now process the requests arriving
      * over the connection. It is allowed to use the event system of the
      * container, and to return immediately (multiplexing processor). It is 
      * also allowed to process the requests synchronously and to first return
      * to the caller when the connection is terminated. 
      *
      * The function {b must} call [when_done] to indicate that it processed
      * this connection completely.
      *
      * The string argument is the protocol name.
     *)

  method supported_ptypes : parallelization_type list
    (** The supported parallelization types *)

end

(** Containers encapsulate the control flow of the service components.
    A container is run in a separate thread or process.

    {b Thread safety:} All methods except [start] can be called from
    any thread, and provide full thread safety.
 *)
and container =
object
  method socket_service_name : string
  method socket_service : socket_service

  method container_id : container_id
    (** Return the container ID *)

  method ptype : parallelization_type
    (** The parallelization type actually used for this container *)

  method event_system : Unixqueue.unix_event_system
    (** The event system the container uses *)

  method start : Unix.file_descr -> Unix.file_descr -> unit
    (** {b Internal Method.} Called by the controller to start the container.
      * It is the responsibility of the container to call the 
      * [post_start_hook] and the [pre_finish_hook].
      *
      * The file descriptors are endpoints of RPC connections to the
      * controller. The first serves calls of the [Control] program,
      * and the second serves calls of the [System] program.
      *
      * When [start] returns the container will be terminated.
     *)

  method shutdown : unit -> unit
    (** Initiates a shutdown of the container. *)

  method n_connections : int
    (** The current number of connections *)

  method n_total : int
    (** The sum of all connections so far *)

  method system : Rpc_client.t
    (** An RPC client that can be used to send messages to the controller.
      * Only available while [start] is running. It is bound to 
      * [System.V1].
      *
      * In multi-threaded programs access to [system] must be governed
      * by [system_monitor]. See {!Uq_mt} for details what this means.
     *)

  method system_monitor : Uq_mt.monitor
    (** The thread monitor protecting the [system] RPC client *)

  method lookup : string -> string -> string option
    (** [lookup service_name protocol_name] tries to find a Unix domain
      * socket for the service and returns it.
     *)

  method lookup_container_sockets : string -> string -> string array
    (** [lookup_container_sockets service_name protocol_name]: returns
      the Unix Domain paths of all container sockets for this service and
      protocol. These are the sockets declared with address type
      "container" in the config file.
   *)

  method owned_container_sockets : (string * string) list
    (** List of pairs [(protocol_name, path)] of all container sockets
        of this container
     *)

  method send_message : string -> string -> string array -> unit
    (** [send_message service_pattern msg_name msg_arguments]: Sends
        a message to all services and message receivers matching
        [service_pattern]. The pattern may include the wildcard [*].

        See the {!Netplex_types.controller.send_message} method for
        the notification guarantees.
     *)

  method log : level -> string -> unit
    (** Sends a log message to the controller. *)

  method log_subch : string -> level -> string -> unit
    (** Sends a log message to the controller. The first string is the
        subchannel
     *)

  method update_detail : Unix.file_descr -> string -> unit
    (** Update the detail string output for the [netplex.connections]
        admin message
     *)

  method var : string -> param_value_or_any
    (** Returns the value of a container variable or [Not_found]. Container
      * variables can be used by the user of a container to store additional
      * values in the container. These values exist once per thread/process.
      *)

  method set_var : string -> param_value_or_any -> unit
    (** Sets the value of a container variable *)

  method call_plugin : plugin -> string -> Xdr.xdr_value-> Xdr.xdr_value
    (** [call_plugin p procname procarg]: This method can be called
        from the container context to invoke the plugin [p] procedure
        [procname]. This means that the [ctrl_receive_call] of the
        same plugin is invoked in the controller context.
     *)

  method activate_lever : int -> encap -> encap
    (** Runs a lever function registered in the controller. The [int]
        argument identifies the lever. The [encap] argument is the parameter,
        and the returned exception is the result. See also
        {!Netplex_cenv.Make_lever} for a convenient way to create
        and use levers.
     *)

  method startup_directory : string
    (** The current directory at Netplex startup time (same view as controller)
     *)
end

(** See {!Netplex_workload} for definitions of workload managers *)
and workload_manager =
object
  method hello : controller -> unit
    (** Called by the controller when the service is added *)

  method shutdown : unit -> unit
    (** Called by the controller to notify the manager about a shutdown *)

  method adjust : socket_service -> socket_controller -> unit
    (** This function is called by the controller at certain events to
      * adjust the number of available containers. The manager can
      * call [start_containers] and [stop_containers] to change the
      * system.
      *
      * The function is called right after the startup to ensure
      * that there are containers to serve requests. It is also called:
      * - just after a connection has been accepted and before it is
      *   decided which container will have the chance to accept in the
      *   round
      * - after the shutdown of a container
      *
      * Of course, the workload manager is free to adjust the load
      * at any other time, too, not only when [adjust] is called.
     *)

  method capacity : container_id -> container_state -> capacity
    (** Computes the capacity, i.e. the number of jobs a certain container
      * can accept in addition to the existing load.
     *)

end

and plugin =
object
  method program : Rpc_program.t
    (** The RPC program structure on which the messaging bases. The program,
        version and procedure numbers are ignored
     *)

  method ctrl_added : controller -> unit
    (** This method is invoked when the plugin has been added to this
        controller. Note that plugins can be added to several controllers.
     *)

  method ctrl_unplugged : controller -> unit
    (** The plugin has been unplugged from this controller *)

  method ctrl_receive_call : 
            controller -> container_id -> string -> Xdr.xdr_value -> 
            (Xdr.xdr_value option -> unit) ->
              unit
    (** [ctrl_receive_call ctrl cid procname procarg emit]:
        This method is called in the controller context [ctrl] when a procedure
        named [procname] is called. In [procarg] the argument of the
        procedure is passed. [cid] is the container ID from where the
        call originates. To pass the result [r] of the call back to the caller,
        is is required to call [emit (Some r)] (either immediately, or at
        some time in the future). By calling [emit None], an error condition
        is propagated back to the caller.
     *)

  method ctrl_container_finished : controller -> container_id -> bool -> unit
    (** This method is called when a container finishes 
        (after [post_finish_hook]).
        The boolean is true if the container is the last of the terminated
        socket service.
 *)
end
  (** Plugins are extensions of the Netplex system that run in the controller
      and can be invoked from containers
   *)
;;


class type par_thread =
object
  method ptype : parallelization_type

  method sys_id : thread_sys_id
    (** Returns a system-dependent identifier for the thread:
     * - [`Thread id]: The [id] as returned by [Thread.id]
     * - [`Process id]: The [id] is the process ID
     *)

  method info_string : string
    (** Outputs the process or thread ID *)

  method watch_shutdown : Unixqueue.unix_event_system -> unit
    (** Called by the controller if it thinks the container is down.
      * {b This method must not be called outside the internal Netplex
      * implementation!}
     *)

  method parallelizer : parallelizer
    (** Returns the parallelizer that created this thread. Can be used
      * to start another thread of the same type. 
     *)
end


and parallelizer =
object
  method ptype : parallelization_type

  method init : unit -> unit
    (** Initializes the main process for usage with this parallelizer.
      * {b This method must not be called outside the internal Netplex
      * implementation!}
     *)

  method start_thread : 
         (par_thread -> unit) -> 
         Unix.file_descr list -> 
         Unix.file_descr list -> 
         string -> 
         logger -> 
           par_thread
    (** [start_thread f l_close l_share name logger]: 
      * Starts a new thread or process and calls
      * [f thread] in that context. Before this is done, file descriptors
      * are closed, controlled by the parameters [l_close] and [l_share].
      * The descriptors in [l_close] are always closed. The descriptors
      * in [l_share] are not closed. The implementation of the parallelizer
      * is free to close a reasonable set of descriptors, and [l_close]
      * is the minimum, and [all - l_share] is the maximum.
      *
      * There is no way to check when the thread terminates.
      *
      * It is allowed that the [par_thread] object passed to [f] is a different
      * object as the returned [par_thread] object.
     *)

  method create_mem_mutex : unit -> ( (unit -> unit) * (unit -> unit) )
    (** [let lock, unlock = par#create_mem_locker()]: Creates a mutex that
      * is sufficient to protect process memory from uncoordinated access.
      * The function [lock] obtains the lock, and [unlock] releases it.
     *)

  method current_sys_id : [ `Thread of int | `Process of int ]
    (** Returns the system-dependent thread identifier of the caller *)
end


type config_tree =
    [ `Section of string * config_tree list
	(* (relative_name, contents) *)
    | `Parameter of string * param_value
	(* (relative_name, contents) *)
    ]

and address = < >

class type config_file =
object
  method filename : string
  method tree : config_tree
  method root_addr : address
  method root_name : string
  method resolve_section : address -> string -> address list
    (* Fails if the address cannot be found. Returns [] if there is no
     * such section at this address
     *)
  method resolve_parameter : address -> string -> address
    (* Fails if the address cannot be found. Raises Not_found if there is no
     * such parameter at this address
     *)
  method print : address -> string
  method string_param : address -> string
  method int_param : address -> int
  method float_param : address -> float
  method bool_param : address -> bool
  method restrict_subsections : address -> string list -> unit
  method restrict_parameters : address -> string list -> unit
end


class type processor_factory =
object
  method name : string
  method create_processor :
    controller_config -> config_file -> address -> processor
end


class type workload_manager_factory =
object
  method name : string
  method create_workload_manager : 
    controller_config -> config_file -> address -> workload_manager
end


class type logger_factory =
object
  method name : string
  method create_logger : config_file -> address -> controller -> logger
end


class type netplex_config =
object
  method ptype : parallelization_type

  method controller_config : controller_config

  method services : (socket_service_config * 
		       (address * processor_factory) * 
		       (address * workload_manager_factory) ) list
end

