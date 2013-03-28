(* $Id: netplex_controller.mli 1496 2010-11-27 21:49:12Z gerd $ *)

(** Controller *)

(** The controller is the main part of the Netplex system that starts and
  * stop the individual service containers.
 *)

open Netplex_types

val create_controller : parallelizer -> controller_config -> controller
  (** Create a controller with the default event system *)

val create_controller_for_esys : 
      Unixqueue.event_system -> parallelizer -> controller_config -> controller
  (** Create a controller for the passed event system *)

val extract_config : 
  logger_factory list -> config_file -> controller_config
  (** Extracts the controller config *)

(** {1 Configuring the controller} 

    There is a [controller] section that may configure the controller:

    {[ netplex {
         ...
         controller {
           socket_directory = "/var/run/myapp_sockets";
           max_level = "debug";
           logging { ... };
         }
         ...
       }
    ]}

    The [socket_directory] overrides the location where the special
    directory with Unix Domain sockets and other runtime files is
    created.

    The [max_level] parameter defines the global maximum log level.
    It defaults to "info". By setting it to "debug" (as shown) debug
    logging is enabled. This parameter can be modified at runtime.

    The [logging] section is explained in {!Netplex_log}.

 *)


module Debug : sig
  val enable : bool ref
end
