(* $Id: netplex_main.mli 1496 2010-11-27 21:49:12Z gerd $ *)

(** Main program for Netplex servers *)

open Netplex_types

type cmdline_config

val args : 
       ?defaults:cmdline_config ->
       unit -> ((Arg.key * Arg.spec * Arg.doc) list * cmdline_config)
  (** [let (opt_list, cmdline_cfg) = args()]:
    * Returns [opt_list] for inclusion in the [Arg.parse] option list.
    * The effects made available by the returned [cmdline_cfg] value.
    *
    * The defaults (unless overridden by [defaults]): The config file
    * is derived from the name of the executable (by appending [.conf]
    * instead of the current extension). There is no config tree.
    * 
    * @param defaults The default argument values
   *)

val create : ?config_filename:string ->
             ?config_tree:config_tree ->
             ?pidfile:string option ->
             ?foreground:bool ->
             unit -> cmdline_config
  (** Creates the command-line configuration object.

      By setting [config_tree] a special configuration can be injected -
      the file [config_filename] is not loaded in this case (but may
      still appear in error messages)
   *)

val modify : ?config_filename:string ->
             ?config_tree:config_tree ->
             ?pidfile:string option ->
             ?foreground:bool ->
             cmdline_config -> cmdline_config
  (** Modifies the command-line configuration object *)

val config_filename : cmdline_config -> string
  (** Returns the filename of the configuration file, or the default
      if it has not been set on the command-line
   *)

val config_filename_opt : cmdline_config -> string option
  (** Returns the filename of the configuration file, or [None] if it
      has not been set on the command-line
   *)

val config_tree_opt : cmdline_config -> config_tree option
  (** Returns the tree of the configuration file, or [None] if it
      has not been set by [create] or [modify]. Note that [args]
      never sets the config tree.
   *)

val pidfile : cmdline_config -> string option
  (** Returns the location of the PID file (if any) *)

val foreground : cmdline_config -> bool
  (** Returns whether the daemon runs in the foreground *)

val startup : 
      ?late_initializer:(config_file -> controller -> unit) ->
      ?config_parser:(string -> config_file) ->
      parallelizer ->
      logger_factory list ->
      workload_manager_factory list ->
      processor_factory list -> 
      cmdline_config -> 
        unit
  (** Establishes a configuration and starts the Netplex daemon.
    *
    * If a ready-made configuration tree is included in the passed configuration
    * it is taken as the configuration. If otherwise only the name of the
    * config file is available, this file is parsed.
    * Fails with [Netplex_config.Config_error] when an error in the
    * configuration is detected.
    *
    * The [late_initializer] is called after the Netplex controller has been
    * fully initialized, and before the main event loop is entered. You can
    * perform here further initializations, e.g. starting helper threads.
    *
    * The [config_parser] is by default [Netplex_config.read_config_file].
    * You can override it by whatever parser you would like to use. The
    * parser is only used if a configuration tree is unavailable, and
    * the configuration needs to be loaded from a file.
    *
    * As side-effect, the current logger of the {!Netlog} module is set
    * to selected Netplex logger. Note that this setting remains active
    * even after [startup] returns to the caller. Also note that log messages
    * submitted via {!Netlog} appear as from component "netplex.other"
    * if they are sent from outside of a container.
   *)


(** {2 Tutorial}
  *
  * The typical main program for a [Netplex] server system looks like:
  *
  * {[ 
  *   let my_factories = ...
  *
  *   let start() =
  *    let opts, cmdconf = Netplex_main.args() in
  *    Arg.parse 
  *      opts
  *      (fun s -> raise(Arg.Bad ("Unknown arg: " ^ s))) 
  *      "usage: protoserver";
  *    let par = Netplex_mp.mp() in  (* or Netplex_mt.mt() *)
  *    Netplex_main.startup
  *      par
  *      Netplex_log.logger_factories
  *      Netplex_workload.workload_manager_factories
  *      my_factories
  *      cmdconf
  *
  *   Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  *   start()
  * ]}
  *
  * This main program enables:
  * - The standard command-line arguments [-conf], [-pid] and [-fg] are 
  *   understood
  * - The configuration file is parsed
  * - The configuration can refer to all loggers and workload managers
  *   coming with Netplex
  * - The parallelizer is selected: Here, it is multi-processing
  *   (Netplex_mp). One could also select multi-threading (Netplex_mt)
  * - The processors defined by [my_factories] are made available
  *   for connection processing
 *)
