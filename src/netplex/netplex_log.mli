(* $Id: netplex_log.mli 1692 2012-02-05 18:44:00Z gerd $ *)

(** Loggers *)

(** A logger is usually configured in the config file. The [logging]
    section is inside [controller], e.g.

    {[ netplex {
         ...
         controller {
           ...
           logging { ... };
           ...
         }
         ...
       }
    ]}

    The possible types of [logging] sections are explained below.
    If there are several [logging] sections, the messages are sent to
    all configured loggers.

    Instead of configuring loggers in the config file, one can also
    create loggers from config objects like [generic_config].

    See also {!Netplex_intro.logging} for more documentation.
 *)

open Netplex_types

class type generic_config =
object
  method log_format : string
  method log_component : string
  method log_subchannel : string
  method log_max_level : level
end


val channel_logger : out_channel -> logger
  (** Outputs messages to the channel *)

val channel_logger_from_obj : generic_config -> out_channel -> logger
  (** Outputs messages to the channel *)

val stderr_logger_factory : logger_factory
  (** Reads a logging section like
    *
    * {[ logging {
    *       type = "stderr";
    *       format = "<format string>";
    *       component = "<name_of_component>";
    *       subchannel = "<name_of_subchannel>";
    *       max_level = "<max_level>";
    *    }
    * ]}
    *
    * - [format]: Optional format string. See below.
    * - [component]: Optional component selector. See below.
    * - [subchannel]: Optional subchannel selector. See below.
    * - [max_level]: Optional maximum log level. See below.
   *)


val file_logger : string -> logger
  (** Writes messages to this file *)

val file_logger_from_obj : generic_config -> string -> logger
  (** Writes messages to this file *)

val file_logger_factory : logger_factory
  (** Reads a logging section like
    *
    * {[ logging {
    *       type = "file";
    *       file = "/path/to/logfile";
    *       format = "<format string>";
    *       component = "<name_of_component>";
    *       subchannel = "<name_of_subchannel>";
    *       max_level = "<max_level>";
    *    }
    * ]}
    *
    * - [file]: Log messages are appended to this file.
    * - [format]: Optional format string. See below.
    * - [component]: Optional component selector. See below.
    * - [subchannel]: Optional subchannel selector. See below.
    * - [max_level]: Optional maximum log level. See below.
   *)

class type multi_file_config =
object
  inherit generic_config
  method log_directory : string
  method log_files :
    (string * string * [ level | `All ] * string * string) list
    (** Triples [ (component, subchannel, max_level, file, format) ]. Use [*] as 
        wildcard in [component] and [subchannel].

        Currently, [`All] is a synonym for the [`Debug] level.
      *)
end

val multi_file_logger : multi_file_config -> logger

val multi_file_logger_factory : logger_factory
  (**  Reads a logging section like
    *
    * {[ logging {
    *       type = "multi_file";
    *       directory = "/path/to/logdir";
    *       format = "<format string>";
    *       file {
    *           component = "<name_of_component>";
    *           subchannel = "<name_of_subchannel>";
    *           max_level = "<max_level>";
    *           file = "<logfile>";
    *           format = "<format string>";
    *       };
    *       file { ... }; ...
    *    }
    * ]}
    *
    * - [format]: Optional format string. See below.
    * - [component]: Optional component selector. See below.
    * - [subchannel]: Optional subchannel selector. See below.
    * - [max_level]: Optional maximum log level. See below.
   *)

class type syslog_config =
object
  inherit generic_config
  method log_identifier : string
  method log_facility : Netsys_posix.syslog_facility
end

val syslog_logger : syslog_config -> logger
  (** Creates a logger writing to syslog *)

val syslog_logger_factory : logger_factory
  (** Reads a logging section like
    *
    * {[ logging {
    *       type = "syslog";
    *       format = "<format string>";
    *       identifier = "<identifier>";
    *       facility = "<facility name>";
    *       component = "<name_of_component>";
    *       subchannel = "<name_of_subchannel>";
    *       max_level = "<max_level>";
    *    }
    * ]}
    *
    * All parameters except [type] are optional:
    * - [facility]: The faciltiy like [LOCAL0], [USER], ...
    * - [identifier]: An identifier prefixing every message
    * - [format]: Optional format string. See below.
    * - [component]: Optional component selector. See below.
    * - [subchannel]: Optional subchannel selector. See below.
    * - [max_level]: Optional maximum log level. See below.
   *)

val logger_factories :  logger_factory list
  (** All built-in logger factories *)

val level_weight : level -> int
  (** An integer corresponding to the level *)

val level_of_string : string -> level
  (** Convert a string to a level *)



(** {2 Common parameters} *)

(** Logging parameters that can occur in all logging configurations: *)

(** {3 format}
  *
  * The format string may include variable
  * parts in the syntax [$name] or [${name}]. The following variable
  * specifications are defined:
  * - [timestamp]: the time in standard format (as set in
      {!Netlog.current_formatter})
  * - [timestamp:<format>] the time in custom format where [<format>] is a
  *   {!Netdate} format string
  * - [timestamp:unix]: the time in seconds since the epoch
  * - [component]: the name of the component emitting the log message
  * - [subchannel]: the name of the subchannel
  * - [level]: the log level
  * - [message]: the log message
 *)

(** {3 component} 
  *
  * The [component] name restricts logging to the given Netplex component.
  * One can use the wildcard "*".
 *)

(** {3 subchannel} 
  *
  * The [subchannel] name restricts logging to this subchannel.
  * One can use the wildcard "*".
 *)

(** {3 max_level}
  *
  * The [max_level] specifier restricts logging to messages with at most
  * this level. Levels are "emerg", "alert", "crit", "err", "warning",
  * "notice", "info", "debug" (in order of increasing level).
 *)
