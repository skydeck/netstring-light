(* $Id: netcgi_plex.mli 1243 2009-05-25 23:45:36Z gerd $ *)

(** {1 Netplex support for FastCGI, SCGI and AJP connectors} *)

(** {2 Factory} *)

open Netcgi

val factory :
      ?config:config ->
      ?enable:[ `FCGI | `SCGI | `AJP ] list ->
      ?name:string ->
      ?output_type:output_type ->
      ?arg_store:arg_store ->
      ?exn_handler:exn_handler ->
      ?configure:(Netplex_types.config_file -> 
                  Netplex_types.address ->
		    Netplex_types.processor_hooks) ->
      (Netplex_types.container -> cgi -> unit) ->
        Netplex_types.processor_factory
  (** Reads a Netplex configuration section like
    * {[
    *    processor {
    *      type = "netcgi";          (* or the overridden [name] *)
    *      timeout = 15;             (* optional *)
    *      mount_dir = "/url/path";  (* optional *)
    *      mount_at = "/url/path";   (* optional alternative to mount_dir *)
    *    }
    * ]}
    *
    * and creates a processor for the protocols "fcgi", "scgi",
    * and "ajp" (or a subset of these protocols if the [enable] 
    * parameter restricts them). A complete service definition
    * looks thus like:
    *
    * {[
    *      service {
    *          name = "name_of_service";
    *          protocol {
    *              name = "fcgi";        (* or "scgi" or "ajp" *)
    *              address {
    *                  type = "internet";
    *                  bind = "localhost:<port>";
    *              };
    *          };
    *          (* ... several protocol sections allowed! *)
    *          processor {
    *              type = "netcgi";
    *          };
    *          workload_manager {
    *              type = "dynamic";
    *              max_jobs_per_thread = 1;  (* only reasonable value *)
    *              min_free_job_capacity = <n>;
    *              max_free_job_capacity = <n>;
    *              max_threads = <n>;
    *          };
    *      }
    * ]}
    *
    * The processor calls the argument function of type [container -> cgi -> unit]
    * for every incoming request.
    *
    * The [timeout] parameter specifies when inactive connections are
    * timed out (in seconds). The [mount_dir] and [mount_at] parameters
    * define which part of the URL is considered as [SCRIPT_NAME]:
    *
    * - By default (if neither [mount_dir] nor [mount_at]) are given
    *   [SCRIPT_NAME] is determined in a protocol-dependent way. Usually,
    *   the server transmits [SCRIPT_NAME], but see the note below.
    * - If [mount_dir] is present, the processor accepts only URLs
    *   that have this path as true prefix directory, i.e. the URL path
    *   is [<mount_dir>/<name><rest>]. The part [<mount_dir>/<name>]
    *   is taken as [SCRIPT_NAME].
    * - If [mount_at] is present, the processor accepts only URLs
    *   that have this path as prefix, i.e. the URL path is
    *   [<mount_at><rest>]. [<mount_at>] is taken as [SCRIPT_NAME].
    *
    * The background is that [SCRIPT_NAME] is commonly used to
    * distinghuish between different web actions of the netcgi
    * application. The actions are simply names in a directory like
    * [/bin/<name>] or [/servlet/<name>]. Not all web servers/protocols
    * transmit good values for [SCRIPT_NAME], however. By specifying
    * [mount_dir] or [mount_at] one can force to interpret a certain
    * prefix of the request URL as [SCRIPT_NAME].
    *
    * @param config The Netcgi configuration to use, default is
    *    {!Netcgi.default_config}
    * @param enable Which protocols to support. Default is to
    *    support all protocols
    * @param name Defines the name of the processor. Default is "netcgi".
    * @param output_type Default: [`Direct ""]
    * @param arg_store Default: [`Automatic] for all arguments.
    * @param exn_handler See {!Netcgi.exn_handler}.  Default: delegate
    *      all exceptions to the default handler.
    * @param configure Parameters are the netplex configuration file
    *     and the address of the [processor] section. The configure
    *     function can look for additional parameters. It returns
    *     service hooks that are added to the resulting processor.
   *)


(** {2 Processors} *)

(** The following functions create the processors directly *)

type mountpoint =
    [ `Mount_dir of string
    | `Mount_at of string
    ]

val fcgi_processor : 
      ?config:config ->
      ?output_type:output_type ->
      ?arg_store:arg_store ->
      ?exn_handler:exn_handler ->
      ?timeout:float ->
      ?mount:mountpoint ->
      (Netplex_types.container -> Netcgi_fcgi.cgi -> unit) ->
        Netplex_types.processor

val scgi_processor :
      ?config:config ->
      ?output_type:output_type ->
      ?arg_store:arg_store ->
      ?exn_handler:exn_handler ->
      ?timeout:float ->
      ?mount:mountpoint ->
      (Netplex_types.container -> cgi -> unit) ->
        Netplex_types.processor

val ajp_processor :
      ?config:config ->
      ?output_type:output_type ->
      ?arg_store:arg_store ->
      ?exn_handler:exn_handler ->
      ?timeout:float ->
      ?mount:mountpoint ->
      (Netplex_types.container -> cgi -> unit) ->
        Netplex_types.processor
