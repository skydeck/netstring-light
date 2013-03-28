(* $Id: nethttpd_plex.mli 1488 2010-09-30 22:10:06Z gerd $ *)

(** {1 Netplex support} *)

(** The important function is [nethttpd_factory], see below. The
    other functions are only needed for special effects.

    An example is explained here: {!Netplex_intro.webserver}
 *)

type config_log_error = Nethttpd_types.request_info -> string -> unit
type config_log_access = Nethttpd_types.full_info -> unit
type config_error_response = Nethttpd_types.error_response_params -> string
  (** Three type abbreviations for logging functions *)

val std_log_error : Netplex_types.container -> config_log_error 
  (** Returns a function that logs errors using the [log_subch] method of
      the passed container
   *)

val std_log_access : ?debug:bool -> 
                     Netplex_types.container -> config_log_access
  (** Returns a function that logs accesses using the [log_subch] method of
      the passed container

      If [debug] is set, additional debug log messages are printed that
      dump the whole access (incl. header and all available information)
   *)

val std_error_response : config_error_response
  (** A sample error response function *)

val restrict_file_service_config : Netplex_types.config_file ->
                                   Netplex_types.address ->  unit
  (** Restricts the subsections and paremeters in the [service]
      configuration section of type "file" to the allowed ones.
   *)


val read_file_service_config : Netplex_types.config_file ->
                               Netplex_types.address -> 
                               string ->
                                 Nethttpd_services.file_service
  (** [read_file_service_config cfg addr uri_path]: Reads the
      [service] configuration section of type "file" from config file
      [cfg] at address [addr].  [uri_path] is the default value put
      into the [file_uri] component of the returned record if no "uri"
      configuration parameter exists. (In other words, this is the
      path of the enclosing "uri" section, or "/" if there is only
      a "host" section.) All other parameters are only
      taken from the configuration section. 

      See below at [nethttpd_factory] how a file service needs to
      be configured.
   *)

val restrict_dynamic_service_config : Netplex_types.config_file ->
                                      Netplex_types.address ->  unit
  (** Restricts the subsections and paremeters in the [service]
      configuration section of type "dynamic" to the allowed ones.
   *)

val read_dynamic_service_config : 
      (string * (Netplex_types.config_file ->
                 Netplex_types.address -> 
                 string ->
                   'a Nethttpd_services.dynamic_service
                ) ) list ->
      Netplex_types.config_file ->
      Netplex_types.address -> 
      string ->
        'a Nethttpd_services.dynamic_service
  (** [read_dynamic_service_config handlers cfg addr uri_path]:
      Reads the [service] configuration section of type "dynamic" from config 
      file [cfg] at address [addr]. The alist [handlers] defines the
      available handlers. Every handler [h] is called like
      [h cfg addr uri_path]. [uri_path] is like in [read_file_service_config],
      i.e. the path of the enclosing "uri" section, or "/" by default.

      The [h] function has to return the dynamic service to use, which
      is also returned by [read_dynamic_service_config].

      See below at [nethttpd_factory] how a dynamic service needs to
      be configured.
   *)


type encap = [ `Reactor | `Engine ]

val nethttpd_processor : 
  ?hooks:Netplex_types.processor_hooks ->
  ?encap:encap ->
  (Netplex_types.container -> #Nethttpd_reactor.http_reactor_config) ->
  'a Nethttpd_types.http_service ->
  Netplex_types.processor
    (** [netplex_processor mk_config http_service]: Creates a Netplex processor
    * for Nethttpd.
    *
    * [mk_config] determines the nethttpd config for a container.
    * This is especially useful for setting the logging functions.
    *
    * The resulting processor must be turned into a full Netplex service
    * by [Netplex_sockserv.create_socket_service] which can then be added
    * by calling the controller's method [add_service].
    *
    * [hooks]: One can pass a Netplex hook object to set the hooks of the
    * processor.
    *
    * [encap]: Selects the encapsulation, [`Reactor] or [`Engine]. 
    * The default is [`Reactor]. Each encapsulation has specific strengths
    * and weaknesses:
    * - [`Reactor] is simpler code. Also, the request and response bodies
    *   need not to be buffered up, and are directly connected with the 
    *   underlying socket (low memory requirement). The disadvantage is
    *   that a reactor processes TCP connections serially (important to know
    *   when there is only a single Unix process)
    * - [`Engine]: The request body needs to be completely buffered up.
    *   If pipelining is enabled, the response bodies are also buffered
    *   (FIXME).
    *   The advantage of this encapsulation is that the engine can
    *   process multiple TCP connections simultaneously, even in a 
    *   single process/thread.
     *)

type ('a,'b) service_factory =
    (string * 'a Nethttpd_services.dynamic_service) list ->
    Netplex_types.config_file ->
    Netplex_types.address -> 
    string ->
      'b Nethttpd_types.http_service
    constraint 'b = [ `Dynamic_service of 'a Nethttpd_services.dynamic_service
                    | `File_service of Nethttpd_services.file_service 
		    ]
  (** The service factory function is called when a [service] configuration
      section of a certain type needs to be read. The function has args
      [handlers], [cfg], [addr], and [uri_path]. It needs to return the
      [http_service].

      Such a function is usually [read_file_service_config], or
      [read_dynamic_service_config], or a derivative, whose return
      value is turned into a [http_service]. This can be done with
      {!Nethttpd_services.file_service} and
      {!Nethttpd_services.dynamic_service}.
   *)

val default_services : (string * ('a,'b) service_factory) list
  (** The default services *)

type httpd_factory =
    { httpd_factory :
	'a . 
	  (Netplex_types.container -> Nethttpd_reactor.http_reactor_config) ->
	    'a Nethttpd_types.http_service ->
	      Netplex_types.processor
    }
  (** The type of the [nethttpd_processor] function *)


val nethttpd_factory :
      ?name:string ->
      ?hooks:Netplex_types.processor_hooks ->
      ?encap:encap ->
      ?config_cgi:Netcgi.config -> 
      ?handlers:(string * 'a Nethttpd_services.dynamic_service) list ->
      ?services:(string * ('a,'b) service_factory) list ->
      ?log_error:(Netplex_types.container -> config_log_error) ->
      ?log_access:(?debug:bool -> Netplex_types.container -> config_log_access) ->
      ?error_response:config_error_response -> 
      ?processor_factory:httpd_factory ->
      unit ->
        Netplex_types.processor_factory
  (** Factory for a web server component.
    *
    * {b Configuration file.} Reads a configuration section like
    * {[
    *    processor {
    *      type = "nethttpd";          (* or what is passed as "name" arg *)
    *      timeout = 300.0;
    *      timeout_next_request = 15.0;
    *      access_log = "enabled";
    *      suppress_broken_pipe = true;
    *      host {
    *        pref_name = "myhost";     (* optional *)
    *        pref_port = 80;           (* optional *)
    *        names = "myhost:80 yourhost:81";  (* use *:0 for any name *)
    *        uri {
    *          path = "/the/path";
    *          method {
    *            allow = "GET POST";
    *            (* or: deny = "..." *)
    *            service {
    *              type = "...";
    *              ...
    *            }
    *          }
    *        }
    *        uri {
    *          ...
    *        }
    *      }
    *      host {
    *        ...
    *      }
    *    }
    * ]}
    *
    * The [access_log] parameter can be set to [off], [enabled], or [debug].
    * The default is [off]. Access messages go to the "access" subchannel
    * of the component logger. If [enabled], one line is printed with the
    * most important data. If [debug] is set, all access data are printed.
    *
    * If [suppress_broken_pipe] the error "Broken pipe" is not logged
    * in the error log. This error occurs frequently, and may be regarded
    * as a normal condition.
    *
    * The sections [host], [uri] and [method] can be nested to any depth.
    * However, on every nesting level only one of these section types must be
    * used. For example, if a [host] section already contains [uri]
    * subsections, it is not allowed to add [method] subsections.
    * Furthermore, the outermost section must be [host].
    *
    * The [service] section may be one of (at least if the [services]
    * parameter is not overridden):
    *
    * {[
    *    service {
    *      type = "file";
    *      docroot = "/a/path/in/the/filesystem";
    *      uri = "/the/uri/prefix/corresponding/to/docroot";
    *      media_types_file = "/etc/mime.types";
    *      media_type {
    *        type = "application/foo";
    *        suffix = "foo"
    *      }
    *      default_media_type = "text/plain";
    *      enable_gzip = true;   (* see doc in nethttpd_services.mli *)
    *      index_files = "index.html";
    *      enable_listings = true;
    *      hide_from_listings = "README";   (* list of PCRE regexps *)
    *    }
    * ]}
    *
    * Note that [uri] is taken from the surrounding [uri] section (or
    * assumed to be "/" if there is none) if omitted.
    *
    * {[
    *    service {
    *      type = "dynamic";
    *      handler = "name_of_handler";
    *    }
    * ]}
    *
    * Binds the passed handler here.
    *
    * Any of [host], [uri], and [method] sections may contain one or several
    * [access] sections (which are AND-connected):
    *
    * {[
    *    access {
    *      type = "host";
    *      allow = "host1 host2 ...";
    *      (* or deny = "host1 host2 ..."; *)
    *    }
    * ]}
    *
    * Other access control methods are not yet available.
    *
    * The [services] optional argument can be used to change the service
    * types understood. If not passed, it defaults to [default_services].
    * The default includes "file" and "dynamic".
    *
    * {b Arguments.}
    *
    * - [name]: The processor name. Defaults to "nethttpd". This name can
    *   be referenced by the "type" parameters in the [processor] section
    *   of the config file.
    * - [hooks]: One can pass a Netplex hook object to set the hooks of the
    *   processor. (This argument is ignored if a [processor_factory] is
    *   passed to this function.)
    * - [encap]: See {!Nethttpd_plex.nethttpd_processor}. (This argument is
    *   ignored if a [processor_factory] is
    *   passed to this function.)
    * - [config_cgi]: The CGI configuration to use
    * - [handlers]: a list of handler function. These functions can be
    *   referenced from a [service] section in the config file where
    *   [type="dynamic"] (see example above). Defaults to the empty list.
    * - [services]: A list of service handlers that can be used 
    *   by [service] sections in the config files. Defaults to
    *   {!Nethttpd_plex.default_services} which defines "file" and "dynamic".
    * - [log_error]: The error logger. Defaults to
    *   {!Nethttpd_plex.std_log_error}.
    * - [log_access]: The access logger. Defaults to
    *   {!Nethttpd_plex.std_log_access}.
    * - [error_response]: a handler which is invoked to generate error
    *   responses. Defaults to {!Nethttpd_plex.std_error_response}.
    * - [processor_factory]: the function creating the processor.
    *   Default is [nethttpd_processor].
   *)
