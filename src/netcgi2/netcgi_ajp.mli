(* netcgi_ajp.mli

   (C) 2005 Christophe Troestler

   This code may be used under either, the GNU GPL, or the same license
   as ocamlnet (see the file LICENSE).

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

(** Apache JServ Protocol (AJP) 1.3 connector.
 *
 * See the {!Netcgi_ajp.setup} section at the end of this file to know
 * how to configure your web server.
 *)

open Netcgi

val arg_parse :
  (Arg.key * Arg.spec * Arg.doc) list -> Arg.anon_fun -> Arg.usage_msg
  -> (string * string) list
  (** [arg_parse speclist anon_fun usage_msg] parses the command line
      and return an associative list describing the content of the
      property file (see {!Netcgi_ajp.props_of_file}).  This function
      allows to fakes the "java" command (JVM startup):

      - the option [-classpath <path>] is ignored;
      - the first anonymous argument (Java class name) is ignored;
      - the second anonymous argument is the name of the property file;
      - other options are interpreted according to the [speclist].

      @raise Failure and prints a usage message if the property file
      cannot be read.  *)

val props_of_file : string -> (string * string) list
  (** [props_of_file fname] parses the property file [fname] and
      returns it as an associative list.  The following properties are
      used:

      - "bindaddress": The address the server socket is bound to.  Can be
      specified as IP address or hostname or "*" (default: "localhost").
      - "port": The port number the server socket is bound to.  Defaults to
      8007.
      - "security.authentication": If "true", the server expects that the
      web server authenticates itself.  Defaults to "true".
      - "security.challengeSize": The length of the challenge string.
      Defaults to 5.
      - "security.secretKey": The file containing the secret key used for
      authentication.
      - "security.allowHost": Only the web server with this IP address is
      allowed to connect (this option can occur several times).
      DNS names are resolved at startup time.
      - "jakarta.servletSubString": The substring that is used as indicator
      for the servlet name (for mod_jk only).  Defaults to "/servlets/".
      - "ocamlnet.https": Whether HTTPS is assumed as basic protocol or not.
      Defaults to "false".

      Other properties are ignored.

      @raise Invalid_argument if the file does not exist or is not readable.
  *)


val run :
  ?props:(string * string) list ->
  ?config:config ->
  ?script_name:string ->
  ?allow:(Unix.sockaddr -> bool) ->
  ?output_type:output_type ->
  ?arg_store:arg_store ->
  ?exn_handler:exn_handler ->
  ?sockaddr:Unix.sockaddr ->
  ?port:int ->
  (cgi -> unit) -> unit
  (** [run f] executes [f cgi] for each AJP request.

      @param config Default: {!Netcgi.default_config}
      @param allow Tells whether a connection from the socket is allowed.
                   Default: allow from all.
      @param output_type Default: [`Direct ""]
      @param arg_store Default: [`Automatic] for all arguments.
      @param port The port used by the web server to send the requests
                  (Default: 8009).
      @param sockaddr The sockaddress (overrides [port])
      @param exn_handler See {!Netcgi.exn_handler}.  Default: delegate
      all exceptions to the default handler.  *)

val handle_request :
  ?script_name:string ->
  config -> output_type -> arg_store -> exn_handler ->
  (cgi -> unit) -> log:(string -> unit) option ->
  Unix.file_descr ->
    connection_directive
  (** [handle_request config output_type arg_store eh f ~log fd]: This
      is a lower-level interface that processes exactly one request
      arriving on the existing connection [fd].

      [log] is the error logger function or [None], in which case
      errors are passed through to the FCGI client.

      The other arguments are just like for [run].

      The return value indicates whether the connection can be kept
      open or must be closed.
   *)


(* ---------------------------------------------------------------------- *)

(** {2:setup Setup}


    {3 Apache}

    You need to use mod_jk to have support for AJP/1.3.  To install
    it, please see
    {{:http://tomcat.apache.org/tomcat-3.3-doc/mod_jk-howto.html}Working
    with mod_jk}.

    In httpd.conf or in a file, say mod_jk.conf, in
    /etc/apache2/conf.d/ (or /etc/apache2/conf.d/ for Apache 1.x), add
    the following:
    {v
    # Shared memory file name (Unix only).  The parent dir must exist.
    JkShmFile  /var/tmp/jk-runtime-status
    LoadModule jk_module mod_jk.so
    # Declare the module for <IfModule> (remove this line on Apache 2.x)
    AddModule  mod_jk.c

    <IfModule mod_jk.c>
      # Configure mod_jk
      # Apache 1.x
      #JkWorkersFile /etc/libapache-mod-jk/workers.properties
      #JkLogFile     /var/log/apache/mod_jk.log
      # Apache 2.x
      JkWorkersFile /etc/libapache2-mod-jk/workers.properties
      JkLogFile     /var/log/apache2/mod_jk.log
      JkLogLevel    info

      # JkMount [URL prefix] [Worker name]
      JkMount /*.jsp ajp13_worker
      JkMount /servlet/* ajp13_worker
    </IfModule>
    v}


    {3 Other web severs}

    Please go to this
    {{:http://tomcat.apache.org/connectors-doc/index.html}configuration
    page}.  Mail us specific instructions or tips for other web
    servers so we can include them here.


    {3 Workers.properties}

    Here is an example of workers.properties:
    {v
    # Comma separated list of worker names:
    worker.list=ajp13_worker
    # Set properties for ajp13_worker
    worker.ajp13_worker.type=ajp13
    worker.ajp13_worker.host=localhost
    worker.ajp13_worker.port=8009
    worker.ajp13_worker.cachesize=1
    v}
*)
