(* netcgi_fcgi.mli

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

(** FastCGI connector.
 *
 * {b Remark:} This connector does not allow requests to be multiplexed
 * (and let it know to the web server via FCGI_MPXS_CONNS=0).
 * Multiplexing requests is seldom done by
 * {{:http://www.fastcgi.com}FastCGI modules} and is even sometimes
 * impossible because of bugs in them.  Moreover, multiplexing is
 * mostly useful if concurrent requests are handled by different
 * threads while this library use a single thread to process all
 * requests coming on a given connection.  If the need is felt (speak
 * out!), a multithreaded connector can be built on the side of this
 * one.
 *)

open Netcgi

(** The usual {!Netcgi.cgi} class with FCGI specific methods.  *)
class type cgi =
object
  inherit Netcgi.cgi

  method role : [`Responder | `Authorizer | `Filter]
    (** A FastCGI application can fulfill each of the following three
	roles:

	- [`Responder]: This is the usual role.  In this case the
	application is expected to act like a CGI program: It receives
	all the information associated with an HTTP request and
	generates an HTTP response.

	- [`Authorizer]: An Authorizer FastCGI application receives
        all the information associated with an HTTP request and
        generates an authorized/unauthorized decision.

	- [`Filter]: A Filter FastCGI application receives all the
        information associated with an HTTP request, plus an extra
        stream of data from a file stored on the Web server, and
        generates a "filtered" version of the data stream as an HTTP
        response.  *)

  method data : Netchannels.in_obj_channel
    (** This the the channel on which the filter data is available.
	All methods of the object raise {!Netchannels.Closed_channel}
	if the role is not [`Filter].  *)
  method data_length : int
    (** How many bytes of the data are available. *)
  method data_mtime : float
    (** The data last modification time, expressed as an integer
	number of seconds since the epoch (January 1, 1970 UTC). *)
end



val run :
  ?config:config ->
  ?allow:(Unix.sockaddr -> bool) ->
  ?output_type:output_type ->
  ?arg_store:arg_store ->
  ?exn_handler:exn_handler ->
  ?sockaddr:Unix.sockaddr ->
  ?port:int ->
  (cgi -> unit) -> unit
  (** [run f] register the function [f] as a main function of the
      script.  Each call to the script will execute [f cgi].  The code
      outside [f] will be executed only once (when the script is
      loaded into memory) which allows to cache database connections,
      etc.

      @param config Default: {!Netcgi.default_config}
      @param allow Tells whether a connection from the socket is allowed.
                   Default: Use the comma separated list given in the
                   environment variable FCGI_WEB_SERVER_ADDRS or allow all
                   if it does not exist.
      @param output_type Default: [`Direct ""]
      @param arg_store Default: [`Automatic] for all arguments.

      @param exn_handler See {!Netcgi.exn_handler}.  Default: delegate
      all exceptions to the default handler.

      @param sockaddr tells on what socket to contact the script.  If
      not specified, the script expects to be launched by the web
      server and to communicate with it through stdin.  For external
      scripts (launched independently of the web server and possibly
      on a different machine), set [sockaddr] to the address the web
      server needs to connect to to talk to the script (this address
      must also be specified in the wen server config file).

      @param port alternative way to specify [sockaddr] for localhost

      Your application should be ready handle SIGUSR1, used to
      resquest a "graceful" process shutdown, and SIGTERM to request a
      quick shutdown.  *)


val handle_request :
  config -> output_type -> arg_store -> exn_handler ->
  (cgi -> unit) -> max_conns:int -> log:(string -> unit) option ->
  Unix.file_descr ->
  connection_directive
    (** [handle_request config output_type arg_store eh f ~max_conns
        ~log fd]: This is a lower-level interface that processes
        exactly one request arriving on the existing connection [fd].

        [max_conns] is passed to the FCGI client and indicates how many
        connections this server can process in parallel.

        [log] is the error logger function or [None], in which case
        errors are passed through to the FCGI client.

        The other arguments are just like for [run].

        The return value indicates whether the connection can be kept
        open or must be closed.
    *)
