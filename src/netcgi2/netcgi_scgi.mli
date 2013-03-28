(* netcgi_scgi.mli

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

(** SCGI connector.

    The {{:http://www.mems-exchange.org/software/scgi/}SCGI} connects
    your web applications through a TCP/IP socket (thus the application
    can run on a machine different from the web server).

    See the {!Netcgi_scgi.setup} section at the end of this file to know
    how to configure your web server.  *)

open Netcgi

val run :
  ?config:config ->
  ?allow:(Unix.sockaddr -> bool) ->
  ?output_type:output_type ->
  ?arg_store:arg_store ->
  ?exn_handler:exn_handler ->
  ?sockaddr:Unix.sockaddr ->
  ?port:int ->
  (cgi -> unit) -> unit
  (** [run f] executes [f cgi] for each SCGI request.

      @param config Default: {!Netcgi.default_config}
      @param allow Tells whether a connection from the socket is allowed.
        Default: allow from all.
      @param output_type Default: [`Direct ""]
      @param arg_store Default: [`Automatic] for all arguments.
      @param sockaddr The socket used by the web server to send the requests.
      @param exn_handler See {!Netcgi.exn_handler}.  Default: delegate
        all exceptions to the default handler.  
      @param sockaddr The sockaddr for listening. Overrides [port]
      @param port The port for listening. Needs to be specified if no
        [sockaddr] is passed.

*)

val handle_request :
  config -> output_type -> arg_store -> exn_handler ->
  (cgi -> unit) -> log:(string -> unit) option ->
  Unix.file_descr -> 
    connection_directive
  (** [handle_request config output_type arg_store eh f ~log fd]:
      This is a 
      lower-level interface that processes exactly one request arriving 
      on the existing connection [fd].

      [log] is the error logger function or [None], in which case 
      errors are passed through to the FCGI client.

      The other arguments are just like for [run].

      The return value indicates whether the connection can be kept
      open or must be closed.
   *)


(* ---------------------------------------------------------------------- *)

(** {2:setup Setup}


    Add to httpd.conf or to, say, scgi.conf in /etc/apache/conf.d
    {[
    LoadModule scgi_module /usr/lib/apache/1.3/mod_scgi.so
    # Serve the URL /scgi by contacting 127.0.0.1 on port 8888
    SCGIMount /scgi 127.0.0.1:8888
    ]}

*)
