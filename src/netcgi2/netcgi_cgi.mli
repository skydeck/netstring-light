(* netcgi_cgi.mli

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

(** Classical CGI connector.
 *
 * CGI scripts have poor performance as they are entirely re-run for
 * each request (so it is not possible to cache in the running program
 * database connections for example).  Still they are suitable for
 * moderate frequented websites (especially because they run
 * everywhere) and for development.  *)

open Netcgi

val run :
  ?config:config ->
  ?output_type:output_type ->
  ?arg_store:arg_store ->
  ?exn_handler:exn_handler ->
  (cgi -> unit) -> unit
  (** [run f] executes [f cgi] for each cgi request.  Contrarily to
      the other connectors, CGI execute the whole code (even the one
      outside [f]) each time the script is accessed.  Therefore, one
      cannot easily cache database connections,...

      @param config Default: {!Netcgi.default_config}
      @param output_type Default: [`Direct ""]
      @param arg_store Default: [`Automatic] for all arguments.
      @param exn_handler See {!Netcgi.exn_handler}.  Default: delegate
      all exceptions to the default handler.  *)

val is_cgi : unit -> bool
  (** [is_cgi] says whether the script is run in a CGI environment.
      This allows for example to call the {!Netcgi_test} module if it
      is not. *)
