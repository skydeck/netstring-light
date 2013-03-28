(* netcgi_test.mli

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

(** Connector for testing your code.
 *
 * This is to be developed.  There are several ways to go about this
 * and it is not clear which one to take.
 *
 * @version $Id: netcgi_test.mli,v 1.7 2005/10/13 17:54:49 chris_77 Exp $
 *)
(* Recommend:
   ab:  Apache Benchmark (apache2-utils)
   w3c: The W3C Command Line Tool (part of libwww0)
*)

open Netcgi


val run :
  ?config:config ->
  ?output_type:output_type ->
  ?arg_store:arg_store ->
  ?args:cgi_argument list ->
  (cgi -> unit) -> unit
  (* More flexibility is definitely required here -- along the lines
     of [custom_environment].  I am thinking one could e.g. be general
     enough to allow the output to be set into a frame, and another
     frame being used for control, logging,... -- a debugger embedded
     in the web page... *)
