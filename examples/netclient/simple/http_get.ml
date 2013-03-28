#require "netclient";;

(* This example shows how to get a file from a HTTP server using
 * the Convenience module.
 *
 * Load this into the toplevel, then:
 * get_and_print "http://www.caml.org/";;
 *)

open Http_client.Convenience;;

let get_and_print url =
  let s = http_get url in
  print_string s;
  flush stdout
;;
