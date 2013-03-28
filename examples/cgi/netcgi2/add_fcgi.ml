(** FCGI execution.  The driver independent code is in add.ml (in
    order to reuse it with other connectors).

    Assuming netcgi2 is installed (through findlib), you can compile
    this code with the compile command given at the bottom of this
    file (if you are in Emacs, just press C-c C-c).

    As you can see below, this script expects that the webserver
    contacts it on the port 1201.  See the Netcgi2_fcgi documentation
    for informations on how FCGI scripts can be contacted through a
    pipe and may or may not be launched by the webserver.
*)

open Netcgi

(* Errors of the function [main] (for example if the arguments "x" and
   "y" are not numbers) are handled by the connector.  They will
   rollback the output (i.e. any uncomitted material will be deleted),
   log the error, and generate an error page instead of the current
   page.  If you do not like this behavior, you can define your own
   error handling function and set it via the [exn_handler] optional
   argument of the connector.  *)
let () =
  let port = 1201 in
  Printf.printf "%s (FCGI) listening on port %i.\n%!" Sys.argv.(0) port;
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
   Netcgi_fcgi.run ~output_type:(`Transactional buffered)
    ~sockaddr:(Unix.ADDR_INET(Unix.inet_addr_any, port))
     (fun cgi -> Add.main(cgi :> cgi))


(* Local Variables: *)
(* compile-command: "ocamlfind ocamlc -o add.fcgi -package netcgi2 -linkpkg add.ml add_fcgi.ml" *)
(* End: *)
