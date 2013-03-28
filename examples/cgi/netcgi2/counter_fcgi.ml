(** CGI execution.  You could of course copy the code of counter.ml
    here but it is not done in order to reuse counter.ml with other
    connectors. *)

open Netcgi

let () =
  let port = 1200 in
  Printf.printf "%s (FCGI) listening on port %i.\n%!" Sys.argv.(0) port;
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_fcgi.run ~output_type:(`Transactional buffered)
    ~sockaddr:(Unix.ADDR_INET(Unix.inet_addr_any, port))
    (fun cgi -> Counter.main(cgi :> cgi))
