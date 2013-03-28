(** CGI execution.  You could of course copy the code of counter.ml
    here but it is not done in order to reuse counter.ml with other
    connectors. *)

let () =
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_cgi.run ~output_type:(`Transactional buffered) Counter.main
