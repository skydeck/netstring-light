
let () =
  let port = 8888 in
  Printf.printf "External SCGI server listening on port %i.\n" port;
  flush stdout;
  let run ~config ~arg_store ~output_type f =
    Netcgi_scgi.run ~config ~arg_store ~output_type f ~port in
  Filemanager.script run
