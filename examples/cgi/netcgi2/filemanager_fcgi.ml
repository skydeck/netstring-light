(* $Id: filemanager_fcgi.ml,v 1.2 2005/09/04 21:10:05 chris_77 Exp $ *)


let () =
  try
    Filemanager.script (fun ~config ~arg_store ~output_type f ->
			  Netcgi_fcgi.run ~config ~arg_store ~output_type f)

  with Unix.Unix_error(Unix.ENOTSOCK, "accept", _) ->
    let port = 8888 in
    Printf.printf "External server listening on port %i.\n" port;
    flush stdout;
    let run ~config ~arg_store ~output_type f =
      Netcgi_fcgi.run ~config ~arg_store ~output_type f
	~sockaddr:(Unix.ADDR_INET(Unix.inet_addr_any, port)) in
    Filemanager.script run
