(* $Id: filemanager_cgi.ml,v 1.1 2005/09/04 11:45:22 chris_77 Exp $ *)


let () =
  Filemanager.script (fun ~config ~arg_store ~output_type f ->
			Netcgi_cgi.run ~config ~arg_store ~output_type f)
