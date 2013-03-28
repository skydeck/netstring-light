(** This program must be compiled to .cmo or .cma.  Moreover, put in
    an Apache conf file:

    NetcgiLoad nums.cma
    NetcgiLoad numerix/numerix.cma
    NetcgiLoad cryptokit/cryptokit.cma
*)

let () =
  Filemanager.script (fun ~config ~arg_store ~output_type f ->
			Netcgi_apache.run ~config ~arg_store ~output_type f)
