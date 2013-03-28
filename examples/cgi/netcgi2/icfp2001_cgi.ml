
let () =
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  let arg_store _ name _ = if name = "file" then `File else `Memory in
  Netcgi_cgi.run ~config:Icfp2001.config ~arg_store
    ~exn_handler:Icfp2001.exn_handler
    ~output_type:(`Transactional buffered)
    Icfp2001.main
