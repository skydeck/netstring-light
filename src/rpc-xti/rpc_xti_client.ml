(* $Id: rpc_xti_client.ml 258 2004-05-25 16:49:11Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

external cots_connect : string -> string -> Unix.file_descr
  = "xti_cots_connect" ;;

type connector =
    [ `Direct of (Rpc_client.connector * Rpc.protocol)
    | `Keyenvoy of string
    ]

let keyserv_connector =
  `Direct
    (Rpc_client.Dynamic_descriptor
       (fun () ->
	  cots_connect "/dev/ticotsord" (Unix.gethostname() ^ ".keyserv")
       ),
       Rpc.Tcp
    )

;;
