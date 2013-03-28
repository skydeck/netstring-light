(* $Id: qclient_auth_dh.ml 286 2006-04-29 16:21:42Z gerd $ *)

module C1 = Queues_clnt.QUEUESPROG.QUEUESVERS1 ;;

let getenv ?(default="") n =
  try Sys.getenv n with Not_found -> default ;;


let getnetname host =
  let host_firstname =
    try
      let k = String.index host '.' in
      String.sub host 0 k
    with
	Not_found -> host
  in
  let domain =
    Rpc_auth_dh.domainname() in
  getenv ~default:("unix." ^ host_firstname ^ "@" ^ domain) "QCLIENT_NETNAME"
;;


Qclient.pluggable_auth_module :=
  ( "auth_dh",
    (fun host ->
       let netname = getnetname host in
       let clnt = C1.create_portmapped_client host Rpc.Tcp in
       Rpc_client.set_auth_methods clnt
         [ Rpc_auth_dh.client_auth_method netname ];
       clnt
    )
  )
;;
