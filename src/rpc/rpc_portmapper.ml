(* $Id: rpc_portmapper.ml 258 2004-05-25 16:49:11Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* Call the portmapper version 2. Note that version 2 is an older version
 * (version 3 and 4 are called 'rpcbind'), but it is normally available.
 *)

open Rtypes
open Xdr
open Rpc

type t =
    { client : Rpc_simple_client.t
    }
;;


let pm2_ts =
  [ "mapping",        X_struct [ "prog", X_uint;
                                 "vers", X_uint;
                                 "prot", X_uint;
                                 "port", X_uint
                               ];
    "pmaplist",       X_rec("pmaplist",
                        (x_optional
                           (X_struct [ "map",  X_type "mapping";
                                       "next", X_refer "pmaplist"
                                     ])));
    "call_args",      X_struct [ "prog", X_uint;
                                 "vers", X_uint;
                                 "proc", X_uint;
                                 "args", x_opaque_max
                               ];
    "call_results",   X_struct [ "port", X_uint;
                                 "res",  x_opaque_max
                               ]
  ]
;;


let pm2_spec() =
    begin
      Rpc_program.create
	(uint4_of_int 100000)
	(uint4_of_int 2)
	(validate_xdr_type_system pm2_ts)
	[ "NULL",    ((uint4_of_int 0), X_void,           X_void);
	  "SET",     ((uint4_of_int 1), X_type "mapping", x_bool);
	  "UNSET",   ((uint4_of_int 2), X_type "mapping", x_bool);
	  "GETPORT", ((uint4_of_int 3), X_type "mapping", X_uint);
	  "DUMP",    ((uint4_of_int 4), X_void, X_type "pmaplist");
	  "CALLIT",  ((uint4_of_int 5), X_type "call_args", X_type "call_results")
	]
    end
;;


let mk_mapping prog vers prot port =
  XV_struct_fast
    [| (* prog *) XV_uint prog;
       (* vers *) XV_uint vers;
       (* prot *) XV_uint (if prot = Tcp then uint4_of_int 6 else uint4_of_int 17);
       (* port *) XV_uint (uint4_of_int port)
    |]
;;


let rec dest_pmaplist l =
  try
    begin match l with
      XV_union_over_enum_fast (1, s) ->
        begin match s with
          XV_struct_fast [| (* map *) XV_struct_fast
	                                [| (* prog *) XV_uint prog;
                                           (* vers *) XV_uint vers;
                                           (* prot *) XV_uint prot;
                                           (* port *) XV_uint port
					|];
                            (* next *) l'
                         |]
          ->
	    let prot' =
	      match int_of_uint4 prot with
		6 -> Tcp
	      |	17 -> Udp
	      |	_  -> failwith "illegal protocol specifier found"
	    in
            (prog, vers, prot', int_of_uint4 port) :: dest_pmaplist l'
        end
    | XV_union_over_enum_fast (0, _) ->
        []
    end
  with
    Match_failure _ -> failwith "dest_pmaplist"
;;


let create connector =
  let spec = pm2_spec() in
  let client = Rpc_simple_client.create connector Tcp spec in
  { client = client }
;;


let create_inet s =
  create (Rpc_client.Inet(s,111))
;;


let shut_down pm =
  Rpc_simple_client.shut_down pm.client
;;


let null pm =
  ignore(Rpc_simple_client.call pm.client "NULL" XV_void)
;;


let set pm prog vers prot port =
  let reply =
    Rpc_simple_client.call pm.client "SET" (mk_mapping prog vers prot port) in
  reply = xv_true
;;


let unset pm prog vers prot port =
  let reply =
    Rpc_simple_client.call pm.client "UNSET" (mk_mapping prog vers prot port) in
  reply = xv_true
;;


let getport pm prog vers prot =
  let reply =
    Rpc_simple_client.call pm.client "GETPORT" (mk_mapping prog vers prot 0) in
  match reply with
    XV_uint n -> int_of_uint4 n
  | _         -> failwith "Rpc_portmapper.getport"
;;


let dump pm =
  let reply =
    Rpc_simple_client.call pm.client "DUMP" XV_void in
  dest_pmaplist reply
;;


let callit pm spec proc arg =
  let (proc_nr, in_t, out_t) = Rpc_program.signature spec proc in
  let prog_nr = Rpc_program.program_number spec in
  let vers_nr = Rpc_program.version_number spec in
  let arg_value = Xdr.pack_xdr_value_as_string arg in_t [] in
  let reply =
    Rpc_simple_client.call
      pm.client
      "CALLIT"
      (XV_struct_fast [| (* prog *) XV_uint prog_nr;
	                 (* vers *) XV_uint vers_nr;
		         (* proc *) XV_uint proc_nr;
		         (* args *) XV_opaque arg_value
                       |] ) in
  let
      XV_struct_fast [| (* port *) XV_uint port;
	                (* res *)  XV_opaque result
                      |] = reply in

  int_of_uint4 port,
  unpack_xdr_value result out_t []
;;


let port_of_program program serverhost prot =
  let pm = create_inet serverhost in
  let p = getport pm (Rpc_program.program_number program)
                     (Rpc_program.version_number program)
                     prot in
  shut_down pm;
  if p = 0 then failwith "portmapper does not know the program";
  p
;;

