(* $Id: netmcore_process.ml 1579 2011-04-12 20:16:22Z gerd $ *)

open Netmcore

type 'a fork_point = res_id

type 'b join_point = res_id

module Encap =
  Netplex_encap.Make_encap(struct type t = Obj.t end)

let def_process proc =
  Netmcore.def_process
    (fun arg_encap ->
       let arg = Obj.obj (Encap.unwrap arg_encap) in
       let res = proc arg in
       Encap.wrap (Obj.repr res)
    )

let start ?inherit_resources fp arg =
  Netmcore.start ?inherit_resources fp (Encap.wrap (Obj.repr arg))

let join jp pid =
  match Netmcore.join jp pid with
    | None -> None
    | Some enc -> Some(Obj.obj (Encap.unwrap enc))

let release_fork_point fp =
  release fp

let release_join_point fp =
  release fp
