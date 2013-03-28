(* $Id: rpc_common.ml 258 2004-05-25 16:49:11Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

open Rtypes
open Xdr

(* auxiliary functions of general use *)

let rec unix_call (f:unit->'a) : 'a =
  try
    f()
  with
    Unix.Unix_error (Unix.EINTR, _, _) -> unix_call f
;;


(* TODO:
 * check if all system calls use unix_call
 *)
