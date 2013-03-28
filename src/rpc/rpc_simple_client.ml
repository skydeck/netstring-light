(* $Id: rpc_simple_client.ml 258 2004-05-25 16:49:11Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* This implementation is an example how to use Rpc_client
 * in a relatively simple way.
 *)

open Xdr;;
open Rpc;;

type t =
    { client : Rpc_client.t;
      queue : Unixqueue.event_system;
    }
;;


type result =
    No
  | Reply of xdr_value
  | Error of exn
;;


let create host prot spec =
  let q = Unixqueue.create_unix_event_system() in
  let c = Rpc_client.create q host prot spec in
  begin match prot with
    Tcp ->
      Rpc_client.configure c 0 300.0
  | Udp ->
      Rpc_client.configure c 3 15.0
  end;
  { client = c;
    queue = q;
  }
;;


let call scl proc arg =
  let cl = scl.client in
  let r = ref No in
  let get_result transmitter =
    try
      r := Reply (transmitter())
    with
      x ->
	r := Error x
  in
  (* push the request onto the queue: *)
  Rpc_client.add_call cl proc arg get_result;
  (* run through the queue and process all elements: *)
  Unixqueue.run scl.queue;
  (* now a call back of 'get_result' should have happened. *)
  match !r with
    No -> failwith "Rpc_simple_client.call: internal error"
  | Reply x -> x
  | Error e -> raise e
;;


let shut_down scl =
  Rpc_client.shut_down scl.client
;;
