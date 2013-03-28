(* $Id: rpc_time.ml 1196 2008-07-23 01:18:00Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

exception Time_not_available

let remote_time ?(timeout = 5) peer =
  let cleanup = ref [] in
  let add_action f = cleanup := f :: !cleanup in
  let do_cleanup() = List.iter (fun f -> f()) !cleanup; cleanup := [] in
  try
    let dom = Netsys.domain_of_inet_addr peer in
    let s = Unix.socket dom Unix.SOCK_STREAM 0 in
    add_action (fun () -> Unix.close s);
    Unix.set_nonblock s;               (* so that Unix.connect does not block *)
    ( try
	Unix.connect s (Unix.ADDR_INET(peer, 37))
      with
	Unix.Unix_error((Unix.EINPROGRESS|Unix.EWOULDBLOCK),_,_) -> ()
	  (* Note: Win32 returns EWOULDBLOCK instead of EINPROGRESS *)
    );
    Unix.clear_nonblock s;
    let buf = String.create 4 in
    let pos = ref 0 in
    while !pos < 4 do
      let ok = 
	Netsys.restart_tmo (Netsys.wait_until_connected s) (float timeout) in
      if not ok then raise Time_not_available;
      let n = Netsys.blocking_read s buf !pos (String.length buf - !pos) in
      pos := !pos + n;
      if !pos < 4 && n=0 then raise Time_not_available;
    done;
    do_cleanup();
    let x3 = float (Char.code buf.[0]) in
    let x2 = float (Char.code buf.[1]) in
    let x1 = float (Char.code buf.[2]) in
    let x0 = float (Char.code buf.[3]) in
    x3 *. 16777216.0 +. x2 *. 65536.0 +. x1 *. 256.0 +. x0 -. 2208988800.0
      (* 2208988800 = 1 Jan 1970 00:00:00 *)
  with
      err ->
	do_cleanup();
	raise Time_not_available
;;
