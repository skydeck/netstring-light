(* $Id: netmcore_camlbox.ml 1803 2012-10-18 23:44:51Z gerd $ *)

open Netcamlbox
open Netmcore
open Printf

let create_camlbox prefix n size =
  let (fd, name) =
    Netsys_posix.shm_create ("/" ^ prefix) 0 in
  let fd_open = ref true in
  try
    assert(name.[0] = '/');
    let name1 = String.sub name 1 (String.length name-1) in
    let box =
      format_camlbox name1 fd n size in
    Unix.close fd;
    fd_open := false;
    let res = manage_shm name in
    (box, res#id)
  with
    | error ->
	if !fd_open then ( 
          (* Apparently, "Unix.close fd" is rejected on OS X when the shm
             has not been ftruncated. So just try that.
          *)
          (try Unix.ftruncate fd 0 with _ -> ());
	  Unix.close fd;
	);
	raise error




let lookup_camlbox_address res_id =
  let name = get_shm res_id in
  assert(name.[0] = '/');
  String.sub name 1 (String.length name - 1)


let lookup_camlbox_sender res_id = 
  let addr = lookup_camlbox_address res_id in
  camlbox_sender addr

