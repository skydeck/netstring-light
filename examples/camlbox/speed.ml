(* Send small messages to a forked subprocess

   Invoke like:

   ./speed <n>

   where <n> are the number of messages to send.
 *)

let name = "camlbox_" ^ string_of_int(Unix.getpid())

let create() =
  (* Create a camlbox with 10 slots, 512 bytes each *)
  let box = Netcamlbox.create_camlbox name 10 512 in
  (* We used here the following trick, but it does not work on some
     platforms (e.g. OSX), where we must keep the camlbox names
     until all processes have opened the box objects:

     We only keep the file descriptor of the box, and unlink the
     name of the box immediately. This is possible because we don't need
     to access the box by name anymore. Instead, the descriptor is inherited
     to the child process. Advantage of this: We never forget to delete
     the box, even if the program crashes.
   *)
  let fd = Netcamlbox.camlbox_fd name in
  (* Netcamlbox.unlink_camlbox name; (* no longer applicable *) *)
  (box, name, fd)

let receiver name box =
  (* This function runs in the receiver process, and runs receives messages
     in a loop until the empty list arrives as message.
   *)
  let unlink_done = ref false in
  let cont = ref true in
  let sum = ref 0 in
  while !cont do
    (* Wait until we have new messages. The index positions of the slots
       with the new messages are returned in new_list.
     *)
    let new_list =
      Netcamlbox.camlbox_wait box in
    (* Process the new messages in turn: *)
    List.iter
      (fun k ->
         (* Get the message, process it, and delete it. We have to be 
            careful that no pointer to the message, or a part of it,
            survives the deletion. If so, the program can crash.
	  *)
	 let (msg : int list ref) =
	   Netcamlbox.camlbox_get box k in
	 sum := List.fold_left ( + ) !sum !msg;
	 if !msg = [] then
	   cont := false;
	 Netcamlbox.camlbox_delete box k
      )
      new_list;
    if not !unlink_done then (
      (* We can now safely unlink the box, because sender and receiver have
	 opened it
       *)
      Netcamlbox.unlink_camlbox name;
      unlink_done := true
    )
  done;
  print_endline ("Sum: " ^ string_of_int !sum)

let sender name fd n =
  (* This function runs in the sender process. We simply send n messages
     to the other end.
   *)
  let bs = Netcamlbox.camlbox_sender_of_fd name fd in
  for k = 1 to n do
    let (msg : int list ref) = 
      ref [ 1; 2; 3 ] in
    Netcamlbox.camlbox_send bs msg
  done;
  (* Finally add the empty list to indicate that the stream of messages is
     over.
   *)
  Netcamlbox.camlbox_send bs (ref [])
    (* that's the reason we use a ref: Otherwise the empty list is not
       boxed, and will cause an error
     *)

let main() =
  let n = int_of_string (Sys.argv.(1)) in
  (* create the camlbox... *)
  let (box, name, fd) = create() in
  (* and fork the child process. *)
  match Unix.fork() with
    | 0 ->
	(* The child process takes the role of the receiver *)
	receiver name box;
	exit 0
    | pid ->
	(* The parent process takes the role of the sender *)
	sender name fd n;
	(* wait until the child is finished *)
	ignore(Unix.waitpid [] pid)

(*
let () =Netcamlbox.Debug.enable := true;;
*)

let () = Netsys_sem.force_emulation()

let () =
  main()

