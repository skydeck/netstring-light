#require "equeue";;

open Unixqueue

type copy_state =
    { copy_ues : Unixqueue.event_system;
      copy_group : Unixqueue.group;
      copy_infd : Unix.file_descr;
      copy_outfd : Unix.file_descr;
      copy_size : int;
      copy_inbuf : string;
      copy_outbuf : string;
      mutable copy_outlen : int;
      mutable copy_eof : bool;
      mutable copy_have_inres : bool;
      mutable copy_have_outres : bool;
      mutable copy_cleared : bool;
    }


let copy_file ues old_name new_name =
  (* Adds the necessary handlers and actions to the Unixqueue.event_system
   * ues that copy the file 'old_name' to 'new_name'.
   *)

  let update_resources state ues =
    let want_input_resource =
      not state.copy_eof && state.copy_outlen < state.copy_size in
    let want_output_resource =
      state.copy_outlen > 0 in
    if want_input_resource && not state.copy_have_inres then
      add_resource ues state.copy_group (Wait_in state.copy_infd, -.1.0);
    if not want_input_resource && state.copy_have_inres then
      remove_resource ues state.copy_group (Wait_in state.copy_infd);
    if want_output_resource && not state.copy_have_outres then
      add_resource ues state.copy_group (Wait_out state.copy_outfd, -.1.0);
    if not want_output_resource && state.copy_have_outres then
      remove_resource ues state.copy_group (Wait_out state.copy_outfd);
    state.copy_have_inres <- want_input_resource;
    state.copy_have_outres <- want_output_resource;
    if not want_input_resource && not want_output_resource && 
      not state.copy_cleared 
    then begin
      (* Close file descriptors at end: *)
      Unix.close state.copy_infd;
      Unix.close state.copy_outfd;
      (* Remove everything: *)
      clear ues state.copy_group;
      state.copy_cleared <- true;   (* avoid to call 'clear' twice *)
    end
  in

  let handle_input state ues esys e =
    (* There is data on the input file descriptor. *)
    (* Calculate the available space in the output buffer: *)
    let n = state.copy_size - state.copy_outlen in
    assert(n > 0);
    (* Read the data: *)
    let n' = Unix.read state.copy_infd state.copy_inbuf 0 n in
    (* End of stream reached? *)
    state.copy_eof <- n' = 0;
    (* Append the read data to the output buffer: *)
    String.blit state.copy_inbuf 0 state.copy_outbuf state.copy_outlen n';
    state.copy_outlen <- state.copy_outlen + n';
    (* Add or remove resources: *)
    update_resources state ues
  in

  let handle_output state ues esys e =
    (* The file descriptor is ready to output data. *)
    (* Write as much as possible: *)
    let n' = Unix.write state.copy_outfd state.copy_outbuf 0 state.copy_outlen 
    in
    (* Remove the written bytes from the output buffer: *)
    String.blit 
      state.copy_outbuf n' state.copy_outbuf 0 (state.copy_outlen - n');
    state.copy_outlen <- state.copy_outlen - n';
    (* Add or remove resources: *)
    update_resources state ues
  in

  let handle state ues esys e =
    (* Only accept events associated with our own group. *)
    match e with
	Input_arrived (g,fd) ->
	  handle_input state ues esys e
      | Output_readiness (g,fd) ->
	  handle_output state ues esys e
      | _ ->
	  raise Equeue.Reject
  in

  let g = new_group ues in
  let infd = Unix.openfile 
	       old_name 
	       [ Unix.O_RDONLY; Unix.O_NONBLOCK ] 
	       0 in
  let outfd = Unix.openfile 
		new_name 
		[ Unix.O_WRONLY; Unix.O_NONBLOCK; Unix.O_CREAT; Unix.O_TRUNC ] 
		0o666 in
  Unix.clear_nonblock infd;
  Unix.clear_nonblock outfd;
  let size = 1024 in
  
  let state =
    { copy_ues = ues;
      copy_group = g;
      copy_infd = infd;
      copy_outfd = outfd;
      copy_size = size; 
      copy_inbuf = String.create size;
      copy_outbuf = String.create size;
      copy_outlen = 0;
      copy_eof = false; 
      copy_have_inres = false;
      copy_have_outres = false;
      copy_cleared = false;
    } in
  
  update_resources state ues;
  add_handler ues g (handle state);
;;


let ues = create_unix_event_system() in
copy_file ues "a.old" "a.new";

run ues
;;
