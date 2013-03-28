let buffer_length = 1024 in
let in_buffer = String.create buffer_length in
let out_buffer = String.create buffer_length in
let out_buffer_length = ref 0 in
let end_of_stream = ref false in
let waiting_for_input = ref true in
let waiting_for_output = ref false in

while !waiting_for_input or !waiting_for_output do
  (* If !waiting_for_input, we are interested whether input arrives.
   * If !waiting_for_output, we are interested whether output is 
   * possible.
   *)
  let (in_fd, out_fd, oob_fd) = 
    Unix.select (if !waiting_for_input then [ Unix.stdin] else [])
                (if !waiting_for_output then [ Unix.stdout] else [])
                []
                (-.1.0) in

  (* If in_fd is non-empty, input is immediately possible and will 
   * not block. 
   *)
  if in_fd <> [] then begin
    (* How many bytes we can read in depends on the amount of 
     * free space in the output buffer.
     *)
    let n = buffer_length - !out_buffer_length in
    assert(n > 0);
    let n' = Unix.read Unix.stdin in_buffer 0 n in
    end_of_stream := (n' = 0);
    (* Convert the bytes, and append them to the output buffer. *)
    let converted = String.uppercase (String.sub in_buffer 0 n') in
    String.blit converted 0 out_buffer !out_buffer_length n';
    out_buffer_length := !out_buffer_length + n';
  end;

  (* If out_fd is non-empty, output is immediately possible and 
   * will not block.
   *)
  if out_fd <> [] then begin
    (* Try to write !out_buffer_length bytes. *)
    let n' = Unix.write Unix.stdout out_buffer 0 !out_buffer_length in
    (* Remove the written bytes from the out_buffer: *)
    String.blit out_buffer n' out_buffer 0 (!out_buffer_length - n');
    out_buffer_length := !out_buffer_length - n'
  end;

  (* Now find out which event is interesting next: *)

  waiting_for_input :=                   (* Input is interesting if...*)
    not !end_of_stream &&                (* ...we are before the end *)
    !out_buffer_length < buffer_length;  (* ...there is space in the out buf *)

  waiting_for_output :=                  (* Output is interesting if... *)
    !out_buffer_length > 0;              (* ...there is material to output *)

done
