(* $Id: netgzip.ml 1800 2012-09-30 12:21:22Z gerd $ *)

open Printf

class input_gzip_rec gzip_ch : Netchannels.rec_in_channel =
object(self)
  val mutable closed = false

  method input s p l = 
    let n = Gzip.input gzip_ch s p l in
    if n = 0 then raise End_of_file;
    n
  method close_in() =
    if not closed then (
      Gzip.close_in gzip_ch;
      closed <- true
    )
end


class input_gzip gzip_ch =
  Netchannels.lift_rec_in_channel (new input_gzip_rec gzip_ch)



class output_gzip_rec gzip_ch : Netchannels.rec_out_channel =
object(self)
  method output s p l =
    Gzip.output gzip_ch s p l;
    l
  method close_out() =
    (* FIXME: No way to suppress errors... *)
    Gzip.close_out gzip_ch
  method flush() =
    Gzip.flush gzip_ch
end


class output_gzip gzip_ch =
  Netchannels.lift_rec_out_channel (new output_gzip_rec gzip_ch)


type in_state =
    { mutable in_hdr_parsed : bool;
      mutable in_stream : Zlib.stream option;
      mutable in_size: int32;
      mutable in_crc: int32;
    }

exception Buffer_underrun

let dispose_in st =
  match st.in_stream with
    | Some stream ->
	Zlib.inflate_end stream;
	st.in_stream <- None;
    | None -> 
	()

let dispose_in_ignore st =
  try dispose_in st with _ -> ()



let inflating_conv st incoming at_eof outgoing =
  let k = ref 0 in    (* how many bytes to consume *)

  let input_byte() =
    if !k < Netbuffer.length incoming then (
      let b = Char.code(Netbuffer.get incoming !k) in
      incr k;
      b
    )
    else raise Buffer_underrun
  in

  let input_int32() =
    let b1 = input_byte() in
    let b2 = input_byte() in
    let b3 = input_byte() in
    let b4 = input_byte() in
    Int32.logor (Int32.of_int b1)
      (Int32.logor (Int32.shift_left (Int32.of_int b2) 8)
	 (Int32.logor (Int32.shift_left (Int32.of_int b3) 16)
            (Int32.shift_left (Int32.of_int b4) 24)))
  in

  if Netbuffer.length incoming > 0 then (
    match st.in_stream with
      | Some stream ->
	  ( try
	      if not st.in_hdr_parsed then (
		let id1 = input_byte() in
		let id2 = input_byte() in
		if id1 <> 0x1F || id2 <> 0x8B then
		  raise(Gzip.Error("bad magic number, not a gzip file"));
		let cm = input_byte() in
		if cm <> 8 then
		  raise(Gzip.Error("unknown compression method"));
		let flags = input_byte() in
		if flags land 0xE0 <> 0 then
		  raise(Gzip.Error("bad flags, not a gzip file"));
		for i = 1 to 6 do ignore(input_byte()) done;
		if flags land 0x04 <> 0 then (
		  let len1 = input_byte() in
		  let len2 = input_byte() in
		  for i = 1 to len1 + len2 lsl 8 do ignore(input_byte()) done
		);
		if flags land 0x08 <> 0 then (
		  while input_byte() <> 0 do () done
		);
		if flags land 0x10 <> 0 then (
		  while input_byte() <> 0 do () done
		);
		if flags land 0x02 <> 0 then (
		  ignore(input_byte()); ignore(input_byte () )
		);
		
	      );
	      
	      let loop = ref true in
	      
	      while !loop do
		let in_buf = Netbuffer.unsafe_buffer incoming in
		let in_pos = !k in
		let in_len = Netbuffer.length incoming - in_pos in
		
		if at_eof && in_len = 0 then (* Hope this is right *)
		  raise(Gzip.Error("premature end of file"));
		
		let used_out =
		  Netbuffer.add_inplace
		    outgoing
		    (fun out_buf out_pos out_len ->
		       let (finished, used_in, used_out) =
			 try
			   Zlib.inflate 
			     stream in_buf in_pos in_len out_buf out_pos out_len 
			     Zlib.Z_SYNC_FLUSH
			 with Zlib.Error(_, _) ->
			   raise (Gzip.Error("error during decompression")) in
		       
		       st.in_size <- 
			 Int32.add st.in_size (Int32.of_int used_out);
		       st.in_crc <- 
			 Zlib.update_crc st.in_crc out_buf out_pos used_out;
		       
		       k := !k + used_in;
		       
		       if finished then (
			 let crc = input_int32() in
			 let size = input_int32() in
			 if st.in_crc <> crc then
			   raise(Gzip.Error("CRC mismatch, data corrupted"));
			 if st.in_size <> size then
			   raise(Gzip.Error("size mismatch, data corrupted"));
			 dispose_in st;
			 loop := false
		       );
		       used_out
		    ) in
		
		if used_out > 0 then loop := false;
	      done;
	      
	      st.in_hdr_parsed <- true;
	      Netbuffer.delete incoming 0 !k
	    with
	      | Buffer_underrun ->
		  if at_eof then (
		    dispose_in_ignore st;
		    raise(Gzip.Error("premature end of file, not a gzip file"))
		  )
		    (* else: do nothing in this case *)
	      | error ->
		  dispose_in_ignore st; raise error
	  )
	    
      | None ->
	  raise(Gzip.Error "zlib stream is already disposed")
  )


class inflating_pipe () =
  let stream = Zlib.inflate_init false in
  let st =
    { in_hdr_parsed = false;
      in_stream = Some stream;
      in_size = 0l;
      in_crc = 0l;
    } in
  let () = Gc.finalise dispose_in_ignore st in
  Netchannels.pipe ~conv:(inflating_conv st) ()


class input_inflate ch =
  Netchannels.input_filter
    ch
    (new inflating_pipe ())


class output_inflate ch =
  Netchannels.output_filter
    (new inflating_pipe ())
    ch


type out_state =
    { mutable out_hdr_written : bool;
      mutable out_stream : Zlib.stream option;
      mutable out_size: int32;
      mutable out_crc: int32;
    }


let dispose_out st =
  match st.out_stream with
    | Some stream ->
	Zlib.deflate_end stream;
	st.out_stream <- None;
    | None -> 
	()


let dispose_out_ignore st =
  try dispose_out st with _ -> ()



let write_int32 nb n =
  let r = ref n in
  for i = 1 to 4 do
    Netbuffer.add_char nb (Char.chr ((Int32.to_int !r) land 0xff));
    r := Int32.shift_right_logical !r 8
  done


let deflating_conv st incoming at_eof outgoing =
  match st.out_stream with
    | Some stream ->
	( try

	    if not st.out_hdr_written then (
	      let hdr = "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\xff" in
	      Netbuffer.add_string outgoing hdr;
	      st.out_hdr_written <- true;
	    );
	    
	    let loop = ref true in

	    if not at_eof && Netbuffer.length incoming = 0 then
	      loop := false;
	    
	    while !loop do
	      let in_buf = Netbuffer.unsafe_buffer incoming in
	      let in_len = Netbuffer.length incoming in
	      let (_:int) =
		Netbuffer.add_inplace
		  outgoing
		  (fun out_buf out_pos out_len ->
		     let (finished, used_in, used_out) =
		       try
			 Zlib.deflate 
			   stream in_buf 0 in_len out_buf out_pos out_len 
			   (if at_eof then Zlib.Z_FINISH else Zlib.Z_NO_FLUSH)
		       with 
			 | Zlib.Error(_, "buffer error") ->
			     (false, 0, 0)
			 |Zlib.Error(_, msg) ->
			    raise (Gzip.Error("error during compression")) in
		     
		     st.out_size <- Int32.add st.out_size (Int32.of_int used_in);
		     st.out_crc <- Zlib.update_crc st.out_crc in_buf 0 used_in;
		     
		     Netbuffer.delete incoming 0 used_in;
		     
		     if at_eof && finished then loop := false;
		     used_out
		  ) in
	      if not at_eof then loop := false
	    done;
	    
	    if at_eof then (
	      write_int32 outgoing st.out_crc;
	      write_int32 outgoing st.out_size;
	      dispose_out st
	    )
	  with
	    | error ->
		dispose_out_ignore st; raise error
	)

    | None ->
	if Netbuffer.length incoming > 0 then
	  failwith "zlib stream is already disposed"


class deflating_pipe ?(level=6) () =
  let stream = Zlib.deflate_init level false in
  let st =
    { out_hdr_written = false;
      out_stream = Some stream;
      out_size = 0l;
      out_crc = 0l;
    } in
  let () = Gc.finalise dispose_out_ignore st in
  Netchannels.pipe ~conv:(deflating_conv st) ()


class input_deflate ?level ch =
  Netchannels.input_filter
    ch
    (new deflating_pipe ?level ())


class output_deflate ?level ch =
  Netchannels.output_filter
    (new deflating_pipe ?level ())
    ch


let () =
  Netcompression.register
    ~iana_name:"gzip"
    ~decoder:(fun () -> new inflating_pipe())
    ~encoder:(fun () -> new deflating_pipe ?level:None ())
    ()


let init() = ()
