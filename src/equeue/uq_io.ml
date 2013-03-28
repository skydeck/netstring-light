(* $Id: uq_io.ml 1692 2012-02-05 18:44:00Z gerd $ *)

open Uq_engines.Operators
open Printf

type string_like =
    [ `String of string
    | `Memory of Netsys_mem.memory
    ]


class type obj_buffer =
object
  method length : int
  method blit_out : int -> string_like -> int -> int -> unit
  method delete_hd : int -> unit
  method index_from : int -> char -> int
  method add : string_like -> int -> int -> unit
  method advance : int -> unit
  method page_for_additions : string_like * int * int
  method page_for_consumption : string_like * int * int
  method clear : unit -> unit
end


class type ['in_device] in_buffer_pre =
object
  method buffer : obj_buffer
  method eof : bool
  method set_eof : unit -> unit
  method start_fill_e : unit -> bool Uq_engines.engine
  method fill_e_opt : bool Uq_engines.engine option
    (* The current fill engine, or None *)
  method udevice : 'in_device
  method shutdown_e : unit -> unit Uq_engines.engine
  method inactivate : unit -> unit
  method event_system : Unixqueue.event_system
end


class type ['out_device] out_buffer_pre =
object
  method buffer : obj_buffer
  method eof : bool
  method max : int option
  method start_flush_e : unit -> unit Uq_engines.engine
  method flush_e_opt : unit Uq_engines.engine option
    (* The current flush engine, or None *)
  method write_eof_e : unit -> bool Uq_engines.engine
    (* The buffer must be empty before [write_eof_e] *)
  method udevice : 'out_device option
  method shutdown_e : float option -> unit Uq_engines.engine
  method inactivate : unit -> unit
  method event_system : Unixqueue.event_system
end

	  
type in_device =
    [ `Polldescr of Netsys.fd_style * Unix.file_descr * Unixqueue.event_system
    | `Multiplex of Uq_engines.multiplex_controller
    | `Async_in of Uq_engines.async_in_channel * Unixqueue.event_system
    | `Buffer_in of in_device in_buffer_pre
    | `Count_in of (int -> unit) * in_device
    ]


type out_device =
    [ `Polldescr of Netsys.fd_style * Unix.file_descr * Unixqueue.event_system
    | `Multiplex of Uq_engines.multiplex_controller
    | `Async_out of Uq_engines.async_out_channel * Unixqueue.event_system
    | `Buffer_out of out_device out_buffer_pre
    | `Count_out of (int -> unit) * out_device
    ]


type in_buffer = in_device in_buffer_pre
type out_buffer = out_device out_buffer_pre


type in_bdevice =
    [ `Buffer_in of in_buffer ]

type inout_device = [ in_device | out_device ]



exception Line_too_long


let rec device_esys0 =
  function
    | `Polldescr(_,_,esys) -> esys
    | `Multiplex mplex -> mplex#event_system
    | `Async_in(_,esys) -> esys
    | `Async_out(_,esys) -> esys
    | `Buffer_in b -> b#event_system
    | `Buffer_out b -> b#event_system
    | `Count_in(_,d) -> device_esys0 (d :> inout_device)
    | `Count_out(_,d) -> device_esys0 (d :> inout_device)


let device_esys d =
  device_esys0 (d :> inout_device)


let is_string =
  function
    | `String _ -> true
    | `Memory _ -> false

let rec device_supports_memory0 =
  function
    | `Polldescr(style,_,_) -> 
	( match style with
	    | `Read_write | `Recv_send _ | `Recv_send_implied ->
		true
	    | _ ->
		false
	)
    | `Multiplex mplex -> 
	mplex # mem_supported
    | `Async_in(_,esys) -> 
	false
    | `Async_out(_,esys) -> 
	false
    | `Buffer_in b -> 
	true
    | `Buffer_out b -> 
	true
    | `Count_in(_,d) -> device_supports_memory0 (d : in_device :> inout_device)
    | `Count_out(_,d) -> device_supports_memory0 (d : out_device :> inout_device)

let device_supports_memory d =
  device_supports_memory0 (d :> inout_device)


let mem_gread style fd m pos len =
  match style with
    | `Read_write ->
	Netsys_mem.mem_read fd m pos len
    | `Recv_send _ | `Recv_send_implied ->
	Netsys_mem.mem_recv fd m pos len []
    | _ ->
	failwith ("Uq_io: This fd style does not support `Memory: " ^ 
		    Netsys.string_of_fd_style style)


let mem_gwrite style fd m pos len =
  match style with
    | `Read_write ->
	Netsys_mem.mem_write fd m pos len
    | `Recv_send _ | `Recv_send_implied ->
	Netsys_mem.mem_send fd m pos len []
    | _ ->
	failwith ("Uq_io: This fd style does not support `Memory: " ^ 
		    Netsys.string_of_fd_style style)


let ach_input_e ch esys s pos len =
  (* case: async channel *)

  let (e, signal) = Uq_engines.signal_engine esys in

  let rec wait_for_input () =
    try
      let n = ch # input s pos len in
      if n > 0 || len = 0 then
	signal (`Done n)
      else (
	ch # request_notification
	  (fun () ->
	     wait_for_input();
	     false
	  )
      )
    with
      | error -> signal (`Error error)
  in

  wait_for_input();
  e


let rec buf_input_e b ms pos len =
  let bl = b#buffer#length in
  if bl > 0 || len = 0 then (
    let n = min len bl in
    b#buffer#blit_out 0 ms pos n;
    b#buffer#delete_hd n;
    eps_e (`Done n) b#event_system
  )
  else if b#eof then
    eps_e (`Error End_of_file) b#event_system
  else (
    (* Optimization: if len is quite large, bypass the buffer *)
    let d = b#udevice in
    if len >= 4096 && (device_supports_memory d || is_string ms) then
      dev_input_e d ms pos len
      >> (function
	    | `Error End_of_file -> 
		b#set_eof(); `Error End_of_file
	    | st -> st
	 )
    else
      let fe =
	match b#fill_e_opt with
	  | None -> b#start_fill_e ()
	  | Some fe -> fe in
      fe ++ (fun _ -> buf_input_e b ms pos len)
  )


and gread_e style fd ms pos len =
  try
    match ms with
      | `String s ->
	  let n = Netsys.gread style fd s pos len in
	  (n, n=0)
      | `Memory m ->
	  let n = mem_gread style fd m pos len in
	  (n, n=0)
  with
    | Unix.Unix_error((Unix.EAGAIN|Unix.EWOULDBLOCK),_,_) ->
	(0, false)

and dev_input_e (d : in_device) ms pos len =
  match d with
    | `Polldescr(style, fd, esys) ->
	new Uq_engines.input_engine
	  (fun fd -> 
	     let (n, eof) = gread_e style fd ms pos len in
	     if len > 0 && n = 0 && eof then raise End_of_file;
	     n
	  )
	  fd (-1.0) esys

    | `Multiplex mplex ->
	let (e, signal) = Uq_engines.signal_engine mplex#event_system in
	let cancel() =
	  if mplex#reading then mplex # cancel_reading() in
	( match ms with
	    | `String s ->
		mplex # start_reading
		  ~when_done:(fun xopt n ->
				match xopt with
				  | None -> 
				      signal (`Done n)
				  | Some Uq_engines.Cancelled ->
				      cancel(); signal `Aborted
				  | Some err -> signal (`Error err)
			     )
		  s pos len;
	    | `Memory m ->
		if mplex#mem_supported then
		  mplex # start_mem_reading
		    ~when_done:(fun xopt n ->
				  match xopt with
				    | None -> 
					signal (`Done n)
				    | Some Uq_engines.Cancelled ->
					cancel(); signal `Aborted
				    | Some err -> signal (`Error err)
			       )
		    m pos len
		else
		  signal
		    (`Error
		       (Failure "Uq_io: This mplex does not support `Memory"));
	);
	e >> (function
		| `Done n -> `Done n
		| `Error e -> `Error e 
		| `Aborted -> cancel(); `Aborted
	     )
		    

    | `Async_in (ch,esys) ->
	( match ms with
	    | `String s ->
		ach_input_e ch esys s pos len
	    | `Memory m ->
		eps_e
		  (`Error
		     (Failure "Uq_io: async channels do not support `Memory"))
		  esys
	)
	
    | `Buffer_in b ->
	buf_input_e b ms pos len

    | `Count_in(c,d) ->
	dev_input_e d ms pos len 
	>> (function
	      | `Done n -> c n; `Done n
	      | st -> st
	   )

let input_e d0 ms pos len =
  let d = (d0 :> in_device) in
  dev_input_e d ms pos len

let rec really_input_e d ms pos len =
  if len = 0 then
    eps_e (`Done ()) (device_esys d)
  else
    input_e d ms pos len ++ 
      (fun n -> really_input_e d ms (pos+n) (len-n))


let input_line_e ?(max_len = Sys.max_string_length) (`Buffer_in b) =
  let consume k1 k2 =
    if k2 > max_len then raise Line_too_long;
    let s = String.create k1 in
    b#buffer#blit_out 0 (`String s) 0 k1;
    b#buffer#delete_hd k2;
    s in
  let rec look_ahead eof =
    try
      let k = b#buffer#index_from 0 '\n' in
      let s = consume k (k+1) in
      eps_e (`Done s) b#event_system
    with
      | Not_found ->
	  if eof then (
	    let n = b#buffer#length in
	    if n = 0 then
	      eps_e (`Error End_of_file) b#event_system
	    else (
	      let s = consume n n in
	      eps_e (`Done s) b#event_system
	    )
	  )
	  else (
	    assert(not b#eof);
	    if b#buffer#length > max_len then
	      eps_e (`Error Line_too_long) b#event_system
	    else
	      let fe =
		match b#fill_e_opt with
		  | None -> b#start_fill_e ()
		  | Some fe -> fe in
	      fe ++ look_ahead
	  )
      | Line_too_long ->
	   eps_e (`Error Line_too_long) b#event_system
  in
  look_ahead b#eof


exception Cont of (unit -> string list Uq_engines.engine)

let input_lines_e ?(max_len = Sys.max_string_length) (`Buffer_in b) =
  let copy_string i l =
    let s = String.create l in
    b#buffer#blit_out i (`String s) 0 l;
    s in
  let consume k =
    b#buffer#delete_hd k in
  let rec look_ahead i acc eof =
    try
      let k = b#buffer#index_from i '\n' in
      if k-i+1 > max_len then raise Line_too_long;
      let s = copy_string i (k-i) in
      raise(Cont(fun () -> look_ahead (k+1) (s::acc) eof))
    with
      | Not_found ->
	  if eof then (
	    let n = b#buffer#length in
	    if n = 0 then (
	      assert(acc = []);
	      eps_e (`Error End_of_file) b#event_system
	    )
	    else (
	      let s = copy_string i (n-i) in
	      if n-i > max_len then raise Line_too_long;
	      consume n;
	      eps_e (`Done (List.rev (s :: acc))) b#event_system
	    )
	  )
	  else (
	    assert(not b#eof);
	    if acc <> [] then (
	      consume i;
	      eps_e (`Done (List.rev acc)) b#event_system
	    ) else (
	      assert(i = 0);
	      if b#buffer#length > max_len then raise Line_too_long;
	      let fe =
		match b#fill_e_opt with
		  | None -> b#start_fill_e ()
		  | Some fe -> fe in
	      fe ++ (look_ahead 0 [])
	    )
	  )
      | Line_too_long ->
	   eps_e (`Error Line_too_long) b#event_system
      | Cont f ->  (* make the recursion tail-recursive *)
	  f ()
  in
  look_ahead 0 [] b#eof


let ach_output_e ch esys s pos len =
  (* case: async channel *)

  let (e, signal) = Uq_engines.signal_engine esys in

  let rec wait_for_output () =
    try
      let n = ch # output s pos len in
      if n > 0 || len = 0 then
	signal (`Done n)
      else (
	ch # request_notification
	  (fun () ->
	     wait_for_output();
	     false
	  )
      )
    with
      | error -> signal (`Error error)
  in

  wait_for_output();
  e


let rec buf_output_e b ms pos len =
  if b # eof then
    eps_e
      (`Error (Failure "Uq_io: Buffer already closed for new data"))
      b#event_system
  else (
    let bl = b#buffer#length in
    (* Optimization: if len is large, try to bypass the buffer *)
    match b#udevice with
      | Some d when (
	  bl=0 && len >= 4096 && (device_supports_memory d || is_string ms)
	) ->
	  dev_output_e d ms pos len
      | _ ->
	  let n =
	    match b # max with
	      | None -> len
	      | Some m -> max (min len (m - bl)) 0 in
	  if n > 0 || len = 0 then (
	    b#buffer#add ms pos n;
	    eps_e (`Done n) b#event_system
	  )
	  else (
	    let fe =
	      match b#flush_e_opt with
		| None -> b#start_flush_e ()
		| Some fe -> fe in
	    fe ++ (fun _ -> buf_output_e b ms pos len)
	  )
  )
    

and dev_output_e (d : out_device) ms pos len =
  match d with
    | `Polldescr(style, fd, esys) ->
	new Uq_engines.output_engine
	  (fun fd -> 
	     match ms with
	       | `String s ->
		   Netsys.gwrite style fd s pos len
	       | `Memory m ->
		   mem_gwrite style fd m pos len
	  )
	  fd (-1.0) esys

    | `Multiplex mplex ->
	let (e, signal) = Uq_engines.signal_engine mplex#event_system in
	let cancel() =
	  if mplex#writing then mplex # cancel_writing() in
	( match ms with
	    | `String s ->
		mplex # start_writing
		  ~when_done:(fun xopt n ->
				match xopt with
				  | None -> signal (`Done n)
				  | Some Uq_engines.Cancelled ->
				      cancel(); signal `Aborted
				  | Some err -> signal (`Error err)
			     )
		  s pos len;
	    | `Memory m ->
		if mplex#mem_supported then
		  mplex # start_mem_writing
		    ~when_done:(fun xopt n ->
				  match xopt with
				    | None -> signal (`Done n)
				    | Some Uq_engines.Cancelled ->
					cancel(); signal `Aborted
				    | Some err -> signal (`Error err)
			       )
		    m pos len
		else
		  signal
		    (`Error
		       (Failure "Uq_io: This mplex does not support `Memory"));
	);
	e >> (function
		| `Done n -> `Done n
		| `Error e -> `Error e 
		| `Aborted -> cancel(); `Aborted
	     )

    | `Async_out (ch,esys) ->
	( match ms with
	    | `String s ->
		ach_output_e ch esys s pos len
	    | `Memory m ->
		eps_e
		  (`Error
		     (Failure "Uq_io: async channels do not support `Memory"))
		  esys
	)
	
    | `Buffer_out b ->
	buf_output_e b ms pos len

    | `Count_out(c,d) ->
	dev_output_e d ms pos len 
	>> (function
	      | `Done n -> c n; `Done n
	      | st -> st
	   )

let output_e d ms pos len =
  dev_output_e (d :> out_device) ms pos len


let rec really_output_e d ms pos len =
  if len = 0 then
    eps_e (`Done ()) (device_esys d)
  else
    output_e d ms pos len ++ 
      (fun n -> really_output_e d ms (pos+n) (len-n))

let output_string_e d s =
  really_output_e d (`String s) 0 (String.length s)

let output_memory_e d m =
  really_output_e d (`Memory m) 0 (Bigarray.Array1.dim m)

let output_netbuffer_e d b =
  let s = Netbuffer.unsafe_buffer b in
  really_output_e d (`String s) 0 (Netbuffer.length b)

let flush_e d =
  match (d :> out_device) with
    | `Buffer_out b ->
	( match b#flush_e_opt with
	    | None -> b#start_flush_e ()
	    | Some fe -> fe
	)
    | _ ->
	eps_e (`Done()) (device_esys d)

let rec write_eof0_e d =
  match d with
    | `Polldescr(style, fd, esys) ->
	eps_e (`Done false) esys
    | `Multiplex mplex ->
	let (e, signal) = Uq_engines.signal_engine mplex#event_system in
	let cancel() =
	  if mplex#writing then mplex # cancel_writing() in
	if mplex # supports_half_open_connection then
	  mplex # start_writing_eof 
	    ~when_done:(fun xopt ->
			  match xopt with
			    | None -> signal (`Done true)
			    | Some Uq_engines.Cancelled ->
				cancel(); signal `Aborted
			    | Some error -> signal (`Error error)
		       )
	    ()
	else
	  signal (`Done false);
	e >> (function
		| `Done n -> `Done n
		| `Error e -> `Error e 
		| `Aborted -> cancel(); `Aborted
	     )
    | `Async_out (ch,esys) ->
	eps_e (`Done false) esys
    | `Buffer_out b ->
	flush_e d ++
	  (fun () -> b#write_eof_e())
    | `Count_out(_,d) ->
	write_eof0_e d

let write_eof_e d =
  write_eof0_e (d :> out_device)



let rec shutdown0_e ?linger d =
  match d with
    | `Polldescr(style, fd, esys) ->
	Netsys.gclose style fd;
	eps_e (`Done()) esys
    | `Multiplex mplex ->
	if mplex#reading then
	  mplex#cancel_reading();
	if mplex#writing then
	  mplex#cancel_writing();
	let (e, signal) = Uq_engines.signal_engine mplex#event_system in
	let cancel() =
	  if not mplex#shutting_down then mplex # cancel_shutting_down() in
	mplex # start_shutting_down
	  ?linger
	  ~when_done:(fun xopt ->
			match xopt with
			  | None ->
			      mplex#inactivate();
			      signal (`Done())
			  | Some Uq_engines.Cancelled ->
			      cancel(); signal `Aborted
			  | Some error ->
			      signal (`Error error)
		     )
	  ();
	e >> (function
		| `Done n -> `Done n
		| `Error e -> `Error e 
		| `Aborted -> cancel(); `Aborted
	     )
    | `Async_in (ch,esys) ->
	ch # close_in();
	eps_e (`Done()) esys
    | `Async_out (ch,esys) ->
	ch # close_out();
	eps_e (`Done()) esys
    | `Buffer_in b ->
	b # shutdown_e ()
    | `Buffer_out b ->
	flush_e (`Buffer_out b) ++ (fun _ -> b # shutdown_e linger)
    | `Count_in(_,d) ->
	shutdown0_e ?linger (d :> [in_device | out_device])
    | `Count_out(_,d) ->
	shutdown0_e ?linger (d :> [in_device | out_device])

let shutdown_e ?linger d =
  shutdown0_e ?linger (d :> [in_device | out_device])

let rec inactivate0 d =
  match d with
    | `Polldescr(style, fd, esys) ->
	Netsys.gclose style fd
    | `Multiplex mplex ->
	mplex#inactivate()
    | `Async_in (ch,esys) ->
	ch # close_in()
    | `Async_out (ch,esys) ->
	ch # close_out()
    | `Buffer_in b ->
	b # inactivate ()
    | `Buffer_out b ->
	b # inactivate ()
    | `Count_in(_,d) ->
	inactivate0 (d :> inout_device)
    | `Count_out(_,d) ->
	inactivate0 (d :> inout_device)

let inactivate d =
  inactivate0 (d :> inout_device)

let mem_obj_buffer small_buffer =
  let psize = 
    if small_buffer then 
      Netsys_mem.small_block_size else Netsys_mem.default_block_size in
  let buf = 
    Netpagebuffer.create psize in
  ( object
      method length = Netpagebuffer.length buf
      method blit_out bpos ms pos len =
	match ms with
	  | `String s -> Netpagebuffer.blit_to_string buf bpos s pos len
	  | `Memory m -> Netpagebuffer.blit_to_memory buf bpos m pos len
      method delete_hd n =
	Netpagebuffer.delete_hd buf n
      method index_from pos c =
	Netpagebuffer.index_from buf pos c
      method add ms pos len =
	match ms with
	  | `String s -> Netpagebuffer.add_sub_string buf s pos len
	  | `Memory m -> Netpagebuffer.add_sub_memory buf m pos len
      method advance n =
	Netpagebuffer.advance buf n
      method page_for_additions =
	let (m,pos,len) = Netpagebuffer.page_for_additions buf in
	(`Memory m, pos, len)
      method page_for_consumption =
	let (m,pos,len) = Netpagebuffer.page_for_consumption buf in
	(`Memory m, pos, len)
      method clear() =
	Netpagebuffer.clear buf
    end
  )

let str_obj_buffer small_buffer =
  let bufsize = 
    if small_buffer then 4096 else 65536 in
  let buf =
    Netbuffer.create bufsize in
  ( object
      method length = Netbuffer.length buf
      method blit_out bpos ms pos len =
	match ms with
	  | `String s -> Netbuffer.blit_to_string buf bpos s pos len
	  | `Memory m -> Netbuffer.blit_to_memory buf bpos m pos len
      method delete_hd n =
	Netbuffer.delete buf 0 n
      method index_from pos c =
	Netbuffer.index_from buf pos c
      method add ms pos len =
	match ms with
	  | `String s -> Netbuffer.add_sub_string buf s pos len
	  | `Memory m -> Netbuffer.add_sub_memory buf m pos len
      method advance n =
	Netbuffer.advance buf n
      method page_for_additions =
	let (s,pos,len) = Netbuffer.area_for_additions buf in
	(`String s, pos, len)
      method page_for_consumption =
	let s = Netbuffer.unsafe_buffer buf in
	(`String s, 0, Netbuffer.length buf)
      method clear() =
	Netbuffer.clear buf
    end
  )
    

let create_in_buffer ?(small_buffer=false) d0 =
  let d = (d0 :> in_device) in
  let esys =
    device_esys d in
  let buf =
    if device_supports_memory d then
      mem_obj_buffer small_buffer
    else
      str_obj_buffer small_buffer in
  let eof = 
    ref false in
  let fill_e_opt =
    ref None in
object
  method buffer = buf
  method eof = !eof
  method set_eof() = eof := true

  method start_fill_e () =
    assert(!fill_e_opt = None);
    if !eof then
      eps_e (`Done true) esys
    else (
      let (ms,pos,len) = buf # page_for_additions in
      let e =
	input_e d ms pos len
	++ (fun n ->
	      assert(n > 0);
	      buf # advance n;
	      fill_e_opt := None;
	      eps_e (`Done false) esys
	   )
	>> (function
	      | `Done flag -> `Done flag
	      | `Error End_of_file -> 
		  eof := true; `Done true
	      | `Error error -> `Error error
	      | `Aborted -> `Aborted
	   ) in
      fill_e_opt := Some e;
      e
    )

  method fill_e_opt =
    !fill_e_opt

  method shutdown_e () =
    shutdown_e d

  method inactivate() =
    buf#clear();
    inactivate d

  method udevice = d
  method event_system = esys
end


let in_buffer_length (b:in_buffer) =
  b#buffer#length

let in_buffer_blit (b:in_buffer) bpos ms mspos len =
  b#buffer#blit_out bpos ms mspos len

let in_buffer_fill_e (b:in_buffer)  =
  match b#fill_e_opt with
    | None -> b#start_fill_e ()
    | Some fe -> fe


let create_out_buffer ?(small_buffer=false) ~max d0 =
  let d = (d0 :> out_device) in
  let esys =
    device_esys d in
  let buf =
    if device_supports_memory d then
      mem_obj_buffer small_buffer
    else
      str_obj_buffer small_buffer in
  let eof = 
    ref false in
  let flush_e_opt =
    ref None in

  let rec flush_e n =
    if n > 0 then (
      let (ms,pos,len) = buf # page_for_consumption in
      let len' = min len n in
      output_e d ms pos len' ++
	(fun k ->
	   buf # delete_hd k;
	   flush_e (n-k)
	)
    )
    else
      eps_e (`Done ()) esys in

 object
  method buffer = buf
  method eof = !eof
  method max = max

  method start_flush_e() =
    assert (!flush_e_opt = None);
    let e = 
      flush_e (buf#length)
      >> (fun st -> flush_e_opt := None; st) in
    flush_e_opt := Some e;
    e

  method flush_e_opt = !flush_e_opt

  method write_eof_e () =
    if buf#length = 0 then
      write_eof_e d
    else
      eps_e 
	(`Error (Failure "Uq_io: called write_eof_e with non-empty buffer"))
	esys

  method shutdown_e linger =
    shutdown_e ?linger d
    
  method inactivate () =
    buf#clear();
    inactivate d

  method udevice = Some d
  method event_system = esys
end


let copy_e ?(small_buffer=false) ?len ?len64 d_in d_out =
  let d_in_esys = device_esys d_in in
  let d_out_esys = device_esys d_out in
  if d_in_esys <> d_out_esys then
    invalid_arg "Uq_io.copy_e: devices must use the same event system";
  let esys = d_in_esys in

  let ms, ms_len, free_ms =
    if device_supports_memory d_in && device_supports_memory d_out then (
      let m, f = 
	Netsys_mem.pool_alloc_memory2 
	  (if small_buffer then Netsys_mem.small_pool 
	   else Netsys_mem.default_pool) in
      (`Memory m, Bigarray.Array1.dim m, f)
    )
    else (
      let s = String.create (if small_buffer then 4096 else 65536) in
      (`String s, String.length s, (fun () -> ()))
    ) in
  (* Note that calling free_ms only accelerates that ms is recognized
     as free after the copy is done. It is not necessary to call it.
   *)

  let rec push_data p n =
    if n = 0 then
      eps_e (`Done ()) esys
    else
      output_e d_out ms p n ++ (fun k -> push_data (p+k) (n-k)) in

  let count = ref 0L in
  let eff_len =
    match len, len64 with
      | None, None -> None
      | Some n, None -> Some(Int64.of_int n)
      | None, Some n -> Some n
      | Some n1, Some n2 -> Some(min (Int64.of_int n1) n2) in

  let rec pull_data() =
    let n =
      match eff_len with
	| None -> 
	    ms_len
	| Some l -> 
	    Int64.to_int( min (Int64.of_int ms_len) (Int64.sub l !count)) in

    let ( >> ) = Uq_engines.fmap_engine in
    (* For a strange reason we need this - somewhere a generalization is
       missing
     *)

    if n=0 then (
      free_ms();
      eps_e (`Done !count) esys
    )
    else
      ( input_e d_in ms 0 n
	>> (function
	      | `Done n -> `Done(`Good n)
	      | `Error End_of_file -> `Done `Eof
	      | `Error error -> free_ms(); `Error error
	      | `Aborted -> free_ms(); `Aborted
	   )
	: [`Good of int | `Eof] Uq_engines.engine
      ) ++
	(function
	   | `Good n ->
	       count := Int64.add !count (Int64.of_int n);
	       push_data 0 n ++ (fun () -> pull_data())
	   | `Eof ->
	       free_ms();
	       eps_e (`Done !count) esys
	) in
  pull_data()

  
let eof_as_none =
  function
    | `Done x -> `Done(Some x)
    | `Error End_of_file -> `Done None
    | `Error e -> `Error e
    | `Aborted -> `Aborted


let filter_out_buffer ~max (p : Netchannels.io_obj_channel) d0 : out_buffer =
  let small_buffer = true in
  let d = (d0 :> out_device) in
  let esys =
    device_esys d in
  let buf = str_obj_buffer small_buffer in
  let eof = 
    ref false in
  let flush_e_opt =
    ref None in

  let rec do_flush_e() =
    let q = ref 0 in
    if buf#length > 0 then (
      assert(not !eof);
      (* First copy everything from buf to p: *)
      let (ms,pos,len) = buf # page_for_consumption in
      let s =
	match ms with
	  | `String s -> s 
	  | `Memory _ -> assert false in
      q := 1;
      let n = p # output s pos len in
      q := 2;
      buf # delete_hd n;
      (* Copy from p to d: *)
      let p_dev =
	`Async_in(new Uq_engines.pseudo_async_in_channel p, esys) in
      ( copy_e p_dev d
	>> (function
	      | `Done _ -> `Done ()
	      | `Error Netchannels.Buffer_underrun -> `Done ()
	      | `Error err -> `Error err
	      | `Aborted -> `Aborted
	   )
      ) ++ do_flush_e
    )
    else 
      if !eof then (
	q := 3;
	p # close_out();
	q := 4;
	let p_dev =
	  `Async_in(new Uq_engines.pseudo_async_in_channel p, esys) in
	copy_e p_dev d
	>> (fun st -> 
	      p#close_in();
	      match st with
		| `Done _ -> `Done()
		| `Error err -> `Error err
		| `Aborted -> `Aborted
	   )
      )
      else (
	eps_e (`Done()) esys
      ) 
  in

object(self)
  method buffer = buf
  method eof = !eof
  method max = max

  method start_flush_e() =
    assert (!flush_e_opt = None);
    let e = 
      do_flush_e ()
      >> (fun st ->
	    flush_e_opt := None; 
	    st
	 ) in
    flush_e_opt := Some e;
    e

  method flush_e_opt = 
    match !flush_e_opt with
      | None -> None
      | Some e ->
	  assert(match e#state with
		   | `Done _ -> false
		   | _ -> true
		);
	  Some e

  method write_eof_e () =
    eof := true;
    flush_e (`Buffer_out self)
    ++ (fun () ->
	  write_eof_e d
       )

  method shutdown_e linger =
    eof := true;
    flush_e (`Buffer_out self)
    ++ (fun () ->
	  shutdown_e ?linger d
       )
    
  method inactivate () =
    p#close_in();
    inactivate d

  method udevice = None
    (* It is not allowed to bypass this buffer *)
  method event_system = esys
end
