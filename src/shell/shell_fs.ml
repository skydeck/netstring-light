(* $Id: shell_fs.ml 1661 2011-08-28 22:45:55Z gerd $ *)

open Printf

type command_context =
    { sfs_command : string;
      sfs_stdin : Shell.producer;
      sfs_stdout : Shell.consumer;
      sfs_stderr : Shell.consumer;
      mutable sfs_status : Unix.process_status option;
    }

class type command_interpreter = 
object
  method exec : command_context -> unit
  method interrupt : unit -> unit
  method run : unit -> unit
end

class type shell_stream_fs =
object
  inherit Netfs.stream_fs
  method last_stderr : string
end


exception Interrupt

let ws_re = Netstring_str.regexp "[ \t\r\n]+"


let cmd_interpreter mk_command =
  let esys = Unixqueue.create_unix_event_system() in
  let g = Unixqueue.new_group esys in
  let cur_eng = ref None in
object(self) 
  method exec cctx =
    if !cur_eng <> None then
      failwith "Shell_fs.local_interpreter.exec: already running";
    let e =
      new Shell_uq.call_engine
	~ignore_error_code:true
	~stdin:cctx.sfs_stdin
	~stdout:cctx.sfs_stdout
	~stderr:cctx.sfs_stderr
	(mk_command cctx)
	esys in
    Uq_engines.when_state
      ~is_done:(fun _ -> 
		  cur_eng := None;
		  match List.rev(Shell_sys.processes e#job_instance) with
		    | p :: _ -> cctx.sfs_status <- Some (Shell_sys.status p)
		    | [] -> assert false
	       )
      ~is_error:(fun err ->
		   cur_eng := None;
		   match err with
		     | Shell.Subprocess_error ((_ :: _) as l) ->
			 let (_,st) = List.hd (List.rev l) in
			 cctx.sfs_status <- Some st
		     | _ ->
			 Unixqueue.once esys g 0.0 (fun () -> raise err)
		)
      e;
    cur_eng := Some e

  method interrupt() =
    Unixqueue.once esys g 0.0 (fun () -> raise Interrupt)
      
  method run() =
    try
      Unixqueue.run esys
    with Interrupt ->
      ()
end


let local_interpreter () =
  cmd_interpreter
    (fun cctx ->
       [ Shell.cmd
	   "/bin/sh"
	   [ "-c"; cctx.sfs_command ]
       ]
    )
	    

let ssh_interpreter ?(options=[ "-o"; "BatchMode yes"]) ?user ~host () =
  let uh =
    match user with
      | None -> host
      | Some u -> u ^ "@" ^ host in
  cmd_interpreter
    (fun cctx ->
       [ Shell.cmd
	   "ssh"
	   (options @ [ uh; cctx.sfs_command ])
       ]
    )


let output_stream_adapter ~ci ~close_in ~skip =
  let page_size = Netsys_mem.pagesize in

  let stdout_buf = Netpagebuffer.create page_size in
  let stdout_eof = ref false in
  let stdout_drop = ref false in
  let to_skip = ref skip in

  let consumer fd =
    try
      let n =
	Netpagebuffer.add_inplace stdout_buf (Netsys_mem.mem_read fd) in
      if !stdout_drop then
	Netpagebuffer.clear stdout_buf;
      if !to_skip > 0L then (
	let q = 
	  min !to_skip (Int64.of_int (Netpagebuffer.length stdout_buf)) in
	Netpagebuffer.delete_hd stdout_buf (Int64.to_int q);
	to_skip := Int64.sub !to_skip q;
      );
      if n = 0 then (
	stdout_eof := true;
	Unix.close fd;
      );
      if Netpagebuffer.length stdout_buf > 16 * page_size then
	ci#interrupt();
      not !stdout_eof
    with Unix.Unix_error(Unix.EINTR,_,_) -> true
  in

  let ch =
    ( object 
	method input s pos len =
	  if Netpagebuffer.length stdout_buf = 0 then (
	    if !stdout_eof then raise End_of_file;
	    ci#run();
	    if Netpagebuffer.length stdout_buf = 0 && !stdout_eof then
	      raise End_of_file;
	  );
	  let n = min len (Netpagebuffer.length stdout_buf) in
	  Netpagebuffer.blit_to_string stdout_buf 0 s pos n;
	  Netpagebuffer.delete_hd stdout_buf n;
	  n
	method close_in() =
	  stdout_drop := true;
	  Netpagebuffer.clear stdout_buf;
	  ci#run();
	  close_in()
      end
    ) in
  let ch' = 
    Netchannels.lift_in ~buffered:true (`Rec ch) in

  (Shell.to_function ~consumer (), ch')


let input_stream_adapter ~ci ~close_out =
  let page_size = Netsys_mem.pagesize in

  let stdin_buf = Netpagebuffer.create page_size in
  let stdin_eof = ref false in
  
  let producer fd =
    try
      let (m,pos,len) = Netpagebuffer.page_for_consumption stdin_buf in
      let n = Netsys_mem.mem_write fd m pos len in
      Netpagebuffer.delete_hd stdin_buf n;
      if Netpagebuffer.length stdin_buf = 0 then (
	if !stdin_eof then
	  Unix.close fd
	else
	  ci#interrupt()
      );
      not !stdin_eof
    with Unix.Unix_error(Unix.EINTR,_,_) -> true
  in
  
  let ch =
    ( object 
	method output s pos len =
	  Netpagebuffer.add_sub_string stdin_buf s pos len;
	  if Netpagebuffer.length stdin_buf > 16 * page_size then
	      ci#run();
	  len
	method flush() = 
	  ci#run()
	method close_out() =
	  stdin_eof := true;
	  ci#run();
	  close_out()
	end
    ) in
  let ch' =
    Netchannels.lift_out ~buffered:false (`Rec ch) in
  (Shell.from_function ~producer (), ch')
    

let execute ci cctx = ci # exec cctx

let wait ci = ci # run()


let slash_re = Netstring_str.regexp "/+"

let link_re = Netstring_str.regexp ".*? -> \\(.*\\)$"

exception Not_absolute

class shell_fs ?encoding ?(root="/") ?(dd_has_excl=false)
               ?tmp_directory ?tmp_prefix
               (ci : command_interpreter) : shell_stream_fs =
  let () =
    match encoding with
      | None -> ()
      | Some e ->
	  if not (Netconversion.is_ascii_compatible e) then
	    failwith
	      "Shell_fs.shell_fs: the encoding is not ASCII-compatible" in
  let check_component path c =
    let iter f s =
      match encoding with
	| None -> 
	    String.iter (fun c -> f (Char.code c)) s
	| Some e -> 
	    Netconversion.ustring_iter e f s in
    try
      let first = ref true in
      iter
	(fun code ->
	   if code = 0 || code = 47 || (!first && code = 45) then
	     raise (Unix.Unix_error(Unix.EINVAL,
				    "Shell_fs: invalid char in path",
				    path));
	   first := false
	)
	c
    with Netconversion.Malformed_code ->
      raise (Unix.Unix_error(Unix.EINVAL,
			     "Shell_fs: path does not comply to charset encoding",
			     path)) in

  let check_and_norm_path p =
    try
      let l = Netstring_str.split_delim slash_re p in
      ( match l with
	  | [] ->  raise (Unix.Unix_error(Unix.EINVAL,
					  "Shell_fs: empty path",
					  p))
	  | "" :: first :: rest -> 
	      ()
	  | first :: rest ->
	      raise Not_absolute
      );
      List.iter (check_component p) l;
      let np = String.concat "/" l in
      np
    with 
      | Not_absolute ->
	  raise (Unix.Unix_error(Unix.EINVAL,
				 "Shell_fs: path not absolute",
				 p))
  in

  let get_sh_path p =
    let np = check_and_norm_path p in
    match root with
      | "" ->
	  (* erase the leading / *)
	  let s = String.sub np 1 (String.length np-1) in
	  if s = "" then "." else s
      | r ->
	  r ^ np
  in

  let stderr_buf = Buffer.create 80 in

  let bs = 65536 in

  let simple_exec cmd filename =
    (* Get stdout as string *)
(* prerr_endline ("cmd: " ^ cmd); *)
    let buf = Buffer.create 1000 in
    let cctx =
      { sfs_command = cmd;
	sfs_stdin = Shell.from_dev_null;
	sfs_stdout = Shell.to_buffer buf;
	sfs_stderr = Shell.to_buffer stderr_buf;
	sfs_status = None
      } in
    ci#exec cctx;
    ci#run();
    match cctx.sfs_status with
      | None ->
	  failwith "Shell_fs.simple_exec: no command status"
      | Some (Unix.WEXITED n) ->
	  (n, Buffer.contents buf)
      | Some _ ->
	  raise(Unix.Unix_error(Unix.EPERM,
				"Shell_fs.simple_exec",
				filename))
  in

object(self)
  
  method path_encoding = encoding
  method path_exclusions = [0,0; 47,47]
  method nominal_dot_dot = false
    
  method last_stderr = Buffer.contents stderr_buf

  method read flags filename =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let skip_d = 
      try
	List.find
	  (fun flag -> 
	     match flag with
	       | `Skip _ -> true
	       | _ -> false
	  ) 
	  flags 
      with Not_found -> `Skip 0L in
    let skip = match skip_d with `Skip n -> n | _ -> assert false in
    let qfn = Filename.quote fn in
    let cmd =
      sprintf "if test -e %s; then dd if=%s bs=%d skip=%Ld || exit 2; \
               else exit 1; fi"
	qfn qfn bs (Int64.div skip (Int64.of_int bs)) in

    let cctx =
      ref
	{ sfs_command = cmd;
	  sfs_stdin = Shell.from_dev_null;
	  sfs_stdout = Shell.to_dev_null;   (* updated later *)
	  sfs_stderr = Shell.to_buffer stderr_buf;
	  sfs_status = None
	} in

    let (stdout_consumer, ch) =
      output_stream_adapter 
	~ci 
	~close_in:(fun () ->
		     match !cctx.sfs_status with
		       | None ->
			   failwith "Shell_fs.read: no command status"
		       | Some (Unix.WEXITED 0) ->
			   ()
		       | Some (Unix.WEXITED 1) ->
			   raise(Unix.Unix_error(Unix.ENOENT,
						 "Shell_fs.read",
						 filename))
		       | Some _ ->
			   raise(Unix.Unix_error(Unix.EPERM,
						 "Shell_fs.read",
						 filename))
		  )
	~skip:(Int64.rem skip (Int64.of_int bs)) in

    cctx := { !cctx with sfs_stdout = stdout_consumer };
    ci#exec !cctx;
    ch

  method read_file flags filename =
    let (tmp_name, inch, outch) =
      Netchannels.make_temporary_file 
        ?tmp_directory ?tmp_prefix () in
    close_in inch;
    try
      Netchannels.with_out_obj_channel
	(new Netchannels.output_channel outch)
	(fun obj_outch ->
	   Netchannels.with_in_obj_channel
	     (self # read [] filename)
	     (fun obj_inch ->
		obj_outch # output_channel obj_inch
	     )
	);
      ( object
	  method filename = tmp_name
	  method close() =
	    try Sys.remove tmp_name with _ -> ()
	end
      )
    with
      | error ->
	  ( try Sys.remove tmp_name with _ -> ());
	  raise error

  method write flags filename =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let create_flag = List.mem `Create flags in
    let trunc_flag = List.mem `Truncate flags in
    let excl_flag = List.mem `Exclusive flags in

    if excl_flag && not dd_has_excl then
      raise(Unix.Unix_error(Unix.EINVAL,
			    "Shell_fs.write: no support for exclusive create",
			    filename));
    let notrunc_opt = 
      if trunc_flag then "" else "conv=notrunc" in

    let qfn = Filename.quote fn in
    let cmd =
      if create_flag then
	sprintf "dd of=%s bs=%d %s" qfn bs notrunc_opt
      else
	sprintf "if test -e %s; then dd of=%s bs=%d %s || exit 2; \
                 else exit 1; fi"
	  qfn qfn bs notrunc_opt in

    let cctx =
      ref
	{ sfs_command = cmd;
	  sfs_stdin = Shell.from_dev_null;   (* updated later *)
	  sfs_stdout = Shell.to_dev_null;
	  sfs_stderr = Shell.to_buffer stderr_buf;
	  sfs_status = None
	} in
    
    let stdin_producer, ch =
      input_stream_adapter
	~ci
	~close_out:(fun () ->
		      match !cctx.sfs_status with
			| None ->
			    failwith "Shell_fs.write: no command status"
			| Some (Unix.WEXITED 0) ->
			    ()
			| Some (Unix.WEXITED 1) ->
			    raise(Unix.Unix_error(Unix.ENOENT,
						  "Shell_fs.write",
						  filename))
			| Some _ ->
			    raise(Unix.Unix_error(Unix.EPERM,
						  "Shell_fs.write",
						  filename))
		   ) in
    
    cctx := { !cctx with sfs_stdin = stdin_producer };
    ci#exec !cctx;
    ch

  method write_file flags filename local =
    let flags' =
      List.map
	(function
	   | #Netfs.write_common as x -> (x :> Netfs.write_flag)
	   | _ -> `Dummy
	)
	flags in
    Netchannels.with_in_obj_channel
      (new Netchannels.input_channel (open_in_bin local#filename))
      (fun obj_inch ->
	 Netchannels.with_out_obj_channel
	   (self # write flags' filename)
	   (fun obj_outch ->
	      obj_outch # output_channel obj_inch
	   )
      )

  method size _ filename =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let qfn = Filename.quote fn in
    let cmd = 
      sprintf "if test -e %s; then ls -ndL %s || exit 2; else exit 1"
	qfn qfn in
    let (n, stdout) = simple_exec cmd filename in
    match n with
      | 0 ->
	  let ch = new Netchannels.input_string stdout in
	  let line = try ch#input_line() with End_of_file -> "" in
	  let fields = Netstring_str.split ws_re line in
	  ( match fields with
	      | _ :: _ :: _ :: _ :: size_str :: _ ->
		  Int64.of_string size_str
	      | _ ->
		  failwith "Shell_fs.size: unexpected output format of 'ls'"
	  )
      | 1 -> 
	  raise(Unix.Unix_error(Unix.ENOENT, "Shell_fs.size", filename))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.size", filename))

  method test flags filename t =
    List.hd (self#test_list flags filename [t])

  method test_list flags filename tl =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let qfn = Filename.quote fn in
    let link_flag = List.mem `Link flags in
    let cmd =
      sprintf "if test -e %s; then echo true; else echo false; fi; \
               if test -d %s; then echo true; else echo false; fi; \
               if test -f %s; then echo true; else echo false; fi; \
               if test -L %s; then echo true; else echo false; fi; \
               if test -s %s; then echo true; else echo false; fi; \
               if test -r %s; then echo true; else echo false; fi; \
               if test -w %s; then echo true; else echo false; fi; \
               if test -x %s; then echo true; else echo false; fi "
	qfn qfn qfn qfn qfn qfn qfn qfn in
    let (_, stdout) = simple_exec cmd filename in
    let ch = new Netchannels.input_string stdout in
    ( try
	let test_e = bool_of_string (ch#input_line ()) in
	let test_d = bool_of_string (ch#input_line ()) in
	let test_f = bool_of_string (ch#input_line ()) in
	let test_L = bool_of_string (ch#input_line ()) in
	let test_s = bool_of_string (ch#input_line ()) in
	let test_r = bool_of_string (ch#input_line ()) in
	let test_w = bool_of_string (ch#input_line ()) in
	let test_x = bool_of_string (ch#input_line ()) in
	List.map
	  (function
	     | `N -> test_e || test_L
	     | `E -> test_e || (link_flag && test_L)
	     | `D -> test_d && not (link_flag && test_L)
	     | `F -> test_f && not (link_flag && test_L)
	     | `H -> test_L
	     | `R -> test_r || (link_flag && test_L)
	     | `W -> test_w || (link_flag && test_L)
	     | `X -> test_x || (link_flag && test_L)
	     | `S -> test_s && not (link_flag && test_L)
	  )
	  tl
      with
	| _ ->
	    failwith "Shell_fs.test_list: unexpected script output"
    )

  method remove flags filename =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let qfn = Filename.quote fn in
    let rec_flag = List.mem `Recursive flags in
    let cmd =
      sprintf "if { test -e %s || test -L %s; }; then rm -f %s %s || exit 2; \
               else exit 1; fi"
	qfn qfn (if rec_flag then "-r" else "") qfn in
    let (n, stdout) = simple_exec cmd filename in
    match n with
      | 0 ->
	  ()
      | 1 -> 
	  raise(Unix.Unix_error(Unix.ENOENT, "Shell_fs.remove", filename))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.remove", filename))

  method rename flags old_name new_name =
    Buffer.clear stderr_buf;
    let old_fn = get_sh_path old_name in
    let old_qfn = Filename.quote old_fn in
    let new_fn = get_sh_path new_name in
    let new_qfn = Filename.quote new_fn in
    (* mv allows it to move into directories. This is not intended by this
       operation, so we have to check for it
     *)
    let cmd =
      sprintf "if { test -e %s || test -L %s; }; then \
                 if test -d %s; then                  \
                   exit 2;                            \
                 else                                 \
                   mv -f %s %s;                       \
                 fi;                                  \
               else                                   \
                 exit 1;                              \
               fi"
	old_qfn old_qfn
	new_qfn
	old_qfn new_qfn in
    let (n, stdout) = simple_exec cmd old_name in
    match n with
      | 0 ->
	  ()
      | 1 -> 
	  raise(Unix.Unix_error(Unix.ENOENT, "Shell_fs.rename", old_name))
      | 2 -> 
	  raise(Unix.Unix_error(Unix.EEXIST, "Shell_fs.rename", new_name))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.rename", old_name))


  method symlink flags old_name new_name =
    Buffer.clear stderr_buf;
    let old_fn = get_sh_path old_name in
    let old_qfn = Filename.quote old_fn in
    let new_fn = get_sh_path new_name in
    let new_qfn = Filename.quote new_fn in
    let cmd =
      sprintf "if { test -e %s || test -L %s; }; then exit 1; \
               else ln -s %s %s || exit 2; fi" 
	new_qfn new_qfn
	old_qfn new_qfn in
    let (n, stdout) = simple_exec cmd new_name in
    match n with
      | 0 ->
	  ()
      | 1 ->
	  raise(Unix.Unix_error(Unix.EEXIST, "Shell_fs.symlink", new_name))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.symlink", new_name))

  method readdir flags filename =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let qfn = Filename.quote fn in
    let cmd =
      sprintf "if test -d %s; then ls -a1 %s/. || exit 2; else exit 1; fi"
	qfn qfn in
    let (n, stdout) = simple_exec cmd filename in
    match n with
      | 0 ->
	  let ch = new Netchannels.input_string stdout in
	  Netchannels.lines_of_in_obj_channel ch
      | 1 ->
	  raise(Unix.Unix_error(Unix.ENOENT, "Shell_fs.readdir", filename))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.readdir", filename))

  method mkdir flags filename =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let qfn = Filename.quote fn in
    let path_flag = List.mem `Path flags in
    let nonexcl_flag = List.mem `Nonexcl flags in
    let cmd =
      if path_flag then
	sprintf "mkdir -p %s || exit 3" qfn
      else
	sprintf "if { test -e %s || test -L %s; }; then \
                   if test -d %s; then exit 1; else exit 2; fi; \
                 else mkdir %s || exit 3; fi"
	  qfn qfn
	  qfn
	  qfn in
    let (n, stdout) = simple_exec cmd filename in
    match n with
      | 0 ->
	  ()
      | 1 ->
	  if not nonexcl_flag then
	    raise(Unix.Unix_error(Unix.EEXIST, "Shell_fs.mkdir", filename))
      | 2 ->
	  raise(Unix.Unix_error(Unix.EEXIST, "Shell_fs.mkdir", filename))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.mkdir", filename))

  method rmdir flags filename =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let qfn = Filename.quote fn in
    let cmd =
      sprintf 
	"if { test -d %s && test ! -L %s; }; then \
           rmdir %s || exit 3; \
         else \
           if test -e %s; then exit 1; else exit 2; fi; \
         fi"
	qfn qfn qfn qfn in
    let (n, stdout) = simple_exec cmd filename in
    match n with
      | 0 ->
	  ()
      | 1 ->
	  raise(Unix.Unix_error(Unix.ENOTDIR, "Shell_fs.rmdir", filename))
      | 2 ->
	  raise(Unix.Unix_error(Unix.ENOENT, "Shell_fs.rmdir", filename))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.rmdir", filename))

  method copy flags old_name new_name =
    Buffer.clear stderr_buf;
    let old_fn = get_sh_path old_name in
    let old_qfn = Filename.quote old_fn in
    let new_fn = get_sh_path new_name in
    let new_qfn = Filename.quote new_fn in
    (* cp allows it to copy into directories. This is not intended by this
       operation, so we have to check for it
     *)
    let cmd =
      sprintf "if { test -e %s || test -L %s; }; then \
                 if test -d %s; then                  \
                   exit 2;                            \
                 else                                 \
                   cp -p %s %s;                       \
                 fi;                                  \
               else                                   \
                 exit 1;                              \
               fi"
	old_qfn old_qfn
	new_qfn
	old_qfn new_qfn in
    let (n, stdout) = simple_exec cmd old_name in
    match n with
      | 0 ->
	  ()
      | 1 -> 
	  raise(Unix.Unix_error(Unix.ENOENT, "Shell_fs.copy", old_name))
      | 2 -> 
	  raise(Unix.Unix_error(Unix.EEXIST, "Shell_fs.copy", new_name))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.copy", old_name))

  method readlink _ filename =
    Buffer.clear stderr_buf;
    let fn = get_sh_path filename in
    let qfn = Filename.quote fn in
    let cmd = 
      sprintf "if test -e %s; then ls -nd %s || exit 2; else exit 1; fi"
	qfn qfn in
    let (n, stdout) = simple_exec cmd filename in
    match n with
      | 0 ->
	  let ch = new Netchannels.input_string stdout in
	  let line = try ch#input_line() with End_of_file -> "" in
	  let fields = Netstring_str.split ws_re line in
	  ( match fields with
	      | perms :: _ ->
		  if perms <> "" && perms.[0] = 'l' then
		    (* look for leftmost occurrence of " -> <name>" *)
		    match Netstring_str.string_match link_re line 0 with
		      | None ->
			  raise(Unix.Unix_error(Unix.EINVAL, 
						"Shell_fs.readlink", filename))
		      | Some m ->
			  Netstring_str.matched_group m 1 line
		  else
		    raise(Unix.Unix_error(Unix.EINVAL, 
					  "Shell_fs.readlink", filename))
	      | _ ->
		  failwith "Shell_fs.size: unexpected output format of 'ls'"
	  )
      | 1 -> 
	  raise(Unix.Unix_error(Unix.ENOENT, "Shell_fs.readlink", filename))
      | _ ->
	  raise(Unix.Unix_error(Unix.EPERM, "Shell_fs.readlink", filename))

  method cancel() = ()

end


let shell_fs = new shell_fs
