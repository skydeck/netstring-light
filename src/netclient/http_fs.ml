(* $Id: http_fs.ml 1661 2011-08-28 22:45:55Z gerd $ *)

(* TODO:
   - factor streaming get/put out
 *)


open Printf
module StrSet = Set.Make(String)


type read_flag =
    [ Netfs.read_flag | `Header of (string*string)list ]

type read_file_flag =
    [ Netfs.read_file_flag | `Header of (string*string)list ]

type write_flag =
    [ Netfs.write_flag | `Header of (string*string)list ]

type write_file_flag =
    [ Netfs.write_file_flag | `Header of (string*string)list ]

type any_wflag = [ write_flag | write_file_flag ]

class type http_stream_fs =
object
  method read : read_flag list -> string -> Netchannels.in_obj_channel
  method read_file : read_file_flag list -> string -> Netfs.local_file
  method write : write_flag list -> string -> Netchannels.out_obj_channel
  method write_file : write_file_flag list -> string -> Netfs.local_file -> unit
  method last_response_header : Nethttp.http_header
  method pipeline : Http_client.pipeline
  method translate : string -> string

  method path_encoding : Netconversion.encoding option
  method path_exclusions : (int * int) list
  method nominal_dot_dot : bool
  method size : Netfs.size_flag list -> string -> int64
  method test : Netfs.test_flag list -> string -> Netfs.test_type -> bool
  method test_list : Netfs.test_flag list -> string -> Netfs.test_type list -> bool list
  method remove : Netfs.remove_flag list -> string -> unit
  method rename : Netfs.rename_flag list -> string -> string -> unit
  method symlink : Netfs.symlink_flag list -> string -> string -> unit
  method readdir : Netfs.readdir_flag list -> string -> string list
  method readlink : Netfs.readlink_flag list -> string -> string
  method mkdir : Netfs.mkdir_flag list -> string -> unit
  method rmdir : Netfs.rmdir_flag list -> string -> unit
  method copy : Netfs.copy_flag list -> string -> string -> unit
  method cancel : unit -> unit
end

(* ensure this is a subtype of Netfs.stream_fs *)
let _f (x : http_stream_fs) =
  ignore(x :> Netfs.stream_fs)


let http_body ~open_value_rd ~open_value_wr =
object(self)
  method open_value_rd = open_value_rd
  method open_value_wr = open_value_wr
  method value = failwith "value: not supported here"
  method store = failwith "store: not supported here"
  method finalize() = ()
  method ro = false
  method set_value _ = failwith "set_value: not supported here"
end


let buffer_body ?(add_sub_string = Netpagebuffer.add_sub_string)
                buf eof ondata onempty drop : Netmime.mime_body =
  (* ondata: is called whenever new data becomes available. The arg is
     the number of buffered bytes
     onempty: is called when the input side runs out of data
     drop: this number of bytes are dropped
   *)
  let drop = ref drop in
  http_body
    ~open_value_rd:(
      fun () ->
	let (inch : Netchannels.rec_in_channel) =
	  ( object
	      method input s pos len =
		let buf_len = Netpagebuffer.length buf in
		if buf_len = 0 then (
		  if !eof then raise End_of_file else onempty()
		);
		let buf_len = Netpagebuffer.length buf in
		let n = min len buf_len in
		Netpagebuffer.blit_to_string buf 0 s pos n;
		Netpagebuffer.delete_hd buf n;
		n
	      method close_in() =
		()
	    end
	  ) in
	Netchannels.lift_in ~buffered:false (`Rec inch)
    )
    ~open_value_wr:(
      fun () ->
	let (out : Netchannels.rec_out_channel) =
	  ( object
	      method output s pos len =
		let d = Int64.to_int (min !drop (Int64.of_int len)) in
		drop := Int64.sub !drop (Int64.of_int d);
		let len' = len - d in
		if len' > 0 then (
		  add_sub_string buf s (pos + d) len';
		  ondata(Netpagebuffer.length buf)
		);
		len
	      method flush() = ()
	      method close_out() = 
		eof := true;
		ondata(Netpagebuffer.length buf)
	    end
	  ) in
	Netchannels.lift_out ~buffered:false (`Rec out)
    )
  

let discarding_body() : Netmime.mime_body =
  http_body  
    ~open_value_rd:(
      fun () ->
	let (inch : Netchannels.rec_in_channel) =
	  ( object
	      method input s pos len =
		raise End_of_file
	      method close_in() =
		()
	    end
	  ) in
	Netchannels.lift_in ~buffered:false (`Rec inch)
    )
    ~open_value_wr:(
      fun () ->
	let (out : Netchannels.rec_out_channel) =
	  ( object
	      method output s pos len =
		len
	      method flush() = ()
	      method close_out() = ()
	    end
	  ) in
	Netchannels.lift_out ~buffered:false (`Rec out)
    )


let drop_out_channel ch drop =
  (* drops the first n bytes written to it, and the remaining bytes are
     sent to ch
   *)
  let drop = ref drop in
  let out =
    ( object(self)
	method output s pos len =
	  let d = Int64.to_int (min !drop (Int64.of_int len)) in
	  drop := Int64.sub !drop (Int64.of_int d);
	  let len' = len - d in
	  ch # really_output s (pos+d) len';
	  len'
	method flush() = ch # flush()
	method close_out() = ch # close_out()
      end
    ) in
  Netchannels.lift_out ~buffered:false (`Rec out)


let rec find_file_members =
  function
    | Nethtml.Element(e,atts,subl) ->
	let m =
	  if String.lowercase e = "a" then (
	    try
	      let href = List.assoc "href" atts in
	      if href="" || href.[0] = '/' then raise Not_found;
	      try
		let i = String.index href '/' in
		if i+1=String.length href then
		  [String.sub href 0 (String.length href-1)]
		else
		  []
	      with Not_found -> [href]
	    with Not_found -> []
	  )
	  else [] in
	m @ find_file_members_in_list subl
    | Nethtml.Data _ -> []
and find_file_members_in_list l =
  List.flatten
    (List.map find_file_members l)


let unique_str_list l =
  let set =
    List.fold_left (fun acc s -> StrSet.add s acc) StrSet.empty l in
  StrSet.elements set


exception Interrupt


let is_error_response ?(precondfailed = Unix.EPERM) path call =
  match call#status with
    | `Unserved -> None
    | `Successful -> None
    | `Http_protocol_error e -> Some e
    | `Redirection | `Client_error | `Server_error ->
	( match call # response_status with
	    | `Not_found ->
		Some(Unix.Unix_error(Unix.ENOENT, "Http_fs", path))
	    | `Forbidden | `Unauthorized ->
		Some(Unix.Unix_error(Unix.EACCES, "Http_fs", path))
	    | `Precondition_failed ->
		Some(Unix.Unix_error(precondfailed, "Http_fs", path))
	    | _ ->
		Some(Unix.Unix_error(Unix.EPERM, "Http_fs", path))
	)


let find_flag f flags =
  let rec loop l =
    match l with
      | flag :: l' ->
	  ( match f flag with
	      | None -> loop l'
	      | Some x -> x
	  )
      | [] ->
	  raise Not_found in
  loop flags


class http_fs
	?(config_pipeline = fun p -> ())
	?(streaming = false)
	?tmp_directory
	?tmp_prefix
	?(path_encoding = `Enc_utf8)
        ?(enable_read_for_directories=false)
	?(enable_ftp=false)
	(* ?(is_error_response = is_error_response) *)
	base_url : http_stream_fs =
  let p = new Http_client.pipeline in
  let () =
    if enable_ftp then (
      let ftp_syn = Hashtbl.find Neturl.common_url_syntax "ftp" in
      let opts = p # get_options in
      let opts' =
	{ opts with
	    Http_client.schemes = opts.Http_client.schemes @
	    [ "ftp", ftp_syn, Some 21, Http_client.proxy_only_cb_id ]
	} in
      p # set_options opts'
    ) in
  let () = config_pipeline p in
  let base_url_ends_with_slash =
    base_url <> "" && base_url.[String.length base_url-1] = '/' in
  let einval path detail =
    raise(Unix.Unix_error(Unix.EINVAL, detail, path)) in
  let translate path =
    if path = "" then
      einval path "Http_fs: path is empty";
    if path.[0] <> '/' then
      einval path "Http_fs: path is not absolute";
    if String.contains path '\000' then
      einval path "Http_fs: path contains NUL byte";
    ( try
	Netconversion.verify path_encoding path
      with
	| Netconversion.Malformed_code_at _ ->
	    einval path "Http_fs: path is not properly encoded"
    );
    let npath = Neturl.norm_path(Neturl.split_path path) in
    let npath_s = Neturl.join_path npath in
    ( match npath with
	| "" :: ".." :: _ -> (* CHECK: maybe ENOENT? *)
	    einval path "Http_fs: path starts with /.."
	| _ -> ()
    );
    if base_url_ends_with_slash then
      base_url ^ String.sub npath_s 1 (String.length npath_s - 1)
    else
      base_url ^ npath_s
  in
  let handle_error ?precondfailed path call =
    match call#status with
      | `Unserved -> assert false
      | `Successful -> assert false
      | _ ->
	  ( match is_error_response ?precondfailed path call with
	      | None -> 
		  failwith "Http_fs: No response received but \
                            is_error_response does not indicate an error"
	      | Some e ->
		  raise e
	  )
  in
  let is_dir_url url =
    url <> "" && url.[String.length url - 1] = '/' in
  let check_dir url path =
    if is_dir_url url then
      raise(Unix.Unix_error(Unix.EISDIR, "Http_fs", path)) in
  let run () =
    try p#run()
    with Interrupt -> () in
  let last_response_header = ref None in

  let cancel_flag = ref (ref false) in
  

object(self)
  method path_encoding = Some path_encoding
  method path_exclusions = [0,0; 47,47]
  method nominal_dot_dot = true
  method pipeline = p
  method translate = translate

  method last_response_header =
    match !last_response_header with
      | None ->
	  raise Not_found
      | Some hdr ->
	  hdr

  method read flags path =
    let url = translate path in
    let call = new Http_client.get url in
    let g = Unixqueue.new_group p#event_system in
    let req_hdr = call # request_header `Base in
    let skip = 
      try find_flag (function `Skip p -> Some p | _ -> None) flags
      with Not_found -> 0L in
    if skip > 0L then
      Nethttp.Header.set_range req_hdr (`Bytes[Some skip, None]);
    call # set_accept_encoding();
    let header =
      try find_flag (function `Header h -> Some h | _ -> None) flags
      with Not_found -> [] in
    List.iter (fun (n,v) -> req_hdr # update_field n v) header;
    last_response_header := None;

    if streaming || List.mem `Streaming flags then (
      let onempty() = () in
      let page_size = Netsys_mem.pagesize in
      let buf = Netpagebuffer.create page_size in
      let eof = ref false in
      let running = ref true in  (* prevents that Interrupt escapes *)
      let ondata n =
	if not !eof then
	  Unixqueue.once p#event_system g 0.0
	    (fun () -> if !running then raise Interrupt) in
      let cur_ch = ref None in
      let call_done = ref false in
      call # set_response_body_storage
	(`Body (fun () -> 
		  (* Check whether this is the last body *)
		  last_response_header := Some(call#response_header);
		  match is_error_response path call with
		    | None ->
			if !cur_ch <> None then
			  failwith "Http_fs: unexpected reconnect";
			let code = call#response_status_code in
			let drop =
			  (* Maybe Range headers are not supported: *)
			  if code = 200 && skip > 0L then skip else 0L in
			let body = buffer_body buf eof ondata onempty drop in
			cur_ch := Some (body # open_value_rd());
			body
		    | Some _ ->
			discarding_body()
	       ));
      (* We cannot reconnect in streaming mode :-( *)
      call # set_reconnect_mode Http_client.Request_fails;
      p # add_with_callback call (fun _ -> call_done := true);
      (* Wait until data is available, or the whole call is done (in case
	 of error)
       *)
      try
	run();
	match !cur_ch with
	  | None ->
	      (* Error *)
	      handle_error path call  (* raise exception *)
	  | Some c_ch ->
	      (* Success *)
	      if not enable_read_for_directories then
		check_dir (call # effective_request_uri) path;
	      let (ch : Netchannels.rec_in_channel) =
		( object
		    method input s pos len =
		      while (not !call_done && not !eof && 
			       Netpagebuffer.length buf < 16 * page_size)
		      do
			run()
		      done;
		      if !call_done then run();   (* ensure proper shutdown *)
		      ( try
			  c_ch # input s pos len
			with
			  | End_of_file ->
			      (* check for pending error *)
			      ( match is_error_response path call with
				  | None -> raise End_of_file
				  | Some e -> handle_error path call
			      )
		      )
		    method close_in() =
		      try
			p # reset();
			while not !call_done do run() done;
			running := false;
			(* We ignore any pending error here *)
		      with
			| err -> running := false; raise err
		  end
		) in
	      Netchannels.lift_in ~buffered:true (`Rec ch)
      with
	| err -> running := false; raise err
    )
    else (
      let cur_tmp = ref None in 
      call # set_response_body_storage
	(`Body (fun () -> 
		  last_response_header := Some(call#response_header);
		  (* Check whether this is the last body *)
		  match is_error_response path call with
		    | None ->
			let (tmp_name, inch, outch) =
			  Netchannels.make_temporary_file 
			    ?tmp_directory ?tmp_prefix () in
			cur_tmp := Some (tmp_name, inch);
			let code = call#response_status_code in
			let drop =
			  (* Maybe Range headers are not supported: *)
			  if code = 200 && skip > 0L then skip else 0L in
			http_body
			  ~open_value_rd:(fun () -> 
					    new Netchannels.input_channel inch)
			  ~open_value_wr: (
			    fun () ->
			      drop_out_channel
             		   	(new Netchannels.output_channel outch)
				drop)
		    | Some _ ->
			discarding_body()
	       ));
      p # add call;
      run();
      match !cur_tmp with
	| None ->
	    (* Error *)
	    handle_error path call  (* raise exception *)
	| Some (tmp_name,c_ch) ->
	    (* Success *)
	    Unix.unlink tmp_name;  (* CHECK Win32 *)
	    if not enable_read_for_directories then
	      check_dir (call # effective_request_uri) path;
	    call # response_body # open_value_rd()
    )

  method read_file flags path =
    let url = translate path in
    let call = new Http_client.get url in
    let req_hdr = call # request_header `Base in
    call # set_accept_encoding();
    let header =
      try find_flag (function `Header h -> Some h | _ -> None) flags
      with Not_found -> [] in
    List.iter (fun (n,v) -> req_hdr # update_field n v) header;
    last_response_header := None;

    let cur_tmp = ref None in 
    call # set_response_body_storage
      (`Body (fun () -> 
		last_response_header := Some(call#response_header);
		(* Check whether this is the last body *)
		match is_error_response path call with
		  | None ->
		      let (tmp_name, inch, outch) =
			Netchannels.make_temporary_file 
			  ?tmp_directory ?tmp_prefix () in
		      close_in inch;
		      cur_tmp := Some tmp_name;
		      http_body
			~open_value_rd:(fun () -> 
					  new Netchannels.input_channel inch)
			~open_value_wr: (fun () ->
             				   new Netchannels.output_channel outch)
		  | Some _ ->
		      discarding_body()
	     ));
    p # add call;
    run();
    match !cur_tmp with
      | None ->
	  (* Error *)
	  handle_error path call  (* raise exception *)
      | Some tmp_name ->
	  (* Success *)
	  let close() =
	    try Unix.unlink tmp_name with _ -> () in
	  if not enable_read_for_directories then
	    check_dir (call # effective_request_uri) path;
	  ( object
	      method filename = tmp_name
	      method close = close
	    end
	  )

  method write flags path =
    self # write_impl (flags :> any_wflag list) path None

  method write_file flags path local =
    ignore(self # write_impl (flags :> any_wflag list) path (Some local))

  method private write_impl flags path local_opt =
    let this_cancel_flag = !cancel_flag in
    let url = translate path in
    let call = new Http_client.put_call in
    call # set_request_uri url;
    let g = Unixqueue.new_group p#event_system in
    let req_hdr = call # request_header `Base in
    req_hdr # update_field "Content-Type" "application/octet-stream";
    req_hdr # update_field "Expect" "100-continue";
    let header =
      try find_flag (function `Header h -> Some h | _ -> None) flags
      with Not_found -> [] in
    List.iter (fun (n,v) -> req_hdr # update_field n v) header;
    last_response_header := None;

    let create_flag = List.mem `Create flags in
    let trunc_flag = List.mem `Truncate flags in
    let excl_flag = List.mem `Exclusive flags in

    if not create_flag && not trunc_flag then
      einval path "Http_fs.write: you need to request either file creation \
                   or file truncation";

    let precondfailed = ref Unix.EPERM in

    if create_flag && excl_flag then (
      req_hdr # update_field "If-None-Match" "*";
      (* = do PUT only if the file does not exist *)
      precondfailed := Unix.EEXIST;
    );
    if create_flag && not excl_flag && not trunc_flag then (
      req_hdr # update_field "If-None-Match" "*";
      (* = do PUT only if the file does not exist *)
      precondfailed := Unix.EPERM;
    );
    if not create_flag then (
      req_hdr # update_field "If-Match" "*";
      (* = do PUT only if the file exists *)
      precondfailed := Unix.ENOENT;
    );

    let precondfailed = !precondfailed in
	    
    last_response_header := None;
    if (streaming || List.mem `Streaming flags) && local_opt = None then (
      (* We cannot reconnect in streaming mode :-( *)
      call # set_reconnect_mode Http_client.Request_fails;
      (* We have to use chunked transfer encoding: *)
      req_hdr # update_field "Transfer-Encoding" "chunked";

      let page_size = Netsys_mem.pagesize in
      let buf = Netpagebuffer.create page_size in
      let eof = ref false in
      let added = ref false in
      let running = ref true in  (* prevents that Interrupt escapes *)

      let ondata n = 
	if n>=16*page_size || !eof then (
	  if not !added then (
	    p # add call;
	    added := true
	  );
	  if !eof then (
	    (* last chunk: *)
	    Netpagebuffer.add_string buf "0\r\n\r\n"
	  );
	  run();
	  if !eof then (
	    running := false;
	    (* check for errors *)
	    last_response_header := Some(call#response_header);
	    match is_error_response ~precondfailed path call with
	      | None -> ()
	      | Some e -> handle_error ~precondfailed path call
	  )
	) in
      let onempty () =
	Unixqueue.once p#event_system g 0.0
	  (fun () -> if !running then raise Interrupt) in
      let add_sub_string buf s pos len =
	(* Create a chunk: *)
	Netpagebuffer.add_string buf (sprintf "%x\r\n" len);
	Netpagebuffer.add_sub_string buf s pos len;
	Netpagebuffer.add_string buf "\r\n";
      in
      let body = buffer_body ~add_sub_string buf eof ondata onempty 0L in
      call # set_request_body body;

      body # open_value_wr()
    )
    else (
      let (fname, mk_return, close) =
	match local_opt with
	  | None ->
	      let (n,inch,outch) =
		Netchannels.make_temporary_file 
		  ?tmp_directory ?tmp_prefix () in
	      close_in inch;
	      let mkr onclose =
		new Netchannels.output_channel ~onclose outch in
	      let close() =
		(try Unix.unlink n with _ -> ()) in
	      (n, mkr, close)
	  | Some local ->
	      let mkr onclose =
		onclose();
		new Netchannels.output_null() in
	      (local#filename, mkr, local#close) in

      let onclose() =
	if !this_cancel_flag then
	  close()
	else (
	  let st = Unix.LargeFile.stat fname in
	  req_hdr # update_field
	    "Content-length" (Int64.to_string st.Unix.LargeFile.st_size);
	  call # set_request_body (new Netmime.file_mime_body fname);
	  ( try
	      p # add call;
	      run();
	      close()
	    with e -> close(); raise e
	  );
	  last_response_header := Some(call#response_header);
	  match is_error_response ~precondfailed path call with
	    | None ->
		()
	    | Some e ->
		raise e
	)
      in
      
      mk_return onclose
    )

  method cancel() =
    (* This cancellation affects all [write]s that were started until
       now...
     *)
    let this_cancel_flag = !cancel_flag in
    this_cancel_flag := true;
    (* All new [write]s are not cancelled, of course: *)
    cancel_flag := (ref false)

  method size _ path =
    last_response_header := None;
    let url = translate path in
    let call = new Http_client.head url in
    p#add call;
    p#run();
    last_response_header := Some(call#response_header);
    match is_error_response path call with
      | None ->
	  ( try
	      Int64.of_string(call # response_header # field "Content-length")
	    with
	      | Not_found ->
		  raise(Unix.Unix_error(Unix.ESPIPE,"Http_fs",path))
	      | _ ->
		  raise(Http_client.Bad_message
			  ("Field Content-length: Parse error"))
	  )
      | Some _ ->
	  handle_error path call

  method test flags path t =
    List.hd(self # test_list flags path [t])

  method test_list flags path tl =
    last_response_header := None;
    let url = translate path in
    let call = new Http_client.head url in
    p#add call;
    p#run();
    last_response_header := Some(call#response_header);
    match is_error_response path call with
      | None ->
	  let is_dir =
	    is_dir_url(call # effective_request_uri) in
	  let not_empty =
	    try
	      Int64.of_string
		(call # response_header # field "Content-length") > 0L
	    with
	      | _ -> false in
	  List.map
	    (function
	       | `N -> true
	       | `E -> true
	       | `F -> not is_dir
	       | `D -> is_dir
	       | `H -> false
	       | `R -> true
	       | `W -> false
	       | `X -> is_dir
	       | `S -> not_empty
	    )
	    tl
      | Some _ ->
	  (* We only raise protocol exceptions *)
	  match call#status with
	    | `Unserved -> assert false
	    | `Http_protocol_error e -> raise e
	    | `Successful -> assert false
	    | `Redirection | `Client_error | `Server_error ->
		List.map (fun _ -> false) tl

  method readdir _ path =
    let fail() =
      (* We generally return ENOTDIR - meaning we cannot access this as
	 directory.
       *)
      raise(Unix.Unix_error(Unix.ENOTDIR,"Http_fs.readdir",path)) in
    last_response_header := None;
    let path1 =
      if path <> "" && path.[String.length path - 1] <> '/' then
	path ^ "/"
      else
	path in
    let url = translate path1 in
    let call = new Http_client.get url in
    p#add call;
    p#run();
    last_response_header := Some(call#response_header);
    match is_error_response path call with
      | None ->
	  (* The is_dir_url test only works if the server redirects to a
	     URL ending with a slash. Some servers don't do this.
	   *)
	  (* if not (is_dir_url(call # effective_request_uri)) then fail(); *)
	  (* Get the MIME type and the charset of the result: *)
	  let (cont_type, charset) =
	    try
	      let (cont_type, params) = call#response_header#content_type() in
	      let charset = 
		try Mimestring.param_value(List.assoc "charset" params)
		with Not_found -> "US-ASCII" in
	      (* FIXME: We could also look into the doc *)
	      (cont_type, charset)
	    with _ -> fail() in
	  (* we only support text/html: *)
	  if String.lowercase cont_type <> "text/html" then fail();
	  (* check if we know the encoding: *)
	  let enc =
	    try Netconversion.encoding_of_string charset
	    with _ -> fail() in
	  (* convert to UTF-8 *)
	  let text0 = call#response_body#value in
	  let text =
	    try
	      Netconversion.convert ~in_enc:enc ~out_enc:`Enc_utf8 text0
	    with
	      | _ -> fail() in
	  (* Now parse this *)
	  let html = Nethtml.parse (new Netchannels.input_string text) in
	  let names = find_file_members_in_list html in
	  (* Convert the names to our path encoding. Omit names with
	     conversion errors.
	   *)
	  let base_syntax = Neturl.ip_url_syntax in
	  let names1 =
	    List.flatten
	      (List.map
		 (fun name -> 
		    try
		      let u = 
			Neturl.parse_url ~base_syntax ~accept_8bits:true 
			  (Neturl.fixup_url_string name) in
		      let q1 = Neturl.url_path u in
		      (* Some URLs contain "%2f" in the path. We don't like
			 this
		       *)
		      if List.exists (fun s -> String.contains s '/') q1 then
			raise Not_found;
		      let q2 =
			if q1 <> [] && q1 <> [""] then
			  let r1 = List.rev q1 in
			  if List.hd r1 = "" then 
			    List.rev(List.tl r1)
			  else
			    q1
			else
			  q1 in
		      if q2 <> [] && not(List.mem "" q2) then (
			let qj = Neturl.join_path q2 in
			Netconversion.verify path_encoding qj;
			[ qj ]
		      )
		      else []
		    with _ -> []
		 )
		 names
	      ) in
	  let names2 = "." :: ".." :: names1 in
	  unique_str_list names2
      | Some(Unix.Unix_error(Unix.ENOENT,_,_)) ->
	  fail()   (* prefer ENOTDIR in this case *)
      | Some _ ->
	  handle_error path call

  method remove flags path =
    last_response_header := None;
    if List.mem `Recursive flags then
      raise(Unix.Unix_error(Unix.EINVAL,
			    "Http_fs.remove: recursion not supported",
			    path));
    let url = translate path in
    let call = new Http_client.get url in
    p#add call;
    p#run();
    last_response_header := Some(call#response_header);
    match is_error_response path call with
      | None ->
	  ()
      | Some _ ->
	  handle_error path call

  method copy _ path1 path2 = assert false (* TODO *)

  method rename _ path1 path2 =
    raise(Unix.Unix_error(Unix.ENOSYS, "Http_fs.rename", path1))

  method readlink _ path =
    raise(Unix.Unix_error(Unix.ENOSYS, "Http_fs.readlink", path))

  method symlink _ path1 path2 = 
    raise(Unix.Unix_error(Unix.ENOSYS, "Http_fs.symlink", path1))

  method mkdir _ path = 
    raise(Unix.Unix_error(Unix.ENOSYS, "Http_fs.mkdir", path))

  method rmdir _ path = 
    raise(Unix.Unix_error(Unix.ENOSYS, "Http_fs.rmdir", path))

end

  
let http_fs = new http_fs
