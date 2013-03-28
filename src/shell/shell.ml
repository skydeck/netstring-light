(* $Id: shell.ml 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

let is_win32 =
  match Sys.os_type with
    | "Win32" -> true
    | _ -> false;;


let dev_null_name =
  if is_win32 then
    "NUL"
  else
    "/dev/null"



let stdin  = Unix.stdin;;
let stdout = Unix.stdout;;
let stderr = Unix.stderr;;

type assignment = Unix.file_descr * Unix.file_descr;;

let assign ~src ~target = (target,src);;
let ( >& ) src target = (target,src);;
let ( <& ) src target = (target,src);;
let assigned_pair (target,src) = (target,src);;

let command
      ?cmdname 
      ?arguments
      ?chdir
      ?environment
      ?descriptors
      ?assignments
      name =
  let filename = Shell_sys.lookup_executable name in
  Shell_sys.command
    ?cmdname
    ?arguments
    ?chdir
    ?environment
    ?descriptors
    ?assignments
    ~filename
    ()
;;


let cmd 
      ?cmdname 
      ?chdir
      ?environment
      ?descriptors
      ?assignments
      name args =
  command 
    ?cmdname
    ?chdir
    ?environment
    ?descriptors
    ?assignments
    ~arguments:(Array.of_list args)
    name
;;


exception Subprocess_error of (string * Unix.process_status) list;;

let () =
  Netexn.register_printer
    (Subprocess_error [])
    (fun e ->
       match e with
	 | Subprocess_error l ->
	     "Shell.Subprocess_error( [" ^ 
	       (String.concat "; " 
		  (List.map
		     (fun (cmd, ps) ->
			"\"" ^ String.escaped cmd ^ "\": " ^ 
			  match ps with
			    | Unix.WEXITED n -> 
				"WEXITED " ^ string_of_int n
			    | Unix.WSIGNALED n -> 
				"WSIGNALED " ^ string_of_int n
			    | Unix.WSTOPPED n ->
				"WSTOPPED " ^ string_of_int n
		     )
		     l)) ^ "] )"
	 | _ ->
	     assert false
    )

type consumer = 
    C_fun of (Unix.file_descr -> bool)
  | C_file of (bool * string)
  | C_fd of Unix.file_descr
  | C_dev_null
  | C_none

type producer = 
    P_fun of (Unix.file_descr -> bool)
  | P_file of string
  | P_fd of Unix.file_descr
  | P_dev_null
  | P_none


let from_string ?pos ?len ?epipe s =
  P_fun (Shell_sys.from_string ?pos:pos ?len:len ?epipe:epipe s);;

let from_stream ?epipe s =
  P_fun (Shell_sys.from_stream ?epipe:epipe s);;

let from_function ~producer () =
  P_fun producer;;

let from_file name =
  P_file name;;

let from_fd fd =
  P_fd fd;;

let from_dev_null =
  P_dev_null;;

let to_buffer b =
  C_fun (Shell_sys.to_buffer b);;

let to_function ~consumer () =
  C_fun consumer;;

let to_file ?(append = false) name =
  C_file (append,name);;

let to_fd fd =
  C_fd fd;;

let to_dev_null =
  C_dev_null;;


let rec iter_pairs f l =
  match l with
      [] -> ()
    | [ c ] -> ()
    | c1 :: c2 :: l' ->
	f c1 c2;
	iter_pairs f (c2 :: l')
;;

let rec last l =
  match l with
      [] -> failwith "last"
    | [ x ] -> x
    | x::l' -> last l'
;;

let rec change_last x' l =
  match l with
      [] -> failwith "change_last"
    | [ x ] -> [ x' ]
    | x::l' -> x :: change_last x' l'
;;


let try_to_install_job_handlers() =
  try
    Shell_sys.install_job_handlers()
  with
      Shell_sys.Already_installed -> ()
;;


let setup_job 
      ?(stdin = P_none)
      ?(stdout = C_none)
      ?(stderr = C_none)
      commands =

  let files_to_close = ref [] in

  let cleanup() =
    try
      List.iter Unix.close !files_to_close;
      files_to_close := []
    with
	ex ->
	  raise (Shell_sys.Fatal_error ex)
  in

  if commands = [] then invalid_arg "Shell.setup_job";

  try
    (* Is /dev/null needed? *) (* TODO *)

    let dev_null = ref Unix.stdin in
    if stdin = P_dev_null || stdout = C_dev_null || stderr = C_dev_null then 
    begin
      dev_null := Unix.openfile dev_null_name [ Unix.O_RDWR ] 0;
      files_to_close := !dev_null :: !files_to_close;
    end;

    (* If there is redirection from a file, change the first command: *)
    
    let new_stdin =
      match stdin with
	  P_fd fd ->
	    Some fd
	| P_file name ->
	    let fd = Unix.openfile name [ Unix.O_RDONLY ] 0 in
	    files_to_close := fd :: !files_to_close;
	    Some fd
	| P_dev_null ->
	    Some (!dev_null)
	| _ ->
	    None
    in
    
    let new_first_cmd =
      match new_stdin with
	  Some fd ->
	    let c = Shell_sys.copy_command (List.hd commands) in
	    Shell_sys.set_assignments 
	      c
	      ( (fd, Unix.stdin) :: (Shell_sys.get_assignments c) );
	    c
	| None ->
	    List.hd commands
    in
    
    let commands' = new_first_cmd :: (List.tl commands) in
    
    (* If there is stdout redirection to a file, change the last command: *)

    let mk_new_out out =
      match out with
	  C_fd fd ->
	    Some fd
	| C_file (append,name) ->
	    let app_flags = if append then [ Unix.O_APPEND ] else [] in
	    let std_flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
	    let fd = Unix.openfile name (app_flags @ std_flags) 0o777 in
	    files_to_close := fd :: !files_to_close;
	    Some fd
	| C_dev_null ->
	    Some (!dev_null)
	| _ ->
	    None
    in
    
    let new_last_cmd =
      match mk_new_out stdout with
	  Some fd ->
	    let c = Shell_sys.copy_command (last commands') in
	    Shell_sys.set_assignments 
	      c
	      ( (fd, Unix.stdout) :: (Shell_sys.get_assignments c) );
	    c
	| None ->
	    last commands'
    in
    
    let commands'' = change_last new_last_cmd commands' in
    
    (* If there is stderr redirection to a file, change all commands: *)

    let commands''' =
      match mk_new_out stderr with
	  Some fd ->
	    List.map
	      (fun c ->
		 let c' = Shell_sys.copy_command c in
		 Shell_sys.set_assignments 
		   c'
		   ( (fd, Unix.stderr) :: (Shell_sys.get_assignments c') );
		 c'
	      )
	      commands''
	| None ->
	    commands''
    in
    
    (* Set up the job: *)

    let j = Shell_sys.new_job () in
    
    List.iter
      (fun c -> Shell_sys.add_command c j)
      commands''';
    
    iter_pairs
      (fun c1 c2 -> Shell_sys.add_pipeline ~src:c1 ~dest:c2 j)
      commands''';

    begin match stdin with
	P_fun p ->
	  let first_cmd = List.hd commands''' in
	  Shell_sys.add_producer ~producer:p first_cmd j
      | _ ->
	  ()
    end;

    begin match stdout with
	C_fun c ->
	  let last_cmd = last commands''' in
	  Shell_sys.add_consumer ~consumer:c last_cmd j
      | _ -> 
	  ()
    end;

    begin match stderr with
	C_fun c ->
	  List.iter
	    (fun cmd ->
	       Shell_sys.add_consumer ~descr:Unix.stderr ~consumer:c cmd j)
	    commands'''
      | _ -> 
	  ()
    end;
    
    (j, !files_to_close)
  with
    | ex ->
	cleanup();
	raise ex
;;


let postprocess_job
      ?(ignore_error_code = false)
      ji =

  if Shell_sys.job_status ji <> Shell_sys.Job_ok then begin
    let can_ignore =
      List.for_all
	(fun p ->
	   match Shell_sys.status p with
	       Unix.WEXITED _ -> true
	     | _              -> false
	)
	(Shell_sys.processes ji)
    in
    
    if not (ignore_error_code && not can_ignore) then begin
      (* Error cannot be ignored, throw a Subprocess_error: *)
      let l =
	List.map
	  (fun p ->
	     let s = Shell_sys.status p in
	     let c = Shell_sys.command_of_process p in
	     let name = Shell_sys.get_filename c in
	     name, s
	  )
	  (Shell_sys.processes ji)
      in
      raise (Subprocess_error l)
    end
  end
;;


let call 
      ?ignore_error_code
      ?mode
      ?stdin
      ?stdout
      ?stderr
      commands =

  let files_to_close = ref [] in

  let cleanup() =
    try
      List.iter Unix.close !files_to_close;
      files_to_close := []
    with
	ex ->
	  raise (Shell_sys.Fatal_error ex)
  in

  if commands = [] then invalid_arg "Shell.call";

  try_to_install_job_handlers();

  let (j, files) = setup_job ?stdin ?stdout ?stderr commands in
  files_to_close := files;

  let ji =
    try
      (* Invoke the job: *)
      let ji = Shell_sys.call_job ?mode:mode j in
      cleanup();           (* Close files no longer needed *)
      ji
    with
	(Shell_sys.Fatal_error _) as ex ->
	  (* We do not try to cleanup *)
	  raise ex
      | ex ->
	  cleanup();
	  raise ex
  in

  (* Interpret the result: *)
  postprocess_job ?ignore_error_code ji  (* may raise Subprocess_error *)
;;
