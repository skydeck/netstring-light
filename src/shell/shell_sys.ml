(* $Id: shell_sys.ml 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


open Printf

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Shell_sys" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Shell_sys" Debug.enable

let () =
  Netlog.Debug.register_module "Shell_sys" Debug.enable


let is_win32 =
  match Sys.os_type with
    | "Win32" -> true
    | _ -> false;;


(*** environments ***)

(* The following functions assume that environments are typically small
 * such that optimized code to access them is normally not necessary.
 * I mean they are rather inefficient...
 *)

type environment = string array ref;;

let create_env () = ref [| |];;

let copy_env e =
  ref
    (Array.map
       String.copy
       !e
    )
;;

let set_env e a =
  let a' = Array.map String.copy a in
  e := a'
;;

let get_env e =
  ! (copy_env e)
;;

let iter_env ~f e =
  Array.iter
    f
    !e
;;

let iter_env_vars ~f e =
  let dest_var s =
    let k = String.index s '=' in
    (String.sub s 0 k, String.sub s (k+1) (String.length s - k - 1))
  in
  Array.iter
    (fun s ->
       try
	 let v, x = dest_var s in
	 f v x
       with
	   Not_found -> ()
    )
    !e
;;

let set_env_var e v x =
  begin try
    ignore(String.index v '=');
    invalid_arg "Shell_sys.set_env_var"
  with
      Not_found -> ();
  end;
  try
    let k = ref 0 in
    iter_env_vars
      (fun v' x' ->
	 if v' = v then begin
	   !e.(!k) <- v ^ "=" ^ x;
	   raise Exit
	 end
	 else
	   incr k
      )
      e;
    e := Array.append !e [| v ^ "=" ^ x |]
  with
      Exit -> ()
;;

let get_env_var e v =
  let x = ref "" in
  try
    iter_env_vars
      (fun v' x' ->
	 if v' = v then begin
	   x := x';
	   raise Exit
	 end
      )
      e;
    raise Not_found
  with
      Exit -> !x
;;

let current_env () =
  ref (Unix.environment())
;;





(*** commands, command groups, processes and process groups ***)

exception Fatal_error of exn;;

let () =
  Netexn.register_printer
    (Fatal_error Not_found)
    (fun e ->
       match e with
	 | Fatal_error e' ->
	     "Shell_sys.Fatal_error(" ^ Netexn.to_string e' ^ ")"
	 | _ ->
	     assert false
    )

type command =
    { mutable c_cmdname     : string;
      mutable c_arguments   : string array;
      mutable c_directory   : string option;
      mutable c_environment : environment;
      mutable c_descriptors : Unix.file_descr list;
      mutable c_assignments : (Unix.file_descr * Unix.file_descr) list;
      mutable c_filename    : string;
    }
;;

let is_letter =
  function
    | 'a'..'z' -> true
    | 'A'..'Z' -> true
    | _ -> false


let name_with_dir name =
  if is_win32 then
    String.contains name '/' || 
      String.contains name '\\' || 
      (String.length name >= 2 && name.[1] = ':' && is_letter name.[0])
  else
    String.contains name '/'
    


let command
      ?cmdname
      ?(arguments = [||])
      ?chdir
      ?(environment = current_env())
      ?(descriptors = [ Unix.stdin; Unix.stdout; Unix.stderr ])
      ?(assignments = [])
      ~filename
      () =
  let cmdname' =
    match cmdname with
	None      -> if name_with_dir filename then
	               filename
  	             else
		       "./" ^ filename
      | Some name -> name
  in
  { c_cmdname = cmdname';
    c_arguments = arguments;
    c_directory = chdir;
    c_environment = environment;
    c_descriptors = descriptors;
    c_assignments = assignments;
    c_filename = filename;
  }
;;

let get_cmdname     c = c.c_cmdname     ;;
let get_arguments   c = c.c_arguments   ;;
let get_chdir       c = c.c_directory   ;;
let get_environment c = c.c_environment ;;
let get_descriptors c = c.c_descriptors ;;
let get_assignments c = c.c_assignments ;;
let get_filename    c = c.c_filename    ;;

let set_cmdname     c x = c.c_cmdname     <- x ;;
let set_arguments   c x = c.c_arguments   <- x ;;
let set_chdir       c x = c.c_directory   <- x ;;
let set_environment c x = c.c_environment <- x ;;
let set_descriptors c x = c.c_descriptors <- x ;;
let set_assignments c x = c.c_assignments <- x ;;
let set_filename    c x = c.c_filename    <- x ;;

let copy_command c =
  { c_cmdname     = String.copy c.c_cmdname;
    c_arguments   = Array.map String.copy c.c_arguments;
    c_directory   = c.c_directory;
    c_environment = copy_env c.c_environment;
    c_descriptors = c.c_descriptors;
    c_assignments = c.c_assignments;
    c_filename    = String.copy c.c_filename;
  }
;;

let is_executable_file name =
  let perms = 
    if is_win32 then [ Unix.R_OK ] else [ Unix.X_OK ] in
  ( not is_win32 ||
      Filename.check_suffix name ".exe" ||
      Filename.check_suffix name ".com"
  ) &&
    ( try
	Unix.access name perms;
	true
      with
	  Unix.Unix_error(_,_,_) -> false
    )
;;

let is_executable c = 
    is_executable_file c.c_filename;;

let split_path s =
  (* move to shell_misc.ml ? *)
  let rec split_at j k =
    if k >= String.length s then
      let u = String.sub s j (k-j) in
      [u]
    else
      if s.[k] = ':' then
	let u = String.sub s j (k-j) in
	u :: split_at (k+1) (k+1)
      else
	split_at j (k+1)
  in
  let l = split_at 0 0 in
  List.filter (fun s -> s <> "") l
;;

exception Executable_not_found of string;;

let posix_lookup_executable
      ?(path = split_path (try Unix.getenv "PATH" with Not_found -> ""))
      name =
  if name_with_dir name then begin
    if is_executable_file name then
      name
    else
      raise (Executable_not_found name)
  end
  else begin
    let foundname = ref "" in
    try
      List.iter
	(fun loc ->
	   let candidate = Filename.concat loc name in
	   if is_executable_file candidate then begin
	     foundname := candidate;
	     raise Exit
	   end
	)
	path;
      raise (Executable_not_found name)
    with
	Exit -> !foundname
  end
;;



let win32_lookup_executable ?path name =
  if name_with_dir name then (
    if is_executable_file name then
      name
    else
      raise (Executable_not_found name)
  )
  else (
    let p_opt =
      match path with
	| None -> None
	| Some p -> Some(String.concat ";" p) in
    Netsys_win32.search_path p_opt name (Some ".exe")
  )

let lookup_executable =
  if is_win32 then win32_lookup_executable else posix_lookup_executable


type group_action =
    New_bg_group
  | New_fg_group
  | Join_group of int
  | Current_group
;;

type fwd_mode =
    No_forward
  | Forward_to_process
  | Forward_to_group


type proc_ref =
  [ `POSIX of int * Unix.file_descr * Netsys_posix.watched_subprocess
  | `Win32 of Netsys_win32.w32_process * Unix.file_descr
  | `Dummy
  ]

type process =
    { p_command : command;
      mutable p_id : proc_ref;
      p_gid : int;
      mutable p_status : Unix.process_status option;
        (* None means: process is still running *)
      mutable p_abandoned : bool;
        (* true means: the SIGCHLD handler will watch the process, "wait"
	 * is no longer possible
	 *)
    }
;;

let dummy_process =
  { p_command = command "XXX" ();
    p_id = `Dummy;
    p_gid = 0;
    p_status = Some (Unix.WEXITED 0);
    p_abandoned = false;
  }
;;


let command_of_process p = p.p_command;;

let all_signals =
  [ Sys.sigabrt;
    Sys.sigalrm;
    Sys.sigfpe;
    Sys.sighup;
    Sys.sigill;
    Sys.sigint;
    (* Sys.sigkill; -- not modifiable *)
    Sys.sigpipe;
    Sys.sigquit;
    Sys.sigsegv;
    Sys.sigterm;
    Sys.sigusr1;
    Sys.sigusr2;
    Sys.sigchld;
    Sys.sigcont;
    (* Sys.sigstop; -- not modifiable *)
    Sys.sigtstp;
    Sys.sigttin;
    Sys.sigttou;
    Sys.sigvtalrm;
    Sys.sigprof; ];;


let posix_run
      ?(group = Current_group)
      ?(forward_mode = No_forward)
      ?(pipe_assignments = [])
      c =

  (* This [run] implementation bases on [Netsys_posix.spawn] *)
  
  let args = Array.append [| c.c_cmdname |] c.c_arguments in

  (* Signals:
   * - Keyboard signals SIGINT, SIGQUIT: the subprocess should inherit the
   *   current setting (ignore or terminate).
   * - SIGCHLD: this must be reset to default otherwise the subprocess will
   *   be confused ("wait" would not work as expected)
   * - other signals: it is good practice also to reset them to default
   *)

  let sig_actions =
    (Netsys_posix.Sig_mask []) ::
      List.flatten
        (List.map
	   (fun signo ->
	      if signo = Sys.sigint || signo = Sys.sigquit then
		[ ]  
		  (* keep them as-is. If a handler exists, it will be reset
		     to the default action by [exec]
		   *)
	      else
		[ Netsys_posix.Sig_default signo ]
	   )
	   all_signals
	) in

  (* Descriptor assignments. We have to translate the parallel
     pipe_assigmnents into a list of [dup2] operations. Also, we have
     to check which descriptors are kept open at all

     Order: First the parallel pipe_assignments are done, then the sequential
     c_assignments.
   *)

  let pipe_assignments =
    List.map
      (fun (from_fd, to_fd) -> (ref from_fd, to_fd))
                           (* such that from_fd can be altered *)
      pipe_assignments
  in

  (* Collect the descriptors that must not be closed by [exec] (final view),
     i.e. this is the set of descriptors that is finally shared by the
     forking and the forked process.
   *)
  let open_descr_ht = Hashtbl.create 50 in

  List.iter
    (fun (from_fd, to_fd) ->
       Hashtbl.replace open_descr_ht to_fd ())
    pipe_assignments;
  List.iter
    (fun (from_fd, to_fd) ->
       (* By removing from_fd we prevent that intermediate descriptors
	  are shared with the subprocess, e.g. fd2 := fd1; fd3 := fd2.
	  Here, only fd3 is automatically shared, but neither fd1 nor fd2.
	  In order to enable sharing, the user can put these descriptors
	  into c_descriptors.
	*)
       Hashtbl.remove  open_descr_ht from_fd;
       Hashtbl.replace open_descr_ht to_fd ())
    c.c_assignments;
  List.iter
    (fun fd ->
       Hashtbl.replace open_descr_ht fd ())
    c.c_descriptors;

  (* In this table we track the use of descriptors (dup2 tracking).
     We need to know at each step of the algorithm whether an fd is
     used or free. In order to get the starting point, we take
     the _final_ set (in open_descr_ht), and go backward.
     In the algorithm below, track_descr_ht is then updated by each dup2.

     If an fd is member of track_descr_ht it is used at the current step of the
     algorithm.
   *)
  let track_descr_ht = Hashtbl.create 50 in

  Hashtbl.iter       (* starting point: the fd's that remain finally open *)
    (fun fd _ -> Hashtbl.replace track_descr_ht fd ())
    open_descr_ht;
  List.iter          (* go backward: first the c_assignments *)
    (fun (from_fd, to_fd) ->
       (* Here we remove to_fd (being assigned, so overwritten), and we mark
	  from_fd as needed. We do this assignment by assignment, so for
	  chains fd2 := fd1; fd3 := fd2 the intermediate descriptor fd2
	  is initially a free descriptor.
	*)
       Hashtbl.remove  track_descr_ht to_fd;
       Hashtbl.replace track_descr_ht from_fd ())
    (List.rev c.c_assignments);
  (* The parallel pipe_assignments do not have an order, to we first
     remove all overwritten descriptors, and then add all source descriptors.
   *)
  List.iter
    (fun (from_fd, to_fd) -> 
       Hashtbl.remove  track_descr_ht to_fd;
    )
    pipe_assignments;
  List.iter
    (fun (from_fd, to_fd) -> 
       Hashtbl.replace track_descr_ht !from_fd ();
    )
    pipe_assignments;

  (* Here we manage additional descriptors that are required for emulating
     parallel assignment by sequential dup2's:
   *)
  let next_fd = ref 3 in
  let rec new_descriptor() =
    let fd = Netsys_posix.file_descr_of_int !next_fd in
    if (Hashtbl.mem track_descr_ht fd) then (
      incr next_fd;
      new_descriptor();
    ) else (
      Hashtbl.add track_descr_ht fd ();
      fd
    ) in
  let alloc_descriptor fd =
    Hashtbl.replace track_descr_ht fd () in
  let rel_descriptor fd =
    if Hashtbl.mem track_descr_ht fd then (
      Hashtbl.remove track_descr_ht fd;
      let ifd = Netsys_posix.int_of_file_descr fd in
      if ifd < !next_fd then next_fd := ifd
    ) in
  
  (* These are all destination fd's of [dup2]. *)
  let dest_descr_ht = Hashtbl.create 50 in

  let fd_actions = ref [] in    (* actions in reverse order *)

  (* Do first pipe_assignments. These are _parallel_ assignments, i.e.
   * if (fd1, fd2) and (fd2, fd3) are in the list, the first assginment
   * fd1 -> fd2 must not overwrite fd2, because the second assignment
   * fd2 -> fd3 refers to the original fd2.
   *)
  let rec assign_parallel fdlist =
    match fdlist with
      | (from_fd, to_fd) :: fdlist' ->
	  (* If to_fd occurs on the left side in fdlist', we must be
           * careful, and rename this descriptor.
	   *)
	  if !from_fd <> to_fd then (
	    if List.exists (fun (fd1,fd2) -> !fd1=to_fd) fdlist' then (
	      let new_fd = new_descriptor() in
	      List.iter
		(fun (fd1, fd2) -> if !fd1 = to_fd then fd1 := new_fd)
		fdlist';
	      fd_actions := 
		(Netsys_posix.Fda_dup2(to_fd, new_fd)) :: !fd_actions;
	      Hashtbl.replace dest_descr_ht new_fd ();
	    );
	    fd_actions := 
	      (Netsys_posix.Fda_dup2(!from_fd, to_fd)) :: !fd_actions;
	    alloc_descriptor to_fd;
	    Hashtbl.replace dest_descr_ht to_fd ();
	    (* It is not evident which from_fd can be released here.
	       The descriptors could be the source in a following
	       c_assignment, or even be shared with the subprocess
	       (in strange configurations). So better keep the hands
	       off.
	     *)
	  );
	  assign_parallel fdlist'
      | [] ->
	  ()
  in
  assign_parallel pipe_assignments;

  (* Also perform c.c_assignments; however this can be done in a
   * sequential way.
   *)
  List.iter
    (fun (from_fd, to_fd) ->
       if from_fd <> to_fd then (
	 fd_actions := 
	   (Netsys_posix.Fda_dup2(from_fd, to_fd)) :: !fd_actions;
	 alloc_descriptor to_fd;
	 Hashtbl.replace dest_descr_ht to_fd ();
	 (* We cannot release from_fd. It might be the source for another
	    assignment (e.g. fd2 := fd1; fd3 := fd1) or it might be finally
	    shared with the subprocess.
	  *)
       )
    )
    c.c_assignments;

  (* Close the descriptors that are not shared with this process: *)
  let max_open_ht = ref 2 in
  Hashtbl.iter
    (fun fd _ ->
       let ifd = Netsys_posix.int_of_file_descr fd in
       if ifd > !max_open_ht then max_open_ht := ifd
    )
    open_descr_ht;
  let keep_open = Array.create (!max_open_ht+1) false in
  Hashtbl.iter
    (fun fd _ ->
       let ifd = Netsys_posix.int_of_file_descr fd in
       keep_open.(ifd) <- true
    )
    open_descr_ht;
  fd_actions :=
    (Netsys_posix.Fda_close_except keep_open) :: !fd_actions;

  (* Clean up track_descr_ht after the above close operation: *)
  let to_release =
    Hashtbl.fold
      (fun fd _ acc ->
	 if not(Hashtbl.mem open_descr_ht fd) then fd :: acc else acc
      )
      track_descr_ht
      [] in
  List.iter (fun fd -> rel_descriptor fd) to_release;

  (* Clear the close-on-exec flag for the shared descriptors. There
     is no Fda_clear_close_on_exec, so we have to get this effect by
     using dup2 (i.e. dup2(fd, tmp_fd); dup2(tmp_fd, fd); close(tmp_fd) ).
     Note that dup2(fd,fd) is not sufficient (POSIX does not mention
     that the close-on-exec flag is cleared in this case).
   *)
  let clear_fd = new_descriptor() in
  Hashtbl.iter
    (fun fd _ ->
       if not (Hashtbl.mem dest_descr_ht fd) then
	 fd_actions := 
	   [ Netsys_posix.Fda_close clear_fd;   (* rev order! *)
	     Netsys_posix.Fda_dup2(clear_fd, fd);
	     Netsys_posix.Fda_dup2(fd, clear_fd)
	   ] @ !fd_actions;
    )
    open_descr_ht;

  let pg =
    match group with
      | Current_group -> Netsys_posix.Pg_keep
      | New_bg_group  -> Netsys_posix.Pg_new_bg_group
      | Join_group g  -> Netsys_posix.Pg_join_group g
      | New_fg_group  -> Netsys_posix.Pg_new_fg_group in

  let chdir =
    match c.c_directory with
      | None   -> Netsys_posix.Wd_keep
      | Some d -> Netsys_posix.Wd_chdir d in

  (* Now spawn the new process: *)
  let pid =
    Netsys_posix.spawn
      ~chdir
      ~pg
      ~fd_actions:(List.rev !fd_actions)
      ~sig_actions
      ~env:!(c.c_environment)
      c.c_filename
      args in

  let pgid =
    try Netsys_posix.getpgid pid
    with Unix.Unix_error(Unix.ESRCH,_,_) -> 0 
      (* This should not happen because if pid has terminated it is
         now a zombie
       *) in

  let pgid_ws, kill_flag =
    match forward_mode with
      | No_forward -> 0, false
      | Forward_to_process -> 0, true
      | Forward_to_group -> pgid, true in

  let p_fd, ws = Netsys_posix.watch_subprocess pid pgid_ws kill_flag in

  Netlog.Debug.track_fd
    ~owner:"Shell_sys"
    ~descr:("event_fd(" ^ c.c_cmdname ^ ")")
    p_fd;

  { p_command = c;
    p_id = `POSIX(pid, p_fd, ws);
    p_gid = pgid;
    p_status = None;
    p_abandoned = false;
  }
;;

let win32_maybe_quote f =  (* stolen from unix.ml *)
  if String.contains f ' ' || String.contains f '\"'
  then Filename.quote f
  else f 


let win32_process_mutex =
  !Netsys_oothr.provider # create_mutex()


let win32_run
    ?(group = Current_group)
    ?(forward_mode = No_forward)
    ?(pipe_assignments = [])
    c =

  (* This [run] implementation bases on [Netsys_win32.create_process] *)

  (* Win32 does not pass arguments as array down to the child, but
     as plain string. The arguments have to be quoted. This is not fully
     documented by MS
   *)

  let args = Array.append [| c.c_cmdname |] c.c_arguments in
  let cmdline =
    String.concat " "
      (List.map win32_maybe_quote (Array.to_list args)) in

  dlogr (fun () ->
	   sprintf "run cmd=%s cmdline=%s"
	     c.c_cmdname cmdline);

  (* Descriptor assignments. We can only assign stdin/stdout/stderr.
     Also, create_process understands the assignment already as parallel
     assignment, so things are quite simple here.
   *)

  let sub_stdin = ref Unix.stdin in
  let sub_stdout = ref Unix.stdout in
  let sub_stderr = ref Unix.stderr in

  let sub_fd fd =
    if Netsys.is_stdin fd then
      sub_stdin
    else if Netsys.is_stdout fd then
      sub_stdout
    else if Netsys.is_stderr fd then
      sub_stderr
    else
      raise Not_found in

  List.iter
    (fun (from_fd, to_fd) ->
       try 
	 (sub_fd to_fd) := from_fd
       with
	 | Not_found ->
	     failwith "Shell_sys.run: Cannot assign this descriptor \
                       (Win32 restriction)"
    )
    pipe_assignments;

  (* When simulating sequential assignments, we have to take care of
     previous assignments, e.g.

     1. stdout := <file>
     2. stderr := stdout

     Here, the second assignment does not mean the original stdout (like
     in a parallel assignment), but the already assigned stdout, i.e. <file>.
   *)
  List.iter
    (fun (from_fd, to_fd) ->
       try
	 (sub_fd to_fd) := (try !(sub_fd from_fd) with Not_found -> from_fd)
       with
	 | Not_found ->
	     failwith "Shell_sys.run: Cannot assign this descriptor \
                       (Win32 restriction)"
    )
    c.c_assignments;

   dlogr (fun () ->
	   sprintf "run cmd=%s stdin=%Ld stdout=%Ld stderr=%Ld"
	     c.c_cmdname
	     (Netsys.int64_of_file_descr !sub_stdin)
	     (Netsys.int64_of_file_descr !sub_stdout)
	     (Netsys.int64_of_file_descr !sub_stderr)
	 );

  let chdir_opts =
    match c.c_directory with
      | None -> []
      | Some d -> [ Netsys_win32.CP_change_directory d ] in

  let env_opts =
    [ Netsys_win32.cp_set_env !(c.c_environment) ] in

  let pg_opts =
    match group with
      | Current_group -> [ Netsys_win32.CP_inherit_process_group ]
      | New_bg_group  -> [ Netsys_win32.CP_new_process_group ]
      | Join_group g  -> failwith "Shell_sys.run: Join_group unsupported \
                                   (Win32 restriction)"
      | New_fg_group  -> failwith "Shell_sys.run: New_fg_group unsupported \
                                   (Win32 restriction)" in


  (* There is no way to close descriptors that must not be shared with 
     the parent process, except clearing the close-on-exec flag.
     Unfortunately, this flag can only be changed for the whole process.
     So we have to serialize here (and even then there is some bad
     conscience).
   *)
  Netsys_oothr.serialize
    win32_process_mutex
    (fun () ->
       let open_descr_st = Hashtbl.create 50 in

       let change_close_on_exec() =
	 List.iter
	   (fun fd ->
	      if not (Hashtbl.mem open_descr_st fd) then (
		let state = Netsys_win32.test_close_on_exec fd in
		Hashtbl.replace open_descr_st fd state;
		Netsys_win32.modify_close_on_exec fd false
	      )
	   )
	   [ !sub_stdin; !sub_stdout; !sub_stderr ] in
       let restore_close_on_exec() =
	 Hashtbl.iter
	   (fun fd state ->
	      Netsys_win32.modify_close_on_exec fd state
	   )
	   open_descr_st in

       change_close_on_exec();
       try
	 let p =
	   Netsys_win32.create_process
	     c.c_cmdname
	     cmdline
	     ( chdir_opts @
		 env_opts @
		 pg_opts @
		 [ Netsys_win32.CP_std_handles(!sub_stdin,
					       !sub_stdout,
					       !sub_stderr) ]
	     ) in
	 let ev = Netsys_win32.as_process_event p in
	 let ev_proxy = Netsys_win32.event_descr ev in
	 restore_close_on_exec();
	 dlog "run returning normally";
	 Netlog.Debug.track_fd
	   ~owner:"Shell_sys"
	   ~descr:("event_fd(" ^ c.c_cmdname ^ ")")
	   ev_proxy;
	 { p_command = c;
	   p_id = `Win32 (p,ev_proxy);
	   p_gid = 0;  (* We don't do anything with that *)
	   p_status = None;
	   p_abandoned = false;
	 }
       with
	 | error ->
	     dlogr(fun () -> 
		     sprintf "run returning exn %s" (Netexn.to_string error));
	     restore_close_on_exec();
	     raise error
    )
    ()
    


let run = 
  if is_win32 then
    win32_run
  else
    posix_run;;


let process_id p = 
  match p.p_id with
    | `POSIX (pid,_,_) -> pid
    | `Win32 (proc,_) -> Netsys_win32.win_pid proc
    | `Dummy -> failwith "Shell_sys.process_id: dummy"
;;


let status p =
  match p.p_status with
      None -> raise Not_found
    | Some s -> s
;;

type process_event =
    File_read of Unix.file_descr
  | File_write of Unix.file_descr
  | Process_event of Unix.file_descr (* the signalling descriptor *)
;;


let string_of_status =
  function
    | Unix.WEXITED n -> sprintf "WEXITED(%d)" n
    | Unix.WSIGNALED n -> sprintf "WSIGNALED(%d)" n
    | Unix.WSTOPPED n -> sprintf "WSTOPPED(%d)" n


let kill ?(signal = Sys.sigterm) p =
  dlogr (fun () ->
	   sprintf "Killing cmd=%s signal=%d"
	     p.p_command.c_cmdname signal);
  match p.p_id with
    | `POSIX (pid,_,ws) ->
	assert(not is_win32);
	Netsys_posix.kill_subprocess signal ws
    | `Win32 (proc,_) ->
	assert(is_win32);
	if signal = Sys.sigterm || signal = Sys.sigkill then (
	  try
	    Netsys_win32.terminate_process proc
	  with _ -> ()
	)
    | `Dummy -> ()
;;



(*** command and process groups ***)

type pipeline =
    { pl_src_command : command;
      pl_dest_command : command;
      pl_src_descr : Unix.file_descr;
      pl_dest_descr : Unix.file_descr;
      pl_bidirectional : bool;
    }
;;

type pipehandler =
    { ph_command : command;
      ph_descr : Unix.file_descr;
      ph_handler : (Unix.file_descr -> bool);
    }
;;

type job =
    { mutable cg_commands : command list;
      mutable cg_pipelines : pipeline list;
      mutable cg_producers : pipehandler list;
      mutable cg_consumers : pipehandler list;
    }
;;


let eq_fd fd1 fd2 =
  (* POSIX: Same as fd1=fd2.
     Win32: We treat the standard descriptors specially, so they are only
     equal to themselves, even if they are compared with another descriptor
     using the same handle (i.e. not(eq_fd Unix.stdout Unix.stderr) even
     if stdout and stderr are connected with the same handle)
   *)
  if is_win32 then
    let fd1_stdin = Netsys.is_stdin fd1 in
    let fd1_stdout = Netsys.is_stdout fd1 in
    let fd1_stderr = Netsys.is_stderr fd1 in
    let fd2_stdin = Netsys.is_stdin fd2 in
    let fd2_stdout = Netsys.is_stdout fd2 in
    let fd2_stderr = Netsys.is_stderr fd2 in
    if fd1_stdin then fd2_stdin
    else if fd1_stdout then fd2_stdout
    else if fd1_stderr then fd2_stderr
    else fd1=fd2
  else
    fd1=fd2


let ( =$ ) = eq_fd


let new_job () =
  { cg_commands = [];
    cg_pipelines = [];
    cg_producers = [];
    cg_consumers = [];
  }
;;


let add_command c cg =
  if List.memq c cg.cg_commands then
    failwith "Shell_sys.add_command: Cannot add the same command twice; \
              use copy_command to add a copy";
  cg.cg_commands <- c :: cg.cg_commands;
  ()
;;


let add_pipeline
      ?(bidirectional = false)
      ?(src_descr = Unix.stdout)
      ?(dest_descr = Unix.stdin)
      ~src
      ~dest
      cg =

  if not (List.memq src cg.cg_commands) then
    failwith "Shell_sys.add_pipeline: the ~src command is not member of \
              the command group";
  if not (List.memq dest cg.cg_commands) then
    failwith "Shell_sys.add_pipeline: the ~dest command is not member of \
              the command group";
  let pl =
    { pl_src_command   = src;
      pl_dest_command  = dest;
      pl_src_descr     = src_descr;
      pl_dest_descr    = dest_descr;
      pl_bidirectional = bidirectional;
    }
  in

  cg.cg_pipelines <- pl :: cg.cg_pipelines
;;


let add_producer
      ?(descr = Unix.stdin)
      ~producer
      c
      cg =

  if not (List.memq c cg.cg_commands) then
    failwith "Shell_sys.add_producer: the passed command is not member of \
              the command group";

  let ph =
    { ph_command = c;
      ph_descr   = descr;
      ph_handler = producer
    }
  in

  cg.cg_producers <- ph :: cg.cg_producers
;;


let add_consumer
      ?(descr = Unix.stdout)
      ~consumer
      c
      cg =

  if not (List.memq c cg.cg_commands) then
    failwith "Shell_sys.add_consumer: the passed command is not member of \
              the command group";

  let ph =
    { ph_command = c;
      ph_descr   = descr;
      ph_handler = consumer
    }
  in

  cg.cg_consumers <- ph :: cg.cg_consumers
;;


let write_close fd_style fd =
  ( try
      Netsys.gshutdown fd_style fd Unix.SHUTDOWN_SEND
    with
      | Netsys.Shutdown_not_supported -> ()
      | Unix.Unix_error(Unix.EAGAIN,_,_) ->
	  ignore(Netsys.wait_until_writable fd_style fd (-1.0));
	  Netsys.gshutdown fd_style fd Unix.SHUTDOWN_SEND
      | Unix.Unix_error(Unix.EPERM,_,_) -> ()
  );
  Netsys.gclose fd_style fd


let from_string
      ?(pos = 0)
      ?len
      ?(epipe = fun () -> ())
      s =
  if pos < 0 || pos > String.length s then
    invalid_arg "Shell_sys.from_string";
  let max_pos =
    match len with
	None   -> String.length s
      | Some l ->
	  if l < 0 then invalid_arg "Shell_sys.from_string";
	  pos + l
  in
  if max_pos > String.length s then invalid_arg "Shell_sys.from_string";
  (* ==> Take material from positions pos to max_pos-1 from s *)

  let current_pos = ref pos in
  let fd_style = ref `Read_write in
  let fd_style_set = ref false in

  function fd ->
    if not !fd_style_set then (
      fd_style := Netsys.get_fd_style fd;
      fd_style_set := true
    );
    let m = max_pos - !current_pos in
    let n =
      if m > 0 then begin
	try
	  Netsys.gwrite !fd_style fd s (!current_pos) m
	with
	    Unix.Unix_error(Unix.EPIPE,_,_) ->
	      epipe();
	      m            (* forces that the descriptor will be closed *)
	  | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK),_,_) ->
	      (* maybe somebody has set non-blocking mode for fd *)
	      0
	  (* We do not catch EINTR - the calling "wait_group" routine
	   * arranges that already
	   *)
      end
      else
	0 in
    current_pos := !current_pos + n;
    if !current_pos = max_pos then begin
      write_close !fd_style fd;
      false
    end
    else
      true
;;


let from_stream
      ?(epipe = fun () -> ())
      s =
  let current_el  = ref None in
  let current_pos = ref 0 in

  let fd_style = ref `Read_write in
  let fd_style_set = ref false in

  function fd ->
    if not !fd_style_set then (
      fd_style := Netsys.get_fd_style fd;
      fd_style_set := true
    );
    (* If necessary, try to get the next stream element: *)
    begin match !current_el with
	None ->
	  begin try
	    let x = Stream.next s in
	    current_el := Some x;
	    current_pos := 0;
	  with
	      Stream.Failure ->
		()
	  end
      | _ ->
	  ()
    end;
    (* (Continue to) write the current stream element: *)
    match !current_el with
	None ->
	  write_close !fd_style fd;
	  false
      | Some x ->
	  let m = String.length x - !current_pos in
	  let n =
	    try
	      Netsys.gwrite !fd_style fd x (!current_pos) m
	    with
		Unix.Unix_error(Unix.EPIPE,_,_) ->
		  epipe();
		  m            (* forces that the descriptor will be closed *)
	      | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK),_,_) ->
		  (* maybe somebody has set non-blocking mode for fd *)
		  0
              (* We do not catch EINTR - the calling "wait_group" routine
	       * arranges that already
	       *)
	  in
	  current_pos := !current_pos + n;
	  if !current_pos = String.length x then current_el := None;
	  true
;;


let to_buffer b =
  let m = 4096 in
  let s = String.create m in

  let fd_style = ref `Read_write in
  let fd_style_set = ref false in

  let next fd =
    if not !fd_style_set then (
      fd_style := Netsys.get_fd_style fd;
      fd_style_set := true
    );
    let n =
      try
	let n = Netsys.gread !fd_style fd s 0 m in
	if n = 0 then -1 else n
      with
	| Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK),_,_) ->
	    (* maybe somebody has set non-blocking mode for fd *)
	    0
    in
    if n < 0 then begin
      (* EOF *)
      Netsys.gclose !fd_style fd;
      false
    end
    else begin
      Buffer.add_substring b s 0 n;
      true
    end
  in
  next
;;


exception No_Unix_process_group;;

type group_mode = Same_as_caller | Foreground | Background
;;

type job_status =
    Job_running
  | Job_partially_running
  | Job_ok
  | Job_error
  | Job_abandoned
;;

type job_instance =
    { pg_id : int;
      pg_cg : job;
      pg_processes : process list;
      mutable pg_processes_closed : bool;
      pg_mode : group_mode;
      pg_forward_signals : bool;
      mutable pg_fd_producer_alist : (Unix.file_descr * pipehandler) list;
      mutable pg_fd_consumer_alist : (Unix.file_descr * pipehandler) list;
      mutable pg_pending : process_event list;
      mutable pg_status : job_status;
      mutable pg_exception : exn;
    }
;;


type safe_fd =
    FD of Netsys.fd_style * Unix.file_descr
  | FD_closed

let mk_fd ?descr fd = 
  ( match descr with
      | None -> ()
      | Some d ->
	  Netlog.Debug.track_fd ~owner:"Shell_sys" ~descr:d fd
  );
  let style = Netsys.get_fd_style fd in
  ref(FD(style,fd));;

let dest_fd safe_fd =
  match !safe_fd with
      FD(_,fd) ->
	fd
    | FD_closed ->
	failwith "Descriptor is closed"
;;

let close_fd safe_fd =
  match !safe_fd with
      FD(style,fd) ->
	Netsys.gclose style fd;
	safe_fd := FD_closed;
    | FD_closed ->
	()
;;

let print_fd safe_fd =
  match !safe_fd with
      FD(style,fd) ->
	"FD(" ^ Int64.to_string(Netsys.int64_of_file_descr fd) ^ ")"
    | FD_closed ->
	"FD_closed"


let print_raw_fd fd =
  Int64.to_string(Netsys.int64_of_file_descr fd)



let create_pipe() =
  let (fd1,fd2) = Unix.pipe() in
  Netsys.set_close_on_exec fd1;
  Netsys.set_close_on_exec fd2;
  (fd1,fd2)

let create_producer_pipe() =
  let (fd1,fd2) = create_pipe() in
  if is_win32 then
    let othr = Netsys_win32.create_output_thread fd2 in
    let fd2' = Netsys_win32.output_thread_proxy_descr othr in
    (fd1,fd2')
  else (
    Unix.set_nonblock fd2;
    (fd1,fd2)
  )

let create_consumer_pipe() =
  let (fd1,fd2) = create_pipe() in
  if is_win32 then
    let ithr = Netsys_win32.create_input_thread fd1 in
    let fd1' = Netsys_win32.input_thread_proxy_descr ithr in
    (fd1',fd2)
  else (
    Unix.set_nonblock fd1;
    (fd1,fd2)
  )

let track_producer fd ph =
  Netlog.Debug.track_fd 
    ~owner:"Shell_sys" 
    ~descr:("producer->" ^ ph.ph_command.c_cmdname)
    fd


let track_consumer fd ph =
  Netlog.Debug.track_fd 
    ~owner:"Shell_sys" 
    ~descr:("consumer<-" ^ ph.ph_command.c_cmdname)
    fd

exception Pass_exn of exn;;

let run_job
      ?(mode = Same_as_caller)
      ?(forward_signals = true)
      cg =

  if cg.cg_commands = [] then
    invalid_arg "Shell_sys.run_job: No commands to start";

  dlogr (fun () ->
	   sprintf "run_job pipeline=%s"
	     (String.concat " | " 
		(List.map (fun c -> c.c_cmdname) cg.cg_commands)));

  (* Global stores: *)

  let pipe_descriptors = ref [] in
    (* The pipeline descriptor pairs created so far *)

  let producer_descriptors = ref [] in
  let consumer_descriptors = ref [] in

  let processes = ref [] in
  let group_id = ref 0 in



  let build_interprocess_pipelines() =
    (* Basically, for every pipeline found in cg a new Unix pipeline is created.
     * However, there are cases where the same Unix pipeline can be reused for
     * several cg.cg_pipelines:
     * - If pipelines read from the same descriptor of the same command
     * - If pipelines write to the same descriptor of the same command
     * This makes it possible that a pipeline may have several readers/writers.
     *)
    List.iter
      (fun pipe ->
	 (* Is there already a pipeline in pipe_descriptors for the same command
	  * and the same descriptor?
	  *)
	 let other_src =
	   try
	     let _, (other_out_end, other_in_end) =
	       List.find (fun (p, _) ->
			    (p.pl_src_command == pipe.pl_src_command) &&
			    (p.pl_src_descr =$ pipe.pl_src_descr))
	                 !pipe_descriptors
	     in
	     Some (other_out_end, other_in_end)
	   with Not_found -> None
	 in
	 let other_dest =
	   try
	     let _, (other_out_end, other_in_end) =
	       List.find (fun (p, _) ->
			    (p.pl_dest_command == pipe.pl_dest_command) &&
			    (p.pl_dest_descr =$ pipe.pl_dest_descr))
	                 !pipe_descriptors
	     in
	     Some (other_out_end, other_in_end)
	   with Not_found -> None
	 in
	 (* Check now src/dest cross comparison. For simple pipelines this is an
	  * error. For bidirectional pipelines it would be possible to make it
	  * working; however, it is not worth the effort.
	  *)
	 if List.exists (fun (p, _) ->
			   ((p.pl_src_command == pipe.pl_dest_command) &&
			    (p.pl_src_descr =$ pipe.pl_dest_descr)) ||
			   ((p.pl_dest_command == pipe.pl_src_command) &&
			    (p.pl_dest_descr =$ pipe.pl_src_descr)))
	                !pipe_descriptors
	 then
	   failwith "Shell_sys.run_group: Pipeline construction not possible or too ambitious";

         (* Distinguish between the various cases: *)

	 match other_src, other_dest with
	   | None, None ->
	       (* Create a new pipeline: *)
	       let out_end, in_end =
		 if pipe.pl_bidirectional then
		   Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
		 else
		   create_pipe() in
	       pipe_descriptors :=
	         (pipe, (mk_fd out_end, mk_fd in_end)) :: !pipe_descriptors;
	       dlogr (fun () ->
			sprintf "Created ipc pipe %s:%s"
			  (print_raw_fd out_end) (print_raw_fd in_end));
	   | Some (out_end, in_end), None ->
	       pipe_descriptors :=
	         (pipe, (out_end, in_end)) :: !pipe_descriptors;
	       dlogr (fun () ->
			sprintf "Reusing ipc pipe %s:%s"
			  (print_fd out_end) (print_fd in_end));
	   | None, Some (out_end, in_end) ->
	       pipe_descriptors :=
	         (pipe, (out_end, in_end)) :: !pipe_descriptors;
	       dlogr (fun () ->
			sprintf "Reusing ipc pipe %s:%s"
			  (print_fd out_end) (print_fd in_end));
	   | _ ->
	       (* case Some, Some: the same pipeline exists twice. We can drop
		* the second.
		*)
	       ()
      )
      cg.cg_pipelines
  in

  let close_interprocess_pipelines() =
    (* Close both ends of the (interprocess) pipeline *)
    List.iter
      (fun (_, (out_end, in_end)) ->
	 close_fd out_end;
	 close_fd in_end;
	 dlogr (fun () ->
		  sprintf "Closed ipc pipe %s:%s"
		    (print_fd out_end) (print_fd in_end));
      )
      !pipe_descriptors;
  in

  let check_ph is_producer ph =
    (* Is there already a pipeline in producer_descriptors for the
     * same command and the same descriptor? Or in pipeline_descriptors?
     * This case cannot be handled and causes an error.
     *)
    let name = if is_producer then "producer" else "consumer" in
    let op   = if is_producer then "write to" else "read from" in
    if
      List.exists
	(fun (ph',_) ->
	   (ph'.ph_command == ph.ph_command) &&
	   (ph'.ph_descr =$ ph.ph_descr))
	!producer_descriptors ||
      List.exists
	(fun (ph',_) ->
	   (ph'.ph_command == ph.ph_command) &&
	   (ph'.ph_descr =$ ph.ph_descr))
	!consumer_descriptors
    then
      failwith ("Shell_sys.run_job: A " ^ name ^
		" cannot " ^ op ^
		" a descriptor which is already bound to another producer/consumer");

    if
      List.exists
	(fun (pl',_) ->
	   (pl'.pl_src_command == ph.ph_command &&
            pl'.pl_src_descr =$ ph.ph_descr) ||
	   (pl'.pl_dest_command == ph.ph_command &&
            pl'.pl_dest_descr =$ ph.ph_descr))
	!pipe_descriptors
    then
      failwith ("Shell_sys.run_job: A " ^ name ^
		" cannot " ^ op ^
		" a descriptor which is already bound to an interprocess pipeline");
  in

  let build_producer_descriptors() =
    (* For every producer create a new pipeline *)
    List.iter
      (fun ph ->
	 check_ph true ph;
	 let out_end, in_end = create_producer_pipe() in
	 producer_descriptors :=
	   (ph, (mk_fd out_end, mk_fd in_end)) :: !producer_descriptors;
	 dlogr (fun () ->
		  sprintf "Created producer pipe %s:%s"
		    (print_raw_fd out_end) (print_raw_fd in_end));
      )
      cg.cg_producers
  in

  let build_consumer_descriptors() =
    (* For every consumer create a new pipeline *)
    List.iter
      (fun ph ->
	 check_ph false ph;
	 let out_end, in_end = create_consumer_pipe() in
	 consumer_descriptors :=
	   (ph, (mk_fd out_end, mk_fd in_end)) :: !consumer_descriptors;
	 dlogr (fun () ->
		  sprintf "Created consumer pipe %s:%s"
		    (print_raw_fd out_end) (print_raw_fd in_end));
      )
      cg.cg_consumers
  in

  let close_producer_descriptors ~fully =
    (* not fully: close the output side of the pipelines only.
     * fully: close both sides of the pipelines
     *)
    List.iter
      (fun (ph,(out_end, in_end)) ->
	 dlogr (fun () ->
		  sprintf "Closing producer pipe %s:%s"
		    (print_fd out_end) 
		    (if fully then print_fd in_end else "omitted"));
	 close_fd out_end;
	 if fully then close_fd in_end;
      )
      !producer_descriptors;
  in

  let close_consumer_descriptors ~fully =
    (* not fully: close the input side of the pipelines only.
     * fully: close both sides of the pipelines
     *)
    List.iter
      (fun (ph,(out_end, in_end)) ->
	 dlogr (fun () ->
		  sprintf "Closing consumer pipe %s:%s"
		    (if fully then print_fd out_end else "omitted")
		    (print_fd in_end));
	 close_fd in_end;
	 if fully then close_fd out_end;
      )
      !consumer_descriptors;
  in

  let start_processes() =
    let group_behaviour =
      ref (match mode with
	       Same_as_caller -> Current_group
	     | Foreground     -> New_fg_group
	     | Background     -> New_bg_group) in
    (* Note: the following iteration is performed in the reverse direction as
     * the commands have been added. This means that the last added command
     * will be started first, and will be the process group leader.
     *)

    List.iter
      (fun c ->

         (* Is there a pipeline reading from this command? *)

         (* Note: multiple reading pipelines for the same descriptor are
	  * supported although such a construction is quite problematic as it
	  * is undefined which pipeline gets which packet of data
	  *)

	 let rd_assignments =
	   let pipes =
	     List.find_all
	       (fun pl' -> pl'.pl_src_command == c)
	       cg.cg_pipelines in
	   let consumers =
	     List.find_all
	       (fun ph -> ph.ph_command == c)
	       cg.cg_consumers in

	   List.map
	     (fun pipe ->
		let (out_end, in_end) = List.assq pipe !pipe_descriptors in
		(dest_fd in_end, pipe.pl_src_descr)
	     )
	     pipes
	   @
	   List.map
	     (fun ph ->
		let (out_end, in_end) = List.assq ph !consumer_descriptors in
		(dest_fd in_end, ph.ph_descr)
	     )
	     consumers
	 in

         (* Is there a pipeline writing to this command? *)

	 let wr_assignments =
	   let pipes =
	     List.find_all
	       (fun pl' -> pl'.pl_dest_command == c)
	       cg.cg_pipelines in
	   let producers =
	     List.find_all
	       (fun ph -> ph.ph_command == c)
	       cg.cg_producers in

	   List.map
	     (fun pipe ->
		let (out_end, in_end) = List.assq pipe !pipe_descriptors in
		(dest_fd out_end, pipe.pl_dest_descr)
	     )
	     pipes
	   @
	   List.map
	     (fun ph ->
		let (out_end, in_end) = List.assq ph !producer_descriptors in
		(dest_fd out_end, ph.ph_descr)
	     )
	     producers
	 in

         (* Note: It is essential that ~pipe_assignments are performed in a
	  * parallel way, because it is possible that assignment pairs exist
	  * (in_end, pl.pl_src_descr) and (out_end, pl.pl_dest_descr) with
	  * pl.pl_src_descr = out_end.
	  *)

	 let forward_mode =
	   if mode = Background && forward_signals then
	     Forward_to_group
	   else
	     No_forward in

	 let p =
	   run
	     ~group: !group_behaviour
	     ~forward_mode
	     ~pipe_assignments: (rd_assignments @ wr_assignments)
	     c
	 in

	 dlogr (fun () -> sprintf "Started process %s" c.c_cmdname);

	 group_id := p.p_gid;
	 processes := p :: !processes;

	 if !group_behaviour = New_fg_group || !group_behaviour = New_bg_group
	 then
	   group_behaviour := Join_group p.p_gid
      )
      cg.cg_commands;

    dlog "Processes started"
  in

  try
    (* Start the new process group: *)

    let fd_producer_alist = ref [] in
    let fd_consumer_alist = ref [] in

    build_interprocess_pipelines();
    build_producer_descriptors();
    build_consumer_descriptors();
    start_processes();
    close_interprocess_pipelines();
    pipe_descriptors := [];
    close_producer_descriptors ~fully:false;
    fd_producer_alist := List.map
                           (fun (ph, (_, in_end)) -> (dest_fd in_end, ph))
                           !producer_descriptors;
    producer_descriptors := [];
    close_consumer_descriptors ~fully:false;
    fd_consumer_alist := List.map
                           (fun (ph, (out_end, _)) -> (dest_fd out_end, ph))
                           !consumer_descriptors;
    consumer_descriptors := [];

    (* fd tracking: *)
    List.iter
      (fun (fd, ph) -> track_producer fd ph)
      !fd_producer_alist;
    List.iter
      (fun (fd, ph) -> track_consumer fd ph)
      !fd_consumer_alist;

    (* Store the new process group: *)

    dlogr (fun () ->
	     sprintf "run_job setup done - pipeline=%s"
	       (String.concat " | " 
		  (List.map (fun c -> c.c_cmdname) cg.cg_commands)));
    let g =
      { pg_id = !group_id;
	pg_cg = cg;
	pg_processes = !processes;
	pg_processes_closed = false;
	pg_mode = mode;
	pg_forward_signals = forward_signals;
	pg_fd_producer_alist = !fd_producer_alist;
	pg_fd_consumer_alist = !fd_consumer_alist;
	pg_pending = [];
	pg_status = Job_running;
	pg_exception = Not_found;
      }
    in

    (* Return g as result *)

    g

  with
    | ex ->
	(* If another error happens while it is tried to recover from the
	 * first error, a Fatal_error is raised.
	 *)
	dlogr (fun () ->
		 sprintf "run_job error %s"
		   (Netexn.to_string ex));
	try
	  (* Close all interprocess pipelines (if not already done) *)
	  close_interprocess_pipelines();
	  pipe_descriptors := [];
	  (* Close all producer/consumer pipelines fully *)
	  close_producer_descriptors ~fully:true;
	  close_consumer_descriptors ~fully:true;
	  producer_descriptors := [];
	  consumer_descriptors := [];
	  (* If there is at least one process, return a partial result *)
	  if !processes <> [] then begin
	    let g =
	      { pg_id = !group_id;
		pg_cg = cg;
		pg_processes = !processes;
		pg_processes_closed = false;
		pg_mode = mode;
		pg_forward_signals = forward_signals;
		pg_fd_producer_alist = [];
		pg_fd_consumer_alist = [];
		pg_pending = [];
		pg_status = Job_partially_running;
		pg_exception = ex;
	      }
	    in
	    g
	  end
	  else
	    (* Raise ex again *)
	    raise (Pass_exn ex)
	with
	  | Pass_exn ex ->
	      dlogr (fun () ->
		       sprintf "run_job returning error %s"
			 (Netexn.to_string ex));
	      raise ex
	  | (Fatal_error ex') as ex ->
	      dlogr (fun () ->
		       sprintf "run_job returning error %s"
			 (Netexn.to_string ex));
	      raise ex
	  | ex' ->
	      let ex'' = Fatal_error ex' in
	      dlogr (fun () ->
		       sprintf "run_job returning error %s"
			 (Netexn.to_string ex''));
	      raise ex''
;;


let processes pg = pg.pg_processes;;

let process_group_leader pg =
  try
    List.find (fun p -> process_id p = pg.pg_id) pg.pg_processes
  with
      Not_found -> raise No_Unix_process_group
;;

let process_group_id pg =
  if pg.pg_id >= 0 then pg.pg_id else raise No_Unix_process_group
;;

let job_status pg = pg.pg_status;;


let string_of_pevent =
  function
    | File_read fd ->
	sprintf "File_read(%Ld)" (Netsys.int64_of_file_descr fd)
    | File_write fd ->
	sprintf "File_write(%Ld)" (Netsys.int64_of_file_descr fd)
    | Process_event fd ->
	sprintf "Process_event(%Ld)" (Netsys.int64_of_file_descr fd)
	


let close_job_descriptors pg =
  (* Close the pipeline descriptors used for producers and consumers.
   * These alists only contain the descriptors that are still open,
   * so we can simply close them.
   *)
  List.iter
    (fun (fd,_) -> 
       Netlog.Debug.release_fd fd;
       Netsys.gclose (Netsys.get_fd_style fd) fd)
    pg.pg_fd_consumer_alist;
  pg.pg_fd_consumer_alist <- [];
  List.iter
    (fun (fd,_) -> 
       Netlog.Debug.release_fd fd;
       Netsys.gclose (Netsys.get_fd_style fd) fd)
    pg.pg_fd_producer_alist;
  pg.pg_fd_producer_alist <- [];
  if not pg.pg_processes_closed then (
    List.iter
      (fun p ->
	 match p.p_id with
	   | `POSIX(_,p_fd,ws) ->
	       Netlog.Debug.release_fd p_fd;
	       Unix.close p_fd;
	       Netsys_posix.ignore_subprocess ws
	   | `Win32(_,p_fd) ->
	       Netlog.Debug.release_fd p_fd;
	       Unix.close p_fd
	   | `Dummy -> ()
      )
      pg.pg_processes;
    pg.pg_processes_closed <- true
  )
;;


class type ['t] job_handler_engine_type = object
  inherit ['t] Uq_engines.engine
  method job : job
  method job_instance : job_instance
end


class job_engine esys pg =
  let read_list() =
    (* Check the list of consumers, and extract the list of file descriptors
     * we are reading from.
     *)
    List.map fst pg.pg_fd_consumer_alist in

  let write_list() =
    (* Check the list of producers, and extract the list of file descriptors
     * we want to write to
     *)
    List.map fst pg.pg_fd_producer_alist in

  let process_list() =
    List.flatten
      (List.map
	 (fun p ->
	    match p.p_status with
	      | None ->
		  ( match p.p_id with
		      | `POSIX(_,p_fd,_) -> [p_fd, p]
		      | `Win32(_,p_fd) -> [p_fd, p]
		      | `Dummy -> []
		  )
	      | Some _ -> []
	 )
	 pg.pg_processes
      ) in

object(self)
  inherit [unit] Uq_engines.engine_mixin (`Working 0) esys

  val mutable group = Unixqueue.new_group esys

  val mutable cur_read = []
  val mutable cur_write = []
  val mutable cur_process = []

  initializer (
    match pg.pg_status with
      | Job_ok | Job_error | Job_abandoned ->
	  (* Register a do-nothing handler: *)
	  Unixqueue.once esys group 0.0 (fun () -> self # set_state (`Done()))
      | _ ->
	  Unixqueue.add_handler
	    esys group (fun _ _ -> self # handle_event);
	  self # update();
  )

  method job = pg.pg_cg
  method job_instance = pg

  method abort() =
    match self#state with
	`Working _ ->
	  close_job_descriptors pg;
	  self # set_state `Aborted;
	  Unixqueue.clear esys group
      | _ ->
	  ()

  method event_system = esys

  method private update() =
    (* Update the resources for file descriptors: *)
    let update_res make_op old_list new_list =
      List.iter
	(fun old_descr ->
	   if not(List.mem old_descr new_list) then
	     Unixqueue.remove_resource esys group (make_op old_descr)
	)
	old_list;
      List.iter
	(fun new_descr ->
	   if not(List.mem new_descr old_list) then
	     Unixqueue.add_resource esys group ((make_op new_descr),-1.0)
	)
	new_list
    in

    let next_read = read_list() in
    let next_write = write_list() in
    let next_process = process_list() in

    update_res (fun op -> Unixqueue.Wait_in op)  cur_read next_read;
    update_res (fun op -> Unixqueue.Wait_out op) cur_write next_write;
    update_res
      (fun op -> Unixqueue.Wait_in op)  
      (List.map fst cur_process)
      (List.map fst next_process);

    cur_read <- next_read;
    cur_write <- next_write;
    cur_process <- next_process;

    (* Maybe everything is done: *)
    if cur_read = [] && cur_write = [] && cur_process = [] then
      self # all_done()


  method private count() =
    match self#state with
	`Working n ->
	  self # set_state (`Working (n+1))
      | _ ->
	  ()


  method private handle_event ev =
    match ev with
      | Unixqueue.Input_arrived(_,fd) -> 
	  if List.mem fd cur_read then
	    self # handle_pevent_protected (File_read fd)
	  else
	    if List.mem_assoc fd cur_process then
	      self # handle_pevent_protected (Process_event fd)
      | Unixqueue.Output_readiness(_,fd) -> 
	  if List.mem fd cur_write then
	    self # handle_pevent_protected (File_write fd)
      | _ ->
	  raise Equeue.Reject


  method private handle_pevent_protected e =
    try
      self # handle_pevent e
    with
      | error ->
	  self # set_state(`Error error);
	  close_job_descriptors pg;
	  Unixqueue.clear esys group


  method private handle_pevent e =
    (* may fail because of an exception in one of the called handlers! *)
    dlogr (fun () -> sprintf "handle_pevent %s" (string_of_pevent e));
    self # count();
    let need_update = ref false in
    ( match e with
	| Process_event fd ->
	    (* Find the process to which this fd points *)
	    let p = 
	      try List.assoc fd cur_process with Not_found -> assert false in
	    (* Right now we only support terminate events. So: *)
	    ( match p.p_id with
		| `POSIX(_,_,ws) ->
		    p.p_status <-
		      Netsys_posix.get_subprocess_status ws
		| `Win32(proc,_) ->
		    p.p_status <-
		      Netsys_win32.get_process_status proc
		| `Dummy -> ()
	    );
	    if p.p_status <> None then
	      need_update := true
	| File_read fd ->
	    (* Find the consumer reading from this fd *)
	    let consumer =
	      try List.assoc fd pg.pg_fd_consumer_alist
	      with Not_found -> assert false
	    in
	    (* fd tracking: we have to release fd first because the
               handler may close fd. If this is not done we re-enable
               tracking
	     *)
	    Netlog.Debug.release_fd fd;
	    let result = Netsys.restart consumer.ph_handler fd in
	    if result then
	      track_consumer fd consumer
	    else begin
	      (* remove the consumer from the list of consumers *)
	      pg.pg_fd_consumer_alist <- ( List.remove_assoc
					     fd
					     pg.pg_fd_consumer_alist
					 );
	      need_update := true
	    end
	| File_write fd ->
	    (* Find the producer writing to this fd *)
	    let producer =
	      try List.assoc fd pg.pg_fd_producer_alist
	      with Not_found -> assert false
	    in
	    (* fd tracking: we have to release fd first because the
               handler may close fd. If this is not done we re-enable
               tracking
	     *)
	    Netlog.Debug.release_fd fd;
	    let result = Netsys.restart producer.ph_handler fd in
	    if result then
	      track_producer fd producer
	    else begin
	      (* remove the producer from the list of producers *)
	      pg.pg_fd_producer_alist <- ( List.remove_assoc
					     fd
					     pg.pg_fd_producer_alist
					 );
	      need_update := true
	    end
    );
    if !need_update then
      self # update()

  method private all_done() =
    (* This is called when all processes have terminated *)
    let successful =
      List.for_all
	(fun p ->
	   try status p = Unix.WEXITED 0 with Not_found -> assert false)
	pg.pg_processes
    in
    dlogr (fun () -> sprintf "successful=%b" successful);
    let new_status = if successful then Job_ok else Job_error in
    pg.pg_status <- new_status;

    close_job_descriptors pg;

    Unixqueue.clear esys group;
    self # set_state(`Done ())

end


let finish_job pg =
  let esys = Unixqueue.create_unix_event_system() in
  let eng = new job_engine esys pg in
  Unixqueue.run esys;
  match eng#state with
    | `Done () -> ()
    | `Error e -> raise e
    | `Aborted -> assert false  (* cannot happen *)
    | `Working _ -> failwith "Shell_sys.finish_job: still running"
;;


let call c =
  (* Create a little job with only one process in it *)
  let j = new_job () in
  add_command c j;
  let pg = run_job ~mode:Same_as_caller ~forward_signals:false j in
  finish_job pg;
  match pg.pg_processes with
    | [ p ] -> p
    | _ -> assert false
;;


let kill_process_group
      ?(signal = Sys.sigterm)
      pg =
  if is_win32 || pg.pg_mode = Same_as_caller then
    raise No_Unix_process_group;
  dlogr (fun () -> sprintf "Killing pg %d signal %d"
	   pg.pg_id signal);
  if not pg.pg_processes_closed then (
    let p = List.hd pg.pg_processes in
    match p.p_id with
      | `POSIX(_,_,ws) ->
	  Netsys_posix.killpg_subprocess signal ws
      | _ ->
	  ()
  )
;;

let kill_processes
      ?(signal = Sys.sigterm)
      pg =
  if pg.pg_status = Job_running || pg.pg_status = Job_partially_running ||
     pg.pg_status = Job_abandoned
  then begin
    if not pg.pg_processes_closed then (
      List.iter
	(fun p ->
	   match p.p_id with
	     | `POSIX(_,_,ws) ->
		 Netsys_posix.kill_subprocess signal ws
	     | _ ->
		 ()
	)
	pg.pg_processes;
    )
  end
;;

let cancel_job ?(signal = Sys.sigterm) pg =

  if pg.pg_status = Job_running || pg.pg_status = Job_partially_running
  then begin

    List.iter
      (fun p -> 
	 p.p_abandoned <- true
      )
      pg.pg_processes;

    pg.pg_status <- Job_abandoned;

    begin try
      kill_process_group ~signal:signal pg
    with
	No_Unix_process_group ->
	  kill_processes ~signal:signal pg
      | Unix.Unix_error(Unix.ESRCH,_,_) ->
	  ()
    end;

    close_job_descriptors pg;

    (* No "wait" for the processes of the abandoned job! The
     * SIGCHLD handler will look after the processes anyway.
     *)
  end
;;


let abandon_job ?signal ji = cancel_job ?signal ji


let call_job
      ?mode
      ?forward_signals
      ?(onerror = fun ji -> abandon_job ji)
      j =
  let ji = run_job ?mode:mode ?forward_signals:forward_signals j in
  if job_status ji = Job_partially_running then begin
    onerror ji;
    raise ji.pg_exception;
  end;
  finish_job ji;
  ji
;;


let mutex_reconf = !Netsys_oothr.provider # create_mutex()
  (* For multi-threaded programs: lock/unlock the mutex while reconfiguring *)

let with_reconf f =
  mutex_reconf # lock();
  ( try
      f()
    with
	any ->
	  mutex_reconf # unlock();
	  raise any
  );
  mutex_reconf # unlock()
;;


let handlers_installed   = ref false;;
let want_sigint_handler  = ref true;;
let want_sigquit_handler = ref true;;
let want_sigterm_handler = ref true;;
let want_sighup_handler  = ref true;;
let want_at_exit_handler = ref true;;


exception Already_installed;;

let configure_job_handlers
      ?(catch_sigint  = true)
      ?(catch_sigquit = true)
      ?(catch_sigterm = true)
      ?(catch_sighup  = true)
      ?(at_exit       = true)
      () =
  with_reconf
    (fun () ->
       if !handlers_installed then
	 failwith "Shell_sys.configure_job_handlers: The handlers are already installed and can no longer be configured";

       want_sigint_handler  := catch_sigint;
       want_sigquit_handler := catch_sigquit;
       want_sigterm_handler := catch_sigterm;
       want_sighup_handler  := catch_sighup;
       want_at_exit_handler := at_exit;
       ()
    )
;;


let install_job_handlers () =
  let install signo h =
    Netsys_signal.register_handler
      ~library:"shell"
      ~name:"Shell_sys forwarding to child processes"
      ~keep_default:(signo <> Sys.sigchld)
      ~signal:signo
      ~callback:h
      ()
  in

  let forward_group_signal signo =
    dlogr (fun () -> sprintf "forward_group_signal signo=%d"
	     signo);
    if not is_win32 then (
      (* Send the signal to all groups that want it: *)
      Netsys_posix.killpg_all_subprocesses signo false;
    )
  in


  let forward_individual_signal signo =
    dlogr (fun () -> sprintf "forward_individual_signal signo=%d"
	     signo);
    if not is_win32 then (
      (* Send the signal to all individual processes that are not managed
         as member of groups:
       *)
      Netsys_posix.kill_all_subprocesses signo true true
    )
  in

  with_reconf
    (fun () ->
       if !handlers_installed then raise Already_installed;

       (* The first argument of forward_signal is 'false' only for keyboard
	* signals. The other signals should be always forwarded.
	*)
       if !want_sigint_handler  then install Sys.sigint  forward_group_signal;
       if !want_sigquit_handler then install Sys.sigquit forward_group_signal;
       if !want_sigterm_handler then install Sys.sigterm forward_individual_signal;
       if !want_sighup_handler  then install Sys.sighup  forward_individual_signal;

       if !want_at_exit_handler then
	 at_exit (fun () -> forward_individual_signal Sys.sigterm);

       ()
    )
;;


let () =
  if not is_win32 then
    Netsys_posix.register_subprocess_handler()
      (* Install the SIGCHLD handler *)
