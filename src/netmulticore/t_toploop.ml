(* A toploop that is running in a worker process. You also need to
   link with toplevellib.cma (see Makefile).

   There is a new directive #spawn which may be used to create new
   workers. Just quit them with #quit.

   Bad interaction with ledit: SIGINT from ledit is only sent to the
   master process but not to the whole process group.

   FIXME: SIGINT does not work properly
 *)

module Unit_encap = Netplex_encap.Make_encap(struct type t = unit end)

(*
let () =
  Netmcore.Debug.enable := true
 *)

let () =
  Toploop.set_paths ()

let _ =
  Topdirs.load_file
  (* just reference this module, so the directives get initialized *)

let wd_fd =
  Unix.openfile "." [Unix.O_RDONLY] 0

let () =
  Netsys_signal.keep_away_from Sys.sigint;
  Sys.set_signal Sys.sigint Sys.Signal_ignore

let run_toploop() =
  print_endline 
    ("Netmcore: Starting toploop in worker child [PID=" ^ 
       string_of_int (Unix.getpid()) ^ "]");
  Netsys_posix.fchdir wd_fd;
  Sys.catch_break true;
  Toploop.loop Format.std_formatter;
    (* We never get here! *)
  exit 0

let toploop_fork, toploop_join =
  Netmcore.def_process
    (fun _ ->
       run_toploop();
       Unit_encap.wrap ()
    )

let spawn_toploop () =
  let pid =
    Netmcore.start ~inherit_resources:`All toploop_fork (Unit_encap.wrap()) in
  Sys.catch_break false;
  ignore(Netmcore.join toploop_join pid);
  Sys.catch_break true;
  print_endline 
    ("Netmcore: Returning to toploop in worker child [PID=" ^ 
       string_of_int (Unix.getpid()) ^ "]")

let () =
  Hashtbl.add Toploop.directive_table
    "spawn"
    (Toploop.Directive_none spawn_toploop)

let () =
  Netmcore.startup
    ~socket_directory:"/tmp/t_toploop"
    ~first_process:(fun() -> Netmcore.start toploop_fork (Unit_encap.wrap()))
    ()
