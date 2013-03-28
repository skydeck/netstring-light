(* $Id: netplex_mp.ml 1496 2010-11-27 21:49:12Z gerd $ *)

(** Multi-processing provider *)

open Netplex_types
open Printf


let close_all_except l = 
  let fd_max = Netsys_posix.sysconf_open_max() in
  for k = 3 to fd_max - 1 do  (* Note: Keep 0, 1, 2 open *)
    if not(List.mem k l) then
      ( try
	  let fd = Netsys_posix.file_descr_of_int k in
	  Netlog.Debug.release_fd ~force:true fd;
	  Unix.close fd
	with
	  | _ -> ()
      )
  done


let close_list l =
  List.iter
    (fun fd ->
      ( try
	  Netlog.Debug.release_fd ~force:true fd;
	  Unix.close fd
	with
	  | _ -> ()
      )
    )
    l


let pid_list = ref []


class mp ?(keep_fd_open=false) ?(terminate_tmo=60) () : 
               Netplex_types.parallelizer =
object(self)

  method ptype = `Multi_processing

  method current_sys_id =
    `Process (Unix.getpid())

  method create_mem_mutex() =
    (fun () -> ()), (fun () -> ())

  val mutable forward_signals = true
    (* This will be disabled in the child processes *)

  method private on_sigterm() =
    if forward_signals then (
      List.iter
	(fun pid ->
	   try Unix.kill pid Sys.sigterm
	   with _ -> ()
	)
	!pid_list;
    );
    exit 6

  method init() =
    (* SIGTERM is forwarded to all children: *)
    Netsys_signal.register_handler
      ~library:"netplex"
      ~name:"Netplex_mp forwarding to child processes"
      ~signal:Sys.sigterm
      ~callback:(fun _ -> self # on_sigterm() )
      ()

  method start_thread : 
           (par_thread -> unit) -> 'x -> 'y -> string -> logger -> par_thread =
    fun f l_close l_share srv_name logger ->
      Netsys.moncontrol false;
      let (fd_rd, fd_wr) = Unix.pipe() in
      let r_fork = 
	try Unix.fork()
	with err ->
	  Unix.close fd_rd; Unix.close fd_wr; raise err in
      match r_fork with
	| 0 ->
	    (* Disable signal forwarding in the child processes: *)
	    forward_signals <- false;

	    Unix.close fd_rd;
	    Netsys_posix.run_post_fork_handlers();
	    Netsys.moncontrol true;

	    (* We close all file descriptors except those in [l_share]. Note that
             * this is important for proper function of the main process
             * (e.g. to detect EOF of file descriptors).
             *
             * Make sure we close [fd_wr] last! This tells the main process
             * that the critical section is over.
             *)

	    if keep_fd_open then
	      close_list l_close
	    else (
	      let l_share' = 
		List.map Netsys_posix.int_of_file_descr (fd_wr :: l_share) in
	      close_all_except l_share';
	    );

	    Unix.close fd_wr;
	    let pid = Unix.getpid() in
	    let arg =
	      ( object
		  method ptype = `Multi_processing
		  method sys_id = `Process pid
		  method info_string = "Process " ^ string_of_int pid
		  method watch_shutdown _ = assert false
		  method parallelizer = (self : #parallelizer :> parallelizer)
		end
	      ) in
	    ( try
		f arg
	      with
		| error ->
		    prerr_endline
		      ("Netplex Catastrophic Error: Uncaught exception in child process " ^ string_of_int pid ^ ": " ^ Netexn.to_string error);
		    exit 2
	    );
	    exit 0
	      (* CHECK: Not sure whether we want to run onexit handlers *)

      | pid ->
	  pid_list := pid :: !pid_list;
	  Netsys.moncontrol true;
	  (* Wait until the child completes the critical section: *)
	  Unix.close fd_wr;
	  ignore (Netsys.restart
		    (Netsys.wait_until_readable `Read_write fd_rd) (-1.0));
	  Unix.close fd_rd;

	  ( object
	      val mutable watching = false

	      method ptype = `Multi_processing
	      method sys_id = `Process pid
	      method info_string = "Process " ^ string_of_int pid
	      method parallelizer = (self : #parallelizer :> parallelizer)
	      method watch_shutdown esys =
		let g = Unixqueue.new_group esys in
		let cnt = ref 0 in

		let remove() =
		  pid_list :=
		    List.filter (fun p -> p <> pid) !pid_list in

		let watch() =
		  incr cnt;
		  if terminate_tmo >= 0 && !cnt = terminate_tmo then (
		    logger # log 
		      ~component:"netplex.controller"
		      ~level:`Alert
		      ~message:(sprintf
				  "Process %d for service %s seems to be non-responsive, killing it now"
				  pid srv_name);
		    Unix.kill pid Sys.sigterm
		  );
		  try
		    let p, s = Unix.waitpid [ Unix.WNOHANG ] pid in
		    if p = 0 then  (* p=0: not yet terminated *)
		      true
		    else
		      ( remove();
			match s with
			  | Unix.WEXITED 0 ->
			      false
			  | Unix.WEXITED n ->
			      logger # log 
				~component:"netplex.controller"
				~level:`Alert
				~message:(sprintf
					    "Process %d for service %s terminated with exit code %d"
					    pid srv_name n);
			      false
			  | Unix.WSIGNALED n ->
			      logger # log 
				~component:"netplex.controller"
				~level:`Alert
				~message:(sprintf
					    "Process %d for service %s terminated with signal %d"
					    pid srv_name n);
			      false
			  | _ ->
			      assert false
		      )
		  with
		    | Unix.Unix_error(Unix.EINTR,_,_) ->
			true
		    | Unix.Unix_error(Unix.EAGAIN,_,_) ->
			true
		in

		let rec watch_loop delta =
		  Unixqueue.once esys g delta
		    (fun () ->
		       if watch() then watch_loop 1.0
		    )
		in
		if not watching then (
		  watching <- true;
		  if watch() then watch_loop 0.01
		)
	    end
	  )
	  

end

(* This instance is only used for getting current_sys_id *)
let the_mp = lazy(
  let par = new mp() in
  Netplex_cenv.register_par par;
  par
)

let mp ?keep_fd_open ?terminate_tmo () = 
  ignore(Lazy.force the_mp);
  new mp ?keep_fd_open ?terminate_tmo ()

