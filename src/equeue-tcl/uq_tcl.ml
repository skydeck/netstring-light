(* $Id: uq_tcl.ml 1219 2009-04-14 13:28:56Z ChriS $ *)

open Equeue
open Unixqueue

type tcl_timer_handler
type tcl_file_handler

type tcl_file_handler_rec =
    { mutable tcl_fh : tcl_file_handler;
      mutable tcl_fd : Unix.file_descr;
      mutable tcl_mask : int;
    }


external tcl_CreateFileHandler 
         : (unit -> unit) -> Unix.file_descr -> int -> tcl_file_handler
	 = "caml_Tcl_CreateFileHandler";;

external tcl_DeleteFileHandler
         : tcl_file_handler -> unit
	 = "caml_Tcl_DeleteFileHandler";;

external tcl_CreateTimerHandler 
         : (unit -> unit) -> int -> tcl_timer_handler
	 = "caml_Tcl_CreateTimerHandler";;

external tcl_DeleteTimerHandler
         : tcl_timer_handler -> unit
	 = "caml_Tcl_DeleteTimerHandler";;

external tcl_null_file_handler : unit -> tcl_file_handler = "caml_return_null";;

external tcl_null_timer_handler : unit -> tcl_timer_handler = "caml_return_null";;


let null_fh = tcl_null_file_handler();;
let null_th = tcl_null_timer_handler();;
let dummy_fh =
  { tcl_fh = tcl_null_file_handler();
    tcl_fd = Unix.stdin;
    tcl_mask = -1;
  }
;;


let create_timer callback ms =
  (* prerr_endline ("create_timer ms=" ^ string_of_int ms); *)
  ref (tcl_CreateTimerHandler callback ms)
;;


let delete_timer t = 
  (* prerr_endline ("delete_timer"); *)
  let old = !t in
  t := null_th;
  tcl_DeleteTimerHandler old
;;



type runner =
    event_system -> (unit -> unit) -> unit


class tcl_event_system ?(run : runner option) () =
object (self)
  inherit Unixqueue_select.select_based_event_system() as super

  val mutable tcl_attaching = false
  val mutable tcl_run_soon = false
  val mutable tcl_is_running = false

  val mutable tcl_last_timer = (None : tcl_timer_handler ref option)

  val mutable tcl_last_file_handlers =
                (Hashtbl.create 1 :
		   (Unix.file_descr, tcl_file_handler_rec) Hashtbl.t)

  method private tcl_attach ?(run_soon=false) () =
    (* Creates an idle callback to reschedule events the next time the
     * event loop is entered. This step can be omitted when this method
     * is called from a Unixqueue callback (it is ensured tcl_setup
     * will be invoked soon).
     *
     * run_soon: if true, the Unixqueue is run once from the idle
     * callback. This can be used to process additional, non-file events.
     *)
    if not tcl_is_running then (
      (* prerr_endline "ATTACH!"; *)
      if not tcl_attaching then (
	tcl_attaching <- true;
	( match tcl_last_timer with
	      Some th -> delete_timer th; tcl_last_timer <- None
	    | None    -> ()
	);
	tcl_last_timer <- Some (create_timer 
				  (fun () ->
				     self#tcl_setup();
				     if tcl_run_soon then (
				       tcl_run_soon <- false;
				       self#tcl_safe_handler ([],[],[],0.0) ()
				     )
				  )
				  0);
      );
      tcl_run_soon <- tcl_run_soon || run_soon;
    )
    (* else prerr_endline "(no attach)"; *)


  method private tcl_setup() =
    let (infiles, outfiles, oobfiles, time) as watch_tuple = super#setup() in

    let ht = Hashtbl.create 50 (* n *) in   (* 50 should be enough *)

    (* Fill ht, the new hash table of file handlers: *)
    List.iter
      (fun fd ->
	 let r =
	   try Hashtbl.find ht fd
	   with Not_found ->
	     let r' =
	       { tcl_fh = null_fh; tcl_fd = fd; tcl_mask = 0 } in
	     Hashtbl.add ht fd r';
	     r'
	 in
	 r.tcl_mask <- r.tcl_mask lor 1)
      infiles;
    
    List.iter
      (fun fd ->
	 let r =
	   try Hashtbl.find ht fd
	   with Not_found ->
	     let r' =
	       { tcl_fh = null_fh; tcl_fd = fd; tcl_mask = 0 } in
	     Hashtbl.add ht fd r';
	     r'
	 in
	 r.tcl_mask <- r.tcl_mask lor 2)
      outfiles;
    
    List.iter
      (fun fd ->
	 let r =
	   try Hashtbl.find ht fd
	   with Not_found ->
	     let r' =
	       { tcl_fh = null_fh; tcl_fd = fd; tcl_mask = 0 } in
	     Hashtbl.add ht fd r';
	     r'
       in
	 r.tcl_mask <- r.tcl_mask lor 4)
      oobfiles;

    (* Remove all TCL file handlers *)
    Hashtbl.iter
      (fun fd r ->
	 (* Only delete if the new condition is different: *)
	 if
	   try
	     let r' = Hashtbl.find ht fd in
	     r'.tcl_mask <> r.tcl_mask
	   with
	       Not_found -> true
	 then begin
	   let fh = r.tcl_fh in
	   r.tcl_fh <- null_fh;        (* to avoid race condition with GC *)
	   (* prerr_endline "DeleteFileHandler"; *)
	   tcl_DeleteFileHandler fh
	 end
      )
      tcl_last_file_handlers;

    (* Create the new file handlers *)
    Hashtbl.iter
      (fun fd r ->
	 (* Only create if the new condition is different: *)
	 let r' = 
	   try
	     Hashtbl.find tcl_last_file_handlers fd 
	   with
	       Not_found -> dummy_fh   (* dummy_fh.tcl_mask <> r.tcl_mask *)
	 in
	 if r'.tcl_mask <> r.tcl_mask then (
	   (* prerr_endline ("CreateFileHandler mask=" ^ string_of_int r.tcl_mask); *)
	   r.tcl_fh <- tcl_CreateFileHandler 
			 (self#tcl_safe_handler watch_tuple)
			 fd
	                 r.tcl_mask )
	 else
	   r.tcl_fh <- r'.tcl_fh;
      )
      ht;
    
    (* Remember [ht] for next round: *)

    tcl_last_file_handlers <- ht;

    let watching_files = infiles <> [] || 
			 outfiles <> [] || 
			 oobfiles <> [] in
    

    (* Remove the old timer, if any. *)
    begin match tcl_last_timer with
	None -> ()
      | Some th -> 
	  delete_timer th;
	  tcl_last_timer <- None;
    end;
    tcl_attaching <- false;

    (* Set the new timer, if necessary *)
    if time >= 0.0 then begin
      (* prerr_endline ("Timeout: " ^ string_of_float time); *)
      tcl_last_timer <- Some (create_timer
				(self#tcl_safe_handler watch_tuple)
				(int_of_float
				   (((*min*) time (*1.0*)) *. 1E3 +. 0.5)));
    end;

    (* If no handler is active, detach. *)
    (*
    if tcl_last_timer = None && not watching_files then begin
      tcl_attached <- false;
      (* prerr_endline "Detached!"; *)
    end;
    *)

  method private tcl_safe_handler watch_tuple () =
    try
      self#tcl_handler watch_tuple ()
    with
	any ->
	  prerr_endline("Uq_tcl: Internal uncaught exception: " ^
			Netexn.to_string any);

  method private tcl_handler 
                   ((infiles, outfiles, oobfiles, time) as watch_tuple) () =

    (* IMPORTANT:
     * It is possible that this is a "ghost event". We need to test whether
     * there is a resource for the event or not.
     *)

    (* Do now a 'select' with zero timeout to test the file descriptors. *)

    let (infiles', outfiles', oobfiles') as actual_tuple =
      (* (infiles', outfiles', oobfiles'): Lists of file descriptors that
       * can be handled
       *)
      Unix.select infiles outfiles oobfiles 0.0
	(* Because of the timeout value 0.0, this "select" call cannot block,
	 * and it cannot raise EINTR.
	 *)
    in

    (* Now we have in infiles', outfiles', oobfiles' the actually happened
     * file descriptor events.
     * Furthermore, pure timeout events may have happened, but this is not
     * indicated specially.
     *)
    ignore(self#queue_events actual_tuple);

    (* Now run the queue (without source). *)
    begin try
      tcl_is_running <- true;
      match run with
	  None   -> super#run()
	| Some r -> r (self : #event_system :> event_system) super#run;
    with
	any ->
	  prerr_endline ("Uq_tcl: Uncaught exception: " ^
			 Netexn.to_string any
			);
    end;
    tcl_is_running <- false;
    
    (* Set up for the next round. *)
    self#tcl_setup ();

  (**********************************************************************)
  (* Overriden methods                                                  *)
  (**********************************************************************)
 
  method private source _ =
    (* Override this method: All events are coming from the tcl loop,
     * so disable this source of events
     *)
    ()

  (* After certain method invocations, we must ensure we are attached: *)

  method add_resource g (op,t) =
    super # add_resource g (op,t);
    self # tcl_attach()

  method remove_resource g op =
    super # remove_resource g op;
    self # tcl_attach()

  method add_event e =
    super # add_event e;
    self # tcl_attach ~run_soon:true ()


  method run() =
    (* Calling this method is an error! *)
    failwith "tcl_event_system#run: This method is disabled. Run the TCL event loop instead!"

end
;;
