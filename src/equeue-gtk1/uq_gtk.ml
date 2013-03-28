(* $Id: uq_gtk.ml 1230 2009-05-11 00:37:38Z gerd $ *)

open Equeue
open Unixqueue

IFDEF GTK1 THEN
  type event_id = GMain.Io.event_source
ELSE
  type event_id = GMain.Io.id
END

type gtk_file_handler_rec = 
    { gtk_fd : Unix.file_descr;
      mutable gtk_event_source_in  : (event_id * bool ref) option;
      mutable gtk_event_source_out : (event_id * bool ref) option;
      mutable gtk_event_source_pri : (event_id * bool ref) option;
      mutable gtk_event_source_err : (event_id * bool ref) option;
      mutable gtk_event_source_hup : (event_id * bool ref) option;
    }


type runner =
    event_system -> (unit -> unit) -> unit


class gtk_event_system ?(run : runner option) () =
object (self)
  inherit Unixqueue_select.select_based_event_system() as super

  val mutable gtk_attaching = false
  val mutable gtk_run_soon = false
  val mutable gtk_is_running = false

  val mutable gtk_last_timer = (None : GMain.Timeout.id option)

  val mutable gtk_last_file_handlers =
                (Hashtbl.create 1 :
		   (Unix.file_descr, gtk_file_handler_rec) Hashtbl.t)

  val mutable gtk_watch_tuple = ([], [], [], -1.0)

  method private gtk_attach ?(run_soon=false) () =
    (* Creates an idle callback to reschedule events the next time the
     * event loop is entered. This step can be omitted when this method
     * is called from a Unixqueue callback (it is ensured gtk_setup
     * will be invoked soon).
     *
     * run_soon: if true, the Unixqueue is run once from the idle
     * callback. This can be used to process additional, non-file events.
     *)
    if not gtk_is_running then (
      (* prerr_endline "ATTACH!"; *)
      if not gtk_attaching then (
	gtk_attaching <- true;
	( match gtk_last_timer with
	      Some th -> GMain.Timeout.remove th; gtk_last_timer <- None
	    | None    -> ()
	);
	gtk_last_timer <- Some (GMain.Timeout.add
				  ~ms:0
				  ~callback:
				  (fun () ->
				     self#gtk_setup();
				     gtk_run_soon && (
				       gtk_run_soon <- false;
				       self#gtk_safe_handler false ([],[],[]) ()
				     )
				  ));
      );
      gtk_run_soon <- gtk_run_soon || run_soon;
    )
    (* else prerr_endline "(no attach)"; *)


  method private gtk_setup() =
    let (infiles, outfiles, oobfiles, time) as watch_tuple = super#setup() in

    gtk_watch_tuple <- watch_tuple;

    let ht = Hashtbl.create 50 (* n *) in   (* 50 should be enough *)

    (* Fill ht, the new hash table of file handlers: *)
    List.iter
      (fun fd ->
	 Hashtbl.replace ht fd (true,false,false))
      infiles;
    
    List.iter
      (fun fd ->
	 let (i,_,_) =
	   try Hashtbl.find ht fd
	   with Not_found -> (false,true,false)
	 in
	 Hashtbl.replace ht fd (i,true,false))
      outfiles;
    
    List.iter
      (fun fd ->
	 let (i,o,_) =
	   try Hashtbl.find ht fd
	   with Not_found -> (false,false,true)
	 in
	 Hashtbl.replace ht fd (i,o,true))
      oobfiles;

    let dest_handler (gh, is_active) =
      is_active := false;
      IFDEF GTK1 THEN
        ignore(GMain.Io.remove_source gh);
      ELSE
        (* GTK2 *)
        ignore(GMain.Io.remove gh);
      END
    in

    (* Update GTK file handlers: *)
    Hashtbl.iter
      (fun fd (i,o,x) ->
	 let mk_handler cond il ol xl =
	   let is_active = ref true in
	   (* Note: prio=150 has slightly lower priority than resize/redraw
	    * operations, but higher priority than idle callbacks
	    *)
	   let gh =
	     GMain.Io.add_watch 
	       ~prio:150
	       ~cond:(
		 IFDEF GTK2_IO_ADD_WATCH_SUPPORTS_LISTS THEN
		   [cond]
		 ELSE
		   cond
		 END)
	       ~callback:(
		 fun _ ->
		   !is_active &&
		     self#gtk_safe_handler true (il,ol,xl) ())
	       (GMain.Io.channel_of_descr fd) in
	   (gh, is_active) in
	 let g = 
	   try Hashtbl.find gtk_last_file_handlers fd 
	   with Not_found ->
	     { gtk_fd = fd;
	       gtk_event_source_in = None;
	       gtk_event_source_out = None;
	       gtk_event_source_pri = None;
	       gtk_event_source_err = None;
	       gtk_event_source_hup = None; } in
	 ( match g.gtk_event_source_in with
	       None when i ->
		 g.gtk_event_source_in <- Some(mk_handler Uq_gtk_helper._in [fd] [] []);
	     | Some s when not i ->
		 dest_handler s;
		 g.gtk_event_source_in <- None
	     | _ ->
		 ()
	 );
	 ( match g.gtk_event_source_out with
	       None when o ->
		 g.gtk_event_source_out <- Some(mk_handler `OUT [] [fd] []);
	     | Some s when not o ->
		 dest_handler s;
		 g.gtk_event_source_out <- None
	     | _ ->
		 ()
	 );
	 ( match g.gtk_event_source_pri with
	       None when x ->
		 g.gtk_event_source_pri <- Some(mk_handler `PRI [] [] [fd]);
	     | Some s when not x ->
		 dest_handler s;
		 g.gtk_event_source_pri <- None
	     | _ ->
		 ()
	 );
	 ( match g.gtk_event_source_err with
	       None when i || o || x ->
		 let il = if i then [fd] else [] in
		 let ol = if o then [fd] else [] in
		 let xl = if x then [fd] else [] in
		 g.gtk_event_source_err <- Some(mk_handler `ERR il ol xl);
	     | Some s when not (i || o || x) ->
		 dest_handler s;
		 g.gtk_event_source_err <- None
	     | _ ->
		 ()
	 );
	 ( match g.gtk_event_source_hup with
	       None when i || o || x ->
		 let il = if i then [fd] else [] in
		 let ol = if o then [fd] else [] in
		 let xl = if x then [fd] else [] in
		 g.gtk_event_source_hup <- Some(mk_handler `HUP il ol xl);
	     | Some s when not (i || o || x) ->
		 dest_handler s;
		 g.gtk_event_source_hup <- None
	     | _ ->
		 ()
	 );
	 Hashtbl.replace gtk_last_file_handlers fd g
      )
      ht;

    Hashtbl.iter
      (fun fd g ->
	 if not (Hashtbl.mem ht fd) then (
	   ( match g.gtk_event_source_in with
		 Some s ->
		   dest_handler s;
		   g.gtk_event_source_in <- None
	       | _ -> ()
	   );
	   ( match g.gtk_event_source_out with
		 Some s ->
		   dest_handler s;
		   g.gtk_event_source_out <- None
	       | _ -> ()
	   );
	   ( match g.gtk_event_source_pri with
		 Some s ->
		   dest_handler s;
		   g.gtk_event_source_pri <- None
	       | _ -> ()
	   );
	   ( match g.gtk_event_source_err with
		 Some s ->
		   dest_handler s;
		   g.gtk_event_source_err <- None
	       | _ -> ()
	   );
	   ( match g.gtk_event_source_hup with
		 Some s ->
		   dest_handler s;
		   g.gtk_event_source_hup <- None
	       | _ -> ()
	   );
	 )
      )
      gtk_last_file_handlers;

    let watching_files = infiles <> [] || 
			 outfiles <> [] || 
			 oobfiles <> [] in
    

    (* Remove the old timer, if any. *)
    begin match gtk_last_timer with
	None -> ()
      | Some th -> 
	  GMain.Timeout.remove th;
	  gtk_last_timer <- None;
    end;
    gtk_attaching <- false;

    (* Set the new timer, if necessary *)
    if time >= 0.0 then begin
      (* prerr_endline ("Timeout: " ^ string_of_float time); *)
      gtk_last_timer <- Some (GMain.Timeout.add
				~ms:(int_of_float (time *. 1E3 +. 0.5))
				~callback:(self#gtk_safe_handler false ([],[],[])));
    end;

    (* If no handler is active, detach. *)
    (*
    if gtk_last_timer = None && not watching_files then begin
      gtk_attached <- false;
      (* prerr_endline "Detached!"; *)
    end;
    *)

  method private gtk_safe_handler keep watch_tuple () =
    try
      self#gtk_handler watch_tuple ();
      keep
    with
	any ->
	  prerr_endline("Uq_gtk: Internal uncaught exception: " ^
			Netexn.to_string any);
	  raise any;
	  keep

  method private gtk_handler watch_tuple () =

    (* IMPORTANT:
     * It is possible that this is a "ghost event". We need to test whether
     * there is a resource for the event or not.
     *)

    (* Do now a 'select' with zero timeout to test the file descriptors. *)

    let (infiles,outfiles,oobfiles) = watch_tuple in

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
      gtk_is_running <- true;
      match run with
	  None   -> super#run()
	| Some r -> r (self : #event_system :> event_system) super#run;
    with
	any ->
	  prerr_endline ("Uq_gtk: Uncaught exception: " ^
			 Netexn.to_string any
			);
    end;
    gtk_is_running <- false;
    
    (* Set up for the next round. *)
    self#gtk_setup ();

  (**********************************************************************)
  (* Overriden methods                                                  *)
  (**********************************************************************)
 
  method private source _ =
    (* Override this method: All events are coming from the glib loop,
     * so disable this source of events
     *)
    ()

  (* After certain method invocations, we must ensure we are attached: *)

  method add_resource g (op,t) =
    super # add_resource g (op,t);
    self # gtk_attach()

  method remove_resource g op =
    super # remove_resource g op;
    self # gtk_attach()

  method add_event e =
    super # add_event e;
    self # gtk_attach ~run_soon:true ()


  method run() =
    (* Calling this method is an error! *)
    failwith "gtk_event_system#run: This method is disabled. Run the Glib event loop instead!"

end
;;
