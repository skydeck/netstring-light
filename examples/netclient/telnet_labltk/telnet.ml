open Tk;;
open Telnet_client;;

(*let _ = Unixqueue.set_debug_mode true;;*)

let user_font = "-adobe-courier-medium-r-normal-*-*-120-*-*-*-*-*-1";;
let button_font = "-adobe-helvetica-bold-r-normal-*-*-100-*-*-*-*-*-1";;
let text_font = "-adobe-helvetica-medium-r-normal-*-*-120-*-*-*-*-*-1";;


let string_of_option_char c =
  let p = option_of_char c in
  match p with
      Telnet_binary       -> "TRANSMIT BINARY"
    | Telnet_echo         -> "ECHO"
    | Telnet_suppress_GA  -> "SUPPRESS GO-AHEAD"
    | Telnet_status       -> "STATUS"
    | Telnet_timing_mark  -> "TIMING MARK"
    | Telnet_ext_opt_list -> "EXTENDED OPTIONS LIST"
    | Telnet_end_of_rec   -> "END OF RECORD"
    | Telnet_window_size  -> "WINDOW SIZE"
    | Telnet_term_speed   -> "TERMINAL SPEED"
    | Telnet_term_type    -> "TERMINAL TYPE"
    | Telnet_X_display    -> "X DISPLAY"
    | Telnet_linemode     -> "LINEMODE"
    | Telnet_flow_ctrl    -> "FLOW CONTROL"
    | Telnet_auth         -> "AUTHENTICATION"
    | Telnet_new_environ  -> "NEW ENVIRONMENT"
    | Telnet_option k     -> "OPTION " ^ string_of_int k
;;


let some p =
  match p  with
      None -> failwith "some"
    | Some q -> q
;;

let connparm_active = ref false;;      (* whether "connect" window is active *)
let hostname_tv = ref None;;
let port_tv = ref None;;

let get_connection_parameters parent f =
  if not !connparm_active then begin
    if !hostname_tv = None then hostname_tv := Some(Textvariable.create());
    if !port_tv = None then begin
      let p = Textvariable.create() in
      port_tv := Some p;
      Textvariable.set p "23"
    end;
    let htv = some !hostname_tv in
    let ptv = some !port_tv in
    let w = Toplevel.create parent in
    Wm.title_set w "Connect";

    let vanish() =
      destroy w;
      connparm_active := false
    in

    let connect() =
      vanish();
      let hostname = Textvariable.get htv in
      let port_s = Textvariable.get ptv in
      let port =
	try int_of_string port_s with _ -> 0 in
      f hostname port
    in

    bind ~events:[`Destroy] ~action:(fun ev -> vanish()) w;

    let px = Winfo.rootx parent in
    let py = Winfo.rooty parent in
    let x = px + 20 in
    let y = py + 20 in

    Wm.geometry_set w ("+" ^ string_of_int x ^ "+" ^ string_of_int y);

    let f1 = Frame.create w in
    let f2 = Frame.create w in
    let f3 = Frame.create w in
    let l1 = Label.create 
	     ~text:"Hostname:" ~textwidth:15 ~anchor:`E ~font:text_font f1 in
    let e1 = Entry.create 
	     ~width:30 ~textvariable:htv ~font:user_font f1 in
    let l2 = Label.create 
	     ~text:"Port:" ~textwidth:15 ~anchor:`E ~font:text_font f2 in
    let e2 = Entry.create 
	     ~width:5 ~textvariable:ptv ~font:user_font f2 in
    let b1 = Button.create
	     ~text:"Connect" ~command:connect ~font:button_font f3 in
    let b2 = Button.create
	     ~text:"Cancel" ~command:vanish ~font:button_font f3 in
    
    pack ~side:`Left [ b1; b2 ];
    pack ~side:`Left [ coe l1; coe e1 ];
    pack ~side:`Left [ coe l2; coe e2 ];
    pack ~anchor:`W  [ f1; f2; f3 ];

    connparm_active := true;
  end
;;


let rec fill_main_window w =
  let connect_command = ref (fun () -> ()) in
  let disconnect_command = ref (fun () -> ()) in
  let interrupt_command = ref (fun () -> ()) in
  let abort_command = ref (fun () -> ()) in
  let ayt_command = ref (fun () -> ()) in
  let brk_command = ref (fun () -> ()) in
  let erase_line_command = ref (fun () -> ()) in
  let synch_command = ref (fun () -> ()) in
  let keypress_handler = ref (fun _ -> ()) in
  let resize_handler = ref (fun _ -> ()) in
  let toggle_local_option = ref (fun _ _ _ -> ()) in
  let toggle_remote_option = ref (fun _ _ _ -> ()) in

  let ues = new Uq_tcl.tcl_event_system() in
  let session = new telnet_session in
  session # set_event_system ues;

  let connected = ref false in

(*
  let opts = session # get_options in
  session # set_options
    { opts with
	verbose_connection = false; };
 *)

  (**** CREATE WINDOWS, MENUS ETC. ****)

  Wm.title_set w "Telnet";

  let buttonbar_frame = Frame.create w in
  let command_frame = Frame.create w in
  let user_frame = Frame.create w in

  let b0 = Menubutton.create 
	     ~text:"Connection" ~font:button_font buttonbar_frame in
  let b1 = Menubutton.create 
	     ~text:"Signals"  ~font:button_font buttonbar_frame in
  let b2 = Menubutton.create
	     ~text:"Local options" ~font:button_font buttonbar_frame in
  let b3 = Menubutton.create
	     ~text:"Remote options" ~font:button_font buttonbar_frame in

  (* "Connection" menu *)

  let cmd f () = !f () in

  let connection_menu = Menu.create b0 in
  let connect_index =
    Menu.add_command 
      ~label:"Connect to server" 
      ~command:(cmd connect_command) ~font:text_font
      connection_menu;
    Menu.index connection_menu `Last in
  let disconnect_index =
    Menu.add_command 
      ~label:"Disconnect" ~state:`Disabled 
      ~command:(cmd disconnect_command)
      ~font:text_font
      connection_menu;
    Menu.index connection_menu `Last in
  let new_win_index =
    Menu.add_command 
      ~label:"New window" ~font:text_font
      ~command:(fun () -> let w' = Toplevel.create w in fill_main_window w')
      connection_menu;
    Menu.index connection_menu `Last in


  (* "Signals" menu *)

  let signals_menu = Menu.create b1 in
  let interrupt_index =
    Menu.add_command 
      ~label:"Interrupt process" ~command:(cmd interrupt_command) ~font:text_font
      signals_menu;
    Menu.index signals_menu `Last in
  let brk_index =
    Menu.add_command 
      ~label:"Break" ~command:(cmd brk_command) ~font:text_font
      signals_menu;
    Menu.index signals_menu `Last in
  let ao_index =
    Menu.add_command 
      ~label:"Abort output" ~command:(cmd abort_command) ~font:text_font
      signals_menu;
    Menu.index signals_menu `Last in
  let ayt_index =
    Menu.add_command 
      ~label:"Are you there?" ~command:(cmd ayt_command) ~font:text_font
      signals_menu;
    Menu.index signals_menu `Last in
  let el_index =
    Menu.add_command 
      ~label:"Erase line" ~command:(cmd erase_line_command) ~font:text_font
      signals_menu;
    Menu.index signals_menu `Last in
  let synch_index =
    Menu.add_command 
      ~label:"Send Synch sequence" ~command:(cmd synch_command) ~font:text_font
      signals_menu;
    Menu.index signals_menu `Last in


  (* "Local options" menu *)

  let lopts_menu = Menu.create b2 in

  let lbinary_tv = Textvariable.create() in
  Textvariable.set lbinary_tv "false";
  let lbinary_index =
    Menu.add_checkbutton 
      ~label:"Transmit 8 bits, not 7" ~font:text_font ~variable:lbinary_tv
      ~onvalue:"true" ~offvalue:"false"
      lopts_menu;
    Menu.index lopts_menu `Last in

  let lwinsize_tv = Textvariable.create() in
  Textvariable.set lwinsize_tv "false";
  let lwinsize_index =
    Menu.add_checkbutton 
      ~label:"Negotiate window size" ~font:text_font ~variable:lwinsize_tv
      ~onvalue:"true" ~offvalue:"false"
      lopts_menu;
    Menu.index lopts_menu `Last in
  let old_width = ref (-1) in  (* What I think the server thinks... *)
  let old_height = ref (-1) in

  let lopts_spec =
    [ lbinary_index,    Telnet_binary,      lbinary_tv;
      lwinsize_index,   Telnet_window_size, lwinsize_tv;
    ] in

  List.iter
    (fun (index,opt,tv) ->
       Menu.configure_checkbutton
	 ~command:(fun () -> !toggle_local_option index opt tv)
	 lopts_menu (`Num index)
    )
    lopts_spec;

  (* "Remote options" menu *)

  let ropts_menu = Menu.create b3 in

  let recho_tv = Textvariable.create() in
  Textvariable.set recho_tv "false";
  let recho_index =
    Menu.add_checkbutton 
      ~label:"Echo" ~font:text_font ~variable:recho_tv
      ~onvalue:"true" ~offvalue:"false"
      ropts_menu;
    Menu.index ropts_menu `Last in

  let rsuppressga_tv = Textvariable.create() in
  Textvariable.set rsuppressga_tv "false";
  let rsuppressga_index =
    Menu.add_checkbutton 
      ~label:"Suppress Go-Ahead" ~font:text_font ~variable:rsuppressga_tv
      ~onvalue:"true" ~offvalue:"false"
      ropts_menu;
    Menu.index ropts_menu `Last in
  let rbinary_tv = Textvariable.create() in
  Textvariable.set rbinary_tv "false";
  let rbinary_index =
    Menu.add_checkbutton 
      ~label:"Transmit 8 bits, not 7" ~font:text_font ~variable:rbinary_tv
      ~onvalue:"true" ~offvalue:"false"
      ropts_menu;
    Menu.index ropts_menu `Last in

  let ropts_spec = 
    [ recho_index,         Telnet_echo,          recho_tv;
      rsuppressga_index,   Telnet_suppress_GA,   rsuppressga_tv;
      rbinary_index,       Telnet_binary,        rbinary_tv;
    ] in

  List.iter
    (fun (index,opt,tv) ->
       Menu.configure_checkbutton
	 ~command: (fun () -> !toggle_remote_option index opt tv)
	 ropts_menu (`Num index)
    )
    ropts_spec;

  Menubutton.configure ~menu:connection_menu b0;
  Menubutton.configure ~menu:signals_menu b1;
  Menubutton.configure ~menu:lopts_menu b2;
  Menubutton.configure ~menu:ropts_menu b3;

  pack ~side:`Left [ b0; b1; b2; b3 ];

  let command_box = Text.create 
		      ~height:5 ~width:80 ~font:user_font command_frame in

  pack ~fill:`X [ command_box ];

  (* Create user_box, and determine the pixel per character ratio.
   * It is assumed that there is a fixed amount of padding pixels,
   * and that the rest is linear to the size of the window.
   *)

  let user_box = Text.create 
		   ~height:25 ~width:81 ~font:user_font user_frame in

  let rh1 = Winfo.reqheight user_box in
  let rw1 = Winfo.reqwidth user_box in

  Text.configure ~height:24 ~width:80 user_box;

  let rh0 = Winfo.reqheight user_box in
  let rw0 = Winfo.reqwidth user_box in

  let xpixels = rw1 - rw0 in
  let ypixels = rh1 - rh0 in

  let xoffset = rw0 - 80 * xpixels in
  let yoffset = rh0 - 24 * ypixels in

  bind ~events:[ `KeyPress ] ~breakable:true ~fields:[`Char] 
       ~action:(fun ev -> !keypress_handler ev) user_box;

  pack ~expand:true ~fill:`Both [ user_box ];

  pack ~anchor:`W [ buttonbar_frame ];
  pack ~anchor:`W ~fill:`X [ command_frame ];
  pack ~anchor:`W ~expand:true ~fill:`Both [ user_frame ];

  bind ~events:[ `Configure ] ~breakable:true ~action:(fun ev -> !resize_handler ev) w;

  (**** SIGNALS MENU ****)
  
  let update_signals_menu() =
    let state = if !connected then `Normal else `Disabled in
    Menu.configure_command ~state signals_menu (`Num interrupt_index);
    Menu.configure_command ~state signals_menu (`Num brk_index);
    Menu.configure_command ~state signals_menu (`Num ao_index);
    Menu.configure_command ~state signals_menu (`Num ayt_index);
    Menu.configure_command ~state signals_menu (`Num el_index);
    Menu.configure_command ~state signals_menu (`Num synch_index);
  in

  (**** OPTION MENUS ****)

  let update_option_menus() =
    (* Set the checkbuttons depending on the result of the negotiation *)
    List.iter
      (fun (index, opt, tv) ->
	 let selected = session # get_remote_option opt = Accepted in
	 Textvariable.set tv (if selected then "true" else "false");
	 let state = if !connected then `Normal else `Disabled in
	 Menu.configure_checkbutton ~state ropts_menu (`Num index) 
      )
      ropts_spec;
    List.iter
      (fun (index, opt, tv) ->
	 let selected = session # get_local_option opt = Accepted in
	 Textvariable.set tv (if selected then "true" else "false");
	 let state = if !connected then `Normal else `Disabled in
	 Menu.configure_checkbutton ~state lopts_menu (`Num index)
      )
      lopts_spec
  in
  update_option_menus();     (* update right at the beginning *)
  update_signals_menu();

  (**** COMMAND BOX ****)

  let add_to_cmd_box s =
    Text.insert ~index:(`End,[]) ~text:s  command_box;
    Text.yview_index_pickplace command_box (`End, []);
  in

  let print_output_queue() =
    let last = (`End, []) in
    Queue.iter
      (fun cmd ->
	 match cmd with
	     Telnet_data s ->
	       add_to_cmd_box "out> data\n";
	   | Telnet_nop ->
	       add_to_cmd_box "out> NOP\n";
	   | Telnet_dm ->
	       add_to_cmd_box "out> DATA MARK\n";
	   | Telnet_brk ->
	       add_to_cmd_box "out> BREAK\n";
	   | Telnet_ip ->
	       add_to_cmd_box "out> INTERRUPT PROCESS\n";
	   | Telnet_ao ->
	       add_to_cmd_box "out> ABORT OUTPUT\n";
	   | Telnet_ayt ->
	       add_to_cmd_box "out> ARE YOU THERE?\n";
	   | Telnet_ec ->
	       add_to_cmd_box "out> ERASE CHARACTER\n";
	   | Telnet_el ->
	       add_to_cmd_box "out> ERASE LINE\n";
	   | Telnet_ga ->
	       add_to_cmd_box "out> GO AHEAD\n";
	   | Telnet_sb c ->
	       let code = string_of_option_char c in
	       add_to_cmd_box ("out> SUBNEGOTATION " ^ code ^ "\n");
	   | Telnet_se ->
	       add_to_cmd_box "out> END OF SUBNEGOTATION\n";
	   | Telnet_will c ->
	       let code = string_of_option_char c in
	       add_to_cmd_box ("out> WILL " ^ code ^ "\n");
	   | Telnet_wont c ->
	       let code = string_of_option_char c in
	       add_to_cmd_box ("out> WON'T " ^ code ^ "\n");
	   | Telnet_do c ->
	       let code = string_of_option_char c in
	       add_to_cmd_box ("out> DO " ^ code ^ "\n");
	   | Telnet_dont c ->
	       let code = string_of_option_char c in
	       add_to_cmd_box ("out> DON'T " ^ code ^ "\n");
	   | Telnet_unknown c ->
	       let code = string_of_int (Char.code c) in
	       add_to_cmd_box ("out> UNKNOWN COMMAND " ^ code ^ "\n");
	   | Telnet_eof ->
	       add_to_cmd_box "out> END OF STREAM\n";
	   | Telnet_timeout ->
	       ()       (* not possible *)
      )
      session # output_queue;
  in


  (**** TERMINAL EMULATION ****)

  (* Supports only Backspace, TAB, CR, LF *)

  let terminal_vpos = ref 1 in       (* vertical position *)
  let terminal_hpos = ref 0 in       (* horizontal position *)
  let terminal_atend = ref true in   (* if cursor is at the end of text *)

  let rec write_to_terminal c =
    match c with
      | '\008' ->
	  (* Backspace *)
	  if !terminal_hpos > 0 then begin
	    decr terminal_hpos;
	    terminal_atend := false;
	    Text.mark_set 
	      user_box "insert" (`Linechar(!terminal_vpos,
					   !terminal_hpos), []);
	  end
      | '\009' ->
	  (* TAB *)
	  let n = 8 - (!terminal_hpos mod 8) in
	  for k = 1 to n do write_to_terminal ' ' done
      | '\010' ->
	  (* LF *)
	  let s = "\n" ^ String.make !terminal_hpos ' ' in
	  Text.insert ~index:(`End, []) ~text:s user_box;
	  terminal_atend := true;
	  incr terminal_vpos;
	  Text.mark_set 
	    user_box "insert" (`Linechar(!terminal_vpos,
					 !terminal_hpos), []);
	  Text.yview_index_pickplace user_box (`End, []);
      | '\013' ->
	  (* CR *)
	  if !terminal_hpos > 0 then begin
	    terminal_hpos := 0;
	    Text.mark_set 
	      user_box "insert" (`Linechar(!terminal_vpos,
					   !terminal_hpos), []);
	    terminal_atend := false
	  end
      | ('\032'..'\126'|'\160'..'\255') ->
	  if !terminal_atend then begin
	    Text.insert (`End, []) (String.make 1 c) user_box;
	    incr terminal_hpos;
	  end
	  else begin
	    Text.delete_char user_box (`Linechar(!terminal_vpos,
						 !terminal_hpos), []);
	    Text.insert 
	      (`Linechar(!terminal_vpos,
			 !terminal_hpos), [])
	      (String.make 1 c) user_box;
	    incr terminal_hpos;
	  end;
	  Text.mark_set 
	    user_box "insert" (`Linechar(!terminal_vpos,
					 !terminal_hpos), []);
      | _ -> ()
  in

  let read_from_terminal s =
    (* 's' is user input (from the keypress handler) *)
    let remote_echo_mode = session # get_remote_option Telnet_echo in
    if remote_echo_mode <> Accepted then begin
      (* Echo locally *)
      let l = String.length s in
      for i = 0 to l-1 do
	write_to_terminal s.[i];
	if s.[i] = '\013' then
	  write_to_terminal '\010'
      done
    end;
    (* send the input to the other side *)
    let oq = session # output_queue in
    let l = String.length s in
    for i = 0 to l-1 do
      match s.[i] with
	  '\013' ->
	    Queue.add (Telnet_data(String.make 1 '\013')) oq;
	    Queue.add (Telnet_data(String.make 1 '\010')) oq;
	| '\008' ->
	    Queue.add Telnet_ec oq;
	| _ ->
	    Queue.add (Telnet_data(String.make 1 s.[i])) oq;
    done;
    print_output_queue();
    session # update()
  in

  (**** TELNET CALLBACK ****)

  let ui_do_disconnect() =
    (* Set the UI state to "disconnected" *)
    Menu.configure_command 
      ~state:`Normal
      connection_menu (`Num connect_index);
    Menu.configure_command 
      ~state:`Disabled
      connection_menu (`Num disconnect_index);
    Wm.title_set w "Telnet";
    connected := false;
    update_option_menus();
    update_signals_menu();
  in

  let rec telnet_callback is_urgent =
    let last = (`End, []) in
    if is_urgent then begin
      add_to_cmd_box
	" in> data mark seen, and data path cleared\n";
    end;
    let iq = session # input_queue in
    if Queue.length iq > 0 then begin
      let cmd = Queue.take iq in
      begin match cmd with
	  Telnet_data s ->
	    let l = String.length s in
	    for i = 0 to l-1 do
	      write_to_terminal s.[i]
	    done;
	    add_to_cmd_box " in> data (see below)\n";
	| Telnet_nop ->
	    add_to_cmd_box " in> NOP\n";
	| Telnet_dm ->
	    add_to_cmd_box " in> DATA MARK\n";
	| Telnet_brk ->
	    add_to_cmd_box " in> BREAK\n";
	| Telnet_ip ->
	    add_to_cmd_box " in> INTERRUPT PROCESS\n";
	| Telnet_ao ->
	    add_to_cmd_box " in> ABORT OUTPUT\n";
	| Telnet_ayt ->
	    add_to_cmd_box " in> ARE YOU THERE?\n";
	| Telnet_ec ->
	    add_to_cmd_box " in> ERASE CHARACTER\n";
	| Telnet_el ->
	    add_to_cmd_box " in> ERASE LINE\n";
	| Telnet_ga ->
	    add_to_cmd_box " in> GO AHEAD\n";
	| Telnet_sb c ->
	    let code = string_of_option_char c in
	    add_to_cmd_box (" in> SUBNEGOTATION " ^ code ^ "\n")
	| Telnet_se ->
	    add_to_cmd_box " in> END OF SUBNEGOTATION\n";
	| Telnet_will c ->
	    let code = string_of_option_char c in
	    add_to_cmd_box (" in> WILL " ^ code ^ "\n");
	    session # process_option_command cmd;
	    update_option_menus();
	| Telnet_wont c ->
	    let code = string_of_option_char c in
	    add_to_cmd_box (" in> WON'T " ^ code ^ "\n");
	    session # process_option_command cmd;
	    update_option_menus();
	| Telnet_do c ->
	    let code = string_of_option_char c in
	    add_to_cmd_box (" in> DO " ^ code ^ "\n");
	    session # process_option_command cmd;
	    update_option_menus();
	| Telnet_dont c ->
	    let code = string_of_option_char c in
	    add_to_cmd_box (" in> DON'T " ^ code ^ "\n");
	    session # process_option_command cmd;
	    update_option_menus();
	| Telnet_unknown c ->
	    let code = string_of_int (Char.code c) in
	    add_to_cmd_box (" in> UNKNOWN COMMAND " ^ code ^ "\n");
	| Telnet_eof ->
	    add_to_cmd_box " in> END OF STREAM\n";
	    ui_do_disconnect();
	| Telnet_timeout ->
	    add_to_cmd_box " in> TIMEOUT\n";
      end;
      telnet_callback false
    end
    else begin (* All input processed *)
      print_output_queue();
      Tk.update_idletasks()
    end
  in

  (**** IMPLEMENTATION OF COMMANDS AND TCL CALLBACKS ****)

  connect_command :=
  (fun () ->
     (* New toplevel window: *)
     get_connection_parameters
       w
       (fun hostname port ->
	  Wm.title_set w ("Telnet " ^ hostname ^ ":" ^ string_of_int port);
	  session # set_connection(Telnet_connect(hostname,port));
	  session # set_callback telnet_callback;
	  session # set_exception_handler
	    (fun x ->
	       let x_text =
		 match x with
		     Sys_error s -> s
		   | Unix.Unix_error(u,_,s) ->
		       (if s = "" then "" else s ^ ": ") ^
		       (Unix.error_message u)
		   | _ ->
		       Printexc.to_string x
	       in
	       let t = Toplevel.create w in
	       Wm.title_set t "Error";
	       let m = Message.create 
			 ~text:x_text ~font:text_font ~aspect:300 t in
	       let ok = Button.create
			  ~text:"OK" ~font:button_font 
			  ~command:(fun () -> destroy t) t in
	       pack [coe m; coe ok];
	       ui_do_disconnect();
	       session # reset();
	    );
	       
	  session # attach();
	  Menu.configure_command
	    ~state:`Disabled
	    connection_menu (`Num connect_index);
	  Menu.configure_command 
	    ~state:`Normal
	    connection_menu (`Num disconnect_index);
	  connected := true;
	  (* Enable all available telnet options *)
	  List.iter
	    (fun (_, opt, tv) ->
	       session # reset_remote_option opt;
	       session # enable_remote_option opt;
	    )
	    ropts_spec;
	  List.iter
	    (fun (_, opt, tv) ->
	       session # reset_local_option opt;
	       session # enable_local_option opt;
	    )
	    lopts_spec;
	  update_option_menus();
	  update_signals_menu();
       );
  );

  disconnect_command :=
  (fun () ->
     let oq = session # output_queue in
     Queue.add Telnet_eof oq;
     session # update();
     print_output_queue();
     Tk.update_idletasks()
  );

  interrupt_command :=
  (fun () ->
     let oq = session # output_queue in
     Queue.add Telnet_ip oq;
     session # update();
     print_output_queue();
     Tk.update_idletasks()
  );

  brk_command :=
  (fun () ->
     let oq = session # output_queue in
     Queue.add Telnet_brk oq;
     session # update();
     print_output_queue();
     Tk.update_idletasks()
  );

  abort_command :=
  (fun () ->
     let oq = session # output_queue in
     Queue.add Telnet_ao oq;
     session # update();
     print_output_queue();
     Tk.update_idletasks()
  );

  ayt_command :=
  (fun () ->
     let oq = session # output_queue in
     Queue.add Telnet_ayt oq;
     session # update();
     print_output_queue();
     Tk.update_idletasks()
  );

  erase_line_command :=
  (fun () ->
     let oq = session # output_queue in
     Queue.add Telnet_el oq;
     session # update();
     print_output_queue();
     Tk.update_idletasks()
  );

  synch_command :=
  (fun () ->
     session # send_synch [];
     Tk.update_idletasks()
  );

  keypress_handler :=
  (fun ev ->
     let s = ev.ev_Char in
     read_from_terminal s;
     break()
  );

  resize_handler :=
  (fun ev ->
     (* Invoked if the characteristics of the top-level window change *)
     if session # get_local_option Telnet_window_size = Accepted then begin
       let width = (Winfo.width user_box - xoffset) / xpixels in
       let height = (Winfo.height user_box - yoffset) / ypixels in
       if width <> !old_width or height <> !old_height then begin
	 old_width := width;
	 old_height := height;
	 let s = String.create 4 in
	 s.[0] <- Char.chr(width lsr 8);
	 s.[1] <- Char.chr(width land 0xff);
	 s.[2] <- Char.chr(height lsr 8);
	 s.[3] <- Char.chr(height land 0xff);
	 let oq = session # output_queue in
	 Queue.add (Telnet_sb (char_of_option Telnet_window_size)) oq;
	 Queue.add (Telnet_data s) oq;
	 Queue.add Telnet_se oq;
	 session # update();
	 print_output_queue();
	 
(*	 prerr_endline (string_of_int width ^ " x " ^ string_of_int height);*)

	 
       end
     end
  );

  toggle_remote_option :=
  (fun index opt tv ->
     let selected = session # get_remote_option opt = Accepted in
     if selected then begin
       session # disable_remote_option opt;
       session # update();
       print_output_queue();
     end
     else begin
       session # enable_remote_option opt;
       session # request_remote_option opt;
       session # update();
       print_output_queue();
     end;
     update_option_menus();
  );

  toggle_local_option :=
  (fun index opt tv ->
     let selected = session # get_local_option opt = Accepted in
     if selected then begin
       session # disable_local_option opt;
       session # update();
       print_output_queue();
     end
     else begin
       session # enable_local_option opt;
       session # offer_local_option opt;
       session # update();
       print_output_queue();
     end;
     update_option_menus();
  )
;;

