#require "netclient";;

(* This is an example for the telnet client. The function below
 * connects with localhost, and logs the user in. It simulates
 * keyboard typing for the username and the password, and finally
 * starts the command "ls".
 *
 * The example may or may not work with your version of telnetd.
 * The program expects the string "login" before the user name must
 * be typed in, and it expects the string "password" before the password
 * must be entered. Furthermore, a new command line is recognized 
 * by the characters >, # or $.
 *)

open Telnet_client;;

type state =
    Start               (* just connected *)
  | Username_sent       (* the user name has been sent to the server *)
  | Password_sent       (* the password has been sent to the server *)
  | Command_sent        (* the command to execute has been sent to the server *)
;;


let login_re = Str.regexp_case_fold "\\(.\\|\n\\)*login";;
let passwd_re = Str.regexp_case_fold "\\(.\\|\n\\)*password";;
let cmd_re = Str.regexp "\\(.\\|\n\\)*[>$#]";;


let login_and_ls username password =
  (* Create a new event system, and the telnet session. We need the 
   * event system only to call Unixqueue.once.
   *)
  let esys = Unixqueue.create_unix_event_system() in
  let session = new telnet_session in
  let state = ref Start in

  let send_string s new_state =
    (* Emulate keyboard typing of the string s. Between the characters there
     * is a delay of 0.1 seconds.
     * When the string has been completely sent, change the state to
     * new_state.
     *)
    let t = ref 0.1 in
    let g = Unixqueue.new_group esys in
    let l = String.length s in
    for i = 0 to l - 1 do
      let c = s.[i] in
      let cs = if c = '\n' then "\r\n" else String.make 1 c in
      (* Do the function !t seconds in the future: *)
      Unixqueue.once esys g !t
	(fun () ->
	   Queue.add (Telnet_data cs) session#output_queue;
	   (* We must call update because we are outside of the regular
	    * callback function. Otherwise the session object would not
	    * notice that the queue has been extended.
	    *)
	   session # update();   
	   if i = l-1 then 
	     state := new_state
	);
      t := !t +. 0.1;
    done
  in

  let got_input is_urgent =
    (* This is the callback function. The session object calls it when
     * telnet commands have been added to the input queue.
     *)
    let oq = session # output_queue in
    let iq = session # input_queue in
    (* Process the input queue command by command: *)
    while Queue.length iq > 0 do
      let cmd = Queue.take iq in
      match cmd with
	| Telnet_will _
	| Telnet_wont _
	| Telnet_do _
	| Telnet_dont _ ->
	    (* These are the commands used to negotiate the telnet options.
	     * The session object can do it for you.
	     *)
	    session # process_option_command cmd
	| Telnet_data s ->
	    (* The data string s has been received. *)
	    ( match !state with
		  Start ->
		    if Str.string_match login_re s 0 then begin
		      (* Assume the host wants our username, and send it. *)
		      send_string (username ^ "\n") Username_sent
		    end
		| Username_sent ->
		    if Str.string_match passwd_re s 0 then begin
		      (* Assume the host wants our password, and send it. *)
		      send_string (password ^ "\n") Password_sent;
		    end
		| Password_sent ->
		    if Str.string_match cmd_re s 0 then begin
		      (* Assume the host wants the command: *)
		      (* Disable now echoing: *)
		      session # disable_remote_option Telnet_echo;
		      (* Send the command "ls" 0.1 seconds in the future.
		       * This way Telnet_echo can be disabled in the
		       * meantime.
		       *)
		      let g = Unixqueue.new_group esys in
		      Unixqueue.once esys g 0.1
			(fun () ->
			   Queue.add (Telnet_data("ls\n")) oq;
			   session # update();
			   state := Command_sent
			)
		    end;
		| Command_sent ->
		    print_string s;
		    (* Again the command-line prompt? *)
		    if Str.string_match cmd_re s 0 then
		      Queue.add Telnet_eof oq;       (* terminate the session *)
	    )
	| Telnet_eof ->
	    ()
	| _ ->
	    (* Unexpected command. *)
	    ()
    done
  in

  session # set_event_system esys;
  let opts = session # get_options in
  session # set_options { opts with 
			    verbose_connection = false;
			    verbose_input = false;
			    verbose_output = false };

  session # set_connection (Telnet_connect("localhost", 23));
  session # enable_remote_option Telnet_suppress_GA;
  session # enable_remote_option Telnet_echo;
  session # set_callback got_input;
  session # attach();
  session # run()
;;
