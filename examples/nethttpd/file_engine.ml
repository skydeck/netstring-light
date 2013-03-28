(* Serves $HOME as docroot *)

open Nethttpd_types
open Nethttpd_services
open Nethttpd_engine
open Printf

let counter = ref 0

let hit_counter env (cgi : Netcgi.cgi_activation) =
  let cur_counter = !counter in
  incr counter;

  if cgi # argument_value "next" <> "" then
    raise (Redirect_response(cgi # argument_value "next", env # input_header));
  (* Quite funny:
   * http://localhost:8765/easteregg?next=/easteregg%3Fnext%3D/
   *)


  cgi # output # output_string "<html><body>\n";
  cgi # output # output_string (sprintf "Counter = %d<br>\n" cur_counter);
  List.iter
    (fun arg ->
       cgi # output # output_string (sprintf "Arg %s = %s<br>\n"
				   (Netencoding.Html.encode_from_latin1 arg#name)
				   (Netencoding.Html.encode_from_latin1 arg#value))
    )
    cgi # arguments;
  cgi # output # output_string (sprintf "SCRIPT_NAME = %s<br>\n" 
				  (Netencoding.Html.encode_from_latin1 
				     env#cgi_script_name));
  cgi # output # output_string (sprintf "PATH_INFO = %s<br>\n" 
				  (Netencoding.Html.encode_from_latin1 
				     env#cgi_path_info));
  cgi # output # output_string (sprintf "PATH_TRANSLATED = %s<br>\n" 
				  (Netencoding.Html.encode_from_latin1 
				     env#cgi_path_translated));
  cgi # output # output_string (sprintf "<a href='%s/tmp'>Append 'tmp'</a><br>\n"
				  env#cgi_script_name);
  cgi # output # output_string "<a href='?p=foo'>Append argument</a><br>\n";
  cgi # output # output_string "<form action='' method=post><input type=submit name=submit value='POST argument'></form>\n";
  cgi # output # output_string "</body></html>\n";
  cgi # output # commit_work();
;;


let fs_spec =
  { file_docroot = Sys.getenv "HOME";
    file_uri = "/";
    file_suffix_types = [ "txt", "text/plain";
			  "html", "text/html" ];
    file_default_type = "application/octet-stream";
    file_options = [ `Enable_gzip;
		     `Enable_listings (simple_listing ?hide:None);
		     `Enable_index_file ["index.html"]
		   ]
  }

let srv =
  host_distributor
    [ default_host ~pref_name:"localhost" ~pref_port:8765 (),
      uri_distributor
	[ "*", (options_service());
	  "/", (file_service fs_spec);
	  "/easteregg", (dynamic_service
			   { dyn_handler = hit_counter;
			     dyn_activation = std_activation `Std_activation_buffered;
			     dyn_uri = Some "/easteregg";
			     dyn_translator = file_translator fs_spec;
			     dyn_accept_all_conditionals = false
			   })
	]
    ]
;;


let serve_connection ues fd =
  let config =
    new Nethttpd_engine.modify_http_engine_config
      ~config_input_flow_control:true
      ~config_output_flow_control:true
      Nethttpd_engine.default_http_engine_config in
  let pconfig = 
    new Nethttpd_engine.buffering_engine_processing_config in

  Unix.set_nonblock fd;

  ignore(Nethttpd_engine.process_connection config pconfig fd ues srv)
;;

let rec accept ues srv_sock_acc =
  (* This function accepts the next connection using the [acc_engine]. After the   
   * connection has been accepted, it is served by [serve_connection], and the
   * next connection will be waited for (recursive call of [accept]). Because
   * [server_connection] returns immediately (it only sets the callbacks needed
   * for serving), the recursive call is also done immediately.
   *)
  let acc_engine = srv_sock_acc # accept() in
  Uq_engines.when_state ~is_done:(fun (fd,fd_spec) ->
                                if srv_sock_acc # multiple_connections then (
                                  serve_connection ues fd;
                                  accept ues srv_sock_acc
                                   ) else
                                  srv_sock_acc # shut_down())
                        ~is_error:(fun _ -> srv_sock_acc # shut_down())
                        acc_engine;
;;


let start () =

  let ues = Unixqueue.create_unix_event_system () in
  let opts = { Uq_engines.default_listen_options with
                 Uq_engines.lstn_backlog = 20;
                 Uq_engines.lstn_reuseaddr = true } in
  let lstn_engine =
    Uq_engines.listener
      (`Socket(`Sock_inet(Unix.SOCK_STREAM, Unix.inet_addr_any, 8765) ,opts)) ues in
  Uq_engines.when_state ~is_done:(accept ues) lstn_engine;

  printf "Listening on port 8765\n";
  flush stdout;
  (* Unixqueue.set_debug_mode true; *)
  Unixqueue.run ues
;;
let conf_debug() =
  (* Set the environment variable DEBUG to either:
       - a list of Netlog module names
       - the keyword "ALL" to output all messages
       - the keyword "LIST" to output a list of modules
     By setting DEBUG_WIN32 additional debugging for Win32 is enabled.
   *)
  let debug = try Sys.getenv "DEBUG" with Not_found -> "" in
  if debug = "ALL" then
    Netlog.Debug.enable_all()
  else if debug = "LIST" then (
    List.iter print_endline (Netlog.Debug.names());
    exit 0
  )
  else (
    let l = Netstring_str.split (Netstring_str.regexp "[ \t\r\n]+") debug in
    List.iter
      (fun m -> Netlog.Debug.enable_module m)
      l
  );
  if (try ignore(Sys.getenv "DEBUG_WIN32"); true with Not_found -> false) then
    Netsys_win32.Debug.debug_c_wrapper true
;;

Netsys_signal.init();
conf_debug();
start();;
