(* Serves $HOME as docroot. This is a simple-minded multithreaded variant *)

open Nethttpd_types
open Nethttpd_services
open Nethttpd_reactor
open Printf

let counter_lock = Mutex.create()
let counter = ref 0

let hit_counter env cgi =
  Mutex.lock counter_lock;
  let cur_counter = !counter in
  incr counter;
  Mutex.unlock counter_lock;
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
		     `Enable_listings (simple_listing ?hide:None)
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


let start () =
  let config = Nethttpd_reactor.default_http_reactor_config in
  let master_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt master_sock Unix.SO_REUSEADDR true;
  Unix.bind master_sock (Unix.ADDR_INET(Unix.inet_addr_any, 8765));
  Unix.listen master_sock 100;
  printf "Listening on port 8765\n";
  flush stdout;

  while true do
    try
      let conn_sock, _ = Unix.accept master_sock in
      Unix.set_nonblock conn_sock;
      let _ =
	Thread.create
	  (process_connection config conn_sock)
	  srv
      in
      ()
    with
        Unix.Unix_error(Unix.EINTR,_,_) -> ()  (* ignore *)
  done
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
