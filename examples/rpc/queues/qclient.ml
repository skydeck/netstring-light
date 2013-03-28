(* $Id: qclient.ml 286 2006-04-29 16:21:42Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

open Rtypes
open Rpc
open Rpc_client
open Printf

module A = Queues_aux ;;
module C1 = Queues_clnt.QUEUESPROG.QUEUESVERS1 ;;

type error =
    [ `not_found
      | `picked
      | `exists
      | `not_picked
      | `chunks_too_large
      | `timeout
      | `empty
      | `queue_deleted
      | `full
      | `bad_name
      | `bad_value
      | `inactive
      | `permission_denied
      | `sys_error
    ]

exception Error of error

let getenv ?(default="") n =
  try Sys.getenv n with Not_found -> default ;;

let default_host = getenv ~default:"localhost" "QCLIENT_HOST" ;;

let pdate seconds =
  let fsecs = Int64.to_float seconds in
  let t = Unix.localtime fsecs in
  sprintf "%4d-%02d-%02d %02d:%02d"
    (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1)
    (t.Unix.tm_mday)
    (t.Unix.tm_hour)
    (t.Unix.tm_min)
;;


let getuser() =
  try
    let pw = Unix.getpwuid (Unix.getuid()) in
    pw.Unix.pw_name
  with
      Not_found -> "UNKNOWN"
;;


let pluggable_auth_module =
  ref ( "<None>",
	(fun _ -> 
	   ( failwith "No auth module linked." : Rpc_client.t ) ) ) ;;


let create_client host =
  let (name, create_client) = !pluggable_auth_module in
  create_client host
;;


let parse_name_is_value s =
  try
    let l = String.length s in
    let k = String.index s '=' in
    (String.sub s 0 k), (String.sub s (k+1) (l-k-1))
  with
      Not_found -> failwith ("Cannot parse: " ^ s)
;;


exception Parse_error of string ;;

let parse_shell_file filename =
  (* Parses a file with settings
   *   NAME=VALUE
   * with shell syntax. The parser understands:
   * - comments
   * - backslashes
   * - single quotes
   * - double quotes
   * The parser does not understand:
   * - Any kind of expansion
   * The functions returns only settings for variables that begin with
   * "PROP_", but without this prefix. For example,
   *   PROP_A=1
   *   B=2
   *   PROP_C=2
   * is parsed as [ "A", "1"; "C"; "2" ].
   *)
  let rec skip_space line k =
    if k >= String.length line then
      k
    else
      match line.[k] with
	  ' ' | '\t' | '\r' -> skip_space line (k+1)
	| '#' -> String.length line
	| _   -> k
  in
  let rec scan_name b line k =
    if k >= String.length line then
      failwith "Syntax error"
    else
      let c = line.[k] in
      match c with
	  'A'..'Z'|'a'..'z'|'_' ->
            Buffer.add_char b c;
	    scan_name b line (k+1)
	| '0'..'9' ->
	    if Buffer.length b = 0 then failwith "Syntax error";
            Buffer.add_char b c;
	    scan_name b line (k+1)
	| '=' ->
	    if Buffer.length b = 0 then failwith "Syntax error";
	    k+1
	| _ ->
	    failwith "Syntax error"
  in
  let rec scan_squote_value b line k =
    if k >= String.length line then
      failwith "Syntax error"
    else
      match line.[k] with
	  '\'' ->
	    scan_value b line (k+1)
	| c ->
	    Buffer.add_char b c;
	    scan_squote_value b line (k+1)
  and scan_dquote_value b line k =
    if k >= String.length line then
      failwith "Syntax error"
    else
      match line.[k] with
	  '"' ->
	    scan_value b line (k+1)
	| '\\' ->
	    if k+1 >= String.length line then failwith "Syntax error";
	    Buffer.add_char b line.[k+1];
	    scan_dquote_value b line (k+2)
	| '$' ->
	    failwith "$ expansion not supported";
	| '`' ->
	    failwith "backtick expansion not supported"
	| c ->
	    Buffer.add_char b c;
	    scan_dquote_value b line (k+1)
  and scan_value b line k =
    if k >= String.length line then
      k
    else
      match line.[k] with
	  '\'' ->
	    scan_squote_value b line (k+1)
	| '"' ->
	    scan_dquote_value b line (k+1)
	| ('|' | '&' | ';' | '(' | ')' | '<' | '>' as c) ->
	    failwith ("special character not supported: " ^ String.make 1 c)
	| ' ' | '\t' | '\r' ->
	    k
	| c ->
	    Buffer.add_char b c;
	    scan_value b line (k+1)
  in
  let rec next_settings f linenumber =
    try
      let line = input_line f in
      let j = skip_space line 0 in
      if j = String.length line then
	next_settings f (linenumber+1)
      else begin
	(* Scan now: VARNAME= *)
	let varname = Buffer.create 20 in
	let k = scan_name varname line j in
	(* Scan now: VALUE *)
	let varval = Buffer.create 50 in
	let k' = scan_value varval line k in
	(* The rest is white space: *)
	let k'' = skip_space line k' in
	if k'' <> String.length line then failwith "Syntax error";
	let vname = Buffer.contents varname in
	if String.length vname >= 6 && String.sub vname 0 5 = "PROP_" then
	  (String.sub vname 5 (String.length vname - 5),
	   Buffer.contents varval) ::
	  next_settings f (linenumber+1)
	else
	  next_settings f (linenumber+1)
      end
    with
	End_of_file -> []
      | Failure s ->
	  raise (Parse_error ("In file " ^ filename ^ ", line " ^
			      string_of_int linenumber ^ ": " ^ s))
  in
  let f = open_in filename in
  try
    let l = next_settings f 1 in
    close_in f;
    l
  with
      err -> close_in f; raise err
;;


let get_result r =
  match r with
      `successful x -> x
    | #error as e -> raise (Error e)
;;


let check_result r =
  match r with
      `successful -> ()
    | #error as e -> raise (Error e)
;;


let cmd_queues() =
  let host = ref default_host in
  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
      ]
      (fun s -> raise (Arg.Bad "Unexpected argument"))
      "qclient queues [ options ]: list the installed queues. Options:";
  let client = create_client !host in
  let l = get_result (C1.list_queues client ()) in
  printf "%-16s  %2s  %6s  %6s  %6s  %-16s  %-16s\n"
    "QUEUE" "ST" "LENGTH" "PICKED" "ADDING" "CREATED" "MODIFIED";
  Array.iter
    (fun q ->
       let status =
	 if q.A.qparams.A.qactive then begin
	   (if q.A.qparams.A.qaccepting then "+" else " ") ^
	   (if q.A.qparams.A.qdelivering then "-" else " ")
	 end
	 else
	   "**"
       in
       printf "%-16s  %2s  %6s  %6s  %6s  %-16s  %-16s\n"
	      (q.A.qname)
	      status
	      (Int32.to_string q.A.qlength)
	      (Int32.to_string q.A.qpicked)
	      (Int32.to_string q.A.quploads)
	      (pdate q.A.qcreation)
              (pdate q.A.qmodified);
    )
    l;
  flush stdout;
  Rpc_client.shut_down client
;;


let cmd_create() =
  let host = ref default_host in
  let qnames = ref [] in
  let activate = ref false in
  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
	"-activate", Arg.Set activate,
	          " Activate the queues now";
      ]
      (fun s -> qnames := !qnames @ [s])
      "qclient create [ options ] qname ...: create these queues. Options:";
  let client = create_client !host in
  List.iter
    (fun qname ->
       check_result (C1.create_queue client qname);
       printf "%s created.\n" qname;
       flush stdout;
       if !activate then begin
	 let q = get_result(C1.get_queue client qname) in
	 q.A.qparams.A.qactive <- true;
	 check_result (C1.set_queue client (qname,q.A.qparams));
	 printf "%s activated.\n" qname;
	 flush stdout;
       end;
    )
    !qnames;
  Rpc_client.shut_down client
;;



let cmd_destroy() =
  let host = ref default_host in
  let qnames = ref [] in
  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
      ]
      (fun s -> qnames := !qnames @ [s])
      "qclient destroy [ options ] qname ...: delete these queues. Options:";
  let client = create_client !host in
  List.iter
    (fun qname ->
       let q = get_result (C1.get_queue client qname) in
       check_result (C1.delete_queue client qname);
    )
    !qnames;
  Rpc_client.shut_down client
;;


let cmd_list() =
  let host = ref default_host in
  let list_properties = ref false in
  let qnames = ref [] in
  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
	"-properties", Arg.Set list_properties,
	            "  Include the property list of each entry";
      ]
      (fun s -> qnames := !qnames @ [s])
      "qclient list [ options ] qname ...: list these queues. Options:";
  let client = create_client !host in
  let first_queue = ref true in
  List.iter
    (fun qname ->
       let q = get_result (C1.get_queue client qname) in
       let l = get_result (C1.list_queue_entries client q.A.qid) in
       if not !first_queue then print_newline();
       first_queue := false;
       if l = [| |] then
	 printf "The queue '%s' is empty.\n" qname
       else begin
	 printf "%4s  %-20s  %9s  %-16s\n"
	   "RANK"
	   "MEMBER"
	   "SIZE"
	   "ADDED";
	 Array.iteri
	   (fun n e ->
	      printf "%4d  %-20s  %9s  %16s\n"
		n
		(qname ^ "-" ^ e.A.eid)
		(Int64.to_string e.A.esize)
		(pdate e.A.ecreation);
	      if !list_properties then begin
		Array.iter
		  (fun prop ->
		     printf "%6s  %s=%s\n"
		       "" prop.A.pname prop.A.pvalue)
		  e.A.eprops;
	      end;
	   )
	   l;
       end;
       flush stdout;
    )
    !qnames;
  Rpc_client.shut_down client
;;


let cmd_status() =
  let host = ref default_host in
  let inc_name = ref false in
  let inc_date_created = ref false in
  let inc_date_modified = ref false in
  let inc_length = ref false in
  let inc_picked = ref false in
  let inc_adding = ref false in
  let inc_maxlen = ref false in
  let inc_active = ref false in
  let inc_owner = ref false in
  let only_values = ref false in
  let all = ref false in
  let qnames = ref [] in
  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
	"-name", Arg.Set inc_name,
	      " Output the name of the queue";
	"-date-created", Arg.Set inc_date_created,
	              " Output creation date";
	"-date-modified", Arg.Set inc_date_modified,
	               " Output date of last modification";
	"-owner", Arg.Set inc_owner,
	       " Output the owner of the queue (netname)";
	"-length", Arg.Set inc_length,
	        " Output queue length";
	"-picked", Arg.Set inc_picked,
	        " Output number of picked entries";
	"-adding", Arg.Set inc_adding,
	        " Output number of entries currently being added";
	"-maxlen", Arg.Set inc_maxlen,
	        " Output the maximum length of the queue";
	"-active", Arg.Set inc_active,
	        " Output whether the queue is active";
	"-only-values", Arg.Set only_values,
	             " Output only values (no labels)";
	"-all", Arg.Set all,
	     " Output all status variables";
      ]
      (fun s -> qnames := !qnames @ [s])
      "qclient status [ options ] qname ...: output queue status. Options:";
  let client = create_client !host in
  List.iter
    (fun qname ->
       let q = get_result (C1.get_queue client qname) in
       let status =
	 [ !inc_name, "Queue", q.A.qname;
	   !inc_date_created, "Date created", (pdate q.A.qcreation);
	   !inc_date_modified, "Date modified", (pdate q.A.qmodified);
	   !inc_owner, "Owner", q.A.qowner;
	   !inc_length, "Length", (Int32.to_string q.A.qlength);
	   !inc_picked, "Picked", (Int32.to_string q.A.qpicked);
	   !inc_adding, "Adding", (Int32.to_string q.A.quploads);
	   !inc_maxlen, "Maximum length", Int32.to_string q.A.qparams.A.qmaxlen;
	   !inc_active, "Active",
	     (if q.A.qparams.A.qactive then begin
		"yes (" ^
		(if q.A.qparams.A.qaccepting
		 then "accepting"
		 else "stopped") ^ "," ^
		(if q.A.qparams.A.qdelivering
		 then "delivering"
		 else "stopped") ^ ")"
	      end
	      else "no");
	 ]
       in
       List.iter
	 (fun (p,label,value) ->
	    if !all || p then begin
	      if not !only_values then printf "%s: " label;
	      print_endline value;
	      flush stdout
	    end
	 )
	 status
    )
    !qnames;
  Rpc_client.shut_down client
;;


let cmd_set() =
  let host = ref default_host in
  let qnames = ref [] in
  let active = ref None in
  let accepting = ref None in
  let delivering = ref None in
  let maxlen = ref None in

  let bool f =
    Arg.String
      (fun s ->
	 match s with
	     "true"|"yes"|"y"|"1" -> f true
	   | "false"|"no"|"n"|"0" -> f false
	   | _ -> raise(Arg.Bad "Option must be true or false")
      )
  in
  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
	"-active", bool (fun b -> active := Some b),
	        "(true|false)  Make the queue active/inactive";
	"-accepting", bool (fun b -> accepting := Some b),
	           "(true|false)  Whether the queue accepts new entries";
	"-delivering", bool (fun b -> delivering := Some b),
	            "(true|false)  Whether the queue delivers entries";
	"-maxlen", Arg.Int (fun n -> maxlen := Some n),
	        "<n>  Set the maximum length (-1 = infinite)";
      ]
      (fun s -> qnames := !qnames @ [s])
      "qclient set [ options ] queue ...: Set the params of the queues. Options:";
  let client = create_client !host in
  List.iter
    (fun qname ->
       let q = get_result (C1.get_queue client qname) in
       let p = q.A.qparams in
       (match !active with
	    None -> ()
	  | Some b -> p.A.qactive <- b
       );
       (match !accepting with
	    None -> ()
	  | Some b -> p.A.qaccepting <- b
       );
       (match !delivering with
	    None -> ()
	  | Some b -> p.A.qdelivering <- b
       );
       (match !maxlen with
	    None -> ()
	  | Some n -> p.A.qmaxlen <- Int32.of_int n
       );
       check_result(C1.set_queue client (qname,p));
       printf "Parameters of queue `%s' set.\n" qname;
       flush stdout
    )
    !qnames;
  Rpc_client.shut_down client
;;



let cmd_add() =
  let host = ref default_host in
  let queue = ref "" in
  let props = ref [] in
  let no_std_properties = ref false in
  let files = ref [] in
  let wait = ref (-1) in

  let add_property s =
    let n,v = parse_name_is_value s in
    props := List.remove_assoc n !props @ [ n,v ]
  in

  let load_properties file =
    let nv_list = parse_shell_file file in
    let names = List.map fst nv_list in
    let props' = List.filter (fun (n,v) -> not(List.mem n names)) !props in
    props := props' @ nv_list
  in

  let add_std_properties props file =
    let cprops = ref props in
    let set_prop n v =
      cprops := List.remove_assoc n !cprops @ [ n,v ]
    in
    let add_prop n v =
      if not (List.mem_assoc n !cprops) then
	cprops := !cprops @ [ n,v ]
    in
    set_prop "SP_FILENAME" (Filename.basename file);
    set_prop "SP_SYS_USER" (getuser());
    set_prop "SP_SYS_HOST" (Unix.gethostname());
    let hops =
      try int_of_string(List.assoc "SP_HOPS" !cprops) with _ -> 0 in
    set_prop "SP_HOPS" (string_of_int (hops+1));
    add_prop "SP_FIRST_SYS_USER" (getuser());
    add_prop "SP_FIRST_SYS_HOST" (Unix.gethostname());
    add_prop "SP_FIRST_DATE" (string_of_float (Unix.time()));
    !cprops
  in

  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
	"-queue", Arg.String (fun s -> queue := s),
	       "<qname>  Add the entry to this queue";
	"-property", Arg.String add_property,
	          "<p>=<v>  Set the property <p> to <v>";
	"-property-file", Arg.String load_properties,
	               "<file>  Load the properties from this file";
	"-no-std-properties", Arg.Set no_std_properties,
	                   "  Do not set/update the standard properties";
	"-wait", Arg.Int(fun n -> wait := n),
	      "<seconds>  Wait this number of seconds for the entry (-1 = endless)";
      ]
      (fun s -> files := !files @ [s])
      "qclient add [ options ] file ...: add these files to a queue. Options:";
  if !queue = "" then
    failwith "The option -queue is mandatory!";
  let client = create_client !host in
  List.iter
    (fun file ->
       let f = open_in file in
       let q = get_result (C1.get_queue client !queue) in
       let qid = q.A.qid in
       let plist =
	 if !no_std_properties then
	   !props
	 else
	   add_std_properties !props file in
       let parray =
	 Array.of_list
	   (List.map (fun (n,v) -> { A.pname = n; A.pvalue = v }) plist) in
       let w = Int32.of_int !wait in
       let handle = get_result
		      (C1.upload_entry client (qid,parray,w)) in
       let len = 8192 in
       let buf = String.create len in
       let n = ref 1 in
       let s = ref Int64.zero in
       while !n > 0 do
	 n := input f buf 0 len;
	 let d = if !n = len then buf else String.sub buf 0 !n in
	 let chunk = { A.serial = !s; A.last = !n=0; A.data = d } in
	 check_result (C1.upload_chunk client (handle, chunk));
	 s := Int64.succ !s
       done;
       printf "File '%s' added.\n" file;
       flush stdout;
       close_in f;
    )
    !files;
  Rpc_client.shut_down client
;;

(* Standard properties:
 * SP_FILENAME
 * SP_SYS_USER
 * SP_SYS_HOST
 * SP_HOPS
 * SP_FIRST_SYS_USER
 * SP_FIRST_SYS_HOST
 * SP_FIRST_DATE
 *)


let cmd_pop() =
  let host = ref default_host in
  let pop_file = ref "" in
  let pop_properties = ref "" in
  let wait = ref (-1) in
  let peek = ref false in
  let qname = ref "" in

  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
	"-get-file", Arg.String (fun s -> pop_file := s),
	          "<name>  Download the entry and store it into this file";
	"-get-properties", Arg.String (fun s -> pop_properties := s),
	                "<name>  Download the properties and put them into this file";
	"-wait", Arg.Int (fun k -> wait := k),
	      "<seconds>  Wait this number of seconds for the entry (-1 = endless)";
	"-peek", Arg.Set peek,
	      " Do not remove the entry from the queue";
      ]
      (fun s ->
	 if !qname <> "" then raise(Arg.Bad "Two many arguments");
	 qname := s
      )
      "qclient pop [ options ] queue: Pop the next entry from the queue. Options:";

  if !qname = "" then failwith "The queue argument is mandatory!";

  let client = create_client !host in
  let q = get_result (C1.get_queue client !qname) in
  let qid = q.A.qid in
  let e = get_result (C1.pick_queue_entry client (qid, Int32.of_int !wait)) in
  if !pop_properties <> "" then begin
    let f = open_out !pop_properties in
    Array.iter
      (fun p ->
	 let name = p.A.pname in
	 let value = p.A.pvalue in
	 output_string f ("PROP_" ^ name);
	 output_string f "='";
	 for i = 0 to String.length value - 1 do
	   match value.[i] with
	       '\'' -> output_string f "'\\''";
	     | c    -> output_char f c
	 done;
	 output_string f "'\n";
      )
      e.A.eprops;
    close_out f
  end;
  if !pop_file <> "" then begin
    let chunksize = Int32.of_string "65536" in
    let handle = get_result (C1.download_entry client (qid,e.A.eid,chunksize)) in
    let f = open_out !pop_file in
    let serial = ref Int64.zero in
    let last = ref false in
    while not !last do
      let chunk = get_result (C1.download_chunk client handle) in
      if chunk.A.serial <> !serial then failwith "Download error (bad serial number)";
      output_string f chunk.A.data;
      serial := Int64.succ !serial;
      last := chunk.A.last
    done;
    close_out f
  end;
  if not !peek then begin
    check_result (C1.remove_picked_queue_entry client (qid,e.A.eid));
    printf "%s-%s popped.\n" !qname e.A.eid;
    flush stdout;
  end
  else begin
    check_result (C1.return_picked_queue_entry client (qid,e.A.eid));
    printf "%s-%s peeked.\n" !qname e.A.eid;
    flush stdout;
  end;
  Rpc_client.shut_down client
;;


let cmd_cancel() =
  let host = ref default_host in
  let queue = ref "" in
  let entries = ref [] in
  let all = ref false in

  Arg.parse
      [ "-host", Arg.String (fun s -> host := s),
	      "<name>  Contact the queue server on this host";
	"-queue", Arg.String (fun s -> queue := s),
	       "<qname>  Remove the entries from this queue";
	"-all", Arg.Set all,
	     " Remove all entries (empty the queue)";
      ]
      (fun s -> entries := !entries @ [s])
      "qclient cancel [ options ] entry ...: remove these entries. Options:";
  if !queue = "" then
    failwith "The option -queue is mandatory!";
  let client = create_client !host in
  let q = get_result (C1.get_queue client !queue) in
  let qid = q.A.qid in
  if !all then begin
    let l = get_result (C1.list_queue_entries client qid) in
    entries := List.map (fun e -> e.A.eid) (Array.to_list l);
  end;
  List.iter
    (fun entry ->
       check_result (C1.remove_queue_entry client (qid, entry));
       printf "%s canceled.\n" entry;
       flush stdout
    )
    !entries;
  Rpc_client.shut_down client
;;



let start() =
  let (auth_name,_) = !pluggable_auth_module in
  let usage() =
    prerr_endline "qclient - access remote queues. Usage:";
    prerr_endline "qclient queues   [ -help | options ]";
    prerr_endline "qclient create   [ -help | options ]";
    prerr_endline "qclient destroy  [ -help | options ]";
    prerr_endline "qclient status   [ -help | options ]";
    prerr_endline "qclient set      [ -help | options ]";
    prerr_endline "qclient list     [ -help | options ]";
    prerr_endline "qclient add      [ -help | options ]";
    prerr_endline "qclient pop      [ -help | options ]";
    prerr_endline "qclient cancel   [ -help | options ]";
    prerr_endline "";
    prerr_endline ("This qclient is linked with the auth module: " ^ auth_name);
    exit 2;
  in
  incr Arg.current;
  if Array.length Sys.argv <= !Arg.current then usage();
  let cmd = Sys.argv.(!Arg.current) in
  match cmd with
      "queues"  -> cmd_queues()
    | "create"  -> cmd_create()
    | "destroy" -> cmd_destroy()
    | "list"    -> cmd_list()
    | "status"  -> cmd_status()
    | "set"     -> cmd_set()
    | "add"     -> cmd_add()
    | "pop"     -> cmd_pop()
    | "cancel"  -> cmd_cancel()
    | _ -> usage()
;;


let main() =
  try
    start();
    exit 0
  with
      Error e ->
	let code, msg =
	  match e with
	      `not_found        -> 10, "not found"
	    | `picked           -> 11, "picked"
	    | `exists           -> 12, "exists"
	    | `not_picked       -> 13, "not picked"
	    | `chunks_too_large -> 14, "chunks too large"
	    | `timeout          -> 15, "timeout"
	    | `empty            -> 16, "empty"
	    | `queue_deleted    -> 17, "queue deleted"
	    | `full             -> 18, "full"
	    | `bad_name         -> 19, "bad name"
	    | `bad_value        -> 20, "bad value"
	    | `inactive         -> 21, "inactive"
	    | `permission_denied -> 22, "permission denied"
	    | `sys_error        -> 23, "system error"
	in
	prerr_endline ("Server response: " ^ msg);
	exit code
    | Rpc.Rpc_server x ->
	prerr_endline
	  ("RPC error: " ^
	     match x with
		 Unavailable_program      -> "Unavailable program"
	       | Unavailable_version(_,_) -> "Unavailable version"
	       | Unavailable_procedure    -> "Unavailable procedure"
	       | Garbage                  -> "Garbage (unknown message format)"
	       | System_err               -> "System error"
	       | Rpc_mismatch(_,_)        -> "Unsupported RPC type"
	       | Auth_bad_cred            -> "Bad credentials"
	       | Auth_rejected_cred       -> "Rejected credentials"
	       | Auth_bad_verf            -> "Bad verifier"
	       | Auth_rejected_verf       -> "Rejected verifier"
	       | Auth_too_weak            -> "Authentication too weak"
	       | Auth_invalid_resp        -> "Invalid response"
	       | Auth_failed              -> "Authentication failed"
	  );
	exit 1
    | Failure s ->
	prerr_endline ("Error: " ^ s);
	exit 1
;;
