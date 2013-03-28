(* $Id: netplex_log.ml 1692 2012-02-05 18:44:00Z gerd $ *)

open Printf

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]

let level_weight = Netlog.level_weight
let level_of_string = Netlog.level_of_string
let string_of_level = Netlog.string_of_level
let level_names = Netlog.level_names


class type generic_config =
object
  method log_format : string
  method log_component : string
  method log_subchannel : string
  method log_max_level : level
end


let std_fmt =
  "[${timestamp}] [${component}] [${level}] ${message}"

class default_generic_config =
object
  method log_format = std_fmt
  method log_component = "*"
  method log_subchannel = "*"
  method log_max_level = (`Debug : level)
end


class type logger =
object
  method log_subch : component:string -> subchannel:string -> 
                     level:level -> message:string -> unit
  method log : component:string -> level:level -> message:string -> unit
  method reopen : unit -> unit
end


let format_message fmt component subchannel level message =
  let (sec,ns) =
    try Netsys_posix.clock_gettime Netsys_posix.CLOCK_REALTIME
    with Invalid_argument _ ->
      (Unix.gettimeofday(), 0) in
  let nd = lazy(Netdate.create ~localzone:true ~nanos:ns sec) in
  let b = Buffer.create 100 in
  Buffer.add_substitute
    b
    (fun var ->
       match var with
	 | "timestamp" ->
	     !Netlog.current_formatter (sec,ns)
	 | "timestamp:unix" ->
	     sprintf "%.0f" (Netdate.since_epoch (Lazy.force nd))
	 | "component" ->
	     component
	 | "subchannel" ->
	     subchannel
	 | "level" ->
	     string_of_level level
	 | "message" ->
	     message
	 | _ ->
	     if (String.length var >= 10 && 
		   String.sub var 0 10 = "timestamp:") then
	       let nd_fmt =
		 String.sub var 10 (String.length var - 10) in
	       Netdate.format nd_fmt (Lazy.force nd)
	     else
	       ""
    )
    fmt;
  Buffer.contents b


let ast_re = Netstring_str.regexp "[*]";;

let regexp_of_pattern s =
  let l = Netstring_str.split_delim ast_re s in
  Netstring_str.regexp
    (String.concat ".*" (List.map (fun u -> Netstring_str.quote u) l) ^ "$")


let mk_filter (gc:generic_config) =
  let comp_re = regexp_of_pattern gc#log_component in
  let subch_re = regexp_of_pattern gc#log_subchannel in

  fun component subchannel level ->
    level_weight level <= level_weight gc#log_max_level && (
      (Netstring_str.string_match comp_re component 0 <> None) &&
      (Netstring_str.string_match subch_re subchannel 0 <> None)
    )
  


class channel_logger (gc:generic_config) out : logger =
  let filter = mk_filter gc in
object(self)
  method log_subch ~component ~subchannel ~level ~message =
    if filter component subchannel level then (
      let fmt = gc#log_format in
      try
	fprintf out "%s\n%!"
	  (format_message fmt component subchannel level message)
      with
	| error ->
	    prerr_endline 
	      ("Netplex Catastrophic Error: Unable to write to log channel: " ^
		 Netexn.to_string error)
    )

  method log =
    self # log_subch ~subchannel:""

  method reopen() = ()

end

let channel_logger = new channel_logger (new default_generic_config)

let channel_logger_from_obj = new channel_logger


class extract_generic_config cf addr =
  let fmt = 
    try cf # string_param (cf # resolve_parameter addr "format")
    with Not_found -> std_fmt in
  let component =
    try cf # string_param (cf # resolve_parameter addr "component")
    with Not_found -> "*" in
  let subchannel =
    try cf # string_param (cf # resolve_parameter addr "subchannel")
    with Not_found -> "*" in
  let max_level_str =
    try  cf # string_param (cf # resolve_parameter addr "max_level")
    with Not_found -> "all" in
  let max_level =
    try
      if String.lowercase(max_level_str) = "all" then
	`Debug
      else
	(level_of_string max_level_str)
    with
      | _ ->
	  failwith ("In section " ^ cf # print addr ^ 
		      ": Bad max_level parameter value: " ^ max_level_str) in
object
  method log_format = fmt
  method log_component = component
  method log_subchannel = subchannel
  method log_max_level = max_level
end


let stderr_logger_factory =
object
  method name = "stderr"
  method create_logger cf addr _ = 
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr [ "type"; "format"; "component";
				    "subchannel"; "max_level"
				  ];
    let gc = new extract_generic_config cf addr in
    channel_logger_from_obj gc stderr
end


class file_logger (gc:generic_config) file : logger =
  let filter = mk_filter gc in
object(self)

  val mutable out = 
    open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 file

  initializer
    Netsys_posix.register_post_fork_handler
      (object
	 method name = "Netplex_log.file_logger"
	 method run() = self # post_fork()
       end
      )


  method log_subch ~component ~subchannel ~level ~message =
    if filter component subchannel level then (
      let fmt = gc#log_format in
      try
	fprintf out "%s\n%!"
	  (format_message fmt component subchannel level message)
      with
	| error ->
	    prerr_endline
	      ("Netplex Catastrophic Error: Unable to write to log file " ^ 
		 file ^ ": " ^ Netexn.to_string error)
    )


  method log =
    self # log_subch ~subchannel:""

  method reopen() =
    close_out out;
    try
      out <-
	open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 file
    with
      | error ->
	  prerr_endline
	    ("Netplex Catastrophic Error: Unable to reopen log file " ^ 
	       file ^ ": " ^ Netexn.to_string error)

  method private post_fork() =
    close_out out
end

let file_logger = new file_logger (new default_generic_config)

let file_logger_from_obj = new file_logger


let file_logger_factory =
object
  method name = "file"
  method create_logger cf addr _ =
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr [ "type"; "file"; "format"; "component";
				    "subchannel"; "max_level"
				  ];
    let fileaddr =
      try
	cf # resolve_parameter addr "file"
      with
	| Not_found ->
	    failwith ("File logger needs parameter 'file'") in
    let file = cf # string_param fileaddr in
    let gc = new extract_generic_config cf addr in
    file_logger_from_obj gc file
end


class type multi_file_config =
object
  inherit generic_config
  method log_directory : string
  method log_files :
    (string * string * [ level | `All ] * string * string) list
end


let no_duplicates l =
  let h = Hashtbl.create 10 in
  List.filter
    (fun p ->
       not(Hashtbl.mem h p) && (
         Hashtbl.add h p ();
         true))
    l
;;


class multi_file_logger (mfc : multi_file_config) : logger =
  let log_files =
    List.map
      (fun (comp_pat, subch_pat, level, file, fmt) ->
	 let comp_re = regexp_of_pattern comp_pat in
	 let subch_re = regexp_of_pattern subch_pat in
	 (comp_re, subch_re, level, file, fmt)
      )
      mfc#log_files in
  let filter = mk_filter (mfc :> generic_config) in
object(self)
  val channels = Hashtbl.create 10
    (* Maps files to channels *)

  initializer
    Netsys_posix.register_post_fork_handler
      (object
	 method name = "Netplex_log.multi_file_logger"
	 method run() = self # post_fork()
       end
      )

  method log_subch ~component ~subchannel ~level ~message =
    if filter component subchannel level then (
      let w = level_weight level in
      let files =
	List.map
	  (fun (_, _, _, file, fmt) -> (file, fmt))
	  (List.filter
	     (fun (comp_re, subch_re, level_pat, _, _) ->
		match Netstring_str.string_match comp_re component 0 with
		  | Some _ ->
		      ( match 
			  Netstring_str.string_match subch_re subchannel 0
			with
			  | Some _ ->
			      ( match level_pat with
				  | `All -> true
				  | #level as l ->
				      w <= level_weight l
			      )
			  | None -> false
		      )
		  | None -> false
	     )
	     log_files
	  ) in
      let files = no_duplicates files in
      List.iter
	(fun (file, fmt) ->
	   let full_path =
	     if file <> "/" && file.[0] = '/' then
	       file
	     else
	       Filename.concat mfc#log_directory file
	   in
	   try
	     let ch = 
	       try
		 Hashtbl.find channels full_path
	       with
		 | Not_found ->
		     let ch =
	               open_out_gen 
			 [ Open_wronly; Open_append; Open_creat ] 0o666 full_path in
		     Hashtbl.add channels full_path ch;
		     ch
	     in
	     fprintf ch "%s\n%!"
	       (format_message fmt component subchannel level message)
	   with
	     | error ->
		 prerr_endline ("Netplex Catastrophic Error: Unable to write to log file " ^ full_path ^ ": " ^ Netexn.to_string error)

	)
	files
    )

  method log =
    self # log_subch ~subchannel:""

  method reopen() =
    Hashtbl.iter
      (fun name ch ->
	 close_out ch)
      channels;
    Hashtbl.clear channels

  method private post_fork() =
    self # reopen()

end


let multi_file_logger = new multi_file_logger

let multi_file_logger_factory =
object
  method name = "multi_file"
  method create_logger cf addr _ =
    cf # restrict_subsections addr [ "file" ];
    cf # restrict_parameters addr [ "type"; "format"; "component";
				    "subchannel"; "max_level";
				    "directory" ];
    let diraddr =
      try
	cf # resolve_parameter addr "directory"
      with
	| Not_found ->
	    failwith ("Multi-file logger needs parameter 'directory'") in
    let dir = cf # string_param diraddr in
    let gc = new extract_generic_config cf addr in
    let log_files =
      List.map
	(fun addr ->
	   cf # restrict_subsections addr [];
	   cf # restrict_parameters addr
	     [ "component"; "subchannel"; "max_level"; "file"; "format" ];
	   let component =
	     try 
	       cf # string_param (cf # resolve_parameter addr "component")
	     with
	       | Not_found -> "*" in
	   let subchannel =
	     try 
	       cf # string_param (cf # resolve_parameter addr "subchannel")
	     with
	       | Not_found -> "*" in
	   let max_level_str =
	     try
	       cf # string_param (cf # resolve_parameter addr "max_level")
	     with
	       | Not_found -> "all" in
	   let max_level =
	     try
	       if String.lowercase(max_level_str) = "all" then
		 `All
	       else
		 (level_of_string max_level_str :> [level | `All] )
	     with
	       | _ ->
		   failwith ("In section " ^ cf # print addr ^ 
			       ": Bad max_level parameter value: " ^ 
			       max_level_str) in
	   let fmt =
	     try cf # string_param (cf # resolve_parameter addr "format")
	     with Not_found -> gc#log_format in
	   let file =
	     try
	       cf # string_param (cf # resolve_parameter addr "file")
	     with
	       | Not_found ->
		   failwith ("In section " ^ cf # print addr ^ ": Parameter 'file' is missing") in
	   (component, subchannel, max_level, file, fmt)
	)
	(cf # resolve_section addr "file") in

    let config =
      ( object
	  method log_format = gc#log_format
	  method log_component = gc#log_component
	  method log_subchannel = gc#log_subchannel
	  method log_max_level = gc#log_max_level
	  method log_directory = dir
	  method log_files = log_files
	end
      ) in

    multi_file_logger config
end


class type syslog_config =
object
  inherit generic_config
  method log_identifier : string
  method log_facility : Netsys_posix.syslog_facility
end


let syslog_logger (sc:syslog_config) =
  let filter = mk_filter (sc :> generic_config) in
  let prepend = 
    if sc#log_identifier = "" then "" else sc#log_identifier ^ ": " in
object(self)
  method log_subch ~component ~subchannel ~level ~message =
    if filter component subchannel level then (
      try
	Netsys_posix.syslog
	  sc#log_facility
	  level
	  (prepend ^ message)
      with
	| error ->
	    prerr_endline
	      ("Netplex Catastrophic Error: Unable to write to syslog")
    )
  method log =
    self # log_subch ~subchannel:""

  method reopen() =
    ()
end


let facilities =
  [ "authpriv", `Authpriv;
    "cron", `Cron;
    "daemon", `Daemon;
    "ftp", `Ftp;
    "kern", `Kern;
    "local0", `Local0;
    "local1", `Local1;
    "local2", `Local2;
    "local3", `Local3;
    "local4", `Local4;
    "local5", `Local5;
    "local6", `Local6;
    "local7", `Local7;
    "lpr", `Lpr;
    "mail", `Mail;
    "news", `News;
    "syslog", `Syslog;
    "user", `User;
    "uucp", `Uucp;
    "default", `Default;
  ]



let syslog_logger_factory =
object
  method name = "syslog"
  method create_logger cf addr _ =
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr [ "type"; "format"; "component";
				    "subchannel"; "max_level";
				    "identifier"; "facility"
				  ];
    let identifier =
      try cf # string_param (cf # resolve_parameter addr "identifier")
      with Not_found -> "" in
    let facility_str =
      try cf # string_param (cf # resolve_parameter addr "facility")
      with Not_found -> "default" in
    let facility =
      try List.assoc (String.lowercase facility_str) facilities 
      with Not_found ->
	failwith "Bad 'facility' parameter in syslog config" in
    let sc =
      ( object
	  inherit extract_generic_config cf addr 
	  method log_identifier = identifier
	  method log_facility = facility
	end
      ) in
    syslog_logger sc
end



let logger_factories =
  [ file_logger_factory;
    multi_file_logger_factory;
    stderr_logger_factory;
    syslog_logger_factory
  ]
