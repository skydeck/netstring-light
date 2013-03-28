(* $Id: shell.mli 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Calls external programs, creates pipelines, etc. (simplified interface) *)

(** {b Signal handlers:} When you call the function [call], signal handlers
 * are automatically installed by {!Shell_sys.install_job_handlers}, unless
 * this installation has already been performed. You can configure these
 * handlers by {!Shell_sys.configure_job_handlers}. The handlers remain
 * in effect even after [call] returns.
 *
 * Note that this has a global side effect on the whole process, because
 * there is only one set of signal handlers.
 *)

(* ******************************************************************** *)
(* **                 Calling commands and pipelines                 ** *)
(* ******************************************************************** *)

(** {1 Calling commands and pipelines} *)

(** The following functions are simplified versions of the
 * [Shell_sys.job] abstraction.
 *)

exception Subprocess_error of (string * Unix.process_status) list;;
  (** The string contains the called commands in a readable representation.
   * The list enumerates the return codes of the processes that have
   * been started for the commands.
   *)

type producer
  (** A producer generates data sent to a called process *)  

type consumer
  (** A consumer receives data from a called process *)

type assignment
  (** An assignment redirects file descriptors while calling a process *)

val command :
      ?cmdname:string ->                   (* default: derived from filename *)
      ?arguments:(string array) ->         (* default: empty *)
      ?chdir:string ->                     (* default: current working dir *)
      ?environment:Shell_sys.environment ->  (* default: current environment *)
      ?descriptors:(Unix.file_descr list) -> 
                                           (* default: stdin, stdout, stderr *)
      ?assignments:(assignment list) ->    
                                           (* default: empty *)
      string ->
        Shell_sys.command
  (** Creates a command descriptor, to be used in [call]. The anonymous
   * string argument is the name of the executable to invoke. If the name
   * contains a '/', it is simply interpreted as the filename of the
   * executable. Otherwise the command is searched in the current PATH.
   *
   * @param cmdname The name of the command passed in [argv[0]]. By
   *   default, this argument is derived from the name of the executable.
   * @param arguments The arguments of the command (starting with the
   *   first real argument, skipping [cmdname]). By default [ [] ].
   * @param chdir Before the command is executed it is changed to
   *   this directory.
   * @param environment The environment of the command. By default, the
   *   current environment
   * @param descriptors The list of file descriptors to share with the
   *   current process; all other file descriptors will be closed.
   *   By default, [ [stdin; stdout; stderr] ].
   * @param assignments The list of descriptor assignments. The assignments
   *   are applied one after the other. By default empty.
   *)

  (* Incompatible change to shell-0.2: the name of the command is no
   * longer passed by a named argument.
   *)

val cmd :
      ?cmdname:string ->                   (* default: derived from filename *)
      ?chdir:string ->                     (* default: current working dir *)
      ?environment:Shell_sys.environment ->  (* default: current environment *)
      ?descriptors:(Unix.file_descr list) -> 
                                           (* default: stdin, stdout, stderr *)
      ?assignments:(assignment list) ->    
                                           (* default: empty *)
      string ->
      string list ->
        Shell_sys.command
  (** The same as [command] but with a slightly different interface: Use
   * {[ cmd "ls" [ "/dir/file" ] ]}
   * instead of
   * {[ command ~arguments:[|"/dir/file"|] "ls" ]}
   *
   * The named arguments have the same meanings as in [command].
   *)

  (* Incompatible change to shell-0.2: the name and args of the command are no
   * longer passed by named arguments.
   *)


val call :
      ?ignore_error_code:bool ->              (* default: false *)
      ?mode:Shell_sys.group_mode ->           (* default: Same_as_caller *)
      ?stdin:producer ->
      ?stdout:consumer ->
      ?stderr:consumer ->
      Shell_sys.command list ->
	unit
  (** Starts the pipeline represented by the list of commands; i.e.
   * if [ [c1;c2;...;cN] ] is passed, this corresponds to the pipeline
   * [ c1 | c2 | ... | cN ] (in shell notation).
   *
   * The function returns normally if all processes can be started and
   * terminate regularly with exit code 0. If a process terminates with
   * some other exit code, and [ignore_error_code] is set, the function
   * returns normally, too. The latter does not apply if a process terminates
   * because of a signal (which triggers always the exception
   * [Subprocess_error]).
   *
   * If a process terminates with an exit code other than 0 and 
   * [ignore_error_code] is not set (the default), or if a process is
   * terminated because of a signal, the exception [Subprocess_error]
   * will be raised. For every command the process result is included
   * in the exception argument.
   *
   * If a process cannot be started (e.g. because of insufficient 
   * resources), the function will try to shut down the already running
   * part of the pipeline by sending SIGTERM to these processes.
   * It is not checked whether the processes actually terminate (no
   * "wait" for them); an appropriate exception will be raised.
   * In the case that it is not even possible to perform these cleanup
   * actions, the exception [Shell_sys.Fatal_error] will be raised.
   * 
   * When the function raises an exception other than [Subprocess_error],
   * a serious error condition has happened, and it is recommended
   * to exit the program as soon as possible.
   *
   * @param ignore_error_code If [true], exit codes other than 0 of the
   *   subprocesses are ignored. This does not apply to signals, however.
   *   By default [false].
   * @param mode See {!Shell_sys.run_job} for a detailed description
   *   of this parameter. By default [Same_as_caller].
   * @param stdin If present, the first process of the pipeline reads
   *   input data from this procucer. By default, there is no such 
   *   producer.
   * @param stdout If present, the last process of the pipeline writes
   *   output data to this consumer. By default, there is no such
   *   consumer.
   * @param stderr If present, all processes of the pipeline write
   *   their error messages to this consumer. By default, there is no
   *   such consumer.
   *)

val setup_job : 
      ?stdin:producer ->
      ?stdout:consumer ->
      ?stderr:consumer ->
      Shell_sys.command list ->
	(Shell_sys.job * Unix.file_descr list)
  (** Creates a job like [call], but does not execute it. In addition to
   * the job, the file descriptors are returned that must be closed 
   * when the job is done.
   *)

val postprocess_job :
      ?ignore_error_code:bool ->              (* default: false *)
      Shell_sys.job_instance ->
	unit
  (** Looks at the error codes of the job, and raises
   * [Subprocess_error] when there is an error that cannot be ignored.
   * As error conditions are considered non-zero exit codes of any
   * called processes, or signals terminating any of the called processes.
   *
   * @param ignore_error_code If [true], exit codes other than 0 of the
   *   subprocesses are ignored. This does not apply to signals, however.
   *   By default [false].
   *)
  
val assign : src:Unix.file_descr -> target:Unix.file_descr -> assignment
  (** Arranges a redirection such that writing to [src] or reading from [src]
   * will actually write to [target] or read from [target]
   * (i.e., the [target] descriptor is duplicated and replaces
   * the [src] descriptor just before the process is launched.)
   *
   * Note that assignments work only if the descriptors are shared
   * with the called process, so they must also be contained in the
   * [descriptors] list of [command] or [cmd]. Furthermore, the
   * close-on-exec flag must not be set.
   *)

val ( >& ) : Unix.file_descr -> Unix.file_descr -> assignment
  (** Same as [assign], but infix notation. For example,
   * [stdout >& stderr] creates an assignment such that all output
   * to stdout is redirected to stderr.
   *
   * [f >& g] is the same as [assign ~src:f target:g]. It should
   * be used for output assignments (as in the Bourne shell).
   *)

val ( <& ) : Unix.file_descr -> Unix.file_descr -> assignment
  (** Same as [assign], but infix notation. For example,
   * [stdin <& f] creates an assignment such that the called process
   * reads from the open file descriptor [f].
   *
   * [f <& g] is the same as [assign ~src:f target:g]. It should
   * be used for input assignments (as in the Bourne shell).
   *)

val assigned_pair : assignment -> (Unix.file_descr * Unix.file_descr)
  (** Returns the target and the source of the assignment as
   * pair of descriptors [(target,src)].
   *)


val stdin  : Unix.file_descr
val stdout : Unix.file_descr
val stderr : Unix.file_descr
  (** The standard descriptors; defined here for convenience. *)

val from_string :
      ?pos:int ->                  (* default: 0 *)
      ?len:int ->                  (* default: until end of string *)
      ?epipe:(unit -> unit) ->     (* default: empty function *)
      string ->
	producer
  (** Creates a producer taking the data from a string [s]. After these data
   * are sent, the pipeline is closed.
   *
   * @param pos The position in [s] where the data slice to transfer begins.
   *    By default [0].
   * @param len The length of the data slice to transfer. By default,
   *    all bytes from the start position [pos] to the end of the
   *    string are taken.
   * @param epipe This function is called when the pipeline breaks
   *    (EPIPE). Default: the empty function. EPIPE exceptions are
   *    always caught, and implicitly handled by closing the pipeline.
   *)

val from_stream :
      ?epipe:(unit -> unit) ->     (* default: empty function *)
      string Stream.t ->
        producer
  (** Creates a producer taking the data from a stream of strings.
   * After the data are sent, the pipeline is closed.
   *
   * @param epipe This function is called when the pipeline breaks
   *    (EPIPE). Default: the empty function. EPIPE exceptions are
   *    always caught, and implicitly handled by closing the pipeline.
   *)

val from_function :
      producer:(Unix.file_descr -> bool) ->
      unit ->
        producer
  (** Creates a producer taking the data from a function. See
   *  {!Shell_sys.add_producer} for the meaning of the [producer]
   *  function.
   *)

val from_file : string -> producer
  (** Creates a producer taking the data from the file whose name is
   * passed to this function.
   *)

val from_fd : Unix.file_descr -> producer
  (** Creates a producer taking the data from the file descriptor passed
   * to this function.
   *)

val from_dev_null : producer
  (** A producer taking the data from [/dev/null].
   *)

val to_buffer :
      Buffer.t ->
        consumer
  (** Creates a consumer writing the data into the passed buffer.
   *)

val to_function :
      consumer:(Unix.file_descr -> bool) ->
      unit ->
        consumer
  (** Creates a consumer writing the data by calling a function. See
   *  {!Shell_sys.add_consumer} for the meaning of the [consumer]
   *  function.
   *)

val to_file : ?append:bool -> string -> consumer
  (** Creates a consumer writing the data into the file whose name is
   * passed to this function. Unless [append] is given, the file is
   * truncated and overwritten. If [append] is [true], the data are
   * appended to the file. By default, [append] is [false].
   *)

val to_fd : Unix.file_descr -> consumer
  (** Creates a consumer redirecting the data to the file descriptor *)

val to_dev_null : consumer
  (** A consumer redirecting the data to [/dev/null]. *)


(* ******************************************************************** *)
(*                          Examples                                    *)
(* ******************************************************************** *)

(** {1 Examples} 
 *
 * The following examples show toploop sessions using [Shell].
 *)

(** {2 Invoking simple commands}
 *  
 * Call the command "ls" without redirection:
 * {[
 * # call [ command "ls" ];;
 * IDEAS       s1.ml~      shell.mli~      shell_sys.ml~  unix_exts.ml
 * META        shell.a     shell.ml~       shell_sys.o    unix_exts.mli
 * Makefile    shell.cma   shell_sys.cmi   t              unix_exts.mli~
 * Makefile~   shell.cmi   shell_sys.cmo   testjob        unix_exts.ml~
 * depend      shell.cmo   shell_sys.cmx   testjob~       unix_exts.o
 * libshell.a  shell.cmxa  shell_sys.ml    unix_exts.cmi  unix_exts_c.c
 * log         shell.ml    shell_sys.mli   unix_exts.cmo  unix_exts_c.c~
 * s1.ml       shell.mli   shell_sys.mli~  unix_exts.cmx  unix_exts_c.o
 * \- : unit = ()
 * ]}
 *
 * {2 Redirecting stdout to a buffer}
 * 
 * The output of "ls" is collected in the buffer [b]:
 * {[
 * # let b = Buffer.create 10;;
 * val b : Buffer.t = <abstr>
 * # call ~stdout:(to_buffer b) [ command "ls" ];;
 * \- : unit = ()
 * # Buffer.contents b;;
 * \- : string =
 * "IDEAS\nMETA\nMakefile\nMakefile~\ndepend\n..."
 * ]}
 * 
 * {2 Subprocess errors are caught and propagated to the caller}
 * 
 * Because "/a" does not exist, "ls" will fail. The command writes the
 * message to stderr (not redirected here), and returns with an exit
 * code of 1, triggering an exception:
 * {[
 * # call [ command ~arguments:[| "/a" |] "ls" ];;
 * /bin/ls: /a: No such file or directory
 * Uncaught exception: Shell.Subprocess_error ["/bin/ls", Unix.WEXITED 1].
 * ]}
 *
 * {2 Redirecting stderr, too}
 *
 * Here, the message written to stderr is collected in [b]:
 * {[
 * # Buffer.clear b;;
 * \- : unit = ()
 * # call ~stderr:(to_buffer b) [ command ~arguments:[| "/a" |] "ls" ];;
 * Uncaught exception: Shell.Subprocess_error ["/bin/ls", Unix.WEXITED 1].
 * # Buffer.contents b;;
 * \- : string = "/bin/ls: /a: No such file or directory\n"
 * ]}
 *
 * {2 Pipelines}
 *
 * Here, the output of "cat" becomes the input of "sort":
 * {[
 * # call [ command ~arguments:[|"META"|] "cat"; command "sort" ];;
 * archive(byte) = "shell.cma"
 * archive(native) = "shell.cmxa"
 * description = "Unix shell functions"
 * linkopts = "-cclib -lshell"
 * requires = "unix str"
 * version = "0.0"
 * \- : unit = ()
 * ]}
 *
 * {2 Combining pipelines and redirections}
 *
 * The same, but the output of "sort" is collected in the buffer [b]:
 * {[
 * # Buffer.clear b;;
 * \- : unit = ()
 * # call ~stdout:(to_buffer b) [ command ~arguments:[|"META"|] "cat"; command "sort" ];;
 * \- : unit = ()
 * # Buffer.contents b;;
 * \- : string =
 * "archive(byte) = \"shell.cma\"\narchive(native) = \"shell.cmxa\"\ndescription = \"Unix shell functions\"\nlinkopts = \"-cclib -lshell\"\nrequires = \"unix str\"\nversion = \"0.0\"\n"
 * ]}
 *
 * {2 Redirection from a string}
 *
 * The contents of the string [s] are written to the input of "sort":
 * {[
 * # let s = "f\na\nd\nc\n";;
 * val s : string = "f\na\nd\nc\n"
 * # call ~stdin:(from_string s) [ command "sort" ];;
 * a
 * c
 * d
 * f
 * \- : unit = ()
 * ]}
 *
 * {2 Combined redirections}
 *
 * It is possible to have several redirections. Here, the string [s] is
 * sorted by "sort", and the output is collected in the buffer [b]:
 * {[
 * # Buffer.clear b;;
 * \- : unit = ()
 * # call ~stdout:(to_buffer b) ~stdin:(from_string s) [ command "sort" ];;
 * \- : unit = ()
 * # Buffer.contents b;;
 * \- : string = "a\nc\nd\nf\n"
 * ]}
 *
 * {2 Redirections combined with assignments}
 *
 * Here, the output and errors of "ls" are both collected in the buffer
 * [b]:
 * {[
 * # Buffer.clear b;;
 * \- : unit = ()
 * # call ~stdout:(to_buffer b) 
 *        [ command 
 *            ~assignments:[ stderr >& stdout ] 
 *            ~arguments:[| "/a" |] 
 *            "ls" ];;
 * Uncaught exception: Shell.Subprocess_error ["/bin/ls", Unix.WEXITED 1].
 * # Buffer.contents b;;
 * \- : string = "/bin/ls: /a: No such file or directory\n"
 * ]}
 *
 * {2 Final notes}
 *
 * Of course, all features can be combined arbitrarily. 
 *
 * Note that error reporting is better than in a traditional shell, because
 * the exit codes of all started commands are returned. (Shells usually only
 * return the exit code of the last command of a pipeline.)
 *
 * For non-standard pipelines, you can also use the functions in
 * Shell_sys. "call" is a simple concatenation of Shell_sys invocations.
 *)

(**/**)


(* ----------------------------------------------------------------------
   Below are some thoughts about functions that might be useful for
   system programming. They are not yet realized.
   ----------------------------------------------------------------------

type call_arg =
    Path_arg   of string            (* %p *)
  | String_arg of string            (* %s *)
  | List_arg   of string list       (* %l *)
  | Descriptor of Unix.file_descr   (* %d *)
  | Open_file  of Unix.file_descr   (* %f *)

val callf :
      ?ignore_error_code:bool ->              (* default: false *)
      ?mode:Shell_sys.group_mode ->           (* default: Same_as_caller *)
      ?environment:Shell_sys.environment ->   (* default: current env *)
      ?path:(string list) ->                  (* default: use PATH *)
      ?stdin:producer ->
      ?stdout:consumer ->
      ?stderr:consumer ->
      string ->                               (* pipeline in shell notation *)
      call_arg list -> 
	unit
  (* This is the simplified version of "call": The pipeline is passed in
   * shell notation, and may contain placeholders in the style of printf
   * (the reason why this function is called callf).
   * Simple example:
   *   callf "cat file.txt | sort" []
   * creates a pipeline with two members, "cat" and "sort", and passes the
   * argument "file.txt" to "cat".
   * Example with placeholders:
   *   callf "cat %l | sort" [ List_arg [ "file1.txt"; "file2.txt" ]]
   * Here, the arguments for "cat" are not constant but a variable list of
   * strings.
   * For every placeholder %p, %s, %l, %d or %f there must be exactly
   * one corresponding call_arg, and the type of the placeholder must be
   * compatible with the variant of call_arg (see type declaration above).
   *
   * %p, %s: These are simple strings which may occur as stand-alone words
   *   or embedded within words (e.g. %s.txt). %p is only compatible with
   *   Path_arg; %s only with String_arg. A (Path_arg p) is first searched
   *   in the current search path, and the expanded file name replaces %p.
   *   A (String_arg s) exactly substitutes the corresponding %s.
   *
   * %l: This stands for a list of strings; this placeholder must only
   *   occur as command argument. For every value of the list passed by
   *   List_arg the word containing %l is instantiated; e.g. %l.txt with 
   *   List_arg ["a";"b";"c"] will expand to "a.txt", "b.txt", "c.txt".
   *   If a word contains %l, it must not contain another placeholder.
   *
   * %d: Refers to a descriptor of the subprocess, to be used in
   *   descriptor assignments. For example:
   *     callf "myscript %d>&%d" [ Descriptor stderr; Descriptor stdout ]
   *   A %d must correspond to a Descriptor value.
   *
   * %f: Refers to a descriptor of the current process (i.e. an open file),
   *   to be used in descriptor assignments. For example:
   *     callf "myscript %d>%f" [ Descriptor stderr; Open_file f ]
   *   - where f is a file open for writing.
   *   A %f must correspond to an Open_file value.
   *
   * The following notations are recognized:
   *
   * First, the string is separated into words which are delimited by
   * spaces, htabs, or pipe symbols.
   *
   * The list of words is now separated into commands, separated by 
   * pipe symbols.
   *
   * Words containing "<" or ">" count as descriptor assignments. The remaining
   * words are analyzed as follows: The first word is the command name. The
   * other words are the arguments of the command.
   *
   * You can include spaces, htabs, |, %, < and > symbols as part of a word
   * by preceding them with percent symbol (e.g. %| is the character '|' and
   * not the command separator '|'). Caution: Besides % there is no other
   * quoting mechanism; neither single nor double quotes nor backslashes
   * can be used to indicate word boundaries.
   *
   * Unlike the shell, the command is not again splitted into words after
   * the placeholders have been replaced by their corresponding values.
   *
   * The following descriptor assignments are possible:
   * - n>&m    where n,m numbers or %d: The descriptor n becomes a duplicate
   *           of m (regardless of whether m is open for reading or writing)
   * - n>name  where n is a number or %d, and name is a file name (may contain
   *           %s) or name is %f: The descriptor n of the subprocess writes
   *           to the file
   * - n>>name where n is a number or %d, and name is a file name (may contain
   *           %s) or name is %f: The descriptor n of the subprocess appends
   *           to the file
   * - n<name  where n is a number or %d, and name is a file name (may contain
   *           %s) or name is %f: The descriptor n of the subprocess reads
   *           from the file
   * - n<>name where n is a number or %d, and name is a file name (may contain
   *           %s) or name is %f: The descriptor n of the subprocess is opened
   *           for reading and writing to the file
   * Note that the forms n>%f, n>>%f, n<%f, n<>%f are equivalent; it is
   * recommended to choose the notation which reminds the reader of the
   * intended purpose of the assignment.
   *
   * Optional arguments:
   * - See also "call" above.
   * - ~environment: The environment to be passed to the processes.
   * - ~path: The search path used for command searching. Commands (both
   *   constant commands and commands passed by Path_arg) are searched in
   *   the path only if they do not contain a slash character '/'.
   *   If ~path is not present, the environment variable PATH is scanned
   *   for the search path. 
   *   To reject commands not containing a slash: ~path:[]
   *   To switch off command searching: ~path:["."]
   *)

val list_files :
      ?name_pattern:string ->           (* default: every name is included *)
      ?filter:(string -> bool) ->       (* default: fun _ -> true *)
      ?recursive:bool ->                (* default: false *)
      ?follow_symlinks:bool ->          (* default: false *)
      ?directory:bool ->                (* default: false *)
      ?sorted:bool ->                   (* default: true *)
      ?omit_dot:bool ->                 (* default: true *)
      ?omit_dot_dot:bool ->             (* default: true *)
      ?omit_hidden:bool ->              (* default: true *)
      string ->
        string list
  (* List the files of the passed directory (yes, it _must_ be a directory).
   * 
   * ~name_pattern: Include only files whose names match the regular
   *    expression (Str-like expression). Only the name of the files count,
   *    not the path before the last '/'
   * ~filter: Include only files for which the filter returns 'true'.
   * ~recursive: If the listed files contain directories other than "." and
   *    "..", these are recursively listed, too. Unless, ~follow_symlinks
   *    is set, symbolic links are not followed in this case.
   * ~follow_symlinks: If set, symbolic links are resolved when descending
   *    into the file tree. Note that a symlink on the toplevel is always
   *    followed (even if ~directory is set).
   * ~directory: If set, the passed directory itself is prepended to the
   *    output (e.g.:
   *    list_files ~directory:false "." = [ "file1"; "file2" ], but
   *    list_files ~directory:true  "." = [ "."; "./file1"; "./file2" ])
   * ~sorted: Every directory list is sorted before output. 
   * ~omit_dot: The file "." is not output (unless it is the name of the
   *    passed directory)
   * ~omit_dot_dot: The file ".." is not output (unless it is the name of the
   *    passed directory)
   * ~omit_hidden: Files beginning with a dot are not output (unless it is 
   *    the name of the passed directory)
   *)

val iter_files :
      ?pattern:string ->
      ?filter:(string -> bool) ->
      ?recursive:bool ->
      ?follow_symlinks:bool ->
      ?directory:bool ->
      ?sorted:bool ->
      ?omit_dot:bool ->
      ?omit_dot_dot:bool ->
      ?omit_hidden:bool ->
      f:(string -> unit) ->
      string ->
        unit
  (* For every file of the output set, the function ~f is invoked. For the
   * other arguments, see list_files.
   *)

  (* TODO: preorder/postorder sorting *)

(* User-friendly file tests: *)

val exists               : string -> bool
val is_regular           : string -> bool
val is_not_empty         : string -> bool
val is_directory         : string -> bool
val is_symlink           : string -> bool
val is_named_pipe        : string -> bool
val is_socket            : string -> bool
val is_special           : string -> bool
val is_block_special     : string -> bool
val is_character_special : string -> bool
val is_suid              : string -> bool
val is_sgid              : string -> bool
val is_readable          : ?effectively:bool -> string -> bool
val is_writable          : ?effectively:bool -> string -> bool
val is_executable        : ?effectively:bool -> string -> bool
val is_newer_than        : string -> string -> bool
val are_the_same         : string -> string -> bool

(* User-friendly file operations: *)

val rm :
      ?force:bool ->             (* default: false *)
      ?only_symlink:bool ->      (* default: false *)
      ?recursively:bool ->
      string ->
        unit
  (* ~force: do not fail if the file does not exists or permission do not
   *  suffice
   * ~only_symlink: only remove the file if it is a symlink; otherwise fail
   *  (unless ~force)
   *)

type lnmode =
    New
  | New_in_directory
  | New_or_directory
  | Update
  | Update_in_directory
  | Update_or_directory

(* TODO:
 * ln = modes New, Update
 * ln_into = modes New_in_directory, Update_in_directory
 *)

(* New: newname must be a non-existing name in an existing directory
 * New_in_directory: if newname is an existing directory, create a new
 *    link for the file there
 * New_or_directory: one of the cases New, New_in_directory
 * Update: if newname is non-existing: see New. If newname exists, it must
 *    not be a directory, and the link is updated
 * Update_in_directory: newname must be an existing directory. If the
 *    link already exists in this directory, update it; otherwise create
 *    it
 * Update_or_directory: one of the cases Update, Update_in_directory
 *)

val ln :
      ?mode:lnmode ->           (* default: New_or_directory *)
      oldname:string ->
      newname:string ->
        unit
  (* creates or updates a hard link *)

val symln : (* or ln_s *)
      ?mode:lnmode ->           (* default: New_or_directory *)
      oldname:string ->
      newname:string ->
        unit
  (* creates or updates a symbolic link *)

val cp :
      ?recursively:bool ->       (* default: false *)
      ?parents:bool ->           (* default: false *)
      ?follow_symlinks:bool ->   (* default: false *)
      ?force:bool ->             (* default: false *)
      ?unlink_src:bool ->        (* default: false *)
      ?install:bool ->           (* default: false *)
      ?perms:int ->              (* default: derived from umask *)
      ?user:string ->            (* default: real user *)
      ?group:string ->           (* default: real group *)
      ?preserve_timestamp:bool ->   (* default: false *)
      ?preserve_perms:bool ->
      ?preserve_user:bool ->
      ?preserve_group:bool ->
      ?create_missing_dirs:bool ->  (* default: false *)
      src:string ->
      dest:string ->
        unit
  (* This "cp" will fail when copying special files *)
  (* 
   * ~parents: see cp --parents (questionable)
   * ~install: removes dest before making the copy
   *
   * ~preserve_xxx beats ~xxx for files that existed as source. However,
   *  for newly created directories the ~xxx options count.
   *
   *)

val cp_into :
      ?recursively:bool ->       (* default: false *)
      ?follow_symlinks:bool ->   (* default: false *)
      ?unlink_src:bool ->        (* default: false *)
      src:string list ->
      dest:string ->             (* must be a directory *)
        unit

val mv :
      ?force:bool ->             (* default: false *)
      ?only_symlink:bool ->      (* default: false *)
      src:string ->
      dest:string ->
        unit

val mv_into :     
      ?force:bool ->             (* default: false *)
      ?only_symlink:bool ->      (* default: false *)
      src:string list ->
      dest:string ->
        unit

val mkdir  (* esp. mkdir -p *)
val rmdir
val chmod (* mit symbolischer Angabe *)
val chown (* mit ausgeschriebenen Usern *)
val touch 
val file_size
val file_user
val file_group
val file_atime
val file_ctime
val file_mtime
val du
val cat
val md5sum

(* Module:
 * Shell_tar: access to the "tar" command
 * Shell_cpio
 * Shell_text: line-by-line text processing
*)
  ---------------------------------------------------------------------- *)
