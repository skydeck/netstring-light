(* $Id: shell_sys.mli 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Calls external programs, creates pipelines, etc. (full interface) *)

(** This module is now thread-safe (as of May 2009), provided the
    threads do no share the same [Shell] or [Shell_sys] values.
    Problems reported earlier here have been resolved.
*)

(** If you get errors like "Netsys_posix.watch_subprocess: uninitialized"
    you should call {!Shell_sys.install_job_handlers}.
 *)

(** {1 Common exceptions} *)

exception Fatal_error of exn
  (** An error is fatal if it is not possible to recover from it in a
   * predictable manner. In this case, many function wrap such exceptions
   * [x] into [Fatal_error x].
   *)

(* ******************************************************************** *)
(* **                       environments                             ** *)
(* ******************************************************************** *)

(** {1 Environments} *)

type environment
  (** The abstract type of a process environment *)

val create_env  : unit -> environment
  (** Creates an empty environment *)

val current_env : unit -> environment
  (** Returns the environment of the current process as abstract environment
   * value 
   *)

val copy_env    : environment -> environment
  (** Copies an environment *)

val set_env     : environment -> string array -> unit
  (** Sets the contents of the environment to the passed string array *)

val get_env     : environment -> string array
  (** Gets the contents of the environment as string array *)

val iter_env    : f:(string -> unit) -> environment-> unit
  (** Iterates over the strings of the environment, and calls
   * [f s] for every string [s].
   *)

val set_env_var : environment -> string -> string -> unit
  (** [set_env_var env varname varval]: Sets the value of the variable
   * [varname] in the environment [env] to [varval].
   *)

val get_env_var : environment -> string -> string
  (** Returns the value of the variable in the environment *)

val iter_env_vars : f:(string -> string -> unit) -> environment -> unit
  (** Iterates over the variables of the environment, and calls
   * [f name value] for every variable with [name] and [value].
   *)

(* ******************************************************************** *)
(* **                    commands and processes                      ** *)
(* ******************************************************************** *)

(** {1 Commands} *)

type command
  (** A command describes how to start a new process *)

(* A _command_ is the description how to start a new process. A
 * _process_ is the running instance of a command; the same command
 * may be started several times.
 *)

val command :
      ?cmdname:string ->                   (* default: derived from filename *)
      ?arguments:(string array) ->         (* default: empty *)
      ?chdir:string ->                     (* default: current working dir *)
      ?environment:environment ->          (* default: current environment *)
      ?descriptors:(Unix.file_descr list) -> 
                                           (* default: stdin, stdout, stderr *)
      ?assignments:((Unix.file_descr * Unix.file_descr) list) ->    
                                           (* default: empty *)
      filename:string ->
      unit ->
	command
  (** Creates a command from the passed arguments:
   *
   * @param filename The name of the executable to start. The executable
   *   file is not searched, use {!Shell_sys.lookup_executable} for this
   *   purpose.
   * @param cmdname The name of the command passed in [argv[0]]. By
   *   default, this argument is derived from [filename].
   * @param arguments The arguments of the command (starting with the
   *   first real argument, skipping [cmdname]). By default [ [] ].
   * @param chdir Before the command is executed it is changed to
   *   this directory.
   * @param environment The environment of the command. By default, the
   *   current environment
   * @param assignments A list of descriptor pairs [ (fd_from,fd_to) ].
   *   The descriptor [fd_from] in the current process will be assigned
   *   to [fd_to] in the subprocess started for the command. 
   *   The list of assignments is executed sequentially, so
   *   later assignments must take the effect of previous assignments
   *   into account. For example, to make stderr of the subprocess write 
   *   to stdout of the parent process, pass [ [(stdout; stderr)] ]. 
   *   - By default, there are no assignments.
   * @param descriptors The list of file descriptors to share with the
   *   current process. In the subprocess only those descriptors remain open
   *   that are either mentioned in [descriptors], or that are the final target
   *   of assignments. By default, [ [stdin; stdout; stderr] ].
   *
   * Note that only the {b final targets} of assignments remain open in the
   * subprocess (unless they are also listed in [descriptors]). If there
   * are cascaded assignments like [ (fd1, fd2); (fd2, fd3) ] the intermediate
   * descriptors like [fd2] are not considered as final targets; only [fd3]
   * would be a final target in this example. 
   *)

exception Executable_not_found of string;;
  (** Raised when an executable file cannot be found; the argument is the
   *  search name
   *)

val lookup_executable :
      ?path:(string list) ->     (* default: use the PATH variable *)
      string ->
	string
  (** Searches an executable file. If the passed search name contains a
   * slash, it is expected that this name is already the path name of the
   * executable. If the search name does not contain a slash character,
   * it is searched in the directories enumerated by the search path.
   *
   * @param path The search path. By default, the contents of the
   *   variable PATH of the current environment, split by ':', are
   *   used (Win32: SearchPath is used)
   *)

val get_cmdname     : command -> string
  (** Returns the name of the command *)

val get_arguments   : command -> string array
  (** Returns the argument array of the command (skipping the command name) *)

val get_chdir       : command -> string option
  (** Returns the [chdir] parameter of the command *)

val get_environment : command -> environment
  (** Returns the designated environment of the command *)

val get_descriptors : command -> Unix.file_descr list
  (** Returns the list of active descriptors *)

val get_assignments : command -> (Unix.file_descr * Unix.file_descr) list
  (** Returns the list of assignments [ (fd_from,fd_to) ] *)

val get_filename    : command -> string
  (** Returns the file name of the executable *)

val set_cmdname     : command -> string          -> unit
  (** Sets the command name *)

val set_arguments   : command -> string array    -> unit
  (** Sets the argument array *)

val set_chdir       : command -> string option   -> unit
  (** Sets the [chdir] parameter of the command *)

val set_environment : command -> environment     -> unit
  (** Sets the environment *)

val set_descriptors : command -> Unix.file_descr list -> unit
  (** Sets the list of active descriptors *)
 
val set_assignments : command -> (Unix.file_descr * Unix.file_descr) list -> unit
  (** Sets the list of assignments [ (fd_from,fd_to) ] *)

val set_filename    : command -> string          -> unit
  (** Sets the file name of the executable to start *)

val copy_command : command -> command
  (** Returns a duplicate of the command description *)

val is_executable : command -> bool
  (** Returns [true] if there is an executable file for the command, and
   * it is permitted to run this file (as stated by the file permissions).
   *
   * [false] means that the command can definitely not be executed. However,
   * even if the function returns [true] there may be still reasons that
   * execution will fail.
   *)

(** {1 Processes} *)

type process
  (** A process is the running instance of a command (a Unix process) *)

type group_action =
    New_bg_group      (** Start process in new background process group *)
  | New_fg_group      (** Start process in new foreground process group *)
  | Join_group of int (** Started process joins this existing process group *)
  | Current_group     (** Started process remains in the current group *)
  (** Determines in which process group the new process will run *)

type fwd_mode =
    No_forward          (** No forwarding of keyboard signals *)
  | Forward_to_process  (** Forward signals directly to subprocess *)
  | Forward_to_group    (** Forward signals to the process group of the subprocess *)
  (** Determines whether and how keyboard signals (SIGINT, SIGQUIT) are
      forwarded from the caller to the new child. There is no forwarding
      in Win32 - all console applications get the keyboard signals anyway.
   *)


val run :
      ?group:group_action ->       (* default: Current_group *)
      ?forward_mode:fwd_mode ->    (* default: No_forward *)
      ?pipe_assignments:((Unix.file_descr * Unix.file_descr) list) ->
                                   (* default: [] *)
      command ->
	process
  (** Executes the command concurrently with the current process. The function
   * does not wait until the process terminates; it returns immediately after
   * the [exec] system call has been successfully performed; errors that
   * occur until [exec] are caught and reported as exception (even errors
   * in the fresh subprocess).
   *
   * On error, one can assume that the process state has been cleaned up:
   * any forked child process has terminated; any modifications of the global
   * process state has been restored. 
   *
   * File descriptor assignments: First, the assignments in [pipe_assignments]
   * are performed, then the assignments contained in the command. The
   * [pipe_assignments] are interpreted as parallel assignment, not
   * as sequential assignment.
   *
   * Note: For users without very special needs, it is recommended to run
   * jobs instead of processes. See below for the job API.
   *
   * @param group Determines in which process group the new process will
   *   run. By default [Current_group].
   * @param forward_mode Whether and how to forward keyboard signals
   *   to the new child. By default [No_forward]. The Win32 implementation
   *   ignores this argument.
   * @param pipe_assignments A list of descriptor pairs [(fd_from,fd_to)].
   *   The descriptor [fd_from] in the current process will be assigned
   *   to [fd_to] in the started subprocess. In order to
   *   take effect, [fd_to] must also be passed in the [descriptors]
   *   property of the started command.
   *   Furthermore, [fd_from] may or may not be member of [descriptors];
   *   in the first case it will remain open, in the latter case it will
   *   be closed. The list of assignments is executed in parallel. For
   *   example, to swap the roles of stdout and stderr, pass the list
   *   [ [(stdout,stderr); (stderr,stdout)] ].
   *)

val process_id : process -> int
  (** Returns the process ID of the process *)

val status : process -> Unix.process_status
  (** Reports the status so far known: If the process 
   * has terminated, the status of the process is returned.
   * If the process is still running, [Not_found] will be raised.
   *)

val command_of_process : process -> command
  (** Returns the command that is now running as the process *)

val call : command -> process
  (** Executes the command and waits until the process terminates
   * (synchronous execution a la [system], but no intermediate shell).
   * [status] is guaranteed to return WEXITED or WSIGNALED.
   *)

val kill :
      ?signal:int ->       (* default: SIGTERM *)
      process ->
        unit
  (** Sends a signal to the passed process.
   *
   * @param signal The signal to send, by default SIGTERM
   *)


(* ******************************************************************** *)
(* **                            jobs                                ** *)
(* ******************************************************************** *)

(** {1 Jobs} *)

(** A [job] is the description of how to run several commands which are
 * linked by pipelines (or which are just a logical unit). A [job_instance]
 * is the running instance of a job.
 *
 * Jobs are implemented on a higher layer than commands; the
 * following means of the operating system are used by job
 * invocations:
 * - Normally a [job_instance] corresponds to a Unix process group. In
 *   this case the last added command will result in the process group
 *   leader.
 * - Controlling the execution of jobs requires that signal
 *   handlers are set in many cases (see [install_job_handlers])
 * - The processes of jobs are often interconnected by pipelines
 *   (see [add_pipeline]).
 * - It is possible to handle pipelines between the current process and
 *   processes of the job (see [add_producer] and [add_consumer])
 *)

(** {b Important:}
 * 
 * In order to run jobs efficiently (without busy waiting) and properly
 * it is strongly recommended to install the signal handlers using
 * [install_job_handlers]
 *)  

type job
type job_instance

val new_job : unit -> job
  (** Creates a new job descriptor. Initially the job is empty, but you can
   * fill it with commands ([add_command]), pipelines ([add_pipeline]), 
   * consumers ([add_consumer]) and producers ([add_producer]).
   * When the job is set up, you can start it ([run_job]/[finish_job] or
   * [call_job]).
   *)

val add_command : command -> job -> unit
  (** Adds a command to a job. 
   *
   * Note that you cannot add the same command twice; however you can
   * add a copy of a command already belonging to the job.
   *)


val add_pipeline :
      ?bidirectional:bool ->           (* default: false *)
      ?src_descr:Unix.file_descr ->    (* default: stdout *)
      ?dest_descr:Unix.file_descr ->   (* default: stdin *)
      src:command ->
      dest:command ->
      job ->
        unit
  (** Adds a pipeline which redirects the output of the command [src] to the
   * input of the command [dest].
   * 
   * @param src_descr determines the file descriptor of the source command
   *    which is redirected. This is by default [stdout].
   * @param dest_descr determines the file descriptor of the destination
   *    command to which the data stream is sent. This is by default [stdin].
   * @param bidirectional if [false] (default), a classical pipe is created
   *    to connect the file descriptors. This normally restricts the data
   *    flow to one direction. If [true], a socketpair is created which is
   *    roughly a bidirectional pipe. In this case, data flow in both
   *    directions is possible.
   *)


val add_producer :
      ?descr:Unix.file_descr ->     (* default: stdin *)
      producer:(Unix.file_descr -> bool) ->
      command ->
      job ->
        unit
  (** Adds a producer to the job. A producer transfers data to the
   * subprocess realizing the passed command. To do so, a pipe is created
   * between the file descriptor [descr] of the subprocess and another
   * descriptor [descr'] which is open in the current process. The
   * function [producer] is called when data can be written into the
   * pipe. The argument of [producer] is the writing end of the pipe
   * [descr']. This file descriptor is in non-blocking mode. The
   * function [producer] must close [descr'] when all data are
   * transferred. The return value of [producer] indicates whether
   * the descriptor is still open.
   *
   * @param descr The descriptor of the subprocess to which the reading
   *    end of the pipe is dup'ed. By default [stdin].
   *)

(* CHECK: Was passiert wenn producer eine exception wirft? *)

val from_string :
      ?pos:int ->                  (* default: 0 *)
      ?len:int ->                  (* default: until end of string *)
      ?epipe:(unit -> unit) ->     (* default: empty function *)
      string ->
        (Unix.file_descr -> bool)
  (** [from_string ?pos ?len ?epipe s] returns a function which can be
   * used as [producer] argument for [add_producer]. The data transferred
   * to the subprocess is taken from the string [s]. After these data
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
        (Unix.file_descr -> bool)
  (** [from_stream ?epipe s] returns a function which can be
   * used as [producer] argument for [add_producer]. The data transferred
   * to the subprocess is taken from the string stream [s]. After these data
   * are sent, the pipeline is closed.
   *
   * @param epipe This function is called when the pipeline breaks
   *    (EPIPE). Default: the empty function. EPIPE exceptions are
   *    always caught, and implicitly handled by closing the pipeline.
   *)

val add_consumer : 
      ?descr:Unix.file_descr ->     (* default: stdout *)
      consumer:(Unix.file_descr -> bool) ->
      command ->
      job ->
        unit
  (** Adds a consumer to the job. A consumer transfers data from the
   * subprocess realizing the passed command to the current process. 
   * To do so, a pipe is created between the file descriptor [descr]
   * of the subprocess and another descriptor [descr'] which is open
   * in the current process. The function [consumer] is called when 
   * data can be read from the pipe. The argument of [consumer] is 
   * reading end of the pipe [descr']. This file descriptor is in
   * non-blocking mode. The function [consumer] must close [descr'] 
   * after EOF is detected. The return value of [consumer] indicates whether
   * the descriptor is still open.
   *
   * @param descr The descriptor of the subprocess to which the writing
   *    end of the pipe is dup'ed. By default [stdout].
   *)

(* CHECK: Was passiert wenn consumer eine exception wirft? *)

val to_buffer :
      Buffer.t ->
        (Unix.file_descr -> bool)
  (** [to_buffer b] returns a function which can be
   * used as [consumer] argument for [add_consumer]. The data received
   * from the subprocess is added to the buffer [b]. 
   *)

type group_mode = 
    Same_as_caller    (** The job runs in the same process group as the current process *)
  | Foreground        (** The job runs in a new foreground process group *)
  | Background        (** The job runs in a new background process group *)
  (** Specifies how the job instance is related to process groups *) 

val run_job :
      ?mode:group_mode ->                (* default: Same_as_caller *)
      ?forward_signals:bool ->           (* default: true *)
      job ->
        job_instance
  (** Invokes the commands of the job such that they run concurrently
   * with the main process.
   *
   * The function returns a [job_instance], i.e. a value recording which
   * processes are started, and how they are related. Furthermore, the
   * function has the side effect of adding the
   * job to the global list of current jobs.
   *
   * The [mode] argument specifies whether a new Unix process group is
   * created for the job instance. A process group has the advantage that
   * it is possible to send signals to all processes of the group at
   * once. For example, one can terminate a group by sending SIGTERM
   * to it: All member processes get the signal. Usually, these are not only
   * the subprocesses initially created, but also further processes 
   * started by the initial members.
   *
   * So if it is necessary to send signals to the processes of the job,
   * it will be advantegous to run it in a new process group. However,
   * this also means that signals sent to the current process group
   * are not automatically forwarded to the created process group. For
   * example, if the current process group is terminated, the job
   * will continue running, because it is member of a different process
   * group. One has to explicitly catch and forward signals to avoid
   * wild-running jobs.
   *
   * The moral of the story is that one should only create new process
   * groups when it is necessary (e.g. the user must be able to stop
   * an action at any time). Furthermore, signal forwarding must be
   * configured.
   *
   * The Unix shell also allows the programmer to specify process group
   * handling to a certain extent. Normally, commands are executed in the
   * same process group as the caller. The syntax "command &" forces that
   * the command is run in a new background process group. There is another
   * situation when new process groups are created: when a new {b interactive} 
   * shell is started the commands are run in new foreground process groups
   * (so the keyboard signals like CTRL-C work).
   *
   * @param mode Specifies the process group handling. By default, the
   *   job is executed in the same process group as the current process
   *   ([Same_as_caller]). The value [Background] causes that a new
   *   background process group is started. The value [Foreground] causes
   *   that a new foreground process group is started. For the latter,
   *   it is required that there is a controlling terminal (i.e. it
   *   does not work for daemons). Any existing foreground process group
   *   (there is at most one) is put into the background, but this is
   *   not restored when the job is over (the caller must do this).
   *   Foreground process groups should be avoided unless you are
   *   writing an interactive shell interpreter.
   * @param forward_signals If [true], the default, keyboard signals
   *   (SIGINT, SIGQUIT) delivered to the current process are forwarded to 
   *   the job. This has only a meaning if the job is running as 
   *   background process group. Furthermore, it is required that
   *   [install_job_handlers] has been called to enable signal 
   *   forwarding.
   * 
   * The function returns normally if at least one process could be started.
   * If no process was startable (i.e. the first command was not startable), 
   * an exception is raised. If one or more processes could be started but
   * not all, [job_status] will return [Job_partially_running]. The caller 
   * should then discard the job and any intermediate result that might
   * already have been produced by the partial job.
   *
   * When all processes could be started and no other exceptional condition
   * happened, the function sets [job_status] to [Job_running].
   *)

(** This type of engine also returns the [job] and the [job_instance].
 *)
class type ['t] job_handler_engine_type = object
  inherit ['t] Uq_engines.engine

  method job : job
    (** Returns the called job *)

  method job_instance : job_instance
    (** Returns the job instance *)
end;;


class job_engine : Unixqueue.event_system -> job_instance ->
                     [unit] job_handler_engine_type
  (** The [job_engine] watches the job, and looks whether the processes
      are finished, and if so, it records the process statuses. Also,
      the engine takes care of pumping producer data into the job,
      and of collecting consumer data.
   *)

val finish_job :
      job_instance -> unit
  (** This creates a [job_engine] internally and runs until it is
   * finished, i.e. until the job has been executed.
   *
   * In previous version of Ocamlnet there was an optional [sys]
   * argument. This is gone now. Also, the error handling is different.
   * It is no longer possible to restart [finish_job] when an error
   * happens.
   *)

val call_job :
      ?mode:group_mode ->                     (* default: Same_as_caller *)
      ?forward_signals:bool ->                (* default: true *)
      ?onerror:(job_instance -> unit) ->      (* default: abandon_job *)
      job ->
        job_instance
  (** Starts the job (see [run_job]) and waits until it finishes (see
   * [finish_job]); i.e. [call_job = run_job + finish_job].
   * The function returns normally if all processes can be started; you can
   * examine [job_status] of the result to get the information whether all
   * processes returned the exit code 0.
   *
   * @param onerror If not all of the processes can be started, the
   *    function passed by [onerror] is invoked. By default, this
   *    function calls [abandon_job] to stop the already running
   *    processes. After the [onerror] function has returned, the original 
   *    exception is raised again. Fatal error conditions are not caught.
   * @param mode See [run_job]
   * @param forward_signals See [run_job]
   *)

val processes : job_instance -> process list
  (** Returns the processes that have actually been started for this job
   * by [run_job]; note that the corresponding Unix process group
   * may have additional processes (e.g. indirectly started processes).
   *)

exception No_Unix_process_group;;
  (** Raised by functions referring to Unix process groups when the
   * job has not been started in its own process group.
   *)

val process_group_leader : job_instance -> process
  (** Returns the process group leader process.
   * This function is not available for jobs in the mode [Same_as_caller].
   *)

val process_group_id : job_instance -> int
  (** Returns the Unix ID of the process group as number > 1.
   * This function is not available for jobs in the mode [Same_as_caller].
   *)

type job_status =
    Job_running            (** All commands could be started, and at least
			    * one process is still running
			    *)
  | Job_partially_running  (** Not all commands could be started, and at least
			    * one process is still running
			    *)
  | Job_ok                 (** all processes terminated with exit code 0 *)
  | Job_error              (** all processes terminated but some abnormally *)
  | Job_abandoned          (** the job has been abandoned (see [abandon_job]) *)
  (** Indicates the status of the job *)

val job_status : job_instance -> job_status
  (** Returns the status. The status may only change after [finish_job]
   * has been called:
   *
   * - after [run_job]: status is [Job_running] or [Job_partially_running]
   * - after [finish_job]: if returning normally: status is [Job_ok] or 
   *   [Job_error]. After an exception happened the other states are possible,
   *   too
   *)

val kill_process_group : 
      ?signal:int ->              (* default: SIGTERM *)
      job_instance -> unit
  (** Kills the process group if it is still (at least partially) running.
   * This operation is not available if the mode is [Same_as_caller]
   * (exception [No_Unix_process_group]).
   *
   * Note 1: In the Unix terminology, "killing a job" only means to send
   * a signal to the job. So the job may continue running, or it may
   * terminate; in general we do not know this. Because of this, the job
   * will still have an entry in the job list.
   *
   * Note 2: Because sub-sub-processes are also killed, this function may send
   * the signal to more processes than kill_processes (below). On the other
   * hand, it is possible that sub-processes change their group ID such that
   * it is also possible that this function sends the signal to fewer processes
   * than kill_processes.
   *
   * @param signal The signal number to send (O'Caml signal numbers as
   *    used by the [Sys] module). Default is [Sys.sigterm].
   *)

val kill_processes : 
      ?signal:int ->              (* default: SIGTERM *)
      job_instance -> unit
  (** Kills the individual processes of the job which are still running.
   *
   * @param signal The signal number to send (O'Caml signal numbers as
   *    used by the [Sys] module). Default is [Sys.sigterm].
   *)

val cancel_job :
      ?signal:int ->              (* default: SIGTERM *)
      job_instance -> unit
  (** Tries to get rid of a running job. If the mode is [Same_as_caller], the
   * signal is sent to the processes individually. If the mode is
   * [Foreground] or [Background], the signal is sent to the process group 
   * corresponding to the job.
   *
   * This function removes the job from the job list; i.e. it is no longer
   * watched. Because of some magic spells it is guaranteed that the job dies
   * immediately without becoming a zombie (provided you have a SIGCHLD
   * handler).
   *
   * @param signal The signal number to send (O'Caml signal numbers as
   *    used by the [Sys] module). Default is [Sys.sigterm].
   *)

val abandon_job : ?signal:int -> job_instance -> unit
  (** {b Deprecated} name for [cancel_job] *)

exception Already_installed;;
  (** Raised when the job handlers are already installed *)

val configure_job_handlers :
      ?catch_sigint:bool ->      (* default: true *)
      ?catch_sigquit:bool ->     (* default: true *)
      ?catch_sigterm:bool ->     (* default: true *)
      ?catch_sighup:bool ->      (* default: true *)
      ?at_exit:bool ->           (* default: true *)
      unit ->
      unit
  (** Configures signal and at_exit handlers for jobs:
   * - The keyboard signals SIGINT and SIGQUIT are forwarded to all jobs
   *   which are running in the background (and thus are not
   *   automatically notified) and want to get such signals ([forward_signals]).
   * - The signals SIGTERM and SIGHUP are (if the handler is installed) 
   *   forwarded to all dependent processes (regardless whether they are
   *   running in their own Unix process group or not, and regardless of
   *   [forward_signals]).
   * - The [at_exit] handler sends a SIGTERM to all dependent processes, too.
   *
   * In previous versions of Ocamlnet it was also possible to configure
   * [catch_sigchld] to set whether a SIGCHLD handler is installed. This
   * is now always done.
   *
   * In previous versions of Ocamlnet there was also a [set_sigpipe] flag.
   * This flag is gone as a SIGPIPE handler is now always installed.
   *
   * The handlers are now managed by {!Netsys_signal}. The handlers of this
   * module set the [keep_default] flag for SIGINT, SIGQUIT, SIGTERM, and
   * SIGHUP, so that the default action for these signals is executed after
   * the forwarding to the child processes is done. By setting another
   * handler in {!Netsys_signal} without that flag this behavior can be
   * overridden.
   *
   * Note that if an uncaught exception leads to program termination,
   * this situation will not be detected; any running jobs will
   * not be terminated (sorry, this cannot be fixed).
   *
   * This function sets only which handlers will be installed when
   * [install_job_handlers] (below) is invoked.
   * The function fails if the handlers are already installed.
   *
   * Win32: No handlers are installed. It would be desirable to some extent
   * that at least [at_exit] is honoured, however, this is not yet done.
   *
   * @param catch_sigint whether to install a SIGINT handler (default: [true])
   * @param catch_sigquit whether to install a SIGQUIT handler (default: [true])
   * @param catch_sigterm whether to install a SIGTERM handler (default: [true])
   * @param catch_sighup whether to install a SIGHUP handler (default: [true])
   * @param at_exit whether to set the [at_exit] handler (default: [true])
   *)

val install_job_handlers : unit -> unit
  (** Installs handlers as configured before.
   * Raises [Already_installed] if the handlers are already installed.
   *)

(** {1 Removed functions} *)

(** The functions [add_rd_polling] and [add_wr_polling] have been removed.
 * They were added prior to the merge with the equeue library. Use a 
 * Unixqueue now, which is much more powerful.
 *)

(** Also no longer supported because the type [system_handler] is gone:

    {[type system_handler]}

    {[ type process_event ]}

    {[val wait : 
      ?wnohang:bool ->                     (* default: false *)
      ?wuntraced:bool ->                   (* default: false *)
      ?restart:bool ->                     (* default: false *)
      ?check_interval:float ->             (* default: 0.1 *)
      ?read:(Unix.file_descr list) ->      (* default: [] *)
      ?write:(Unix.file_descr list) ->     (* default: [] *)
      ?except:(Unix.file_descr list) ->    (* default: [] *)
      process list ->
        process_event list
    ]}
    {[val register_job : system_handler -> job_instance -> unit]}

    {[val iter_job_instances : f:(job_instance -> unit) ->  unit]}

    {[val watch_for_zombies : unit -> unit]}

    {[val process_group_expects_signals : job_instance -> bool]}

 *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module  *)
end




