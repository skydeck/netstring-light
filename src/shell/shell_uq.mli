(* $Id: shell_uq.mli 50 2004-10-03 17:06:28Z gerd $ *)

(** Run shell commands within Unixqueues *)

(** This module is {b not thread-safe}. See the module [Shell_sys] for
 * more information.
 *)
 
(** {b Signal handlers:} It is important to have a number of signal handlers
 * installed for proper function of the engines. It is recommended to
 * call the pair of functions {!Shell_sys.configure_job_handlers} and
 * {!Shell_sys.install_job_handlers} for this purpose. This is not
 * done automatically.
 *
 * Note that this has a global side effect on the whole process, because
 * there is only one set of signal handlers.
 *)

(** {1 Engines} *)


(** This type of engine also returns the [job] and the [job_instance].
 *)
class type ['t] job_handler_engine_type = object
  inherit ['t] Uq_engines.engine

  method job : Shell_sys.job
    (** Returns the called job *)

  method job_instance : Shell_sys.job_instance
    (** Returns the job instance *)
end;;


class call_engine :
      ?ignore_error_code:bool ->
      ?mode:Shell_sys.group_mode ->
      ?stdin:Shell.producer ->
      ?stdout:Shell.consumer ->
      ?stderr:Shell.consumer ->
      Shell_sys.command list ->
      Unixqueue.event_system ->
        [Shell_sys.job_status] job_handler_engine_type
  (** This engine corresponds to {!Shell.call}. The command list is
   * executed until the job is done. In this case,
   * the status of the engine changes to [`Done] or [`Error].
   * If not [ignore_error_code], failed jobs are reported by the
   * error status [`Error Subprocess_failure]. If [ignore_error_code],
   * failed jobs are ignored, and result in the status [`Done]
   * (however, jobs terminated by signals are still reported as
   * errors).
   *
   * For the other arguments see {!Shell.call}.
   *
   * When the engine goes to [`Error] or [`Aborted], the job
   * is terminated ( {!Shell_sys.abandon_job} ).
   *)

(** {1 Examples} 
 *
 * All examples presented for the {!Shell} module can be easily rewritten
 * for [Shell_uq]. For example, to call "ls" and collect the result in a
 * buffer, use this piece of code:
 *
 * {[
 * let ues = Unixqueue.create_unix_event_system();;
 * let b = Buffer.create 10;;
 * let e = new call_engine ~stdout:(Shell.to_buffer b) [ Shell.command "ls" ];;
 * Unixqueue.run ues;;
 * let r = Buffer.contents b;;
 * ]}
 *
 * This means, one simply creates a [call_engine] instead of invoking
 * {!Shell.call}, and then runs the queue. Note that you must look at
 * [e#state] to find out whether the engine [e] produced an error, no
 * exception is raised in this case.
 *
 * It is allowed (and somehow the purpose of this module) to create
 * several job engines, and let them run in parallel.
 *)


