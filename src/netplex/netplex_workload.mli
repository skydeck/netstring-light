(* $Id: netplex_workload.mli 1718 2012-02-21 14:59:45Z gerd $ *)

(** Workload management
  *
  * Workload managers control when additional containers are started or
  * idle containers are stopped. See {!Netplex_intro.workload} for 
  * additional documentation how they are configured.
 *)

open Netplex_types

val create_constant_workload_manager : 
      ?restart:bool -> ?greedy_accepts:bool -> ?max_jobs_per_thread:int ->
      int -> workload_manager
  (** A constant number of threads is created (the int argument). 
    *
    * [restart]: If threads
    * crash, new threads are created until the specified number is again
    * reached. This is on by default.
    *
    * [greedy_accepts]: whether greedy accepts are allowed (default: false)
    *
    * [max_jobs_per_thread]: if passed, limits the number of jobs (connections)
    * that can be simultaneously processed by a thread/process. By default
    * there is no limit.
   *)

val constant_workload_manager_factory : workload_manager_factory
  (** Reads a workload_manager section like
    *
    * {[ workload_manager {
    *      type = "constant";
    *      threads = <n>;
    *      max_jobs_per_thread = <n>;
    *      greedy_accepts = <bool>;
    *    }
    * ]}
   *)

class type dynamic_workload_config =
object
  method max_jobs_per_thread : int
    (** How many jobs every thread can execute concurrently until it is
      * considered as fully loaded. For configurations where the threads
      * can only handle one connection at a time this number must be 1.
     *)

  method recommended_jobs_per_thread : int
    (** The number of jobs every thread can execute with normal
      * service quality. Must be less than or equal to
      * [max_jobs_per_thread]
     *)

  method min_free_job_capacity : int
    (** The manager starts as many threads as required to ensure that this
      * number of jobs can be executed. Must be at least 1.
     *)

  method max_free_job_capacity : int
    (** If more job capacity is available than this number, threads are
      * terminated. Must be greater than or equal to 
      * [min_free_job_capacity].
     *)

  method inactivity_timeout : int
    (** After this number of seconds a free thread can be terminated *)

  method max_threads : int
    (** The manager does not start more threads than this number *)

  method greedy_accepts : bool
    (** Whether greedy accepts are permitted *)

end


val create_dynamic_workload_manager :
      dynamic_workload_config -> workload_manager

val dynamic_workload_manager_factory : workload_manager_factory
  (** Reads a workload_manager section like
    *
    * {[ workload_manager {
    *      type = "dynamic";
    *      max_jobs_per_thread = <n>;
    *      min_free_jobs_capacity = <n>;
    *      max_free_jobs_capacity = <n>;
    *      max_threads = <n>;
    *      greedy_accepts = <bool>;
    *    }
    * ]}
   *)

val workload_manager_factories : workload_manager_factory list
  (** All built-in workload manager factories *)
