(* $Id: shell_uq.ml 50 2004-10-03 17:06:28Z gerd $ *)

open Shell_sys
open Uq_engines

class type ['t] job_handler_engine_type = object
  inherit ['t] engine
  method job : Shell_sys.job
  method job_instance : Shell_sys.job_instance
end


class call_engine ?ignore_error_code ?mode ?stdin ?stdout ?stderr cmds ues =
  let (job, fdlist) = Shell.setup_job ?stdin ?stdout ?stderr cmds in
  let ji = run_job ?mode job in
  let close_fdlist() = List.iter Unix.close fdlist in
  let eng = new job_engine ues ji in
object(self)
  inherit
    [unit, job_status] map_engine
      ~map_done:(fun _ ->
		   close_fdlist();
		   try
		     Shell.postprocess_job ?ignore_error_code ji;
		     `Done (job_status ji)
		   with
		       Shell.Subprocess_error _ as error ->
			 `Error error
		)
      ~map_error:(fun error ->
		    close_fdlist();
		    Shell_sys.cancel_job ji;
		    `Error error)
      ~map_aborted:(fun () ->
		      close_fdlist();
		      Shell_sys.cancel_job ji;
		      `Aborted)
      (eng :> unit engine)

  method job = job
  method job_instance = ji
end
;;
