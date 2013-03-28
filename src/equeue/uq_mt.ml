(* $Id: uq_mt.ml 1707 2012-02-15 22:49:54Z gerd $ *)

open Uq_engines.Operators

module Cond = struct
  type t = Netsys_oothr.condition
  let compare = compare
end


module CondSet = Set.Make(Cond)


type monitor =
    { esys : Unixqueue.event_system;
      mutable esys_running : bool;
      mutable waiters : CondSet.t;
        (* All threads waiting for an engine to complete have their own
	   condition variable. The variable is signalled when the engine
	   is finished (unless it is this thread that runs the esys).
	   We collect all these variables for another purpose, though.
	   When the current esys runner is done, another thread must be
	   woken up. We just take one condition from this set. 
	   The alternative would be to use a single condition variable
	   for all conditions. This is simpler, but the downside is that
	   we need then to broadcast it from time to time.
	 *)
      mutex : Netsys_oothr.mutex;
    }

exception Esys_exit


let create_monitor esys =
  { esys = esys;
    esys_running = false;
    waiters = CondSet.empty;
    mutex = !Netsys_oothr.provider # create_mutex()
  }


let monitor_run mon f arg =
  let cond = !Netsys_oothr.provider # create_condition() in
  let result = ref None in
  let esys_owner = ref false in
  let g = Unixqueue.new_group mon.esys in

  Unixqueue.once mon.esys g 0.0
    (fun () ->
       let inner_e =
	 try f mon.esys arg
	 with error -> eps_e (`Error error) mon.esys in
       ignore(
	 inner_e
	 >> (fun st ->
	       mon.mutex # lock();
	       result := Some st;
	       if !esys_owner then
		 Unixqueue.once mon.esys g 0.0 (fun () -> raise Esys_exit)
	       else
		 cond # signal();
	       mon.mutex # unlock();
	       `Done ()
	    )
       )
    );

  mon.mutex # lock();
  while !result = None do
    while !result = None && mon.esys_running do
      mon.waiters <- CondSet.add cond mon.waiters;
      cond # wait mon.mutex;
      mon.waiters <- CondSet.remove cond mon.waiters;
    done;
    if !result = None then (
      mon.esys_running <- true;
      esys_owner := true;
      mon.mutex # unlock();
      ( try
	  Unixqueue.run mon.esys
	with
	  | Esys_exit -> ()
	  | error ->
	      Netlog.logf `Crit
		"Uq_mt.monitor: caught exception: %s"
		(Netexn.to_string error)
      );
      mon.mutex # lock();
      esys_owner := false;
      mon.esys_running <- false
    )
  done;
  if not mon.esys_running && mon.waiters <> CondSet.empty then
    (CondSet.choose mon.waiters) # signal();
  mon.mutex # unlock();

  match !result with
    | Some(`Done x) -> x
    | Some(`Error e) -> raise e
    | Some(`Aborted) -> failwith "Uq_mt.monitor: aborted"
    | None -> assert false


let monitor_do mon f arg =
  monitor_run mon 
    (fun esys arg -> 
       let result = f arg in 
       eps_e (`Done result) esys
    )
    arg

let monitor_async mon f arg =
  monitor_run mon
    (fun esys arg ->
       let e, signal = Uq_engines.signal_engine esys in
       let emit get_result =
	 signal
	   ( try `Done(get_result())
	     with error -> `Error error
	   ) in
       f arg emit;
       e
    )
    arg
