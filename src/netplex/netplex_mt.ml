(* $Id: netplex_mt.ml 1262 2009-08-31 18:14:21Z gerd $ *)

open Netplex_types

let close_list l =
  List.iter
    (fun fd ->
      ( try
	  Unix.close fd
	with
	  | _ -> ()
      )
    )
    l


class mt () : Netplex_types.parallelizer =
  let oothr = !Netsys_oothr.provider in
object(self)
  method ptype = `Multi_threading

  method init() =
    ()

  method current_sys_id =
    `Thread (oothr # self # id)

  method create_mem_mutex() =
    let m = oothr # create_mutex() in
    (fun () -> m#lock() ), (fun () -> m#unlock() )

  method start_thread : (par_thread -> unit) -> 'x -> 'y -> string -> logger -> par_thread =
    fun f l_close l_share srv_name logger ->
      let throbj t =
	( object
	    method ptype = `Multi_threading
	    method sys_id = `Thread (t#id)
	    method info_string = "Thread " ^ string_of_int (t#id)
	    method watch_shutdown _ =
	      (* We cannot do anything here to ensure the thread is really dead *)
	      ()
	    method parallelizer = (self : #parallelizer :> parallelizer)
	  end
	) in
      let m = oothr # create_mutex() in
      let c = oothr # create_condition() in
      let t = 
	oothr # create_thread 
	  (fun () ->
	     try
	       let o = throbj (oothr # self) in
	       close_list l_close;
	       c # signal();
	       f o
	     with
	       | e ->
		   (* cannot do much better here: *)
		   prerr_endline
		     ("Killed thread " ^ string_of_int oothr#self#id ^ 
			" on exception: "  ^ Netexn.to_string e)
	  ) 
	  () in
      m # lock();
      c # wait m;
      m # unlock();
      throbj t
end


let the_mt = lazy(
  let par = new mt() in
  Netplex_cenv.register_par par;
  par
)

let mt() = Lazy.force the_mt
