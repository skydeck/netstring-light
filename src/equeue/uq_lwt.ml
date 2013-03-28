(* $Id: uq_lwt.ml 1616 2011-06-10 15:08:57Z gerd $ *)

open Printf

let fd_of_op =
  function
    | Unixqueue.Wait_in fd -> fd
    | Unixqueue.Wait_out fd -> fd
    | _ -> assert false

exception Esys_exit

class lwt_backend (esys:Unixqueue.event_system) =
  let g = Unixqueue.new_group esys in
  let rd_ht = Hashtbl.create 5 in
  let wr_ht = Hashtbl.create 5 in
  let timers = Hashtbl.create 5 in

  let add ht fd x =
    let l =
      try Hashtbl.find ht fd with Not_found -> [] in
    let l' =
      x :: l in
    Hashtbl.replace ht fd l' in

  let add_res ht op =
    let fd = fd_of_op op in
    if Hashtbl.mem ht fd then
      Unixqueue.add_resource esys g (op, (-1.0)) in

  let remove ht fd x =
    let l =
      try Hashtbl.find ht fd with Not_found -> [] in
    let l' =
      List.filter (fun y -> y != x) l in
    if l' = [] then
      Hashtbl.remove ht fd
    else
      Hashtbl.replace ht fd l' in

  let remove_res ht op =
    let fd = fd_of_op op in
    if not(Hashtbl.mem ht fd) then
      Unixqueue.remove_resource esys g op in


object(self)

  initializer (
    Unixqueue.add_handler esys g self#handler
  )

  method private handler _ _ ev =
    let l =
      match ev with
	| Unixqueue.Input_arrived(_,fd) ->
	    ( try Hashtbl.find rd_ht fd with Not_found -> [] )
	| Unixqueue.Output_readiness(_,fd) ->
	    ( try Hashtbl.find wr_ht fd with Not_found -> [] )
	| _ ->
	    raise Equeue.Reject in
    let l1 = List.rev l in
    match l1 with
      | [] -> ()
      | [f] -> f()
      | _ ->
	  (* Run from the event queue, so exceptions are separately handled *)
	  List.iter
	    (fun f -> Unixqueue.once esys g 0.0 f)
	    l1


  method private register_readable fd f =
    (* eprintf "+reg_rd fd=%Ld\n%!" (Netsys.int64_of_file_descr fd); *)
    let op = Unixqueue.Wait_in fd in
    add rd_ht fd f;
    add_res rd_ht op;
    lazy (
      (* eprintf "-reg_rd fd=%Ld\n%!" (Netsys.int64_of_file_descr fd); *)
      remove rd_ht fd f; remove_res rd_ht op)

  method private register_writable fd f =
    (* eprintf "+reg_wr fd=%Ld\n%!" (Netsys.int64_of_file_descr fd);*)
    let op = Unixqueue.Wait_out fd in
    add wr_ht fd f;
    add_res wr_ht op;
    lazy (
      (* eprintf "-reg_wr fd=%Ld\n%!" (Netsys.int64_of_file_descr fd);*)
      remove wr_ht fd f; remove_res wr_ht op)

  method private register_timer tmo repeat_flag f =
    let tg = Unixqueue.new_group esys in
    let rec loop() =
      Unixqueue.once esys tg tmo 
	(fun () -> 
	   if repeat_flag then
	     loop()
	   else
	     Hashtbl.remove timers tg;
	   f()
	) in
    loop();
    Hashtbl.add timers tg ();
    lazy (Unixqueue.clear esys tg; Hashtbl.remove timers tg)

  method private cleanup =
    Unixqueue.clear esys g;
    Hashtbl.iter (fun tg _ -> Unixqueue.clear esys tg) timers;
    Hashtbl.clear rd_ht;
    Hashtbl.clear wr_ht;
    Hashtbl.clear timers

  method iter block =
    let n = if block then 2 else 1 in
    let k = ref 0 in

    try
      esys # when_blocking
	(fun () ->
	   incr k;
	   if !k >= n then (
	     esys # when_blocking (fun () -> ());
	     raise Esys_exit
	   )
	);
      Unixqueue.run esys
    with
      | Esys_exit -> ()

end
