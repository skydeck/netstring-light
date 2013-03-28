(* $Id: equeue.ml 1529 2011-01-04 01:37:10Z gerd $
 *
 * Event queues
 * written by Gerd Stolpmann
 *)

open Printf;;

type 'a t =
 { mutable queue : 'a Queue.t;            (* The queue of unprocessed events *)
   mutable new_queue : 'a Queue.t;        (* new events *)
   mutable error_queue : 'a Queue.t;      (* re-scheduled events*)
   mutable handlers : ('a hdl option ref) Queue.t;     (* The list of event handlers - if None, the handler is terminated *)
   mutable live_handlers : int;
   mutable new_handlers : ('a hdl option ref) Queue.t; (* New handlers *)
   mutable source : 'a t -> unit;         (* The default source of events *)
   string_of_event : 'a -> string;        (* A printer for debugging purposes *)
   mutable running : bool;
 }

and 'a hdl = 'a t -> 'a -> unit           (* The type of handlers *)
;;

exception Reject;;          (* Event handler rejects an event *)
exception Terminate;;       (* Event handler removes itself from the list *)
exception Out_of_handlers;; (* There is an event  but no event handler *)

let create ?(string_of_event = fun _ -> "<abstr>") source =
  { queue        = Queue.create();   (* main event queue *)
    new_queue    = Queue.create(); 
    error_queue  = Queue.create(); 
    handlers     = Queue.create();
    new_handlers = Queue.create();
    live_handlers = 0;
    source       = source;
    string_of_event = string_of_event;
    running      = false;
  }
;;


module Debug = struct
  type debug_target = [ `Any | `Process of int | `Thread of int ]

  let enable = ref false
  let target = ref `Any

  let set_debug_mode flag =
    enable := flag

  let set_debug_target t =
    target := t

  let test_debug_target t =
    match t with
      | `Any -> true
      | `Process pid -> Unix.getpid() = pid
      | `Thread id -> 
	  !Netsys_oothr.provider # self # id = id

end

let dlog0 = Netlog.Debug.mk_dlog "Equeue" Debug.enable
let dlogr0 = Netlog.Debug.mk_dlogr "Equeue" Debug.enable

let dlog m =
  if Debug.test_debug_target !Debug.target then dlog0 m

let dlogr gm =
  if Debug.test_debug_target !Debug.target then dlogr0 gm

let () =
  Netlog.Debug.register_module "Equeue" Debug.enable



let add_handler esys h =
  if !Debug.enable then
    dlogr (fun () -> "add_handler");
  Queue.push (ref (Some h)) esys.new_handlers
;;


let add_event esys e =
  if !Debug.enable then
    dlogr (fun () -> sprintf "add_event <event: %s>" (esys.string_of_event e));
  Queue.push e esys.new_queue
;;


let run esys =
  if esys.running then
    failwith "Equeue.run: Already running";
  if !Debug.enable then
    dlogr (fun () -> "run <starting>");
  esys.running <- true;
  try
    if Queue.is_empty esys.queue then (
      if Queue.is_empty esys.new_queue && Queue.is_empty esys.error_queue then (
	(* try to get new events *)
	if !Debug.enable then
	  dlogr (fun () -> "run <invoking source>");
	esys.source esys;
      );
      (* schedule new events or events again that previously caused errors *)
      dlogr (fun () -> "run <reloading queue>");
      Queue.transfer esys.new_queue esys.queue;
      Queue.transfer esys.error_queue esys.queue;
    );
    while not (Queue.is_empty esys.queue) do
      if !Debug.enable then
	dlogr
	  (fun () ->
	     let n = Queue.length esys.queue in
	     let qs = 
	       String.concat "; " 
		 (Queue.fold
		    (fun acc e ->
		       esys.string_of_event e :: acc)
		    []
		    esys.queue) in
	     sprintf "run <queue has %d events, 1st will be processed: %s>" n qs
	  );

      if 2 * esys.live_handlers < Queue.length esys.handlers then (
	if !Debug.enable then
	  dlogr (fun () -> "run <garbage collecting handlers>");
	let handlers' = Queue.create() in
	Queue.iter
	  (fun h ->
	     match !h with
	       | None -> ()
	       | Some hf ->
		   Queue.push h handlers'
	  )
	  esys.handlers;
	esys.handlers <- handlers';
	esys.live_handlers <- Queue.length handlers'
      );
      let l_new_handlers = Queue.length esys.new_handlers in
      if l_new_handlers > 0 then (
	Queue.transfer esys.new_handlers esys.handlers;
	esys.live_handlers <- esys.live_handlers + l_new_handlers;
	if !Debug.enable then
	  dlogr (fun () -> 
		   sprintf "run <considering %d new handlers>" l_new_handlers)
      );
     
      let e = Queue.take esys.queue in
      let accept = ref false in
      (* dlogr (fun () -> (sprintf "run <%d event handlers>" (List.length esys.handlers))); *)
      (* Printing the handlers does not make sense; e.g. Unixqueue only adds one global handler *)
      (* Exceptions occuring in 'h' have to be done: 
       * - The exception is propagated up
       * - The event is moved to the end of the queue
       * - If 'run' is called again, the event is scheduled again
       *)
      ( try
	  Queue.iter
	    (fun h ->
	       match !h with
		 | None -> ()   (* terminated handler *)
		 | Some hf ->
		     ( if not !accept then (
			 try
			   hf esys e;
			   accept := true
			 with
			   | Reject ->
			       ()
			   | Terminate ->
			       accept := true;
			       h := None;
			       esys.live_handlers <- esys.live_handlers - 1;
			       if !Debug.enable then
				 dlogr 
				   (fun () -> 
				      sprintf "run <got Terminate #handlers=%d>"
					esys.live_handlers);
		       )
		     )
	    )
	    esys.handlers;
	  if !Debug.enable then
	    dlogr (fun () -> 
		     sprintf "run <event %s: %s>" 
		       (esys.string_of_event e) 
		       (if !accept then "accepted" else "dropped"));
	with
	  | error ->
	      Queue.push e esys.error_queue;
	      if !Debug.enable then
		dlogr (fun () -> (sprintf "run <event %s: exception %s>"
				    (esys.string_of_event e)
				    (Netexn.to_string error)));
	      raise error
      );
      if Queue.is_empty esys.queue then (
	if (Queue.is_empty esys.new_queue && Queue.is_empty esys.error_queue)
	then (
	  (* try to get new events (or handlers!) *)
	  if !Debug.enable then
	    dlogr (fun () -> "run <invoking source>");
	  esys.source esys;
	  if (not (Queue.is_empty esys.new_queue) && 
              Queue.is_empty esys.handlers && 
	      Queue.is_empty esys.new_handlers
	     )
	  then (
	    if !Debug.enable then
	      dlogr (fun () -> "run <out of handlers>");
	    raise Out_of_handlers
	  )
	    (* If there are new events there must also be (new) handlers.
   	     * Otherwise the program would loop infinitely.
             *)
	);

	(* schedule new events or events again that previously caused errors *)
	if !Debug.enable then
	  dlogr (fun () -> "run <reloading queue>");
	Queue.transfer esys.new_queue esys.queue;
	Queue.transfer esys.error_queue esys.queue;
      )
    done;
    if !Debug.enable then
      dlogr (fun () -> "run <returning normally>");
    esys.running <- false;
  with
      any ->
	if !Debug.enable then
	  dlogr (fun () -> (sprintf "run <returning with exception %s>"
			      (Netexn.to_string any)));
	esys.running <- false;
	raise any
;;


let is_running esys = esys.running
