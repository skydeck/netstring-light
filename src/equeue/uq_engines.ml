(* 
 * $Id: uq_engines.ml 1800 2012-09-30 12:21:22Z gerd $
 *)

open Printf


exception Closed_channel
exception Broken_communication
exception Addressing_method_not_supported
exception Watchdog_timeout
exception Cancelled
exception Timeout

class type async_out_channel = object
  method output : string -> int -> int -> int
  method close_out : unit -> unit
  method pos_out : int
  method flush : unit -> unit
  method can_output : bool
  method request_notification : (unit -> bool) -> unit
end
;;


class type async_in_channel = object
  method input : string -> int -> int -> int
  method close_in : unit -> unit
  method pos_in : int
  method can_input : bool
  method request_notification : (unit -> bool) -> unit
end
;;


type 't engine_state =
  [ `Working of int
  | `Done of 't
  | `Error of exn
  | `Aborted
  ]
;;

type 't final_state =
  [ `Done of 't
  | `Error of exn
  | `Aborted
  ]

class type [ 't ] engine = object
  method state : 't engine_state
  method abort : unit -> unit
  method request_notification : (unit -> bool) -> unit
  method event_system : Unixqueue.event_system
end
;;


class type async_out_channel_engine = object
  inherit [ unit ] engine
  inherit async_out_channel
end
;;


class type async_in_channel_engine = object
  inherit [ unit ] engine
  inherit async_in_channel
end
;;


class type ['a] serializer_t =
object
  method serialized : (Unixqueue.event_system -> 'a engine) -> 'a engine
end


class type ['a] prioritizer_t =
object
  method prioritized : (Unixqueue.event_system -> 'a engine) -> int -> 'a engine
end


class type ['a] cache_t =
object
  method get_engine : unit -> 'a engine
  method get_opt : unit -> 'a option
  method put : 'a -> unit
  method invalidate : unit -> unit
  method abort : unit -> unit
end


module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Uq_engines" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Uq_engines" Debug.enable

let () =
  Netlog.Debug.register_module "Uq_engines" Debug.enable


let string_of_state =
  function
    | `Working n -> "Working(" ^ string_of_int n ^ ")"
    | `Done v -> "Done(_)"
    | `Error e -> "Error(" ^ Netexn.to_string e ^ ")"
    | `Aborted -> "Aborted"

let is_active state =
  match state with
      `Working _ -> true
    | _          -> false
;;

module IntSet =
  Set.Make
    (struct
       type t = int
       let compare (x:t) (y:t) = Pervasives.compare x y
     end
    )


class [ 't ] engine_mixin_i (state : 't engine_state ref) esys =
  let notify_list = ref [] in
  let notify_list_new = ref [] in
object(self)
  method state = !state

  method event_system = esys

  method request_notification f =
    if (not (is_active !state)) then
      dlog "engine_mixin warning: the method request_notification was called \
            when the engine already reached the final state";
    notify_list_new := f :: !notify_list_new
    
  method private set_state s =
    if is_active !state then (
      state := s;
      self # notify();
    )

  method private notify() =
    ( match !notify_list_new with
	| [] -> ()
	| n -> 
	    notify_list := !notify_list @ n;
	    notify_list_new := [];
    );
    (* Optimize the case that we only have 1 element in the list. The
       expensive part here is the assignment (calls caml_modify).
     *)
    ( match !notify_list with
	| [] -> ()
	| [ f ] ->
	    let keep =
	      try
		f()
	      with
		| error ->
		    Unixqueue.epsilon esys (fun () -> raise error);
		    false in
	    if not keep then
	      notify_list := []
	| _ ->
	    notify_list := (
	      List.filter
		(fun f ->
		   try
		     f()
		   with
		     | error ->
			 Unixqueue.epsilon esys (fun () -> raise error);
			 false
		)
		!notify_list
	    )
    )
end ;;


class [ 't ] engine_mixin init_state esys =
  ['t] engine_mixin_i (ref init_state) esys



let when_state ?(is_done = fun _ -> ())
               ?(is_error = fun _ -> ())
	       ?(is_aborted = fun _ -> ())
	       ?(is_progressing = fun _ -> ())
	       (eng : 'a #engine) =
  (* Execute is_done when the state of eng goes to `Done,
   * execute is_error when the state goes to `Error, and
   * execute is_aborted when the state goes to `Aborted.
   * The argument of the callback function is the argument
   * of the state value.
   *)
  let last_n = 
    match eng#state with
      | `Done _ | `Error _ | `Aborted -> ref 0 
      | `Working n -> ref n in
  eng # request_notification
    (fun () ->
       match eng#state with
	   `Done v    -> is_done v; false
	 | `Error x   -> is_error x; false
	 | `Aborted   -> is_aborted(); false
	 | `Working n -> 
	     if n <> !last_n then is_progressing n;
	     last_n := n;
	     true
    )
;;


class ['a,'b] map_engine ~map_done ?map_error ?map_aborted 
              ?(propagate_working = true)
              (eng : 'a #engine) =
  let map_state eng_state =
    match eng_state with
	(`Working _ as wrk_state) -> 
	  wrk_state
      | `Done x -> 
	    map_done x
      | (`Error x as err_state) ->
	  ( match map_error with
		Some f -> f x
	      | None   -> err_state
	  )
      | `Aborted ->
	  ( match map_aborted with
		Some f -> f ()
	      | None   -> `Aborted
	  ) in

object(self)
  inherit ['b] engine_mixin (map_state eng#state) eng#event_system

  initializer
    if is_active eng#state then
      eng # request_notification self#map_forward_notification;

  method private map_forward_notification() =
    (* This method is called when [eng] changes its state. We compute our
     * mapped state, and notify our own listeners.
     *)
    let eng_state = eng#state in
    let state' = map_state eng_state in
    let cont =
      match state' with
	  (`Working _) -> true
	| (`Done _)
	| (`Error _)
	| `Aborted ->     false in
    if not cont || propagate_working then
      self # set_state state';
    cont


  method event_system = eng#event_system

  method abort() = 
    eng#abort()
end;;


let map_engine = new map_engine


class ['a,'b] fmap_engine e (f : 'a final_state -> 'b final_state) =
  [_,_] map_engine
    ~map_done:(fun x -> (f (`Done x) :> 'b engine_state))
    ~map_error:(fun e -> (f (`Error e) :> 'b engine_state))
    ~map_aborted:(fun () -> (f `Aborted :> 'b engine_state))
    e

let fmap_engine = new fmap_engine

class ['a] meta_engine e =
  ['a,'a final_state] map_engine
    ~map_done:(fun x -> `Done (`Done x))
    ~map_error:(fun e -> `Done (`Error e))
    ~map_aborted:(fun () -> `Done `Aborted)
    e

let meta_engine = new meta_engine


let const_engine st esys =
  ( object (self)
      inherit [_] engine_mixin st esys
      method abort() = ()
    end
  )

let aborted_engine esys =
  const_engine `Aborted esys


class ['a,'b] seq_engine (eng_a : 'a #engine)
                         (make_b : 'a -> 'b #engine) =
  let esys = eng_a # event_system in
  let eng_a_state = ref (eng_a # state) in
  let eng_a = ref (Some (eng_a :> 'a engine)) in
  (* to get rid of the eng_a value when it is done *)

  let eng_b = ref None in
  let eng_b_state = ref (`Working 0) in

object(self)
  inherit ['b] engine_mixin (`Working 0) esys 

  initializer
    match !eng_a with
      | Some e ->
	  if is_active e#state then
	    e # request_notification self#update_a
	  else (
	    (* eng_a is already in a final state *)
	    ignore(self#update_a())
	  )
      | None -> assert false

  method private update_a() =
    (* eng_a is running, eng_b not yet existing *)
    let ea =
      match !eng_a with Some e -> e | None -> assert false in
    let s = ea # state in
    match s with
      |	`Working n ->
	  ( match !eng_a_state with
	      | `Working n' when n = n' -> ()
	      | _ ->   (* i.e. s <> !eng_a_state *)
		  self # seq_count();
		  eng_a_state := s
	  );
	  true
      | `Done arg ->
	  (* Create eng_b *)
	  (* get rid of eng_a - otherwise mem leak: *)
	  eng_a := None;
	  let e = 
	    try (make_b arg :> _ engine)
	    with error ->
	      const_engine (`Error error) esys in
	  eng_b := Some e;
	  let s' = e # state in
	  eng_b_state := s';
	  self # seq_count();
	  if is_active s' then
	    e # request_notification self#update_b
	  else
	    ignore(self#update_b());
	  false
      | `Error arg ->
	  self # set_state (`Error arg);
	  false
      | `Aborted ->
	  self # set_state `Aborted;
	  false

  method private update_b() =
    (* eng_a is `Done, eng_b is running *)
    let e = match !eng_b with Some e -> e | None -> assert false in
    let s = e # state in
    match s with
      | `Working n ->
	  ( match !eng_b_state with
	      | `Working n' when n=n' -> ()
	      | _ ->
		  self # seq_count();
		  eng_b_state := s
	  );
	  true
      | `Done arg ->
	  self # set_state s;
	  false
      | `Error arg ->
	  self # set_state s;
	  false
      | `Aborted ->
	  self # set_state s;
	  false

  method private seq_count() =
    match self#state with
      | `Working n ->
	  self # set_state (`Working (n+1))
      | _ ->
	  assert false

  method abort() =
    ( match !eng_a with
	| Some e -> 
	    e # abort()
	| None -> ()
    );
    ( match !eng_b with
	| Some e -> 
	    e # abort()
	| None -> ()
    )
end;;


let seq_engine = new seq_engine


class ['a] delegate_engine e =
object(self)
  inherit ['a] engine_mixin e#state e#event_system

  initializer (
    if is_active e#state then 
      when_state
	~is_done:(fun x -> self # set_state (`Done x))
	~is_error:(fun e -> self # set_state (`Error e))
	~is_aborted:(fun () -> self # set_state `Aborted)
	~is_progressing:(fun n -> self # set_state (`Working n))
	e
  )

  method abort() =
    e#abort();
    self # set_state `Aborted
end



class ['a] stream_seq_engine x0 (s : ('a -> 'a #engine) Stream.t)  esys =
object(self)
  inherit ['a] engine_mixin (`Working 0) esys

  val mutable x = x0
  val mutable cur_e = aborted_engine esys

  initializer
    self#next()

  method private next() =
    match Stream.peek s with
      | None ->
	  self # set_state (`Done x)
      | Some f ->
	  let _ = Stream.next s in  (* yep, it's "partial" *)
	  let e =
	    try (f x :> _ engine)
	    with error -> const_engine (`Error error) esys in
	  cur_e <- e;
	  if is_active e#state then
	    when_state
	      ~is_done:(fun x1 -> 
			  x <- x1;
			  Unixqueue.epsilon esys self#next
			    (* avoids stack overflow *)
		       )
	      ~is_error:(fun e -> self # set_state (`Error e))
	      ~is_aborted:(fun () -> self # set_state `Aborted)
	      ~is_progressing:(fun _ -> self # sseq_count())
	      e
	  else
	    self # set_state e#state

  method abort() =
    cur_e # abort();
    self # set_state `Aborted

  method private sseq_count() =
    match self#state with
	`Working n ->
	  self # set_state (`Working (n+1))
      | _ ->
	  ()
end


let stream_seq_engine = new stream_seq_engine



let abort_if_working eng =
  match eng#state with
      `Working _ ->
	eng # abort()
    | _ ->
	()
;;


class ['a,'b] sync_engine (eng_a : 'a #engine) (eng_b : 'b #engine) =
object(self)

  val mutable eng_a_state = eng_a # state

  val mutable eng_b_state = eng_b # state

  inherit ['a * 'b] engine_mixin (`Working 0) eng_a#event_system


  initializer
    if is_active eng_a#state then
      eng_a # request_notification self#sy_update_a
    else
      ignore(self#sy_update_a());
    if is_active eng_b#state then
      eng_b # request_notification self#sy_update_b
    else
      ignore(self#sy_update_b())

  method private sy_update_a() =
    let s = eng_a # state in
    match s with
      |	`Working n ->
	  if s <> eng_a_state then self # transition();
	  eng_a_state <- s;
	  true
      | `Done _ ->
	  eng_a_state <- s;
	  self # transition();
	  false
      | _ ->
	  eng_a_state <- s;
	  self # transition();
	  abort_if_working eng_b;
	  false

  method private sy_update_b() =
    let s = eng_b # state in
    match s with
      | `Working n ->
	  if s <> eng_b_state then self # transition();
	  eng_b_state <- s;
	  true
      | `Done _ ->
	  eng_b_state <- s;
	  self # transition();
	  false
      | _ ->
	  eng_b_state <- s;
	  self # transition();
	  abort_if_working eng_a;
	  false

  method private transition() =
    (* Compute new state from eng_a_state and eng_b_state: *)
    let state' =
      match self#state with
	  `Working n ->
	    ( match (eng_a_state, eng_b_state) with
		  (`Working _, `Working _) ->
		    `Working (n+1)
		| (`Working _, `Done _) ->
		    `Working (n+1)
		| (`Done _, `Working _) ->
		    `Working (n+1)
		| (`Done a, `Done b) ->
		    `Done (a,b)
		| (`Error x, _) ->
		    `Error x
		| (_, `Error x) ->
		    `Error x
		| (`Aborted, _) ->
		    `Aborted
		| (_, `Aborted) ->
		    `Aborted
	    )
	| _ ->
	    (* The state will never change again! *)
	    self#state
    in
    self # set_state state'

  method abort() =
    eng_a # abort();
    eng_b # abort();
end;;


let sync_engine = new sync_engine


class ['t] epsilon_engine (target_state:'t engine_state) ues : ['t] engine =
  let aborted = ref false in
object(self)
  inherit ['t] engine_mixin (`Working 0) ues

  initializer (
    Unixqueue.epsilon ues
      (fun () -> 
	 if not !aborted then
	   self # set_state target_state
      )
  )

  method abort() =
    aborted := true;
    self # set_state `Aborted
end

let epsilon_engine = new epsilon_engine



class poll_engine ?(extra_match = fun _ -> false) 
                  oplist ues =
  let state = ref (`Working 0) in
object(self)

  inherit [Unixqueue.event] engine_mixin_i state ues

  val mutable group = Unixqueue.new_group ues

  initializer
    self # restart()


  method group = group

  method restart() =
    group <- Unixqueue.new_group ues;
    state := (`Working 0 : Unixqueue.event engine_state);
      (* N.B. set_state would not work here *)
    (* Define the event handler: *)
    Unixqueue.add_handler ues group (fun _ _ -> self # handle_event);
    (* Add the resources: *)
    List.iter (Unixqueue.add_resource ues group) oplist;


  method private handle_event ev =
    match ev with
	Unixqueue.Input_arrived(g,fd) when g = group ->
	  self # accept_event ev
      | Unixqueue.Output_readiness(g,fd) when g = group ->
	  self # accept_event ev
      | Unixqueue.Out_of_band(g,fd) when g = group ->
	  self # accept_event ev
      | Unixqueue.Timeout(g,op) when g = group ->
	  self # accept_event ev
      | Unixqueue.Extra x ->
	  if extra_match x then
	    self # accept_event ev
	  else
	    raise Equeue.Reject
      | _ ->
	  raise Equeue.Reject


  method private accept_event ev =
    Unixqueue.clear ues group;
    self # set_state (`Done ev);


  method private handle_exception x =
    self # set_state (`Error x)


  method abort() =
    match self#state with
	`Working _ ->
	  Unixqueue.clear ues group;
	  self # set_state `Aborted;
      | _ ->
	  ()

  method event_system = ues

end ;;


class ['a] delay_engine t f esys =
  let wid = Unixqueue.new_wait_id esys in
  [_,'a] seq_engine
    (new poll_engine [ Unixqueue.Wait wid, t ] esys)
    (fun _ -> f())


let delay_engine = new delay_engine

let signal_engine esys =
  let wid = Unixqueue.new_wait_id esys in
  let op = Unixqueue.Wait wid in
  let p = new poll_engine [op, (-1.0)] esys in
  let r = ref `Aborted in
  let flag = ref false in
  let e = new map_engine
            ~map_done:(fun _ -> (!r :> _ engine_state))
	    ~map_aborted:(fun _ -> (!r :> _ engine_state)) p in
  let signal st =
    if not !flag then (   (* atomic *)
      r := st;
      flag := true
    );
    (* p#abort() - old implementation *)
    Unixqueue.add_event esys (Unixqueue.Timeout(p#group, op)) in
  (e, signal)


class ['a] signal_engine esys =
  let (e, signal) = signal_engine esys in
object(self)
  inherit ['a] delegate_engine e
  method signal x = signal (x : _ final_state)
end


let timeout_engine d exn eng =
  let esys = eng#event_system in
  let g = Unixqueue.new_group esys in
  let timeout_flag = ref false in
  Unixqueue.once esys g d 
    (fun () ->
       timeout_flag := true;
       eng#abort();
    );
  map_engine
    ~map_done:(fun r -> Unixqueue.clear esys g; `Done r)
    ~map_aborted:(fun _ -> 
		    if !timeout_flag then `Error exn
		    else ( Unixqueue.clear esys g; `Aborted))
    ~map_error:(fun e -> Unixqueue.clear esys g; `Error e)
    eng


class ['a] timeout_engine d exn (eng : _ engine) =
  ['a] delegate_engine(timeout_engine d exn eng)


class poll_process_engine ?(period = 0.1) ~pid ues =
object(self)

  inherit [Unix.process_status] engine_mixin (`Working 0) ues

  val group = Unixqueue.new_group ues
  val wait_id = Unixqueue.new_wait_id ues

  initializer
    (* Define the event handler: *)
    Unixqueue.add_handler ues group (fun _ _ -> self # handle_event);
    (* Define the abort (exception) handler: *)
    Unixqueue.add_abort_action ues group (fun _ -> self # handle_exception);
    (* Add the resources: *)
    Unixqueue.add_resource ues group (Unixqueue.Wait wait_id, period);


  method private handle_event ev =
    match ev with
	Unixqueue.Timeout(g, Unixqueue.Wait wid) 
	                                   when g = group && wid = wait_id ->
	  self # check_process()
      | Unixqueue.Signal ->
	  self # check_process();
	  raise Equeue.Reject    (* Signal must not be accepted! *)
      | _ ->
	  raise Equeue.Reject


  method private check_process () =
    try
      let (w_pid, w_status) = Unix.waitpid [ Unix.WNOHANG ] pid in
      if w_pid > 0 then (
	Unixqueue.clear ues group;
	self # set_state (`Done w_status);
      )
    with
	error ->
	  raise(Unixqueue.Abort(group,error))


  method private handle_exception x =
    self # set_state (`Error x)


  method abort() =
    match self#state with
	`Working _ ->
	  Unixqueue.clear ues group;
	  self # set_state `Aborted;
      | _ ->
	  ()

  method event_system = ues

end ;;


class watchdog period eng =
  let ues = eng#event_system in
  let wid = Unixqueue.new_wait_id ues in
object (self)
  inherit [unit] engine_mixin (`Working 0) ues

  val mutable last_eng_state = eng # state
  val timer_eng = new poll_engine [ Unixqueue.Wait wid, 0.1 *. period ] ues
  val mutable aborted = false
  val mutable inactivity = 0
			     (* Counts to 10 *)

  initializer
    let rec watch() =
      when_state 
	~is_done:(fun _ ->
		    let eng_state = eng # state in
		    if eng_state = last_eng_state then (
		      inactivity <- inactivity + 1;
		      if inactivity >= 10 then (
			aborted <- true;
			self # set_state (`Error Watchdog_timeout)
		      )
		      else (
			timer_eng # restart();
			watch();
		      )
		    )
		    else (
		      last_eng_state <- eng_state;
		      inactivity <- 0;
		      timer_eng # restart();
		      watch()
		    )
		 )
	timer_eng
    in

    watch();

    when_state
      ~is_done:(fun _ -> if not aborted then self # set_state (`Done()))
      ~is_error:(fun _ -> if not aborted then self # set_state (`Done()))
      ~is_aborted:(fun _ -> if not aborted then self # set_state (`Done()))
      eng


  method abort() =
    match self#state with
	`Working _ ->
	  aborted <- true;
	  timer_eng # abort();
	  self # set_state `Aborted;
      | _ ->
	  ()

  method event_system = 
    ues

end ;;


let watchdog = new watchdog


let rec msync_engine l f x0 esys =
  match l with
    | [] ->
	new epsilon_engine (`Done x0) esys
    | [e] ->
	new map_engine
	  ~map_done:(fun r -> `Done (f r x0))
	  e
    | e1 :: l' ->
	new map_engine
	  ~map_done:(fun (r,x) -> `Done (f r x))
	  (new sync_engine e1 (msync_engine l' f x0 esys))


class ['a,'b] msync_engine (l : 'a #engine list) f (x0:'b) esys = 
  ['b] delegate_engine (msync_engine l f x0 esys)


class ['a] serializer (esys : Unixqueue.event_system) =
object(self)
  val mutable running = None
  val mutable queue = Queue.create()

  method serialized ( f : (Unixqueue.event_system -> 'a engine) ) =
    (** Will call [f esys] when it is time to start the engine *)
    let rec next f signal =
      let e = 
	try (f esys : 'a engine)
	with error -> epsilon_engine (`Error error) esys in
      running <- Some e;
      signal e;
      if is_active e#state then 
	when_state
	  ~is_done:(fun _ -> check())
	  ~is_error:(fun _ -> check())
	  ~is_aborted:(fun _ -> check())
	  e
      else (
	Unixqueue.epsilon esys check;
      );
      e
    and check() =
      running <- None; 
      if not (Queue.is_empty queue) then (
	let (f,signal) = Queue.take queue in
	ignore(next f signal)
      )
    in

    match running with
      | Some _ ->
	  (** Create a wrapper engine. When [f] is finally called, the
              wrapper is terminated and "replaced" by the engine returned
              by [f]
	   *)
	  let sig_e, do_signal = signal_engine esys in
	  let eff_e = ref None in
	  let wrap_e =
	    new seq_engine
	      sig_e
	      (fun _ ->
		 match !eff_e with
		   | None -> assert false
		   | Some e -> e
	      ) in
	  let signal e =
	    eff_e := Some e;
	    do_signal(`Done()) in
	  Queue.push (f,signal) queue;
	  wrap_e
      | None ->
	  next f (fun _ -> ())
end

let serializer = new serializer


class ['a] prioritizer (esys : Unixqueue.event_system) =
object(self)
  val mutable prio = 0       (* priority of [running] engines *)
  val mutable running = 0    (* # running engines *)
  val mutable prios = IntSet.empty        (* all waiting priorities *)
  val mutable preempting = false          (* whether there is a bigger prio in prios *)
  val mutable waiting = Hashtbl.create 3  (* the waiting engines by prio *)

  method prioritized f p =
    let rec next f signal =
      running <- running + 1;
      prio <- p;
      let e = 
	try (f esys : 'a engine)
	with error -> epsilon_engine (`Error error) esys in
      signal e;
      if is_active e#state then 
	when_state
	  ~is_done:(fun _ -> check())
	  ~is_error:(fun _ -> check())
	  ~is_aborted:(fun _ -> check())
	  e
      else (
	Unixqueue.epsilon esys check;
      );
      e

    and check () =
      running <- running - 1;
      if running = 0 && prios <> IntSet.empty then (
	let highest = IntSet.min_elt prios in
	prios <- IntSet.remove highest prios;
	preempting <- false;
	let l = 
	  try Hashtbl.find waiting highest with Not_found -> assert false in
	Hashtbl.remove waiting highest;
	List.iter
	  (fun (f,signal) ->
	     ignore(next f signal)
	  )
	  (List.rev l);
      )
    in

    if running = 0 || prio = p || not preempting then (
      (* we can start immediately *)
      next f (fun _ -> ())
    )
    else (
      (* push f onto the queue *)
      let sig_e, do_signal = signal_engine esys in
      let eff_e = ref None in
      let wrap_e =
	seq_engine
	  sig_e
	  (fun _ ->
	     match !eff_e with
	       | None -> assert false
	       | Some e -> e
	  ) in
      let signal e =
	eff_e := Some e;
	do_signal(`Done()) in
      let l = try Hashtbl.find waiting p with Not_found -> [] in
      Hashtbl.replace waiting p ((f,signal)::l);
      prios <- IntSet.add p prios;
      if p < prio then
	preempting <- true;
      wrap_e
    )
end


let prioritizer = new prioritizer


class ['a] cache call_get_e esys =
object(self)
  val mutable value_opt = (None : 'a option)
  val mutable value_gen = 0
  val mutable getting = None

  method get_opt() = value_opt

  method get_engine() =
    match value_opt with
      | None ->
	  ( match getting with
	      | None ->
		  (* No get engine is running. Start a new one *)
		  let get_e = call_get_e esys in
		  getting <- Some get_e;
		  let gen = value_gen in
		  let get_e' =
		    new map_engine
		      ~map_done:(fun v -> 
				   (* There could be a [put] writing: *)
				   if value_gen = gen then (
				     value_opt <- Some v;
				     value_gen <- gen+1
				   );
				   `Done v
				)
		      get_e in
		  get_e'
	      | Some get_e ->
		  (* Some previous user already called [get] but it was
                     not yet finished. For simplicity we return here the
                     same engine. This is ok except that when one user
		     aborts this engine, all other users are also affected.
		   *)
		  get_e
	)

    | Some v ->
	new epsilon_engine (`Done v) esys

  method put v' =
    value_opt <- Some v';
    value_gen <- value_gen + 1

  method invalidate () =
    value_opt <- None;
    value_gen <- value_gen + 1

  method abort() =
    ( match value_opt with
	| None ->
	    ( match getting with
		| None ->
		    ()
		| Some get_e ->
		    get_e # abort()
	    )
	| Some _ -> ()
    );
    self#invalidate()

end

let cache = new cache


class ['a] input_engine f fd tmo esys =
  [Unixqueue.event, 'a]
  seq_engine
    (new poll_engine [ Unixqueue.Wait_in fd, tmo ] esys)
    (fun ev ->
       match ev with
	 | Unixqueue.Input_arrived(_,_) ->
	     ( try
		 let r = f fd in
		 epsilon_engine (`Done r) esys
	       with
		 | error -> epsilon_engine (`Error error) esys
	     )
	 | Unixqueue.Timeout(_,_) ->
	     epsilon_engine (`Error Timeout) esys
	 | _ ->
	     assert false
    )


class ['a] output_engine f fd tmo esys =
  [Unixqueue.event, 'a]
  seq_engine
    (new poll_engine [ Unixqueue.Wait_out fd, tmo ] esys)
    (fun ev ->
       match ev with
	 | Unixqueue.Output_readiness(_,_) ->
	     ( try
		 let r = f fd in
		 epsilon_engine (`Done r) esys
	       with
		 | error -> epsilon_engine (`Error error) esys
	     )
	 | Unixqueue.Timeout(_,_) ->
	     epsilon_engine (`Error Timeout) esys
	 | _ ->
	     assert false
    )


class pseudo_async_in_channel ch : async_in_channel =
object
  method input = ch # input
  method close_in = ch # close_in
  method pos_in = ch # pos_in
  method can_input = true
  method request_notification _ = ()
end


let pseudo_async_in_channel = new pseudo_async_in_channel



class pseudo_async_out_channel ch : async_out_channel =
object
  method output = ch # output
  method close_out = ch # close_out
  method pos_out = ch # pos_out
  method flush = ch # flush
  method can_output = true
  method request_notification _ = ()
end


let pseudo_async_out_channel = new pseudo_async_out_channel


(* TODO: Avoid the usage of Extra events here. Extra events are more
 * expensive than other events because all handlers see them.
 * Can be substituted with Timeout events.
 *)

exception Receiver_attn of Unixqueue.group ;;
let receiver_attn g = Unixqueue.Extra(Receiver_attn g);;

exception Sender_attn of Unixqueue.group ;;
let sender_attn g = Unixqueue.Extra(Sender_attn g);;


let buf_max_size = 4096;;


class receiver ~src ~(dst : #async_out_channel) ?(close_src=true) 
               ?(close_dst=true) ues 
      : [ unit ] engine = 
object(self)
  (* The receiver has to copy data if (1) the src file descriptor is
   * readable, and (2) the dst channel accepts output. There is also
   * an internal buffer that stored read data that cannot yet be 
   * written into the dst channel.
   *
   * We implement the following logic:
   *
   * - The src file descriptor is polled when there is space in the
   *   internal buffer. Every time new data is added to the buffer,
   *   the event Receiver_attn is generated
   * - When the dst state changes, the event Receiver_attn is generated
   * - The event handler catches Receiver_attn, and checks whether
   *   the output channel is ready. If so, data of the internal
   *   buffer is written to the output channel, and a new Receiver_attn
   *   event is generated. If the output channel is not ready, nothing
   *   will happen.
   *)

  inherit [unit] engine_mixin (`Working 0 : unit engine_state) ues

  val group = Unixqueue.new_group ues

  val buf = String.create buf_max_size
  val mutable buf_size = 0

  val mutable in_eof = false
  val mutable in_polling = false

  val mutable out_eof = false

  val mutable deferred_exn = None

  initializer
    (* Arrange that Receiver_attn is generated when the dst state changes: *)
    dst # request_notification
      (fun () ->
	 (* Note: With MT, we do not know which thread calls this function.
	  * Fortunately, add_event is thread-safe.
	  *)
	 if is_active self#state then (
	   Unixqueue.add_event ues (receiver_attn group);
	   true
	     (* Continue notifications *)
	 )
	 else
	   false
	     (* The engine is no longer active: disable any further
	      * notification
	      *)
      );
    (* Define the event handler: *)
    Unixqueue.add_handler ues group (fun _ _ -> self # handle_event);
    (* Define the abort (exception) handler: *)
    Unixqueue.add_abort_action ues group (fun _ -> self # handle_exception);
    (* Because the internal buffer is empty initially, we can poll
     * src: 
     *)
    Unixqueue.add_resource ues group (Unixqueue.Wait_in src, -1.0);
    in_polling <- true   (* Remember add_resource *)


  method abort() =
    match self#state with
	`Working _ ->
	  if not in_eof && close_src then Unix.close src;
	  in_eof <- true;
	  if not out_eof && close_dst then dst # close_out();
	  out_eof <- true;
	  self # set_state `Aborted;
	  Unixqueue.clear ues group
      | _ ->
	  ()


  method event_system = ues


  method private rcv_count() =
    match self#state with
	`Working n -> 
	  self # set_state (`Working (n+1))
      | _ ->
	  ()


  method private handle_event ev =
    match ev with
	Unixqueue.Input_arrived(g,_) when g = group ->
	  self # handle_input();
	  self # check_input_polling();
      | Unixqueue.Extra (Receiver_attn g) when g = group ->
	  self # handle_output();
	  if out_eof then (
	    Unixqueue.clear ues group;    (* Delete the whole group *)
	    raise Equeue.Terminate        (* Deactivate this handler *)
	  )
      | _ ->
	  raise Equeue.Reject


  method private handle_exception exn =
    (* Unixqueue already ensures that the whole group will be deleted,
     * so we need not to do it here
     *)
    if not in_eof && close_src then (
      try Unix.close src
      with error ->
	Netlog.logf `Err
	  "Uq_engines.receiver#handle_exception: %s" 
	  (Netexn.to_string error) );
    in_eof <- true;
    if not out_eof && close_dst then (
      try dst # close_out()
      with error ->
	Netlog.logf `Err
	  "Uq_engines.receiver#handle_exception: %s" 
	  (Netexn.to_string error) );
    out_eof <- true;
    self # set_state (`Error exn)


  method private handle_input() =
    if not in_eof && buf_size < buf_max_size then
      try
	let n = Unix.read src buf buf_size (buf_max_size - buf_size) in
	buf_size <- buf_size + n;
	in_eof <- (n = 0);
	if in_eof && close_src then Unix.close src;
	Unixqueue.add_event ues (receiver_attn group);
	self # rcv_count();
      with
	  Unix.Unix_error(Unix.EAGAIN,_,_)
	| Unix.Unix_error(Unix.EWOULDBLOCK,_,_)
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    (* These exceptions are expected, and can be ignored *)
	    ()
	| error ->
	    (* Any other exception stops the engine. But first it is tried to
	     * process the buffer contents:
	     *)
	    in_eof <- true;
	    deferred_exn <- Some error;
	    if in_eof && close_src then Unix.close src;
	    Unixqueue.add_event ues (receiver_attn group);
	    self # rcv_count();
	    

  method private check_input_polling() =
    let need_polling = not in_eof && buf_size < buf_max_size in
    ( if need_polling && not in_polling then
	Unixqueue.add_resource ues group (Unixqueue.Wait_in src, -1.0)
      else
	if not need_polling && in_polling then
	  Unixqueue.remove_resource ues group (Unixqueue.Wait_in src);
    );
    in_polling <- need_polling


  method private handle_output() =
    (* If this method is called when out_eof, we assume that this is
     * an event coming too late. Just ignore.
     *)
    if not out_eof then (
      (* First check the state of dst: If [pos_out] raises an exception,
       * we assume that the output channel is broken.
       *)
      ( try ignore(dst#pos_out)
	with
	    _ ->
	      (* dst is in an error state, or somebody has closed it *)
	      raise(Unixqueue.Abort(group,Broken_communication))
      );

      (* It is possible that dst#can_output is false, because we get
       * Reciever_attn events for many conditions, not just that
       * output is again accepted. Ignore this case.
       *)
      try
	if dst#can_output then (
	  if buf_size > 0 then (
	    let n = dst # output buf 0 buf_size in
	    if n > 0 then (
	      String.blit buf n buf 0 (buf_size - n);
	      buf_size <- buf_size - n;
	      if (buf_size > 0 && dst#can_output) || in_eof then
		Unixqueue.add_event ues (receiver_attn group);
	      self # check_input_polling();
	      self # rcv_count();
	    )
	  )
	  else if in_eof then (
	    (* Note: we do not close dst. out_eof just means that copying
	     * is done
	     *)
	    if close_dst then dst # close_out();
	    out_eof <- true;
	    ( match deferred_exn with
		| None -> self # set_state (`Done());
		| Some err -> self # set_state (`Error err);
	    )
	  )
	)
      with
	  error ->
	    (* In most cases coming from dst#output *)
	    raise(Unixqueue.Abort(group,error))
    )

end
;;


class sender ~(src : #async_in_channel) ~dst ?(close_src=true) 
               ?(close_dst=true) ues 
      : [ unit ] engine = 
object(self)
  (* The sender has to copy data if (1) the src channel is
   * readable, and (2) the dst descriptor accepts output. There is also
   * an internal buffer that stored read data that cannot yet be 
   * written into the dst descriptor.
   *
   * We implement the following logic:
   *
   * - The dst file descriptor is polled when there is data in the
   *   internal buffer. Every time new data is added to the buffer,
   *   the event Sender_attn is generated
   * - When the src state changes, the event Sender_attn is generated
   * - The event handler catches Sender_attn, and checks whether
   *   the input channel has data. If so, the data is appended to the internal
   *   buffer, and a new Sender_attn
   *   event is generated.
   *)

  inherit [unit] engine_mixin (`Working 0 : unit engine_state) ues

  val group = Unixqueue.new_group ues

  val buf = String.create buf_max_size
  val mutable buf_size = 0

  val mutable in_eof = false

  val mutable out_eof = false
  val mutable out_polling = false


  initializer
    (* Arrange that Sender_attn is generated when the src state changes: *)
    src # request_notification
      (fun () ->
	 (* Note: With MT, we do not know which thread calls this function.
	  * Fortunately, add_event is thread-safe.
	  *)
	 if is_active self#state then (
	   Unixqueue.add_event ues (sender_attn group);
	   true
	     (* Continue notifications *)
	 )
	 else
	   false
	     (* The engine is no longer active: disable any further
	      * notification
	      *)
      );
    (* Define the event handler: *)
    Unixqueue.add_handler ues group (fun _ _ -> self # handle_event);
    (* Define the abort (exception) handler: *)
    Unixqueue.add_abort_action ues group (fun _ -> self # handle_exception);
    (* Because the internal buffer is empty initially, we cannot poll
     * dst. 
     *)
    out_polling <- false;
    (* Immediately check for input: *)
    Unixqueue.add_event ues (sender_attn group);



  method abort() =
    match self#state with
	`Working _ ->
	  if not in_eof && close_src then src # close_in();
	  in_eof <- true;
	  if not out_eof && close_dst then Unix.close dst;
	  out_eof <- true;
	  self # set_state `Aborted;
	  Unixqueue.clear ues group
      | _ ->
	  ()


  method event_system = ues


  method private snd_count() =
    match self#state with
	`Working n -> 
	  self # set_state (`Working (n+1))
      | _ ->
	  ()


  method private handle_event ev =
    match ev with
	Unixqueue.Extra (Sender_attn g) when g = group ->
	  self # handle_input();
      | Unixqueue.Output_readiness(g,_) when g = group ->
	  self # handle_output();
	  self # check_output_polling();
	  if out_eof then (
	    Unixqueue.clear ues group;    (* Delete the whole group *)
	    raise Equeue.Terminate        (* Deactivate this handler *)
	  )
      | _ ->
	  raise Equeue.Reject


  method private handle_exception exn =
    (* Unixqueue already ensures that the whole group will be deleted,
     * so we need not to do it here
     *)
    if not in_eof && close_src then (
      try src # close_in();
      with error ->
	Netlog.logf `Err
	  "Uq_engines.sender#handle_exception: %s" 
	  (Netexn.to_string error) );
    in_eof <- true;
    if not out_eof && close_dst then (
      try Unix.close dst
      with error ->
	Netlog.logf `Err
	  "Uq_engines.sender#handle_exception: %s" 
	  (Netexn.to_string error) );
    out_eof <- true;
    self # set_state (`Error exn)


  method private handle_output() =
    if not out_eof then
      try
	let n = Unix.single_write dst buf 0 buf_size in
	String.blit buf n buf 0 (buf_size - n);
	buf_size <- buf_size - n;
	if buf_size = 0 && in_eof then (
	  out_eof <- true;
	  if close_dst then Unix.close dst;
	  self # set_state (`Done());
	)
	else (
	  self # snd_count();
	  if n > 0 && not in_eof && src#can_input then
	    Unixqueue.add_event ues (sender_attn group);
	  (* if not src#can_input, we will be notified when input is 
	   * again possible.
	   *)
	)
      with
	  Unix.Unix_error(Unix.EAGAIN,_,_)
	| Unix.Unix_error(Unix.EWOULDBLOCK,_,_)
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    (* These exceptions are expected, and can be ignored *)
	    ()
	| error ->
	    (* Any other exception stops the engine *)
	    raise(Unixqueue.Abort(group,error))
	    

  method private check_output_polling() =
    let need_polling = not out_eof && (buf_size > 0 || in_eof) in
    ( if need_polling && not out_polling then
	Unixqueue.add_resource ues group (Unixqueue.Wait_out dst, -1.0)
      else
	if not need_polling && out_polling then
	  Unixqueue.remove_resource ues group (Unixqueue.Wait_out dst);
    );
    out_polling <- need_polling


  method private handle_input() =
    (* If this method is called when in_eof, we assume that this is
     * an event coming too late. Just ignore.
     *)
    if not in_eof then (
      (* First check the state of src: If [pos_in] raises an exception,
       * we assume that the input channel is broken.
       *)
      ( try ignore(src#pos_in)
	with
	    _ ->
	      (* src is in an error state, or somebody has closed it *)
	      raise(Unixqueue.Abort(group,Broken_communication))
      );

      (* It is possible that src#can_input is false, because we get
       * Sender_attn events for many conditions, not just that
       * input data is again available. Ignore this case.
       *)
      try
	if src#can_input then (
	  let l = String.length buf in
	  if buf_size < l then (
	    try
	      let n = src # input buf buf_size (l-buf_size) in
	      if n > 0 then (
		buf_size <- buf_size + n;
		(* Check for more input data immediately: *)
		if buf_size < l then
		  Unixqueue.add_event ues (sender_attn group);
		self # check_output_polling();
		self # snd_count();
	      )
	    with
		End_of_file ->
		  (* We do see EOF for the first time! *)
		  if close_src then src # close_in();
		  in_eof <- true;
		  self # check_output_polling();
		  self # snd_count();
	  )
	)
      with
	  error ->
	    (* In most cases coming from src#input *)
	    raise(Unixqueue.Abort(group,error))
    )

end
;;


exception Mem_not_supported

class type multiplex_controller =
object
  method alive : bool
  method mem_supported : bool
  method event_system : Unixqueue.event_system
  method reading : bool
  method start_reading : 
    ?peek:(unit -> unit) ->
    when_done:(exn option -> int -> unit) -> string -> int -> int -> unit
  method start_mem_reading : 
    ?peek:(unit -> unit) ->
    when_done:(exn option -> int -> unit) -> Netsys_mem.memory -> int -> int ->
    unit
  method cancel_reading : unit -> unit
  method writing : bool
  method start_writing :
    when_done:(exn option -> int -> unit) -> string -> int -> int -> unit
  method start_mem_writing : 
    when_done:(exn option -> int -> unit) -> Netsys_mem.memory -> int -> int ->
    unit
  method supports_half_open_connection : bool
  method start_writing_eof :
    when_done:(exn option -> unit) -> unit -> unit
  method cancel_writing : unit -> unit
  method read_eof : bool
  method wrote_eof : bool
  method shutting_down : bool
  method start_shutting_down :
    ?linger : float ->
    when_done:(exn option -> unit) -> unit -> unit
  method cancel_shutting_down : unit -> unit
  method inactivate : unit -> unit
end


class type datagram_multiplex_controller =
object
  inherit multiplex_controller
  method received_from : Unix.sockaddr
  method send_to : Unix.sockaddr -> unit
end


type onshutdown_out_spec =
    [ `Ignore
    | `Initiate_shutdown
    | `Action of async_out_channel_engine -> multiplex_controller -> 
                   unit engine_state -> unit
    ]

type onshutdown_in_spec =
    [ `Ignore
    | `Initiate_shutdown
    | `Action of async_in_channel_engine -> multiplex_controller -> 
                   unit engine_state -> unit
    ]


type onclose_spec = [ `Ignore | `Write_eof ]

class output_async_mplex ?(onclose = (`Ignore : onclose_spec) )
                         ?(onshutdown = (`Ignore : onshutdown_out_spec) )
                         ?buffer_size
                         (mplex : multiplex_controller)
                         : async_out_channel_engine =
object (self)

  inherit [unit] engine_mixin (`Working 0 : unit engine_state) mplex#event_system

  val data_queue = Queue.create()
		     (* The queue of strings to output *)

  val mutable data_top_pos = 0
		     (* How many bytes of the first string of data_queue
		      * have already been copied to buf.
		      *)

  val mutable data_queue_length = 0
		     (* The sum of all strings in data_queue, not counting
		      * data_top_pos
		      *)

  val buf = String.create buf_max_size
	      (* The output buffer. The strings from data_queue are
	       * appended to this buffer to reduce the number of
	       * Unix.write syscalls
	       *)

  val mutable buf_size = 0
	     (* The number of bytes used at the beginning of [buf]. *)

  val mutable pos_out = 0
	     (* The position of the channel *)

  (* Note that the object buffers the strings in data_queue plus the
   * string in buf, and buffer_size is the limit for 
   * data_queue_length + buf_size
   *)

  val mutable in_eof = false
  val mutable shutdown_done = false

  method output s p l =
    if p < 0 || l < 0 || p > String.length s || p+l > String.length s then
      invalid_arg "Uq.engines.output_async_mplex#output";

    if in_eof then raise Closed_channel;

    let l' =
      match buffer_size with
	  None ->
	    (* Unrestricted buffers *)
	    if l > 0 then Queue.add (String.sub s p l) data_queue;
	    l
	| Some max_size ->
	    let size = data_queue_length + buf_size in
	    let n = min l (max_size - size) in
	    if n > 0 then Queue.add (String.sub s p n) data_queue;
	    n
    in

    pos_out <- pos_out + l';
    data_queue_length <- data_queue_length + l';
    assert(data_queue_length >= 0);  (* must never overflow *)

    if not mplex#writing && l' > 0 then 
      self # check_for_output();

    if l' > 0 then self # oam_count();
    (* If l' = 0, there was no space in the buffer. No need for notification *)

    l'


  method close_out () =
    if not in_eof then (
      in_eof <- true;
      if not mplex#writing then
	self # check_for_output();
    )


  method pos_out =
    if in_eof then raise Closed_channel;
    pos_out


  method flush () = 
    if in_eof then raise Closed_channel;
    ()


  method abort() =
    match self#state with
	`Working _ ->
	  mplex # cancel_writing();
	  self # shutdown `Aborted;
      | _ ->
	  ()

  method event_system = mplex # event_system

  method private oam_count() =
    match self#state with
	`Working n -> 
	  self # set_state (`Working (n+1))
      | _ ->
	  ()

  method can_output =
    not in_eof &&
    match buffer_size with
	None ->
	  (* Unrestricted buffers *)
	  true
      | Some max_size ->
	  let size = data_queue_length + buf_size in
	  size < max_size


  method private handle_exception exn =
    mplex # cancel_writing();
    self # shutdown (`Error exn)


  method private check_for_output() =
    assert(not mplex#writing);
    if not mplex#wrote_eof then (
      (* Refill buf: *)
      while buf_size < buf_max_size && not (Queue.is_empty data_queue) do
	let s0 = Queue.top data_queue in
	let m = String.length s0 - data_top_pos in
	let space = buf_max_size - buf_size in
	let n = min space m in
	String.blit s0 data_top_pos buf buf_size n;
	buf_size <- buf_size + n;
	data_top_pos <- data_top_pos + n;
	data_queue_length <- data_queue_length - n;
	if data_top_pos >= String.length s0 then (
	  ignore(Queue.take data_queue);
	  data_top_pos <- 0
	);
	assert(data_queue_length >= 0);  (* must never overflow *)
	assert(data_top_pos >= 0);       (* must never overflow *)
      done;
      (* Have something to write? *)
      if buf_size > 0 then (
	let cur_buf_size = buf_size in
	mplex # start_writing 
	  ~when_done:(fun exn_opt n ->
			match exn_opt with
			  | None ->
			      assert(buf_size = cur_buf_size);
			      String.blit buf n buf 0 (buf_size - n);
			      buf_size <- buf_size - n;
			      self # check_for_output();
			      if n > 0 then self # oam_count();
			      (* Note: this also implies notification because
                               * [can_output] returns true
			       *)
			  | Some Cancelled ->
			      (* Called from [abort], so ignore any data *)
			      ()
			  | Some error ->
			      self # handle_exception error
		     )
	  buf 0 cur_buf_size;
      )
      else
	if in_eof then (
	  match onclose with
	    | `Write_eof ->
		mplex # start_writing_eof
		  ~when_done:(fun exn_opt ->
				match exn_opt with
				  | None ->
				      self # shutdown (`Done());
				  | Some Cancelled ->
				      ()
				  | Some error ->
				      self # handle_exception error
			     )
		  ()
	    | `Ignore ->
		self # shutdown (`Done());
	)
    )

  method private shutdown next_state =
    (* See also input_async_mplex # shutdown *)
    if not shutdown_done then (
      shutdown_done <- true;
      in_eof <- true;
      Queue.clear data_queue;
      data_queue_length <- 0;
      data_top_pos <- 0;
      ( match onshutdown with
	  | `Ignore -> ()
	  | `Initiate_shutdown ->
	      mplex # start_shutting_down ~when_done:(fun _ -> ()) ()
		(* CHECK: What to do if shutdown not possible? E.g. because
                 * there is also a reader?
                 *)
	  | `Action f ->
	      ( try
		  f
		    (self : #async_out_channel_engine :> async_out_channel_engine)
		    mplex
		    next_state
		with error ->
		  (* CHECK: We could map that also to state Error *)
		  Netlog.logf `Err
		    "Uq_engines.output_async_mplex#shutdown: %s" 
		    (Netexn.to_string error)
	      )
      );
      self # set_state next_state
    )

end
;;


class input_async_mplex ?(onshutdown = (`Ignore : onshutdown_in_spec) )
                        ?buffer_size
                        (mplex : multiplex_controller)
                        : async_in_channel_engine =
object (self)

  inherit [unit] engine_mixin (`Working 0 : unit engine_state) mplex#event_system

  val data_queue = Queue.create()
		     (* The queue of the read strings *)

  val mutable data_top_pos = 0
		     (* How many bytes of the first string of data_queue
		      * have already been copied to the reading user.
		      *)

  val mutable data_queue_length = 0
		     (* The sum of all strings in data_queue, not counting
		      * data_top_pos
		      *)

  val buf = String.create buf_max_size
	      (* The input buffer *)

  val mutable pos_in = 0

  val mutable in_eof = false
  val mutable shutdown_done = false

  initializer
    self # check_for_input()


  method input s p l =
    if p < 0 || l < 0 || p > String.length s || p+l > String.length s then
      invalid_arg "Uq.engines.input_async_mplex#input";

    if in_eof then raise Closed_channel;

    let l' = min l data_queue_length in
    let l_todo = ref l' in
    let s_pos = ref p in

    while !l_todo > 0 do
      let u = try Queue.peek data_queue with Queue.Empty -> assert false in
      let n = min !l_todo (String.length u - data_top_pos) in
      String.blit u data_top_pos s !s_pos n;
      s_pos := !s_pos + n;
      data_top_pos <- data_top_pos + n;
      l_todo := !l_todo - n;
      if data_top_pos = String.length u then (
	let _ = Queue.take data_queue in
	data_top_pos <- 0
      )
    done;

    pos_in <- pos_in + l';
    data_queue_length <- data_queue_length - l';
    assert(data_queue_length >= 0);  (* must never overflow *)

    if not mplex#reading then 
      self # check_for_input();

    if l' > 0 then self # iam_count();
    (* If l' = 0, there were no data in the buffer. No need for notification *)

    if l' = 0 && mplex # read_eof then
      raise End_of_file
    else
      l'


  method close_in () =
    if not in_eof then (
      mplex # cancel_reading();
      self # shutdown (`Done())
    )


  method pos_in =
    if in_eof then raise Closed_channel;
    pos_in


  method abort() =
    match self#state with
	`Working _ ->
	  mplex # cancel_reading();
	  self # shutdown `Aborted;
      | _ ->
	  ()

  method event_system = mplex # event_system

  method private iam_count() =
    match self#state with
	`Working n -> 
	  self # set_state (`Working (n+1))
      | _ ->
	  ()

  method can_input =
    not in_eof && (data_queue_length > 0 || mplex#read_eof)


  method private handle_exception exn =
    mplex # cancel_reading();
    self # shutdown (`Error exn)


  method private check_for_input() =
    assert(not mplex#reading);
    if not mplex#read_eof then (
      (* Space to read something? *)
      let space =
	match buffer_size with
	  | None -> String.length buf
	  | Some m -> min (String.length buf) (m - data_queue_length) in
      if space > 0 then (
	mplex # start_reading 
	  ~when_done:(fun exn_opt n ->
			match exn_opt with
			  | None ->
			      if n > 0 then (
				let s = String.sub buf 0 n in
				Queue.add s data_queue;
				data_queue_length <- data_queue_length + n;
				assert(data_queue_length >= 0);
				      (* must never overflow *)
			      );
			      self # check_for_input();
			      if n > 0 then self # iam_count()
			  | Some End_of_file ->
			      self # iam_count()
			  | Some Cancelled ->
			      (* Called from [abort], so ignore any data *)
			      ()
			  | Some error ->
			      self # handle_exception error
		     )
	  buf 0 space
      )
    )

  method private shutdown next_state =
    (* See also output_async_mplex # shutdown *)
    if not shutdown_done then (
      shutdown_done <- true;
      in_eof <- true;
      Queue.clear data_queue;
      data_top_pos <- 0;
      data_queue_length <- 0;
      ( match onshutdown with
	  | `Ignore -> ()
	  | `Initiate_shutdown ->
	      mplex # start_shutting_down ~when_done:(fun _ -> ()) ()
	  | `Action f ->
	      ( try
		  f
		    (self : #async_in_channel_engine :> async_in_channel_engine)
		    mplex
		    next_state
		with error ->
		  Netlog.logf `Err
		    "Uq_engines.input_async_mplex#shutdown: %s" 
		    (Netexn.to_string error)
	      )
      );
      self # set_state next_state
    )

end
;;


let anyway ~finally f arg =
  try
    let r = f arg in
    finally();
    r
  with 
    | error ->
	finally();
	raise error
;;


class socket_multiplex_controller
         ?(close_inactive_descr = true)
         ?(preclose = fun () -> ())
         ?(supports_half_open_connection = false)
	 ?timeout
         fd esys : datagram_multiplex_controller =

  let fd_style = Netsys.get_fd_style fd in

  let get_ph() = Netsys_win32.lookup_pipe fd in
    (* To be used only when fd_style = `Pipe *)

  let supports_half_open_connection =
    match fd_style with
      | `W32_pipe -> false
      | _ -> supports_half_open_connection in

  let mem_supported =
    match fd_style with
      | `Read_write -> true
      | `Recv_send _ -> true
      | `Recv_send_implied -> true
      | _ -> false in

  let start_timer f =
    (* Call [f x] when the timer fires *)
    match timeout with
      | None ->
	  None
      | Some (tmo, x) ->
	  let tmo_g = Unixqueue.new_group esys in
	  Unixqueue.once esys tmo_g tmo (fun () -> f x);
	  Some (tmo_g, f) in

  let stop_timer r =
    match !r with
      | None -> ()
      | Some (old_tmo_g, f) ->
	  Unixqueue.clear esys old_tmo_g;
	  r := None in

(*
  let () =
    prerr_endline ("fd style: " ^ Netsys.string_of_fd_style fd_style) in
 *)

object(self)
  val mutable alive = true
  val mutable read_eof = false
  val mutable wrote_eof = false
  val mutable reading = `None
  val mutable reading_tmo = ref None
  val mutable writing = `None
  val mutable writing_tmo = ref None
  val mutable writing_eof = None
  val mutable shutting_down = None
  val mutable shutting_down_tmo = ref None
  val mutable disconnecting = None
  val mutable need_linger = false
  val mutable have_handler = false

  val mutable rcvd_from = None
  val mutable send_to = None

  val group = Unixqueue.new_group esys

  method alive = alive
  method mem_supported = mem_supported
  method reading = reading <> `None
  method writing = writing <> `None || writing_eof <> None
  method shutting_down = shutting_down <> None
  method read_eof = read_eof
  method wrote_eof = wrote_eof

  method supports_half_open_connection = supports_half_open_connection

  method received_from =
    match rcvd_from with
      | None -> 
	  failwith "#received_from: Nothing received yet, or unknown address"
      | Some a -> a

  method send_to a =
    send_to <- Some a

  initializer
    ( match fd_style with
	| `W32_pipe -> ()
	| `W32_event | `W32_pipe_server | `W32_input_thread 
	| `W32_output_thread | `W32_process ->
	    invalid_arg "Uq_engines.socket_multiplex_controller: \
                       invalid type of file descriptor"
	| _ -> 
	    Unix.set_nonblock fd
    );
    dlogr (fun () ->
	     sprintf
	       "new socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd))

  method private restart_all_timers() =
    match timeout with
      | None ->
	  ()
      | Some (tmo, x) ->
	  List.iter
	    (fun r ->
	       match !r with
		 | None -> ()
		 | Some (old_tmo_g, f) ->
		     Unixqueue.clear esys old_tmo_g;
		     r := start_timer f
	    )
	    [ reading_tmo; writing_tmo; shutting_down_tmo ]


  method start_reading ?(peek = fun ()->()) ~when_done s pos len =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "#start_reading";
    if reading <> `None then
      failwith "#start_reading: already reading";
    if shutting_down <> None then
      failwith "#start_reading: already shutting down";
    if not alive then
      failwith "#start_reading: inactive connection";
    self # check_for_connect();
    Unixqueue.add_resource esys group (Unixqueue.Wait_in fd, -1.0);
    reading <- `String(when_done, peek, s, pos, len);
    reading_tmo := start_timer self#cancel_reading_with;
    disconnecting <- None;
    dlogr (fun () ->
	     sprintf
	       "start_reading socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd))


  method start_mem_reading ?(peek = fun ()->()) ~when_done m pos len =
    if not mem_supported then raise Mem_not_supported;
    if pos < 0 || len < 0 || pos > Bigarray.Array1.dim m - len then
      invalid_arg "#start_mem_reading";
    if reading <> `None then
      failwith "#start_mem_reading: already reading";
    if shutting_down <> None then
      failwith "#start_mem_reading: already shutting down";
    if not alive then
      failwith "#start_mem_reading: inactive connection";
    self # check_for_connect();
    Unixqueue.add_resource esys group (Unixqueue.Wait_in fd, -1.0);
    reading <- `Mem(when_done, peek, m, pos, len);
    reading_tmo := start_timer self#cancel_reading_with;
    disconnecting <- None;
    dlogr (fun () ->
	     sprintf
	       "start_reading socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd))


  method cancel_reading () =
    self # cancel_reading_with Cancelled

  method private cancel_reading_with x =
    match reading with
      | `None ->
	  ()
      | `String(f_when_done, _, _, _, _) ->
	  self # really_cancel_reading();
	  anyway
	    ~finally:self#check_for_disconnect
	    (f_when_done (Some x)) 0
      | `Mem(f_when_done, _, _, _, _) ->
	  self # really_cancel_reading();
	  anyway
	    ~finally:self#check_for_disconnect
	    (f_when_done (Some x)) 0

  method private really_cancel_reading() =
    stop_timer reading_tmo;
    if reading <> `None then (
      Unixqueue.remove_resource esys group (Unixqueue.Wait_in fd);
      reading <- `None;
      dlogr (fun () ->
	       sprintf
		 "cancel_reading socket_multiplex_controller mplex=%d fd=%Ld"
		 (Oo.id self) (Netsys.int64_of_file_descr fd))
    )

  method start_writing ~when_done s pos len =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "#start_writing";
    if writing <> `None || writing_eof <> None then
      failwith "#start_writing: already writing";
    if shutting_down <> None then
      failwith "#start_writing: already shutting down";
    if wrote_eof then
      failwith "#start_writing: already past EOF";
   if not alive then
      failwith "#start_writing: inactive connection";
    self # check_for_connect();
    Unixqueue.add_resource esys group (Unixqueue.Wait_out fd, -1.0);
    writing <- `String(when_done, s, pos, len);
    writing_tmo := start_timer self#cancel_writing_with;
    disconnecting <- None;
    dlogr (fun () ->
	     sprintf
	       "start_writing socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd))

  method start_mem_writing ~when_done m pos len =
    if not mem_supported then raise Mem_not_supported;
    if pos < 0 || len < 0 || pos > Bigarray.Array1.dim m - len then
      invalid_arg "#start_mem_writing";
    if writing <> `None || writing_eof <> None then
      failwith "#start_mem_writing: already writing";
    if shutting_down <> None then
      failwith "#start_mem_writing: already shutting down";
    if wrote_eof then
      failwith "#start_mem_writing: already past EOF";
    if not alive then
      failwith "#start_mem_writing: inactive connection";
    self # check_for_connect();
    Unixqueue.add_resource esys group (Unixqueue.Wait_out fd, -1.0);
    writing <- `Mem(when_done, m, pos, len);
    writing_tmo := start_timer self#cancel_writing_with;
    disconnecting <- None;
    dlogr (fun () ->
	     sprintf
	       "start_writing socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd))

  method start_writing_eof ~when_done () =
    if not supports_half_open_connection then
      failwith "#start_writing_eof: operation not supported";
    (* From here on we know fd is not a named pipe *)
    if writing <> `None || writing_eof <> None then
      failwith "#start_writing_eof: already writing";
    if shutting_down <> None then
      failwith "#start_writing_eof: already shutting down";
    if wrote_eof then
      failwith "#start_writing_eof: already past EOF";
    if not alive then
      failwith "#start_writing_eof: inactive connection";
    self # check_for_connect();
    Unixqueue.add_resource esys group (Unixqueue.Wait_out fd, -1.0);
    writing_eof <- Some when_done;
    writing_tmo := start_timer self#cancel_writing_with;
    disconnecting <- None;
    dlogr (fun () ->
	     sprintf
	       "start_writing_eof socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd))


  method cancel_writing () =
    self # cancel_writing_with Cancelled

  method private cancel_writing_with x =
    match writing, writing_eof with
      | `None, None ->
	  ()
      | (`String(f_when_done, _, _, _) | `Mem(f_when_done, _, _, _)), None ->
	  self # really_cancel_writing();
	  anyway
	    ~finally:self#check_for_disconnect
	    (f_when_done (Some x)) 0
      | `None, Some f_when_done ->
	  self # really_cancel_writing();
	  anyway
	    ~finally:self#check_for_disconnect
	    f_when_done (Some x)
      | _ ->
	  assert false

  method private really_cancel_writing() =
    stop_timer writing_tmo;
    if writing <> `None || writing_eof <> None then (
      Unixqueue.remove_resource esys group (Unixqueue.Wait_out fd);
      writing <- `None;
      writing_eof <- None;
      dlogr (fun () ->
	       sprintf
		 "cancel_writing socket_multiplex_controller mplex=%d fd=%Ld"
		 (Oo.id self) (Netsys.int64_of_file_descr fd))

    )

  method start_shutting_down ?(linger = 60.0) ~when_done () =
    if reading <> `None || writing <> `None || writing_eof <> None then
      failwith "#start_shutting_down: still reading or writing";
    if shutting_down <> None then
      failwith "#start_shutting_down: already shutting down";
    if not alive then
      failwith "#start_shutting_down: inactive connection";
    self # check_for_connect();
    let linger_timeout = 
      if need_linger then linger else 0.0 in
    let wid = Unixqueue.new_wait_id esys in
    let (op, tmo) =
      if linger_timeout = 0.0 then
	(Unixqueue.Wait wid, 0.0)
      else
	(Unixqueue.Wait_in fd, linger_timeout) in
    Unixqueue.add_resource esys group (op,tmo);
    shutting_down <- Some(when_done, op);
    shutting_down_tmo := start_timer self#cancel_shutting_down_with;
    disconnecting <- None;
    dlogr (fun () ->
	     sprintf
	       "start_shutting_down socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd))

  method cancel_shutting_down () =
    self # cancel_shutting_down_with Cancelled
    
  method private cancel_shutting_down_with x =
    match shutting_down with
      | None ->
	  ()
      | Some (f_when_done, _) ->
	  self # really_cancel_shutting_down ();
	  anyway
	    ~finally:self#check_for_disconnect
	    f_when_done (Some x)


  method private really_cancel_shutting_down () =
    stop_timer shutting_down_tmo;
    match shutting_down with
      | None -> 
	  ()
      | Some (_, op) ->
	  Unixqueue.remove_resource esys group op;
	  shutting_down <- None;
	  dlogr (fun () ->
		   sprintf
		     "cancel_shutting_down \
                        socket_multiplex_controller mplex=%d fd=%Ld"
		     (Oo.id self) (Netsys.int64_of_file_descr fd))


  method private check_for_connect() =
    if not have_handler then (
      Unixqueue.add_handler esys group (fun _ _ -> self # handle_event);
      have_handler <- true
    );
    disconnecting <- None

  method private check_for_disconnect() =
    if reading = `None && writing = `None && writing_eof = None && 
         shutting_down = None && disconnecting = None then
	   (
	     let wid = Unixqueue.new_wait_id esys in
	     let disconnector = Unixqueue.Wait wid in
	     Unixqueue.add_event esys (Unixqueue.Timeout(group,disconnector));
	     disconnecting <- Some disconnector
	   )

  method private notify_rd f_when_done exn_opt n =
    self # really_cancel_reading();
    self # restart_all_timers();
    dlogr (fun () ->
	     sprintf
	       "input_done \
                socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd));
    anyway
      ~finally:self#check_for_disconnect
      (f_when_done exn_opt) n

  method private notify_wr f_when_done exn_opt n =
    self # really_cancel_writing();
    self # restart_all_timers();
    dlogr (fun () ->
	     sprintf
	       "output_done \
                socket_multiplex_controller mplex=%d fd=%Ld"
	       (Oo.id self) (Netsys.int64_of_file_descr fd));
    anyway
      ~finally:self#check_for_disconnect
      (f_when_done exn_opt) n

  method private handle_event ev =
    match ev with
      | Unixqueue.Input_arrived(g, _) when g = group ->
	  dlogr (fun () ->
		   sprintf
		     "input_event \
                        socket_multiplex_controller mplex=%d fd=%Ld"
		     (Oo.id self) (Netsys.int64_of_file_descr fd));
	  ( match reading with
	      | `None -> ()
	      | `String (f_when_done, peek, _, _, _)
	      | `Mem    (f_when_done, peek, _, _, _) -> (
		  peek();
		  try
		    rcvd_from <- None;
		    let n = 
		      match reading with
			| `None -> assert false
			| `String(_,_, s, pos, len) -> (
			    match fd_style with
			      | `Recv_send(_,a) ->
				  let n = Unix.recv fd s pos len [] in
				  rcvd_from <- Some a;
				  n
			      | `Recvfrom_sendto ->
				  let (n, a) = Unix.recvfrom fd s pos len [] in
				  rcvd_from <- Some a;
				  n
			      | _ ->
				  Netsys.gread fd_style fd s pos len
			  )
			| `Mem(_,_, m, pos, len) -> (
			    match fd_style with
			      | `Recv_send(_,a) ->
				  let n = 
				    Netsys_mem.mem_recv fd m pos len [] in
				  rcvd_from <- Some a;
				  n
			      | `Recv_send_implied ->
				  Netsys_mem.mem_recv fd m pos len []
			      | `Read_write ->
				  Netsys_mem.mem_read fd m pos len
			      | _ ->
				  assert false
			  )  in
		    if n = 0 then (
		      read_eof <- true;
		      need_linger <- false;
		      self # notify_rd f_when_done (Some End_of_file) 0
		    )
		    else
		      self # notify_rd f_when_done None n
		  with
		    | Unix.Unix_error(Unix.EAGAIN,_,_)
		    | Unix.Unix_error(Unix.EWOULDBLOCK,_,_)
		    | Unix.Unix_error(Unix.EINTR,_,_) ->
			()
		    | error ->
			self # notify_rd f_when_done (Some error) 0
		)
	  );
	  ( match shutting_down with
	      | Some (f_when_done, op) when op = Unixqueue.Wait_in fd ->
		  let exn_opt, notify =
		    try
		      Netsys.gshutdown fd_style fd Unix.SHUTDOWN_ALL;
		      (None, true)
		    with
		      | Unix.Unix_error(Unix.EAGAIN,_,_)
		      | Unix.Unix_error(Unix.EWOULDBLOCK,_,_)
		      | Unix.Unix_error(Unix.EINTR,_,_) ->
			  (None, false)
		      | Unix.Unix_error(Unix.ENOTCONN,_,_) ->
			  (None, true)
		      | Unix.Unix_error(Unix.EPERM,_,_) ->
			  (None, true)
		      | error ->
			  (Some error, true)
		  in
		  if notify then (
		    self # really_cancel_shutting_down();
		    stop_timer shutting_down_tmo;
		    read_eof <- true;
		    wrote_eof <- true;
		    dlogr (fun () ->
			     sprintf
			       "shutdown_done \
                                  socket_multiplex_controller mplex=%d fd=%Ld"
			       (Oo.id self) (Netsys.int64_of_file_descr fd));
		    anyway
		      ~finally:self#check_for_disconnect
		      f_when_done exn_opt
		  )
	      | _ -> ()
	  )

      | Unixqueue.Output_readiness(g, _) when g = group ->
	  dlogr (fun () ->
		   sprintf
		     "output_event \
                        socket_multiplex_controller mplex=%d fd=%Ld"
		     (Oo.id self) (Netsys.int64_of_file_descr fd));
	  ( match writing with
	      | `None -> ()
	      | `String (f_when_done, _, _, _)
	      | `Mem    (f_when_done, _, _, _) -> (
		  try
		    let n = 
		      match writing with
			| `None -> assert false
			| `String(_, s, pos, len) -> (
			    match fd_style with
			      | `Recvfrom_sendto ->
				  ( match send_to with
				      | None ->
					  failwith "socket_multiplex_controller: Unknown receiver of message to send"
				      | Some a ->
					  Unix.sendto fd s pos len [] a
				  )
			      | _ ->
				  Netsys.gwrite fd_style fd s pos len 
			  )
			| `Mem(_, m, pos, len) -> (
			    match fd_style with
			      | `Recv_send _ ->
				  Netsys_mem.mem_send fd m pos len []
			      | `Recv_send_implied ->
				  Netsys_mem.mem_send fd m pos len []
			      | `Read_write ->
				  Netsys_mem.mem_write fd m pos len
			      | _ ->
				  assert false
			  ) in
		    self # notify_wr f_when_done None n
		  with
		    | Unix.Unix_error(Unix.EAGAIN,_,_)
		    | Unix.Unix_error(Unix.EWOULDBLOCK,_,_)
		    | Unix.Unix_error(Unix.EINTR,_,_) ->
			()
		    | error ->
			self # notify_wr f_when_done (Some error) 0
		)
	  );
	  ( match writing_eof with
	      | None -> ()
	      | Some f_when_done ->
		  let exn_opt, notify =
		    try
		      if not wrote_eof then (
			Netsys.gshutdown fd_style fd Unix.SHUTDOWN_SEND;
			if not read_eof then need_linger <- true;
			wrote_eof <- true
		      );
		      (None, true)
		    with
		      | Unix.Unix_error(Unix.EAGAIN,_,_)
		      | Unix.Unix_error(Unix.EWOULDBLOCK,_,_)
		      | Unix.Unix_error(Unix.EINTR,_,_) ->
			  (None, false)
		      | Netsys.Shutdown_not_supported ->
			  (None, true)
		  in
		  if notify then (
		    self # really_cancel_writing();
		    dlogr (fun () ->
			     sprintf
			       "output_eof_done \
                                  socket_multiplex_controller mplex=%d fd=%Ld"
			       (Oo.id self) (Netsys.int64_of_file_descr fd));
		    anyway
		      ~finally:self#check_for_disconnect
		      f_when_done exn_opt
		  )
	  )

      | Unixqueue.Timeout (g, op) when g = group ->
	  (* Note: The following is incompatible with [once] because we
           * always accept timeout events!
           *)
	  dlogr (fun () ->
		   sprintf
		     "other_event \
                        socket_multiplex_controller mplex=%d fd=%Ld"
		     (Oo.id self) (Netsys.int64_of_file_descr fd));
	  ( match shutting_down with
	      | Some (f_when_done, op') when op = op' ->
		  let exn_opt, notify =
		    try
		      match fd_style with
			| `W32_pipe ->
			    let ph = get_ph() in
			    Netsys_win32.pipe_shutdown ph;
			    (None, true)
			| _ ->
			    Unix.shutdown fd 
			      (if wrote_eof then Unix.SHUTDOWN_RECEIVE else
				 Unix.SHUTDOWN_ALL);
			    (None, true)
		    with
		      | Unix.Unix_error(Unix.EAGAIN,_,_)
		      | Unix.Unix_error(Unix.EWOULDBLOCK,_,_)
		      | Unix.Unix_error(Unix.EINTR,_,_) ->
			  assert false  (* Not documented in man pages *)
		      | Unix.Unix_error(Unix.ENOTCONN,_,_) ->
			  (None, true)
		      | error ->
			  (Some error, true)
		  in
		  if notify then (
		    self # really_cancel_shutting_down();
		    read_eof <- true;
		    wrote_eof <- true;
		    dlogr (fun () ->
			     sprintf
			       "shutdown_done \
                                  socket_multiplex_controller mplex=%d fd=%Ld"
			       (Oo.id self) (Netsys.int64_of_file_descr fd));
		    anyway
		      ~finally:self#check_for_disconnect
		      f_when_done exn_opt
		  )
	      | _ -> ()
	  );
	  ( match disconnecting with
	      | Some op' when op = op' ->
		  disconnecting <- None;
		  have_handler <- false;
		  raise Equeue.Terminate

	      | _ -> ()
	  )

      | _ ->
	  raise Equeue.Reject

  method inactivate() =
    dlogr (fun () ->
	     sprintf 
	       "inactivate \
                  socket_multiplex_controller mplex=%d fd=%Ld alive=%b \
                  close_inactive_descr=%b"
	       (Oo.id self) (Netsys.int64_of_file_descr fd) alive
	       close_inactive_descr);
    if alive then (
      alive <- false;
      self # really_cancel_reading();
      self # really_cancel_writing();
      self # really_cancel_shutting_down();
      stop_timer reading_tmo;
      stop_timer writing_tmo;
      stop_timer shutting_down_tmo;
      disconnecting <- None;
      have_handler <- false;
      Unixqueue.clear esys group;
      if close_inactive_descr then (
	preclose();
	Netsys.gclose fd_style fd
 	(* It is important that Unix.close (or substitute) is the very
           last action. From here on, a thread running in parallel can
           allocate this descriptor again, so it is essential that there
           are no references anymore to it when the old descriptor is closed.
	 *)
      )
    )

  method event_system = esys

end
;;


let create_multiplex_controller_for_connected_socket 
       ?close_inactive_descr ?preclose ?supports_half_open_connection 
       ?timeout
       fd esys =
  let mplex = 
    new socket_multiplex_controller
      ?close_inactive_descr ?preclose ?supports_half_open_connection 
      ?timeout
      fd esys in
  (mplex :> multiplex_controller)
;;


let create_multiplex_controller_for_datagram_socket 
       ?close_inactive_descr ?preclose ?timeout fd esys =
  let mplex = 
    new socket_multiplex_controller
      ?close_inactive_descr ?preclose ~supports_half_open_connection:false 
      ?timeout
      fd esys in
  (mplex :> datagram_multiplex_controller)
;;


class output_async_descr ~dst ?buffer_size ?(close_dst=true) esys =
  (* Map to output_async_mplex. Be careful not to depend on socket
   * functionaliy (esp. shutdown).
   *)
  let mplex = create_multiplex_controller_for_connected_socket dst esys in
  let shutdown ach mplex _ =
    if close_dst then Unix.close dst
  in
  output_async_mplex 
    ~onclose:`Ignore
    ~onshutdown:(`Action shutdown)
    ?buffer_size
    mplex
;;


class input_async_descr ~src ?buffer_size ?(close_src=true) esys =
  (* Map to input_async_mplex. Be careful not to depend on socket
   * functionaliy (esp. shutdown).
   *)
  let mplex = create_multiplex_controller_for_connected_socket src esys in
  let shutdown ach mplex _ =
    if close_src then Unix.close src
  in
  input_async_mplex 
    ~onshutdown:(`Action shutdown)
    ?buffer_size
    mplex
;;


type copy_task =
    [ `Unidirectional of (Unix.file_descr * Unix.file_descr)
    | `Uni_socket of (Unix.file_descr * Unix.file_descr)
    | `Bidirectional of (Unix.file_descr * Unix.file_descr)
    | `Tridirectional of (Unix.file_descr * Unix.file_descr * Unix.file_descr)
    ]
;;


class copier (copy_task : copy_task) ues : [unit] engine =
object(self)
  val mutable engines = []
  val mutable last_eng_states = []
  val mutable last_count = 0

  initializer
    ( match copy_task with
	  `Unidirectional(fd1, fd2) ->
	    self # init_unidirectional fd1 fd2

	| `Uni_socket(fd1, fd2) ->
	    self # init_uni_socket fd1 fd2

	| `Bidirectional(fd1, fd2) ->
	    self # init_tridirectional true fd1 fd2 fd2
	    
	| `Tridirectional(fd1, fd2, fd3) ->
	    self # init_tridirectional false fd1 fd2 fd3
(*
	| _ ->
	    assert false
 *)
    );
    last_eng_states <- List.map (fun eng -> eng # state) engines;


  method private init_unidirectional fd1 fd2 =
    (* This is quite simple. fd2_ch is an output channel
     * writing data to fd2. fd1_rcv is a receiver transferring
     * data from fd1 to fd2_ch. If fd1_rcv is at EOF, it will
     * close fd1, and close fd2_ch. fd2_ch closes fd2 after
     * it has written all buffered data.
     *)
    let fd2_ch = new output_async_descr 
		   ~dst:fd2 
		   ~buffer_size:buf_max_size
		   ues in
    let fd1_rcv = new receiver
		    ~src:fd1 
		    ~dst:(fd2_ch :> async_out_channel)
		    ues in
    engines <- [ fd1_rcv;
		 (fd2_ch :> unit engine);
	       ]


  method private init_uni_socket fd1 fd2 =
    (* Here, we have to modify the EOF behaviour. First,
     * fd1_rcv must not close src. Of course, it must
     * close the output channel fd2_ch, otherwise the
     * channel would not know that it is at the end of 
     * the data stream. However, fd2_ch must not close
     * dst; instead we catch the EOF situation, and
     * shutdown the socket.
     *)
    let fd2_ch = new output_async_descr 
		   ~dst:fd2 
		   ~close_dst:false
		   ~buffer_size:buf_max_size
		   ues in
    let fd1_rcv = new receiver
		    ~src:fd1 
		    ~close_src:false
		    ~dst:(fd2_ch :> async_out_channel)
		    ues in
    when_state ~is_done:(fun () -> 
			   Unix.shutdown fd2 Unix.SHUTDOWN_SEND)
               fd2_ch;
    engines <- [ fd1_rcv;
		 (fd2_ch :> unit engine);
	       ]


  method private init_tridirectional bi_case fd1 fd2 fd3 =
    (* Basically, we have two `Uni_socket copiers where one copier
     * transfers data into the reverse direction as the other
     * copier. Additionally, we have to close the descriptors
     * when work is done, either successfully or with error.
     *)
    (* bi_case: fd2 = fd3 is assumed, and fd2 must be a socket
     *)

    (* Copy fd1 to fd2: *)
    let fd2_ch = new output_async_descr 
		   ~dst:fd2 
		   ~close_dst:false
		   ~buffer_size:buf_max_size
		   ues in
    let fd1_rcv = new receiver
		    ~src:fd1 
		    ~close_src:false
		    ~dst:(fd2_ch :> async_out_channel)
		    ues in
    
    (* Copy fd3 to fd1: *)
    let fd1_ch = new output_async_descr 
		   ~dst:fd1 
		   ~close_dst:false
		   ~buffer_size:buf_max_size
		   ues in
    let fd3_rcv = new receiver
		    ~src:fd3
		    ~close_src:false
		    ~dst:(fd1_ch :> async_out_channel)
		    ues in
	    
    (* Check state: *)
    let fd1_eof = ref false in  (* whether output to fd1 @ eof *)
    let fd1_closed = ref false in
    let fd2_eof = ref false in  (* whether output to fd2 @ eof *)
    let fd2_closed = ref false in
    let fd3_closed = ref false in
    let full_close _ =
      if not !fd1_closed then
	Unix.close fd1;
      fd1_eof := true;
      fd1_closed := true;

      if not !fd2_closed then (
	Unix.close fd2;
	if bi_case then fd3_closed := true;
      );
      fd2_eof := true;
      fd2_closed := true;

      if not !fd3_closed then 
	Unix.close fd3;
      fd3_closed := true;
    in
    let half_close_fd1() =
      if !fd2_eof then (
	full_close()
      ) else (
        if not !fd1_eof then
	  Unix.shutdown fd1 Unix.SHUTDOWN_SEND;
	fd1_eof := true
      ) in
    let half_close_fd2() =
      if !fd1_eof then (
	full_close()
      ) else (
        if not !fd2_eof then (
	  if bi_case then
	    Unix.shutdown fd2 Unix.SHUTDOWN_SEND
	  else (
	    Unix.close fd2;
	    fd2_closed := true;
	  )
	);
	fd2_eof := true
      ) in

    when_state ~is_done:half_close_fd2
               ~is_error:full_close
               ~is_aborted:full_close
               (fd2_ch :> 'a engine);
    when_state ~is_done:half_close_fd1
	       ~is_error:full_close
	       ~is_aborted:full_close
	       fd1_ch;

    engines <- [ fd1_rcv;
		 (fd2_ch :> unit engine);
		 fd3_rcv;
		 (fd1_ch :> unit engine);
	       ]


  method state =
    (* We inspect the states of all engines. If there is an engine
     * in error state, this state will be returned (Broken_communication
     * has lower priority than other errors). Otherwise: If there is
     * an aborted engine, we return that the copier is aborted.
     * Otherwise: If there is at least one working engine, we return
     * working state. The last case is that all engines are done, and
     * we return done.
     *
     * Note that the progress meter for `Working is emulated, and
     * the more often [state] is invoked, the more frequent the progress
     * meter is increased. But it is only increased if at least one
     * engine has made some progress.
     *
     * CHECK: This seems to be a generalization of the sync_engine above.
     * Maybe we want it as basic engine construct?
     *)

    let eng_states =
      List.map (fun eng -> eng # state) engines in

    let our_state = ref(`Done()) in

    List.iter
      (fun st ->
	 match st with
	     `Done _ -> ()
	   | `Working _ ->
	       ( match !our_state with
		     `Done _ -> our_state := `Working 0
		   | _       -> ()
	       )
	   | `Aborted ->
	       ( match !our_state with
		     `Done _ 
		   | `Working _ -> our_state := `Aborted
		   | _          -> ()
	       )
	   | `Error err ->
	       ( match !our_state with
		     `Done _
		   | `Working _ 
		   | `Aborted
		   | `Error Broken_communication ->
		       our_state := st
		   | `Error _ ->
		       ()
	       )
		       
      )
      eng_states;

    ( match !our_state with
	  `Working _ ->
	    if eng_states <> last_eng_states then 
	      last_count <- last_count + 1;
	    our_state := `Working last_count
	| _ ->
	    ()
    );
    last_eng_states <- eng_states;

    !our_state

  method abort () =
    (* Simply abort all engines *)
    List.iter
      (fun eng -> eng # abort())
      engines
      (* CHECK: Hopefully, no engine goes to an error state because the
       * other engine aborts...
       *)

  method request_notification f =
    (* Simply forward the request to all engines *)

    let enabled = ref true in
    (* After the first notification has disabled further notifications, 
     * it must be ensured that no more notifications will happen.
     * [enabled] is [true] as long as notifications are enabled.
     *)

    let f'() = 
      !enabled && 
      ( let enabled' = f() in
	enabled := !enabled && enabled';
	!enabled
      )
    in

    List.iter
      (fun eng -> eng # request_notification f')
      engines

  method event_system =  ues

end
;;


type inetspec =
  [ `Sock_inet of (Unix.socket_type * Unix.inet_addr * int)
  | `Sock_inet_byname of (Unix.socket_type * string * int)
  ]

type sockspec =
  [ `Sock_unix of (Unix.socket_type * string)
  | inetspec
  ]
;;


let sockspec_of_sockaddr st =
  function
    | Unix.ADDR_INET(ip,port) -> `Sock_inet(st, ip, port)
    | Unix.ADDR_UNIX path -> `Sock_unix(st, path)


let sockspec_of_socksymbol st =
  function
    | `Inet(ip,port) -> `Sock_inet(st, ip, port)
    | `Inet_byname(n,port) -> `Sock_inet_byname(st, n, port)
    | `Unix p -> `Sock_unix(st, p)


type connect_address =
    [ `Socket of sockspec * connect_options
    | `Command of string * (int -> Unixqueue.event_system -> unit)
    | `W32_pipe of Netsys_win32.pipe_mode * string
    ]

and connect_options =
    { conn_bind : sockspec option;
    }

;;

let default_connect_options = { conn_bind = None } ;;



type connect_status =
    [ `Socket of Unix.file_descr * sockspec
    | `Command of Unix.file_descr * int
    | `W32_pipe of Unix.file_descr
    ]
;;


let client_endpoint =
  function
      `Socket(fd,_) -> fd
    | `Command(fd,_) -> fd
    | `W32_pipe fd -> fd
;;

let client_socket = client_endpoint


class type client_endpoint_connector = object
  method connect : connect_address -> 
                   Unixqueue.event_system ->
		     connect_status engine
end ;;

class type client_socket_connector = client_endpoint_connector



let addr_of_name name =
  let entry = Uq_resolver.get_host_by_name name in
  entry.Unix.h_addr_list.(0)
;;


let getsockspec stype s =
  match Unix.getsockname s with
      Unix.ADDR_UNIX path ->
	`Sock_unix(stype, path)
    | Unix.ADDR_INET(addr, port) ->
	`Sock_inet(stype, addr, port)
;;


(*
let getpeerspec stype s =
  (* Warning: this function may fail if the socket is connected and the
     peer does not have an address (e.g. older OSX)
   *)
  match Netsys.getpeername s with
      Unix.ADDR_UNIX path ->
	`Sock_unix(stype, path)
    | Unix.ADDR_INET(addr, port) ->
	`Sock_inet(stype, addr, port)
;;
 *)


let getinetpeerspec stype s =
  try
    match Netsys.getpeername s with
        Unix.ADDR_UNIX path ->
	  None
      | Unix.ADDR_INET(addr, port) ->
	  Some(`Sock_inet(stype, addr, port))
  with
    | _ -> None
;;


(*
let getconnerror s = (* Call this after getpeerspec raised ENOTCONN *)
  try 
    let b = String.make 1 ' ' in
    let _ = Unix.recv s b 0 1 [] in
    assert false
  with
    | error -> error
 *)


(*
type xdom =
    [ `Socket of socket_domain
    | `Pipe
    ]
 *)


(* - new impl, not yet ready *)
(*
class direct_socket_connector() : client_socket_connector =
object (self)
  method connect connaddr ues =

    let const_eng v =
      new epsilon_engine(`Done v) ues in

    let sock_prim_data_eng sockspec =
      match sockspec with
	| `Sock_unix(stype, path) ->
	    let addr = Unix.ADDR_UNIX path in
	    const_eng(`Socket Unix.PF_UNIX, stype, addr)
	| `Sock_inet(stype, ip, port) ->
	    let dom = Netsys.domain_of_inet_addr ip in
	    let addr = Unix.ADDR_INET(ip,port) in
	    const_eng(`Socket dom, stype, addr)
	| `Sock_inet_by_name(stype, name, port) ->
	    let ip_opt = XXX in
	    ( match ip_opt with
		| Some ip ->
		    let dom = Netsys.domain_of_inet_addr ip in
		    let addr = Unix.ADDR_INET(ip, port) in
		    const_eng(`Socket dom, stype, addr)
		| None ->
		    (* Now need a real host lookup *)
		    let r = Uq_resolver.current_resolver() in
		    let eng = r # host_by_name name ues in
		    new map_engine
		      ~map_done:(fun he ->
				   let dom = he.Unix.h_addrtype in
				   let ip = he.Unix.h_addr_list.(0) in
				   let addr = Unix.ADDR_INET(ip, port) in
				   `Done(`Socket dom, stype, addr)
				)
		      eng
	    )

	    
	| `Pipe XXX ->
    in

    let sock_data_eng sockspec opts =
      (* Creates an engine that finds out the relevant data to create
         the socket
       *)
      match opts.conn_bind with
	| None ->
	    new map_engine
	      ~map_done:(fun (dom, stype, addr) ->
			   `Done(dom, stype, addr, None))
	      (sock_prim_data_eng sockspec)
	| Some bind_sockspec ->
	    (* Run two resolver engines in parallel *)
	    let d1_eng = sock_prim_data_eng sockspec in
	    let d2_eng = sock_prim_data_eng bind_sockspec in
	    new map_engine
	      ~map_done:(fun ((dom1, stype1, addr1), (dom2, _, addr2)) ->
			   if dom1 <> dom2 then
			     `Error(Failure("direct_socket_connector: Socket domain mismatch"))
			   else
			     `Done(dom1, stype1, addr1, Some addr2)
			)
	      (new sync_engine d1_eng d2_eng)
    in

    let sock_data_check sockspec opts =
      (* Check whether params are valid *) 
      match (sockspec, opts.conn_bind) with
	| `Sock_unix(stype,_), None -> ()
	| `Sock_unix(stype,_), Some(`Sock_unix(stype, _)) -> ()
	| `Sock_inet(stype,_,_), None -> ()
	| `Sock_inet_byname(stype,_,_), None -> ()
	| `Sock_inet(stype,_,_), Some(`Sock_inet(stype,_,_)) -> ()
	| `Sock_inet(stype,_,_), Some(`Sock_inet_byname(stype,_,_)) -> ()
	| `Sock_inet_byname(stype,_,_), Some(`Sock_inet(stype,_,_)) -> ()

	| `Sock_inet_byname(stype,_,_), Some(`Sock_inet_byname(stype,_,_)) -> ()
	| `Pipe(_,_), None -> ()
	| `Pipe(_,_), Some(`Pipe(_,_)) -> ()
	| _ ->
	    invalid_arg "direct_socket_connector: socket type mismatch"
    in

    let create_eng sockspec (dom, stype, addr, bind_addr_opt) ->
      (* Create and connect the socket s, and return the connect engine
       *)
      match dom with
	| `Socket sockdom ->
	    let connect_tried = ref false in
	    let s = Unix.socket sockdom 0 in
	    ( try
		Netsys.set_close_on_exec s;
		Unix.set_nonblock s;
		( match bind_addr_opt with
		    | None -> ()
		    | Some bind_addr ->
			Unix.bind s bind_addr
		);
		connect_tried := true;
		Unix.connect s addr;
		Netsys.connect_check s;
		let fake_conn_eng =
		  const_eng(`Socket(s, getsockspec stype s)) in
		when_state
		  ~is_aborted:(fun _ -> Unix.close s)
		  fake_conn_eng;
		fake_conn_eng
	      with
		| Unix.Unix_error((Unix.EINPROGRESS|Unix.EWOULDBLOCK),_,_) 
		    when !connect_tried -> 
		    (* Note: Win32 returns EWOULDBLOCK instead of EINPROGRESS *)
		    (* Wait until the socket is writeable. Win32 reports connect
                       errors by signaling that out-of-band data can be received
                       (funny, right?), so we wait for that condition, too.
		     *)
		    let poll_eng = 
		      new poll_engine [ Unixqueue.Wait_out s, (-1.0);
					Unixqueue.Wait_oob s, (-1.0)
				      ] ues in
		    let conn_eng =
		      new map_engine
			~map_done:(fun _ ->
				     try
				       Netsys.connect_check s;
				       `Done(getsockspec stype s)
				     with
				       | error -> 
					   Unix.close s; `Error error
				  )
			~map_error:(fun e ->
				      Unix.close s; `Error e)
			~map_aborted:(fun _ ->
					Unix.close s; `Aborted)
			(poll_eng :> Unixqueue.event engine) in
		    conn_eng
		| e ->
		    Unix.close s; raise e
	    )

	| `Pipe ->
	    ( match sockspec with
		| `Pipe(mode,name) ->
		    let ph = Netsys_win32.pipe_connect name mode in
		    let s = Netsys_win32.pipe_descr ph in
		    (s, None)
		      (* CHECK: do we need
		         Netsys_win32.pipe_shutdown ph
		       *)
		| _ ->
		    assert false
	    )
    in

    match connaddr with
      | `Socket(sockspec,opts) ->
	  (* Check on wrong arguments: *)
	  sock_data_check sockspec opts;
	  (* Create and use the engines: *)
	  let data_eng = sock_data_eng sockspec opts in
	  new seq_engine
	    data_eng
	    (fun sock_data ->
	       create_eng sockspec sock_data)
      | _ ->
	  raise Addressing_method_not_supported
end
 *)


(* Old impl with sync name lookup *)
  class direct_connector() : client_endpoint_connector =
  object (self)
    method connect connaddr ues =

      let setup_socket s stype dest_addr opts =
	try
	  Netsys.set_close_on_exec s;
	  Unix.set_nonblock s;
	  ( match opts.conn_bind with
	      | Some bind_spec ->
		  ( match bind_spec with
		      | `Sock_unix(stype', path) ->
			  if stype <> stype' then 
			    invalid_arg "Socket type mismatch";
			  Unix.bind s (Unix.ADDR_UNIX path)
		      | `Sock_inet(stype', addr, port) ->
			  if stype <> stype' then 
			    invalid_arg "Socket type mismatch";
			  Unix.bind s (Unix.ADDR_INET(addr,port))
		      | `Sock_inet_byname(stype', name, port) ->
			  if stype <> stype' then 
			    invalid_arg "Socket type mismatch";
			  let addr = addr_of_name name in
			  Unix.bind s (Unix.ADDR_INET(addr,port))
		  )
	      | None -> ()
	  );
	  Unix.connect s dest_addr;
	  (s, stype, true)
	with
	    Unix.Unix_error((Unix.EINPROGRESS|Unix.EWOULDBLOCK),_,_) -> 
	      (s,stype,false)
		(* Note: Win32 returns EWOULDBLOCK instead of EINPROGRESS *)
	  | error ->
	      (* Remarks:
               * We can get here EAGAIN. Unfortunately, this is a kind of
               * "catch-all" error for Unix.connect, e.g. you can get it when
               * you are run out of local ports, or if the backlog limit is
               * exceeded. It is totally unclear what to do in this case,
               * so we do not handle it here. The user is supposed to connect
               * later again.
               *)
	      Unix.close s; raise error
      in

      match connaddr with
	  `Socket(sockspec,opts) ->
	    let (s, stype, is_connected) = 
	      match sockspec with
		| `Sock_unix(stype, path) ->
		    let s = Unix.socket Unix.PF_UNIX stype 0 in
		    setup_socket s stype (Unix.ADDR_UNIX path) opts;
		| `Sock_inet(stype, addr, port) ->
		    let dom = Netsys.domain_of_inet_addr addr in
		    let s = Unix.socket dom stype 0 in
		    setup_socket s stype (Unix.ADDR_INET(addr,port)) opts;
		| `Sock_inet_byname(stype, name, port) ->
		    let addr = addr_of_name name in
		    let dom = Netsys.domain_of_inet_addr addr in
		    let s = Unix.socket dom stype 0 in
		    setup_socket s stype (Unix.ADDR_INET(addr,port)) opts;
	    in
	    let conn_eng =
	      if is_connected then (
		let status =
		  try 
		    Netsys.connect_check s;
		    `Done(`Socket(s, getsockspec stype s))
		  with
		    | error -> 
			`Error error in
		new epsilon_engine status ues
	      )
	      else (
		(* Now wait until the socket is writeable. Win32 reports connect
                   errors by signaling that out-of-band data can be received
                   (funny, right?), so we wait for that condition, too.
		 *)
		let e = new poll_engine [ Unixqueue.Wait_out s, (-1.0);
					  Unixqueue.Wait_oob s, (-1.0)
					] ues in
		new map_engine
		  ~map_done:(fun _ ->
			       try
				 Netsys.connect_check s;
				 `Done(`Socket(s, getsockspec stype s))
			       with
				 | error -> 
				     `Error error
			    )
		  (e :> Unixqueue.event engine)
	      ) in
	    (* It is possible that somebody aborts conn_eng. In this case,
             * the socket must be closed. Same when we enter an error state.
             *)
	    when_state
	      ~is_aborted:(fun () -> Unix.close s)
	      ~is_error:(fun _ -> Unix.close s)
	      conn_eng;
	    (* conn_eng is what the user sees: *)
	    conn_eng

	| `W32_pipe(mode,name) ->
	    let ph = Netsys_win32.pipe_connect name mode in
	    let s = Netsys_win32.pipe_descr ph in
	    let status = `Done(`W32_pipe s) in
	    let conn_eng = new epsilon_engine status ues in
	    (* It is possible that somebody aborts conn_eng. In this case,
             * the descr must be closed. The error state cannot be reached.
             *)
	    let close() =
	      Netsys_win32.pipe_shutdown ph;
	      Unix.close s
	    in
	    when_state
	      ~is_aborted:(fun () -> close())
	      conn_eng;
	    (* conn_eng is what the user sees: *)
	    conn_eng

	| _ ->
	    raise Addressing_method_not_supported
  end ;;


(* TODO: Close u, v on abort/error *)
(* TODO: port to Win32 *)
class command_connector () : client_endpoint_connector =
object(self)
  method connect connaddr ues =
    match connaddr with
	`Command (cmdstr,cmdcb) ->
          let (u,v) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in

	  Unix.set_nonblock u;
	  Unix.set_nonblock v;
          Netsys.set_close_on_exec u;
          Netsys.set_close_on_exec v;
 
          let (s_in_sub, s_in) = Unix.pipe() in
          let (s_out, s_out_sub) = Unix.pipe() in
          let _e1 = new copier (`Tridirectional(v, s_in, s_out)) ues in
 
	  Netsys.set_close_on_exec s_in;
          Netsys.set_close_on_exec s_out;

	  let e2 = new epsilon_engine (`Done ()) ues in
	    
	  new map_engine
	    ~map_done:(fun _ ->
			 let pid =
			   Unix.create_process
			     "/bin/sh"
			     [| "/bin/sh"; "-c"; cmdstr |]
			     s_in_sub s_out_sub Unix.stderr in
			 (* CHECK: Are the other descriptors closed? *)
			 Unix.close s_in_sub;
			 Unix.close s_out_sub;
			 cmdcb pid ues;
			 `Done (`Command(u, pid)))
	    e2

      | _ ->
	  raise Addressing_method_not_supported
end
;;

	    (* CHECK: Maybe we want a combinator for engines that:
	     * - Is only `Done when all engines are done
	     * - Goes to `Error when one engine is `Error
	     * - Goes to `Aborted when one engine is `Aborted
	     *
	     * See copier. It implements a similar idea.
	     *)

let connector ?proxy connaddr ues =
  let eff_proxy =
    match proxy with
	Some p -> ( p :> client_socket_connector )
      | None   -> 
	  ( match connaddr with
	      | `Socket _ 
	      | `W32_pipe _ ->
		  new direct_connector()
	      | `Command _ ->
		  new command_connector()
	  )
  in
  eff_proxy # connect connaddr ues
;;


type listen_address =
    [ `Socket of sockspec * listen_options
    | `W32_pipe of Netsys_win32.pipe_mode * string * listen_options
    ]

and listen_options =
    { lstn_backlog : int;
      lstn_reuseaddr : bool;
    }
;;


let default_listen_options =
  { lstn_backlog = 20;
    lstn_reuseaddr = false;
  }
;;


class type server_endpoint_acceptor = object
  method server_address : connect_address
  method multiple_connections : bool
  method accept : unit -> (Unix.file_descr * inetspec option) engine
  method shut_down : unit -> unit
end
;;

class type server_socket_acceptor = server_endpoint_acceptor



class type server_endpoint_listener = object
  method listen : listen_address ->
                  Unixqueue.event_system ->
		    server_endpoint_acceptor engine
end
;;


class type server_socket_listener = server_endpoint_listener


class direct_acceptor ?(close_on_shutdown=true) ?(preclose=fun()->())
                      fd ues : server_socket_acceptor =
  let fd_style = Netsys.get_fd_style fd in
  let pipe_objects = lazy (
    let psrv = Netsys_win32.lookup_pipe_server fd in
    let cn_ev = Netsys_win32.pipe_connect_event psrv in
    let cn_ev_descr = Netsys_win32.event_descr cn_ev in
    (psrv, cn_ev, cn_ev_descr)
  ) in
  let () =
    match fd_style with
      | `Read_write | `W32_pipe | `W32_event ->
	  failwith "Uq_engines.direct_acceptor: endpoint not supported"
      | `W32_pipe_server ->
	  ignore(Lazy.force pipe_objects)
      | _ -> () in
object(self)
  val mutable acc_engine = None
			     (* The engine currently accepting connections *)

  method server_address = 
    match fd_style with
      | `W32_pipe_server ->
	  let (psrv, _, _) = Lazy.force pipe_objects in
	  let name = Netsys_win32.pipe_server_name psrv in
	  let mode = Netsys_win32.pipe_server_mode psrv in
	  let mode' = Netsys_win32.rev_mode mode in
	  `W32_pipe(mode',name)
      | `Recv_send _ | `Recvfrom_sendto | `Recv_send_implied ->
	  `Socket(getsockspec Unix.SOCK_STREAM fd,
		  default_connect_options)
      | _ ->
	  assert false

  method multiple_connections = true

  method accept () =
    (* Poll until the socket becomes readable, then accept it *)
    if acc_engine <> None then
      failwith "Uq_engines.direct_acceptor: Already waiting for connection";

    let socket_accept_eng() =
      let eng = new poll_engine [ Unixqueue.Wait_in fd, (-1.0) ] ues in
      new map_engine
	~map_done:(fun _ ->
		     try
		       let (fd',_) = Unix.accept fd in
		       Unix.set_nonblock fd';
		       (* There seem to be buggy kernels out there where
                        * [accept] does not always return a connected socket.
                        * We get ENOTCONN for [getpeername] then.
                        * Who does that? The super hackers at SW-Soft with
                        * their Virtuozzo shit.
                        *)
		       let ps =
			 try getinetpeerspec Unix.SOCK_STREAM fd'
			 with
			   | Unix.Unix_error(Unix.ENOTCONN,_,_) as e ->
			       Unix.close fd';
			       raise e in
		       acc_engine <- None;
		       `Done(fd', ps)
		     with
		       | Unix.Unix_error( (Unix.EAGAIN | Unix.EINTR | 
					       Unix.ENOTCONN), _, _) ->
			   eng # restart();
			   `Working 0
		       | error ->
			   `Error error
		  )
	eng
    in

    let w32_pipe_accept_eng() =
      let (psrv, cn_ev, cn_ev_descr) = Lazy.force pipe_objects in
      let eng = 
	new poll_engine [ Unixqueue.Wait_in cn_ev_descr, (-1.0) ] ues in
      new map_engine
	~map_done:(fun _ ->
		     try
		       let pipe = Netsys_win32.pipe_accept psrv in
		       let pipe_fd = Netsys_win32.pipe_descr pipe in
		       acc_engine <- None;
		       `Done(pipe_fd, None)
		     with
		       | Unix.Unix_error( (Unix.EAGAIN | Unix.EINTR | 
					       Unix.ENOTCONN), _, _) ->
			   eng # restart();
			   `Working 0
		       | error ->
			   `Error error
		  )
	eng
    in

    let acc_eng = 
      match fd_style with
	| `Recv_send _ | `Recvfrom_sendto | `Recv_send_implied ->
	    socket_accept_eng()
	| `W32_pipe_server ->
	    w32_pipe_accept_eng() 
	| _ ->
	    assert false in
    when_state
      ~is_error:(fun x -> acc_engine <- None)
      ~is_aborted:(fun () -> acc_engine <- None)
      acc_eng;

    acc_engine <- Some acc_eng;
    acc_eng

  method shut_down() =
    if close_on_shutdown then (
      preclose();
      ( match fd_style with
	  | `Recv_send _ | `Recvfrom_sendto | `Recv_send_implied ->
	      Unix.close fd
	  | `W32_pipe_server ->
	      let (psrv, _, cn_ev_descr) = Lazy.force pipe_objects in
	      Unix.close cn_ev_descr;
	      Netsys_win32.pipe_shutdown_server psrv;
	      Unix.close fd
	  | _ ->
	      assert false
      )
    );
    (* else: if not close_on_shutdown, there is no portable way of
       achieving that further connection attempts are refused by the
       kernel. listen(fd,0) works on some systems, but not on all.
     *)
    match acc_engine with
	None -> 
	  ()
      | Some acc -> 
	  acc # abort()

end
;;


class direct_socket_acceptor fd esys = direct_acceptor fd esys


class direct_listener () : server_socket_listener =
object(self)
  method listen lstnaddr ues =
    let accept fd =
      let acc = new direct_acceptor fd ues in
      let eng = new epsilon_engine (`Done acc) ues in
      when_state
	~is_aborted:(fun () -> acc # shut_down())
	~is_error:(fun _ -> acc # shut_down())
	eng;
      eng
    in

    match lstnaddr with
	`Socket (sockspec, opts) ->
	  ( match sockspec with
		`Sock_unix(stype, path) ->
		  let s = Unix.socket Unix.PF_UNIX stype 0 in
		  Unix.set_nonblock s;
		  if opts.lstn_reuseaddr then 
		    Unix.setsockopt s Unix.SO_REUSEADDR true;
		  Unix.bind s (Unix.ADDR_UNIX path);
		  Unix.listen s opts.lstn_backlog;
		  accept s
	      | `Sock_inet(stype, addr, port) ->
		  let dom = Netsys.domain_of_inet_addr addr in
		  let s = Unix.socket dom stype 0 in
		  Unix.set_nonblock s;
		  if opts.lstn_reuseaddr then 
		    Unix.setsockopt s Unix.SO_REUSEADDR true;
		  Unix.bind s (Unix.ADDR_INET(addr,port));
		  Unix.listen s opts.lstn_backlog;
		  accept s
	      | `Sock_inet_byname(stype, name, port) ->
		  let addr = addr_of_name name in
		  let dom = Netsys.domain_of_inet_addr addr in
		  let s = Unix.socket dom stype 0 in
		  Unix.set_nonblock s;
		  if opts.lstn_reuseaddr then 
		    Unix.setsockopt s Unix.SO_REUSEADDR true;
		  Unix.bind s (Unix.ADDR_INET(addr,port));
		  Unix.listen s opts.lstn_backlog;
		  accept s
	  )
      | `W32_pipe (mode, name, opts) ->
	  let backlog = opts.lstn_backlog in
	  let psrv = Netsys_win32.create_local_pipe_server name mode max_int in
	  Netsys_win32.pipe_listen psrv backlog;
	  let fd = Netsys_win32.pipe_server_descr psrv in
	  accept fd
      | _ ->
	  raise Addressing_method_not_supported
end
;;


let listener ?proxy lstnaddr ues =
  let eff_proxy =
    match proxy with
	Some p -> ( p :> server_socket_listener )
      | None   -> 
	  ( match lstnaddr with
	      | `Socket _ | `W32_pipe _ ->
		  new direct_listener()
	  )
  in
  eff_proxy # listen lstnaddr ues
;;


type datagram_type =
    [ `Unix_dgram
    | `Inet_udp
    | `Inet6_udp
    ]
;;


class type wrapped_datagram_socket =
object
  method descriptor : Unix.file_descr
  method sendto : 
    string -> int -> int -> Unix.msg_flag list -> sockspec -> int
  method recvfrom : 
    string -> int -> int -> Unix.msg_flag list -> (int * sockspec)
  method shut_down : unit -> unit
  method datagram_type : datagram_type
  method socket_domain : Unix.socket_domain
  method socket_type : Unix.socket_type
  method socket_protocol : int
end;;


class type datagram_socket_provider =
object
  method create_datagram_socket : datagram_type ->
                                  Unixqueue.event_system ->
                                    wrapped_datagram_socket engine
end ;;


class direct_datagram_socket dgtype (sdom,stype,sproto) 
      : wrapped_datagram_socket =
  let sock = Unix.socket sdom stype sproto in
  let _ = 
    Unix.set_nonblock sock;
    Netsys.set_close_on_exec sock in
object(self)
  method descriptor = sock
  method sendto s p n flags spec = 
    let sockaddr =
      match spec with
	  `Sock_unix(stype', path) ->
	    if stype <> stype' then invalid_arg "Socket type mismatch";
	    Unix.ADDR_UNIX path
	| `Sock_inet(stype', addr, port) ->
	    if stype <> stype' then invalid_arg "Socket type mismatch";
	    Unix.ADDR_INET(addr,port)
	| `Sock_inet_byname(stype', name, port) ->
	    if stype <> stype' then invalid_arg "Socket type mismatch";
	    let addr = addr_of_name name in
	    Unix.ADDR_INET(addr,port)
    in
    Unix.sendto sock s p n flags sockaddr

  method recvfrom s p n flags =
    let (n, sockaddr) = Unix.recvfrom sock s p n flags in
    let sockspec = 
      match sockaddr with
	  Unix.ADDR_UNIX path ->
	    `Sock_unix(stype, path)
	| Unix.ADDR_INET(addr,port) ->
	    `Sock_inet(stype, addr, port)
    in
    (n,sockspec)

  method shut_down() =
    Unix.close sock

  method datagram_type = dgtype
  method socket_domain = sdom
  method socket_type = stype
  method socket_protocol = sproto
end ;;


let datagram_provider ?proxy dgtype ues = 
  match proxy with
      Some p ->
	( p :> datagram_socket_provider ) # create_datagram_socket dgtype ues
    | None   -> 
	let (sdom,stype,sproto) =
	  match dgtype with
	      `Unix_dgram -> (Unix.PF_UNIX, Unix.SOCK_DGRAM, 0)
	    | `Inet_udp   -> (Unix.PF_INET, Unix.SOCK_DGRAM, 0)
	    | `Inet6_udp  -> (Unix.PF_INET6, Unix.SOCK_DGRAM, 0)
	in
	let wsock =
	  new direct_datagram_socket dgtype (sdom,stype,sproto) in
	let eng = new epsilon_engine (`Done wsock) ues in

	when_state
	  ~is_aborted:(fun () -> wsock # shut_down())
	  ~is_error:(fun _ -> wsock # shut_down())
	  eng;

	eng
;;


module Operators = struct
  let ( ++ ) = seq_engine
  let ( >> ) = fmap_engine
  let eps_e = epsilon_engine
end
