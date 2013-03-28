(* $Id: nethttpd_engine.ml 1590 2011-05-05 11:30:08Z gerd $
 *
 *)

(*
 * Copyright 2005 Baretta s.r.l. and Gerd Stolpmann
 *
 * This file is part of Nethttpd.
 *
 * Nethttpd is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Nethttpd is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Nethttpd; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Nethttpd_engine" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Nethttpd_engine" Debug.enable

let () =
  Netlog.Debug.register_module "Nethttpd_engine" Debug.enable



open Nethttp
open Nethttp.Header
open Nethttpd_types
open Nethttpd_kernel
open Printf

type engine_req_state =
    [ `Received_header
    | `Receiving_body
    | `Received_request 
    | `Finishing
    ]


class type http_engine_config =
object
  inherit Nethttpd_reactor.http_processor_config
  method config_input_flow_control : bool
  method config_output_flow_control : bool
end


class type extended_async_environment =
object
  inherit extended_environment
  method input_ch_async : Uq_engines.async_in_channel
  method output_ch_async : Uq_engines.async_out_channel
end


class type http_request_header_notification =
object
  method req_state : engine_req_state
  method environment : extended_async_environment
  method schedule_accept_body : on_request:(http_request_notification -> unit) ->
                               ?on_error:(unit -> unit) -> unit -> unit
  method schedule_reject_body : on_request:(http_request_notification -> unit) ->
                               ?on_error:(unit -> unit) -> unit -> unit
  method schedule_finish : unit -> unit
end

and http_request_notification =
object
  method req_state : engine_req_state
  method environment : extended_async_environment
  method schedule_finish : unit -> unit
end


type conn_state =
    [ `Active of http_protocol
    | `Closing of lingering_close
    | `Closed
    ]


type reset_cond = unit -> unit


exception Ev_output_filled of Unixqueue.group * reset_cond
  (** condition: The output channel filled data into the [http_response] object, and
    * notifies now the [http_engine] 
   *)
 (* FIXME: This event is also sent for response objects that are
    still buffering but not yet writing to the output descriptor.
    This is not harmful, because it means this event is sent too often.
    This is inelegant, though, and might be a performance problem.
  *)


exception Ev_input_empty of Unixqueue.group * reset_cond
  (** condition: The input channel became empty, and the engine must be notified 
    * (used for input flow control)
   *)


class condition ues mk_ev =
object(self)
  val mutable signaled = false
    (* Records whether there is [ev] on the event queue. As [ev] is used
       only for expressing a condition, it is nonsense to add [ev] twice to
       the queue.
     *)

  val mutable ev = lazy(assert false)

  initializer (
    let ev0 = mk_ev self#reset in
    ev <- lazy ev0
  )

  method signal() =
    if not signaled then (
      Unixqueue.add_event ues (Lazy.force ev);
      signaled <- true
    )

  method reset() =
    (* To be called when [ev] is consumed by the event handler *)
    signaled <- false
end
    


class http_engine_input config ues group in_cnt fdi rqid =
  let cond_input_empty =
    new condition ues 
      (fun reset -> Unixqueue.Extra(Ev_input_empty(group,reset))) in
object(self)
  (* The input channel is fed with data by the main event handler that invokes
   * [add_data] and [add_eof] to forward new input data to this channel.
   *)

  val mutable front = None
  val mutable data_queue = Queue.create()
  val mutable eof = false
  val mutable pos_in = 0
  val mutable closed = false
  val mutable locked = true
  val mutable aborted = false
  val mutable notify_list = []
  val mutable notify_list_new = []

  method cond_input_empty = 
    cond_input_empty

  method input s spos slen =
    dlogr (fun () -> sprintf "FD %Ld req-%d: input.input" fdi !rqid);
    if closed then raise Netchannels.Closed_channel;
    if locked then failwith "Nethttpd_engine: channel is locked";
    if aborted then failwith "Nethttpd_engine: channel aborted";
    (match front with
       | None ->
	   ( try
	       front <- Some(Queue.take data_queue)
	     with
		 Queue.Empty -> ()
	   )
       | Some _ -> ()
    );
    (match front with
       | None ->
	   if eof then 
	     raise End_of_file
	   else
	     0    (* buffer underrun *)
       | Some (u,upos,ulen) ->
	   let len = min slen ulen in
	   String.blit u upos s spos len;
	   if len = ulen then
	     front <- None
	   else
	     front <- Some(u,upos+len,ulen-len);
	   pos_in <- pos_in + len;
	   if not (self # can_input) then (
	     if config#config_input_flow_control then
	       cond_input_empty # signal();
	     self # notify();
	   );
	   len
    )

  method pos_in =
    pos_in

  method close_in() =
    dlogr (fun () -> sprintf "FD %Ld req-%d: input.close_in" fdi !rqid);
    if not closed then (
      if locked then failwith "Nethttpd_engine: channel is locked";
      front <- None;
      Queue.clear data_queue;
      closed <- true
    )

  method can_input =
    not closed && (front <> None || not(Queue.is_empty data_queue) || eof)

  method request_notification f =
    notify_list_new <- f :: notify_list_new

  method private notify() =
    notify_list <- notify_list @ notify_list_new;
    notify_list_new <- [];
    notify_list <- List.filter (fun f -> f()) notify_list

  method add_data ((_,_,len) as data_chunk) =
    dlogr (fun () -> sprintf "FD %Ld req-%d: input.add_data" fdi !rqid);
    assert(len > 0);
    if not eof then (
      let old_can_input = self # can_input in
      Queue.push data_chunk data_queue;
      in_cnt := Int64.add !in_cnt (Int64.of_int len);
      if not old_can_input && not closed then self # notify()
    )
      (* else: we are probably dropping all data! *)

  method add_eof() =
    dlogr (fun () -> sprintf "FD %Ld req-%d: input.add_eof" fdi !rqid);
    if not eof then (
      let old_can_input = self # can_input in
      eof <- true;
      if not old_can_input && not closed then self # notify()
    )
    
  method unlock() =
    dlogr (fun () -> sprintf "FD %Ld req-%d: input.unlock" fdi !rqid);
    locked <- false

  method drop() =
    dlogr (fun () -> sprintf "FD %Ld req-%d: input.drop" fdi !rqid);
    locked <- false;
    eof <- true

  method abort() =
    dlogr (fun () -> sprintf "FD %Ld req-%d: input.abort" fdi !rqid);
    aborted <- true

end


class http_engine_output config ues group resp output_state 
                         (f_access : unit->unit) fdi =
  let cond_output_filled =
    new condition ues 
      (fun reset -> Unixqueue.Extra(Ev_output_filled(group,reset))) in
object(self)
  (* The output channel adds the incoming data to the [http_response] object
   * [resp]. The main [http_engine] is notified about new data by a
   * [Ev_output_filled] event. This gives the [http_engine] has the chance to
   * check whether it again enables output because new data is to be output.
   *
   * Note that [resp] is setup such that it calls [resp_notify] whenever the state
   * of [resp] changes, or the [resp] queue becomes empty. We do not immediately
   * forward this notification, but delay it a bit by pusing the invocation
   * onto the event queue. This indirection
   * is necessary because the moment of the [resp_notify] invocation is quite
   * hairy, and failures must not be risked.
   *)

  val mutable pos_out = 0
  val mutable closed = false
  val mutable locked = true
  val mutable aborted = false
  val mutable notify_list = []
  val mutable notify_list_new = []

  initializer (
    resp # set_callback self#resp_notify
  )

  method cond_output_filled = cond_output_filled

  method output s spos slen =
    (* In principle we always accept any amount of output data. For practical reasons,
     * the length of an individual chunk is limited to 8K - it just prevents
     * some dumb programming errors.
     *)
    dlogr (fun () -> sprintf "FD %Ld resp-%d: output.output" fdi (Oo.id resp));
    if closed then raise Netchannels.Closed_channel;
    if locked then failwith "Nethttpd_engine: channel is locked";
    if aborted then failwith "Nethttpd_engine: channel aborted";
    if !output_state <> `Sending then
      failwith "output channel: cannot output now";
    let len = min slen 8192 in
    if len > 0 then (
      let old_can_output = self # can_output in    
      let u = String.sub s spos len in
      resp # send (`Resp_body(u,0,String.length u));
      cond_output_filled # signal();
      pos_out <- pos_out + len;
      if old_can_output <> self # can_output then self # notify();
    );
    len

  method pos_out =
    pos_out

  method flush() =
    dlogr (fun () -> sprintf "FD %Ld resp-%d: output.flush" fdi (Oo.id resp));
    ()

  method close_out() =
    dlogr (fun () -> 
	     sprintf "FD %Ld resp-%d: output.close_out" fdi (Oo.id resp));
    if not closed then (
      if locked then failwith "Nethttpd_engine: channel is locked";
      let old_can_output = self # can_output in
      resp # send `Resp_end;
      closed <- true;
      output_state := `End;
      f_access();
      cond_output_filled # signal();
      if old_can_output <> self # can_output then self # notify();
    )

  method close_after_send_file() =
    dlogr (fun () -> sprintf "FD %Ld resp-%d: output.close_after_send_file" 
	     fdi (Oo.id resp));
    closed <- true;
    f_access();
    
  method can_output =
    (not config#config_output_flow_control) ||
    ((resp # state = `Active) && resp # send_queue_empty)

  method unlock() =
    dlogr (fun () -> sprintf "FD %Ld resp-%d: output.unlock" fdi (Oo.id resp));
    locked <- false

  method resp_notify() =
    Unixqueue.once ues group 0.0 self#notify
    (* CHECK: It is assumed that self#notify is called before the condition
     * is again invalid.
     * If this turns out to be wrong, we have to implement a quicker way of
     * notification. It is known that [resp_notify] is called back from the
     * current [cycle]. One could check after every [cycle] whether the
     * notification is still valid.
     *)
    
  method request_notification f =
    notify_list_new <- f :: notify_list_new

  method private notify() =
    notify_list <- notify_list @ notify_list_new;
    notify_list_new <- [];
    notify_list <- List.filter (fun f -> f()) notify_list

  method abort() =
    dlogr (fun () -> sprintf "FD %Ld resp-%d: output.abort" fdi (Oo.id resp));
    aborted <- true

end


class http_async_environment config ues group
                             ((req_meth, req_uri), req_version) req_hdr 
                             fd_addr peer_addr

                             in_ch_async in_cnt out_ch_async output_state
                             resp reqrej fdi =
  let in_ch = 
    Netchannels.lift_in ~buffered:false 
                        (`Raw (in_ch_async :> Netchannels.raw_in_channel)) in

  let out_ch =
    Netchannels.lift_out ~buffered:false
                         (`Raw (out_ch_async :> Netchannels.raw_out_channel)) in

  (* [in_ch] and [out_ch] are standard channels corresponding to [in_ch_async] and
   * [out_ch_async]. Note that there is no buffering. A buffer would introduce
   * a delay between the standard and the asynchronous channels - a very surprising
   * effect. Furthermore, there is already a lot of buffering in [http_protocol],
   * so buffers are probably not needed here.
   *)

object (self)
  inherit Nethttpd_reactor.http_environment 
                             config
                             req_meth req_uri req_version req_hdr 
  			     fd_addr peer_addr
                             in_ch in_cnt out_ch output_state resp 
			     out_ch_async#close_after_send_file reqrej fdi
			     as super

  method input_ch_async = (in_ch_async :> Uq_engines.async_in_channel)
  method output_ch_async = (out_ch_async :> Uq_engines.async_out_channel)

  method send_output_header() =
    let ok = !output_state = `Start in
    super # send_output_header();
    if ok then
      out_ch_async # cond_output_filled # signal()

  method send_file fd length =
    super # send_file fd length;
      (* does not block, because [synch] is now [ fun () -> () ] *)
    out_ch_async # cond_output_filled # signal()
    

end


class http_request_manager config ues group req_line req_hdr expect_100_continue 
                           fd_addr peer_addr resp fdi =
  let f_access = ref (fun () -> ()) in  (* set below *)
  let in_cnt = ref 0L in
  let reqrej = ref false in
  let rqid = ref (-1) in

  let output_state = ref `Start in
  let in_ch = new http_engine_input config ues group in_cnt fdi rqid in
  let out_ch = new http_engine_output config ues group resp output_state
                 (fun () -> !f_access()) fdi in

  let env = new http_async_environment 
	      config ues group req_line req_hdr fd_addr peer_addr 
	      in_ch in_cnt
	      out_ch output_state resp reqrej fdi in
     (* may raise Standard_response! *)

  let () =
    f_access := env # log_access in


object(self)
  (* This class also satisfies the types [http_request_notification] and
   * [http_request_header_notification]
   *)

  initializer (
    dlogr (fun () -> sprintf "FD=%Ld: new request_manager req-%d resp-%d"
	     fdi (Oo.id self) (Oo.id resp));
    rqid := (Oo.id self)
  )

  val mutable req_state = ( `Received_header : engine_req_state )
    (* When this object is created, the header has just been received *)  

  val mutable req_handler = (fun _ -> failwith "Nethttpd_engine: No [on_request] function")

  val mutable error_handler = (fun () -> ())

  method real_input_ch = in_ch    (* full type! *)
  method real_output_ch = out_ch  (* full type! *)
  method environment = (env :> extended_async_environment)
  method req_state = req_state
  method set_req_state s = req_state <- s
  method req_handler = (req_handler : http_request_notification -> unit)
  method error_handler = error_handler

  method log_access = env#log_access

  method abort() =
    dlogr (fun () -> sprintf "FD %Ld req-%d: abort" fdi (Oo.id self));
    in_ch # abort();
    out_ch # abort();

  method schedule_accept_body ~on_request ?(on_error = fun ()->()) () =
    dlogr (fun () -> sprintf "FD %Ld req-%d: accept_body" fdi (Oo.id self));
    (* Send the "100 Continue" response if requested: *)
    if expect_100_continue then (
      resp # send resp_100_continue;
      out_ch # cond_output_filled # signal();
    );
    (* Unlock everything: *)
    in_ch # unlock();
    out_ch # unlock();
    env # unlock();
    (* Remember the callback functions: *)
    req_handler <- on_request;
    error_handler <- on_error
      

  method schedule_reject_body ~on_request ?(on_error = fun ()->()) () =
    dlogr (fun () -> sprintf "FD %Ld req-%d: reject_body" fdi (Oo.id self));
    (* Unlock everything: *)
    in_ch # drop();
    out_ch # unlock();
    env # unlock();
    reqrej := true;
    (* Remember the callback functions: *)
    req_handler <- on_request;
    error_handler <- on_error

  method schedule_finish() =
    (* This is quite tricky:
     * - Any remaining data in the input channel is dropped. The [drop] method
     *   does this. This has also the effect that any additional data still
     *   arriving is thrown away.
     * - We have to check the output state for the response. If it is still `Start,
     *   the whole response is missing. We generate a "Server Error" in this case.
     *   Otherwise we just close the output channel and hope we are done.
     * - We also set [req_state] to `Finishing to inform all other parts of the
     *   engine what is going on.
     *)
    dlogr (fun () -> sprintf "FD %Ld req-%d: schedule_finish" fdi (Oo.id self));
    in_ch # drop();
    out_ch # unlock();
    env # unlock();
    req_state <- `Finishing;
    match !(env # output_state) with
      | `Start ->
	  (* The whole response is missing! Generate a "Server Error": *)
	  dlogr (fun () -> 
		   sprintf "FD %Ld req-%d: missing response error" 
		     fdi (Oo.id self));
	  output_std_response config env `Internal_server_error None
	    (Some "Nethttpd: Missing response, replying 'Server Error'");
	  (env # output_state) := `End;
      | `Sending ->
	  (* The response body is probably incomplete or missing. Try to close
	   * the channel.
	   *)
	  ( try env # output_ch # close_out() with Netchannels.Closed_channel -> () );
	  (env # output_state) := `End;
      | `End ->
	  (* Everything ok, just to be sure... *)
	  ( try env # output_ch # close_out() with Netchannels.Closed_channel -> () )
end


let ensure_not_reentrant name lock f arg =
  if !lock then (
    prerr_endline ("Illegal reentrancy: " ^ name);
    assert false
  );
  lock := true;
  try 
    let r = f arg in
    lock := false;
    r
  with err ->
    lock := false;
    raise err


class http_engine ~on_request_header () config fd ues =
  (* note that "new http_engine" can already raise exceptions, e.g.
     Unix.ENOTCONN
   *)
  let _proto = new http_protocol config fd in
  let handle_event_lock = ref false in
  let fdi = Netsys.int64_of_file_descr fd in
object(self)
  inherit [unit] Uq_engines.engine_mixin (`Working 0) ues

  val fd_addr = Unix.getsockname fd
  val peer_addr = Netsys.getpeername fd

  val mutable conn_state = (`Active _proto : conn_state)
  val mutable group = Unixqueue.new_group ues

  val mutable enable_in = false
  val mutable in_timeout = 0.0

  val mutable enable_out = false
  (* - When to listen for input events? In principle, when [proto # do_input]
   *   indicates this. This flag can only change after every [proto # cycle].
   * - When to listen for output events? In principle, when [proto # do_output]
   *   indicates this. This flag can change after [proto # cycle], but also
   *   after new data have been output. Our output channel will tell us, and
   *   sends [Ev_output_filled] events to us.
   *)

  val mutable cur_request_manager = None
  val mutable eof_seen = false

  initializer (
    self # start();
    Netlog.Debug.track_fd   (* configure tracking as last step of init *)
      ~owner:"Nethttpd_engine"
      ~descr:(sprintf "HTTP %s->%s"
		(Netsys.string_of_sockaddr peer_addr)
		(Netsys.string_of_sockaddr fd_addr))
      fd
  )

  method private start() =
    Unixqueue.add_handler ues group (fun _ _ -> self # handle_event);
    Unixqueue.add_abort_action ues group (fun _ -> self # handle_exception);
    self # enable_input true

  method private enable_input flag =
    let timeout =
      match conn_state with
	| `Active proto ->
	    ( match proto # input_timeout_class with
		| `None         -> (-1.0)
		| `Normal       -> config#config_timeout
		| `Next_message -> config#config_timeout_next_request
	    )
	| `Closing lc ->
	    1.0  (* i.e. check every second whether the lingering phase is over *)
	| `Closed ->
	    assert false 
    in
    ( match (flag, enable_in) with
	| (true, false) ->
	    Unixqueue.add_resource ues group (Unixqueue.Wait_in fd, timeout);
	| (false, true) ->
	    Unixqueue.remove_resource ues group (Unixqueue.Wait_in fd);
	| (true, true) when timeout <> in_timeout ->
	    Unixqueue.remove_resource ues group (Unixqueue.Wait_in fd);
	    Unixqueue.add_resource ues group (Unixqueue.Wait_in fd, timeout);
	| _ -> ()
    );
    enable_in <- flag;
    in_timeout <- timeout;

  method private enable_output flag =
    if flag && not enable_out then
      Unixqueue.add_resource ues group (Unixqueue.Wait_out fd, config#config_timeout);
    if not flag && enable_out then
      Unixqueue.remove_resource ues group (Unixqueue.Wait_out fd);
    enable_out <- flag

  method private handle_event ev =
    ensure_not_reentrant
      "Nethttpd_engine.http_engine#handle_event"
      handle_event_lock
      (fun () ->
	 match ev with
	   | Unixqueue.Input_arrived(g,_) when g = group ->
	       (* Input data from the HTTP client *)
	       ( match conn_state with
		   | `Active proto -> self # cycle_proto proto
		   | `Closing lc   -> self # cycle_lc lc
		   | `Closed       -> ()   (* drop this event *)
	       );
	       self # count()
	   | Unixqueue.Output_readiness(g,_) when g = group ->
	       (* The HTTP client accepts new data *)
	       ( match conn_state with
		   | `Active proto -> self # cycle_proto proto
		   | `Closing lc   -> ()  (* Strange. Ignore for now. *)
		   | `Closed       -> ()  (* drop this event *)
	       );
	       self # count()
	   | Unixqueue.Timeout(g,_) when g = group ->
	       (* Register a timeout *)
	       ( match conn_state with
		   | `Active proto -> self # timeout_proto proto
		   | `Closing lc   -> self # cycle_lc lc
		   | `Closed       -> ()  (* drop this event *)
	       )
	   | Unixqueue.Extra (Ev_output_filled(g,reset)) when g = group ->
	       (* The output channel is filled with fresh data *)
	       reset();
	       ( match conn_state with
		   | `Active proto -> (* Check whether to enable output now: *)
		       self # enable_output proto#do_output
		   | `Closing lc   -> ()  (* drop this event *)
		   | `Closed       -> ()  (* drop this event *)
	       )
	   | Unixqueue.Extra (Ev_input_empty(g,reset)) when g = group ->
	       (* The input channel is empty. CHECK: why is this a no-op? *)
	       reset();
	       ( match conn_state with
		   | `Active proto -> ()
		   | `Closing lc   -> ()  (* drop this event *)
		   | `Closed       -> ()  (* drop this event *)
	       )
	   | _ ->
	       raise Equeue.Reject   (* Other engines may want to see this event *)
      )
      ()

  method private count() =
    match self#state with
      | `Working n -> self # set_state(`Working(n+1))
      | _ -> ()

  method private cycle_proto proto =
    (* Do a HTTP protocol cycle, and check whether I/O is still enabled *)
    dlogr (fun () -> sprintf "FD %Ld: cycle" (Netsys.int64_of_file_descr fd));
    proto # cycle();   (* do not block! *)
    self # goon_proto proto

  method private timeout_proto proto =
    (* Either input or output has timed out. *)
    dlogr (fun () -> sprintf "FD %Ld: timeout"
	     (Netsys.int64_of_file_descr fd));
    proto # timeout();
    self # goon_proto proto

  method private goon_proto proto =
    dlogr (fun () -> sprintf "FD %Ld: go on" (Netsys.int64_of_file_descr fd));
    let enabled_input_flow =
      (not config#config_input_flow_control) || (
	(* Input flow control: We stop reading from the descriptor when there are
	 * unprocessed request tokens.
	 *)
	match cur_request_manager with
	  | None ->
	      true
		(* CHECK: This might be nonsense. It is possible that in the
		 * last request manager the input channel has unprocessed data.
		 * Don't know how to handle this. (I don't think it is possible
		 * to attack the server because of this issue, because the
		 * pipeline length option limits the number of unresponded
		 * requests anyway.)
		 *)
	  | Some rm ->
	      (* If the input channel is empty, we enable input, and vice versa: *)
	      not(rm # environment # input_ch_async # can_input)
      )
    in
    self # forward_input_tokens proto;
    self # enable_input (enabled_input_flow && proto#do_input);
    self # enable_output proto#do_output;
    (* Check whether the HTTP connection is processed and can be closed: *)
    if eof_seen && not proto#do_output then (
      if proto # need_linger then (
	(* FIXME: It is strange to check here for a lingering close. This
	   should never be necessary after getting an EOF from the client
	 *)
	dlogr (fun () -> sprintf "FD %Ld: lingering"
		 (Netsys.int64_of_file_descr fd));
	let lc = 
	  new lingering_close 
	    ~preclose:(fun () -> Netlog.Debug.release_fd fd) 
	    fd in
	conn_state <- `Closing lc;
	self # enable_input true;
	self # enable_output false;
      )
      else (
	(* Just close the descriptor and shut down the engine: *)
	dlogr (fun () -> sprintf "FD %Ld: closing"
		 (Netsys.int64_of_file_descr fd));
	Netlog.Debug.release_fd fd;
	conn_state <- `Closed;
	Unix.close fd;
	Unixqueue.clear ues group;    (* Stop in Unixqueue terms *)
	self # set_state (`Done());   (* Report the new state *)
      )
    )

  method private receive proto =
    let tok = proto # receive() in
    dlogr (fun () -> sprintf "FD %Ld: next token: %s"
	     (Netsys.int64_of_file_descr fd)
	     (Nethttpd_kernel.string_of_req_token tok)
	  );
    tok

  method private forward_input_tokens proto =
    (* Interpret all available input tokens: *)
    dlogr (fun () -> sprintf "FD %Ld: forward_input_tokens"
	     (Netsys.int64_of_file_descr fd));
    while proto # recv_queue_len > 0 do
      match self # receive proto with
	| `Req_header(req_line, req_hdr, resp) ->
	    (* The next request starts. *)
	    assert(cur_request_manager = None);
	    let expect_100_continue =
	      try
		proto # peek_recv() = `Req_expect_100_continue
	      with
		  Recv_queue_empty -> false in
	    if expect_100_continue then
	      ignore(self # receive proto);

	    let f_access = ref (fun () -> ()) in

	    ( try
		let rm = new http_request_manager    (* or Standard_response *)
			   config ues group
			   req_line req_hdr expect_100_continue 
			   fd_addr peer_addr resp fdi in
		
		f_access := rm # log_access;
		cur_request_manager <- Some rm;
	    
		dlogr (fun () -> 
			 sprintf "FD %Ld: got request req-%d, notifying user"
			 (Netsys.int64_of_file_descr fd) (Oo.id rm));

		(* Notify the user that we have received the header: *)
		let () =
		  on_request_header (rm :> http_request_header_notification) in
  	           (* Note: the callback may raise an arbitrary exception *)

		dlogr (fun () -> 
			 sprintf "FD %Ld: back from user for req-%d"
			 (Netsys.int64_of_file_descr fd) (Oo.id rm))

	      with
		| Standard_response(status, hdr_opt, msg_opt) ->
		    (* Probably a problem when decoding a header field! *)
		    dlogr (fun () -> 
			     sprintf "FD %Ld: got request -> std response"
			       (Netsys.int64_of_file_descr fd));
		    let (req_meth, req_uri) = fst req_line in
		    let (in_cnt,reqrej,env_opt) =
		      match cur_request_manager with
			| Some rm -> 
			    let env = 
			      (rm # environment :> extended_environment) in
			    (env#input_body_size,
			     env#request_body_rejected,
			     Some env)
			| None ->
			    (0L, false, None) in
		    Nethttpd_reactor.logged_error_response
		      fd_addr peer_addr (Some(req_meth,req_uri))
		      in_cnt reqrej status (Some req_hdr)
		      msg_opt env_opt 
		      (Some resp) 
		      (config :> Nethttpd_reactor.http_processor_config);
		    Unixqueue.add_event ues
		      (Unixqueue.Extra(Ev_output_filled(group,(fun () -> ()))));
		    (* Now [cur_request_manager = None]. This has the effect that 
	 	     * all further input tokens for this request are silently
		     * dropped.
		     *)
	    )
	    
	| `Req_expect_100_continue ->
	    assert false   (* already processed *)

	| `Req_body data_chunk ->
	    (* Just forward data to the current request manager: *)
	    dlogr (fun () -> 
		     sprintf "FD %Ld: got body data"
		       (Netsys.int64_of_file_descr fd));
	    ( match cur_request_manager with
		| Some rm ->
		    if rm # req_state <> `Finishing then
		      rm # set_req_state `Receiving_body;
		    rm # real_input_ch # add_data data_chunk
		| None -> ()  (* drop it *)
	    )

	| `Req_trailer _ ->
	    (* Don't know what to do with the trailer. *)
	    dlogr (fun () -> 
		     sprintf "FD %Ld: got trailer"
		       (Netsys.int64_of_file_descr fd));
	    ()

	| `Req_end ->
	    (* Just forward this event to the current request manager: *)
	    dlogr (fun () -> 
		     sprintf "FD %Ld: got request end"
		       (Netsys.int64_of_file_descr fd));
	    ( match cur_request_manager with
		| Some rm ->
		    dlogr (fun () -> 
			     sprintf "FD %Ld: req end for req-%d"
			       (Netsys.int64_of_file_descr fd) (Oo.id rm));
		    cur_request_manager <- None;
		    ( match rm # req_state with
			| `Finishing ->
			    (* The request has been dropped, so call the error
			     * handler.
			     *)
			    rm # error_handler()
			| _ ->
			    (* Normal end of request: *)
			    rm # set_req_state `Received_request;
			    rm # real_input_ch # add_eof ();
			    (* Now call the function given by the [on_request] argument: *)
			    rm # req_handler (rm :> http_request_notification);
		            (* Note: the callback may raise an arbitrary exception *)
		    )
		| None -> ()   (* drop *)
	    );

	| `Eof ->
	    (* If there is still a request manager, finish the current request. *)
	    dlogr (fun () -> 
		     sprintf "FD %Ld: got eof"
		       (Netsys.int64_of_file_descr fd));
	    ( match cur_request_manager with
		| Some rm ->
		    cur_request_manager <- None;
		    ( match rm # req_state with
			| `Received_request -> 
			    (* This is impossible! *)
			    assert false
			| `Finishing ->
			    (* The current request has not been arrived completely.
			     * It has been dropped, so call the error handler.
			     *)
			    rm # error_handler()
			| _ ->
			    (* Same as before, but the request was not yet properly
			     * finished.
			     *)
			    rm # schedule_finish();
			    rm # error_handler()
		    );
		| None -> ()
	    );
	    (* Record this event. Will be checked in [cylce_proto] *)
	    eof_seen <- true

	| `Fatal_error e ->
	    (* The connection is already down. Just log the incident: *)
	    dlogr (fun () -> 
		     sprintf "FD %Ld: got fatal error"
		       (Netsys.int64_of_file_descr fd));
	    if e <> `Broken_pipe_ignore then (
	      let msg = Nethttpd_kernel.string_of_fatal_error e in
	      Nethttpd_reactor.logged_error_response
		fd_addr peer_addr None 0L false `Internal_server_error
		None (Some msg) None None 
		(config :> Nethttpd_reactor.http_processor_config)
	      (* Note: The kernel ensures that the following token will be [`Eof].
	       * Any necessary cleanup will be done when [`Eof] is processed.
	       *)
	    )
	    
	| `Bad_request_error (e, resp) ->
	    (* Log the incident, and reply with a 400 response. There isn't any
	     * request manager, because `Bad_request_error replaces `Req_header
	     * when bad requests arrive.
	     *)
	    dlogr (fun () -> 
		     sprintf "FD %Ld: got bad request"
		       (Netsys.int64_of_file_descr fd));
	    assert(cur_request_manager = None);
	    let msg = string_of_bad_request_error e in
	    let status = status_of_bad_request_error e in
	    Nethttpd_reactor.logged_error_response
	      fd_addr peer_addr None 0L false status None (Some msg) None
	      (Some resp) (config :> Nethttpd_reactor.http_processor_config);
	    Unixqueue.add_event ues
	      (Unixqueue.Extra(Ev_output_filled(group,(fun () -> ()))));
	    (* Note: The kernel ensures that the following token will be [`Eof].
	     * Any necessary cleanup will be done when [`Eof] is processed.
	     *)

	| `Timeout -> 
	    (* A non-fatal timeout. Always followed by [`Eof] *)
	    dlogr (fun () -> 
		     sprintf "FD %Ld: got timeout"
		       (Netsys.int64_of_file_descr fd));
	    ()
    done


  method private cycle_lc lc =
    (* Do a cycle of the [lc] engine. *)
    dlogr (fun () -> 
	     sprintf "FD %Ld: cycle_lc"
	       (Netsys.int64_of_file_descr fd));
    lc # cycle();     (* do not block! *)
    let cont = lc # lingering in
    self # enable_output false;
    self # enable_input cont;
    if not cont then (
      (* Now stop the whole engine! *)
      dlogr (fun () -> 
	       sprintf "FD %Ld: cycle_lc transitioning to Done"
		 (Netsys.int64_of_file_descr fd));
      conn_state <- `Closed;
      Unixqueue.clear ues group;    (* Stop in Unixqueue terms *)
      self # set_state (`Done());   (* Report the new state *)
    )

  method private handle_exception err =
    (* In general this should not happen. The HTTP kernel already handles all kinds
     * of I/O errors. This means all remaining exceptions are programming errors.
     *)
    assert false

  method event_system = ues

  method abort() =
    (* The hard way to stop everything: *)
    dlogr (fun () -> 
	     sprintf "FD %Ld: abort"
	       (Netsys.int64_of_file_descr fd));
    match self#state with
      | `Working _ ->
	  Unixqueue.clear ues group;    (* Stop the queue immediately *)
	  if conn_state <> `Closed then ( 
	    Netlog.Debug.release_fd fd;
	    try Unix.close fd with _ -> ()
	  );
	  ( match conn_state with
	      | `Active proto ->
		  proto # abort `Broken_pipe
		    (* This closes the file descriptors of all files currently
		     * being sent by [send_file_response].
		     *)
	      | _ -> ()
	  );
	  ( match cur_request_manager with
	      | Some rm -> rm # abort()
		  (* The input and output channels are forced to fail when data
		   * is input/output.
		   *)
	      | None -> ()
	  );
	  self # set_state `Aborted
      | _ ->
	  ()   (* already in a final state *)
    
end


class type http_engine_processing_config =
object
  method config_synch_input : 
           (Netchannels.in_obj_channel -> unit) ->
           Uq_engines.async_in_channel ->
           unit
  method config_synch_output : 
           (Netchannels.out_obj_channel -> unit) ->
           Uq_engines.async_out_channel ->
           unit
end


class buffering_engine_processing_config : http_engine_processing_config =
object
  method config_synch_input f (ch : Uq_engines.async_in_channel) =
    let queue = Queue.create() in

    let ch' = object(self)
      val mutable closed = false
      val mutable token = None
      val mutable pos_in = 0

      method input u up ul =
	if closed then raise Netchannels.Closed_channel;
	try
	  let (s,sp,sl) = self # next_token in
	  let n = min ul sl in
	  String.blit s sp u up n;
	  token <- if n = sl then None else Some(s,sp+n,sl-n);
	  pos_in <- pos_in + n;
	  n
	with
	    Queue.Empty -> raise End_of_file

      method close_in() = closed <- true

      method pos_in = pos_in

      method private next_token =
	match token with
	  | None ->
	      let tok = Queue.take queue in
	      token <- Some tok;
	      tok
	  | Some(s,p,l) ->
	      (s,p,l)

    end in

    let s = String.create 8192 in
    let on_data() =
      try
	while ch # can_input do
	  let n = ch # input s 0 8192 in
	  Queue.push (String.sub s 0 n, 0, n) queue
	done;
	true  (* notify again *)
      with
	  End_of_file -> 
	    let ch' = Netchannels.lift_in ~buffered:false (`Raw ch') in
	    f ch';
	    false  (* don't notify any longer *)
    in

    ch # request_notification on_data;
    if ch # can_input then ignore(on_data());

  method config_synch_output f (ch : Uq_engines.async_out_channel) =
    let ch' = 
      Netchannels.lift_out ~buffered:false (`Raw (ch :> Netchannels.raw_out_channel)) in
    f ch'
      (* The output channel of the engine buffers anyway! *)
end


class type http_engine_processing_context =
object
  method engine : unit Uq_engines.engine
end


type x_reaction = 
    [ http_service_reaction
    | `Redirect_request of string * http_header
    ]


exception Ev_stage2_processed of 
  (http_service_generator option * 
     http_request_header_notification *
     Unixqueue.group)
  (* Event: Stage 2 has been processed. The argument is the follow-up action:
   * - None: Finish request immediately
   * - Some g: Proceed with generator g
   * The other args only allow identification of the event.
   *)

exception Ev_stage3_processed of
  ((string * http_header) option * 
      http_request_notification *
     Unixqueue.group)
  (* Event: Stage 2 has been processed. The argument is the follow-up action:
   * - None: Finish request
   * - Some(uri,hdr): Redirect response to this location
   *)


class redrained_environment ~out_channel (env : extended_environment) =
object(self)
  inherit redirected_environment env
  method output_ch = out_channel
  method out_channel = out_channel

    (* CHECK: send_file? maybe it needs also to be overridden *)
end


class fake_rhn (req:http_request_notification) : http_request_header_notification =
object
  method req_state = `Received_header
  method environment = req # environment
  method schedule_accept_body ~on_request ?(on_error = fun () -> ()) () =
    on_request req
  method schedule_reject_body ~on_request ?(on_error = fun () -> ()) () =
    on_request req
  method schedule_finish() = req # schedule_finish()
end


let process_connection config pconfig fd ues (stage1 : _ http_service)
                       : http_engine_processing_context =
  let fd_addr = Unix.getsockname fd in
  let peer_addr = Netsys.getpeername fd in

  dlogr
    (fun () ->
       sprintf "FD %Ld (%s -> %s) processing connection"
	 (Netsys.int64_of_file_descr fd)
	 (Netsys.string_of_sockaddr peer_addr)
	 (Netsys.string_of_sockaddr fd_addr)
    );

  let on_req_hdr = ref (fun _ -> ()) in

  let eng_config = object
    inherit Nethttpd_reactor.modify_http_processor_config 
              (config :> Nethttpd_reactor.http_processor_config)
    method config_input_flow_control = true
    method config_output_flow_control = true
  end in

  let log_error req msg =
    let env = (req # environment :> extended_environment) in
    let meth = env # cgi_request_method in
    let uri = env # cgi_request_uri in
    Nethttpd_reactor.logged_error_response
      fd_addr peer_addr (Some(meth,uri)) 0L false `Internal_server_error
      (Some env#input_header) (Some msg) (Some env) None 
      (config :> Nethttpd_reactor.http_processor_config)
  in

  let group = Unixqueue.new_group ues in
     (* for the extra events *)

  let fdi = Netsys.int64_of_file_descr fd in (* debug msg *)

object(self)

  val mutable watched_groups = []

  val mutable engine = lazy (assert false)

  initializer (
    on_req_hdr := self # on_request_header;
    (* Create the http_engine, but be careful in case of errors: *)
    try
      let eng = 
	new http_engine 
	  ~on_request_header:(fun req -> !on_req_hdr req) ()
	  eng_config fd ues in
      Uq_engines.when_state 
	~is_aborted:(fun _ -> 
		       List.iter (Unixqueue.clear ues) watched_groups;
		       watched_groups <- []
		    )
	eng;
      engine <- lazy eng
    with
      | error ->
	  Unix.close fd;  (* fd is not yet tracked here *)
	  let eng = new Uq_engines.epsilon_engine (`Error error) ues in
	  engine <- lazy eng
  )


  method private do_stage3 req_id (req : http_request_notification) 
                           env redir_count stage3 =
    (* We call the response generator with a synchronized output channel. By sending
     * the [Ev_stage3_processed] event, we catch the point in time when the whole
     * request is over, and can be finished.
     *)

    dlogr (fun () -> sprintf "FD %Ld req-%d: preparing stage3 env=%d"
	     fdi req_id (Oo.id env));

    (* This construction just catches the [Ev_stage3_processed] event: *)
    let pe = new Uq_engines.poll_engine 
	       ~extra_match:(function 
			       | Ev_stage3_processed (_,r,g) -> r=req && g=group
			       | _ -> false) 
	       [] ues in
    watched_groups <- pe#group :: watched_groups;
    Uq_engines.when_state
      ~is_done:(function
		  | Unixqueue.Extra(Ev_stage3_processed(redirect_opt,_,_)) ->
		      (* Maybe we have to perform a redirection: *)
		      ( match redirect_opt with
			  | Some (new_uri,new_hdr) ->
			      dlogr (fun () ->
				       sprintf "FD %Ld req-%d: redirect_response \
                                                to %s" fdi req_id new_uri);
			      if !(env # output_state) <> `Start 
			      then
				log_error req
				  "Nethttpd: Redirect_response is not allowed \
                                   after output has started"
			      else (
				let (new_script_name, new_query_string) = 
				  decode_query new_uri in
				new_hdr # update_field "Content-length" "0";
				let new_properties =
				  update_alist 
				    [ "REQUEST_URI", new_uri;
				      "SCRIPT_NAME", new_script_name;
				      "QUERY_STRING", new_query_string;
				      "REQUEST_METHOD", "GET"
				    ] 
				    env#cgi_properties in
				let new_env =
				  new redirected_environment 
				    ~properties:new_properties
				    ~in_header:new_hdr
				    env in
				let new_req = new fake_rhn req in
				(* The fake_rhn accepts/rejects the body immediately.
				 * In reality this is already done, but when we
				 * redirect the response we must fake a fresh
				 * request header object
				 *)
				self # process_request new_req new_env (redir_count+1)
			      )
			  | None ->
			      dlogr (fun () -> 
				       sprintf "FD %Ld req-%d: stage3 done" 
					 fdi req_id);
			      req # schedule_finish())
		  | _ -> assert false)
      pe;

    pconfig # config_synch_output
      (fun out_ch ->
	 let env' =
	   new redrained_environment ~out_channel:out_ch env in
	 dlogr (fun () -> sprintf "FD %Ld req-%d: stage3 reqenv=%d env=%d"
		  fdi req_id (Oo.id req#environment) (Oo.id env'));
	 let redirect_opt =
	   try
	     stage3 # generate_response env';
	     None
	   with
	     | Redirect_request(_,_) ->
		 log_error req
		   "Nethttpd: Caught Redirect_request in stage 3, but it is only allowed in stage 1";
		 None
	     | Redirect_response(new_uri, new_hdr) ->
		 Some(new_uri, new_hdr)
             | Standard_response(status, hdr_opt, errmsg_opt) 
		 when !(env'#output_state) = `Start ->
		   output_std_response config env' status hdr_opt errmsg_opt;
		   None
	     | err when !(env#output_state) = `Start ->
		 output_std_response config env' `Internal_server_error None 
		   (Some("Nethttpd: Exception (sending server error): " ^
			   Netexn.to_string err));
		 None
	     | err ->
		 log_error req
		   ("Nethttpd: Exception: " ^ Netexn.to_string err);
		 (* Better do an abort here. We probably cannot finish
		    the cycle regularly
		  *)
		 self # engine # abort();
		 None
	 in
	 dlogr (fun () -> 
		  sprintf "FD %Ld req-%d: stage3 postprocessing" fdi req_id);
	 (* Send the event that we are done here: *)
	 ues # add_event
	   (Unixqueue.Extra(Ev_stage3_processed(redirect_opt,req,group)))
      )
      req # environment # output_ch_async


  method private do_stage2 (req : http_request_header_notification)
                           env redir_count stage2 =
    (* This is quite complicated. First, we schedule the body acceptance. Second,
     * we call the synchronized stage2 processor. In an MT environment, both
     * processes may run in parallel, so we have to synchronize them again (i.e.
     * determine the point in time when the body has accepted due to 
     * [schedule_accept_body], and when the stage2 processor is done). To do so,
     * we send a [Ev_stage2_processed] event, and catch that by the main event loop.

     * (NB. We could also use a Uq_engines.signal_engine for this purpose,
     * but signal engines are not thread-safe (the signal function cannot be
     * called from other threads). Thread-safety is announced in the API,
     * though.)
     *)
    dlogr (fun () -> sprintf "FD %Ld req-%d: preparing stage2 env=%d"
	     fdi (Oo.id req) (Oo.id env));
    let accepted_request = ref None in
    let stage3_opt = ref None in
    (* When both variables are [Some], we are in synch again. *)

    let check_synch() =
      match (!accepted_request, !stage3_opt) with
	| (Some req', Some(Some stage3)) ->
	    (* Synch + stage2 was successful. Continue with stage3: *)
	    dlogr (fun () -> sprintf "FD %Ld synch req-%d req'-%d"
		     fdi (Oo.id req) (Oo.id req'));
	    (* assertion: req = req' *)
	    self # do_stage3 (Oo.id req) req' env redir_count stage3
	| (Some req', Some None) ->
	    (* Synch, but stage2 was not successful. Finish the request immediately. *)
	    dlogr (fun () -> sprintf "FD %Ld synch-err req-%d req'-%d"
		     fdi (Oo.id req) (Oo.id req'));
	    (* assertion: req = req' *)
	    req' # schedule_finish()
	| _ ->
	    (* All other cases: not yet in synch. Do nothing. *)
	    ()
    in

    req # schedule_accept_body
      ~on_request:(fun req' -> 
		     accepted_request := Some req';
		     check_synch())
      (* ~on_error:XXX *)  (* CHECK: close async in channel? *)
      ();

    (* This construction just catches the [Ev_stage2_processed] event: *)
    let pe = new Uq_engines.poll_engine 
	       ~extra_match:(function 
			       | Ev_stage2_processed(_,r,g) -> r=req && g=group 
			       | _ -> false) 
	       [] ues in
    watched_groups <- pe#group :: watched_groups;
    Uq_engines.when_state
      ~is_done:(function
		  | Unixqueue.Extra(Ev_stage2_processed(st3_opt,_,_)) ->
		      stage3_opt := Some st3_opt;
		      check_synch()
		  | _ -> assert false)
      pe;

    pconfig # config_synch_input
      (fun in_ch ->
	 let env' =
	   new redirected_environment ~in_channel:in_ch env in
	 dlogr (fun () -> sprintf "FD %Ld req-%d: stage2 reqenv=%d env=%d"
		  fdi (Oo.id req) (Oo.id req#environment) (Oo.id env'));
	 let stage3_opt =
	   try
	     Some(stage2 # process_body env')
	   with
	     | Redirect_request(_,_) ->
		 log_error req
		   "Nethttpd: Caught Redirect_request in stage 2, \
                    but it is only allowed in stage 1";
		 None
	     | Redirect_response(_,_) ->
		 log_error req
		   "Nethttpd: Caught Redirect_response in stage 2, \
                    but it is only allowed in stage 3";
		 None
             | Standard_response(status, hdr_opt, errmsg_opt) 
		 when !(env'#output_state) = `Start ->
		   output_std_response config env' status hdr_opt errmsg_opt;
		   None
	     | err when !(env#output_state) = `Start ->
		 output_std_response config env' `Internal_server_error None 
		   (Some("Nethttpd: Exception (sending server error): " ^ 
			   Netexn.to_string err));
		 None
	     | err -> (* Unlikely case *)
		 log_error req
		   ("Nethttpd: Exception: " ^ Netexn.to_string err);
		 (* Better do an abort here. We probably cannot finish
		    the cycle regularly
		  *)
		 self # engine # abort();
		 None
	 in
	 (* Send the event that we are done here: *)
	 dlogr (fun () -> sprintf "FD %Ld req-%d: stage2 done" fdi (Oo.id req));
	 ues # add_event
	   (Unixqueue.Extra(Ev_stage2_processed(stage3_opt,req,group)))
      )
      req # environment # input_ch_async

  method private process_request (req:http_request_header_notification) 
                                 redir_env redir_count =
    (* [redir_env]: The environment of the request, possibly rewritten by redirects.
     * [redir_count]: The number of already performed redirections
     * [req]: Contains always the original environment
     *)
    dlogr 
      (fun () ->
	 sprintf "FD %Ld req-%d: process_request FD=%Ld env=%d redir_count=%d"
	   fdi (Oo.id req) (Netsys.int64_of_file_descr fd) (Oo.id redir_env)
	   redir_count);
    if redir_count > 10 then
      failwith "Too many redirections";
    dlogr (fun () -> sprintf "FD %Ld req-%d: stage1" fdi (Oo.id req));
    let reaction = 
      try (stage1 # process_header redir_env :> x_reaction)
      with 
	| Redirect_request(new_uri, new_hdr) ->
	    `Redirect_request(new_uri, new_hdr)
	| Redirect_response(_,_) ->
	    failwith "Caught Redirect_response in stage 1, but it is only allowed in stage 3"
    in
    dlogr
      (fun () ->
	 let s_reaction =
	   match reaction with
	     | `Accept_body stage2 -> "Accept_body (next stage: stage2)"
	     | `Reject_body stage3 -> "Reject_body (next stage: stage3)"
	     | `Static _ -> "Static"
	     | `File _ -> "File"
	     | `Std_response _ -> "Std_response"
	     | `Redirect_request _ -> "Redirect_request" in
	 sprintf "FD %Ld req-%d: stage1 results in: %s" fdi (Oo.id req) s_reaction
      );
    ( match reaction with
	| `Accept_body stage2 ->
	    self # do_stage2 req redir_env redir_count stage2
	| `Reject_body stage3 ->
	    req # schedule_reject_body
	      ~on_request:(fun req' -> 
			     self # do_stage3 
			       (Oo.id req)
			       req' redir_env redir_count stage3)
	      ();
	| `Static(status, resp_hdr_opt, resp_str) ->
	    req # schedule_reject_body
	      ~on_request:(fun req' -> 
			     output_static_response redir_env status 
			       resp_hdr_opt resp_str;
			     req' # schedule_finish())
	      ();
	| `File(status, resp_hdr_opt, resp_filename, pos, length) ->
	    req # schedule_reject_body
	      ~on_request:(fun req' -> 
			     output_file_response redir_env status resp_hdr_opt
			       resp_filename pos length;
			     req' # schedule_finish())
	      ();
	| `Std_response(status, resp_hdr_opt, errlog_opt) ->
	    req # schedule_reject_body
	      ~on_request:(fun req' -> 
			     output_std_response config redir_env status 
			       resp_hdr_opt errlog_opt;
			     req' # schedule_finish())
	      ();
	| `Redirect_request(new_uri, new_hdr) ->
	    dlogr (fun () -> sprintf "FD %Ld req-%d: redirect_request to: %s"
		     fdi (Oo.id req) new_uri);
	    let (new_script_name, new_query_string) = decode_query new_uri in
	    new_hdr # update_multiple_field 
	      "Content-length" (redir_env # multiple_input_header_field "Content-length");
	    let new_properties =
	      update_alist 
		[ "REQUEST_URI", new_uri;
		  "SCRIPT_NAME", new_script_name;
		  "QUERY_STRING", new_query_string ] 
		redir_env#cgi_properties in
	    let new_env =
	      new redirected_environment 
		~properties:new_properties
		~in_header:new_hdr
		~in_channel:(redir_env # input_channel) redir_env in
	    self # process_request req new_env (redir_count+1)
    )

  method private on_request_header req =
    try
      self # process_request req (req#environment :> extended_environment) 0
    with
      | err ->
	  log_error req
	    ("Nethttpd: Exception: " ^ Netexn.to_string err);
	  (* Better do an abort here. We probably cannot finish
	     the cycle regularly
	   *)
	  self # engine # abort()
	  
	  
  method engine = Lazy.force engine

end


let override v opt =
  match opt with
    | None -> v
    | Some x -> x


let default_http_engine_config =
  ( object
      inherit Nethttpd_reactor.modify_http_processor_config
	         Nethttpd_reactor.default_http_processor_config
      method config_input_flow_control = false
      method config_output_flow_control = true
    end
  )


class modify_http_engine_config
        ?modify_http_protocol_config
        ?modify_http_processor_config:(m2 = fun cfg -> cfg)
        ?config_input_flow_control
        ?config_output_flow_control
        (config : http_engine_config) : http_engine_config =
  let config_input_flow_control =
    override config#config_input_flow_control config_input_flow_control in
  let config_output_flow_control =
    override config#config_output_flow_control config_output_flow_control in
object
  inherit Nethttpd_reactor.modify_http_processor_config
            ?modify_http_protocol_config
            (m2 (config:>Nethttpd_reactor.http_processor_config))
  method config_input_flow_control = config_input_flow_control
  method config_output_flow_control = config_output_flow_control
end

