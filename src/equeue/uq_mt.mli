(* $Id: uq_mt.mli 1707 2012-02-15 22:49:54Z gerd $ *)

(** {1 Using engines in multi-threaded programs} *)

(** Monitors can be used to coordinate access to a resource from several
    threads so that only one thread comes into contact with the resource.
    Of course, the resources we talk about must have an engine interface.
    For example, the resource could be an RPC client. In contrast to a
    critical section (protected with mutexes), the accesses are not
    serialized, but simply pushed onto the same event system, and run
    there concurrently. Effectively, the monitor translates heavy-weight
    threading (kernel threads) into light-weight threading (engines).
 *)

type monitor
  (** A thread monitor governs which threads have access to a set
      of engines running together on an event system
   *)

val create_monitor : Unixqueue.event_system -> monitor
  (** Creates a new monitor. The passed event system becomes the
      inner event system of the monitor. All engines attached to
      the monitor will use this event system.
   *)

val monitor_run : monitor ->
                   (Unixqueue.event_system -> 'a -> 'b Uq_engines.engine) ->
                  'a -> 'b
  (** [let result = monitor_run mon f outer_esys arg]: 
      Runs another engine within the
      monitor. The engine [inner_e] is created as

      {[ let inner_e = f inner_esys arg ]}

      where [inner_esys] is the event system of the monitor. It is
      obligatory that [inner_e] uses this event system.

      When [inner_e] reaches a final state, the thread leaves the
      monitor, and the result of [inner_e] is returned (or the
      recorded exception is thrown).

      The "special effect" of the monitor is that several threads can
      share the same monitor, even at the same time. All parallel running
      engines [inner_e] will use the same event system [inner_esys].
      Of course, this also means that they run in the same thread.
      Note that it is possible that the monitor switches the thread
      it uses for executing the event system (and the engines). In
      particular, this can happen when one of the inner engines finishes,
      and the thread that put this engine into the monitor was chosen
      as the thread for executing the event system. When this happens,
      another thread waiting for the monitor will be selected for
      running the event system.

      If an inner engine establishes configurations that are not bound
      to the lifetime of the inner engine, there is no guarantee that
      these additional events and handlers will be considered when the
      inner engine ends. For example, if the engine starts a timer to
      run an action later, it is not said that the action is carried
      out at all if the engine terminates before.

      Exceptions falling through to the caller of the event system
      are logged and otherwise ignored.

      It is possible to use monitors in single-threaded programs -
      this is treated as if only one thread was using the monitor in a
      multi-threaded program.
   *)

val monitor_do : monitor -> ('a -> 'b) -> 'a -> 'b
  (** [let result = monitor_do mon f arg]: Just runs [f arg] in the
      scope of the monitor, and returns the result.
   *)

val monitor_async : monitor -> ('a -> ((unit -> 'b) -> unit) -> unit) ->
                      'a -> 'b
  (** [let result = monitor_async f arg]: For calling RPC-style 
      asynchronous clients. If [f] is called like [f arg emit], 
      the result is passed back by calling [emit (fun () -> result)].
   *)


(** {2:rpc_client Example: Threads sharing an RPC client}

    In this example we want to achieve that several threads can use the
    same RPC client. This type of client is not thread-safe, but it is
    able to run asynchronously. Because of this, the requests coming from
    several threads can be concurrently handled.

    Prepare the RPC client and the monitor: We assume that [M_clnt] is
    the generated RPC module with the client, and that we access program
    [P] at version [V].

    {[
      let esys = Unixqueue.create_unix_event_system
      let rpc = M_clnt.P.V.create_client ~esys connector protocol
      let mon = Uq_mt.create_monitor esys
    ]}

    It is essential that the RPC client and the monitor use the same
    [esys].

    Next, let's assume we are in a spawned thread, and we want to
    call an RPC procedure [M_clnt.P.V.proc]. The trick here is to
    use the asynchronous variant [M_clnt.P.V.proc'async], and to
    call it via the monitor:

    {[
      let result = Uq_mt.monitor_async mon (M_clnt.P.V.proc'async rpc) arg
    ]}

    The procedure is called with argument [arg] and returns the [result].
    The monitor waits until the asynchronous call is done, so this appears
    as a synchronous call to the user.

    If you need to call functions to configure and control the RPC
    client, use [monitor_do]:

    {[
      let sockaddr = Uq_mt.monitor_do mon Rpc_client.get_peer_name rpc
    ]}
 *)
