(* $Id: uq_lwt.mli 1740 2012-02-22 22:31:47Z gerd $ *)

(** Compatibility with [Lwt] *)

(** Lwt is another library for event-driven programming. Here are some
    helpers for running Lwt code on top of Ocamlnet event queues.

    This is for Lwt-2.3 or better.

    See also the comments in {!Equeue_howto.lwt}.
 *)

(** Adapter for turning an Ocamlnet [event_system] into an Lwt [Lwt_engine.t].

    Use it like:

    {[
       class lwt_engine esys =
       object
         inherit Lwt_engine.abstract
         inherit Uq_lwt.lwt_backend esys
       end
    ]}

    (We've intentionally left out {b this} definition to avoid any 
    build dependency on Lwt. Also note that [Lwt_engine] is in the
    package [lwt.unix].)

    Now, activate this Lwt engine (event loop):

    {[
      Lwt_engine.set (new lwt_engine esys)
    ]}

    Note that Lwt can only deal with one event loop at a time, and the
    new event loop will be used for all Lwt code.

    It is, unfortunately, necessary that you use the Lwt main loop
    ([Lwt_main.run] or [Lwt_unix.run]), because otherwise some hook
    functions are never executed (and execution will hang).

    For an example, see [tests/equeue/manual/relay.ml] in the distribution
    tarball.

    Netplex users: If you want to use [lwt_engine] for driving the
    event loop of the container, you can do so by overriding the
    processor hooks [container_event_system] and [container_run], e.g.

    {[
    method container_event_system () =
      let esys = Unixqueue.create_unix_event_system() in
      Lwt_engine.set (new lwt_engine esys);
      esys

    method container_run esys =
      Lwt_main.run <something>
    ]}

    The Lwt thread [<something>] must at least run until the container is
    shut down. You can catch this moment by also defining the [shutdown]
    method.
 *)
class lwt_backend : Unixqueue.event_system ->
  object
      method iter : bool -> unit
      method private cleanup : unit
      method private register_readable : Unix.file_descr -> (unit -> unit) -> unit Lazy.t
      method private register_writable : Unix.file_descr -> (unit -> unit) -> unit Lazy.t
      method private register_timer : float -> bool -> (unit -> unit) -> unit Lazy.t
    end
