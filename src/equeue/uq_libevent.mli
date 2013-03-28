(* $Id: uq_libevent.mli 1635 2011-06-26 15:20:34Z gerd $ *)

(** Use Libevent as event loop *)

(** This gives (experimental) support for using libevent as event loop.
    To do so, you also need Maas-Maarten Zeeman's bindings for libevent,
    ocaml-event. (N.B. The current name, changed several times, is now
    liboevent.)

    Restrictions:

    - ocaml-event is not thread-safe
    - There can only be one instance of the event loop at a time
    - It is not supported to wait for out of band data
    - Signals do not stop the event loop. As a consequence, signals
      are delayed until the next event occurs.
 *)

module type LIBOEVENT = sig
  type event
  type event_flags = 
      TIMEOUT
    | READ
    | WRITE
    | SIGNAL
  type event_callback = Unix.file_descr -> event_flags -> unit
  val create : unit -> event
  val set : event -> 
    Unix.file_descr -> event_flags list -> persist:bool -> event_callback -> 
    unit
  val add : event -> float option -> unit
  val del : event -> unit
  type loop_flags =
      ONCE            
    | NONBLOCK        
  val loop : loop_flags -> unit
end


module type POLLSET = sig
  val create_pollset : unit -> Netsys_pollset.pollset
  val create_event_system : unit -> Unixqueue.event_system
end


module Make(L:LIBOEVENT) : POLLSET
  (** Instantiate this module. Just do

      {[
  let P = Make(Liboevent)
      ]}

      and then call [P.create_pollset()] to create a [pollset]
      object, or [P.create_event_system()] to create an [event_system].

      (N.B. This functor saves Ocamlnet from being directly dependent
      on Liboevent.)
   *)

      
