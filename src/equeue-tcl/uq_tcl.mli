(* $Id: uq_tcl.mli 967 2006-07-26 21:38:43Z gerd $ *)

open Equeue
open Unixqueue

(** Integration with the labltk event system *)

(** This module provides integration of Unixqueue event systems with the
 * TCL event queue (and, as a consequence, with the labltk event queue).
 *)


type runner =
    event_system -> (unit -> unit) -> unit


class tcl_event_system : ?run:runner -> unit -> event_system
(** This class is an alternate implementation of the Unixqueue event systems
 * for the TCL event loop. Use this class instead of [unix_event_system].
 * 
 * Both Unixqueue and TCL provide event queues for system events,
 * and it is possible to merge both queues such that events may happen and
 * be processed on one queue while the other queue blocks.
 *
 * To achieve this, just use this class instead of [unix_event_system].
 * It automatically creates handlers for the TCL loop when necessary.
 * However, you must not invoke the method [run], as this class does not
 * provide its own event loop. Instead, ensure that [Tk.mainLoop] is
 * called. 
 *
 * Of course, this is all intended to help writing applications which have
 * a graphical user interface (GUI) built with labltk, and some network 
 * functionality which is designed to work in the background. Simply create
 * your GUI with labltk, and after the button is pressed which starts the
 * network I/O, you add resources to this event queue, and the I/O will be
 * processed concurrently with any user input coming from the GUI.
 *
 * Note: The implementation is not thread-safe (and neither TCL).
 *)
