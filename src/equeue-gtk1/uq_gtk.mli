 (* $Id: uq_gtk.mli 967 2006-07-26 21:38:43Z gerd $ *)

open Equeue
open Unixqueue

(** Integration with lablgtk/lablgtk2 event systems *)

(** This module provides integration of Unixqueue event systems with the
 * Glib event queue (and, as a consequence, with the lablgtk event queue).
 *
 * There are two flavours of this module, both named [Uq_gtk], and with
 * identical interfaces, but one is used with lablgtk, and one with lablgtk2.
 * Compile with
 * {[ ocamlfind ... -package equeue-gtk1 ... ]}
 * to get the variant for lablgtk, and compile with
 * {[ ocamlfind ... -package equeue-gtk2 ... ]}
 * to get the variant for lablgtk2.
 *)


type runner =
    event_system -> (unit -> unit) -> unit


class gtk_event_system : ?run:runner -> unit -> event_system
(** This class is an alternate implementation of the Unixqueue event systems
 * for the Glib event loop. Use this class instead of [unix_event_system].
 * 
 * Both Unixqueue and Glib provide event queues for system events,
 * and it is possible to merge both queues such that events may happen and
 * be processed on one queue while the other queue blocks.
 *
 * To achieve this, just use this class instead of [unix_event_system].
 * It automatically creates handlers for the Glib loop when necessary.
 * However, you must not invoke the method [run], as this class does not
 * provide its own event loop. Instead, ensure that [GMain.main] is
 * called. 
 *
 * Of course, this is all intended to help writing applications which have
 * a graphical user interface (GUI) built with lablgtk, and some network 
 * functionality which is designed to work in the background. Simply create
 * your GUI with lablgtk, and after the button is pressed which starts the
 * network I/O, you add resources to this event queue, and the I/O will be
 * processed concurrently with any user input coming from the GUI.
 *)
