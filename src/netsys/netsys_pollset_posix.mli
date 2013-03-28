(* $Id: netsys_pollset_posix.mli 1696 2012-02-08 19:27:53Z gerd $ *)

(** Pollsets for POSIX operating systems *)

open Netsys_pollset

val poll_based_pollset : unit -> pollset
  (** Returns a poll set whose implementation is based on the [poll] system
      call. 

      Win32: On Win32 this implementation works, but only for sockets,
      and is not cancellable in multi-threaded programs. (This is a 
      restriction because we have to map it to the [select] call of the
      WinSock layer.)
   *)

val reset : unit -> unit
  (** This module may keep some global state. This function resets this
      state. As the state may contain file descriptors, it is advisable
      to reset after calling [fork] to free these descriptors.
   *)


val accelerated_pollset : unit -> pollset
  (** Returns a pollset using a "fast" poll mechanism, if available.
      Otherwise this is the same as [poll_based_pollset].

      Fast poll mechanisms are:
      - [epoll] on Linux

      Generally, these mechanisms are not 100% compatible with the
      standard [poll]. In particular, the poll events may be slightly
      differently interpreted. Also, there are many, many reports about
      buggy implementations in the OS.
   *)

(* TODO: pollsets for kqueue, /dev/poll etc. *)
