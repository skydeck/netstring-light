(* Copies from stdin to stdout *)

#use "topfind";;
#require "equeue";;

open Uq_engines

let main() =
  let e = Unixqueue.create_unix_event_system() in
  let cp = new copier (`Unidirectional(Unix.stdin,Unix.stdout)) e in
  Unixqueue.run e
;;


main();;
