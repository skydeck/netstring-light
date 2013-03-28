(* $Id: netplex_mutex.ml 1460 2010-06-03 12:12:02Z gerd $ *)

open Netplex_types

let plugin = Netplex_semaphore.plugin

type mutex = string

(* The #s semaphore reflects the state: 
     0 = unlocked, 1 = locked

   The #w semaphore reflects the locking resource:
     0 = cannot be locked, 1 = can be locked
 *)

let access name =
  let _ = Netplex_semaphore.create ~protected:true (name ^ "#w") 1L in
  let _ = Netplex_semaphore.create ~protected:true (name ^ "#s") 0L in
     (* The second [create] is superflous - this would implicitly happen 
        at first access time
      *)
  name

let lock name =
  let w = Netplex_semaphore.decrement ~wait:true (name ^ "#w") in
  (* Netlog.logf `Debug "lock: w=%Ld" w; *)
  assert(w = 0L);
  let s = Netplex_semaphore.increment (name ^ "#s") in
  (* Netlog.logf `Debug "lock: s=%Ld" s; *)
  assert(s = 1L)

let unlock name =
  let s = Netplex_semaphore.decrement (name ^ "#s") in  (* no wait! *)
  (* Netlog.logf `Debug "unlock: s=%Ld" s; *)
  if (s >= 0L) then (
    assert(s = 0L);
    let w = Netplex_semaphore.increment (name ^ "#w") in
    (* Netlog.logf `Debug "unlock: w=%Ld" w; *)
    assert(w = 1L)
  )
