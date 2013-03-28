(* From Patrick M Doane *)

(* This example demonstrates an important property of Unixqueue:
   the queues are thread-safe. Even better, one can add event from
   one thread into a running Unixqueue that is being executed by 
   a different thread. This "event injection" will interrupt the
   queue if it is waiting, and makes it consider the additional event.
 *)

let esys = Unixqueue.create_unix_event_system()
let after timeout f =
  let group = Unixqueue.new_group esys in
  Unixqueue.once esys group timeout f
;;

let m = Mutex.create();;
let c = Condition.create();;
  (* Unixqueue.run should not be called before any event handlers are
   * registered; it would return immediately. The condition variable
   * is used to wait until the first handler is added to the queue.
   *)

let queue_it () =
  print_endline "queue_it started"; flush stdout;
  after 4.0 (fun () -> print_endline "4 seconds"; flush stdout);
  Mutex.lock m;
  Condition.signal c;
  Mutex.unlock m;
  Thread.delay 1.0;
  print_endline "1 second"; flush stdout;
  (* The following is an event injection from a different thread: *)
  after 1.0 (fun () -> print_endline "2 seconds"; flush stdout);  
  print_endline "queue_it finishes"; flush stdout;
;;

let run_it () =
  print_endline "run_it started"; flush stdout;
  Condition.wait c m;
  print_endline "run_it executes event system"; flush stdout;
  Unixqueue.run esys;
  print_endline "run_it finishes"; flush stdout;
;;

Mutex.lock m;;
let t1 = Thread.create queue_it ();;
let t2 = Thread.create run_it ();;

Thread.join t1;
Thread.join t2

