(* This is a working demonstration of the class funny_async_buffer
 * of the user's manual. We combine such a buffer with a receiver
 * to load a file in a "funny" way.
 *)

open Uq_engines;;

class funny_async_buffer b ues =
  (* The same class as in the manual with a number of added prerr_endline
   * calls to visualize its effects.
   *)
object (self)
  val real_buf = new Netchannels.output_buffer b
  val mutable barrier_enabled = true
  val mutable barrier_reached = false
  val mutable notify_list = []
  val mutable notify_list_new = []
				  
  method output s k n =
    prerr_endline ("output n=" ^ string_of_int n);
    let rr =
      if barrier_enabled then (
	let m = 1024 - real_buf#pos_out in
	let r = real_buf # output s k (min n m) in
	if m > 0 && real_buf#pos_out = 1024 then (
          barrier_reached <- true;
          self # configure_sleep_second();
          self # notify()
	);
	r
      )
      else 
	real_buf # output s k n
    in
    prerr_endline("    returns " ^ string_of_int rr);
    rr
	
  method flush() = ()
		     
  method pos_out = real_buf#pos_out

  method close_out() = real_buf#close_out()

  method can_output =
    if barrier_enabled then
      not barrier_reached
    else
      true

  method request_notification f =
    prerr_endline "request_notification";
    notify_list_new <- f :: notify_list_new

  method private notify() =
    prerr_endline ("notify: can_output=" ^ string_of_bool self#can_output ^
		   " barrier_enabled=" ^ string_of_bool barrier_enabled ^ 
		   " barrier_reached=" ^ string_of_bool barrier_reached);
    notify_list <- notify_list @ notify_list_new;
    notify_list_new <- [];
    notify_list <- List.filter (fun f -> f()) notify_list

  method private configure_sleep_second() =
    prerr_endline "configure_sleep_second";
    let g = Unixqueue.new_group ues in
    Unixqueue.once ues g 1.0 self#wake_up
      
  method private wake_up() =
    prerr_endline "wake_up";
    barrier_enabled <- false;
    self # notify()
end ;;


let main() =
  (* Call this function to read "input.data" into a buffer. This should
   * be an arbitrary file > 1024 bytes
   *)
  let ues = Unixqueue.create_unix_event_system() in
  let filename = "input.data" in
  let b = Buffer.create 10000 in
  let src = Unix.openfile filename [Unix.O_RDONLY] 0 in
  let dst = new funny_async_buffer b ues in
  let recv = new receiver ~src ~dst ues in

  prerr_endline "Starting event system...";
  Unixqueue.run ues;
  prerr_endline "Returning from event system!"
;;
