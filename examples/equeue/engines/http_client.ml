(* This is the HTTP client example from the User's Manual *)

open Uq_engines
open Uq_engines.Operators

class async_buffer b =
object (self)
  inherit Netchannels.output_buffer b
  method can_output = true
  method request_notification (f : unit->bool) = ()
end ;;


let main() =
  let ues = Unixqueue.create_unix_event_system() in
  let c = connector (`Socket(`Sock_inet_byname(Unix.SOCK_STREAM,
					       "www.npc.de", 80),
			     default_connect_options
			    )) ues in
  let b = Buffer.create 10000 in

  let e =
    c ++ 
      (fun connstat ->
	 match connstat with
	  | `Socket(fd, _) ->
	       prerr_endline "CONNECTED";     (* debug output *)
	       let d = `Polldescr(Netsys.get_fd_style fd, fd, ues) in
	       Uq_io.output_string_e d "GET / HTTP/1.0\n\n" ++
		 (fun () ->
		    Uq_io.write_eof_e d ++
                      (fun _ ->
			let buffer = new async_buffer b in
			new receiver ~src:fd ~dst:buffer ues
                      )
		 )
	  | _ -> assert false
      ) in

  when_state
    ~is_done:(fun _ ->
                prerr_endline "HTTP RESPONSE RECEIVED!")
    ~is_error:(fun _ ->
                prerr_endline "ERROR!")
    e;

  Unixqueue.run ues;

  b
;;
