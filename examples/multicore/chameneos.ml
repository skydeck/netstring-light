(* $Id: chameneos.ml 1579 2011-04-12 20:16:22Z gerd $ *)

(* Chameneos game implemented with message passing

   see
   http://cedric.cnam.fr/PUBLIS/RC474.pdf
   http://shootout.alioth.debian.org/u32/benchmark.php?test=chameneosredux&lang=all

   This solution does not claim to be fast in any way. It is just a
   coding example. In particular, message passing is not optimal here
   because the messages are very short (and thus the whole message
   copying machinery consumes a lot of setup time relative to total
   time), and the worker processes do almost nothing (Color.complement).
   So this program tests mostly the bare minimum time needed for
   synchronization, but for any real-world example it will be much better.

 *)

open Printf

let spell_int i = 
  let spell_char = function 
    | '0' -> "zero"
    | '1' -> "one"
    | '2' -> "two"
    | '3' -> "three"
    | '4' -> "four"
    | '5' -> "five"
    | '6' -> "six"
    | '7' -> "seven"
    | '8' -> "eight"
    | '9' -> "nine"
    | x -> failwith "unexpected char"
  in
  let s = string_of_int i in
  String.iter (fun c -> printf " %s" (spell_char c)) s;


module Color = struct 
  type t =
  | Blue
  | Red
  | Yellow

  let complement t t' =
  match t, t' with 
    | Blue, Blue -> Blue
    | Blue, Red -> Yellow
    | Blue, Yellow -> Red
    | Red, Blue -> Yellow
    | Red, Red -> Red
    | Red, Yellow -> Blue
    | Yellow, Blue -> Red
    | Yellow, Red -> Blue
    | Yellow, Yellow -> Yellow

  let to_string = function
    | Blue -> "blue"
    | Red -> "red"
    | Yellow -> "yellow"

  let all = [ Blue; Red; Yellow ]
end


module Chameneos_type = struct
  type t = {
    id : int;
    mutable color : Color.t;
    mutable meetings : int;
    mutable meetings_with_self : int;
  }
end


module Meeting_place = struct
  (* In this solution the meeting place is an independent process to
     which messages are sent (i.e. no "monitor" as suggested by the
     original article)
   *)

  module Id_var =
    Netplex_sharedvar.Make_var_type(struct type t = Netmcore.res_id end)

  let get_box_id_var (`Process pid) =
    sprintf "Meeting_place.box.%d" pid 

  let set_box_id pid box_id =
    let box_id_var = get_box_id_var pid in
    ignore(Netplex_sharedvar.create_var ~enc:true box_id_var);
    Id_var.set box_id_var box_id

  let get_box_id pid =
    let box_id_var = get_box_id_var pid in
    ignore(Netplex_sharedvar.create_var ~enc:true box_id_var);
    ignore(Netplex_sharedvar.wait_for_enc_value box_id_var);
    Id_var.get box_id_var
    
  type config =
      { num_slots : int;   (* #slots of the box *)
	meetings : int;
      }

  type meet_request =
      { me : Chameneos_type.t;
	response_box : Netmcore.res_id;
      }

  type request =
    | Meet_request of meet_request
    | Shutdown

  type response =
      { mate_opt : Chameneos_type.t option }
	(* None = all meetings done *)
  
  type state =
    | Empty
    | First of int * meet_request
	(* slot, request *)
	
  let meeting_place config =
    let (box : request ref Netcamlbox.camlbox), box_id =
      Netmcore_camlbox.create_camlbox "chameneos" config.num_slots 512 in
    (* 512: just an upper limit for the message size *)

    (* Put the ID of the box into a global variable, so all workers
       can get it from there
     *)
    let pid = Netmcore.self_process_id() in
    set_box_id pid box_id;

    let response_boxes = Hashtbl.create 29 in
    let get_response_box id =
      try
	Hashtbl.find response_boxes id 
      with Not_found ->
	let b = Netmcore_camlbox.lookup_camlbox_sender (`Resource id) in
	Hashtbl.add response_boxes id b;
	(b : response Netcamlbox.camlbox_sender)
    in

    let meetings_left = ref config.meetings in
    let state = ref Empty in
    let live = ref true in
    while !live do
      let req_slots = Netcamlbox.camlbox_wait box in
      List.iter
	(fun req_slot ->
	   let req = !(Netcamlbox.camlbox_get box req_slot) in
	   (* no copy! So be careful... *)
	   match req with
	     | Meet_request mreq ->
		 if !meetings_left > 0 then (
		   match !state with
		     | Empty ->
			 state := First(req_slot,mreq)
		     | First(first_slot,first_req) ->
			 let `Resource r1 = first_req.response_box  in
			 let `Resource r2 = mreq.response_box in
			 let fst_box = 
			   get_response_box r1 in
			 Netcamlbox.camlbox_send 
			   fst_box { mate_opt = Some mreq.me };
			 let snd_box = 
			   get_response_box r2 in
			 Netcamlbox.camlbox_send 
			   snd_box { mate_opt = Some first_req.me };
			 decr meetings_left;
			 state := Empty;
			 Netcamlbox.camlbox_delete box first_slot;
			 Netcamlbox.camlbox_delete box req_slot
		 )
		 else (
		   let `Resource r = mreq.response_box in
		   let r_box = get_response_box r in
		   Netcamlbox.camlbox_delete box req_slot;
		   Netcamlbox.camlbox_send r_box { mate_opt = None };
		 )
	     | Shutdown ->
		 Netlog.logf `Debug "Got shutdown request";
		 Netcamlbox.camlbox_delete box req_slot;
		 live := false
	)
	req_slots
    done;
    let box_id_var = get_box_id_var pid in
    ignore(Netplex_sharedvar.delete_var box_id_var)


  let fork_meeting_place, join_meeting_place =
    Netmcore_process.def_process meeting_place

  let start config =
    Netmcore_process.start fork_meeting_place config

  let join pid =
    ignore(Netmcore_process.join join_meeting_place pid)

  let shutdown pid =
    let req_box_id = get_box_id pid in
    let b = Netmcore_camlbox.lookup_camlbox_sender req_box_id in
    Netcamlbox.camlbox_send b (ref Shutdown)

  type connector =
      { mp_req_box : request ref Netcamlbox.camlbox_sender;
	mp_resp_box : response Netcamlbox.camlbox;
	mp_resp_box_id : Netmcore.res_id;
      }

  let connect pid =
    let req_box_id = get_box_id pid in
    let (r_box : response Netcamlbox.camlbox), r_box_id =
      Netmcore_camlbox.create_camlbox "chameneos" 2 512 in
    { mp_req_box = Netmcore_camlbox.lookup_camlbox_sender req_box_id;
      mp_resp_box = r_box;
      mp_resp_box_id = r_box_id;
    }
    
  let meet pref_slot mp_conn ch =
    let req = { me = ch; response_box = mp_conn.mp_resp_box_id } in
    Netcamlbox.camlbox_send
      ~prefer:!pref_slot ~slot:pref_slot
      mp_conn.mp_req_box (ref (Meet_request req));
    match Netcamlbox.camlbox_wait mp_conn.mp_resp_box with
      | [ slot ] ->
	  let r =
	    (Netcamlbox.camlbox_get mp_conn.mp_resp_box slot).mate_opt in
	  (* No camlbox_get_copy of r! Instead, we make our own copy, which
	     is cheaper for very simple messages
	   *)
	  let r_copy =
	    match r with
	      | None -> 
		  None
	      | Some ch -> 
		  Some 
		    { ch with Chameneos_type.id = ch.Chameneos_type.id } in
	  Netcamlbox.camlbox_delete mp_conn.mp_resp_box slot;
	  r_copy
      | _ ->
	  assert false
end


module Chameneos = struct 
  include Chameneos_type

  let create = 
    let id = ref 0 in
    let new_id () = 
      let r = !id in
      id := r + 1;
      r
    in
    fun color -> 
      { id = new_id ();
	color = color;
	meetings = 0;
	meetings_with_self = 0;
      }
	
  type run =
      { place_pid : Netmcore.process_id;
	chameneos : t;
      }
	
  let run arg =
    let ch = arg.chameneos in
    let connector = Meeting_place.connect arg.place_pid in
    let pref_slot = ref 0 in
    (* The idea of pref_slot is to avoid cache bouncing: when the
       same slot is reused by the same process, the memory of this
       slot continues to be owned by the same cache
     *)

    let rec loop () =
      match Meeting_place.meet pref_slot connector ch with 
	| None -> ()
	| Some other -> 
	    ch.meetings <- ch.meetings + 1;
	    if ch.id = other.id then
	      ch.meetings_with_self <- ch.meetings_with_self + 1;
	    ch.color <- Color.complement ch.color other.color;
	    loop () 
    in
    loop();
    { arg with chameneos = ch }
      
  let fork_chameneos, join_chameneos =
    Netmcore_process.def_process run
      
  let start arg =
    Netmcore_process.start fork_chameneos arg
	
  let join pid =
    match Netmcore_process.join join_chameneos pid with
      | None ->
	  failwith "no result from chameneos"
      | Some arg ->
	  arg
end


module Compute = struct
  let work colors n = 
    Netlog.logf `Debug "Compute.work n=%d" n;

    List.iter (fun c -> printf " %s" (Color.to_string c)) colors;
    printf "\n";

    let config =
      { Meeting_place.num_slots = 2 * List.length colors;
	meetings = n 
      } in
    let place_pid = Meeting_place.start config in
    let `Process p = place_pid in
    Netlog.logf `Debug "Compute.work place_pid=%d" p;
    let cs = List.map Chameneos.create colors in
    let cs_pids = 
      List.map 
	(fun ch -> 
	   Chameneos.start
	     { Chameneos.place_pid = place_pid; chameneos = ch }
	) 
	cs in
    Netlog.logf `Debug "Compute.work started chameneos processes";
    let cs' =
      List.map (fun pid -> Chameneos.join pid) cs_pids in
    Netlog.logf `Debug "Compute.work joined chamaneos processes";
    Meeting_place.shutdown place_pid;
    Meeting_place.join place_pid;
    Netlog.logf `Debug "Compute.work joined place";

    let sum_meets = ref 0 in
    List.iter 
      (fun res ->
	 let ch = res.Chameneos.chameneos in
	 printf "%d" ch.Chameneos.meetings; 
	 spell_int ch.Chameneos.meetings_with_self;
	 printf "\n";
	 sum_meets := !sum_meets + ch.Chameneos.meetings
      )
      cs';
    spell_int !sum_meets; 
    printf "\n%!"


  let compute n =
    work [ Color.Blue; Color.Red; Color.Yellow ] n;
    printf "\n%!";
    work [ Color.Blue; Color.Red; Color.Yellow; Color.Red; Color.Yellow;
	   Color.Blue; Color.Red; Color.Yellow; Color.Red; Color.Blue ] n;
    printf "\n%!";
    
    ()

  let fork_compute, join_compute =
    Netmcore_process.def_process compute

  let start n =
    Netmcore_process.start fork_compute n
	
  let join pid =
    ignore(Netmcore_process.join join_compute pid)

end


let print_complements () = 
  List.iter 
    (fun c1 -> 
       List.iter 
	 (fun c2 ->
	    printf "%s + %s -> %s\n" 
	      (Color.to_string c1)
	      (Color.to_string c2)
	      (Color.to_string (Color.complement c1 c2))
	 )
	 Color.all
    )
    Color.all;
  printf "\n%!";
;;

let main () = 
  (* Netmcore.Debug.enable := true; *)
  let n = 
    try 
      int_of_string (Sys.argv.(1))
    with
    | _ -> 600
  in
  print_complements ();
  (* Netmcore.Debug.enable := true; *)
  List.iter
    (fun sigcode ->
       Netsys_signal.register_handler 
	 ~name:"application"
	 ~signal:sigcode 
	 ~callback:(fun _ -> Netmcore.destroy_resources())
	 ~keep_default:true
	 ()
    )
    [ Sys.sigint; Sys.sigterm ];
  Netmcore.startup
    ~socket_directory:"run_chameneos"
    ~init_ctrl:(fun ctrl -> 
		  ctrl#add_plugin Netplex_sharedvar.plugin;
		  (* ctrl#controller_config#set_max_level `Debug *)
	       )
    ~first_process:(fun () ->
		      Netmcore_process.start
			Compute.fork_compute n)
    ()
;;

let () = main ()
