(* $Id: unixqueue_util.ml 1616 2011-06-10 15:08:57Z gerd $ *)

(** Definitions common to {!Unixqueue and {!Unixqueue2} *)

(** These are internals of Ocamlnet! *)

open Printf

module Debug = struct
  let enable = ref false
  let target = ref `Any
  let set_debug_mode flag = enable := flag
  let set_debug_target t = target := t
end

let dlog0 = Netlog.Debug.mk_dlog "Unixqueue" Debug.enable
let dlogr0 = Netlog.Debug.mk_dlogr "Unixqueue" Debug.enable

let dlog m =
  if Equeue.Debug.test_debug_target !Debug.target then dlog0 m

let dlogr gm =
  if Equeue.Debug.test_debug_target !Debug.target then dlogr0 gm

let () =
  Netlog.Debug.register_module "Unixqueue" Debug.enable



(* [group] and [wait_id] are now objects. The structural equality
 * ( = ) compares object IDs if applied to objects, so that this
 * is exactly what we need. It is no longer necessary to manage
 * the IDs ourselves, because the language already manages object IDs.
 *
 * This has also the advantage that groups can now have additional
 * properties.
 *)

class group_object =
object(self)
  val mutable terminating = false
      (* Whether the group is terminating *)
  method is_terminating = terminating
  method terminate() = 
    dlogr (fun () -> (sprintf "group_terminate <group %d>" (Oo.id self)));
    (* eprintf "group_terminate <group %d>\n%!" (Oo.id self); *)
    terminating <- true
end

type group = group_object

let nogroup : group =
  ( object
      method is_terminating = false
      method terminate() =
	failwith "Unixqueue_util.nogroup.terminate"
    end
  )
  (* nogroup: experimental special group for events that do not need
     group management. Note that these can effectively only be
     Immediate events because nogroup events cannot be sent to handlers
   *)

class wait_object =
object
end

type wait_id = wait_object

type operation =
    Wait_in  of Unix.file_descr
  | Wait_out of Unix.file_descr
  | Wait_oob of Unix.file_descr
  | Wait of wait_id

type event =
    Input_arrived of (group * Unix.file_descr)
  | Output_readiness of (group * Unix.file_descr)
  | Out_of_band of (group * Unix.file_descr)
  | Timeout of (group * operation)
  | Signal
  | Extra of exn
  | Immediate of (group * (unit -> unit))

type resource_prop =
    group * float * float ref
    (* group, timeout value, time of last event *)


type event_system_t = 
    < new_group : unit -> group;
      new_wait_id : unit -> wait_id;
      exists_resource : operation -> bool;
      add_resource : group -> (operation * float) -> unit;
      add_weak_resource : group -> (operation * float) -> unit;
      add_close_action : group -> (Unix.file_descr * (Unix.file_descr -> unit)) -> unit;
      add_abort_action : group -> (group -> exn -> unit) -> unit;
      remove_resource : group -> operation -> unit;
      add_handler : group -> (event_system_t -> event Equeue.t -> event -> unit) -> unit;
      add_event : event -> unit;
      clear : group -> unit;
      run : unit -> unit;
      is_running : bool;
      when_blocking : (unit -> unit) -> unit  (* experimental *)
    >

class type event_system =
object
  method new_group : unit -> group
  method new_wait_id : unit -> wait_id
  method exists_resource : operation -> bool
  method add_resource : group -> (operation * float) -> unit
  method add_weak_resource : group -> (operation * float) -> unit
  method add_close_action : group -> (Unix.file_descr * (Unix.file_descr -> unit)) -> unit
  method add_abort_action : group -> (group -> exn -> unit) -> unit
  method remove_resource : group -> operation -> unit
  method add_handler : group -> (event_system_t -> event Equeue.t -> event -> unit) -> unit
  method add_event : event -> unit
  method clear : group -> unit
  method run : unit -> unit
  method is_running : bool
  method when_blocking : (unit -> unit) -> unit  (* experimental *)
end



type handler =
    event_system_t -> event Equeue.t -> event -> unit


exception Abort of (group * exn)


let () =
  Netexn.register_printer
    (Abort(new group_object, Not_found))
    (fun e ->
       match e with
	 | Abort(g,e') ->
	     "Unixqueue.Abort(" ^ string_of_int(Oo.id g) ^ 
	       ", " ^ Netexn.to_string e' ^ ")"
	 | _ -> assert false
    )


let string_of_fd fd =
  Int64.to_string (Netsys.int64_of_file_descr fd)
;;


let string_of_op =
  function
      Wait_in fd   -> sprintf "Wait_in(%s)" (string_of_fd fd)
    | Wait_out fd  -> sprintf "Wait_out(%s)" (string_of_fd fd)
    | Wait_oob fd  -> sprintf "Wait_oob(%s)" (string_of_fd fd)
    | Wait id      -> sprintf "Wait(wait_id %d)" (Oo.id id)
;;


let string_of_event ev =
  match ev with
    Input_arrived (g,fd) ->
      sprintf "Input(group %d, fd %s)" (Oo.id g) (string_of_fd fd)
  | Output_readiness (g, fd) ->
      sprintf "Output(group %d, fd %s)" (Oo.id g) (string_of_fd fd)
  | Out_of_band (g, fd) ->
      sprintf "Out_of_band(group %d, fd %s)" (Oo.id g) (string_of_fd fd)
  | Timeout (g, op) ->
      sprintf "Timeout(group %d, %s)" (Oo.id g) (string_of_op op)
  | Signal ->
      "Signal"
  | Extra x ->
      sprintf "Extra(%s)" (Netexn.to_string x)
  | Immediate(g,_) ->
      sprintf "Immediate(group %d)" (Oo.id g)
;;


let fd_cmp =
  match Sys.os_type with
    | "Win32" ->
	Pervasives.compare
    | _ ->
	(fun (fd1:Unix.file_descr) fd2 ->
	   (Obj.magic fd1 : int) - (Obj.magic fd2 : int)
	)


let is_op_eq = (* in native code faster then op1=op2 *)
  match Sys.os_type with
    | "Win32" ->
	(fun op1 op2 -> op1 = op2)
    | _ ->
	(fun op1 op2 ->
	   match op1 with
	     | Wait_in fd1 ->
		 ( match op2 with
		     | Wait_in fd2 -> fd1==fd2
		     | _ -> false
		 )
	     | Wait_out fd1 ->
		 ( match op2 with
		     | Wait_out fd2 -> fd1==fd2
		     | _ -> false
		 )
	     | Wait_oob fd1 ->
		 ( match op2 with
		     | Wait_oob fd2 -> fd1==fd2
		     | _ -> false
		 )
	     | Wait wid1 ->
		 ( match op2 with
		     | Wait wid2 -> (Oo.id wid1 = Oo.id wid2)
		     | _ -> false
		 )
	)


let op_cmp op1 op2 =
  match op1 with
    | Wait_in fd1 ->
	( match op2 with
	    | Wait_in fd2 -> fd_cmp fd1 fd2
	    | _ -> (-1)
	)
    | Wait_out fd1 ->
	( match op2 with
	    | Wait_in _ -> 1
	    | Wait_out fd2 -> fd_cmp fd1 fd2
	    | _ -> (-1)
	)
    | Wait_oob fd1 ->
	( match op2 with
	    | Wait_in _ -> 1
	    | Wait_out _ -> 1
	    | Wait_oob fd2 -> fd_cmp fd1 fd2
	    | _ -> (-1)
	)
    | Wait wid1 ->
	( match op2 with
	    | Wait wid2 -> compare (Oo.id wid1) (Oo.id wid2)
	    | _ -> 1
	)


let op_hash =
  match Sys.os_type with
    | "Win32" ->
        Hashtbl.hash
    | _ ->
        (fun op ->
           match op with
             | Wait_in fd -> (Obj.magic fd : int)
             | Wait_out fd -> 1031 + (Obj.magic fd : int)
             | Wait_oob fd -> 2029 + (Obj.magic fd : int)
             | Wait wid -> 3047 + Oo.id wid
        )


module OpSet =
  Set.Make
    (struct
       type t = operation
       let compare = op_cmp
     end
    )


module OpTbl =
  Hashtbl.Make
    (struct
       type t = operation
       let equal = is_op_eq
       let hash = op_hash
     end
    )


let epsilon_int esys g f =
  (* The callback is invoked again if the previous attempt caused an error.
     We protect here against this, and suppress any action in this case.
   *)
  let called_back = ref false in
  let f'() =
    if !called_back then
      ()
    else (
      called_back := true;
      f()
    ) in
  let e = Immediate(g, f') in
  esys#add_event e


let epsilon esys f =
  epsilon_int esys nogroup f


let once_int is_weak (esys:event_system) g duration f =
  if not is_weak && duration = 0.0 then
    epsilon_int esys g f
  else (
    let id = esys#new_wait_id () in
    let op = Wait id in
    let called_back = ref false in

    let e_ref = Timeout(g,op) in

    let handler _ ev e =
      if !called_back then (
	dlogr
	  (fun () ->
	     (sprintf
		"once handler <unexpected terminate group %d>" (Oo.id g)));
	raise Equeue.Terminate
      )
      else
	if e = e_ref then begin
	  dlogr
	    (fun () ->
	       (sprintf
		  "once handler <regular timeout group %d>" (Oo.id g)));
          esys#remove_resource g op;  (* delete the resource *)
          called_back := true;
          let () = f() in             (* invoke f (callback) *)
          raise Equeue.Terminate      (* delete the handler *)
	end
	else (
	  dlogr
	    (fun () ->
	       (sprintf
		  "once handler <rejected timeout group %d, got %s but expected %s >"
		  (Oo.id g) (string_of_event e) (string_of_event e_ref)));
          raise Equeue.Reject
	)
    in

    if duration >= 0.0 then (
      if is_weak then
	esys#add_weak_resource g (op, duration)
      else
	esys#add_resource g (op, duration);
      esys#add_handler g handler
    )
  )


let once = once_int false
let weak_once = once_int true


(*
let debug_log esys ?label msg =
  if Equeue.test_debug_target !debug_mode then
    prerr_endline("Unixqueue debug log: " ^
                    ( match label with
                          Some l -> l
                        | None -> "anonymous" ) ^
                    " <" ^ msg ^ ">")

let exn_log esys ?(suppressed = false) ?(to_string = Netexn.to_string)
                 ?label e =
  if Equeue.test_debug_target !debug_mode then
    let msg =
      if suppressed then
        "Suppressed exn " ^ to_string e
      else
        "Exn " ^ to_string e in
    debug_log esys ?label msg
 *)


let () =
  Netsys_signal.init()
