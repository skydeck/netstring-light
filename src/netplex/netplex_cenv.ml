(* $Id: netplex_cenv.ml 1774 2012-04-03 21:28:51Z gerd $ *)

open Netplex_types
open Printf

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netplex_cenv" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netplex_cenv" Debug.enable

let () =
  Netlog.Debug.register_module "Netplex_cenv" Debug.enable


exception Not_in_container_thread

let obj_of_thread = Hashtbl.create 10
  (* We assume here that obj_of_thread is filled with all parallelizers
     before Netplex starts the first thread. Hence, it is not protected
     by mutexes
   *)

let register_par par =
  if not (Hashtbl.mem obj_of_thread par#ptype) then (
    let (lock, unlock) = par # create_mem_mutex() in
    let m = Hashtbl.create 10 in
    Hashtbl.add obj_of_thread par#ptype (lock, unlock, par, m)
  )
;;


let register_cont (cont : container) thread =
  if thread#ptype <> `Controller_attached then (
    dlogr (fun () ->
	     sprintf "register_cont cont=%d thread=%s"
	       (Oo.id cont) thread#info_string);
    let (lock, unlock, par, m) =
      try Hashtbl.find obj_of_thread thread#ptype
      with Not_found -> 
	failwith "Netplex_cenv.register_cont: Unknown parallelizer type" in
    
    lock();
    Hashtbl.replace m thread#sys_id (`Container cont);
    unlock()
  )
;;


let register_ctrl (ctrl : controller) =
  if ctrl#ptype <> `Controller_attached then (
    let (lock, unlock, par, m) =
      try Hashtbl.find obj_of_thread ctrl#ptype
      with Not_found -> 
	failwith "Netplex_cenv.register_ctrl: Unknown parallelizer type" in
    
    lock();
    Hashtbl.replace m ctrl#sys_id (`Controller ctrl);
    unlock()
  )
;;


let unregister ptype sys_id =
  let (lock, unlock, par, m) =
    try Hashtbl.find obj_of_thread ptype
    with Not_found -> 
      failwith "Netplex_cenv.unregister: Unknown parallelizer type" in
    
  lock();
  Hashtbl.remove m sys_id;
  unlock();
  dlogr (fun () ->
	   sprintf "unregister remaining_objects=%d"
	     (Hashtbl.length m))


let unregister_cont cont thread =
  if thread#ptype <> `Controller_attached then (
    dlogr (fun () ->
	     sprintf "unregister_cont cont=%d thread=%s"
	       (Oo.id cont) thread#info_string);
    unregister thread#ptype thread#sys_id
  )
;;


let unregister_ctrl ctrl =
  if ctrl#ptype <> `Controller_attached then
    unregister ctrl#ptype ctrl#sys_id
;;



exception Found

let self_obj_par() =
  (* We do not know the parallelizer, so simply try them one after the other *)
  let found = ref None in
  try
    Hashtbl.iter
      (fun ptype (lock, unlock, par, m) ->
	 let my_sys_id = par # current_sys_id in
	 lock();
	 try
	   let obj = Hashtbl.find m my_sys_id in
	   unlock();
	   found := Some (obj, par);
	   raise Found
	 with
	   | Not_found ->
	       unlock();
      )
      obj_of_thread;
    raise Not_found
  with
    | Found ->
	match !found with
	  | None -> assert false
	  | Some (obj,par) -> (obj,par)
;;

let self_cont_par() =
  try
    match self_obj_par() with
      | (`Container c, par) -> (c,par)
      | _ -> raise Not_found
  with
    | Not_found -> raise Not_in_container_thread


let self_cont() =
  fst(self_cont_par())

let self_obj() =
  fst(self_obj_par())

let self_par() =
  try
    snd(self_obj_par())
  with
    | Not_found -> raise Not_in_container_thread

let current_sys_id() =
  (self_par()) # current_sys_id


let log level msg =
  let obj,_ = 
    try self_obj_par() 
    with Not_found -> raise Not_in_container_thread in
  match obj with
    | `Container cont -> 
	cont # log level msg
    | `Controller ctrl -> 
	ctrl # logger # log 
	  ~component:"netplex.controller"
	  ~level ~message:msg

let logf level fmt =
  Printf.ksprintf (log level) fmt

let report_connection_string fd detail =
  let fd_name =
    try Netsys.string_of_sockaddr(Unix.getsockname fd)
    with _ -> "*" in
  let fd_peer =
    try Netsys.string_of_sockaddr(Netsys.getpeername fd)
    with _ -> "*" in
  let cid =
    match current_sys_id() with
      | `Process pid -> "pid " ^ string_of_int pid
      | `Thread pid -> "thr " ^ string_of_int pid in
  sprintf "netplex.connection (%s) %s -> %s: %s"
    cid fd_peer fd_name
    (if detail = "" then "(ok)" else detail)


type timer = < f : timer -> bool; tmo : float; cont : container > ;;

let timer_table = Hashtbl.create 50
let timer_mutex = ( !Netsys_oothr.provider ) # create_mutex()


let cancel_timer_int do_clear tobj =
  let cont = self_cont() in
  dlogr (fun () -> sprintf "cancel_timer timer=%d cont=%d"
	   (Oo.id tobj) (Oo.id cont));
  let esys = cont#event_system in
  timer_mutex # lock();
  let g_opt =
    try Some(Hashtbl.find timer_table tobj) with Not_found -> None in
  Hashtbl.remove timer_table tobj;
  timer_mutex # unlock();
  if do_clear then
    match g_opt with
      | None -> ()
      | Some g -> 
	  Unixqueue.clear esys g


let cancel_timer = cancel_timer_int true

let rec restart_timer tobj g =
  let cont = self_cont() in
  dlogr (fun () -> sprintf "restart_timer timer=%d cont=%d"
	   (Oo.id tobj) (Oo.id cont));
  let esys = cont#event_system in
  timer_mutex # lock();
  Hashtbl.add timer_table tobj g;
  timer_mutex # unlock();
  Unixqueue.once esys g tobj#tmo 
    (fun () ->
       cancel_timer_int false tobj;
        dlogr (fun () -> sprintf "callback_timer timer=%d cont=%d"
		 (Oo.id tobj) (Oo.id cont));
       (* We let exceptions fall through to Netplex_container.run *)
       let flag = tobj#f tobj in
       if flag then restart_timer tobj g
    )


let create_timer f tmo =
  let cont = self_cont() in
  let esys = cont#event_system in
  let g = Unixqueue.new_group esys in
  let tobj =
    ( object
	method f = f
	method tmo = tmo
	method cont = cont
      end
    ) in
  dlogr (fun () -> sprintf "create_timer timer=%d cont=%d"
	   (Oo.id tobj) (Oo.id cont));
  restart_timer tobj g;
  tobj
  

let cancel_all_timers() =
  let cont = self_cont() in
  dlogr (fun () -> sprintf "cancel_all_timers cont=%d" (Oo.id cont));
  let esys = cont#event_system in
  timer_mutex # lock();
  let tlist = ref [] in
  Hashtbl.iter
    (fun tobj g ->
       if tobj # cont = cont then (
	 Unixqueue.clear esys g;
	 tlist := tobj :: !tlist
       )
    )
    timer_table;
  List.iter
    (fun tobj ->
       Hashtbl.remove timer_table tobj
    )
    !tlist;
  timer_mutex # unlock()


let timer_id tobj =
  Oo.id tobj


exception Container_variable_not_found of string
exception Container_variable_type_mismatch of string

let get_var name =
  let cont = self_cont() in
  try cont # var name 
  with Not_found -> raise(Container_variable_not_found name)

let int_var name =
  match get_var name with
    | `Int i -> i
    | _ -> raise(Container_variable_type_mismatch name)

let string_var name =
  match get_var name with
    | `String i -> i
    | _ -> raise(Container_variable_type_mismatch name)

let float_var name =
  match get_var name with
    | `Float i -> i
    | _ -> raise(Container_variable_type_mismatch name)

let bool_var name =
  match get_var name with
    | `Bool i -> i
    | _ -> raise(Container_variable_type_mismatch name)

let set_int_var name i =
  let cont = self_cont() in
  cont # set_var name (`Int i)

let set_string_var name i =
  let cont = self_cont() in
  cont # set_var name (`String i)

let set_float_var name i =
  let cont = self_cont() in
  cont # set_var name (`Float i)

let set_bool_var name i =
  let cont = self_cont() in
  cont # set_var name (`Bool i)

let make_var_type wrap unwrap =
  let get name =
    match get_var name with
      | `Encap x ->
	  ( try unwrap x
	    with
	      | Netplex_encap.Type_mismatch -> 
		  raise(Container_variable_type_mismatch name)
	  )
      | _ ->
	  raise(Container_variable_type_mismatch name) in
  let set name x =
    let cont = self_cont() in
    cont # set_var name (`Encap (wrap x)) in
  (get, set)

module type TYPE = sig type t end

module type VAR_TYPE = sig
  type t 
  val get : string -> t
  val set : string -> t -> unit
end

module Make_var_type(T:TYPE) = struct
  type t = T.t
  module E = Netplex_encap.Make_encap(T)
  let (get, set) = make_var_type E.wrap E.unwrap
end


let admin_connector() =
  let cont = self_cont() in
  match cont#lookup "netplex.controller" "admin" with
    | None ->
	failwith "Netplex_cenv.admin_connector: Socket not found"
    | Some path ->
	let c = Netplex_util.any_file_client_connector path in
	`Socket(Rpc.Tcp,
		c,
		Rpc_client.default_socket_config)

let admin_call f =
  let conn = admin_connector() in
  let client = Netplex_ctrl_clnt.Admin.V2.create_client2 conn in
  try
    f client ();
    Rpc_client.shut_down client
  with
    | err ->
	Rpc_client.shut_down client;
	raise err

let system_shutdown() =
  match self_obj() with
    | `Container _ ->
	admin_call Netplex_ctrl_clnt.Admin.V2.system_shutdown
    | `Controller ctrl ->
	ctrl # shutdown()

let system_restart() =
  match self_obj() with
    | `Container _ ->
	admin_call Netplex_ctrl_clnt.Admin.V2.restart_all
    | `Controller ctrl ->
	ctrl # restart()

let send_message pat msg args =
  match self_obj() with
    | `Container cont ->
	cont # send_message pat msg args
    | `Controller ctrl ->
	ctrl # send_message pat msg args

let lookup sname pname =
  let cont = self_cont() in
  cont # lookup sname pname

let lookup_container_sockets sname pname =
  let cont = self_cont() in
  cont # lookup_container_sockets sname pname

let pmanage() =
  let obj,_ = 
    try self_obj_par() 
    with Not_found -> raise Not_in_container_thread in
  let sockdir =
    match obj with
      | `Container cont -> 
          cont # socket_service # socket_service_config # controller_config 
            # socket_directory
    | `Controller ctrl -> 
        ctrl # controller_config # socket_directory in
  Netsys_pmanage.pmanage (Filename.concat sockdir "netplex.pmanage")

let run_in_esys esys f =
  let mutex = !Netsys_oothr.provider # create_mutex() in
  let cond = !Netsys_oothr.provider # create_condition() in
  let g = Unixqueue.new_group esys in
  let r = ref (fun () -> assert false) in
  Unixqueue.once esys g 0.0
    (fun () ->
       ( try
	   f();
	   mutex # lock();
	   r := (fun () -> ());
	   mutex # unlock();
	 with
	   | e -> 
	       mutex # lock();
	       r := (fun () -> raise e);
	       mutex # unlock();
       );
       cond # signal()
    );
  mutex # lock();
  cond # wait mutex;
  mutex # unlock();
  !r()
  

let run_in_controller_context ctrl f =
  if ctrl#ptype <> `Multi_threading then
    failwith "Netplex_cenv.run_in_controller_context: only possible for multi-threaded environments";
  let esys = ctrl # event_system in
  run_in_esys esys f


let run_in_container_context cont f =
  if cont#ptype <> `Multi_threading then
    failwith "Netplex_cenv.run_in_container_context: only possible for multi-threaded environments";
  let esys = cont # event_system in
  run_in_esys esys f


module type FUN_TYPE = 
  sig 
    type s  (** argument type *)
    type r  (** result type *)
  end

module type LEVER = sig
  type s  (** argument type *)
  type r  (** result type *)
  type t = s->r

  val register : Netplex_types.controller -> 
                 (Netplex_types.controller -> t) -> t
end

module Make_lever(T:FUN_TYPE) = struct
  type s = T.s
  type r = T.r
  type t = s->r

  module ES = Netplex_encap.Make_encap(struct type t = s end)
  module ER = Netplex_encap.Make_encap(struct type t = r end)

  let register ctrl raw_lever =
    let id =
      ctrl # register_lever
	(fun ctrl enc_arg ->
	   let arg = ES.unwrap enc_arg in
	   let res = raw_lever ctrl arg in
	   ER.wrap res
	) in
    (fun arg ->
       let cont = self_cont() in
       let res_enc = cont # activate_lever id (ES.wrap arg) in
       ER.unwrap res_enc
    )
end
