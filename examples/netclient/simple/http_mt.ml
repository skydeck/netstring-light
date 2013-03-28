(* HTTP Pipelines and multithreading
 *
 *
 * This is the recommended solution for multi-threaded apps:
 * A designated HTTP thread handles all HTTP requests, and the other threads
 * of the program send their HTTP requests to the HTTP thread. The HTTP
 * thread can process several requests in parallel.
 *)

(* Compile with:
 * ocamlfind ocamlopt -o t -package netclient,threads -linkpkg -thread http_mt.ml
 *)


open Http_client

exception HTTP_Job of http_call * (http_call -> unit)
  (* This is not an exception in the usual sense, but simply a tagged
   * pair (call, f_done). This pair is pushed onto the event queue to
   * send another HTTP request [call] to the HTTP thread. When the
   * request is processed, the function [f_done] is called. Note that
   * [f_done] is called in the context of the HTTP thread, and it must
   * arrange some synchronisation with the calling thread to return
   * the result.
   *)


let http_esys = ref None

let get_http_esys() =
  match !http_esys with
    | None -> failwith "No event system"
    | Some e -> e

let http_keep_alive_group = ref None 

let get_http_keep_alive_group() =
  match !http_keep_alive_group with
    | None -> failwith "No keep alive group"
    | Some g -> g


let http_init() =
  let esys = Unixqueue.create_unix_event_system() in
  let keep_alive_group = Unixqueue.new_group esys in
  http_esys := Some esys;
  http_keep_alive_group := Some keep_alive_group
;;


let http_thread() =
  (* Create the HTTP pipeline for a known event system: *)
  let esys = get_http_esys() in
  let pipeline = new pipeline in
  pipeline # set_event_system esys;

  (* In order to keep the event system active when there are no HTTP requests
   * to process, we add an artificial timer that never times out (-1.0).
   * The timer is bound to a Unixqueue group, and by clearing this group
   * the timer can be deleted.
   *)
  let keep_alive_group = get_http_keep_alive_group() in
  let w = Unixqueue.new_wait_id esys in
  Unixqueue.add_resource esys keep_alive_group (Unixqueue.Wait w,(-1.0));

  (* We arrange now that whenever a HTTP_Job arrives on the event queue,
   * a new HTTP call is started.
   *)
  Unixqueue.add_handler
    esys
    keep_alive_group
    (fun _ _ event ->
       match event with
	 | Unixqueue.Extra (HTTP_Job (call, f_done)) ->
	     pipeline # add_with_callback call f_done
	 | _ ->
	     raise Equeue.Reject  (* The event is not for us *)
    );

  (* Now start the event queue. It returns when all jobs are done and
   * the keep_alive_group is cleared.
   *)
  Unixqueue.run esys;
  ()
;;


let shutdown_http_thread() =
  let esys = get_http_esys() in
  let keep_alive_group = get_http_keep_alive_group() in
  Unixqueue.clear esys keep_alive_group;
  http_keep_alive_group := None;
  http_esys := None
;;


let caller_thread() =
  (* This is a thread that calls for an HTTP request *)
  let esys = get_http_esys() in
  let mutex = Mutex.create() in
  let cond = Condition.create () in
  let call = new get "http://localhost/" in
  let result = ref "" in
  let f_done call =
    (* This function is called from the scope of the HTTP thread!
     * Signal the calling thread that the call is done:
     *)
    Mutex.lock mutex;
    result := ( match call # status with
		  | `Successful ->
		      let body = call # response_body # value in
		      body
		  | _ ->
		      "some problem"
	      );
    Condition.signal cond;
    Mutex.unlock mutex
  in
  Unixqueue.add_event esys (Unixqueue.Extra (HTTP_Job(call, f_done)));
  (* Wait until we get a signal: *)
  Mutex.lock mutex;
  Condition.wait cond mutex;
  print_endline !result;
  flush stdout;
  Mutex.unlock mutex
;;


let _ =
  (* Unixqueue.set_debug_mode true; *)

  (* Initialize first: *)
  http_init();

  (* Start the HTTP thread: *)
  let http_thr = Thread.create http_thread () in
  
  (* Start a lot of caller threads: *)
  let callers = ref [] in
  for n = 1 to 100 do
    let thr = Thread.create caller_thread () in
    callers := thr :: !callers
  done;

  (* Wait until the callers return: *)
  List.iter Thread.join !callers;

  (* Shut down the HTTP thread, and wait until it is done *)
  shutdown_http_thread();
  Thread.join http_thr
;;

  
