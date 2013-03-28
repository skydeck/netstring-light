(* $Id: qserver.ml 1219 2009-04-14 13:28:56Z ChriS $
 * ----------------------------------------------------------------------
 *
 *)


(* TODO:
 * - Improve log messages
 * - Keep qlist backup
 * - Authentication
 *   Rpc_server: limit message size. Find out peer IP address, port number.
 * - Auth protocol: Password authentication
 * - Put all config options into config section.
 *)

let spooldir = "/var/spool/qserver";;
let max_chunksize = Int32.of_string "1048576" ;;   (* 1 MB *)



open Rtypes
open Xdr
open Rpc
open Rpc_server

module A = Queues_aux ;;
module S1 = Queues_srv.QUEUESPROG.QUEUESVERS1 ;;

type error =
    [ `not_found
      | `picked
      | `exists
      | `not_picked
      | `chunks_too_large
      | `timeout
      | `empty
      | `queue_deleted
      | `full
      | `bad_name
      | `bad_value
      | `inactive
      | `permission_denied
      | `sys_error
    ]

exception Error of error

let log message =
  (* Output [message] to the log medium. This is currently stderr. *)
  prerr_endline message
;;

let check_name name =
  (* Checks that [name] is a proper name. Raises Error if not. *)
  for i = 0 to String.length name - 1 do
    match name.[i] with
	'A'..'Z' -> ()
      | 'a'..'z' -> ()
      | '0'..'9' -> ()
      | '_'      -> ()
      | _        -> raise(Error `bad_name)
  done
;;


let catch_error f reply =
  try (f reply : unit)
  with
      Error code ->
	(* This match...with is only to help the typechecker inferring the
	 * right type
	 *)
	(match code with
           #error as c -> reply c
	)
    | Sys_error e ->
	log ("Sys_error: " ^ e);
	reply `sys_error
    | Unix.Unix_error(code,msg,fn) ->
	log ("Unix_error: " ^ Unix.error_message code ^
 	     (if msg <> "" then ": " ^ msg else "") ^
	     (if fn <> "" then " [" ^ fn ^ "]" else ""));
	reply `sys_error
;;


let protected_call f what =
  try
    f what
  with
      err ->
	log("Exception in deferred function: " ^
	    Printexc.to_string err);
	false    (* remove from [ongrow], [onshrink] *)
;;


let with_out_file file fn =
  try
    let r = fn file in
    close_out file;
    r
  with
      err -> close_out file; raise err
;;


let with_in_file file fn =
  try
    let r = fn file in
    close_in file;
    r
  with
      err -> close_in file; raise err
;;


let directory_list name =
  let l = ref [] in
  let dir = Unix.opendir name in
  try
    while true do
      match Unix.readdir dir with
	  "." | ".." -> ()
	| name ->
	    l := name :: !l
    done; assert false
  with
      End_of_file ->
	Unix.closedir dir;
	List.rev !l
    | err ->
	Unix.closedir dir;
	raise err
;;


let t_queue = Xdr.validate_xdr_type A.xdrt_queue;;
let t_entry = Xdr.validate_xdr_type A.xdrt_entry;;


type managed_entry =
    { qentry : A.entry;
      mutable is_picked : bool;
      mutable pick_connection : Rpc_server.connection_id option;
      mutable is_uploading : bool;
      mutable upload_connection : Rpc_server.connection_id option;
      mutable current_downloads : string list;   (* dhandles *)
      mutable current_upload : string option;    (* uhandle *)
    }

type managed_queue =
    { mutable qentries : managed_entry list;
      mutable next_eid : int;
      mutable qinfo : A.queue;
      mutable deleted : bool;
      mutable onshrink : (float * (bool -> bool)) list;
        (* These functions are called in order when the queue shrinks.
	 * If the functions return 'false' they are removed
	 * from the [ongrow] list.
	 * The float is the absolute time when the function should be
	 * called anyway (timeout). The argument passed to the called
	 * function indicates whether the reason of the call is a timeout (true)
	 * or a shrinking queue (false).
	 *)
      mutable ongrow : (float * (bool -> bool)) list;
        (* These functions are called in order when an entry is added to
	 * a queue. If the functions return 'false' they are removed
	 * from the [ongrow] list.
	 * The float is the absolute time when the function should be
	 * called anyway (timeout). The argument passed to the called
	 * function indicates whether the reason of the call is a timeout (true)
	 * or a growing queue (false).
	 *)
    }

type managed_download =
    { mutable downfile : in_channel;
      mutable downsize : int;
      mutable downchunksize : int;
      mutable downserial : int64;
              downentry : managed_entry;
	      downconnection : connection_id;
    }

type managed_upload =
    { mutable upfile : out_channel;
      mutable upserial : int64;
              upentry : managed_entry;
	      upconnection : connection_id;
    }


let queues = (Hashtbl.create 100 : (string, managed_queue) Hashtbl.t) ;;
  (* Cached queues *)

let next_dhandle = ref 0;;
let next_uhandle = ref 0;;

let dhandles = (Hashtbl.create 100 : (string, managed_download) Hashtbl.t) ;;
let uhandles = (Hashtbl.create 100 : (string, managed_upload) Hashtbl.t) ;;

let instance_prefix
  = Digest.string (string_of_float (Unix.gettimeofday())) ;;
  (* A prefix that is prepended before the dhandle and uhandle numbers in
   * the protocol. The instance_prefix distinguishes between instances of
   * qserver.
   *)


let queue_accepts_entries mq =
  let maxl = mq.qinfo.A.qparams.A.qmaxlen in
  mq.qinfo.A.qparams.A.qaccepting &&
  ( maxl < Int32.zero ||
    let l = List.length mq.qentries in
    (Int32.of_int l) < maxl
  )
;;


let queue_delivers_entries mq =
  mq.qinfo.A.qparams.A.qdelivering &&
  ( (* At least one unpicked entry needed: *)
    List.exists
      (fun e -> not e.is_picked && not e.is_uploading)
      mq.qentries
  )
;;


let load_managed_queue qid =
  (* Reads the "qlist" file and the "e*" files containing the meta data
   * of the entries
   *)
  let queuedir = Filename.concat spooldir qid in
  let infofile = Filename.concat queuedir "qinfo" in
  let listfile = Filename.concat queuedir "qlist" in
  if not (Sys.file_exists infofile) then raise (Error `not_found);
  let qinfo =
    with_in_file (open_in_bin infofile)
      (fun f ->
	 let l = in_channel_length f in
	 let buf = String.create l in
	 really_input f buf 0 l;
	 A._to_queue (Xdr.unpack_xdr_value ~fast:true buf t_queue [])
      )
  in
  let eid_list =
    with_in_file (open_in listfile)
      (fun f ->
	 let l = ref [] in
	 try
	   while true do
	     let line = input_line f in
	     l := (int_of_string line) :: !l;
	   done;
	   assert false
	 with
	     End_of_file -> List.rev !l
      )
  in
  let next_eid =
    (List.fold_left max 0 eid_list) + 1 in
  let mentry_list =
    List.map
      (fun eid ->
	 let qentry_file =
	   Filename.concat queuedir ("e" ^ string_of_int eid) in
	 let v =
	   with_in_file (open_in_bin qentry_file)
	     (fun f ->
		let l = in_channel_length f in
		let buf = String.create l in
		really_input f buf 0 l;
		buf
	     )
	 in
	 let e =
	   A._to_entry (Xdr.unpack_xdr_value ~fast:true v t_entry []) in
	 { qentry = e;
	   is_picked = false;
	   pick_connection = None;
	   is_uploading = false;
	   upload_connection = None;
	   current_downloads = [];
	   current_upload = None
	 }
      )
      eid_list
  in
  let mqueue =
    { qentries = mentry_list;
      next_eid = next_eid;
      deleted = false;
      qinfo = qinfo;
      onshrink = [];
      ongrow = [];
    }
  in
  mqueue
;;


let store_managed_queue qid mq =
  (* Writes the "qlist" file. Does not write anything else. *)
  let queuedir = Filename.concat spooldir qid in
  let infofile = Filename.concat queuedir "qinfo" in
  let listfile = Filename.concat queuedir "qlist" in
  if not (Sys.file_exists infofile) then raise (Error `not_found);
  (* Update qmodified: *)
  mq.qinfo.A.qmodified <- Int64.of_float (Unix.time());
  with_out_file (open_out_bin infofile)
    (fun f ->
       let v = A._of_queue mq.qinfo in
       Xdr.pack_xdr_value v t_queue [] (output_string f);
    );
  with_out_file (open_out listfile)
    (fun f ->
       List.iter
	 (fun me ->
	    (* Write now the "qlist" file. Don't include entries currently
	     * being uploaded because these are considered as non-persistent
	     * until the upload finishes.
	     * Picked entries are considered as still persistent, so include
	     * them.
	     *)
	    if not me.is_uploading then begin
	      output_string f me.qentry.A.eid;
	      output_string f "\n";
	    end
	 )
	 mq.qentries;
    )
;;


let lookup_managed_queue qid =
  (* Checks whether the queue is cached. If yes, the cached queue record
   * is returned. Otherwise, the queue is loaded and added to the cache.
   *)
  try
    Hashtbl.find queues qid
  with
      Not_found ->
	let mq = load_managed_queue qid in
	Hashtbl.add queues qid mq;
	(* Note: There are not any picked and uploaded entries, so reset: *)
	mq.qinfo.A.qpicked <- Int32.zero;
	mq.qinfo.A.quploads <- Int32.zero;
	(* Check the length: *)
	let l = Int32.of_int (List.length mq.qentries) in
	if l <> mq.qinfo.A.qlength then begin
	  log "Load error: The qlength field is not correct. (fixed)";
	  mq.qinfo.A.qlength <- l;
	end;
	mq
;;


let proc_ping session () reply =
  reply()
;;

let proc_create_queue session queuename =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: queuename must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name queuename;
       let queuedir = Filename.concat spooldir queuename in
       let infofile = Filename.concat queuedir "qinfo" in
       let listfile = Filename.concat queuedir "qlist" in
       (* Check if the queue already exists: *)
       if Sys.file_exists infofile then raise(Error `exists);
       (* Create the directory [queuedir] if necessary: *)
       if not (Sys.file_exists queuedir) then
	 Unix.mkdir queuedir 0o777;
       (* Create qlist: *)
       with_out_file (open_out listfile)
	 (fun f -> () );
       (* Create qinfo: *)
       let now = Int64.of_float (Unix.time()) in
       let qinfo = { A.qid = queuename;
		     A.qname = queuename;
		     A.qowner = user;
		     A.qcreation = now;
		     A.qmodified = now;
		     A.qlength = Int32.zero;
		     A.qpicked = Int32.zero;
		     A.quploads = Int32.zero;
		     A.qparams = { A.qmaxlen = Int32.minus_one;
				   A.qactive = false;
				   A.qaccepting = true;
				   A.qdelivering = true;
				 }
		   } in
       with_out_file (open_out_bin infofile)
	 (fun f ->
	    let v = A._of_queue qinfo in
	    Xdr.pack_xdr_value v t_queue [] (output_string f);
	 );
       reply `successful
    )
;;


let proc_delete_queue session queuename =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: queuename must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name queuename;
       let queuedir = Filename.concat spooldir queuename in
       (* Remove the files: *)
       let files = directory_list queuedir in
       List.iter
	 (fun f -> let path = Filename.concat queuedir f in Sys.remove path)
	 files;
       (* Remove the directory. It is ok if this fails (no permission) *)
       ( try Unix.rmdir queuedir with _ -> ());
       (* Is there a managed_queue? *)
       begin try
	 let mq = Hashtbl.find queues queuename in  (* or Not_found *)
	 if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
	 Hashtbl.remove queues queuename;
	 mq.deleted <- true;
	 (* Remove all upload/download handles: *)
	 List.iter
	   (fun me ->
	      List.iter
		(Hashtbl.remove dhandles)
		me.current_downloads;
	      ( match me.current_upload with
		    None -> ()
		  | Some uh -> Hashtbl.remove uhandles uh
	      );
	   )
	   mq.qentries;
	 (* Tell the [ongrow] and [onshrink] handlers that something
	  * errorneous happened
	  *)
	 List.iter
	   (fun (_,f) -> ignore(protected_call f true)) mq.ongrow;
	 List.iter
	   (fun (_,f) -> ignore(protected_call f true)) mq.onshrink;
	 mq.ongrow <- [];
	 mq.onshrink <- [];
       with
	   Not_found -> ()   (* no managed_queue *)
       end;
       reply `successful
    )
;;


let proc_list_queues session () =
  catch_error
    (fun reply ->
       (* Get all files in the spooldir: *)
       let l = directory_list spooldir in
       (* Keep only files that are actually directories containing "qinfo": *)
       let l' =
	 List.filter
	   (fun name ->
	      Sys.file_exists
		(Filename.concat
		   (Filename.concat spooldir name)
		   "qinfo")
	   )
	   l in
       (* Read in the "qinfo" files, and decode them: *)
       let qlist =
	 List.map
	   (fun name ->
	      let mq = lookup_managed_queue name in
	      mq.qinfo
	   )
	   l'
       in
       (* Pass the result back: *)
       reply (`successful (Array.of_list qlist))
    )
;;


let proc_get_queue session queuename =
  catch_error
    (fun reply ->
       (* Sanity check: queuename must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name queuename;
       let mq = lookup_managed_queue queuename in
       reply (`successful mq.qinfo);
    )
;;


let proc_set_queue session (queuename,params) =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: queuename must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name queuename;
       let mq = lookup_managed_queue queuename in
       if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
       if params.A.qactive then begin
	 (* Activate the queue/change limits: *)
	 let old_accepting = queue_accepts_entries mq in
	 let old_delivering = queue_delivers_entries mq in
	 mq.qinfo.A.qparams <- params;
	 let new_accepting = queue_accepts_entries mq in
	 let new_delivering = queue_delivers_entries mq in
	 store_managed_queue queuename mq;
	 (* NOTE: If the queue has previously been inactive, the [ongrow] and
	  * [onshrink] lists are empty. So the following statements do
	  * nothing.
	  *)
	 (* Maybe there is now space for growth: *)
	 if not old_delivering && new_delivering then
	   mq.ongrow <-
             List.filter (fun (_,f) -> protected_call f false) mq.ongrow;
	 (* Maybe there is now an entry to pick: *)
	 if not old_accepting && new_accepting then
	   mq.onshrink <-
             List.filter (fun (_,f) -> protected_call f false) mq.onshrink;
       end
       else begin
	 (* Deactivate the queue: *)
	 mq.qinfo.A.qparams <- params;
	 store_managed_queue queuename mq;
	 (* Make that everybody notices the inactive queue: *)
	 mq.ongrow <-
           List.filter (fun (_,f) -> protected_call f true) mq.ongrow;
	 mq.onshrink <-
           List.filter (fun (_,f) -> protected_call f true) mq.onshrink;
	 (* Make these lists empty: *)
	 if mq.ongrow <> [] then
	   log "Warning: After deactivation the ongrow list is not empty";
	 if mq.onshrink <> [] then
	   log "Warning: After deactivation the onshrink list is not empty";
	 mq.ongrow <- [];
	 mq.onshrink <- [];
       end;
       reply `successful
    )
;;


let proc_list_queue_entries session qid =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: qid must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name qid;
       let mq = lookup_managed_queue qid in
       if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
       let visible =
	 List.filter
	   (fun qe ->
	      not qe.is_picked && not qe.is_uploading
	   )
	   mq.qentries
       in
       let visible' = List.map (fun qe -> qe.qentry) visible in
       reply (`successful (Array.of_list visible'))
    )
;;


let timeout_pick mq =  (* raise the right exception *)
  if mq.deleted then
    raise (Error `queue_deleted)
  else
    if not mq.qinfo.A.qparams.A.qactive then
      raise (Error `inactive)
  else
    if mq.qinfo.A.qparams.A.qdelivering then
      raise (Error `empty)
    else
      raise (Error `timeout)
;;


let try_pick session mq reply0 timed_out =
  let keep = ref true in
  let reply r =
    (* If [reply] is called, the function [try_pick] must return [false]
     * to indicate the it is to be removed from the [ongrow] list
     *)
    keep := false; reply0 r
  in
  catch_error
    (fun reply ->
       if timed_out then timeout_pick mq;
       if not mq.qinfo.A.qparams.A.qactive then raise(Error `inactive);
       try
	 if not mq.qinfo.A.qparams.A.qdelivering then raise Not_found;
	 let first =
	   List.find
	     (fun qe -> not qe.is_picked && not qe.is_uploading)
	     mq.qentries
	 in (* or Not_found *)
	 reply (`successful first.qentry);  (* may raise Connection_lost *)
	 first.is_picked <- true;
	 first.pick_connection <- Some(Rpc_server.get_connection_id session);
	 mq.qinfo.A.qpicked <- Int32.succ mq.qinfo.A.qpicked;
	 mq.qinfo.A.qlength <- Int32.pred mq.qinfo.A.qlength;
       with
	   Not_found -> ()
	       (* Try again *)
    )
    reply;
  !keep




let proc_pick_queue_entry session (qid,timeout) =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: qid must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name qid;
       let mq = lookup_managed_queue qid in
       if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
       if try_pick session mq reply false then begin
	 (* [try_pick] was not successful. Add [try_pick] to the [ongrow]
	  * list
	  *)
	 if timeout = Int32.zero then timeout_pick mq;
	 let max_time =
	   if timeout >= Int32.zero then
	     Unix.time() +. Int32.to_float timeout
           else
	     (-1.0)
	 in
	 mq.ongrow <- mq.ongrow @ [ max_time, try_pick session mq reply ];
       end
    )
;;


let stop_downloads mq entry =
  List.iter
    (fun dhandle ->
       let dl =
	 try Hashtbl.find dhandles dhandle
	 with Not_found -> assert false
       in
       close_in dl.downfile;
       Hashtbl.remove dhandles dhandle
    )
    entry.current_downloads;
  mq.qinfo.A.qpicked <- Int32.pred mq.qinfo.A.qpicked;
  mq.qinfo.A.qlength <- Int32.succ mq.qinfo.A.qlength;
  entry.current_downloads <- []
;;


let proc_return_picked_queue_entry session (qid,eid) =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: qid must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name qid;
       let mq = lookup_managed_queue qid in
       if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
       let entry =
	 try
	   List.find
	     (fun qe -> qe.qentry.A.eid = eid)
	     mq.qentries
	 with Not_found -> raise (Error `not_found)
       in

       if not entry.is_picked then raise (Error `not_picked );
       if entry.pick_connection <> Some(Rpc_server.get_connection_id session)
       then raise(Error `not_found);

       entry.is_picked <- false;
       (* Stop all current downloads: *)
       stop_downloads mq entry;
       reply `successful
    )
;;


let proc_remove_picked_queue_entry session (qid,eid) =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: qid must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name qid;
       let mq = lookup_managed_queue qid in
       if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
       let entry =
	 try
	   List.find
	     (fun qe -> qe.qentry.A.eid = eid)
	     mq.qentries
	 with Not_found -> raise (Error `not_found)
       in

       if not entry.is_picked then raise (Error `not_picked );
       if entry.pick_connection <> Some(Rpc_server.get_connection_id session)
       then raise(Error `not_found);

       (* Stop all current downloads: *)
       stop_downloads mq entry;
       (* Remove this entry from the list, and store the list: *)
       mq.qentries <- List.filter
	                (fun qe -> qe.qentry.A.eid <> eid)
	                mq.qentries;
       mq.qinfo.A.qlength <- Int32.pred mq.qinfo.A.qlength;
       store_managed_queue qid mq;
       (* Remove the "e*" and "d*" files: *)
       let queuedir = Filename.concat spooldir qid in
       let qentry_file = Filename.concat queuedir ("e" ^ eid) in
       let qdata_file = Filename.concat queuedir ("d" ^ eid) in
       Sys.remove qentry_file;
       Sys.remove qdata_file;
       reply `successful;
       (* Finally, consider the [onshrink] activities: *)
       mq.onshrink <- List.filter
	                (fun (_,f) -> protected_call f false) mq.onshrink
    )
;;


let proc_remove_queue_entry session (qid,eid) =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: qid must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name qid;
       let mq = lookup_managed_queue qid in
       if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
       let entry =
	 try
	   List.find
	     (fun qe -> qe.qentry.A.eid = eid)
	     mq.qentries
	 with Not_found -> raise (Error `not_found)
       in

       if entry.is_picked &&
	  entry.pick_connection <> Some(Rpc_server.get_connection_id session)
       then raise(Error `not_found);
       if entry.is_picked then raise (Error `picked );
       if entry.is_uploading then raise (Error `sys_error);  (* strange *)

       (* Remove this entry from the list, and store the list: *)
       mq.qentries <- List.filter
	                (fun qe -> qe.qentry.A.eid <> eid)
	                mq.qentries;
       mq.qinfo.A.qlength <- Int32.pred mq.qinfo.A.qlength;
       store_managed_queue qid mq;
       (* Remove the "e*" and "d*" files: *)
       let queuedir = Filename.concat spooldir qid in
       let qentry_file = Filename.concat queuedir ("e" ^ eid) in
       let qdata_file = Filename.concat queuedir ("d" ^ eid) in
       Sys.remove qentry_file;
       Sys.remove qdata_file;
       reply `successful;
       (* Finally, consider the [onshrink] activities: *)
       mq.onshrink <- List.filter
	                (fun (_,f) -> protected_call f false) mq.onshrink
    )
;;


let proc_download_entry session (qid,eid,chunksize) =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: qid must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name qid;
       if chunksize > max_chunksize then raise (Error `chunks_too_large);
       if chunksize <= Int32.zero then raise (Error `bad_value);
       let ichunksize = Int32.to_int chunksize in
       let mq = lookup_managed_queue qid in
       if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
       let entry =
	 try
	   List.find
	     (fun qe -> qe.qentry.A.eid = eid)
	     mq.qentries
	 with Not_found -> raise (Error `not_found)
       in

       if not entry.is_picked then raise (Error `not_picked );
       if entry.pick_connection <> Some(Rpc_server.get_connection_id session)
       then raise(Error `not_found);

       (* Create new dhandle: *)
       let n = !next_dhandle in
       incr next_dhandle;
       let dhandle =
	 instance_prefix ^ Rtypes.int4_as_string (Rtypes.int4_of_int n) in
       (* Create new download record *)
       let queuedir = Filename.concat spooldir qid in
       let qdata_file = Filename.concat queuedir ("d" ^ eid) in
       let file = open_in_bin qdata_file in
       begin try
	 let dl =
	   { downfile = file;
	     downsize = in_channel_length file;
	     downchunksize = ichunksize;
	     downserial = Int64.zero;
	     downentry = entry;
	     downconnection = Rpc_server.get_connection_id session;
	   }
	 in
	 Hashtbl.add dhandles dhandle dl;
	 entry.current_downloads <- dhandle :: entry.current_downloads;
       with
	   err -> close_in file; raise err
       end;
       (* Note: [file] remains open. It will be closed once the download
	* is finished, or the download stops for some reason.
	*)
       (* Return the dhandle: *)
       reply (`successful dhandle)
    )
;;


let proc_download_chunk session dhandle =
  catch_error
    (fun reply ->
       let dl =
	 try Hashtbl.find dhandles dhandle
	 with Not_found -> raise (Error `not_found)
       in
       if dl.downconnection <> Rpc_server.get_connection_id session
       then raise(Error `not_found);

       let buf = String.create (dl.downchunksize) in
       let n = input dl.downfile buf 0 dl.downchunksize in
       let chunk =
	 { A.serial = dl.downserial;
	   A.last = pos_in dl.downfile >= dl.downsize;
	   A.data =
	     if n < dl.downchunksize then
	       String.sub buf 0 n
	     else
	       buf;
	 }
       in
       dl.downserial <- Int64.succ dl.downserial;
       reply (`successful chunk)
    )
;;


let timeout_upload mq =  (* raise the right exception *)
  if mq.deleted then
    raise (Error `queue_deleted)
  else
    if not mq.qinfo.A.qparams.A.qactive then
      raise (Error `inactive)
  else
    if mq.qinfo.A.qparams.A.qaccepting then
      raise (Error `full)
    else
      raise (Error `timeout)
;;


let stop_upload mq entry =
  match entry.current_upload with
      None -> ()
    | Some uhandle ->
	let ul =
	  try Hashtbl.find uhandles uhandle
	  with Not_found -> assert false
	in
	close_out ul.upfile;
	let queuedir = Filename.concat spooldir entry.qentry.A.eqid in
	let qdata_file = Filename.concat queuedir ("d" ^ entry.qentry.A.eid) in
	Sys.remove qdata_file;
	Hashtbl.remove uhandles uhandle;
	mq.qinfo.A.quploads <- Int32.pred mq.qinfo.A.quploads;
	entry.current_upload <- None
;;


let start_upload session mq props reply0 timed_out =
  let keep = ref true in
  let reply r =
    (* If [reply] is called, the function [start_upload] must return [false]
     * to indicate the it is to be removed from the [onshrink] list
     *)
    keep := false; reply0 r
  in
  catch_error
    (fun reply ->
       if timed_out then timeout_upload mq;
       if not mq.qinfo.A.qparams.A.qactive then raise (Error `inactive);
       (* Is there space in the queue? *)
       let qid = mq.qinfo.A.qid in
       if queue_accepts_entries mq then begin
	 (* Create new uhandle: *)
	 let n = !next_uhandle in
	 incr next_uhandle;
	 let uhandle =
	   instance_prefix ^ Rtypes.int4_as_string (Rtypes.int4_of_int n) in
	 (* Create new entry record in memory: *)
	 let eid = string_of_int (mq.next_eid) in
	 mq.next_eid <- mq.next_eid + 1;
	 let qe =
	   { A.eid = eid;
	     A.eqid = qid;
	     A.ecreation = Int64.of_float (Unix.time());
	     A.esize = Int64.zero;
	     A.eprops = props;
	   }
	 in
	 let entry =
	   { qentry = qe;
	     is_picked = false;
	     pick_connection = None;
	     is_uploading = true;
	     upload_connection = Some (Rpc_server.get_connection_id session);
	     current_downloads = [];
	     current_upload = Some uhandle;
	   }
	 in
	 (* Create new data file: *)
	 let queuedir = Filename.concat spooldir qid in
	 let qdata_file = Filename.concat queuedir ("d" ^ eid) in
	 let file = open_out_bin qdata_file in
	 (* Note: the entry file will be created when the upload is finished *)
	 begin try
	   (* Create new upload record: *)
	   let ul =
	     { upfile = file;
	       upserial = Int64.zero;
	       upentry = entry;
	       upconnection = Rpc_server.get_connection_id session;
	     }
	   in
	   (* Register [entry]: *)
	   mq.qentries <- mq.qentries @ [ entry ];
	   mq.qinfo.A.quploads <- Int32.succ mq.qinfo.A.quploads;
	   (* Register [ul]: *)
	   Hashtbl.add uhandles uhandle ul;
	 with
	     err -> close_out file; raise err
	 end;
	 (* The upload is accepted: *)
	 try
	   reply (`successful uhandle);
	 with
	     Rpc_server.Connection_lost ->
	       stop_upload mq entry
       end
    )
    reply;
  !keep
;;


let proc_upload_entry session (qid,props,timeout) =
  catch_error
    (fun reply ->
       let user = Rpc_server.get_user session in
       (* Sanity check: qid must only consist of alphanumeric characters,
	* including underscore
	*)
       check_name qid;
       (* If the queue is short enough, begin the upload immediately;
	* otherwise wait
	*)
       let mq = lookup_managed_queue qid in
       if mq.qinfo.A.qowner <> user then raise(Error `permission_denied);
       if start_upload session mq props reply false then begin
	 (* Upload is not possible now. Check if should do it later. *)
	 if timeout = Int32.zero then raise (Error `timeout);
	 let max_time = if timeout >= Int32.zero then
	                   Unix.time() +. Int32.to_float timeout
	                else
			  -1.0
	 in
	 mq.onshrink <- mq.onshrink @
	                [ max_time, start_upload session mq props reply ];
       end
    )
;;


let proc_upload_chunk session (uhandle,chunk) =
  catch_error
    (fun reply ->
       let ul =
	 try Hashtbl.find uhandles uhandle
	 with Not_found -> raise (Error `not_found)
       in
       if ul.upconnection <> Rpc_server.get_connection_id session
       then raise(Error `not_found);

       if chunk.A.serial <> ul.upserial then raise (Error `bad_value);
       output_string ul.upfile chunk.A.data;
       let l = String.length chunk.A.data in
       ul.upentry.qentry.A.esize <-
         Int64.add ul.upentry.qentry.A.esize (Int64.of_int l);
       if chunk.A.last then begin
	 let entry = ul.upentry in
	 (* Close the data file *)
	 close_out ul.upfile;
	 (* Write the entry file *)
	 let qid = entry.qentry.A.eqid in
	 let eid = entry.qentry.A.eid in
	 let queuedir = Filename.concat spooldir qid in
	 let qentry_file = Filename.concat queuedir ("e" ^ eid) in
	 with_out_file (open_out_bin qentry_file)
	   (fun file ->
	      let v = A._of_entry entry.qentry in
	      Xdr.pack_xdr_value v t_entry [] (output_string file);
	   );
	 (* Deallocate uhandle: *)
	 Hashtbl.remove uhandles uhandle;
	 entry.current_upload <- None;
	 (* Make the new entry visible: *)
	 let mq = lookup_managed_queue qid in
	 entry.is_uploading <- false;
	 mq.qinfo.A.qlength <- Int32.succ mq.qinfo.A.qlength;
	 mq.qinfo.A.quploads <- Int32.pred mq.qinfo.A.quploads;
	 store_managed_queue qid mq;
	 (* Tell waiters that there is a new entry: *)
	 mq.ongrow <- List.filter
	                (fun (_,f) -> protected_call f false) mq.ongrow;
	 reply `successful
       end
       else begin
	 ul.upserial <- Int64.succ ul.upserial;
	 reply `successful
       end
    )
;;


let onclose conn_id =
  (* This function is called when a TCP connection is closed. We have to
   * iterate over the whole cache of queues and to:
   * - return all picked entries (by this connection)
   * - stop all downloads (by this connection)
   * - stop and remove all uploads (by this connection)
   *)
  Hashtbl.iter
    (fun qid mq ->
       let canceled_uploads = ref false in
       List.iter
	 (fun entry ->
	    if entry.is_picked && entry.pick_connection = Some conn_id then
	      begin
		entry.is_picked <- false;
		stop_downloads mq entry;
	      end;

	    if entry.is_uploading && entry.upload_connection = Some conn_id then
	      begin
		entry.is_uploading <- false;
		stop_upload mq entry;
		canceled_uploads := true;
	      end;
	 )
	 mq.qentries;
       (* If there are cancaled uploads, execute [onshrink]: *)
       if !canceled_uploads then
	 mq.onshrink <- List.filter
	                  (fun (_,f) -> protected_call f false) mq.onshrink;
    )
    queues
;;


let check_timeouts ev =
  (* This function is called every minute. We have to
   * iterate over the whole cache of queues and to:
   * - return all timed out picked entries and stop all downloads
   * - stop and remove all timed out uploads
   *)
  ( match ev with
	Unixqueue.Timeout(_,_) -> ()
      | _ -> raise Equeue.Reject
  );
  let now = Unix.time() in
  Hashtbl.iter
    (fun qid mq ->
       (* Are there timed out events in [onshrink]? *)
       mq.onshrink <-
         List.filter
	   (fun (t,f) ->
	      if t >= 0.0 && t <= now then
		protected_call f true
	      else
		true
	   )
	   mq.onshrink;
       (* Are there timed out events in [ongrow]? *)
       mq.ongrow <-
         List.filter
	   (fun (t,f) ->
	      if t >= 0.0 && t <= now then
		protected_call f true
	      else
		true
	   )
	   mq.ongrow;
    )
    queues
;;

let pluggable_auth_module = 
  ref ("<None>", 
       (`Socket(Tcp, Rpc_server.Portmapped, Rpc_server.default_socket_config)),
       (fun _ -> 
	  failwith "No auth module linked, startup not possible"; () )) ;;


let main() =
  let esys = Unixqueue.create_unix_event_system() in

  let (auth_name, srv_mode, f_srv_config) = !pluggable_auth_module in
  prerr_endline ("Starting queues server with auth module: " ^ auth_name);

  let server = Rpc_server.create2 srv_mode esys in

  S1.bind_async
    ~proc_ping
    ~proc_create_queue
    ~proc_delete_queue
    ~proc_list_queues
    ~proc_get_queue
    ~proc_set_queue
    ~proc_list_queue_entries
    ~proc_pick_queue_entry
    ~proc_return_picked_queue_entry
    ~proc_remove_picked_queue_entry
    ~proc_remove_queue_entry
    ~proc_download_entry
    ~proc_download_chunk
    ~proc_upload_entry
    ~proc_upload_chunk
    server;

  Rpc_server.set_onclose_action server onclose;

  f_srv_config server;

  (* Arrange that [check_timeouts] is called every second: *)
  let w = Unixqueue.new_wait_id esys in
  let g = Unixqueue.new_group esys in
  Unixqueue.add_handler esys g (fun _ _ -> check_timeouts);
  Unixqueue.add_resource esys g (Unixqueue.Wait w, 1.0);

  List.iter
    (fun signal ->
       Netsys_signal.register_handler
	 ~signal
	 ~name:"Qserver"
	 ~callback:(fun _ ->
		      Unixqueue.remove_resource esys g (Unixqueue.Wait w);
		      Rpc_server.stop_server server;
		   )
	 ()
    )
    [ Sys.sighup; Sys.sigint; Sys.sigquit; Sys.sigterm ];

  Sys.set_signal
    Sys.sigpipe
    Sys.Signal_ignore;

(*
  Rpc_client.verbose true;
  Rpc_server.verbose true;
 *)

  let rec auto_restart f arg =
    try f arg
    with err ->
      prerr_endline ("Server: Uncaught exception: " ^ Printexc.to_string err);
      auto_restart f arg
  in

  (* Fork *)
  match Unix.fork() with
      0 ->
        (* Child *)
	Sys.chdir "/";
	ignore(Unix.setsid());
        auto_restart Unixqueue.run esys;
        exit 99
    | n when n > 0 ->
        (* Parent *)
	()
    | _ ->
        assert false

;;
