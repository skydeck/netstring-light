(* n-queens problem

   This is a parallel solution of the n-queens problem. In order to
   make a bit more interesting, we only search for results that cannot
   be derived from other results by mirroring or rotation.  This means
   we have to filter out results that are in the same equivalence
   class.

   We require that the result set is available at the end in a
   single data structure, and that the number of results is printed.

   At most n workers can be started.

   This program is "five in one":
   - Shared_hashtable:
     a parallel solution using a shared hashtable for storing the results
   - Shared_hashtable_2:
     an improved version with two hashtables
   - Message_passing:
     a parallel solution where the results are passed to a filter (which
     is sequential)
   - Message_passing_2:
     an improved version with two filtering stages
   - Sequential: a sequential solution, just for comparison
 *)

open Printf

type board = int array
  (* row of the queen in the k-th column. Rows and columns are counted from
     0 onwards
   *)

(* Algorithmic core *)

let identity b = b

let x_mirror (b:board) =
  let n = Array.length b in
  Array.init
    n
    (fun k -> b.(n-k-1))

let rot_90 (b:board) =
  let n = Array.length b in
  let b' = Array.make n 0 in
  for k = 0 to n-1 do
    let j = b.(k) in
    b'.(n-1-j) <- k
  done;
  b'

let ( ++ ) f1 f2 (b:board) =
  f2 (f1 b)

let transformations (b:board) =
  let b_90 = rot_90 b in
  let b_180 = rot_90 b_90 in
  let b_270 = rot_90 b_180 in
  let b' = x_mirror b in
  let b'_90 = x_mirror b_270 in
  let b'_180 = x_mirror b_180 in
  let b'_270 = x_mirror b_90 in
  [ b; b_90; b_180; b_270; b'; b'_90; b'_180; b'_270 ]


let print b =
  let buf = Buffer.create 40 in
  for k = 0 to Array.length b - 1 do
    bprintf buf "%d " b.(k)
  done;
  printf "Solution: %s\n%!" (Buffer.contents buf)


let solve first_queen n emit =
  (* The first queen (column 0) is put into row [first_queen].
     For each non-filtered result [emit] is called
   *)
  let b0 = Array.make n (-1) in
  (* The board with the current solution *)

  let b1 = Array.make n true in
  (* Whether a row is free *)

  let d1 = Array.make (2*n-1) true in
  (* Diagonal 1 *)

  let d2 = Array.make (2*n-1) true in
  (* Diagonal 2 *)

  let rec search col =
    if col = n then
      emit b0
    else (
      for row = 0 to n-1 do
	(* Try to put a queen into column [col] at row [row] *)
	let k1 = row - col + n - 1 in
	let k2 = n + n - 2 - row - col in
	let ok = b1.(row) && d1.(k1) && d2.(k2) in
	if ok then (
	  b0.(col) <- row;
	  b1.(row) <- false;
	  d1.(k1) <- false;
	  d2.(k2) <- false;
	  search (col+1);
	  b1.(row) <- true;
	  d1.(k1) <- true;
	  d2.(k2) <- true;
	)
      done
    )
  in

  b0.(0) <- first_queen;
  b1.(first_queen) <- false;
  d1.(first_queen + n - 1) <- false;
  d2.(n + n - 2 - first_queen) <- false;
  
  search 1


(**********************************************************************)

module Sequential = struct
  let run n =
    let t0 = Unix.gettimeofday() in
    let ht = Hashtbl.create 91 in
    for k = 0 to n-1 do
      solve k n
	(fun b ->
	   if not (Hashtbl.mem ht b) then (
	     let b = Array.copy b in
	     List.iter
	       (fun b' ->
		  Hashtbl.add ht b' ()
	       )
	       (transformations b);
	     print b
	   )
	)
    done;
    let t1 = Unix.gettimeofday() in
    printf "Number solutions: %n\n%!" (Hashtbl.length ht / 8);
    printf "Time: %.3f\n%!" (t1-.t0)
end

(**********************************************************************)

module Shared_hashtable = struct
  let pool_size = 800 * 1024 * 1024  (* 800M *)

  type ht_header =
      { mutable lock : Netmcore_mutex.mutex }

  type ht =
      (board, unit, ht_header) Netmcore_hashtbl.t

  type ht_descr =
      (board, unit, ht_header) Netmcore_hashtbl.t_descr

  let worker (pool, ht_descr, first_queen, n) =
    let ht = Netmcore_hashtbl.hashtbl_of_descr pool ht_descr in
    solve first_queen n
      (fun b ->
	 let b = Array.copy b in
	 let b_list = transformations b in
	 let b_min =
	   List.fold_left
	     (fun acc b1 -> min acc b1)
	     (List.hd b_list)
	     (List.tl b_list) in
	 (* Because this is a read-modify-update operation we have to lock
	    the hash table
	  *)
	 let header = Netmcore_hashtbl.header ht in
	 Netmcore_mutex.lock header.lock;
	 try
	   if not (Netmcore_hashtbl.mem ht b_min) then (
	     Netmcore_hashtbl.add ht b_min ();
	     print b_min
	   );
	   Netmcore_mutex.unlock header.lock;
	 with
	   | error ->
	       Netmcore_mutex.unlock header.lock;
	       raise error
      )

  let worker_fork, worker_join =
    Netmcore_process.def_process worker

  let controller (pool, n) =
    let header_orig =
      { lock = Netmcore_mutex.dummy() } in
    let ht = Netmcore_hashtbl.create pool header_orig in
    Netmcore_heap.modify
      (Netmcore_hashtbl.heap ht)
      (fun mut ->
	 let header = Netmcore_hashtbl.header ht in
	 header.lock <- Netmcore_mutex.create mut `Normal
      );
    let ht_descr = Netmcore_hashtbl.descr_of_hashtbl ht in
    let l = Array.to_list (Array.init n (fun k -> k)) in
    let pids =
      List.fold_left
	(fun acc k ->
	   let pid =
	     Netmcore_process.start
	       ~inherit_resources:`All
	       worker_fork
	       (pool, ht_descr, k, n) in
	   pid :: acc
	)
	[]
	l in
    List.iter
      (fun pid ->
	 match Netmcore_process.join worker_join pid with
	   | None ->
	       failwith "Error in worker"
	   | Some _ ->
	       ()
      )
      pids;
    printf "Number solutions: %n\n%!" (Netmcore_hashtbl.length ht)


  let controller_fork, controller_join =
    Netmcore_process.def_process controller


  let run n =
    let t0 = Unix.gettimeofday() in
    let pool = Netmcore_mempool.create_mempool pool_size in
    Netmcore.startup
      ~socket_directory:"run_nqueens"
      ~first_process:(fun () ->
			Netmcore_process.start
			  ~inherit_resources:`All
			  controller_fork
			  (pool,n))
      ();
    Netmcore.release pool;
    let t1 = Unix.gettimeofday() in
    printf "Time: %.3f\n%!" (t1-.t0)

end

(**********************************************************************)

module Shared_hashtable_2 = struct
  (* Use two independent hash tables *)

  let pool_size = 1600 * 1024 * 1024  (* 1600M *)

  type ht_header =
      { mutable lock : Netmcore_mutex.mutex }

  type ht =
      (board, unit, ht_header) Netmcore_hashtbl.t

  type ht_descr =
      (board, unit, ht_header) Netmcore_hashtbl.t_descr

  let worker (pool, ht1_descr, ht2_descr, first_queen, n) =
    let ht1 = Netmcore_hashtbl.hashtbl_of_descr pool ht1_descr in
    let ht2 = Netmcore_hashtbl.hashtbl_of_descr pool ht2_descr in
    solve first_queen n
      (fun b ->
	 (* Because this is a read-modify-update operation we have to lock
	    the hash table
	  *)
	 let b = Array.copy b in
	 let b_list = transformations b in
	 let b_min =
	   List.fold_left
	     (fun acc b1 -> min acc b1)
	     (List.hd b_list)
	     (List.tl b_list) in
	 let ht = if b_min.(0) < n/2 then ht1 else ht2 in
	 let header = Netmcore_hashtbl.header ht in
	 Netmcore_mutex.lock header.lock;
	 try
	   if not (Netmcore_hashtbl.mem ht b_min) then (
	     Netmcore_hashtbl.add ht b_min ()
	   );
	   Netmcore_mutex.unlock header.lock;
	 with
	   | error ->
	       Netmcore_mutex.unlock header.lock;
	       raise error
      )

  let worker_fork, worker_join =
    Netmcore_process.def_process worker

  let controller (pool, n) =
    let create_ht() =
      let header_orig =
	{ lock = Netmcore_mutex.dummy() } in
      let ht = Netmcore_hashtbl.create pool header_orig in
      Netmcore_heap.modify
	(Netmcore_hashtbl.heap ht)
	(fun mut ->
	   let header = Netmcore_hashtbl.header ht in
	   header.lock <- Netmcore_mutex.create mut `Normal
	);
      ht in

    let ht1 = create_ht() in
    let ht1_descr = Netmcore_hashtbl.descr_of_hashtbl ht1 in

    let ht2 = create_ht() in
    let ht2_descr = Netmcore_hashtbl.descr_of_hashtbl ht2 in

    let l = Array.to_list (Array.init n (fun k -> k)) in
    let pids =
      List.fold_left
	(fun acc k ->
	   let pid =
	     Netmcore_process.start
	       ~inherit_resources:`All
	       worker_fork
	       (pool, ht1_descr, ht2_descr, k, n) in
	   pid :: acc
	)
	[]
	l in
    List.iter
      (fun pid ->
	 match Netmcore_process.join worker_join pid with
	   | None ->
	       failwith "Error in worker"
	   | Some _ ->
	       ()
      )
      pids;
    (* Merge both tables (which are disjoint by construction): *)
    Netmcore_hashtbl.iter
      (fun b _ ->
	 Netmcore_hashtbl.add ht1 b ()
      )
      ht2;
    (* Print: *)
    Netmcore_hashtbl.iter
      (fun b _ ->
	 print b
      )
      ht1;
    printf "Number solutions: %n\n%!" (Netmcore_hashtbl.length ht1)


  let controller_fork, controller_join =
    Netmcore_process.def_process controller


  let run n =
    let t0 = Unix.gettimeofday() in
    let pool = Netmcore_mempool.create_mempool pool_size in
    Netmcore.startup
      ~socket_directory:"run_nqueens"
      ~first_process:(fun () ->
			Netmcore_process.start
			  ~inherit_resources:`All
			  controller_fork
			  (pool,n))
      ();
    Netmcore.release pool;
    let t1 = Unix.gettimeofday() in
    printf "Time: %.3f\n%!" (t1-.t0)

end

(**********************************************************************)

module Message_passing = struct

  (* The worker determines a representative of the equivalence class
     of a result, and only sends the representative to the collector,
     where is is checked for duplicates.

     By experimentation I've found out that it is essential to minimize
     the work to be done in the collector, even at the cost of
     additional computations in the workers.

     The representative is simply the "minimum" of [transformations b]
     when [b] is a solution. The "minimum" is the minimum according to
     OCaml's built-in [min] function. It could be any other criterion,
     provided it can be unambigously determined.
   *)

  type message = 
    | Boards of board list
    | End

  type camlbox =
      message ref Netcamlbox.camlbox
  (* The "ref" just exists to ensure that the message is heap-allocated *)

  type camlbox_sender =
      message ref Netcamlbox.camlbox_sender


  let n_max = 1000


  let worker (camlbox_id, first_queen, n) =
    (* Just send _all_ solutions to the Collector *)
    let cbox = 
      (Netmcore_camlbox.lookup_camlbox_sender camlbox_id : camlbox_sender) in
    let current = ref [] in
    let count = ref 0 in

    let send() =
      Netcamlbox.camlbox_send cbox (ref (Boards !current));
      current := [];
      count := 0 in
    
    solve first_queen n
      (fun b ->
	 let b = Array.copy b in
	 let b_list = transformations b in
	 let b_min =
	   List.fold_left
	     (fun acc b1 -> min acc b1)
	     (List.hd b_list)
	     (List.tl b_list) in
	 current := b_min :: !current;
	 incr count;
	 if !count = n_max then send()
      );
    if !count > 0 then send();
    Netcamlbox.camlbox_send cbox (ref End)


  let worker_fork, worker_join =
    Netmcore_process.def_process worker

  let collector n =
    let msg_max_size =
      ((n+1) * 3 * n_max + 500) * Sys.word_size / 8 in

    let ((cbox : camlbox), camlbox_id) =
      Netmcore_camlbox.create_camlbox "nqueens" (4*n) msg_max_size in

    let l = Array.to_list (Array.init n (fun k -> k)) in
    let pids =
      List.fold_left
	(fun acc k ->
	   let pid =
	     Netmcore_process.start
	       ~inherit_resources:`All
	       worker_fork
	       (camlbox_id, k, n) in
	   pid :: acc
	)
	[]
	l in

    let ht = Hashtbl.create 91 in
    let w = ref n in
    while !w > 0 do
      let slots = Netcamlbox.camlbox_wait cbox in
      List.iter
	(fun slot ->
	   ( match !(Netcamlbox.camlbox_get cbox slot) with
	       | Boards b_list ->
		   List.iter
		     (fun b ->
			if not (Hashtbl.mem ht b) then (
			  let b = Array.copy b in
			  Hashtbl.add ht b ();
			  print b
			)
		     )
		     b_list
	       | End ->
		   decr w
	   );
	   Netcamlbox.camlbox_delete cbox slot
	)
	slots
    done;

    List.iter
      (fun pid ->
	 match Netmcore_process.join worker_join pid with
	   | None ->
	       failwith "Error in worker"
	   | Some _ ->
	       ()
      )
      pids;
    printf "Number solutions: %n\n%!" (Hashtbl.length ht)


  let collector_fork, collector_join =
    Netmcore_process.def_process collector

  let run n =
    let t0 = Unix.gettimeofday() in
    Netmcore.startup
      ~socket_directory:"run_nqueens"
      ~first_process:(fun () -> Netmcore_process.start collector_fork n)
      ();
    let t1 = Unix.gettimeofday() in
    printf "Time: %.3f\n%!" (t1-.t0)

end

(**********************************************************************)

module Message_passing_2 = struct
  (* Use two levels of filtering *)

  (* Make the ID's of the camlboxes known *)
  type id_rec =
      { mutable camlbox_id1 : Netmcore.res_id option;
	mutable camlbox_id2 : Netmcore.res_id option;
	mutable lock : Netmcore_mutex.mutex;
	mutable cond : Netmcore_condition.condition;
	mutable wait_set : Netmcore_condition.wait_set;
      }

  type id_rec_sref = id_rec Netmcore_ref.sref
  type id_rec_descr = id_rec Netmcore_ref.sref_descr


  type message = 
    | Boards of board list
    | End

  type camlbox =
      message ref Netcamlbox.camlbox
  (* The "ref" just exists to ensure that the message is heap-allocated *)

  type camlbox_sender =
      message ref Netcamlbox.camlbox_sender

  let n_max = 1000

  let msg_max_size n =
    ((n+1) * 3 * n_max + 500) * Sys.word_size / 8 

  let unopt =
    function
      | None -> assert false
      | Some x -> x

  let worker (pool, id_rec_descr, first_queen, n) =
    (* We send one partition to camlbox_id1, and the other one to 
       camlbox_id2
     *)
    let id_rec_ref = Netmcore_ref.sref_of_descr pool id_rec_descr in
    let id_rec = Netmcore_ref.deref_ro id_rec_ref in
    let we =
      Netmcore_heap.modify
	(Netmcore_ref.heap id_rec_ref)
	(fun mut -> Netmcore_condition.alloc_wait_entry mut id_rec.wait_set) in

    (* Wait until both camlbox ID's are set: *)
    Netmcore_mutex.lock id_rec.lock;
    while id_rec.camlbox_id1 = None || id_rec.camlbox_id2 = None do
      Netmcore_condition.wait we id_rec.cond id_rec.lock
    done;
    let camlbox_id1 = unopt (id_rec.camlbox_id1) in
    let camlbox_id2 = unopt (id_rec.camlbox_id2) in
    Netmcore_mutex.unlock id_rec.lock;

    let cbox1 = 
      (Netmcore_camlbox.lookup_camlbox_sender camlbox_id1 : camlbox_sender) in
    let cbox2 = 
      (Netmcore_camlbox.lookup_camlbox_sender camlbox_id2 : camlbox_sender) in
    let current1 = ref [] in
    let count1 = ref 0 in
    let current2 = ref [] in
    let count2 = ref 0 in

    let send1() =
      Netcamlbox.camlbox_send cbox1 (ref (Boards !current1));
      current1 := [];
      count1 := 0 in
    
    let send2() =
      Netcamlbox.camlbox_send cbox2 (ref (Boards !current2));
      current2 := [];
      count2 := 0 in
    
    solve first_queen n
      (fun b ->
	 let b = Array.copy b in
	 let b_list = transformations b in
	 let b_min =
	   List.fold_left
	     (fun acc b1 -> min acc b1)
	     (List.hd b_list)
	     (List.tl b_list) in
	 if b_min.(0) < n/2 then (
	   current1 := b_min :: !current1;
	   incr count1;
	   if !count1 = n_max then send1()
	 ) else (
	   current2 := b_min :: !current2;
	   incr count2;
	   if !count2 = n_max then send2()
	 )
      );
    if !count1 > 0 then send1();
    if !count2 > 0 then send2();
    Netcamlbox.camlbox_send cbox1 (ref End);
    Netcamlbox.camlbox_send cbox2 (ref End)


  let worker_fork, worker_join =
    Netmcore_process.def_process worker

  let collector (pool, id_rec_descr, outbox_id, n, coll_id) =
    let id_rec_ref = Netmcore_ref.sref_of_descr pool id_rec_descr in
    let id_rec = Netmcore_ref.deref_ro id_rec_ref in

    let ((cbox : camlbox), camlbox_id) =
      Netmcore_camlbox.create_camlbox
	"nqueens" (4*n) (msg_max_size n) in

    Netmcore_mutex.lock id_rec.lock;
    Netmcore_heap.modify
      (Netmcore_ref.heap id_rec_ref)
      (fun mut ->
	 if coll_id = 0 then
	   id_rec.camlbox_id1 <- Netmcore_heap.add mut (Some camlbox_id)
	 else
	   id_rec.camlbox_id2 <- Netmcore_heap.add mut (Some camlbox_id);
      );
    Netmcore_mutex.unlock id_rec.lock;
    Netmcore_condition.broadcast id_rec.cond;

    let outbox = 
      (Netmcore_camlbox.lookup_camlbox_sender outbox_id : camlbox_sender) in

    let current = ref [] in
    let count = ref 0 in

    let send() =
      Netcamlbox.camlbox_send outbox (ref (Boards !current));
      current := [];
      count := 0 in
    
    let ht = Hashtbl.create 91 in
    let w = ref n in
    while !w > 0 do
      let slots = Netcamlbox.camlbox_wait cbox in
      List.iter
	(fun slot ->
	   ( match !(Netcamlbox.camlbox_get cbox slot) with
	       | Boards b_list ->
		   List.iter
		     (fun b ->
			if not (Hashtbl.mem ht b) then (
			  let b = Array.copy b in
			  Hashtbl.add ht b ();
			  current := b :: !current;
			  incr count;
			  if !count = n_max then send()
			)
		     )
		     b_list
	       | End ->
		   decr w
	   );
	   Netcamlbox.camlbox_delete cbox slot
	)
	slots
    done;

    if !count > 0 then send();
    Netcamlbox.camlbox_send outbox (ref End)


  let collector_fork, collector_join =
    Netmcore_process.def_process collector

  let master_collector (pool,n) =
    let ((outbox : camlbox), outbox_id) =
      Netmcore_camlbox.create_camlbox "nqueens" (4*n) (msg_max_size n) in
    
    (* Initialize id_rec: *)
    let id_rec_orig =
      { camlbox_id1 = None;
	camlbox_id2 = None;
	lock = Netmcore_mutex.dummy();
	cond = Netmcore_condition.dummy_condition();
	wait_set = Netmcore_condition.dummy_wait_set();
      } in
    let id_rec_ref =
      Netmcore_ref.sref pool id_rec_orig in
    Netmcore_heap.modify
      (Netmcore_ref.heap id_rec_ref)
      (fun mut ->
	 let id_rec = Netmcore_ref.deref_ro id_rec_ref in
	 id_rec.lock <- Netmcore_mutex.create mut `Normal;
	 id_rec.cond <- Netmcore_condition.create_condition mut;
	 id_rec.wait_set <- Netmcore_condition.create_wait_set mut;
      );
    let id_rec_descr = Netmcore_ref.descr_of_sref id_rec_ref in

    let pid1 =
      Netmcore_process.start
	~inherit_resources:`All
	collector_fork
	(pool, id_rec_descr, outbox_id, n, 0) in

    let pid2 =
      Netmcore_process.start
	~inherit_resources:`All
	collector_fork
	(pool, id_rec_descr, outbox_id, n, 1) in

    let l = Array.to_list (Array.init n (fun k -> k)) in
    let worker_pids =
      List.fold_left
	(fun acc k ->
	   let pid =
	     Netmcore_process.start
	       ~inherit_resources:`All
	       worker_fork
	       (pool, id_rec_descr, k, n) in
	   pid :: acc
	)
	[]
	l in

    let ht = Hashtbl.create 91 in
    let w = ref 2 in
    while !w > 0 do
      let slots = Netcamlbox.camlbox_wait outbox in
      List.iter
	(fun slot ->
	   ( match !(Netcamlbox.camlbox_get outbox slot) with
	       | Boards b_list ->
		   List.iter
		     (fun b ->
			if not (Hashtbl.mem ht b) then (
			  let b = Array.copy b in
			  Hashtbl.add ht b ();
			  print b
			)
		     )
		     b_list
	       | End ->
		   decr w
	   );
	   Netcamlbox.camlbox_delete outbox slot
	)
	slots
    done;

    List.iter
      (fun pid ->
	 match Netmcore_process.join worker_join pid with
	   | None ->
	       failwith "Error in worker"
	   | Some _ ->
	       ()
      )
      worker_pids;
    List.iter
      (fun pid ->
	 match Netmcore_process.join collector_join pid with
	   | None ->
	       failwith "Error in collector"
	   | Some _ ->
	       ()
      )
      [ pid1; pid2 ];
    printf "Number solutions: %n\n%!" (Hashtbl.length ht)

  let master_collector_fork, master_collector_join =
    Netmcore_process.def_process master_collector


  let run n =
    let t0 = Unix.gettimeofday() in
    let pool = Netmcore_mempool.create_mempool (80 * 1024) in
    Netmcore.startup
      ~socket_directory:"run_nqueens"
      ~first_process:(fun () ->
			Netmcore_process.start
			  ~inherit_resources:`All
			  master_collector_fork
			  (pool,n))
      ();
    Netmcore.release pool;
    let t1 = Unix.gettimeofday() in
    printf "Time: %.3f\n%!" (t1-.t0)

end

(**********************************************************************)

let () =
  let n = ref 8 in
  let t = ref `Seq in
  Arg.parse
    [ "-seq", Arg.Unit (fun () -> t := `Seq),
      "   solve the problem sequentially";

      "-sht", Arg.Unit (fun () -> t := `Sht),
      "   solve the problem with a shared hashtable";

      "-sht2", Arg.Unit (fun () -> t := `Sht2),
      "   solve the problem with two shared hashtables";

      "-mp", Arg.Unit (fun () -> t := `Mp),
      "   solve the problem with message passing";

      "-mp2", Arg.Unit (fun () -> t := `Mp2),
      "   solve the problem with message passing and 2-stage filtering";

      "-n", Arg.Set_int n,
      "<n>  Set problem size";

      "-debug-sem-emu", Arg.Unit Netsys_sem.force_emulation,
      "   Emulate anon semaphores via named semaphores"
    ]
    (fun s -> raise(Arg.Bad("Unexpected arg: " ^ s)))
    "usage: nqueens [-n <number>] (-seq | -sht)";

  printf "Problem size: %d\n%!" !n;

  (* Netmcore_heap.Debug.enable := true; *)

  match !t with
    | `Seq ->
	Sequential.run !n
    | `Sht ->
	Shared_hashtable.run !n
    | `Sht2 ->
	Shared_hashtable_2.run !n
    | `Mp ->
	Message_passing.run !n
    | `Mp2 ->
	Message_passing_2.run !n
