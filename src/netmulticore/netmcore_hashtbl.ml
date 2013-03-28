(* $Id: netmcore_hashtbl.ml 1574 2011-04-10 15:13:54Z gerd $ *)

(* Parts of the implementation are taken over from hashtbl.ml of the
   O'Caml distribution
 *)

module H = Netmcore_heap

type ('a, 'b, 'h) tbl =
  { mutable size: int;                        (* number of elements *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    header : 'h;
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of ('a, 'b) bucketcell

and ('a, 'b) bucketcell =
    { mutable key : 'a;
      mutable value : 'b;
      mutable tail : ('a, 'b) bucketlist
    }

type ('a, 'b, 'h) t = ('a, 'b, 'h) tbl H.heap

type ('a, 'b, 'h) t_descr = ('a, 'b, 'h) tbl H.descr
  
let descr_of_hashtbl = H.descr_of_heap
let hashtbl_of_descr = H.heap_of_descr

let create pool h =
  let tbl =
    { size = 0;
      data = Array.make 391 Empty;
      header = h
    } in
  H.create_heap
    pool
    (H.minimum_size tbl)
    tbl

let clear t =
  H.modify t
    (fun mut ->
       let tbl = H.root t in
       tbl.size <- 0;
       Array.fill tbl.data 0 (Array.length tbl.data) Empty
    )

let length t =
  (H.root t).size

let resize mut tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then (
    let ndata = H.add mut (Array.create nsize Empty) in
    H.pin mut ndata;
    let rec insert_bucket bucket = 
      match bucket with
	| Empty -> ()
	| Cons cell ->
            insert_bucket cell.tail; (* preserve original order of elements *)
            let nidx = (Hashtbl.hash cell.key) mod nsize in
	    let prev = ndata.(nidx) in
	    ndata.(nidx) <- bucket;
	    cell.tail <- prev;
    in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  )

let add_1 tbl mut key value =
  let i = (Hashtbl.hash key) mod (Array.length tbl.data) in
  let cell_orig = { key = key; value = value; tail = Empty } in
  let elem = H.add mut (Cons cell_orig) in
  let cell = match elem with Cons c -> c | _ -> assert false in
  cell.tail <- tbl.data.(i);
  tbl.data.(i) <- elem;
  tbl.size <- succ tbl.size;
  if tbl.size > Array.length tbl.data lsl 1 then resize mut tbl
    
let add t key value =
  H.modify t
    (fun mut ->
       let tbl = H.root t in
       add_1 tbl mut key value
    )

let remove t key =
  H.modify t
    (fun mut ->
       let tbl = H.root t in
       let i = (Hashtbl.hash key) mod (Array.length tbl.data) in
       let prev = ref None in
       let cur = ref tbl.data.(i) in
       while !cur <> Empty do
	 match !cur with
	   | Cons cell ->
               if compare cell.key key = 0 then (
		 ( match !prev with
		     | None ->
			 tbl.data.(i) <- cell.tail
		     | Some pcell ->
			 pcell.tail <- cell.tail
		 );
		 tbl.size <- pred tbl.size;
		 cur := Empty
	       )
	       else (
		 prev := Some cell;
		 cur := cell.tail;
	       )
	   | Empty -> assert false
       done;
    )

let rec find_rec key = function
  | Empty ->
      raise Not_found
  | Cons cell ->
      if compare cell.key key = 0 then cell.value else find_rec key cell.tail

let find_quickly tbl key =
  match tbl.data.((Hashtbl.hash key) mod (Array.length tbl.data)) with
      Empty -> raise Not_found
    | Cons cell1 ->
	if compare key cell1.key = 0 then cell1.value else
	  match cell1.tail with
              Empty -> raise Not_found
	    | Cons cell2 ->
		if compare key cell2.key = 0 then cell2.value else
		  match cell2.tail with
		      Empty -> raise Not_found
		    | Cons cell3 ->
			if compare key cell3.key = 0 then cell3.value else
			  find_rec key cell3.tail


let find_ro t key =
  (* unprotected version! *)
  find_quickly (H.root t) key

let find_p t key f =
  H.with_value
    t
    (fun () ->
       find_quickly (H.root t) key
    )
    f

let find_c t key =
  find_p t key H.copy

let rec find_in_bucket key = function
  | Empty ->
      []
  | Cons cell ->
      if compare cell.key key = 0
      then cell.value :: find_in_bucket key cell.tail
      else find_in_bucket key cell.tail

let find_all_ro t key =
  let tbl = H.root t in
  find_in_bucket 
    key 
    tbl.data.((Hashtbl.hash key) mod (Array.length tbl.data))

let find_all_p t key f =
  let tbl = H.root t in
  H.with_value_n
    t
    (fun () ->
       find_in_bucket 
	 key 
	 tbl.data.((Hashtbl.hash key) mod (Array.length tbl.data))
    )
    f

let find_all_c t key =
  find_all_p t key H.copy

let replace t key value =
  H.modify t
    (fun mut ->
       let tbl = H.root t in

       let rec replace_bucket = function
	 | Empty -> raise Not_found
	 | Cons cell ->
	     if compare cell.key key = 0 then
	       cell.value <- H.add mut value
	     else
	       replace_bucket cell.tail in

       let i = (Hashtbl.hash key) mod (Array.length tbl.data) in
       try
	 replace_bucket tbl.data.(i)
       with
	 | Not_found ->
	     add_1 tbl mut key value
    )

let mem_ro t key =
  try ignore(find_ro t key); true with Not_found -> false

let mem t key =
  H.with_value t
    (fun () ->
       mem_ro t key
    )
    (fun r -> r)

let iter f t =
  H.with_value t
    (fun () ->
       let rec do_bucket = function
	 | Empty -> ()
	 | Cons cell ->
             f cell.key cell.value; do_bucket cell.tail in
       let tbl = H.root t in
       let d = tbl.data in
       for i = 0 to Array.length d - 1 do
	 do_bucket d.(i)
       done
    )
    (fun () -> ())

let header t =
  (H.root t).header

let heap t =
  Obj.magic t


