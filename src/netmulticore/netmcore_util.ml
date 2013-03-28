(* $Id: netmcore_util.ml 1567 2011-03-31 16:47:19Z gerd $ *)

module AVL : sig
  (* AVL trees for use in shm data structures. Keys and values are just
     ints (could be e.g. pointers into further arrays).
   *)
  
  type header =
      { mutable fl : int;    (* start of free list or (-1) *)
	mutable root : int;
	mutable nodes : node array;
      }

  and node =
      { mutable left : int;   (* left child or (-1) *)
	mutable right : int;  (* right child or (-1) *)
	mutable bal : int;    (* balance value *)
	mutable key : int;
	mutable value : int;
      }

  exception Tree_full

  val create_header : unit -> header
    (* Creates an uninitialized header without nodes *)

  val create_node : unit -> node
    (* Creates an uninitialized node *)

  val init_header : header -> unit
    (* Puts all nodes into the freelist so that the tree does not contain
       any elements
     *)

  val find_pred : header -> int -> (int * int) option
    (* [find_pred h key]: Searches the element preceding [key], and
       returns it as [Some(k,v)]. Returns [None] if [key] is the 
       first key.
     *)

  val find : header -> int -> (int * int) option
    (* [find h key]: Searches for the element with [key], or the succeeding
       element, and returns it as [Some(k,v)]. Returns [None] if [key]
       is the last key.
     *)

  val add : header -> int -> int -> unit
    (* [add h k v]: Adds or replaces the element with key [k] and value [v]
     *)

  val remove : header -> int -> unit
    (* [remove h k]: Removes the element with key [k] if it exists *)

  val as_debug_list : header -> string
    (* Returns the contents as a string ["{k->v,...}"] *)

  val as_debug_tree : header -> string
    (* Returns the contents as multi-line string depicting the tree *)
end = struct

  type header =
      { mutable fl : int;
	mutable root : int;
	mutable nodes : node array;
      }

  and node =
      { mutable left : int;
	mutable right : int;
	mutable bal : int;
	mutable key : int;
	mutable value : int;
      }

  exception Tree_full


  let as_debug_list hdr =
    let b = Buffer.create 100 in
    let first = ref true in

    let rec descent k =
      if k = (-1) then
	()
      else (
	let node = hdr.nodes.(k) in
	descent node.left;
	if not !first then Buffer.add_char b ',';
	Buffer.add_string b (string_of_int node.key);
	Buffer.add_string b "->";
	Buffer.add_string b (string_of_int node.value);
	first := false;
	descent node.right;
      )
    in
    Buffer.add_string b "{";
    descent hdr.root;
    Buffer.add_string b "}";
    Buffer.contents b

  let as_debug_tree hdr =
    let b = Buffer.create 100 in

    let rec descent k depth =
      if k = (-1) then (
	let indent = String.make (depth * 4) ' ' in
	Buffer.add_string b indent;
	Buffer.add_string b "-\n";
      )
      else (
	let node = hdr.nodes.(k) in
	let indent = String.make (depth * 4) ' ' in
	Buffer.add_string b indent;
	Buffer.add_string b "* ";
	Buffer.add_string b (string_of_int node.key);
	Buffer.add_string b "->";
	Buffer.add_string b (string_of_int node.value);
	Buffer.add_string b ("     (bal=" ^ string_of_int node.bal ^ ")");
	Buffer.add_string b "\n";
	descent node.left (depth+1);
	descent node.right (depth+1);
      )
    in
    descent hdr.root 0;
    Buffer.contents b


  let create_header() =
    { fl = (-1);
      root = (-1);
      nodes = [| |];
    }

  let create_node() =
    { left = (-1);
      right = (-1);
      bal = 0;
      key = 0;
      value = 0;
    }

  let init_header hdr =
    let n = Array.length hdr.nodes in
    for k = 0 to n-2 do
      hdr.nodes.(k).left <- (k+1)
    done;
    hdr.nodes.(n-1).left <- (-1);
    hdr.fl <- 0;
    hdr.root <- (-1)


  let alloc_node hdr =
    let k = hdr.fl in
    if k = (-1) then
      raise Tree_full;
    let node = hdr.nodes.(k) in
    hdr.fl <- node.left;
    k

  let free_node hdr k =
    let node = hdr.nodes.(k) in
    node.left <- hdr.fl;
    hdr.fl <- k


  let find_pred hdr key =
    let rec descent k best_opt =
      if k = (-1) then
	best_opt
      else (
	let node = hdr.nodes.(k) in
	if node.key >= key then (
	  descent node.left best_opt
	)
	else
	  let new_best_opt = Some(node.key, node.value) in
	  descent node.right new_best_opt
      )
    in
    descent hdr.root None


  let find hdr key =
    let rec descent k best_opt =
      if k = (-1) then
	best_opt
      else (
	let node = hdr.nodes.(k) in
	if node.key = key then
	  Some(key, node.value)
	else (
	  if node.key > key then (
	    let new_best_opt = Some(node.key, node.value) in
	    descent node.left new_best_opt
	  )
	  else
	    descent node.right best_opt
	)
      )
    in
    descent hdr.root None


  let add hdr key value =

    let new_node() =
      let p = alloc_node hdr in
      hdr.nodes.(p).key <- key;
      hdr.nodes.(p).value <- value;
      hdr.nodes.(p).bal <- 0;
      hdr.nodes.(p).left <- (-1);
      hdr.nodes.(p).right <- (-1);
      p in

    let rec insert k =
      if k = (-1) then (
	let p = new_node() in
	(p, true)
      )
      else
	let node = hdr.nodes.(k) in
	if node.key = key then (
	  node.value <- value;
	  (k, false)
	)
	else
	  if node.key > key then (
	    let (l, l_got_higher) = insert node.left in
	    node.left <- l;
	    if l_got_higher then (
	      match node.bal with
		| 1 ->
		    node.bal <- 0;
		    (k, false)
		| 0 ->
		    node.bal <- (-1);
		    (k, true)
		| (-1) ->
		    let node_l = hdr.nodes.(l) in
		    if node_l.bal = (-1) then (
		      let lr = node_l.right in
		      node_l.right <- k;
		      node.left <- lr;
		      node.bal <- 0;
		      node_l.bal <- 0;
		      (l, false)
		    )
		    else (
		      let lr = node_l.right in
		      let node_lr = hdr.nodes.(lr) in
		      let lrl = node_lr.left in
		      let lrr = node_lr.right in
		      node_lr.left <- l;
		      node_lr.right <- k;
		      node_l.right <- lrl;
		      node.left <- lrr;
		      node.bal <- if node_lr.bal = (-1) then 1 else 0;
		      node_l.bal <- if node_lr.bal = 1 then (-1) else 0;
		      node_lr.bal <- 0;
		      (lr, false)
		    )
		| _ -> assert false
	    )
	    else (k, false)
	  )
	  else (
	    let (r, r_got_higher) = insert node.right in
	    node.right <- r;
	    if r_got_higher then (
	      match node.bal with
		| (-1) ->
		    node.bal <- 0;
		    (k, false)
		| 0 ->
		    node.bal <- 1;
		    (k, true)
		| 1 ->
		    let node_r = hdr.nodes.(r) in
		    if node_r.bal = 1 then (
		      let rl = node_r.left in
		      node_r.left <- k;
		      node.right <- rl;
		      node.bal <- 0;
		      node_r.bal <- 0;
		      (r, false)
		    )
		    else (
		      let rl = node_r.left in
		      let node_rl = hdr.nodes.(rl) in
		      let rll = node_rl.left in
		      let rlr = node_rl.right in
		      node_rl.right <- r;
		      node_rl.left <- k;
		      node_r.left <- rlr;
		      node.right <- rll;
		      node.bal <- if node_rl.bal = 1 then (-1) else 0;
		      node_r.bal <- if node_rl.bal = (-1) then 1 else 0;
		      node_rl.bal <- 0;
		      (rl, false)
		    )
		| _ -> assert false
	    )
	    else (k, false)
	  )
    in
  
    let (q, _) = insert hdr.root in
    hdr.root <- q


  let remove hdr key =
    let rec delete k =
      if k = (-1) then
	raise Not_found
      else (
	let node = hdr.nodes.(k) in
	if node.key = key then (
	  let (t, t_got_lower) =
	    if node.right = (-1) then (node.left, true)
	    else if node.left = (-1) then (node.right, true)
	    else (
	      let (rm, l, l_got_lower) = extract_rightmost_node node.left in
	      let node_rm = hdr.nodes.(rm) in
	      node_rm.left <- l;
	      node_rm.right <- node.right;
	      node_rm.bal <- node.bal;
	      balance1 rm l_got_lower
	    ) in
	  free_node hdr k;
	  (t, t_got_lower)
	)
	else if node.key > key then (
	  let (l, l_got_lower) = delete node.left in
	  node.left <- l;
	  balance1 k l_got_lower
	)
	else (
	  let (r, r_got_lower) = delete node.right in
	  node.right <- r;
	  balance2 k r_got_lower
	)
      )
	
    and extract_rightmost_node k =
      (* find the rightmost node in the tree, and separate it from the
	 tree. Returns (rm, j, got_lower) where rm is the rightmost node
	 and j the new tree without rm
       *)
      let node = hdr.nodes.(k) in
      if node.right = (-1) then
	(k, node.left, true)
      else (
	let (rm, r, got_lower) = extract_rightmost_node node.right in
	node.right <- r;
	let (j, got_lower') = balance2 k got_lower in
	(rm, j, got_lower')
      )
	
    and balance1 k l_got_lower =
      (* to be called when the left subtree of k may have decreased in height *)
      if l_got_lower then (
	let node = hdr.nodes.(k) in
	match node.bal with
	  | (-1) ->
	      node.bal <- 0;
	      (k, true)
	  | 0 ->
	      node.bal <- 1;
	      (k, false)
	  | 1 ->
	      let r = node.right in
	      let node_r = hdr.nodes.(r) in
	      if node_r.bal >= 0 then (
		let rl = node_r.left in
		node_r.left <- k;
		node.right <- rl;
		let rl_deep = node_r.bal = 0 in
		node_r.bal <- if rl_deep then (-1) else 0;
		node.bal <- if rl_deep then 1 else 0;
		(r, not rl_deep)
	      )
	      else (
		let rl = node_r.left in
		let node_rl = hdr.nodes.(rl) in
		let rll = node_rl.left in
		let rlr = node_rl.right in
		node_rl.left <- k;
		node_rl.right <- r;
		node.right <- rll;
		node_r.left <- rlr;
		let rl_bal = node_rl.bal in
		node_rl.bal <- 0;
		node.bal <- if rl_bal > 0 then (-1) else 0;
		node_r.bal <- if rl_bal < 0 then 1 else 0;
		(rl, true)
	      )
	  | _ -> assert false
      )
      else (k, false)
	
    and balance2 k r_got_lower =
      (* to be called when the right subtree of k may have decreased in height *)
      if r_got_lower then (
	let node = hdr.nodes.(k) in
	match node.bal with
	  | 1 ->
	      node.bal <- 0;
	      (k, true)
	  | 0 ->
	      node.bal <- (-1);
	      (k, false)
	  | (-1) ->
	      let l = node.left in
	      let node_l = hdr.nodes.(l) in
	      if node_l.bal <= 0 then (
		let lr = node_l.right in
		node_l.right <- k;
		node.left <- lr;
		let lr_deep = node_l.bal = 0 in
		node_l.bal <- if lr_deep then 1 else 0;
		node.bal <- if lr_deep then (-1) else 0;
		(l, not lr_deep)
	      )
	      else (
		let lr = node_l.right in
		let node_lr = hdr.nodes.(lr) in
		let lrr = node_lr.right in
		let lrl = node_lr.left in
		node_lr.right <- k;
		node_lr.left <- l;
		node.left <- lrr;
		node_l.right <- lrl;
		let lr_bal = node_lr.bal in
		node_lr.bal <- 0;
		node.bal <- if lr_bal < 0 then 1 else 0;
		node_l.bal <- if lr_bal > 0 then (-1) else 0;
		(lr, true)
	      )
	  | _ -> assert false
      )
      else (k, false)
    in
    
    try
      let (q, _) = delete hdr.root in
      hdr.root <- q
    with
      | Not_found -> ()



end
