type el =
    { url : string;
      mutable code : int;
      mutable time_ok : float;
      mutable time_access : float;
      mutable mime_type : string;
      mutable visited : bool;
    }

type t =
    { mutable a : el option array;
      mutable l : int;
      mutable h : (string, int) Hashtbl.t;
    }

let space_re = Str.regexp "[ ]";;

let protect_spaces s =
  (* TODO: repair this *)
  (* Str.global_replace space_re s "+" *)
  let s' = String.copy s in
  for i = 0 to String.length s - 1 do
    if s.[i] = ' ' then s'.[i] <- '+'
  done;
  s'
;;


let create() = 
  { a = Array.make 100 None;
    l = 0;
    h = Hashtbl.create 100;
  }
;;

let save db filename =
  let f = open_out filename in
  try
    for i = 0 to db.l - 1 do
      match db.a.(i) with
	  None -> ()
	| Some x ->
	    output_string f (protect_spaces x.url);
	    output_string f " ";
	    output_string f (string_of_int x.code);
	    output_string f " ";
	    output_string f (string_of_float x.time_ok);
	    output_string f " ";
	    output_string f (string_of_float x.time_access);
	    output_string f " ";
	    output_string f x.mime_type;
	    output_string f "\n";
    done;
    close_out f
  with
      any ->
	close_out f;
	raise any
;;

let add db url =
  (* TODO: normalize the URL with respect to capitalization *)
  try
    ignore(Hashtbl.find db.h url)
  with
      Not_found ->
	if db.l >= Array.length db.a then begin
	  (* Allocate new space *)
	  let a' = Array.make (2 * db.l) None in
	  Array.blit db.a 0 a' 0 db.l;
	  db.a <- a';
	end;
	let x =
	  { url = url;
	    code = 0;
	    time_ok = 0.0;
	    time_access = 0.0;
	    mime_type = "";
	    visited = false;
	  } in
	db.a.(db.l) <- Some x;
	Hashtbl.add db.h url db.l;
	db.l <- db.l + 1
;;


let update db url code time_ok time_access mime_type =
  let n = Hashtbl.find db.h url in
  match  db.a.(n) with
      None -> assert false
    | Some x ->
	x.code <- code;
	x.time_ok <- time_ok;
	x.time_access <- time_access;
	x.mime_type <- mime_type;
	x.visited <- false;
;;


let lookup db url =
  let n = Hashtbl.find db.h url in
  match  db.a.(n) with
      None -> assert false
    | Some x ->
	x.code, x.time_ok, x.time_access, x.mime_type
;;


let restore filename =
  let db = create() in
  let f = open_in filename in
  try
    while true do
      let line = input_line f in
      let fields = Str.bounded_split_delim space_re line 5 in
      match fields with
	  [ url; code; time_ok; time_access; mime_type ] ->
	    add db url;
	    update 
	      db 
	      url 
	      (int_of_string code)
	      (float_of_string time_ok)
	      (float_of_string time_access)
	      mime_type
	| _ ->
	    prerr_endline ("Questionable line: "  ^ line)
    done;
    assert false
  with
      End_of_file ->
	close_in f;
	db
    | any ->
	close_in f;
	raise any
;;

	    
let iter db age interval =
  let rec next_round () =
    (* Iterate over the complete array: *)
    let rec next_element k n t0 =
      if k >= db.l then begin
	if n > 0 then
	  next_round()
	else
	  [< >]
      end
      else
	match db.a.( k ) with
	    None -> assert false
	  | Some x ->
	      let v = x.visited in
	      let doit =
		if (x.code >= 200 && x.code <= 299) || x.code = 304 then begin
		  (* Successful code *)
		  x.time_ok +. age <= t0
		end 
		else begin
		  (* Failure code *)
		  x.code = 0 or x.time_access +. age <= t0
		end
	      in
	      if doit && not v then begin
		x.visited <- true;
		[< '(x.url,x.code,x.time_ok,x.time_access,x.mime_type);
		   next_element (k+1) (n+1) (Unix.gettimeofday()) >]
	      end
	      else
		next_element (k+1) n t0
    in
    next_element 0 0 (Unix.gettimeofday())
  in
  for k = 0 to db.l-1 do
    match db.a.( k ) with
	Some x -> x.visited <- false
      | _ -> ()
  done;
  next_round ()
(*
  let t0 = Unix.gettimeofday() in
  let m = ref 1e30 in
  for k = 0 to db.l - 1 do
    match db.a.(k) with
	None -> assert false
      | Some x ->
	  if (x.code >= 200 && x.code <= 299) || x.code = 304 then begin
	    (* Successful code *)
	    m := min !m (x.time_ok +. age -. t0)
	  end 
	  else begin
	    (* Failure code *)
	    m := min !m (x.time_access +. age -. t0)
	  end
  done;
  !m
*)
;;

