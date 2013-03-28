open Html;;
open Scan;;

exception End_of_scan;;


let no_end_tag =  (* empty HTML elements *)
  [ "isindex";
    "base";
    "meta";
    "link";
    "p";
    "hr";
    "input";
    "img";
    "param";
    "basefont";
    "br";
    "area";
  ]
;;


let special_tag =   (* other lexical rules *)
  [ "script";
    "style";
  ]
;;


let rec parse_comment buf =
  let t = scan_comment buf in
  match t with
      Mcomment ->
	parse_comment buf
    | Eof ->
	raise End_of_scan
    | _ ->
	()
;;


let rec parse_doctype buf =
  let t = scan_doctype buf in
  match t with
      Mdoctype ->
	parse_doctype buf
    | Eof ->
	raise End_of_scan
    | _ ->
	()
;;


let parse_document buf =
  let current_name = ref "" in
  let current_atts = ref [] in
  let current_subs = ref [] in
  let stack = Stack.create() in

  let rec parse_atts () =
    let rec next_no_space() =
      match scan_element buf with
	  Space _ -> next_no_space()
	| t -> t
    in
    match next_no_space() with
	Relement -> []
      | Name n ->
	  begin match next_no_space() with
	      Is ->
		begin match next_no_space() with
		    Name v ->
		      (String.lowercase n, String.uppercase v) :: parse_atts()
		  | Literal v ->
		      (String.lowercase n,v) :: parse_atts()
		  | Eof ->
		      raise End_of_scan
		  | Relement ->
		      (* Illegal *)
		      []
		  | _ ->
		      (* Illegal *)
		      parse_atts()
		end
	    | Eof ->
		raise End_of_scan
	    | Relement ->
		(* Illegal *)
		[]
	    | _ ->
		(* Illegal *)
		parse_atts()
	  end
      | Eof ->
	  raise End_of_scan
      | _ ->
	  (* Illegal *)
	  parse_atts()
  in

  let rec parse_special name =
    (* Parse until </name> *)
    match scan_special buf with
	Lelementend n ->
	  if n = name then
	    ""
	  else
	    "</" ^ n ^ parse_special name
      | Eof ->
	  raise End_of_scan
      | Cdata s ->
	  s ^ parse_special name
      | _ ->
	  (* Illegal *)
	  parse_special name
  in

  let rec skip_element() =
    (* Skip until ">" *)
    match scan_element buf with
	Relement ->
	  ()
      | Eof ->
	  raise End_of_scan
      | _ ->
	  skip_element()
  in

  let rec parse_next() =
    let t = scan_document buf in
    match t with
	Lcomment -> 
	  parse_comment buf;
	  parse_next()
      | Ldoctype ->
	  parse_doctype buf;
	  parse_next()
      | Lelement name ->
	  let name = String.lowercase name in
	  if List.mem name no_end_tag then begin
	    let atts = parse_atts() in
	    current_subs := (Element(name, atts, [])) :: !current_subs;
	    parse_next()
	  end
	  else if List.mem name special_tag then begin
	    let atts = parse_atts() in
	    let data = parse_special name in
	    (* Read until ">" *)
	    skip_element();
	    current_subs := (Element(name, atts, [Data data])) :: !current_subs;
	    parse_next()
	  end
	  else begin
	    let atts = parse_atts() in
	    Stack.push (!current_name, !current_atts, !current_subs) stack;
	    current_name := name;
	    current_atts := atts;
	    current_subs := [];
	    parse_next()
	  end
      | Cdata data ->
	  current_subs := (Data data) :: !current_subs;
	  parse_next()
      | Lelementend name ->
	  let name = String.lowercase name in
	  (* Read until ">" *)
	  skip_element();
	  (* Search the element to close on the stack: *)
	  let found = ref (name = !current_name) in
	  Stack.iter
	    (fun (old_name, _, _) ->
	       if name = old_name then found := true)
	    stack;
	  (* If not found, the end tag is wrong. Simply ignore it. *)
	  if not !found then
	    parse_next()
	  else begin
	    (* Put the current element on to the stack: *)
	    Stack.push (!current_name, !current_atts, !current_subs) stack;
	    (* If found: Remove the elements from the stack, and append
	     * them to the previous element as sub elements
	     *)
	    let rec remove() =
	      let old_name, old_atts, old_subs = Stack.pop stack in
	        (* or raise Stack.Empty *)
	      if old_name = name then
		old_name, old_atts, old_subs
	      else
		let older_name, older_atts, older_subs = remove() in
		older_name, 
		older_atts,
		(Element (old_name, old_atts, List.rev old_subs) :: older_subs)
	    in
	    let old_name, old_atts, old_subs = remove() in
	    (* Remove one more element: the element containing the element
	     * currently being closed.
	     *)
	    let new_name, new_atts, new_subs = Stack.pop stack in
	    current_name := new_name;
	    current_atts := new_atts;
	    current_subs := (Element (old_name, old_atts, List.rev old_subs)) 
                            :: new_subs;
	    (* Go on *)
	    parse_next()
	  end
      | Eof ->
	  raise End_of_scan
      | _ ->
	  parse_next()
  in
  try
    parse_next();
    List.rev !current_subs
  with
      End_of_scan ->
	(* Close all remaining elements: *)
	Stack.push (!current_name, !current_atts, !current_subs) stack;
	let rec remove() =
	  let old_name, old_atts, old_subs = Stack.pop stack in
	        (* or raise Stack.Empty *)
	  try
	    let older_name, older_atts, older_subs = remove() in
	    older_name, 
	    older_atts,
	    (Element (old_name, old_atts, List.rev old_subs) :: older_subs)
	  with
	      Stack.Empty ->
		old_name, old_atts, old_subs
	in
	let name, atts, subs = remove() in
	List.rev subs
;;


let parse_string s =
  let buf = Lexing.from_string s in
  parse_document buf
;;


let parse_file fname =
  let f = open_in fname in
  try
    let buf = Lexing.from_channel f in
    let doc = parse_document buf in
    close_in f;
    doc
  with
      any ->
	close_in f;
	raise any
;;
