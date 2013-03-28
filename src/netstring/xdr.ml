(*
 * $Id: xdr.ml 1702 2012-02-14 21:01:59Z gerd $
 *)

(* This is an XDR implementation.
 * See RFC 1014
 *)

open Netnumber
open Printf

exception Propagate of string;;

(**********************************************************************)

(* auxiliary stuff: *)

let aux_cmp (ha,sa) (hb,sb) =
  if ha = hb then
    compare sa sb
  else
    ha - hb
;;


let all_distinct_q l =
  (* returns whether all elements of l are distinct *)
  let a =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l) in
  Array.sort aux_cmp a;
  let distinct = ref true in
  let k = ref 0 in
  while !distinct && !k < Array.length a - 1 do
    let (ha,sa) = a.( !k ) in
    let (hb,sb) = a.( !k + 1 ) in
    distinct := (ha != hb) && (sa <> sb);
    incr k
  done;
  !distinct
;;


let all_distinct =
  function
      []
    | [_] -> true
    | [a;b] -> a <> b
    | l -> all_distinct_q l
;;


let sub_set_q l1 l2 =
  (* returns whether all elements of l1 occur in l2 *)
  let a1 =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l1) in
  let a2 =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l2) in
  Array.sort aux_cmp a1;
  Array.sort aux_cmp a2;
  let occurs = ref true in
  let k1 = ref 0 in
  let k2 = ref 0 in
  while !occurs && !k1 < Array.length a1 && !k2 < Array.length a2 do
    let (h1,s1) = a1.( !k1 ) in
    let found = ref false in
    while not !found && !k2 < Array.length a2 do
      let (h2,s2) = a2.( !k2 ) in
      found := (h1 == h2) && (s1 = s2);
      if not !found then incr k2
    done;
    occurs := !found;
    incr k1
  done;
  !occurs
;;


let sub_set l1 l2 =
  match (l1,l2) with
      ([],_) -> true
    | ([x],_) -> List.mem x l2
    | _ -> sub_set_q l1 l2
;;


(* (* currently unused! *)
let equal_sets_q l1 l2 =
  (* returns whether all elements of l1 occur in l2, and vice versa *)
  let a1 =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l1) in
  let a2 =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l2) in
  Array.sort aux_cmp a1;
  Array.sort aux_cmp a2;
  let equal = ref true in
  let k1 = ref 0 in
  let k2 = ref 0 in
  let k2_match = ref false in
      (* can only be false when !k2 = 0 *)
  while !equal && !k1 < Array.length a1 && !k2 < Array.length a2 do
    let (h1,s1) = a1.( !k1 ) in
    let (h2,s2) = a2.( !k2 ) in
    if (h1 == h2) && (s1 = s2) then (
      incr k1;
      k2_match := true;   (* a match for the k2 element has been found *)
    ) else (
      if !k2_match then (
	incr k2;
	while !k2 < Array.length a2 && (h2,s2) = a2.( !k2 ) do
	  incr k2
	done;
	if !k2 < Array.length a2 then (
	  let (h2',s2') = a2.( !k2 ) in
	  if (h1 == h2') && (s1 = s2') then (
	    incr k1;
	  )
	  else equal := false
	)
	else equal := false
      )
      else equal := false
    )
  done;
  if !equal && !k1 = Array.length a1 && !k2 < Array.length a2 && !k1 > 0 then (
    (* !k1 > 0 ==> a1 is not empty && !k2_match,
     * !k2 < length a2 ==> a2 is not empty
     *)
    let (h2,s2) = a2.( !k2 ) in
    incr k2;
    while !k2 < Array.length a2 && (h2,s2) = a2.( !k2 ) do
      incr k2
    done;
  );
  !equal && !k1 = Array.length a1 && !k2 = Array.length a2
;;


let equal_sets l1 l2 =
  match (l1,l2) with
      ([],[])    -> true
    | ([x],[y])  -> x = y
    | _ -> equal_sets_q l1 l2
;;
*)


(**********************************************************************)
(* definition of XDR types and type systems                           *)
(**********************************************************************)

(* restriction: it is not allowed to have an X_param as enumerator type
 * in union_over_enum. There must always be a real X_enum or, in a
 * type system, a resolvable X_type at this position.
 *)


type xdr_type_term =
    X_int
  | X_uint
  | X_hyper
  | X_uhyper
  | X_enum of (string * int4) list
  | X_float
  | X_double
  | X_opaque_fixed of uint4
  | X_opaque of uint4
  | X_string of uint4
  | X_mstring of string * uint4
  | X_array_fixed of xdr_type_term * uint4
  | X_array of       xdr_type_term * uint4
  | X_struct of (string * xdr_type_term) list
  | X_union_over_int of
      (int4   * xdr_type_term) list * xdr_type_term option
  | X_union_over_uint of
      (uint4  * xdr_type_term) list * xdr_type_term option
  | X_union_over_enum of
      xdr_type_term * (string * xdr_type_term) list * xdr_type_term option
  | X_void
  | X_type of string
  | X_param of string
  | X_rec of (string * xdr_type_term)      (* define a recursive type *)
  | X_refer of string                      (* refer to a recursive type *)
  | X_direct of xdr_type_term * 
                (string -> int ref -> int -> exn) *
                (exn -> string -> int ref -> unit) *
                (exn -> int)
;;


module StringSet = Set.Make(String)
;;


type xdr_type0 =
  { mutable term   : xdr_term;
    mutable params : StringSet.t;
      (* "params" is normally only non-empty in the top node *)
    mutable min_size : int
      (* min_size: the minimum number of bytes every element of the array
	 will take in XDR form. This does not include any inner parameters.
       *)
  }
and xdr_term =
    T_int
  | T_uint
  | T_hyper
  | T_uhyper
  | T_enum of (string * int32) array
      (* array must be sorted by ascending int32 *)
  | T_float
  | T_double
  | T_opaque_fixed of uint4
  | T_opaque of uint4
  | T_string of uint4
  | T_mstring of string * uint4
  | T_array_fixed of xdr_type0 * uint4
  | T_array of       xdr_type0 * (* max size: *) uint4
  | T_struct of (string * xdr_type0) array
  | T_union_over_int of
      (int4, xdr_type0) Hashtbl.t * xdr_type0 option
  | T_union_over_uint of
      (uint4, xdr_type0) Hashtbl.t * xdr_type0 option
  | T_union_over_enum of
      xdr_type0 * xdr_type0 option array * xdr_type0 option
      (* The array corresponds to the T_enum array. None means that the
       * constant is not mapped.
       *)
  | T_void
  | T_param of string
  | T_rec of (string * xdr_type0)
  | T_refer of (string * xdr_type0)
  | T_direct of xdr_type0 * 
                (string -> int ref -> int -> exn) *
                (exn -> string -> int ref -> unit) *
                (exn -> int)
;;

type xdr_type =
    xdr_type0 * xdr_type0
      (* left: includes T_rec and T_refer,
         right: does not include T_rec, T_refer
       *)


type xdr_type_term_system =
  (string * xdr_type_term) list
;;

type xdr_type_system =
  (string * xdr_type) list
  (* export xdr_type_system in an opaque manner *)


let t_name = 
  function
    | T_int -> "T_int"
    | T_uint -> "T_uint"
    | T_hyper -> "T_hyper"
    | T_uhyper -> "T_uhyper"
    | T_enum _ -> "T_enum"
    | T_float -> "T_float"
    | T_double -> "T_double"
    | T_opaque_fixed _ -> "T_opaque_fixed"
    | T_opaque _ -> "T_opaque"
    | T_string _ -> "T_string"
    | T_mstring(_,_) -> "T_mstring"
    | T_array_fixed (_,_) -> "T_array_fixed"
    | T_array (_,_) -> "T_array"
    | T_struct _ -> "T_struct"
    | T_union_over_int(_,_) -> "T_union_over_int"
    | T_union_over_uint(_,_) -> "T_union_over_uint"
    | T_union_over_enum(_,_,_) -> "T_union_over_enum"
    | T_void -> "T_void"
    | T_param _ -> "T_param"
    | T_rec _ -> "T_rec"
    | T_refer _ -> "T_refer"
    | T_direct _ -> "T_direct"
	

let x_bool =
  X_enum ["FALSE", int4_of_int 0; "TRUE", int4_of_int 1]
;;


let x_optional t =
  X_union_over_enum
    (x_bool,
     ["TRUE", t; "FALSE", X_void],
     None)
;;


let x_opaque_max =
  X_opaque (mk_uint4 ('\255', '\255', '\255', '\255'));;

let x_string_max =
  X_string (mk_uint4 ('\255', '\255', '\255', '\255'));;

let x_mstring_max name =
  X_mstring (name, mk_uint4 ('\255', '\255', '\255', '\255'));;

let x_array_max t =
  X_array (t,  (mk_uint4 ('\255', '\255', '\255', '\255')));;

(**********************************************************************)
(* definition of XDR values                                           *)
(**********************************************************************)

type xdr_value_version =
    [ `V1 | `V2 | `V3 | `V4 | `Ocamlrpcgen ]

type xdr_value =
    XV_int of int4
  | XV_uint of uint4
  | XV_hyper of int8
  | XV_uhyper of uint8
  | XV_enum of string
  | XV_float of fp4
  | XV_double of fp8
  | XV_opaque of string
  | XV_string of string
  | XV_array of xdr_value array
  | XV_struct of (string * xdr_value) list
  | XV_union_over_int of (int4 * xdr_value)
  | XV_union_over_uint of (uint4 * xdr_value)
  | XV_union_over_enum of (string * xdr_value)
  | XV_void
  | XV_enum_fast of int
  | XV_struct_fast of xdr_value array
  | XV_union_over_enum_fast of (int * xdr_value)
  | XV_array_of_string_fast of string array
  | XV_mstring of Xdr_mstring.mstring
  | XV_direct of exn * int
;;

let xv_true = XV_enum_fast 1 (* "TRUE" *);;
let xv_false = XV_enum_fast 0 (*  "FALSE" *);;

let xv_none = XV_union_over_enum_fast (0,XV_void);;
let xv_some v = XV_union_over_enum_fast (1,v);;

exception Dest_failure

let dest_xv_int v =
  match v with XV_int x -> x | _ -> raise Dest_failure;;
let dest_xv_uint v =
  match v with XV_uint x -> x | _ -> raise Dest_failure;;
let dest_xv_hyper v =
  match v with XV_hyper x -> x | _ -> raise Dest_failure;;
let dest_xv_uhyper v =
  match v with XV_uhyper x -> x | _ -> raise Dest_failure;;
let dest_xv_enum v =
  match v with XV_enum x -> x | _ -> raise Dest_failure;;
let dest_xv_enum_fast v =
  match v with XV_enum_fast x -> x | _ -> raise Dest_failure;;
let dest_xv_float v =
  match v with XV_float x -> x | _ -> raise Dest_failure;;
let dest_xv_double v =
  match v with XV_double x -> x | _ -> raise Dest_failure;;
let dest_xv_opaque v =
  match v with XV_opaque x -> x | _ -> raise Dest_failure;;
let dest_xv_string v =
  match v with XV_string x -> x | _ -> raise Dest_failure;;
let dest_xv_mstring v =
  match v with XV_mstring x -> x | _ -> raise Dest_failure;;
let dest_xv_array v =
  match v with XV_array x -> x | _ -> raise Dest_failure;;
let dest_xv_array_of_string_fast v =
  match v with XV_array_of_string_fast x -> x | _ -> raise Dest_failure;;
let dest_xv_struct v =
  match v with XV_struct x -> x | _ -> raise Dest_failure;;
let dest_xv_struct_fast v =
  match v with XV_struct_fast x -> x | _ -> raise Dest_failure;;
let dest_xv_void v =
  match v with XV_void -> () | _ -> raise Dest_failure;;

let dest_xv_union_over_int v =
  match v with XV_union_over_int x -> x | _ -> raise Dest_failure;;

let dest_xv_union_over_uint v =
  match v with XV_union_over_uint x -> x | _ -> raise Dest_failure;;

let dest_xv_union_over_enum v =
  match v with XV_union_over_enum x -> x | _ -> raise Dest_failure;;

let dest_xv_union_over_enum_fast v =
  match v with XV_union_over_enum_fast x -> x | _ -> raise Dest_failure;;


let fail_map_xv_enum_fast k =
  failwith ("Xdr.map_xv_enum_fast [" ^ string_of_int k ^ "]") ;;

let map_xv_enum_fast0 t v =
  match t.term with
      T_enum l ->
	let m = Array.length l in
	( match v with
	      XV_enum_fast k ->
		if k >= 0 && k < m then
		  snd(Array.unsafe_get l k)
		else
		  fail_map_xv_enum_fast 1
	    | XV_enum name ->
		let k = ref 0 in
		while !k < m && (fst l.( !k ) <> name) do
		  incr k
		done;
		if !k >= m then
		  fail_map_xv_enum_fast 2;
		snd(l.( !k ))
	    | _ ->
		fail_map_xv_enum_fast 3
	)
    | _ ->
	fail_map_xv_enum_fast 4

let map_xv_enum_fast (_,t) v =
  map_xv_enum_fast0 t v



let fail_map_xv_struct_fast k =
  failwith ("Xdr.map_xv_struct_fast [" ^ string_of_int k ^ "]") ;;

let map_xv_struct_fast0 t v =
  match t.term with
      T_struct decl ->
	let m = Array.length decl in
	( match v with
	      XV_struct_fast x ->
		let k = Array.length x in
		if k = m then
		  x
		else
		  fail_map_xv_struct_fast 1
	    | XV_struct l ->
		( try
		    Array.map
		      (fun (name,y) -> List.assoc name l)
		      decl
		  with
		      Not_found -> fail_map_xv_struct_fast 2
		)
	    | _ ->
		fail_map_xv_struct_fast 3
	)
    | _ ->
	fail_map_xv_struct_fast 4

let map_xv_struct_fast (_,t) v =
  map_xv_struct_fast0 t v

let fail_map_xv_union_over_enum_fast k =
  failwith ("Xdr.map_xv_union_over_enum_fast [" ^ string_of_int k ^ "]") ;;

let map_xv_union_over_enum_fast0 t v =
  match t.term with
      T_union_over_enum( { term = T_enum e }, u, u_dfl ) ->
	let m = Array.length e in
	assert( m = Array.length u );
	( match v with
	      XV_union_over_enum_fast(k, x) ->
		if k >= 0 && k < m then
		  (k, (snd e.(k)), x)
		else
		  fail_map_xv_union_over_enum_fast 1
	    | XV_union_over_enum(name, x) ->
		let k = ref 0 in
		while !k < m && fst(e.( !k )) <> name do
		  incr k
		done;
		if !k >= m then
		  fail_map_xv_union_over_enum_fast 2;
		(!k, (snd e.(!k)), x)
	    | _ ->
		fail_map_xv_union_over_enum_fast 3;
	)
    | _ ->
	fail_map_xv_union_over_enum_fast 4

let map_xv_union_over_enum_fast (_,t) v =
  map_xv_union_over_enum_fast0 t v



exception Xdr_format of string;;
exception Xdr_format_message_too_long of xdr_value;;
(* raised in unpack_xdr_value if the byte stream does not match
 * the expected type. The string is an explanation and might be
 * useful while debugging. In the special case Xdr_format_message_too_long
 * there are more bytes than expected, but a prefix matches the type.
 * The prefix is returned as xdr_value.
 *)

let () =
  Netexn.register_printer
    (Xdr_format "")
    (function
       | Xdr_format s ->
	   sprintf "Xdr.Xdr_format(%S)" s
       | _ ->
	   assert false
    )

exception Xdr_failure of string


let safe_add x y = (* exported *)
  (* pre: x >= 0 && y >= 0 *)
  let s = x + y in
  if s < 0 then (* can only happen on 32 bit platforms *)
    raise(Xdr_failure "int overflow while computing size");
  s

let safe_mul x y = (* exported *)
  (* pre: x >= 0 && y >= 0 *)
  if x=0 || y=0 then
    0
  else
    let n = max_int / y in
    if x > n then
      raise(Xdr_failure "int overflow while computing size");
    x * y


(**********************************************************************)
(* check if XDR types are well-formed                                 *)
(**********************************************************************)

(* TODO: check on recursions without finite fix point. *)


let rec validate_xdr_type_i1
        (r:xdr_type_term -> xdr_type0)
        (b:(string * xdr_type0) list)
        (t:xdr_type_term)
      : xdr_type0 =

  (* r: function that resolves X_type references
   * t: the xdr_type_term to validate
   * b: list of recursive bindings
   *
   * raise Not_found on any error
   *)

  let mktype tm = { term = tm; params = StringSet.empty; min_size = (-1) } in
  (* min_size is calculated in a second pass *)

  match t with
    X_int    -> mktype T_int
  | X_uint   -> mktype T_uint
  | X_hyper  -> mktype T_hyper
  | X_uhyper -> mktype T_uhyper
  | X_float  -> mktype T_float
  | X_double -> mktype T_double
  | X_void   -> mktype T_void
  | X_enum e ->
      let e_names, e_values = List.split e in
      if all_distinct e_names && all_distinct e_values then
	let ea =
	  Array.map
	    (fun (n,i) -> (n, Netnumber.int32_of_int4 i))
	    (Array.of_list e) in
	Array.sort (fun (_,i) (_,i') -> compare i i') ea;
	mktype (T_enum ea)
      else
	raise (Propagate "Bad enumeration type: double values")
  | X_opaque_fixed n    -> mktype (T_opaque_fixed n)
  | X_opaque n          -> mktype (T_opaque n)
  | X_string n          -> mktype (T_string n)
  | X_mstring (name,n)  -> mktype (T_mstring (name,n))
  | X_array_fixed (s,n) -> 
      let nL = int64_of_uint4 n in
      if nL > 0x3fff_ffff_ffffL then
	raise (Propagate "Bad fixed array: bound too high");
      mktype (T_array_fixed(validate_xdr_type_i1 r b s, n))
  | X_array (s,n)       -> mktype (T_array (validate_xdr_type_i1 r b s, n))
  | X_struct s ->
      let s_names, s_types = List.split s in
      if all_distinct s_names then
	mktype
	  (T_struct
	     (Array.of_list
		(List.map (fun (n,x) -> n,validate_xdr_type_i1 r b x) s)))
      else
	raise (Propagate "Bad struct type: components with same names found")
  | X_union_over_int (u, default) ->
      let u_values, u_types = List.split u in
      if all_distinct u_values then begin
	let default' =
	  match default with
	    Some d -> Some (validate_xdr_type_i1 r b d)
	  | None   -> None
	in
	let htbl = Hashtbl.create(List.length u) in
	List.iter
	  (fun (n,x) ->
	     let x' = validate_xdr_type_i1 r b x in
	     Hashtbl.add htbl n x')
	  u;
	mktype(T_union_over_int(htbl, default'))
      end
      else
	raise (Propagate "Bad union_over_int type: variants found with same tags")
  | X_union_over_uint (u,default) ->
      let u_values, u_types = List.split u in
      if all_distinct u_values then begin
	let default' =
	  match default with
	    Some d -> Some (validate_xdr_type_i1 r b d)
	  | None   -> None
	in
	let htbl = Hashtbl.create(List.length u) in
	List.iter
	  (fun (n,x) ->
	     let x' = validate_xdr_type_i1 r b x in
	     Hashtbl.add htbl n x')
	  u;
	mktype(T_union_over_uint(htbl, default'))
      end
      else
	raise (Propagate "Bad union_over_uint type: variants found with same tags")
  | X_union_over_enum (e,u,default) ->
      let e' = validate_xdr_type_i1 r b e in
      let u_values, u_types = List.split u in
      let el =
	match e'.term with
	  T_enum x -> x
	| _ -> raise (Propagate "Bad union_over_enum type: discriminator is not enumerator")
      in
      let el_names, el_values = List.split (Array.to_list el) in
      if all_distinct u_values && sub_set u_values el_names then begin
	   let default' =
	     match default with
	       Some d -> Some (validate_xdr_type_i1 r b d)
	     | None   -> None
	   in
	   mktype
	     (T_union_over_enum
		(e',
		 Array.map
		   (fun (name, _) ->
		      try Some(validate_xdr_type_i1 r b (List.assoc name u))
		      with Not_found -> default'
		   )
		   el,
		 default'))
	 end
      else
	raise (Propagate "Bad union_over_enum type: variants found with identical tags")
  | X_type _ ->
      r t
  | X_param p ->
      mktype (T_param p)
  | X_rec (name, s) ->
      let node = mktype T_void in
      let t' = validate_xdr_type_i1 r ((name,node)::b) s in
      node.term <- T_rec (name, t');
      node
  | X_refer name ->
      mktype (T_refer (name, List.assoc name b))
  | X_direct(s, read, write, size) ->
      mktype (T_direct (validate_xdr_type_i1 r b s, read, write, size))
;;


let rec find_params (t:xdr_type0) : StringSet.t =
  (* collect all parameters *)
  match t.term with
    T_param p ->
      StringSet.singleton p
  | T_array_fixed (t',n) ->
      find_params t'
  | T_array (t',n) ->
      find_params t'
  | T_struct s ->
      Array.fold_left
        (fun set (s,t') -> StringSet.union (find_params t') set)
        StringSet.empty
        s
  | T_union_over_int (htbl,def_opt) ->
      Hashtbl.fold
        (fun n t' set -> StringSet.union (find_params t') set)
        htbl
        (match def_opt with
	     None -> StringSet.empty
	   | Some def -> find_params def)
  | T_union_over_uint (htbl,def_opt) ->
      Hashtbl.fold
        (fun n t' set -> StringSet.union (find_params t') set)
        htbl
        (match def_opt with
	     None -> StringSet.empty
	   | Some def -> find_params def)
  | T_union_over_enum (e,u,def_opt) ->
      Array.fold_left (fun set t' ->
			 match t' with
			     Some t'' -> StringSet.union (find_params t'') set
			   | None     -> set)
                      (match def_opt with
			   None -> StringSet.empty
			 | Some def -> find_params def)
                      u
  | T_rec (_,t') ->
      find_params t'
  | T_direct(t',_,_,_) ->
      find_params t'
  | _ ->
      StringSet.empty
;;


(* Elimination of rec/refer  *)

let map_opt f o =
  match o with
    | None -> None
    | Some x -> Some(f x)

let map_hashtbl f t =
  let acc = Hashtbl.create (Hashtbl.length t) in
  Hashtbl.iter
    (fun k v ->
       let v' = f k v in
       Hashtbl.add acc k v';  (* !!! reverses order of bindings !!! *)
    )
    t;
  acc


let rec elim_rec t = (* get rid of T_rec and T_refer *)
  match t.term with
    | T_int | T_uint | T_hyper | T_uhyper | T_enum _ | T_float
    | T_double | T_opaque_fixed _ | T_opaque _ | T_string _ 
    | T_mstring _ | T_void | T_param _ ->
	t
    | T_array_fixed(t',n) ->
	{ t with term = T_array_fixed(elim_rec t', n) }
    | T_array(t',n) ->
	{ t with term = T_array(elim_rec t', n) }
    | T_struct s ->
	let s' = 
	  Array.map
	    (fun (n,t') ->  (n, elim_rec t'))
	    s in
	{ t with term = T_struct s' }
    | T_union_over_int(ht, dt) ->
	let ht' =
	  map_hashtbl
	    (fun c t' -> elim_rec t')
	    ht in
	let dt' = map_opt elim_rec dt in
	{ t with term = T_union_over_int(ht', dt') }
    | T_union_over_uint(ht, dt) ->
	let ht' =
	  map_hashtbl
	    (fun c t' ->  elim_rec t')
	    ht in
	let dt' = map_opt elim_rec dt in
	{ t with term = T_union_over_uint(ht', dt') }
    | T_union_over_enum(et,ct,dt) ->
	let et' = elim_rec et in
	let ct' = Array.map (map_opt elim_rec) ct in
	let dt' = map_opt elim_rec dt in
	{ t with term = T_union_over_enum(et',ct',dt') }
    | T_rec(n,t') ->
	elim_rec t'
    | T_refer(n,t') ->
	t'
    | T_direct(t',read,write,size) ->
	{ t with term = T_direct(elim_rec t', read, write, size) }


let rec calc_min_size t =

  let ( ++ ) x y =
    (* pre: x >= 0 && y >= 0 *)
    let s = x + y in
    if s < 0 then (* can only happen on 32 bit platforms *)
      raise(Propagate("Minimum size of type exceeds limit"));
    s in


  let calc_for_union u_snd default =
    ( match default with
	| None -> ()
	| Some d -> calc_min_size d
    );
    List.iter (fun t' -> calc_min_size t') u_snd;
    let l =
      (match default with
	 | None -> []
	 | Some d -> [d]
      ) @ u_snd in
    assert(l <> []);
    4 ++
      (List.fold_left
	 (fun acc x ->
	    min acc x.min_size
	 )
	 ((List.hd l).min_size)
	 (List.tl l)
      )
  in

  let hashtbl_vals h =
    Hashtbl.fold (fun _ v acc -> v :: acc) h [] in

  let optarray_elems a =
    Array.fold_left
      (fun acc x_opt ->
	 match x_opt with
	   | None -> acc
	   | Some x -> x :: acc
      )
      []
      a in

  if t.min_size < 0 then (
    t.min_size <- 0;   (* for stopping recursions *)
    ( match t.term with
	  T_int    -> t.min_size <- 4
	| T_uint   -> t.min_size <- 4
	| T_hyper  -> t.min_size <- 8
	| T_uhyper -> t.min_size <- 8
	| T_float  -> t.min_size <- 4
	| T_double -> t.min_size <- 8
	| T_void   -> t.min_size <- 0
	| T_enum e -> t.min_size <- 4
	| T_opaque_fixed n -> 
	    let nL = int64_of_uint4 n in
	    let min_size =
	      if nL=0L then 0 
	      else Int64.to_int(Int64.succ (Int64.div (Int64.pred nL) 4L)) in
	    t.min_size <- min_size
	| T_opaque n -> t.min_size <- 4
	| T_string n -> t.min_size <- 4
	| T_mstring (name,n) -> t.min_size <- 4
	| T_array_fixed (s,n) -> 
	    calc_min_size s;
	    if s.min_size = 0 then
	      raise(Propagate "Array elements must not have length 0");
	    let nL = int64_of_uint4 n in
	    let n_max = max_int / s.min_size in
	    if nL > Int64.of_int n_max then
	      raise(Propagate "Minimum size of type exceeds limit");
	    let iL = Int64.of_int s.min_size in
	    t.min_size <- Int64.to_int (Int64.mul nL iL)
	| T_array (s,n) -> 
	    calc_min_size s;
	    if s.min_size = 0 then
	      raise(Propagate "Array elements must not have length 0");
	    t.min_size <- 4
	| T_struct s ->
	    Array.iter (fun (_,t') -> calc_min_size t') s;
	    t.min_size <-
	      (Array.fold_left
		 (fun acc (_,x) ->
		    acc ++ x.min_size
		 )
		 0
		 s
	      )
	| T_union_over_int (u, default) ->
	    t.min_size <- calc_for_union (hashtbl_vals u) default
	| T_union_over_uint (u, default) ->
	    t.min_size <- calc_for_union (hashtbl_vals u) default
	| T_union_over_enum (e,u,default) ->
	    t.min_size <- calc_for_union (optarray_elems u) default
	| T_param p ->
	    (* not optimal, but we do not know it better at this point *)
	    t.min_size <- 0
	| T_direct(t',_,_,_) ->
	    calc_min_size t';
	    t.min_size <- t'.min_size
	| T_rec (_,t') ->
	    calc_min_size t';
	    t.min_size <- t'.min_size
	| T_refer (r,t') ->
	    calc_min_size t';
	    t.min_size <- t'.min_size;
	    (* eprintf "%s: " r*)
    );
    (* eprintf "min_size(%s) = %d\n" (t_name t.term) t.min_size*)
  )


let rec validate_xdr_type (t:xdr_type_term) : xdr_type =
  let r n =
    raise (Propagate "Cannot resolve X_type element")
  in
  try
    let t0' = validate_xdr_type_i1 r [] t in
    let pl = find_params t0' in
    t0'.params <- pl;
    let t1' = elim_rec t0' in
    calc_min_size t0';
    calc_min_size t1';
    (t0', t1')
  with
    Not_found ->
      failwith "Xdr.validate_xdr_type: unspecified error"
  | Propagate s ->
      failwith ("Xdr.validate_xdr_type: " ^ s)
;;


let params (t:xdr_type) =
  StringSet.fold
    (fun p acc -> p :: acc)
    (fst t).params
    []


let rec expand_X_type (s:xdr_type_system) (t:xdr_type_term) : xdr_type0 =
  match t with
    X_type n ->
      begin
	let rec r s1 s2 =
	  match s2 with
	    []       -> raise (Propagate ("Cannot resolve X_type " ^ n))
	  | (n',t') :: s2' ->
	      if n = n' then
		fst t'
	      else
		r (s1 @ [n',t']) s2'
	in
	r [] s
      end
  | _ ->
      raise (Propagate "Found X_type where it must not occur")
;;


let validate_xdr_type_system (s:xdr_type_term_system) : xdr_type_system =
  let names = List.map fst s in
  if all_distinct names then begin
    let rec r (s1:xdr_type_system) (s2:xdr_type_term_system) =
      match s2 with
	[]           -> []
      |	(n,t) :: s2' ->
	  let t2 =
	  begin
	    try
	      let t0' = validate_xdr_type_i1 (expand_X_type s1) [] t in
	      let pl = find_params t0' in
	      t0'.params <- pl;
	      let t1' = elim_rec t0' in
	      calc_min_size t0';
	      calc_min_size t1';
	      (t0',t1')
	    with
	      Not_found -> failwith "Xdr.validate_xdr_type_system: unspecified error"
	    | Propagate s -> failwith ("Xdr.validate_xdr_type_system: " ^ s)
	  end
	  in
	  (n,t2)::(r (s1 @ [n,t2]) s2')
    in
    r [] s
  end
  else
    failwith "Xdr.validate_xdr_type_system: type system has members with same names"
;;


(**********************************************************************)
(* the reverse way                                                    *)
(**********************************************************************)


let rec xdr_type_term0 (t:xdr_type0) : xdr_type_term =
  let conv_list l =
    List.map (fun (x, t') -> x, xdr_type_term0 t') l in
  let conv_htbl htbl =
    Hashtbl.fold (fun x t' l -> (x, xdr_type_term0 t') :: l) htbl [] in
  let conv_option p =
    match p with None -> None | Some t' -> Some (xdr_type_term0 t') in

  match t.term with
    T_int    -> X_int
  | T_uint   -> X_uint
  | T_hyper  -> X_hyper
  | T_uhyper -> X_uhyper
  | T_enum l -> X_enum (Array.to_list
			  (Array.map
			     (fun (n,i) -> (n,Netnumber.int4_of_int32 i))
			     l))
  | T_float  -> X_float
  | T_double -> X_double
  | T_void   -> X_void
  | T_param p        -> X_param p
  | T_opaque_fixed n -> X_opaque_fixed n
  | T_opaque n       -> X_opaque n
  | T_string n       -> X_string n
  | T_mstring(name,n)-> X_mstring(name,n)
  | T_array_fixed (t', n) -> X_array_fixed (xdr_type_term0 t',n)
  | T_array (t', n)       -> X_array       (xdr_type_term0 t',n)
  | T_struct s       -> X_struct (conv_list (Array.to_list s))
  | T_rec (n, t')    -> X_rec (n, xdr_type_term0 t')
  | T_refer (n, t')  -> X_refer n
  | T_union_over_int (u,d)  -> X_union_over_int  (conv_htbl u, conv_option d)
  | T_union_over_uint (u,d) -> X_union_over_uint (conv_htbl u, conv_option d)
  | T_union_over_enum ( { term = T_enum e } as e_term ,u,d) ->
      let u' =
	List.flatten
	  (Array.to_list
	     (Array.mapi
		(fun k t'_opt ->
		   match t'_opt with
		       Some t' ->
			 let name = fst(e.(k)) in
			 [ name, xdr_type_term0 t' ]
		     | None ->
			 []
		)
		u
	     )
	  )
      in
      X_union_over_enum (xdr_type_term0 e_term, u', conv_option d)
  | T_direct (t', read, write, size) -> 
      X_direct (xdr_type_term0 t',read, write, size)
  | _ ->
      assert false
;;


let xdr_type_term (t:xdr_type) : xdr_type_term =
  xdr_type_term0 (fst t)


let xdr_type_term_system (s:xdr_type_system) : xdr_type_term_system =
  List.map (fun (n,t) -> n,xdr_type_term t) s
;;


(**********************************************************************)
(* expand X_type members relative to given systems                    *)
(**********************************************************************)

(* The implementation of "expanded_xdr_type_term" repeats many phrases
 * that have been defined for "validate_xdr_type" in a very similar
 * way.
 * TODO: Currently many checks have been left out
 *)


let rec expanded_xdr_type_term (s:xdr_type_term_system) (t:xdr_type_term)
        : xdr_type_term =
  match t with
    X_array_fixed (t',n) ->
      X_array_fixed ((expanded_xdr_type_term s t'), n)
  | X_array (t',n) ->
      X_array ((expanded_xdr_type_term s t'), n)
  | X_struct st ->
      let s_names, s_types = List.split st in
      X_struct
	(List.combine
	   s_names
	   (List.map (expanded_xdr_type_term s) s_types))
  | X_union_over_int (u,default) ->
      let u_values, u_types = List.split u in
      let default' =
	match default with
	  Some d -> Some (expanded_xdr_type_term s d)
	| None   -> None
      in
      X_union_over_int
	(List.combine
	   u_values
	   (List.map (expanded_xdr_type_term s) u_types), default')
  | X_union_over_uint (u,default) ->
      let u_values, u_types = List.split u in
      let default' =
	match default with
	  Some d -> Some (expanded_xdr_type_term s d)
	| None   -> None
      in
      X_union_over_uint
	(List.combine
	   u_values
	   (List.map (expanded_xdr_type_term s) u_types), default')
  | X_union_over_enum (e,u,default) ->
      let u_values, u_types = List.split u in
      let default' =
	match default with
	  Some d -> Some (expanded_xdr_type_term s d)
	| None   -> None
      in
      X_union_over_enum
	( (expanded_xdr_type_term s e),
	 (List.combine
	    u_values
	    (List.map (expanded_xdr_type_term s) u_types)),
	 default')
  | X_type n ->
      let rec r s1 s2 =
	match s2 with
	  [] ->
	    failwith ("Xdr.expanded_xdr_type_term: cannot resolve X_type " ^ n)
	| (n',t') :: s2' ->
	      if n = n' then
		expanded_xdr_type_term s1 t'
	      else
		r (s1 @ [n',t']) s2'
      in
      r [] s
  | X_rec (n, t') ->
      X_rec (n, expanded_xdr_type_term s t')
  | X_direct (t',read, write, size) ->
      X_direct ((expanded_xdr_type_term s t'), read, write, size)
  | _ ->
      t
;;


let expanded_xdr_type (s:xdr_type_system) (t:xdr_type_term) : xdr_type =
  try
    let t0 = validate_xdr_type_i1 (expand_X_type s) [] t in
    let t1 = elim_rec t0 in
    calc_min_size t0;
    calc_min_size t1;
    (t0,t1)
  with
    Not_found -> failwith "Xdr.expanded_xdr_type: unspecified error"
  | Propagate s -> failwith ("Xdr.expanded_xdr_type: " ^ s)
;;


(**********************************************************************)
(* test on compatibility                                              *)
(**********************************************************************)

let are_compatible (s1:xdr_type) (s2:xdr_type) : bool =
  (* implementation:
   * enum, struct and union members can be swapped
   *)

  failwith "Xdr.are_compatible: not implemented"

;;


(**********************************************************************)
(* common implementation of value_matches_type & pack_xdr_value       *)
(**********************************************************************)

(* pack: interestingly, two loops over the value where one loop only
   determines the size of the final buffer are _faster_ than a single
   loop over the value doing everything. Whoever understands that.
 *)

type encoder = Xdr_mstring.mstring list -> Xdr_mstring.mstring list
type decoder = string -> int -> int -> (string * int)


let overflow() =
  raise(Xdr_failure "overflow in ++")


let ( ++ ) x y =
  (* pre: x >= 0 && y >= 0 *)
  let s = x + y in
  if s < 0 then overflow();
  s


let get_string_decoration_size x_len n =
  (* header field plus padding *)
  let x_len_u = uint4_of_int x_len in
  let x_len_mod_4 = x_len land 3 in
  if Netnumber.le_uint4 x_len_u n then begin
    (if x_len_mod_4 = 0
     then 4 
     else 8 - x_len_mod_4
    )
  end
  else
    raise 
      (Xdr_failure "string is longer than allowed")


let sizefn_string n x =
  let x_len = String.length x in
  get_string_decoration_size x_len n + x_len


let sizefn_mstring n x =
  let x_len = x#length in
  get_string_decoration_size x_len n + x_len


let pack_size
      (v:xdr_value)
      (t:xdr_type0)
      (get_param:string->xdr_type)
      (get_encoder:string->encoder option)
    : int =

  (* returned size does not include mstrings! *)

  let rec get_size v t =
    match t.term with
      | T_int ->
	  4
      | T_uint ->
	  4
      | T_hyper ->
	  8
      | T_uhyper ->
	  8
      | T_enum e ->
	  4
      | T_float ->
	  4
      | T_double ->
	  8
      | T_opaque_fixed n ->
	  int_of_uint4 n
      | T_opaque n ->
	  let x = dest_xv_opaque v in
	  sizefn_string n x
      | T_string n ->
	  let x = dest_xv_string v in
	  sizefn_string n x
      | T_mstring(_,n) ->
	  (* for an mstring we only count the length field plus padding *)
	  let x = dest_xv_mstring v in
	  let l = x#length in
	  get_string_decoration_size l n
      | T_array_fixed (t',n) ->
	  get_array_size v t' n (fun m n -> m=n)
      | T_array (t',n) ->
	  4 + get_array_size v t' n Netnumber.le_uint4
      | T_struct s ->
	  let v_array = map_xv_struct_fast0 t v in
	  let sum = ref 0 in
	  Array.iteri
	    (fun k v_component ->
	       sum := !sum ++ get_size v_component (snd s.(k)))
	    v_array;
	  !sum
      | T_union_over_int (u,default) ->
	  let i,x = dest_xv_union_over_int v in
	  let t' =
      	    try
	      Hashtbl.find u i
	    with
		Not_found ->
		  match default with
		      Some d -> d
		    | None   -> raise (Xdr_failure "T_union_over_int")
	  in
	  4 ++ get_size x t'
      | T_union_over_uint (u,default) ->
	  let i,x = dest_xv_union_over_uint v in
	  let t' =
      	    try
	      Hashtbl.find u i
	    with
		Not_found ->
		  match default with
		      Some d -> d
		    | None   -> raise (Xdr_failure "T_union_over_uint")
	  in
	  4 ++ get_size x t'
      | T_union_over_enum (et,u,default) ->
	  let k,i,x = map_xv_union_over_enum_fast0 t v in
	  let t' =
	    match u.(k) with
		Some u_t -> u_t
	      | None     ->
		  ( match default with
			Some d -> d
		      | None -> raise (Xdr_failure "T_union_over_enum")
		  )
	  in
	  4 ++ get_size x t'
      | T_void ->
	  0
      | T_param n ->
	  let t' = get_param n in
	  let enc_opt = get_encoder n in
	  if enc_opt = None then
	    get_size v (snd t')
	  else
	    0
      | T_rec (n, t') ->
	  get_size v t'
      | T_refer (n, t') ->
	  get_size v t'
      | T_direct(t', _, _, _) ->
	  ( match v with
	      | XV_direct(_,size) -> size
	      | _ -> get_size v t'
	  )

  and get_array_size v t' n cmp =  (* w/o array header *)
    (* TODO: optimize arrays of types with fixed repr length *)
    match v with
      | XV_array x ->  (* generic *)
	  let m = uint4_of_int (Array.length x) in
	  if cmp m n then (
	    let s = ref 0 in
	    Array.iter
	      (fun v' -> s := !s ++ get_size v' t')
	      x;
	    !s
	  )
	  else
	    raise (Xdr_failure "array length mismatch")
      | XV_array_of_string_fast x ->
	  ( match t'.term with
	      | T_string sn ->
		  let m = uint4_of_int (Array.length x) in
		  if cmp m n then (
		    let sum = ref 0 in
		    Array.iter
		      (fun s -> sum := !sum ++ sizefn_string sn s)
		      x;
		    !sum
		  )
		  else 
		    raise (Xdr_failure "array length mismatch")
	      | T_direct(t1, _, _, _) ->
		  get_array_size v t1 n cmp
	      | _ -> 
		  raise Dest_failure
	  )
      | _ ->
	  raise Dest_failure

  in
  get_size v t


let print_string_padding l buf pos =
  let n = 4-(l land 3) in
  if n < 4 then begin
    let p = !pos in
    if n >= 1 then String.unsafe_set buf p '\000';
    if n >= 2 then String.unsafe_set buf (p + 1) '\000';
    if n >= 3 then String.unsafe_set buf (p + 2) '\000';
    pos := p + n
  end


let rec pack_mstring 
      (v:xdr_value)
      (t:xdr_type0)
      (get_param:string->xdr_type)
      (get_encoder:string->encoder option)
    : Xdr_mstring.mstring list =
  (* The recursion over pack_mstring is only used for encoded parameters *)

  let size = pack_size v t get_param get_encoder in
  (* all sanity checks are done here! Also, [size] does not include the
     size for mstrings (only the length field, and padding), and it does
     not include encoded parameters
   *)

  let buf = String.create size in
  let buf_start = ref 0 in
  let buf_pos = ref 0 in

  let result = ref [] in
  (* The resulting mstrings in reverse order *)

  let save_buf() =
    if !buf_pos > !buf_start then (
      let x =
	Xdr_mstring.string_based_mstrings # create_from_string
	  buf !buf_start (!buf_pos - !buf_start) false in
      result := x :: !result;
      buf_start := !buf_pos
    )
  in

  let print_string s l =
    String.unsafe_blit s 0 buf !buf_pos l;
    buf_pos := !buf_pos + l;
    print_string_padding l buf buf_pos
  in

  let rec pack v t =
    match t.term with
	T_int ->
	  let x = dest_xv_int v in
	  Netnumber.BE.write_int4_unsafe buf !buf_pos x;
	  buf_pos := !buf_pos + 4
      | T_uint ->
	  let x = dest_xv_uint v in
	  Netnumber.BE.write_uint4_unsafe buf !buf_pos x;
	  buf_pos := !buf_pos + 4
      | T_hyper ->
	  let x = dest_xv_hyper v in
	  Netnumber.BE.write_int8_unsafe buf !buf_pos x;
	  buf_pos := !buf_pos + 8
      | T_uhyper ->
	  let x = dest_xv_uhyper v in
	  Netnumber.BE.write_uint8_unsafe buf !buf_pos x;
	  buf_pos := !buf_pos + 8
      | T_enum e ->
	  let i = map_xv_enum_fast0 t v in
	  Netnumber.BE.write_int4_unsafe buf !buf_pos (int4_of_int32 i);
	  buf_pos := !buf_pos + 4
      | T_float ->
	  let x = dest_xv_float v in
	  let s = Netnumber.BE.fp4_as_string x in
	  String.unsafe_blit s 0 buf !buf_pos 4;
	  buf_pos := !buf_pos + 4
      | T_double ->
	  let x = dest_xv_double v in
	  let s = Netnumber.BE.fp8_as_string x in
	  String.unsafe_blit s 0 buf !buf_pos 8;
	  buf_pos := !buf_pos + 8
      | T_opaque_fixed n ->
	  let x = dest_xv_opaque v in
	  print_string x (String.length x)
      | T_opaque n ->
	  let x = dest_xv_opaque v in
	  let x_len = String.length x in
	  Netnumber.BE.write_uint4_unsafe buf !buf_pos (uint4_of_int x_len);
	  buf_pos := !buf_pos + 4;
	  print_string x x_len
      | T_string n ->
	  let x = dest_xv_string v in
	  let x_len = String.length x in
	  Netnumber.BE.write_uint4_unsafe buf !buf_pos (uint4_of_int x_len);
	  buf_pos := !buf_pos + 4;
	  print_string x x_len
      | T_mstring(_,n) ->
	  let x = dest_xv_mstring v in
	  let x_len = x#length in
	  Netnumber.BE.write_uint4_unsafe buf !buf_pos (uint4_of_int x_len);
	  buf_pos := !buf_pos + 4;
	  save_buf();
	  result := x :: !result;
	  print_string_padding x_len buf buf_pos
      | T_array_fixed (t',n) ->
	  pack_array v t' n false
      | T_array (t',n) ->
	  pack_array v t' n true
      | T_struct s ->
	  let v_array = map_xv_struct_fast0 t v in
	  Array.iteri
	    (fun k v_component ->
	       pack v_component (snd s.(k)))
	    v_array
      | T_union_over_int (u,default) ->
	  let i,x = dest_xv_union_over_int v in
	  let t' =
      	    try
	      Hashtbl.find u i
	    with
		Not_found ->
		  match default with
		      Some d -> d
		    | None   -> raise (Xdr_failure "T_union_over_int")
	  in
	  Netnumber.BE.write_int4_unsafe buf !buf_pos i;
	  buf_pos := !buf_pos + 4;
	  pack x t'
      | T_union_over_uint (u,default) ->
	  let i,x = dest_xv_union_over_uint v in
	  let t' =
      	    try
	      Hashtbl.find u i
	    with
		Not_found ->
		  match default with
		      Some d -> d
		    | None   -> raise (Xdr_failure "T_union_over_uint")
	  in
	  Netnumber.BE.write_uint4_unsafe buf !buf_pos i;
	  buf_pos := !buf_pos + 4;
	  pack x t'
      | T_union_over_enum (et,u,default) ->
	  let k,i,x = map_xv_union_over_enum_fast0 t v in
	  let t' =
	    match u.(k) with
		Some u_t -> u_t
	      | None     ->
		  ( match default with
			Some d -> d
		      | None -> raise (Xdr_failure "T_union_over_enum")
		  )
	  in
	  Netnumber.BE.write_int4_unsafe buf !buf_pos (int4_of_int32 i);
	  buf_pos := !buf_pos + 4;
	  pack x t'
      | T_void ->
	  ()
      | T_param n ->
	  let t' = get_param n in
	  let enc_opt = get_encoder n in
	  ( match enc_opt with
	      | None -> pack v (snd t')
	      | Some enc ->
		  save_buf();
		  let l = 
		    pack_mstring v (snd t')
		      (fun _ -> assert false) (fun _ -> assert false) in
		  let e =
		    enc l in
		  result := List.rev e @ !result
	  )
      | T_rec (n, t') ->
	  pack v t'
      | T_refer (n, t') ->
	  pack v t'
      | T_direct(t', _, write, _) ->
	  ( match v with
	      | XV_direct(x,xv_size) ->
		  let old = !buf_pos in
		  write x buf buf_pos;
(* Printf.eprintf "old=%d new=%d size=%d\n" old !buf_pos size; *)
		  assert(!buf_pos = old + xv_size);
	      | _ -> pack v t'
	  )

  and pack_array v t' n have_array_header =
    match v with
      | XV_array x ->  (* generic *)
	  if have_array_header then pack_array_header (Array.length x);
	  Array.iter
	    (fun v' -> pack v' t')
	    x
      | XV_array_of_string_fast x ->
	  ( match t'.term with
	      | T_string n ->
		  if have_array_header then pack_array_header (Array.length x);
		  Array.iter
		    (fun s ->
		       let s_len = String.length s in
		       Netnumber.BE.write_uint4_unsafe
			 buf !buf_pos (uint4_of_int s_len);
		       buf_pos := !buf_pos + 4;
		       print_string s s_len
		    )
		    x
	      | T_direct(t1,_,_,_) ->
		  pack_array v t1 n have_array_header
	      | _ -> raise Dest_failure
	  )
      | _ -> raise Dest_failure

  and pack_array_header x_len =
    Netnumber.BE.write_uint4_unsafe buf !buf_pos (uint4_of_int x_len);
    buf_pos := !buf_pos + 4;
  in
  pack v t;
  save_buf();
  List.rev !result
;;


let write_string_fixed n x buf pos = (* exported *)
  let x_len = String.length x in
  if x_len <> n then
    raise (Xdr_failure "fixed string has bad length");
  String.unsafe_blit x 0 buf !pos x_len;
  pos := !pos + x_len;
  print_string_padding x_len buf pos
  

let write_string x buf pos = (* exported *)
  let x_len = String.length x in
  Netnumber.BE.write_uint4_unsafe buf !pos (uint4_of_int x_len);
  pos := !pos + 4;
  String.unsafe_blit x 0 buf !pos x_len;
  pos := !pos + x_len;
  print_string_padding x_len buf pos


let value_matches_type
    (v:xdr_value)
    ((_,t):xdr_type)
    (p:(string * xdr_type) list)
  : bool =
  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty (fst t').params) p then
    try
      ignore(pack_size v t (fun n -> List.assoc n p) (fun _ -> None));
      true
    with
      _ ->      (* we assume here that no other errors can occur *)
      	false
  else
    false
;;


(**********************************************************************)
(* pack and unpack values                                             *)
(**********************************************************************)

let pack_xdr_value
    ?(encode = [])
    (v:xdr_value)
    ((_,t):xdr_type)
    (p:(string * xdr_type) list)
    (print:string->unit)
  : unit =

  (* DEBUG *)
  (* List.iter (fun pn -> prerr_endline ("param " ^ pn)) t.params; *)

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty (fst t').params) p then
    try
      let mstrings = 
	pack_mstring v t
	  (fun n -> List.assoc n p) 
	  (fun n -> try Some(List.assoc n encode) with Not_found -> None) in
      List.iter
	(fun ms ->
	   let (s,p) = ms#as_string in
	   print (String.sub s p ms#length)
	)
	mstrings
    with
      | Dest_failure ->
	  raise(Xdr_failure "Xdr.pack_xdr_value [2]: XDR type mismatch")
      | Netnumber.Cannot_represent _ ->
	  raise(Xdr_failure "Xdr.pack_xdr_value [3]: integer not representable")
      | Netnumber.Out_of_range ->
	  raise(Xdr_failure "Xdr.pack_xdr_value [4]: index out of range")
      | Failure s ->
	  raise(Xdr_failure ("Xdr.pack_xdr_value [5]: " ^ s))
  else
    raise(Xdr_failure "Xdr.pack_xdr_value [1]")
;;


let pack_xdr_value_as_string
    ?(rm = false)
    ?(encode = [])
    (v:xdr_value)
    ((_,t):xdr_type)
    (p:(string * xdr_type) list)
  : string =

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty (fst t').params) p then
    try
      let mstrings0 = 
	pack_mstring v t 
	  (fun n -> List.assoc n p) 
	  (fun n -> try Some(List.assoc n encode) with Not_found -> None) in
      let rm_prefix =
	if rm then
	  let s = "\000\000\000\000" in
	  [ Xdr_mstring.string_based_mstrings # create_from_string s 0 4 false ]
	else
	  [] in
      let mstrings = rm_prefix @ mstrings0 in
      Xdr_mstring.concat_mstrings mstrings
    with
      | Dest_failure ->
(*let bt = Printexc.get_backtrace() in
eprintf "Backtrace: %s\n" bt; *)
	  raise(Xdr_failure
		  "Xdr.pack_xdr_value_as_string [2]: XDR type mismatch")
      | Netnumber.Cannot_represent _ ->
	  raise(Xdr_failure
		  "Xdr.pack_xdr_value_as_string [3]: integer not representable")
      | Netnumber.Out_of_range ->
	  raise(Xdr_failure
		  "Xdr.pack_xdr_value_as_string [4]: index out of range")
      | Failure s ->
	  raise(Xdr_failure ("Xdr.pack_xdr_value_as_string [5]: " ^ s))
  else
    raise(Xdr_failure "Xdr.pack_xdr_value_as_string [1]")
;;

let pack_xdr_value_as_mstrings
    ?(encode = [])
    (v:xdr_value)
    ((_,t):xdr_type)
    (p:(string * xdr_type) list)
    =

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty (fst t').params) p then
    try
      pack_mstring v t 
	(fun n -> List.assoc n p)
	(fun n -> try Some(List.assoc n encode) with Not_found -> None)
    with
      | Dest_failure ->
	  raise(Xdr_failure
		  "Xdr.pack_xdr_value_as_mstring [2]: XDR type mismatch")
      | Netnumber.Cannot_represent _ ->
	  raise
	    (Xdr_failure
	       "Xdr.pack_xdr_value_as_mstring [3]: integer not representable")
      | Netnumber.Out_of_range ->
	  raise(Xdr_failure
		  "Xdr.pack_xdr_value_as_mstring [4]: index out of range")
      | Failure s ->
	  raise(Xdr_failure ("Xdr.pack_xdr_value_as_mstring [5]: " ^ s))
  else
    raise(Xdr_failure "Xdr.pack_xdr_value_as_mstring [1]")
;;

(* "let rec" prevents that these functions are inlined. This is wanted here,
   because these are error cases, and for a function call less code
   is generated than for raising an exception
 *)

let rec raise_xdr_format_too_short () =
  raise (Xdr_format "message too short")

let rec raise_xdr_format_value_not_included () =
  raise (Xdr_format "value not included in enumeration")

let rec raise_xdr_format_maximum_length () =
  raise (Xdr_format "maximum length of field exceeded")

let rec raise_xdr_format_undefined_descriminator() =
  raise (Xdr_format "undefined discriminator")


let rec find_enum (e : (string * int32) array) (i : int32) =
  (* no inlining! *)
  let rec loop lb ub =
    (* The element is between lb and ub *)
    if lb > ub then raise_xdr_format_value_not_included ();
    let m = (ub + lb) lsr 1 in
    let x_m = snd(e.(m)) in
    if i = x_m then
      (* Found! *)
      m
    else if i < x_m then
      loop lb (m-1)
    else
      (* i > x_m *)
      loop (m+1) ub
  in
  loop 0 (Array.length e - 1)
;;

(* DEBUG*)
(*
let hex_dump_s s pos len =
  let b = Buffer.create 100 in
  for k = 0 to len - 1 do
    let c = s.[pos+k] in
    bprintf b "%02x " (Char.code c)
  done;
  Buffer.contents b
 *)

let read_string_fixed n str k k_end = (* exported *)
  let k0 = !k in
  let m = if n land 3 = 0 then n else n+4-(n land 3) in
  if k0 > k_end - m then raise_xdr_format_too_short ();
  let s = String.create n in
  String.unsafe_blit str k0 s 0 n;
  k := k0 + m;
  s

let read_string n str k k_end = (* exported *)
  let k0 = !k in
  k := k0 + 4;
  if !k > k_end then raise_xdr_format_too_short();
  let m = Netnumber.BE.read_uint4_unsafe str k0 in
    (* Test: n < m as unsigned int32: *)
  if Netnumber.lt_uint4 n m then
    raise_xdr_format_maximum_length ();
  read_string_fixed (int_of_uint4 m) str k k_end
  

let empty_mf = Hashtbl.create 1

let rec unpack_term
    ?(pos = 0)
    ?len
    ?(fast = false)
    ?(prefix = false)
    ?(mstring_factories = empty_mf)
    ?(xv_version = if fast then `Ocamlrpcgen else `V1)
    (str:string)
    (t:xdr_type0)
    (get_param:string->xdr_type)
    (get_decoder:string->decoder option)
  : xdr_value * int =

  (* The recursion over unpack_term is only used for decoding encrypted
     parameters
   *)

  let xv_version =
    if xv_version = `Ocamlrpcgen then `V4 else xv_version in

  let v2 = (xv_version <> `V1) in      (* meaning: at least v2 *)
  let v3 = v2 && (xv_version <> `V2) in
  let v4 = v3 && (xv_version <> `V3) in

  let len =
    match len with
	None -> String.length str - pos
      | Some l -> l
  in

  if pos < 0 || len < 0 || len > String.length str - pos then
    invalid_arg "Xdr.unpack_xdr_value";

  let k_end = pos+len in
  let k = ref pos in

  let rec read_fp4 k0 =
    if k0 + 4 > k_end then raise_xdr_format_too_short();
    k := !k + 4;
    Netnumber.BE.read_fp4 str k0
  in

  let rec read_fp8 k0 =
    if k0 + 8 > k_end then raise_xdr_format_too_short();
    k := !k + 8;
    Netnumber.BE.read_fp8 str k0
  in

  let rec read_enum e k0 =
    k := k0 + 4;
    if !k > k_end then raise_xdr_format_too_short();
    let i = Netnumber.int32_of_int4(Netnumber.BE.read_int4_unsafe str k0) in
    let j = find_enum e i in   (* returns array position, or Xdr_format *)
    if v2 then
      XV_enum_fast j
    else
      XV_enum(fst(e.(j)))
  in

  let rec read_string_or_opaque n k0 =
    k := k0 + 4;
    if !k > k_end then raise_xdr_format_too_short();
    let m = Netnumber.BE.read_uint4_unsafe str k0 in
    (* Test: n < m as unsigned int32: *)
    if Netnumber.lt_uint4 n m then
      raise_xdr_format_maximum_length ();
    read_string_fixed (int_of_uint4 m) str k k_end
  in

  let rec read_mstring name n k0 =
    let factory =
      try Hashtbl.find mstring_factories name
      with Not_found -> 
	( try Hashtbl.find mstring_factories "*"
	  with Not_found ->
	    failwith "read_mstring: no such factory"
	) in
    k := k0 + 4;
    if !k > k_end then raise_xdr_format_too_short();
    let m = Netnumber.BE.read_uint4_unsafe str k0 in
    (* Test: n < m as unsigned int32: *)
    if Netnumber.lt_uint4 n m then
      raise_xdr_format_maximum_length ();
    let m = int_of_uint4 m in
    let p = if m land 3 = 0 then m else m+4-(m land 3) in
    if !k > k_end - p then raise_xdr_format_too_short ();
    let ms = factory # create_from_string str !k m false in
    k := !k + p;
    ms
  in

  let rec unpack_array t' p =
    (* Estimate the maximum p *)
(* eprintf "unpack_array: t' = %s\n" (t_name t'.term);*)
    assert(t'.min_size > 0);
    let p_max = (k_end - !k) / t'.min_size in
    if p > p_max then 
      raise_xdr_format_too_short();
    match t'.term with
      | T_string n ->
	  let n' = Netnumber.logical_int32_of_uint4 n in
	  let a = Array.create p "" in
	  let k' = 
	    Netsys_xdr.s_read_string_array_unsafe str !k (k_end - !k) n' a in
	  if k' = (-1) then raise_xdr_format_too_short();
	  if k' = (-2) then raise_xdr_format_maximum_length ();
	  k := k';
	  if v3 then
	    XV_array_of_string_fast a
	  else
	    XV_array(Array.map (fun s -> XV_string s) a)
      | _ ->
	  let a = Array.create p XV_void in
	  for i = 0 to p-1 do
	    Array.unsafe_set a i (unpack t')
	  done;
	  XV_array a

  and unpack t =
    let k0 = !k in
(*    fprintf stderr "unpack k=%d t=%s\n%!" k0 (t_name t.term); *)
    match t.term with
      T_int ->
	k := k0 + 4;
	if !k > k_end then raise_xdr_format_too_short();
	XV_int (Netnumber.BE.read_int4_unsafe str k0)
    | T_uint ->
	k := k0 + 4;
	if !k > k_end then raise_xdr_format_too_short();
	XV_uint (Netnumber.BE.read_uint4_unsafe str k0)
    | T_hyper ->
	k := k0 + 8;
	if !k > k_end then raise_xdr_format_too_short();
	XV_hyper (Netnumber.BE.read_int8_unsafe str k0)
    | T_uhyper ->
	k := !k + 8;
	if k0 > k_end then raise_xdr_format_too_short();
	XV_uhyper (Netnumber.BE.read_uint8_unsafe str k0)
    | T_enum e ->
	read_enum e k0
    | T_float ->
	XV_float (read_fp4 k0)
    | T_double ->
	XV_double (read_fp8 k0)
    | T_opaque_fixed n ->
	XV_opaque (read_string_fixed (int_of_uint4 n) str k k_end)
    | T_opaque n ->
	XV_opaque (read_string_or_opaque n k0)
    | T_string n ->
	XV_string (read_string_or_opaque n k0)
    | T_mstring(name,n) ->
	XV_mstring (read_mstring name n k0)
    | T_array_fixed (t',n) ->
	let p = int_of_uint4 n in
	unpack_array t' p
    | T_array (t',n) ->
	k := k0 + 4;
	let m = Netnumber.BE.read_uint4 str k0 in
	if Netnumber.lt_uint4 n m then
	  raise_xdr_format_maximum_length ();
	unpack_array t' (int_of_uint4 m)
    | T_struct s ->
	if v2 then
	  XV_struct_fast
	    ( Array.map
		(fun (name,t') -> unpack t')
		s
	    )
	else
	  XV_struct
	    (List.map
	       (fun (name,t') -> (name,unpack t'))
	       (Array.to_list s)
	    )
    | T_union_over_int (u,default) ->
	unpack_union_over_int u default k0
    | T_union_over_uint (u,default) ->
	unpack_union_over_uint u default k0
    | T_union_over_enum ( { term = T_enum e },u,default) ->
	unpack_union_over_enum e u default k0
    | T_void ->
	XV_void
    | T_param p ->
	let t' = get_param p in
	let dec_opt = get_decoder p in
	( match dec_opt with
	    | None -> unpack (snd t')
	    | Some decoder ->
		let (dec_s, n) = decoder str k0 (k_end - k0) in
		k := !k + n;
		assert( !k <= k_end );
		let (v, p) = 
		  unpack_term
		    ~mstring_factories ~xv_version dec_s (snd t')
		    (fun _ -> assert false)
		    (fun _ -> None) in
		v
	)
    | T_rec (_, t')
    | T_refer (_, t') ->
	unpack t'
    | T_direct(t', read, _, _) ->
	if v4 then
	  let k0 = !k in
	  let xv = read str k k_end in
	  XV_direct(xv, !k-k0)
	else
	  unpack t'
    | _ ->
	assert false

  and unpack_union_over_int u default k0 =
    k := k0 + 4;
    let n = Netnumber.BE.read_int4 str k0 in
    let t' =
      try
	Hashtbl.find u n
      with
	  Not_found ->
	    match default with
		None   -> raise_xdr_format_undefined_descriminator()
	      |	Some d -> d
    in
    XV_union_over_int (n, unpack t')

  and unpack_union_over_uint u default k0 =
    k := k0 + 4;
    let n = Netnumber.BE.read_uint4 str k0 in
    let t' =
      try
	Hashtbl.find u n
      with
	  Not_found ->
	    match default with
		None   -> raise_xdr_format_undefined_descriminator()
	      |	Some d -> d
    in
    XV_union_over_uint (n, unpack t')

  and unpack_union_over_enum e u default k0 =
    k := k0 + 4;
    let i = Netnumber.int32_of_int4 (Netnumber.BE.read_int4 str k0) in
    let j = find_enum e i  (* returns array position, or Xdr_format *) in
    let t' =
      match u.(j) with
	  Some u_t -> u_t
	| None ->
	    ( match default with
		  Some d -> d
		| None ->
		    raise_xdr_format_undefined_descriminator()
	    )
    in
    if v2 then
      XV_union_over_enum_fast(j, unpack t')
    else
      let name = fst(e.(j)) in
      XV_union_over_enum(name, unpack t')
	
  in
  try
    let v = unpack t in
    if prefix || !k = k_end then
      (v, !k - pos)
    else (
(*
      fprintf stderr "Too LONG: k=%d k_end=%d\n%!" !k k_end;
      fprintf stderr "Dump: %s\n%!" (hex_dump_s str pos (k_end-pos));
 *)
      raise (Xdr_format_message_too_long v)
    )
  with
      Cannot_represent _ ->
	raise (Xdr_format "implementation restriction")
    | Out_of_range ->
	raise (Xdr_format "message too short")
;;


let unpack_xdr_value
    ?pos ?len ?fast ?prefix ?mstring_factories ?xv_version ?(decode=[])
    (str:string)
    ((_,t):xdr_type)
    (p:(string * xdr_type) list)
  : xdr_value =

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty (fst t').params) p then

    fst(unpack_term 
	  ?pos ?len ?fast ?prefix ?mstring_factories ?xv_version
	  str t
	  (fun n -> List.assoc n p)
	  (fun n -> try Some(List.assoc n decode) with Not_found -> None)
       )

  else
    failwith "Xdr.unpack_xdr_value"
;;


let unpack_xdr_value_l
    ?pos ?len ?fast ?prefix ?mstring_factories ?xv_version ?(decode=[])
    (str:string)
    ((_,t):xdr_type)
    (p:(string * xdr_type) list)
  : xdr_value * int =

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty (fst t').params) p then

    unpack_term
      ?pos ?len ?fast ?prefix ?mstring_factories ?xv_version
      str t
      (fun n -> List.assoc n p)
      (fun n -> try Some(List.assoc n decode) with Not_found -> None)

  else
    failwith "Xdr.unpack_xdr_value"
;;

