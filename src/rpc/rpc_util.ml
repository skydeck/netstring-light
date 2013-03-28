(* $Id: rpc_util.ml 1701 2012-02-14 14:48:46Z gerd $ *)

open Printf

type verbosity =
    [ `Name_only | `Name_abbrev_args | `Name_full_args ]

module StrMap = Map.Make(String)

(* The map functions are like those in Xdr, but the type is a
   xdr_type_term, not an xdr_type
 *)

let rec get_enum t =
  match t with
    | Xdr.X_enum enum -> enum
    | Xdr.X_direct(t1,_,_,_) -> get_enum t1
    | _ -> failwith "Rpc_util.get_enum"

let fail_map_xv_enum_fast () =
  failwith "Rpc_util.map_xv_enum_fast"

let map_xv_enum_fast t v =
  match t with
    | Xdr.X_enum l ->
	let l = Array.of_list l in
	let m = Array.length l in
	( match v with
	    | Xdr.XV_enum_fast k ->
		if k >= 0 && k < m then
		  snd(l.(k))
		else
		  fail_map_xv_enum_fast()
	    | Xdr.XV_enum name ->
		let k = ref 0 in
		while !k < m && (fst l.( !k ) <> name) do
		  incr k
		done;
		if !k >= m then
		  fail_map_xv_enum_fast();
		snd(l.( !k ))
	    | _ ->
		fail_map_xv_enum_fast()
	)
    | _ ->
	fail_map_xv_enum_fast()


let fail_map_xv_struct_fast () =
  failwith "Rpc_util.map_xv_struct_fast"

let map_xv_struct_fast t v =
  match t with
    | Xdr.X_struct decl ->
	let decl = Array.of_list decl in
	let m = Array.length decl in
	( match v with
	    | Xdr.XV_struct_fast x ->
		let k = Array.length x in
		if k = m then
		  x
		else
		  fail_map_xv_struct_fast()
	    | Xdr.XV_struct l ->
		( try
		    Array.map
		      (fun (name,y) -> List.assoc name l)
		      decl
		  with
		      Not_found -> fail_map_xv_struct_fast()
		)
	    | _ ->
		fail_map_xv_struct_fast()
	)
    | _ ->
	fail_map_xv_struct_fast()


let fail_map_xv_union_over_enum_fast () =
  failwith "Rpc_util.map_xv_struct_fast"

let map_xv_union_over_enum_fast t v =
  match t with
    | Xdr.X_union_over_enum(enum_t, u, u_dfl ) ->
	let e = Array.of_list (get_enum enum_t) in
	let u = Array.of_list u in
	let m = Array.length e in
	assert( m = Array.length u );
	( match v with
	    | Xdr.XV_union_over_enum_fast(k, x) ->
		if k >= 0 && k < m then
		  (k, (snd e.(k)), x)
		else
		  fail_map_xv_union_over_enum_fast()
	    | Xdr.XV_union_over_enum(name, x) ->
		let k = ref 0 in
		while !k < m && fst(e.( !k )) <> name do
		  incr k
		done;
		if !k >= m then
		  fail_map_xv_union_over_enum_fast();
		(!k, (snd e.(!k)), x)
	    | _ ->
		fail_map_xv_union_over_enum_fast()
	)
    | _ ->
	fail_map_xv_union_over_enum_fast()


let string_of_opaque s l =
  let b = Buffer.create 32 in
  for k = 0 to l - 1 do
    Buffer.add_string b (sprintf "%02x" (Char.code s.[k]))
  done;
  Buffer.contents b


let string_of_struct print_elem t v =
  let tl = 
    match t with
      | Xdr.X_struct tl -> Array.of_list tl
      | _ -> assert false in
  let vl = map_xv_struct_fast t v in
  "{" ^ 
    String.concat ";"
    (Array.to_list
       (Array.mapi
	  (fun k (elem_name, elem_t) ->
	     let elem_v = vl.(k) in
	     sprintf "%s=%s" elem_name (print_elem elem_t elem_v)
	  )
	  tl
       )
    ) ^ "}"


let string_of_array print_elem t v =
  let elem_t =
    match t with 
      | Xdr.X_array_fixed(u,_)
      | Xdr.X_array(u,_) ->
	  u
      | _ -> 
	  assert false in
  let vl =
    Xdr.dest_xv_array v in
  "[" ^ 
    String.concat ";"
    (Array.to_list
       (Array.map
	  (fun elem_v -> print_elem elem_t elem_v)
	  vl)) ^ "]"


let string_of_union print_elem t v =
  let elem_t, elem_v, case =
    match t with
      | Xdr.X_union_over_int(l, default) ->
	  let (n, elem_v) = Xdr.dest_xv_union_over_int v in
	  let elem_t =
	    try List.assoc n l
	    with Not_found ->
	      ( match default with
		  | None -> assert false
		  | Some d -> d
	      ) in
	  (elem_t, elem_v, sprintf "%ld" (Rtypes.int32_of_int4 n))
      | Xdr.X_union_over_uint(l, default) ->
	  let (n, elem_v) = Xdr.dest_xv_union_over_uint v in
	  let elem_t =
	    try List.assoc n l
	    with Not_found ->
	      ( match default with
		  | None -> assert false
		  | Some d -> d
	      ) in
	  (elem_t, elem_v, sprintf "%lu" (Rtypes.logical_int32_of_uint4 n))
      | Xdr.X_union_over_enum(enum_t, l, default) ->
	  let (k,_,elem_v) = map_xv_union_over_enum_fast t v in
	  let enum = get_enum enum_t in
	  let case, _ = List.nth enum k in
	  let elem_t =
	    try List.assoc case l
	    with Not_found ->
	      ( match default with
		  | None -> assert false
		  | Some d -> d
	      ) in
	  (elem_t, elem_v, case) 
      | _ -> assert false
  in
  sprintf
    "union<case=%s %s>"
    case
    (print_elem elem_t elem_v)


let rec string_of_rec_arg recdefs t v =
  match t with
    | Xdr.X_int ->
	sprintf "%ld" 
	  (Rtypes.int32_of_int4 (Xdr.dest_xv_int v))
    | Xdr.X_uint ->
	sprintf "%lu" 
	  (Rtypes.logical_int32_of_uint4 (Xdr.dest_xv_uint v))
    | Xdr.X_hyper ->
	sprintf "%Ld" 
	  (Rtypes.int64_of_int8 (Xdr.dest_xv_hyper v))
    | Xdr.X_uhyper ->
	sprintf "%Lu" 
	  (Rtypes.logical_int64_of_uint8 (Xdr.dest_xv_uhyper v))
    | Xdr.X_enum enum ->
	( match v with
	    | Xdr.XV_enum case ->
		case
	    | Xdr.XV_enum_fast n ->
		fst(List.nth enum n)
	    | _ -> assert false
	)
    | Xdr.X_float ->
	string_of_float
	  (Rtypes.float_of_fp4 (Xdr.dest_xv_float v))
    | Xdr.X_double ->
	string_of_float
	  (Rtypes.float_of_fp8 (Xdr.dest_xv_double v))
    | Xdr.X_opaque_fixed _
    | Xdr.X_opaque _ ->
	let s = Xdr.dest_xv_opaque v in
	string_of_opaque s (String.length s)
    | Xdr.X_string _ ->
	let s = Xdr.dest_xv_string v in
	"\"" ^ String.escaped s ^ "\""
    | Xdr.X_mstring(_, _) ->
	let ms = Xdr.dest_xv_mstring v in
	let (s,p) = ms#as_string in
	"\"" ^ String.escaped (String.sub s p (ms#length-p)) ^ "\""
    | Xdr.X_array_fixed _
    | Xdr.X_array _ ->
	string_of_array
	  (string_of_rec_arg recdefs)
	  t
	  v
    | Xdr.X_struct _ ->
	string_of_struct
	  (string_of_rec_arg recdefs)
	  t
	  v
    | Xdr.X_union_over_int _
    | Xdr.X_union_over_uint _
    | Xdr.X_union_over_enum _ ->
	string_of_union
	  (string_of_rec_arg recdefs)
	  t
	  v
    | Xdr.X_void ->
	"void"
    | Xdr.X_rec (n, u) ->
	let recdefs' = StrMap.add n u recdefs in
	string_of_rec_arg recdefs' t v

    | Xdr.X_refer n ->
	let u =
	  try StrMap.find n recdefs
	  with Not_found -> assert false in
	string_of_rec_arg recdefs u v

    | Xdr.X_direct(t1, _, _, _) ->
	string_of_rec_arg recdefs t1 v

    | Xdr.X_type _
    | Xdr.X_param _ ->
	assert false


let string_of_full_arg =
  string_of_rec_arg StrMap.empty


let rec string_of_abbrev_arg t v =
  match t with
    | Xdr.X_int
    | Xdr.X_uint
    | Xdr.X_hyper
    | Xdr.X_uhyper
    | Xdr.X_enum _
    | Xdr.X_float
    | Xdr.X_double
    | Xdr.X_void ->
	string_of_full_arg t v

    | Xdr.X_opaque_fixed _
    | Xdr.X_opaque _ ->
	let s = Xdr.dest_xv_opaque v in
	let l = min 16 (String.length s) in
	let suffix = if l < String.length s then "..." else "" in
	string_of_opaque s l ^ suffix

    | Xdr.X_string _ ->
	let s = Xdr.dest_xv_string v in
	let l = min 16 (String.length s) in
	let suffix = if l < String.length s then "..." else "" in
	"\"" ^ (String.escaped (String.sub s 0 l)) ^ "\"" ^ suffix

    | Xdr.X_mstring (_,_) ->
	let ms = Xdr.dest_xv_mstring v in
	let (s,p) = ms#as_string in
	let l = min 16 ms#length in
	let suffix = if l < ms#length then "..." else "" in
	"\"" ^ (String.escaped (String.sub s p l)) ^ "\"" ^ suffix

    | Xdr.X_array_fixed _
    | Xdr.X_array _ ->
	let a = Xdr.dest_xv_array v in
	"array<" ^ string_of_int (Array.length a) ^ ">"

    | Xdr.X_struct _ ->
	"struct"

    | Xdr.X_union_over_int(_,_) ->
	let (n,_) = Xdr.dest_xv_union_over_int v in
	sprintf "union<case=%ld>" (Rtypes.int32_of_int4 n)

    | Xdr.X_union_over_uint(_,_) ->
	let (n,_) = Xdr.dest_xv_union_over_uint v in
	sprintf "union<case=%lu>" (Rtypes.logical_int32_of_uint4 n)

    | Xdr.X_union_over_enum(enum_t,_,_) ->
	let e = get_enum enum_t in
	let (k,_,_) = map_xv_union_over_enum_fast t v in
	let (n,_) = List.nth e k in
	sprintf "union<case=%s>" n

    | Xdr.X_direct(t1, _,_,_) ->
	string_of_abbrev_arg t1 v

    | Xdr.X_refer _
    | Xdr.X_type _
    | Xdr.X_param _ -> 
	assert false

    | Xdr.X_rec(_,t') ->
	string_of_abbrev_arg t' v

let rec string_of_abbrev_args t v =
  match t with
    | Xdr.X_void ->
	""
    | Xdr.X_struct _ ->
	string_of_struct
	  string_of_abbrev_arg
	  t
	  v

    | Xdr.X_direct(t1,_,_,_) ->
	string_of_abbrev_args t1 v

    | _ ->
	string_of_abbrev_arg t v


let rec string_of_full_args t v =
  match t with
    | Xdr.X_void ->
	""
    | Xdr.X_struct _ ->
	string_of_struct
	  string_of_full_arg
	  t
	  v

    | Xdr.X_direct(t1,_,_,_) ->
	string_of_full_args t1 v

    | _ ->
	string_of_full_arg t v



let string_of_request v prog procname args =
  try
    let prognr = Rpc_program.program_number prog in
    let versnr = Rpc_program.version_number prog in
    let (procnr, in_t, _) = Rpc_program.signature prog procname in
    let in_t = Xdr.xdr_type_term in_t in
    let s_args =
      match v with
	| `Name_only -> ""
	| `Name_abbrev_args -> string_of_abbrev_args in_t args
	| `Name_full_args -> string_of_full_args in_t args in
    sprintf
      "%s[0x%lx,0x%lx,0x%lx](%s)"
      procname
      (Rtypes.logical_int32_of_uint4 prognr)
      (Rtypes.logical_int32_of_uint4 versnr)
      (Rtypes.logical_int32_of_uint4 procnr)
      s_args
  with
    | e ->
	sprintf "[Exception in string_of_request: %s]"
	  (Netexn.to_string e)


let string_of_response v prog procname rv =
  try
    let prognr = Rpc_program.program_number prog in
    let versnr = Rpc_program.version_number prog in
    let (procnr, _, out_t) = Rpc_program.signature prog procname in
    let out_t = Xdr.xdr_type_term out_t in
    let s_rv =
      match v with
	| `Name_only -> ""
	| `Name_abbrev_args -> string_of_abbrev_arg out_t rv
	| `Name_full_args -> string_of_full_arg out_t rv in
    sprintf
      "%s[0x%lx,0x%lx,0x%lx] returns %s"
      procname
      (Rtypes.logical_int32_of_uint4 prognr)
      (Rtypes.logical_int32_of_uint4 versnr)
      (Rtypes.logical_int32_of_uint4 procnr)
      s_rv
  with
    | e ->
	sprintf "[Exception in string_of_response: %s]"
	  (Netexn.to_string e)


let string_of_value t xv =
  string_of_full_arg t xv

let hex_dump_m m pos len =
  let b = Buffer.create 100 in
  for k = 0 to len - 1 do
    let c = Bigarray.Array1.get m (pos+k) in
    bprintf b "%02x " (Char.code c)
  done;
  Buffer.contents b


let hex_dump_s s pos len =
  let b = Buffer.create 100 in
  for k = 0 to len - 1 do
    let c = s.[pos+k] in
    bprintf b "%02x " (Char.code c)
  done;
  Buffer.contents b
