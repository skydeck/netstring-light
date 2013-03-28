(* $Id: generate.ml 1702 2012-02-14 21:01:59Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


open Syntax;;
open Format;;


(* Common functions *)


let extract_type_info dl =
  let typenames = Hashtbl.create 100 in
  let typemap = Hashtbl.create 100 in
  List.iter
    (function
	 Typedef td ->
	   if not (Hashtbl.mem typenames td.decl_symbol.xdr_name) then begin
	     (* Only get the first type definition *)
	     Hashtbl.add
	       typenames
	       td.decl_symbol.xdr_name
	       td.decl_symbol.ocaml_name;
	     Hashtbl.add
	       typemap
	       td.decl_symbol.xdr_name
	       td.decl_type
	   end
       | _ -> ()
    )
    dl;
  (typenames,typemap)
;;


let rec get_type_from_map typemap t =
  match t with
      T_refer_to (_,n) ->
	( try get_type_from_map typemap (Hashtbl.find typemap !n)
	  with
	      Not_found -> assert false
	)
    | t -> t
;;


let output_uint4_pattern f (sign,n) =
  assert (not sign);
  let n32 = Netnumber.logical_int32_of_uint4 n in
  fprintf f "(%ldl)" n32
(*
  let (n1,n2,n3,n4) = Netnumber.dest_uint4 n in
  fprintf f "('\\%03d','\\%03d','\\%03d','\\%03d')"
            (Char.code n1)
            (Char.code n2)
            (Char.code n3)
            (Char.code n4);
 *)
;;

let max_int_as_uint4 =
  Netnumber.uint4_of_int32 0x3fff_ffffl  (* assuming 32 bit system *)


let output_uint4 f (sign,n) =
  assert (not sign);
  if Netnumber.le_uint4 n max_int_as_uint4 then
    fprintf f "(Netnumber.uint4_of_int (%d))" (Netnumber.int_of_uint4 n)
  else
    let n32 = Netnumber.logical_int32_of_uint4 n in
    fprintf f "(Netnumber.logical_uint4_of_int32 (%ldl))" n32
;;


let int64_of_const (sign,n) =
  let l = Netnumber.int64_of_uint4 n in
  if sign then Int64.neg l else l


let output_int4_pattern f (sign,n) =
  let n32_0 = Netnumber.int32_of_uint4 n in
  let n32 = if sign then Int32.neg n32_0 else n32_0 in
  fprintf f "(%ldl)" n32
(*
  let plus1 (a,b,c,d) =
    if d < 255 then
      (a,b,c,d+1)
    else
      if c < 255 then
	(a,b,c+1,0)
      else
	if b < 255 then
	  (a,b+1,0,0)
	else
	  if a < 255 then
	    (a+1,0,0,0)
	  else
	    (0,0,0,0)
  in
  let neg (a,b,c,d) =
    plus1 (255-a, 255-b, 255-c, 255-d)
  in
  let (n1,n2,n3,n4) = Netnumber.dest_uint4 n in
  let (m1,m2,m3,m4) =
    if sign then
      neg (Char.code n1, Char.code n2, Char.code n3, Char.code n4)
    else
      (Char.code n1, Char.code n2, Char.code n3, Char.code n4)
  in
  fprintf f "('\\%03d','\\%03d','\\%03d','\\%03d')"
            m1 m2 m3 m4
 *)
;;

let output_int4 f (sign,n) =
  if Netnumber.le_uint4 n max_int_as_uint4 then
    let k0 = Netnumber.int_of_uint4 n in
    let k = if sign then -k0 else k0 in
    fprintf f "(Netnumber.int4_of_int (%d))" k
  else
    let k0 = Netnumber.int32_of_uint4 n in
    let k = if sign then Int32.neg k0 else k0 in
    fprintf f "(Netnumber.int4_of_int32 (%ldl))" k
;;


let strip_enum_list l =
  (* Remove double enums, sort enums *)
  let constants = ref [] in
  let rec strip l =
    match l with
	(id, c) :: l' ->
	  let c' = constant !c in
	  if List.mem c' !constants then
	    strip l'
	  else begin
	    constants := c' :: !constants;
	    (id, c) :: strip l'
	  end
      | [] -> []
  in
  let cmp (id1,c1) (id2,c2) =
    let (sign1,v1) = constant !c1 in
    let (sign2,v2) = constant !c2 in
    match (sign1,sign2) with
	(false,false) ->
	  compare (Netnumber.int32_of_uint4 v1) (Netnumber.int32_of_uint4 v2)
      | (true,true) ->
	  -(compare (Netnumber.int32_of_uint4 v1) (Netnumber.int32_of_uint4 v2))
      | (false, true) ->
	  1
      | (true, false) ->
	  -1
  in
  List.sort cmp (strip l)
;;


let n0 = constant_of_string "0";;
let n1 = constant_of_string "1";;

let enum_type t =
  match t with
      T_enum l -> strip_enum_list l
    | T_bool ->
	[ mk_mapped_id "FALSE" "False", ref(Constant n0);
	  mk_mapped_id "TRUE"  "True",  ref(Constant n1);
	]
    | _ ->
	assert false
;;

let values_of_enum_type t =
  match t with
      T_enum l ->
	List.map (fun (_,c) -> !c) (strip_enum_list l)
    | T_bool ->
	[ Constant n0; Constant n1 ]
    | _ ->
	assert false
;;


(**********************************************************************)
(* Output constant definitions                                        *)
(**********************************************************************)

let output_consts (mli:formatter) (f:formatter) (dl:xdr_def list) =

  let output_signed_const id c =
    (* MLI: *)
    fprintf mli "val %s : Netnumber.int4;;@\n" id.ocaml_name;
    (* ML: *)
    fprintf f "let %s = " id.ocaml_name;
    output_int4 f c;
    fprintf f ";;@\n"
  in

  let output_unsigned_const id c =
    (* MLI: *)
    fprintf mli "val %s : Netnumber.uint4;;@\n" id.ocaml_name;
    (* ML: *)
    fprintf f "let %s = " id.ocaml_name;
    output_uint4 f c;
    fprintf f ";;@\n"
  in

  let rec output_type t = (
    match t with
      | T_option t'         -> output_type t'
      | T_array_fixed(_,t') -> output_type t'
      | T_array(_,t')       -> output_type t'
      | T_array_unlimited t'-> output_type t'
      | T_enum l            -> List.iter
                                 (fun (id,c) ->
				    output_signed_const id (constant !c)
                                 )
	                         (strip_enum_list l)
      | T_struct td         -> List.iter output_type_decl td
      | T_union u           -> output_type_decl (u.discriminant);
                               List.iter (fun (_,_,td) ->
                                            output_type_decl td) u.cases;
                               (match u.default with
                                    Some td -> output_type_decl td
                                  | None    -> ()
                               )
      | _                   -> ()
  )

  and output_type_decl td =
    output_type td.decl_type

  and check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    List.iter (check_procedure prog vers) vers.version_def

  and check_procedure prog vers proc =
    List.iter output_type proc.proc_params;
    output_type proc.proc_result
  in

  fprintf f "@[<v>";
  fprintf mli "@[<v>";

  List.iter
    (function
	 Typedef td ->
	   output_type_decl td
       | Progdef prog ->
	   check_program prog
       | Constdef(id, (sign,c)) ->
	   if sign then
	     output_signed_const id (sign,c)
	   else
	     output_unsigned_const id (sign,c)
    )
    dl;

  fprintf f "@]";
  fprintf mli "@]"
;;


(**********************************************************************)
(* Output O'Caml type declarations                                    *)
(**********************************************************************)

let output_type_declarations (f:formatter) (dl:xdr_def list) =
  let typenames, typemap = extract_type_info dl in
      (* typenames: maps xdr_name to ocaml_name *)
      (* typemap: maps xdr_name to bound type *)
  let anontype = ref 0 in
  let deferred = Queue.create() in
  let firstdecl = ref true in

  let begin_decl() =
    if !firstdecl then
      fprintf f "type "
    else
      fprintf f "and ";
    firstdecl := false
  in

  let get_type t = get_type_from_map typemap t in
  let get_type_of_decl td = get_type td.decl_type in


  let rec output_type t = (
    match t with
	T_opaque_fixed _
      | T_opaque _
      | T_opaque_unlimited
      | T_string _
      | T_string_unlimited ->
	  fprintf f "string"
      | T_mstring(_,_)
      | T_mstring_unlimited _ ->
	  fprintf f "Xdr_mstring.mstring"
      | T_option t' ->
          fprintf f "@[<hv 2>";
	  output_type t';
	  fprintf f "@ option@]"
      | T_array_fixed(_,t') ->
          fprintf f "@[<hv 2>";
	  output_type t';
	  fprintf f "@ array@]"
      | T_array(_,t') ->
          fprintf f "@[<hv 2>";
	  output_type t';
	  fprintf f "@ array@]"
      | T_array_unlimited t' ->
          fprintf f "@[<hv 2>";
	  output_type t';
	  fprintf f "@ array@]"
      | T_int v ->
	  (match v with
	       Abstract-> fprintf f "Netnumber.int4"
	     | INT32   -> fprintf f "int32"
	     | INT64   -> fprintf f "int64"
	     | Unboxed -> fprintf f "int"
	  )
      | T_uint v ->
	  (match v with
	       Abstract-> fprintf f "Netnumber.uint4"
	     | INT32   -> fprintf f "int32"
	     | INT64   -> fprintf f "int64"
	     | Unboxed -> fprintf f "int"
	  )
      | T_hyper v ->
	  (match v with
	       Abstract-> fprintf f "Netnumber.int8"
	     | INT64   -> fprintf f "int64"
	     | Unboxed -> fprintf f "int"
	     | _       -> assert false
	  )
      | T_uhyper v ->
	  (match v with
	       Abstract-> fprintf f "Netnumber.uint8"
	     | INT64   -> fprintf f "int64"
	     | Unboxed -> fprintf f "int"
	     | _       -> assert false
	  )
      | T_double
      | T_float ->
	  fprintf f "float"
      | T_bool ->
	  fprintf f "bool"
      | T_void ->
	  fprintf f "unit"
      | T_refer_to (_,s) ->
	  let n =
	    try Hashtbl.find typenames !s
	    with Not_found -> assert false
	  in
	  fprintf f "%s" n
      | T_enum _ ->
	  fprintf f "Netnumber.int4"
      | T_struct tdl ->
	  let n = "_t" ^ string_of_int !anontype in
	  incr anontype;
	  Queue.add (n,t) deferred;
	  fprintf f "%s" n
      | T_union u ->
	  let discr_type = get_type_of_decl u.discriminant in
	  let make_tag c =
	    let (sign,absval) = constant c in
	    match discr_type with
		(T_int _|T_uint _) ->
		  (if sign then "__" else "_") ^ string_of_uint4 absval
	      | T_bool ->
		  assert(not sign);
		  ( match string_of_uint4 absval with
			"0" -> "False"
		      | "1" -> "True"
		      | _   -> assert false
		  )
	      | T_enum l ->
		  ( try
		      let id,_ =
			List.find
			  (fun (id,n) -> constant !n = (sign,absval))
			  l
		      in
		      id.ocaml_name
		    with
			Not_found -> assert false
		  )
	      | _ ->
		  assert false
	  in
	  let output_tag c om td =
	    let tag =
	      match om with
		  None -> make_tag c
		| Some om_tag -> om_tag
	    in
	    if get_type_of_decl td = T_void then
	      fprintf f "@,| `%s " tag
	    else begin
	      fprintf f "@,| `%s of (" tag;
	      output_type td.decl_type;
	      fprintf f ") "
	    end
	  in

	  fprintf f "@[<hv>[ ";
	  List.iter (fun (c,om,td) -> output_tag !c om td) u.cases;
	  ( match u.default with
		None -> ()         (* TODO: Check! *)
	      | Some td ->
		  (* If the discriminant is countable, the missing cases are
		   * enumerated here. Otherwise, a "default" tag is generated.
		   *)
		  if match discr_type with T_int _ | T_uint _ -> true
		                                          | _ -> false
		  then begin
		    (* The default case is represented by a default tag *)
		    let tag = "default" in
		    fprintf f "@,| `%s of (" tag;
		    if get_type_of_decl td = T_void then
		      output_type u.discriminant.decl_type
		    else begin
		      fprintf f "(";
		      output_type u.discriminant.decl_type;
		      fprintf f ") * (";
		      output_type td.decl_type;
		      fprintf f ")";
		    end;
		    fprintf f ") ";
		  end
		  else begin
		    (* Iterate over all possible values of the discriminant: *)
		    let l = values_of_enum_type discr_type in
		    List.iter
		      (fun n ->
			 (* Find out the missing cases: *)
			 if not (List.exists (fun (c,_,_) -> !c = n)
				             u.cases) then begin
			   (* n is missing! *)
			   output_tag n None td
			 end
		      )
		      l
		  end
	  );
	  fprintf f "@,]@]"
  )

  and output_declaration n t = (
    fprintf f "@[<hov 6>";
    begin_decl();
    fprintf f "%s = @\n" n;
    (match t with
	T_struct tdl ->
	  fprintf f "@[<hov>{ ";
	  List.iter
	    (fun td' ->
	       if td'.decl_symbol.xdr_name <> "" then begin
		 fprintf f "@\n  mutable %s : @[<b 4>@," td'.decl_symbol.ocaml_name;
		 output_type td'.decl_type;
		 fprintf f "@];";
	       end
		 (* else: td' is a void component *)
	    )
	    tdl;
	  fprintf f "@\n}@]";
      | t ->
	  output_type t);
    fprintf f "@]@\n";
  )

  and output_tuple_declaration n args = (
    fprintf f "@[<hov 6>";
    begin_decl();
    fprintf f "%s = @\n" n;
    fprintf f "(@[<hv> ";
    let isfirst = ref true in
    List.iter
      (fun arg ->
	 if not !isfirst then fprintf f " *@ ";
	 isfirst := false;
	 output_type arg;
      )
      args;
    fprintf f " )@]";
    fprintf f "@]@\n";
  )

  and check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    List.iter (check_procedure prog vers) vers.version_def

  and check_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
	      vers.version_symbol.ocaml_name ^ "'" ^
	      proc.proc_symbol.ocaml_name in

    ( match proc.proc_params with
	  [] -> assert false
	| [arg] ->
	    output_declaration
	      ("t_" ^ pvp ^ "'arg")
	      arg
	| args ->
	    output_tuple_declaration
	      ("t_" ^ pvp ^ "'arg")
	      args
    );
    output_declaration
      ("t_" ^ pvp ^ "'res")
      proc.proc_result

  and output_deferred_structs() = (
    try
      while true do
	let (n,t) = Queue.take deferred in
	output_declaration n t
      done
    with
	Queue.Empty -> ()
  )
  in

  fprintf f "@[<v>";

  List.iter
    (function
	 Typedef td ->
	   output_declaration td.decl_symbol.ocaml_name td.decl_type
       | Progdef prog ->
	   check_program prog
       | _ ->
	   ())
    dl;

  output_deferred_structs();

  if not !firstdecl then fprintf f ";;@\n";
  fprintf f "@]";

  (* Now output exceptions for all named types: *)

  if !Options.enable_direct then (
    fprintf f "@[<v>";
    firstdecl := true;
  
    List.iter
      (function
	   Typedef td ->
	     let n = td.decl_symbol.ocaml_name in
	     fprintf f "@[<hov 6>exception X_%s of %s@]@\n" n n;
	     firstdecl := false
	 | _ ->
	     ())
      dl;
    
    if not !firstdecl then fprintf f ";;@\n";
    fprintf f "@]";
  )
;;

(**********************************************************************)
(* Output XDR type definition                                         *)
(**********************************************************************)

let output_xdr_type (mli:formatter) (f:formatter) (dl:xdr_def list) =
  let typenames, typemap = extract_type_info dl in
    (* typenames: maps xdr_name to ocaml_name *)
    (* typemap: maps xdr_name to bound type *)

  let get_type t = get_type_from_map typemap t in
  let get_type_of_decl td = get_type td.decl_type in

  let generated_types = ref [] in

  let rec output_type rectypes direct t = (
    match t with
	T_opaque_fixed n ->
	  fprintf f "@[<hv 2>Xdr.X_opaque_fixed@ ";
	  output_uint4 f (constant !n);
	  fprintf f "@]";
      | T_opaque n ->
	  fprintf f "@[<hv 2>Xdr.X_opaque@ ";
	  output_uint4 f (constant !n);
	  fprintf f "@]";
      | T_opaque_unlimited ->
	  fprintf f "Xdr.x_opaque_max"
      | T_string n ->
	  fprintf f "@[<hv 2>Xdr.X_string@ ";
	  output_uint4 f (constant !n);
	  fprintf f "@]";
      | T_string_unlimited ->
	  fprintf f "Xdr.x_string_max"
      | T_mstring(name,n) ->
	  fprintf f "@[<hv 2>Xdr.X_mstring(@,";
	  fprintf f "%S" name;
	  fprintf f ",@ ";
	  output_uint4 f (constant !n);
	  fprintf f ")@]";
      | T_mstring_unlimited name ->
	  fprintf f "(Xdr.x_mstring_max %S)" name
      | T_option t' ->
	  fprintf f "@[<hv 2>Xdr.x_optional@ (";
	  output_type rectypes false t';
	  fprintf f ")@]";
      | T_array_fixed(n,t') ->
	  fprintf f "@[<hv 2>Xdr.X_array_fixed(@,";
	  output_type rectypes false t';
	  fprintf f ",@ ";
	  output_uint4 f (constant !n);
	  fprintf f ")@]";
      | T_array(n,t') ->
	  fprintf f "@[<hv 2>Xdr.X_array(@,";
	  output_type rectypes false t';
	  fprintf f ",@ ";
	  output_uint4 f (constant !n);
	  fprintf f ")@]";
      | T_array_unlimited t' ->
	  fprintf f "@[<hv>Xdr.x_array_max@ (";
	  output_type rectypes false t';
	  fprintf f ")@]";
      | T_int _ ->
	  fprintf f "Xdr.X_int"
      | T_uint _ ->
	  fprintf f "Xdr.X_uint"
      | T_hyper _ ->
	  fprintf f "Xdr.X_hyper"
      | T_uhyper _ ->
	  fprintf f "Xdr.X_uhyper"
      | T_double ->
	  fprintf f "Xdr.X_double"
      | T_float ->
	  fprintf f "Xdr.X_float"
      | T_bool ->
	  fprintf f "Xdr.x_bool"
      | T_void ->
	  fprintf f "Xdr.X_void"
      | T_refer_to (_,s) ->
	  if List.mem !s !generated_types then begin
	    (* There was already a complete definition for this type *)
	    let n =
	      try Hashtbl.find typenames !s
	      with Not_found -> assert false
	    in
	    fprintf f "xdrt_%s" n
	  end
	  else if List.mem !s rectypes then begin
	    (* There was already the beginning of a definition for this type: *)
	    fprintf f "Xdr.X_refer \"%s\"" !s
	  end
	  else begin
	    let t' = get_type t in
	    fprintf f "@[<hv 2>Xdr.X_rec(\"%s\",@ " !s;
	    if direct then
	      fprintf f "@[<hv 2>Xdr.X_direct(";
	    output_type (!s :: rectypes) false t';
	    if direct then
	      fprintf f ",@ _read_%s,@ _write_%s,@ _size_%s)@]" !s !s !s;
	    fprintf f ")@]";
	  end
      | T_enum l ->
	  fprintf f "@[<hv 2>Xdr.X_enum@ [@ ";
	  List.iter
	    (fun (id,c) ->
	       fprintf f "  (\"%s\", " id.xdr_name;
	       output_int4 f (constant !c);
	       fprintf f ");@ ";
	    )
	    (strip_enum_list l);
	  fprintf f "]@]";
      | T_struct tdl ->
	  fprintf f "@[<hv 2>Xdr.X_struct@ @[<hv>[@ ";
	  List.iter
	    (fun d ->
	       if d.decl_type <> T_void then begin
		 fprintf f "  @[<hv 2>(\"%s\",@ (" d.decl_symbol.xdr_name;
		 output_type rectypes false d.decl_type;
		 fprintf f "));@]@ ";
	       end
	    )
	    tdl;
	  fprintf f "]@]@]";
      | T_union u ->
	    let discr_type = get_type_of_decl u.discriminant in
	    if match discr_type with T_int _ | T_uint _ -> true
                                                    | _ -> false
	    then begin
	      (* Unions of integers *)
	      let constr, printint =
		match discr_type with
		    T_int _  -> "Xdr.X_union_over_int",  output_int4
		  | T_uint _ -> "Xdr.X_union_over_uint", output_uint4
		  | _        -> assert false
	      in
	      fprintf f "@[<hv 2>";
	      fprintf f "%s(" constr;
	      fprintf f "@[<hv>[@ ";
	      List.iter
		(fun (c, _, d) ->
		   fprintf f "  @[<hv 2>";
		   printint f (constant !c);
		   fprintf f ",@ (";
		   output_type rectypes false d.decl_type;
		   fprintf f ");@]@ ";
		)
		u.cases;
	      fprintf f "],@ ";
	      begin match u.default with
		  None ->
		    fprintf f "None"
		| Some d ->
		    fprintf f "Some(";
		    output_type rectypes false d.decl_type;
		    fprintf f ")"
	      end;
	      fprintf f ")@]@]";
	    end
	    else begin
	      (* Unions of enumerators (and bools) *)
	      fprintf f "@[<hv 2>";
	      fprintf f "Xdr.X_union_over_enum(@,(";
	      output_type rectypes false discr_type;
	      fprintf f "),@ [@ ";
	      let l = enum_type discr_type in
	      List.iter
		(fun (c, _, d) ->
		   let name, _ =
		     try
		       List.find
			 (fun (id, c') -> !c' = !c)
			 l
		     with Not_found -> assert false
		   in
		   fprintf f "  @[<hv 2>\"%s\"" name.xdr_name;
		   fprintf f ",@ (";
		   output_type rectypes false d.decl_type;
		   fprintf f ");@]@ ";
		)
		u.cases;
    	      fprintf f "],@ ";
	      begin match u.default with
		  None ->
		    fprintf f "None"
		| Some d ->
		    fprintf f "Some(";
		    output_type rectypes false d.decl_type;
		    fprintf f ")"
	      end;
	      fprintf f ")@]@]";
	    end
  )

  and output_xdr_declaration n direct t =
    (* MLI: *)
    fprintf mli "val %s : Xdr.xdr_type_term;;@\n" n;
    (* ML: *)
    fprintf f "@[<hv 2>let %s =@ " n;
    (* fprintf f "@[<hv 2>Xdr.validate_xdr_type@ ("; *)
    output_type [] direct t;
    (* fprintf f ")@]"; *)
    fprintf f "@]@\n;;@\n"

  and output_xdr_tuple_declaration n tl =
    (* MLI: *)
    fprintf mli "val %s : Xdr.xdr_type_term;;@\n" n;
    (* ML: *)
    fprintf f "@[<hv 2>let %s =@ " n;
    (* fprintf f "@[<hv 2>Xdr.validate_xdr_type@ ("; *)
    fprintf f "@[<hv 2>Xdr.X_struct@ @[<hv>[@ ";
    let k = ref 0 in
    List.iter
      (fun t ->
	 fprintf f "  (\"%s\", " (string_of_int !k);
	 output_type [] false t;
	 fprintf f ");@ ";
	 incr k;
      )
      tl;
    fprintf f "]@]@]";
    (* fprintf f ")@]"; *)
    fprintf f "@]@\n;;@\n";

  and check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    List.iter (check_procedure prog vers) vers.version_def

  and check_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
	      vers.version_symbol.ocaml_name ^ "'" ^
	      proc.proc_symbol.ocaml_name in

    ( match proc.proc_params with
	  [] -> assert false
	| [arg] ->
	    output_xdr_declaration
	      ("xdrt_" ^ pvp ^ "'arg")
	      false
	      arg
	| args ->
	    output_xdr_tuple_declaration
	      ("xdrt_" ^ pvp ^ "'arg")
	      args
    );
    output_xdr_declaration
      ("xdrt_" ^ pvp ^ "'res")
      false
      proc.proc_result
  in

  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
	 Typedef td ->
	   output_xdr_declaration
	     ("xdrt_" ^ td.decl_symbol.ocaml_name)
	     (!Options.enable_direct && td.decl_direct)
	     (T_refer_to (R_any, ref td.decl_symbol.xdr_name));
	   generated_types := td.decl_symbol.xdr_name :: !generated_types
       | Progdef prog ->
	   check_program prog
       | _ ->
	   ())
    dl;

  fprintf mli "@]";
  fprintf f "@]"
;;


(**********************************************************************)
(* Get min size                                                       *)
(**********************************************************************)

(* Simplified so far. Look also into Xdr.calc_min_size *)

let calc_min_size dl =
  let (typenames,typemap) = extract_type_info dl in

  let visiting = Hashtbl.create 15 in
  let visited = Hashtbl.create 15 in

  fun t ->

    let ( ++ ) x y =
      (* pre: x >= 0 && y >= 0 *)
      let s = x + y in
      if s < 0 then (* can only happen on 32 bit platforms *)
	failwith "Minimum size of type exceeds limit";
      s in

    let rec calc t =
      match t with
	  T_int _    -> 4
	| T_uint _   -> 4
	| T_hyper _  -> 8
	| T_uhyper _ -> 8
	| T_float    -> 4
	| T_double   -> 8
	| T_void     -> 0
	| T_bool     -> 4
	| T_enum _   -> 4
	| T_opaque_fixed c -> 
	    let nL = int64_of_const (constant !c) in
	    if nL=0L then 0 
	    else Int64.to_int(Int64.succ (Int64.div (Int64.pred nL) 4L))
	| T_opaque _ -> 4
	| T_opaque_unlimited -> 4
	| T_string _ -> 4
	| T_string_unlimited -> 4
	| T_mstring _ -> 4
	| T_mstring_unlimited _ -> 4
	| T_option t' -> 4
	| T_array_fixed (c,t') -> 
	    let size = calc t' in
	    if size = 0 then
	      failwith "Array elements must not have length 0";
	    let nL = int64_of_const(constant !c) in
	    let n_max = max_int / size in
	    if nL > Int64.of_int n_max then
	      failwith "Minimum size of type exceeds limit";
	    let iL = Int64.of_int size in
	    Int64.to_int (Int64.mul nL iL)
	| T_array (_,t')
	| T_array_unlimited t' -> 
	    let size = calc t' in
	    if size = 0 then
	      failwith "Array elements must not have length 0";
	    4
	| T_struct s ->
	    List.fold_left
	      (fun acc td ->
		 acc ++ calc td.decl_type
	      )
	      0
	      s
	| T_union u ->
	    let l =
	      ( match u.default with
		  | None -> []
		  | Some d -> [d]
	      ) @ (List.map (fun (_,_,d) -> d) u.cases) in
	    assert(l <> []);
	    4 ++
	      (List.fold_left
		 (fun acc d ->
		    min acc (calc d.decl_type)
		 )
		 (calc (List.hd l).decl_type)
		 (List.tl l)
	      )
	| T_refer_to (_,r) ->
	    ( try
		Hashtbl.find visited !r
	      with
		| Not_found ->
		    if Hashtbl.mem visiting !r then
		      0
		    else (
		      Hashtbl.add visiting !r ();
		      let t =
			try Hashtbl.find typemap !r
			with Not_found -> assert false in
		      let size = calc t in
		      Hashtbl.add visited !r size;
		      Hashtbl.remove visiting !r;
		      size
		    )
	    )
    in
    calc t

(**********************************************************************)
(* Helpers for ints                                                   *)
(**********************************************************************)

let conversion_custom_int_of_netnumber t =
  match t with
    | T_int Abstract     -> ""
    | T_int INT32        -> "Netnumber.int32_of_int4"
    | T_int INT64        -> "Netnumber.int64_of_int4"
    | T_int Unboxed      -> "Netnumber.int_of_int4"
    | T_uint Abstract    -> ""
    | T_uint INT32       -> "Netnumber.logical_int32_of_uint4"
    | T_uint INT64       -> "Netnumber.int64_of_uint4"
    | T_uint Unboxed     -> "Netnumber.int_of_uint4"
    | T_hyper Abstract   -> ""
    | T_hyper INT32      -> assert false
    | T_hyper INT64      -> "Netnumber.int64_of_int8"
    | T_hyper Unboxed    -> "Netnumber.int_of_int8"
    | T_uhyper Abstract  -> ""
    | T_uhyper INT32     -> assert false
    | T_uhyper INT64     -> "Netnumber.logical_int64_of_uint8"
    | T_uhyper Unboxed   -> "Netnumber.int_of_uint8"
    | _ -> assert false


let conversion_netnumber_of_custom_int t =
  match t with
    | T_int Abstract     -> ""
    | T_int INT32        -> "Netnumber.int4_of_int32"
    | T_int INT64        -> "Netnumber.int4_of_int64"
    | T_int Unboxed      -> "Netnumber.int4_of_int"
    | T_uint Abstract    -> ""
    | T_uint INT32       -> "Netnumber.logical_uint4_of_int32"
    | T_uint INT64       -> "Netnumber.uint4_of_int64"
    | T_uint Unboxed     -> "Netnumber.uint4_of_int"
    | T_hyper Abstract   -> ""
    | T_hyper INT32      -> assert false
    | T_hyper INT64      -> "Netnumber.int8_of_int64"
    | T_hyper Unboxed    -> "Netnumber.int8_of_int"
    | T_uhyper Abstract  -> ""
    | T_uhyper INT32     -> assert false
    | T_uhyper INT64     -> "Netnumber.logical_uint8_of_int64"
    | T_uhyper Unboxed   -> "Netnumber.uint8_of_int"
    | _ -> assert false


let name_of_int t =
  match t with
    | T_int _    -> "int"
    | T_uint _   -> "uint"
    | T_hyper _  -> "hyper"
    | T_uhyper _ -> "uhyper"
    | _ -> assert false

let xv_name_of_int t =
  match t with
    | T_int _    -> "XV_int"
    | T_uint _   -> "XV_uint"
    | T_hyper _  -> "XV_hyper"
    | T_uhyper _ -> "XV_uhyper"
    | _ -> assert false

let netnumber_name_of_int t =
  match t with
    | T_int _    -> "int4"
    | T_uint _   -> "uint4"
    | T_hyper _  -> "int8"
    | T_uhyper _ -> "uint8"
    | _ -> assert false


let size_of_int t =
  match t with
    | T_int _    -> 4
    | T_uint _   -> 4
    | T_hyper _  -> 8
    | T_uhyper _ -> 8
    | _ -> assert false



let output_any_int f t (sign,n) =  (* hyper, uhyper not needed *)
  match t with
    | T_int _    -> output_int4 f (sign,n)
    | T_uint _   -> output_uint4 f (sign,n)
    | _ -> assert false



let conversion_int32_of_discr discr_type =
  match discr_type with
    | T_int _ -> "Netnumber.int32_of_int4"
    | T_uint _ -> "Netnumber.logical_int32_of_uint4"
    | _ -> assert false


(**********************************************************************)
(* Generators for unions                                              *)
(**********************************************************************)

let have_enum_default_with_arg u get_type_of_decl  =
  let discr_type = get_type_of_decl u.discriminant in
  match discr_type with
    | T_int _ | T_uint _ -> false
    | T_enum _ | T_bool ->
	( match u.default with
	    | None ->
		false
	    | Some d ->
		get_type_of_decl d <> T_void
	)
    | _ -> assert false


let output_match_union_by_cases f u var get_type_of_decl 
                                f_case f_default f_let =
  (* Outputs a "match" statement over the Ocaml variants. The variable var
     is matched. For every variant of var the function f_case is called:
     
     f_case k (sign,n) decl have_x is_default
     
     with
     - k: the k-th case (for unions over enums: the k-th enum variant)
     - (sign,n): the corresponding XDR value
     - decl: the xdr_decl of this case
     - have_x: whether there is a generated "x" variable
     - is_default: whether this is a default case (only for enum unions)
     
     At this moment, a branch like
     
     | `tag -> OR
     | `tag x ->
     
     has already been generated.
     
     For unions over ints/uints, the function f_default can be called
     for the default case. It is called as
     
     f_default decl have_x
     
     At this moment, a branch like
     
     | `default(discriminant,x) -> OR
     | `default(discriminant) -> OR
     
     has already been generated.
     
     The function f_let can be used to generate "let v = ... in ..."
     statements.
   *)
  let discr_type = get_type_of_decl u.discriminant in
  match discr_type with
    | T_int _ | T_uint _ ->
	fprintf f "@[<v 2>";
	fprintf f "( ";
	f_let();
	fprintf f "match %s with" var;
	let k = ref 0 in
	List.iter
	  (fun (c,om,d) ->
	     let (sign,n) = constant !c in
	     let tag =
	       match om with
		   None ->
		     (if sign then "__" else "_") ^ string_of_uint4 n
		 | Some om_tag -> om_tag
	     in
	     fprintf f "@ @[<hv 6>| `%s " tag;
	     let have_x = get_type_of_decl d <> T_void in
	     if have_x then fprintf f "x ";
	     fprintf f "->@ ";
	     f_case !k (sign,n) d have_x false;
	     fprintf f "@]";
	     incr k;
	  )
	  u.cases;
	( match u.default with
	    | None ->
		()
	    | Some d ->
		fprintf f "@ @[<hv 6>| ";
		if get_type_of_decl d <> T_void then (
		  fprintf f "`default(discriminant,x) ->@ ";
		  f_default d true
		)
		else (
		  fprintf f "`default discriminant ->@ ";
		  f_default d false
		);
		fprintf f "@]";
	);
	fprintf f "@]@ )";
	
    | T_enum _
    | T_bool ->
	fprintf f "( @[<v>";
	f_let();
	fprintf f "match %s with" var;
	let l = enum_type discr_type in
	let k = ref 0 in
	List.iter
	  (fun (id,c) ->
	     let (sign,n) = constant !c in
	     let om, d_opt, d_is_default =
	       try
		 let _, om, d =
		   (List.find
		      (fun (c',_,d') -> !c' = !c)
		      u.cases
		   )
		 in
		 om, Some d, false
	       with Not_found -> None, u.default, true in
	     match d_opt with
	       | Some d ->
		   let tag = 
		     match om with
		       | None -> id.ocaml_name
		       | Some om_tag -> om_tag in
		   fprintf f "@ @[<hv 6>";
		   fprintf f "| `%s " tag;
		   let have_x = get_type_of_decl d <> T_void in
		   if have_x then fprintf f "x ";
		   fprintf f "->@ ";
		   f_case !k (sign,n) d have_x d_is_default;
		   fprintf f "@]";
		   incr k
	       | None ->
		   incr k		     
	  )
	  l;
	fprintf f "@]@ )"
	  
    | _ ->
	assert false



let output_match_union_by_number f u var by_k get_type_of_decl
                                 f_case f_default f_let f_coerce =
  let discr_type = get_type_of_decl u.discriminant in
  match discr_type with
    | T_int _ | T_uint _ ->
	fprintf f "@[<v 2>( ";
	f_let();
	fprintf f "match %s with" var;
	let printint_pattern =
	  match discr_type with
	    | T_int _ -> output_int4_pattern
	    | T_uint _ -> output_uint4_pattern
	    | _ -> assert false in
	let k = ref 0 in
	List.iter
	  (fun (c, om, d) ->
	     let (sign,n) = constant !c in
	     let tag =
	       match om with
		   None ->
		     (if sign then "__" else "_") ^ string_of_uint4 n
		 | Some om_tag -> om_tag in
	     fprintf f "@ @[<hv 4>";
	     fprintf f "| ";
	     if by_k then
	       fprintf f "%d" !k
	     else
	       printint_pattern f (sign,n);
	     fprintf f " ->@ ";
	     f_case !k (sign,n) tag d false;
	     fprintf f "@]";
	     incr k
	  )
	  u.cases;
	( match u.default with
	    | None ->
		fprintf f
		  "@ | _ -> Xdr.raise_xdr_format_undefined_descriminator()"
	    | Some d ->
		fprintf f "@ @[<hv 4>";
		fprintf f "| discriminant ->@ ";
		f_default d;
		fprintf f "@]"
	);
	f_coerce();
	fprintf f "@]@ )"

    | T_enum _
    | T_bool ->
	fprintf f "@[<v 2>( ";
	f_let();
	fprintf f "match %s with" var;
	let l = enum_type discr_type in
	let k = ref 0 in
	List.iter
	  (fun (id,c) ->
	     let (sign,n) = constant !c in
	     let om, d_opt, d_is_default =
	       try
		 let _, om, d =
		   (List.find
		      (fun (c',_,d') -> !c' = !c)
		      u.cases
		   )
		 in
		 om, Some d, false
	       with Not_found -> None, u.default, true in
	     match d_opt with
	       | Some d ->
		   let tag = 
		     match om with
		       | None -> id.ocaml_name
		       | Some om_tag -> om_tag in
		   fprintf f "@ @[<hv 6>";
		   fprintf f "| ";
		   if by_k then
		     fprintf f "%d" !k
		   else
		     output_int4_pattern f (sign,n);
		   fprintf f " ->@ ";
		   f_case !k (sign,n) tag d d_is_default;
		   fprintf f "@]";
		   incr k
	       | None ->
		   incr k		     
	  )
	  l;
	fprintf f
	  "@ | _ -> Xdr.raise_xdr_format_undefined_descriminator()";
	f_coerce();
	fprintf f "@]@ )"

    | _ ->
	assert false


let output_coerce_pattern f u get_type_of_decl =
  (* outputs a type pattern to which values of [u] can be coerced to *)
  let discr_type = get_type_of_decl u.discriminant in
  match discr_type with
    | T_int _ | T_uint _ ->
	fprintf f "@[<hv>[";
	List.iter
	  (fun (c, om, d) ->
	     let (sign,n) = constant !c in
	     let tag =
	       match om with
		 | None ->
		     (if sign then "__" else "_") ^ string_of_uint4 n
		 | Some om_tag -> om_tag
	     in
	     fprintf f "@ | `%s" tag;
	     if get_type_of_decl d <> T_void then
	       fprintf f " of _";
	  )
	  u.cases;
	begin match u.default with
	    None ->
	      ()
	  | Some d ->
	      fprintf f "@ | `default of ";
	      if get_type_of_decl d <> T_void then
		fprintf f "(_ * _)"
	      else
		fprintf f "_"
	end;
	fprintf f "@ ]@]"

    | T_enum _
    | T_bool ->
	fprintf f "@[<hv>[";
	let l = enum_type discr_type in
	List.iter
	  (fun (id, c) ->
	     ( try
		 let _, om, d =
		   (List.find
		      (fun (c',_,d') -> !c' = !c)
		      u.cases
		   )
		 in
		 let tag = match om with
		   | None -> id.ocaml_name
		   | Some om_tag -> om_tag
		 in
		 fprintf f "@ | `%s" tag;
		 if get_type_of_decl d <> T_void then
		   fprintf f " of _";
	       with
		   Not_found ->
		     match u.default with
		       | None -> ()
		       | Some d ->
			   fprintf f "@ | `%s" id.ocaml_name;
			   if get_type_of_decl d <> T_void then
			     fprintf f " of _"
	     )
	  )
	  l;
	fprintf f "@ ]@]"

    | _ ->
	assert false 

(**********************************************************************)
(* Output conversion functions                                        *)
(**********************************************************************)

let output_conversions (mli:formatter) (f:formatter) (dl:xdr_def list) =

  (* Names of conversions:
   * - For every named type t there are two conversion functions:
   *   _to_<t> : value -> t
   *   _of_<t> : t -> value
   * - For every procedure argument and procedure result, there are
   *   such functions, too:
   *   _to_<prog>'<vers>'<proc>'arg
   *   _to_<prog>'<vers>'<proc>'res
   *   _of_<prog>'<vers>'<proc>'arg
   *   _of_<prog>'<vers>'<proc>'res
   *   Here, <prog>, <vers>, and <proc> are the names of the program, the
   *   version, and the procedure, resp. The character ' is used as
   *   delimiter
   *
   * Helpers for direct mapping (not exported, and only for certain t):
   * - _size_<t> : t -> int
   *   computes the byte size of the XDR representation for t
   * - _write_<t> : t -> string -> int -> unit
   *   writes the XDR representation to a string at a position
   *)

  let typenames, typemap = extract_type_info dl in
      (* typenames: maps xdr_name to ocaml_name *)
      (* typemap: maps xdr_name to bound type *)

  let get_type t = get_type_from_map typemap t in
  let get_type_of_decl td = get_type td.decl_type in
  let min_size = calc_min_size dl in

  let generate_direct_case direct_opt =
    match direct_opt with
      | None -> ()
      | Some n -> 
	  fprintf f "| Xdr.XV_direct(X_%s x, _) -> x@ " n in

  let generate_dest direct_opt var regname regconv =
    fprintf f "@[<hv 2>( match %s with@ " var;
    fprintf f "| Xdr.%s x -> %sx@ " 
      regname (if regconv <> "" then regconv ^ " " else "");
    generate_direct_case direct_opt;
    fprintf f "| _ -> raise Xdr.Dest_failure";
    fprintf f "@]@ )" in


  let rec output_toconv_for_type (var:string) (t:xdr_type) direct_opt = (
    (* Generates an expression that converts the xdr_value variable var
     * into the O'Caml value corresponding to t
     *)
    fprintf f "@[<hv>";

    ( match t with
	| T_void ->
	    fprintf f "()"
	| T_opaque_fixed _
	| T_opaque _
	| T_opaque_unlimited ->
	    generate_dest direct_opt var "XV_opaque" ""
	| T_string _
	| T_string_unlimited ->
	    generate_dest direct_opt var "XV_string" ""
	| T_mstring(_,_)
	| T_mstring_unlimited _ ->
	    generate_dest direct_opt var "XV_mstring" ""
	| T_option t' ->
	    fprintf f "@[<hv>";
	    fprintf f "( match %s with@ " var;
	    fprintf f "| Xdr.XV_union_over_enum_fast (0, _) -> None@ ";
	    fprintf f "@[<hv 4>| Xdr.XV_union_over_enum_fast (1, x) ->@ Some ";
	    output_toconv_for_type "x" t' None;
	    fprintf f "@]";
	    fprintf f "@ ";
	    generate_direct_case direct_opt;
	    fprintf f "| _ -> raise Xdr.Dest_failure@]@ )";
	| T_array_fixed(_,t') ->
	    output_toconv_for_array var t' direct_opt
	| T_array(_,t') ->
	    output_toconv_for_array var t' direct_opt
	| T_array_unlimited t' ->
	    output_toconv_for_array var t' direct_opt
	| T_int _
	| T_uint _
	| T_hyper _
	| T_uhyper _ ->
	    let xv_name = xv_name_of_int t in
	    let conv = conversion_custom_int_of_netnumber t in
	    generate_dest direct_opt var xv_name conv
	| T_double ->
	    generate_dest direct_opt var "XV_double" "Netnumber.float_of_fp8"
	| T_float ->
	    generate_dest direct_opt var "XV_float" "Netnumber.float_of_fp4"
	| T_bool ->
	    generate_dest direct_opt var "XV_enum_fast" "(fun b -> b=1)"
	| T_refer_to (_,n) ->
	    let ocaml_n =
	      try Hashtbl.find typenames !n
	      with Not_found -> assert false
	    in
	    fprintf f "(_to_%s %s)" ocaml_n var
	| T_enum l ->
	    fprintf f "@[<hv 2>";
	    fprintf f "( match %s with@ " var;
	    let k = ref 0 in
	    List.iter
	      (fun (id,c) ->
		 fprintf f "@[<hv>";
		 fprintf f "| Xdr.XV_enum_fast %d ->@;<1 4>" !k;
		 output_int4 f (constant !c);
		 fprintf f "@]";
		 fprintf f "@ ";
		 incr k
	      )
	      (strip_enum_list l);
	    generate_direct_case direct_opt;
	    fprintf f "| _ -> raise Xdr.Dest_failure@ ";
	    fprintf f "@]@ )";
	| T_struct tl ->
	    fprintf f "@[<hv 2>";
	    fprintf f "( let f s =@ ";
	    fprintf f "  @[<hv>{ @[<hv>";
	    let isfirst = ref true in
	    let k = ref 0 in
	    List.iter
	      (fun d ->
		 if d.decl_type <> T_void then begin
		   if not !isfirst then fprintf f "@ ";
		   isfirst:= false;
		   let ocaml_n = d.decl_symbol.ocaml_name in
		   (* let xdr_n   = d.decl_symbol.xdr_name in *)
		   fprintf f "%s = " ocaml_n;
		   fprintf f "@[<hv>";
		   fprintf f "(fun x -> ";
		   output_toconv_for_type "x" d.decl_type None;
		   fprintf f ")@ s.(%d)" !k;
		   fprintf f "@]";
		   fprintf f "; ";
		   incr k
		 end
	      )
	      tl;
	    fprintf f "@]@ }@] in@ ";
	    generate_dest direct_opt var "XV_struct_fast" "f";
	    fprintf f "@]@ )"
	| T_union u ->
	    let check_direct = direct_opt <> None in
	    if check_direct then (
	      fprintf f "@[<hv 2>( match %s with@ " var;
	      generate_direct_case direct_opt;
	      fprintf f "| _ ->@ ";
	    );
	    let discr_type = get_type_of_decl u.discriminant in
	    if match discr_type with T_int _ | T_uint _ -> true
	                                            | _ -> false
	    then begin
	      (* Unions of integers *)
	      output_match_union_by_number
		f
		u
		"discriminant"
		false  (* by_k *)
		get_type_of_decl
		(fun k (sign,n) tag d d_is_default ->
		   (* f_case *)
		   fprintf f "`%s " tag;
		   if get_type_of_decl d <> T_void then
		     output_toconv_for_type "x" d.decl_type None;
		)
		(fun d ->
		   (* f_default *)
		    let int_conversion =
		      conversion_custom_int_of_netnumber discr_type in
		    fprintf f "`default(@[<hv>%s discriminant0" int_conversion;
		    if get_type_of_decl d <> T_void then begin
		      fprintf f ",@ ";
		      output_toconv_for_type "x" d.decl_type None;
		    end;
		    fprintf f "@])";
		)
		(fun () ->
		   (* f_let *)
		   fprintf f "let discriminant0, x = %s %s in@ "
		     (match discr_type with
			| T_int _  -> "Xdr.dest_xv_union_over_int"
			| T_uint _ -> "Xdr.dest_xv_union_over_uint"
			| _ -> assert false
		     )
		     var;
		   fprintf f "let discriminant = %s discriminant0 in@ "
		     (conversion_int32_of_discr discr_type)
		)
		(fun () ->
		   (* f_coerce *)
		   fprintf f "@ :> ";
		   output_coerce_pattern f u get_type_of_decl
		)
	    end
	    else begin
	      (* Unions of enumerators (and bools) *)
	      let have_mkdefault = 
		have_enum_default_with_arg u get_type_of_decl in
	      output_match_union_by_number
		f
		u
		"k"
		true  (* by_k *)
		get_type_of_decl
	      	(fun k (sign,n) tag d d_is_default ->
		   (* f_case *)
		   fprintf f "`%s " tag;
		   if d_is_default then (
		     if have_mkdefault then
		       fprintf f "(mkdefault x)"
		   ) else (
		     if get_type_of_decl d <> T_void then
		       output_toconv_for_type "x" d.decl_type None;
		   )
		)
		(fun d -> assert false)
		(fun () ->
		   (* f_let *)
		   fprintf f 
		     "let k, x = Xdr.dest_xv_union_over_enum_fast %s in@ "
		     var;
		   ( match u.default with
			 None ->
			   ()
		       | Some d ->
			   if have_mkdefault then begin
			     fprintf f "let mkdefault x =@;<1 4>";
			     fprintf f "@[<hv>";
			     output_toconv_for_type "x" d.decl_type None;
			     fprintf f "@]";
			     fprintf f " in@ ";
			   end
		   )
		)
		(fun () ->
		   (* f_coerce *)
		   fprintf f "@ :> ";
		   output_coerce_pattern f u get_type_of_decl
		)
	    end;
	    if check_direct then
	      fprintf f "@]@ )"
    );
    fprintf f "@]"
  )

  and output_toconv_for_array var t' direct_opt =
    let t1 = get_type_from_map typemap t' in
    fprintf f "@[<hv 2>";
    fprintf f "( match %s with@ " var;
    fprintf f "@[<hv 4>| Xdr.XV_array x ->@ ";
    fprintf f "@[<hv 2>Array.map@ ";
    fprintf f "@[<hv 2>(fun x -> ";
    output_toconv_for_type "x" t' None;
    fprintf f ")@]@ x@]@]@ ";
    ( match t1 with
	| T_string _
	| T_string_unlimited ->
	    fprintf f "@[<hv 4>| Xdr.XV_array_of_string_fast x ->@ ";
	    fprintf f "x@]@ ";
	| _ -> ()
    );
    generate_direct_case direct_opt;
    fprintf f "| _ -> raise Xdr.Dest_failure";
    fprintf f "@]@ )";

  and output_toconv_for_tuple var tl =
    fprintf f "@[<hv>";
    fprintf f "(let s = Xdr.dest_xv_struct_fast %s in@;<1 3>" var;
    fprintf f "( @[<hv>";
    let isfirst = ref true in
    let n = ref 0 in
    List.iter
      (fun t ->
	 if not !isfirst then fprintf f ", @,";
	 isfirst:= false;
	 fprintf f "@[<hv>";
	 fprintf f "(fun x -> ";
	 output_toconv_for_type "x" t None;
	 fprintf f ")@ s.(%d)" !n;
	 fprintf f "@]";
	 incr n;
      )
      tl;
    fprintf f "@]@;<0 3>))";
    fprintf f "@]";
  in

  let firstdecl = ref true in

  let begin_decl() =
    if !firstdecl then
      fprintf f "let rec "
    else
      fprintf f "and ";
    firstdecl := false
  in

  let output_toconv_declaration n t tname direct =
    (* MLI: *)
    fprintf mli "val _to_%s : Xdr.xdr_value -> %s;;@\n" n tname;
    (* ML: *)
    fprintf f "@[<hv>";
    begin_decl();
    fprintf f "_to_%s (x:Xdr.xdr_value) : %s =@;<1 2>"
      n
      tname;
    let direct_opt =
      if !Options.enable_direct && direct then
	Some tname
      else
	None in
    output_toconv_for_type "x" t direct_opt;
    fprintf f "@]@\n"
  in

  let output_toconv_tuple_declaration n tl tname =
    (* MLI: *)
    fprintf mli "val _to_%s : Xdr.xdr_value -> %s;;@\n" n tname;
    (* ML: *)
    fprintf f "@[<hv>";
    begin_decl();
    fprintf f "_to_%s (x:Xdr.xdr_value) : %s =@;<1 2>"
      n
      tname;
    output_toconv_for_tuple "x" tl;
    fprintf f "@]@\n"
  in

  let rec output_ofconv_for_type (name:string) (var:string) (t:xdr_type) =
    (* Generates an expression converting the O'Caml value contained in the
     * variable with name var to the corresponding XDR value
     *)
    fprintf f "@[<hv>";
    ( match t with
	| T_void ->
	    fprintf f "Xdr.XV_void"
	| T_opaque_fixed _
	| T_opaque _
	| T_opaque_unlimited ->
	    fprintf f "(Xdr.XV_opaque %s)" var
	| T_string _
	| T_string_unlimited ->
	    fprintf f "(Xdr.XV_string %s)" var
	| T_mstring(_,_)
	| T_mstring_unlimited _ ->
	    fprintf f "(Xdr.XV_mstring %s)" var
	| T_option t' ->
	    fprintf f "@[<hv 2>";
	    fprintf f "( match %s with@ " var;
	    fprintf f "| None   -> Xdr.xv_none@ ";
	    fprintf f "| Some x -> @[<hv 2>Xdr.xv_some@ ";
	    output_ofconv_for_type name "x" t';
	    fprintf f "@]@]@ )";
	| T_array_fixed(_,t') ->
	    output_ofconv_for_array name  var t'
	| T_array(_,t') ->
	    output_ofconv_for_array name var t'
	| T_array_unlimited t' ->
	    output_ofconv_for_array name var t'
	| T_int _ 
	| T_uint _
	| T_hyper _
	| T_uhyper _ ->
	    let xv_name = xv_name_of_int t in
	    let conv = conversion_netnumber_of_custom_int t in
	    fprintf f "(Xdr.%s (%s %s))" xv_name conv var
	| T_double ->
	    fprintf f "(Xdr.XV_double (Netnumber.fp8_of_float %s))" var
	| T_float ->
	    fprintf f "(Xdr.XV_float (Netnumber.fp4_of_float %s))" var
	| T_bool ->
	    fprintf f "(if %s then Xdr.xv_true else Xdr.xv_false)" var
	| T_refer_to (_,n) ->
	    let ocaml_n =
	      try Hashtbl.find typenames !n
	      with Not_found -> assert false
	    in
	    fprintf f "(_of_%s %s)" ocaml_n var
	| T_enum l ->
	    fprintf f "@[<hv>";
	    fprintf f "(match Netnumber.int32_of_int4 %s with@ " var;
	    let k = ref 0 in
	    List.iter
	      (fun (id,c) ->
		 fprintf f "@[<hv>";
		 fprintf f "| ";
		 output_int4_pattern f (constant !c);
		 fprintf f "@ -> Xdr.XV_enum_fast %d" !k;
		 fprintf f "@]";
		 fprintf f "@ ";
		 incr k;
	      )
	      (strip_enum_list l);
	    fprintf f "| _ -> failwith \"RPC/XDR error: invalid enum value for type `%s'\"@ " name;
	    fprintf f ")";
	    fprintf f "@]";
	| T_struct tdl ->
	    fprintf f "@[<hv>(@[<hv 2>Xdr.XV_struct_fast@ ";
	    fprintf f "[|@ ";
	    List.iter
	      (fun d ->
		 if d.decl_type <> T_void then begin
		   let ocaml_n = d.decl_symbol.ocaml_name in
		   let _xdr_n   = d.decl_symbol.xdr_name in
		   fprintf f "  @[<hv 2>(";
		   fprintf f "let x = %s.%s in@ " var ocaml_n;
		   output_ofconv_for_type name "x" d.decl_type;
		   fprintf f ")@];@ ";
		 end
	      )
	      tdl;
	    fprintf f "|]@])@]"
	| T_union u ->
	    let discr_type = get_type_of_decl u.discriminant in
	    let have_mkdefault =
	      have_enum_default_with_arg u get_type_of_decl in
	    output_match_union_by_cases
	      f
	      u
	      var
	      get_type_of_decl
	      (fun k (sign,n) d have_x is_default ->
		 (* f_case *)
		 match discr_type with
		   | T_int _ | T_uint _ ->
		       fprintf f "Xdr.XV_union_over_%s(@[<hv>"
			 (name_of_int discr_type);
		       output_any_int f discr_type (sign,n);
		       fprintf f ",@ ";
		       if have_x then
			 output_ofconv_for_type name "x" d.decl_type
		       else
			 fprintf f "Xdr.XV_void";
		       fprintf f "@])";
		   | _ ->
		       fprintf f "@[<hv 2>Xdr.XV_union_over_enum_fast@ (%d," k;
		       if is_default then (
			 if have_x then (
			   assert(have_mkdefault);
			   fprintf f "(mkdefault x)"
			 )
			 else
			   fprintf f "Xdr.XV_void"
		       )
		       else (
			 if have_x then
			   output_ofconv_for_type name "x" d.decl_type
			 else
			   fprintf f "Xdr.XV_void"
		       );
		       fprintf f ")@]"
	      )
	      (fun d have_x ->
		 (* f_default *)
		 let constr =
		   sprintf 
		     "Xdr.XV_union_over_%s"
		     (name_of_int discr_type) in
		 let int_conversion =
		   conversion_netnumber_of_custom_int discr_type in
		 if have_x then (
		   fprintf f "let x = ";
		   output_ofconv_for_type name "x" d.decl_type;
		   fprintf f " in@ ";
		   fprintf f "%s(%s discriminant, x)@]" 
		     constr int_conversion;
		 ) else
		   fprintf f "%s(%s discriminant, Xdr.XV_void)@]" 
		     constr int_conversion
	      )
	      (fun () ->
		 (* f_let *)
		 if have_mkdefault then (
		   match u.default with
		     | None -> assert false
		     | Some d ->
			 fprintf f "let mkdefault x =@;<1 2>";
			 fprintf f "@[<hv>";
			 output_ofconv_for_type name "x" d.decl_type;
			 fprintf f "@]";
			 fprintf f " in@ ";
		 )
	      )
    );
    fprintf f "@]"

  and output_ofconv_for_array name var t' =
    let t1 = get_type_from_map typemap t' in
    match t1 with
      | T_string _
      | T_string_unlimited ->
	  fprintf f "@[<hv 2>(Xdr.XV_array_of_string_fast %s)@]" var
      | _ ->
	  fprintf f "@[<hv 2>Xdr.XV_array@ ";
	  fprintf f "@[<hv 2>(Array.map@ ";
	  fprintf f "(fun x -> ";
	  output_ofconv_for_type name "x" t';
	  fprintf f ")@ %s)@]@]" var
	  
  and output_ofconv_for_tuple name var tl =
    fprintf f "@[<hv 1>";
    fprintf f "(let (";
    let n = ref 0 in
    let isfirst = ref true in
    List.iter
      (fun t ->
	 if not !isfirst then fprintf f ", ";
	 isfirst := false;
	 fprintf f "x%d" !n;
	 incr n
      )
      tl;
    fprintf f ") = %s in@ " var;
    fprintf f "@[<hv 2>Xdr.XV_struct_fast@ [|@ ";
    n := 0;
    List.iter
      (fun t ->
	 fprintf f "  @[<hv 2>(";
	 output_ofconv_for_type name ("x" ^ string_of_int !n) t;
	 fprintf f ");@]@ ";
	 incr n
      )
      tl;
    fprintf f "|]@]@ )@]"
  in

  let rec output_sizefn_for_type (name:string) (tname:string) (t:xdr_type) =
    (* Generates a function returning the packed size *)
    fprintf f "@[<hv 2>";
    begin_decl();
    fprintf f "_sizeexpr_%s (x:%s) : int =@ "
      name
      tname;
    output_sizeexpr_for_type name (calc_sizefn_for_type name t);
    fprintf f "@]@\n";

    fprintf f "@[<hv 2>";
    begin_decl();
    fprintf f "_size_%s (x:exn) : int =@ " name;
    fprintf f "match x with@ ";
    fprintf f "| X_%s y -> _sizeexpr_%s y@ " tname name;
    fprintf f "| _ -> raise Xdr.Dest_failure";
    fprintf f "@]@\n"

  and output_sizeexpr_for_type name calcexpr =
    match calcexpr with
      | `Size_const n ->
	  fprintf f "%Ld" n
      | `Size_fun name ->
	  fprintf f "(%s x)" name
      | `Size_opt(n,calcexpr1) ->
	  fprintf f "@[<hv 2>(match x with@ ";
	  fprintf f "| None -> %Ld@ " n;
	  fprintf f "| Some x ->@[<hv 2>@ ";
	  output_sizeexpr_for_type 
	    name 
	    (calc_sizefn_for_struct ["",`Size_const n; "",calcexpr1]);
	  fprintf f "@])@]"
      | `Size_struct l ->
	  fprintf f "@[<hv 2>(";
	  let first = ref true in
	  List.iter
	    (fun (component,calcexpr1) ->
	       if not !first then
		 fprintf f " +!@ ";
	       first := false;
	       if component = "" then
		 output_sizeexpr_for_type name calcexpr1
	       else (
		 fprintf f "@[<hv 2>( let x = x%s in@ " component;
		 output_sizeexpr_for_type name calcexpr1;
		 fprintf f "@])"
	       )
	    )
	    l;
	  fprintf f ")@]"
      | `Size_array (n_head,cond,calcexpr1) ->
	  ( match cond with
	      | None -> ()
	      | Some (`Fixed n) ->
		  fprintf f "@[<hv 2>( ";
		  fprintf f "if Array.length x <> %d then@ " n;
		  fprintf f 
		    "  raise(Xdr.Xdr_failure \"array length mismatch\");@ ";
	      | Some (`Limit n) ->
		  fprintf f "@[<hv 2>( ";
		  fprintf f "if Array.length x > %d then@ " n;
		  fprintf f 
		    "  raise(Xdr.Xdr_failure \"array length mismatch\");@ ";
	  );
	  ( match calcexpr1 with
	      | `Size_const n ->
		  fprintf f "(%Ld *! Array.length x +! %Ld)" n n_head
	      | _ ->
		  fprintf f "@[<hv 2>(Array.fold_left@ ";
		  fprintf f "@[<hv 2>(fun s x ->@ ";
		  fprintf f "s +! ";
		  output_sizeexpr_for_type name calcexpr1;
		  fprintf f "@]@ )@ ";
		  fprintf f "%Ld@ " n_head;
		  fprintf f "x";
		  fprintf f "@]@ )"
	  );
	  ( match cond with
	      | None -> ()
	      | Some _ ->
		  fprintf f "@]@ )"
	  )
      | `Size_union(u,sizeexpr_cases,sizeexpr_default) ->
	  (* let discr_type = get_type_of_decl u.discriminant in *)
	  let have_mkdefault =
	    have_enum_default_with_arg u get_type_of_decl in
	  fprintf f "@[<hv 2>( 4 +!@ ";
	  output_match_union_by_cases
	    f
	    u
	    "x"
	    get_type_of_decl
	    (fun k (sign,n) d have_x is_default ->       (* f_case *)
	       if have_x then (
		 if is_default && have_mkdefault then 
		   fprintf f "mkdefault x"
		 else
		   let sizeexpr = List.nth sizeexpr_cases k in
		   output_sizeexpr_for_type name sizeexpr
	       )
	       else
		 fprintf f "0"
	    )
	    (fun d have_x ->                              (* f_default *)
	       if have_x then
		 match sizeexpr_default with
		   | None -> assert false
		   | Some sizeexpr ->
		       output_sizeexpr_for_type name sizeexpr
	       else
		 fprintf f "0"
	    )
	    (fun () ->                                   (* f_let *)
	       if have_mkdefault then (
		 match sizeexpr_default with
		   | None -> assert false
		   | Some sizeexpr ->
		       fprintf f "let mkdefault x =@;<1 2>";
		       fprintf f "@[<hv>";
		       output_sizeexpr_for_type name sizeexpr;
		       fprintf f "@]";
		       fprintf f " in@ ";
	       )
	    );
	  fprintf f "@]@ )";


  and calc_sizefn_for_type (name:string) (t:xdr_type) = (
    match t with
      | T_void ->
	  `Size_const 0L
      | T_opaque_fixed n ->
	  (* no size check here - this is done in writefn *)
	  let nL = int64_of_const (constant !n) in
	  `Size_const
	    (if nL=0L then 0L else 
	       Int64.mul (Int64.succ(Int64.div (Int64.pred nL) 4L)) 4L)
      | T_opaque n
      | T_string n ->
	  `Size_fun 
	    (sprintf 
	       "(Xdr.sizefn_string (Netnumber.logical_uint4_of_int32 (%ldl)))"
	       (Int64.to_int32 (int64_of_const (constant !n)))
	    )
      | T_opaque_unlimited
      | T_string_unlimited ->
	  `Size_fun 
	    "(Xdr.sizefn_string (Netnumber.logical_uint4_of_int32 (-1l)))"
      | T_option t' ->
	  `Size_opt(4L, calc_sizefn_for_type name t')
      | T_int _ | T_uint _ | T_float | T_bool | T_enum _  ->
	  `Size_const 4L
      | T_hyper _ | T_uhyper _ | T_double ->
	  `Size_const 8L
      | T_struct tdl ->
	  calc_sizefn_for_struct
	    (List.map 
	       (fun d -> 
		  let component = "." ^ d.decl_symbol.ocaml_name in
		  (component, calc_sizefn_for_type name d.decl_type)
	       ) 
	       (List.filter
		  (fun d -> d.decl_type <> T_void)
		  tdl
	       )
	    )
      | T_refer_to (_,refname) ->
	  let ocaml_name =
	    try Hashtbl.find typenames !refname
	    with Not_found -> assert false in
	  `Size_fun
	    (sprintf "_sizeexpr_%s" ocaml_name)
      | T_array_fixed(c,t') ->
	  let n = Int64.to_int(int64_of_const (constant !c)) in
	  `Size_array(0L, Some(`Fixed n),  calc_sizefn_for_type name t')
      | T_array(c,t') ->
	  let nL = int64_of_const (constant !c) in
	  if nL >= Int64.of_int Sys.max_array_length then
	    `Size_array(4L, None, calc_sizefn_for_type name t')
	  else
	    let n = Int64.to_int nL in
	    `Size_array(4L, Some(`Limit n),  calc_sizefn_for_type name t')
      | T_array_unlimited t' ->
	  `Size_array(4L, None, calc_sizefn_for_type name t')
      | T_union u ->
	  let sizeexpr_cases =
	    List.map
	      (fun (_,_,td) -> calc_sizefn_for_type name td.decl_type)
	      u.cases in
	  let sizeexpr_default =
	    match u.default with
	      | None -> None
	      | Some td -> Some(calc_sizefn_for_type name td.decl_type) in
	  `Size_union(u, sizeexpr_cases, sizeexpr_default)

      | T_mstring(_,_)
      | T_mstring_unlimited _ ->
	  failwith "output_sizefn_for_type"
  )

  and calc_sizefn_for_struct l1 =
    let n_const =
      List.fold_left
	Int64.add
	0L
	(List.map
	   (function
	      | (_, `Size_const n) -> n
	      | _ -> 0L
	   )
	   l1
	) in
    let l_other =
      List.flatten
	(List.map
	   (function
	      | (_, `Size_const _) -> []
	      | x -> [x]
	   )
	   l1
	) in
    let l2 =
      (if n_const > 0L then ["", `Size_const n_const] else []) @ l_other in
    match l2 with
      | [] -> `Size_const 0L
      | ["", x] -> x
      | _ -> `Size_struct l2
  in

  let rec output_writefn_for_type (name:string) (tname:string) (t:xdr_type) =
    (* Generates a function writing directly *)
    fprintf f "@[<hv 2>";
    begin_decl();
    fprintf f "_writeexpr_%s (x:%s) s p : unit =@ " name tname;
    output_writeexpr_for_type name t;
    fprintf f "()";
    fprintf f "@]@\n";

    fprintf f "@[<hv 2>";
    begin_decl();
    fprintf f "_write_%s (x:exn) s p : unit =@ " name;
    fprintf f "match x with@ ";
    fprintf f "| X_%s y -> _writeexpr_%s y s p@ " tname name;
    fprintf f "| _ -> raise Xdr.Dest_failure";
    fprintf f "@]@\n"

  and output_writeexpr_for_type name t =
    match t with
      | T_void ->
	  ()
      | T_opaque_fixed c ->
	  let (_, n_uint4) = constant !c in
	  let n =
	    try Netnumber.int_of_uint4 n_uint4
	    with _ -> assert false in (* already checked in Syntax *)
	  fprintf f
	    "Xdr.write_string_fixed %d x s p;@ "
	    n
      | T_opaque _
      | T_opaque_unlimited
      | T_string _
      | T_string_unlimited ->
	  (* The size constraint has already been checked by sizefn *)
	  fprintf f "Xdr.write_string x s p;@ ";
      | T_option t' ->
	  fprintf f "@[<hv>( match x with@ ";

	  fprintf f "| @[<hv 2>None ->@ ";
	  fprintf f "Netnumber.BE.write_int4_unsafe s !p \
                       (Netnumber.int4_of_int 0);@ ";
	  fprintf f "p := !p + 4";
	  fprintf f "@]@ ";

	  fprintf f "| @[<hv 2>Some x ->@ ";
	  fprintf f "Netnumber.BE.write_int4_unsafe s !p \
                       (Netnumber.int4_of_int 1);@ ";
	  fprintf f "p := !p + 4;@ ";
	  output_writeexpr_for_type name t';
	  fprintf f "()";
	  fprintf f "@]@ )";

	  fprintf f "@];@ ";
      | T_int _
      | T_uint _
      | T_hyper _
      | T_uhyper _ ->
	  fprintf f "Netnumber.BE.write_%s_unsafe s !p (%s x)"
	    (netnumber_name_of_int t)
	    (conversion_netnumber_of_custom_int t);
	  fprintf f ";@ ";
	  fprintf f "p := !p + %d;@ " (size_of_int t)
      | T_float ->
	  fprintf f
	    "Netnumber.BE.write_fp4 s !p (Netnumber.fp4_of_float x);@ ";
	  fprintf f "p := !p + 4;@ "
      | T_double ->
	  fprintf f
	    "Netnumber.BE.write_fp8 s !p (Netnumber.fp8_of_float x);@ ";
	  fprintf f "p := !p + 8;@ "
      | T_bool ->
	  fprintf f "Netnumber.BE.write_int4_unsafe s !p ";
	  fprintf f "(Netnumber.int4_of_int (if x then 1 else 0));@ ";
	  fprintf f "p := !p + 4;@ "
      | T_enum e  ->
	  let e = strip_enum_list e in
	  let cases =
	    String.concat "; "
	      (List.map
		 (fun (_,c) ->
		    Int64.to_string (int64_of_const (constant !c)) ^ "l"
		 )
		 e
	      ) in
	  fprintf f
	    "@[<hv 2>if not(List.mem (Netnumber.int32_of_int4 x) [ %s ]) \
               then@ " cases;
	  fprintf f "raise(Xdr.Xdr_failure \"invalid enum\");@]@ ";
	  fprintf f "Netnumber.BE.write_int4_unsafe s !p x;@ ";
	  fprintf f "p := !p + 4;@ "
      | T_struct tdl ->
	  List.iter
	    (fun d ->
	       if d.decl_type <> T_void then (
		 fprintf f "@[<hov 2>( let x = x.%s in@ "
		   d.decl_symbol.ocaml_name;
		 output_writeexpr_for_type name d.decl_type;
		 fprintf f "()@]@ );@ ";
	       )
	    )
	    tdl
      | T_refer_to (_,refname) ->
	  let ocaml_name =
	    try Hashtbl.find typenames !refname
	    with Not_found -> assert false in
	  fprintf f "_writeexpr_%s x s p;@ " ocaml_name
      | T_array_fixed(_,t')
      | T_array(_,t')
      | T_array_unlimited t' ->
	  (* The size constraints have already been checked by _size *)
	  ( match t with
	      | T_array(_,t')
	      | T_array_unlimited t' ->
		  fprintf f "Netnumber.BE.write_uint4_unsafe s !p@ ";
		  fprintf f "  (Netnumber.uint4_of_int (Array.length x));@ ";
		  fprintf f "p := !p + 4;@ "
	      | _ -> ()
	  );
	  fprintf f "@[<hv 2>Array.iter@ ";
	  fprintf f "@[<hv 2>(fun x ->@ ";
	  output_writeexpr_for_type name t';
	  fprintf f "()@]@ ";
	  fprintf f ")@ x@];@ "
      | T_union u ->
	  let discr_type = get_type_of_decl u.discriminant in
	  let have_mkdefault = 
	    have_enum_default_with_arg u get_type_of_decl in
	  output_match_union_by_cases
	    f
	    u
	    "x"
	    get_type_of_decl
	    (fun k (sign,n) d have_x is_default ->  (* f_case *)
	       ( match discr_type with
		   | T_int _ | T_enum _ | T_bool ->
		       fprintf f "Netnumber.BE.write_int4_unsafe s !p ";
		       output_int4 f (sign,n);
		       fprintf f ";@ ";
		       fprintf f "p := !p + 4;@ ";
		   | T_uint _ ->
		       fprintf f "Netnumber.BE.write_uint4_unsafe s !p ";
		       output_uint4 f (sign,n);
		       fprintf f ";@ ";
		       fprintf f "p := !p + 4;@ ";
		   | _ ->
		       assert false
	       );
	       if have_x then (
		 if is_default then
		   fprintf f "mkdefault x"
		 else (
		   output_writeexpr_for_type name d.decl_type;
		   fprintf f "()"
		 )
	       ) else
		 fprintf f "()"
	    )
	    (fun d have_x ->                        (* f_default *)
	       fprintf f "let d = %s discriminant in@ "
		 (conversion_netnumber_of_custom_int discr_type);
	       fprintf f 
		 "Netnumber.BE.write_%s_unsafe s !p d"
		 (netnumber_name_of_int discr_type);
	       fprintf f ";@ ";
	       fprintf f "p := !p + 4;@ ";
	       if have_x then (
		 output_writeexpr_for_type name d.decl_type;
		 fprintf f "()"
	       )
	    )
	    (fun () ->                             (* f_let *)
	       if have_mkdefault then (
		 match u.default with
		   | None -> assert false
		   | Some d ->
		       fprintf f "let mkdefault x =@;<1 2>";
		       fprintf f "@[<hv>";
		       output_writeexpr_for_type name d.decl_type;
		       fprintf f "@]";
		       fprintf f " in@ ";
	       )
	    );
	  fprintf f ";@ "
	    
      | T_mstring(_,_)
      | T_mstring_unlimited _ ->
	  failwith "output_writefn_for_type"
  in    
  
  let rec output_readfn_for_type (name:string) (tname:string) (t:xdr_type) =
    (* Generates a function reading directly *)
    fprintf f "@[<hv 2>";
    begin_decl();
    fprintf f "_readexpr_%s s p p_end =@ " name;
    output_readexpr_for_type name t;
    fprintf f "@]@\n";

    fprintf f "@[<hv 2>";
    begin_decl();
    fprintf f "_read_%s s p p_end : exn =@ " name;
    fprintf f "X_%s(_readexpr_%s s p p_end)" tname name;
    fprintf f "@]@\n"

  and output_readexpr_for_type name t =
    match t with
      | T_void ->
	  fprintf f "()"
      | T_opaque_fixed c ->
	  let (_, n_uint4) = constant !c in
	  let n =
	    try Netnumber.int_of_uint4 n_uint4
	    with _ -> assert false in (* already checked in Syntax *)
	  fprintf f
	    "Xdr.read_string_fixed %d s p p_end@ "
	    n
      | T_opaque n
      | T_string n ->
	  let n32 =
	    Int64.to_int32 (int64_of_const (constant !n)) in
	  fprintf f "Xdr.read_string (Netnumber.logical_uint4_of_int32 (%ldl)) \
                     s p p_end@ " n32
      | T_opaque_unlimited
      | T_string_unlimited ->
	  fprintf f "Xdr.read_string Netnumber.max_uint4 s p p_end@ ";
      | T_option t' ->
	  fprintf f "( @[<hv>";
	  fprintf f "if !p > p_end-4 then raise Netnumber.Out_of_range;@ ";
	  fprintf f "let d = Netnumber.BE.read_int4_unsafe s !p in@ ";
	  fprintf f "p := !p + 4;@ ";
	  fprintf f "match Netnumber.int_of_int4 d with@ ";
	  fprintf f "| 0 -> None@ ";
	  fprintf f "@[<hv 2>| 1 ->@ ";
	  fprintf f "@[<hv 2>Some(@ ";
	  output_readexpr_for_type name t';
	  fprintf f ")@]@]@ ";
	  fprintf f "| _ -> raise Xdr.Dest_failure";
	  fprintf f "@]@ )";
      | T_int _
      | T_uint _
      | T_hyper _
      | T_uhyper _ ->
	  let s = size_of_int t in
	  fprintf f "( @[<hv>";
	  fprintf f "if !p > p_end-%d then raise Netnumber.Out_of_range;@ " s;
	  fprintf f "let x = Netnumber.BE.read_%s_unsafe s !p in@ "
	    (netnumber_name_of_int t);
	  fprintf f "p := !p + %d;@ " s;
	  fprintf f "(%s x)" (conversion_custom_int_of_netnumber t);
	  fprintf f "@]@ )"
      | T_float ->
	  fprintf f "( @[<hv>";
	  fprintf f "if !p > p_end-4 then raise Netnumber.Out_of_range;@ ";
	  fprintf f "let x = Netnumber.BE.read_fp4 s !p in@ ";
	  fprintf f "p := !p + 4;@ ";
	  fprintf f "Netnumber.float_of_fp4 x";
	  fprintf f "@]@ )"
      | T_double ->
	  fprintf f "( @[<hv>";
	  fprintf f "if !p > p_end-8 then raise Netnumber.Out_of_range;@ ";
	  fprintf f "let x = Netnumber.BE.read_fp8 s !p in@ ";
	  fprintf f "p := !p + 8;@ ";
	  fprintf f "Netnumber.float_of_fp8 x";
	  fprintf f "@]@ )"
      | T_bool ->
	  fprintf f "( @[<hv>";
	  fprintf f "if !p > p_end-4 then raise Netnumber.Out_of_range;@ ";
	  fprintf f "let x = Netnumber.BE.read_int4_unsafe s !p in@ ";
	  fprintf f "p := !p + 4;@ ";
	  fprintf f "(Netnumber.int_of_int4 x = 1)";
	  fprintf f "@]@ )"
      | T_enum e  ->
	  let e = strip_enum_list e in
	  let cases =
	    String.concat "; "
	      (List.map
		 (fun (_,c) ->
		    Int64.to_string (int64_of_const (constant !c)) ^ "l"
		 )
		 e
	      ) in
	  fprintf f "( @[<hv>";
	  fprintf f "if !p > p_end-4 then raise Netnumber.Out_of_range;@ ";
	  fprintf f "let x = Netnumber.BE.read_int4_unsafe s !p in@ ";
	  fprintf f "p := !p + 4;@ ";
	  fprintf f
	    "@[<hv 2>if not(List.mem (Netnumber.int32_of_int4 x) [ %s ]) \
             then@ " cases;
	  fprintf f "raise(Xdr.Xdr_format \"invalid enum\");@]@ ";
	  fprintf f "x";
	  fprintf f "@]@ )"
      | T_struct tdl ->
	  fprintf f "( @[<hv>";
	  let i = ref 0 in
	  List.iter
	    (fun d ->
	       if d.decl_type <> T_void then (
		 fprintf f "@[<hv 2>let x%d =@ " !i;
		 output_readexpr_for_type name d.decl_type;
		 fprintf f " in@]@ ";
		 incr i;
	       )
	    )
	    tdl;
	  i := 0;
	  fprintf f "{ @[<hv>";
	  List.iter
	    (fun d ->
	       if d.decl_type <> T_void then (
		 fprintf f "@ %s = x%d;" d.decl_symbol.ocaml_name !i;
		 incr i;
	       )
	    )
	    tdl;
	  fprintf f "@]@ }";
	  fprintf f "@]@ )"
      | T_refer_to (_,refname) ->
	  let ocaml_name =
	    try Hashtbl.find typenames !refname
	    with Not_found -> assert false in
	  fprintf f "( _readexpr_%s s p p_end)" ocaml_name
      | T_array_fixed(c,t') ->
	  let nL = int64_of_const (constant !c) in
	  let n = Int64.to_int nL in
	  output_readexpr_for_array name (Some (`Fixed n)) t'
      | T_array(c,t') ->
	  let nL = int64_of_const (constant !c) in
	  if nL >= Int64.of_int Sys.max_array_length then
	    output_readexpr_for_array name None t'
	  else
	    let n = Int64.to_int nL in
	    output_readexpr_for_array name (Some (`Limit n)) t'
      | T_array_unlimited t' ->
	  output_readexpr_for_array name None t'
      | T_union u ->
	  let discr_type = get_type_of_decl u.discriminant in
	  let int_name =
	    match discr_type with
	      | T_int _ | T_enum _ | T_bool -> "int4"
	      | T_uint _ -> "uint4"
	      | _ -> assert false in
	  fprintf f "( @[<hv>";
	  fprintf f "if !p > p_end-4 then raise Netnumber.Out_of_range;@ ";
	  fprintf f "let d0 = Netnumber.BE.read_%s_unsafe s !p in@ " int_name;
	  fprintf f "p := !p + 4;@ ";
	  let have_mkdefault =
	    have_enum_default_with_arg u get_type_of_decl in
	  output_match_union_by_number
	    f
	    u
	    "d"
	    false  (* by_k *)
	    get_type_of_decl
	    (fun k (sign,n) tag d d_is_default ->      (* f_case *)
	       let have_x = get_type_of_decl d <> T_void in
	       if have_x then (
		 fprintf f "@[<hv 2>let x =@ ";
		 if d_is_default && have_mkdefault then
		   fprintf f "(mkdefault())"
		 else
		   output_readexpr_for_type name d.decl_type;
		 fprintf f " in@]@ ";
	       );
	       fprintf f "`%s%s"
		 tag (if have_x then " x" else "");
	    )
	    (fun d ->                                 (* f_default *)
	       let int_conversion =
		 conversion_custom_int_of_netnumber discr_type in
	       let have_x = get_type_of_decl d <> T_void in
	       if have_x then (
		 fprintf f "@[<hv 2>let x =@ ";
		 output_readexpr_for_type name d.decl_type;
		 fprintf f " in@]@ ";
	       );
	       fprintf f "`default(@[<hv>%s d0" int_conversion;
	       if have_x then
		 fprintf f ",@ x";
	       fprintf f "@])";
	    )
	    (fun () ->                                (* f_let *)
	       fprintf f "let d = %s d0 in@ "
		 (match discr_type with
		    | T_int _  | T_enum _ | T_bool -> "Netnumber.int32_of_int4"
		    | T_uint _ -> "Netnumber.logical_int32_of_uint4"
		    | _ -> assert false
		 );
		 if have_mkdefault then (
		   match u.default with
		     | None -> assert false
		     | Some d ->
			 fprintf f "let mkdefault x =@;<1 2>";
			 fprintf f "@[<hv>";
			 output_readexpr_for_type name d.decl_type;
			 fprintf f "@]";
			 fprintf f " in@ ";
		 )
	    )
	    (fun () ->    	                      (* f_coerce *)
	       fprintf f "@ :> ";
	       output_coerce_pattern f u get_type_of_decl
	    );
	  fprintf f "@]@ )"

      | T_mstring(_,_)
      | T_mstring_unlimited _ ->
	  failwith "output_readexpr_for_type"

  and output_readexpr_for_array name cond t_elem =
    let ms = min_size t_elem in
    fprintf f "( @[<hv>";
    ( match cond with
	| None
	| Some (`Limit _) ->
	    fprintf f "if !p > p_end-4 then raise Netnumber.Out_of_range;@ ";
	    fprintf f "let ulen = Netnumber.BE.read_uint4_unsafe s !p in@ ";
	    fprintf f "p := !p + 4;@ ";
	    fprintf f "let len =@ ";
	    fprintf f "  try Netnumber.int_of_uint4 ulen@ ";
	    fprintf f "  with _ -> Xdr.raise_xdr_format_maximum_length() in@ ";
	    ( match cond with
		| Some (`Limit n) ->
		    fprintf f "if len > %d then@ " n;
		    fprintf f "  Xdr.raise_xdr_format_maximum_length();@ "
		| _ -> ()
	    )
	| Some (`Fixed n) ->
	    fprintf f "let len = %d in@ " n;
    );
    fprintf f "let m = (p_end - !p) / %d in@ " ms;
    fprintf f "if len > m then@ ";
    fprintf f "  Xdr.raise_xdr_format_too_short();@ ";
    fprintf f "Array.init@ ";
    fprintf f "  len@ ";
    fprintf f "@[<hv 4>  (fun _ ->@ ";
    output_readexpr_for_type name t_elem;
    fprintf f ")@]@]@ )"
  in    

  let permit_direct t =
    (* For which type we permit the generation of XV_direct values.
       We don't do this for all "atomic" types, so only a few types remain.
       Note that we nevertheless need to generate the "_size" and "_write"
       functions for those types that are suppressed here!
     *)
    match t with
      | T_struct _ -> true
      | T_option _ -> true
      | T_array _ | T_array_fixed _ | T_array_unlimited _ -> true
      | T_union _ -> true
      | _ -> false in

  let output_ofconv_declaration n t tname direct =
    (* MLI: *)
    fprintf mli "val _of_%s : %s -> Xdr.xdr_value;;@\n" n tname;
    (* ML: *)
    fprintf f "@[<hv>";
    begin_decl();
    fprintf f "_of_%s (x:%s) : Xdr.xdr_value =@;<1 2>"
      n
      tname;
    if !Options.enable_direct && direct && permit_direct t then (
      fprintf f
	"@[<hv>Xdr.XV_direct(X_%s x, _sizeexpr_%s x)@]" tname n
    )
    else
      output_ofconv_for_type n "x" t;
    fprintf f "@]@\n"
  in

  let output_ofconv_tuple_declaration n tl tname =
    (* MLI: *)
    fprintf mli "val _of_%s : %s -> Xdr.xdr_value;;@\n" n tname;
    (* ML: *)
    fprintf f "@[<hv>";
    begin_decl();
    fprintf f "_of_%s (x:%s) : Xdr.xdr_value =@;<1 2>"
      n
      tname;
    output_ofconv_for_tuple n "x" tl;
    fprintf f "@]@\n"
  in

  let rec check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    List.iter (check_procedure prog vers) vers.version_def

  and check_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
	      vers.version_symbol.ocaml_name ^ "'" ^
	      proc.proc_symbol.ocaml_name in

    ( match proc.proc_params with
	  [] -> assert false
	| [arg] ->
	    output_toconv_declaration
	      (pvp ^ "'arg")
	      arg
	      ("t_" ^ pvp ^ "'arg")
	      false;
	    output_ofconv_declaration
	      (pvp ^ "'arg")
	      arg
	      ("t_" ^ pvp ^ "'arg")
	      false;
	| args ->
	    output_toconv_tuple_declaration
	      (pvp ^ "'arg")
	      args
	      ("t_" ^ pvp ^ "'arg");
	    output_ofconv_tuple_declaration
	      (pvp ^ "'arg")
	      args
	      ("t_" ^ pvp ^ "'arg")
    );
    output_toconv_declaration
      (pvp ^ "'res")
      proc.proc_result
      ("t_" ^ pvp ^ "'res")
      false;
    output_ofconv_declaration
      (pvp ^ "'res")
      proc.proc_result
      ("t_" ^ pvp ^ "'res")
      false;
  in

  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  fprintf f "let ( +! ) = Xdr.safe_add@\n";
  fprintf f "let ( *! ) = Xdr.safe_mul@\n;;@\n";


  List.iter
    (function
	 Typedef td ->
	   output_toconv_declaration
	     td.decl_symbol.ocaml_name
	     td.decl_type
	     td.decl_symbol.ocaml_name
	     td.decl_direct;
	   output_ofconv_declaration
	     td.decl_symbol.ocaml_name
	     td.decl_type
	     td.decl_symbol.ocaml_name
	     td.decl_direct;
	   if !Options.enable_direct && td.decl_direct then (
	     output_sizefn_for_type
	       td.decl_symbol.ocaml_name
	       td.decl_symbol.ocaml_name
	       td.decl_type;
	     output_writefn_for_type
	       td.decl_symbol.ocaml_name
	       td.decl_symbol.ocaml_name
	       td.decl_type;
	     output_readfn_for_type
	       td.decl_symbol.ocaml_name
	       td.decl_symbol.ocaml_name
	       td.decl_type;
	   )
       | Progdef prog ->
	   check_program prog
       | _ ->
	   ())
    dl;

  if not !firstdecl then fprintf f ";;@\n";
  fprintf mli "@]";
  fprintf f "@]"
;;

(**********************************************************************)
(* Output program definitions                                         *)
(**********************************************************************)

let output_progdefs (mli:formatter) (f:formatter) (dl:xdr_def list) =

  let rec check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    let pv =
      prog.prog_symbol.ocaml_name ^ "'" ^ vers.version_symbol.ocaml_name in
    (* MLI: *)
    fprintf mli "val program_%s : Rpc_program.t;;@\n" pv;
    (* ML: *)
    fprintf f "@[<hv 2>let program_%s =@ " pv;
    fprintf f "@[<hv 2>Rpc_program.create@ ";
    output_uint4 f (false, prog.prog_number);
    fprintf f "@ ";
    output_uint4 f (false, vers.version_number);
    fprintf f "@ ";
    fprintf f "(Xdr.validate_xdr_type_system [])@ ";
    fprintf f "@[<hv 2>[";
    List.iter (declare_procedure prog vers) vers.version_def;
    fprintf f "@]@ ]";
    fprintf f "@]@]@\n;;@\n";

  and declare_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
	      vers.version_symbol.ocaml_name ^ "'" ^
	      proc.proc_symbol.ocaml_name in
    fprintf f "@ @[<hv 2>";
    fprintf f "\"%s\",@ (" proc.proc_symbol.xdr_name;
    output_uint4 f (false, proc.proc_number);
    fprintf f ",@ xdrt_%s'arg,@ xdrt_%s'res);" pvp pvp;
    fprintf f "@]";
  in

  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
       | Progdef prog ->
	   check_program prog
       | _ ->
	   ())
    dl;

  fprintf mli "@]@\n";
  fprintf f "@]@\n"
;;

(**********************************************************************)
(* Output clients                                                     *)
(**********************************************************************)

let output_client (mli:formatter) (f:formatter) (dl:xdr_def list) auxname =

  let rec check_program prog =
    (* Make functor: *)
    
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module Make'%s(U'C:Rpc_client.USE_CLIENT) : sig@ "
      prog.prog_symbol.ocaml_name;
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module Make'%s(U'C:Rpc_client.USE_CLIENT) = struct@ " 
      prog.prog_symbol.ocaml_name;
    (* Both: *)
    List.iter (check_version `Make prog) prog.prog_def;
    (* MLI: *)
    fprintf mli "@]@ ";
    fprintf mli "end@ ";
    fprintf mli "@]@\n";
    (* ML: *)
    fprintf f "@]@ ";
    fprintf f "end@ ";
    fprintf f "@]@\n";

    (* Mapping with U'C=Rpc_client: *)

    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig@ "
      prog.prog_symbol.ocaml_name;
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " 
      prog.prog_symbol.ocaml_name;
    (* Both: *)
    List.iter (check_version `Client prog) prog.prog_def;
    (* MLI: *)
    fprintf mli "@]@ ";
    fprintf mli "end@ ";
    fprintf mli "@]@\n";
    (* ML: *)
    fprintf f "@]@ ";
    fprintf f "end@ ";
    fprintf f "@]@\n";
    

  and check_version inst prog vers =
    let pv =
      prog.prog_symbol.ocaml_name ^ "'" ^ vers.version_symbol.ocaml_name in
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig" vers.version_symbol.ocaml_name;
    fprintf mli "@ ";
    fprintf mli "open %s@ " auxname;
    ( match inst with
	| `Client ->
	    fprintf mli "type t = Rpc_client.t@ ";
	    fprintf mli "val @[<hv 4>create_client :@ ?esys:Unixqueue.event_system ->@ ?program_number:Netnumber.uint4 -> @ ?version_number:Netnumber.uint4 -> @ Rpc_client.connector ->@ Rpc.protocol ->@ Rpc_client.t@]";
	    fprintf mli "@ ";
	    fprintf mli "val @[<hv 4>create_portmapped_client :@ ?esys:Unixqueue.event_system ->@ ?program_number:Netnumber.uint4 -> @ ?version_number:Netnumber.uint4 -> @ string ->@ Rpc.protocol ->@ Rpc_client.t@]";
	    fprintf mli "@ ";
	    fprintf mli "val @[<hv 4>create_client2 :@ ?esys:Unixqueue.event_system ->@ ?program_number:Netnumber.uint4 -> @ ?version_number:Netnumber.uint4 -> @ Rpc_client.mode2 ->@ Rpc_client.t@]";
	    fprintf mli "@ ";
	| `Make ->
	    fprintf mli "type t = U'C.t@ ";
    );
    fprintf mli "val _program : Rpc_program.t@ ";
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " vers.version_symbol.ocaml_name;
    ( match inst with
	| `Client ->
	    (* Ocaml doesn't like: Make'<prog>(Rpc_client).<vers> *)
	    fprintf f "module M'0 = Make'%s(Rpc_client)@ "
	      prog.prog_symbol.ocaml_name ;
	    fprintf f "include M'0.%s@ " 
	      vers.version_symbol.ocaml_name;
	    fprintf f "open %s@ " auxname;
	    fprintf f "let _program = program_%s@ " pv;

	    fprintf f "@ ";
	    fprintf f "@[<hv 2>let create_client@ ";
	    fprintf f "?(esys = Unixqueue.create_unix_event_system())@ ";
	    fprintf f "?program_number@ ";
	    fprintf f "?version_number@ ";
	    fprintf f "connector@ ";
	    fprintf f "protocol =@ ";
	    fprintf f "  Rpc_client.create ?program_number ?version_number esys connector protocol _program";
	    fprintf f "@]";

	    fprintf f "@ @ ";
	    fprintf f "@[<hv 2>let create_portmapped_client ?esys ?program_number ?version_number host protocol =@ ";
	    fprintf f "create_client ?esys ?program_number ?version_number (Rpc_client.Portmapped host) protocol";
	    fprintf f "@]";
	    
	    fprintf f "@ @ ";
	    fprintf f "@[<hv 2>let create_client2@ ";
	    fprintf f "?(esys = Unixqueue.create_unix_event_system())@ ";
	    fprintf f "?program_number@ ";
	    fprintf f "?version_number@ ";
	    fprintf f "mode2 =@ ";
	    fprintf f "  Rpc_client.create2 ?program_number ?version_number mode2 _program esys";
	    fprintf f "@]";

	    fprintf f "@ ";
	| `Make ->
	    fprintf f "open %s@ " auxname;
	    fprintf f "let _program = program_%s@ " pv;
	    fprintf f "type t = U'C.t@ ";
	    fprintf f "@ ";
    );

    (* Both: *)
    List.iter (define_procedure inst prog vers) vers.version_def;

    (* MLI: *)
    fprintf mli "@]@ end@ @]";
    (* ML: *)
    fprintf f "@]@ ";
    fprintf f "end@ ";
    fprintf f "@]";

  and define_procedure inst prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
	      vers.version_symbol.ocaml_name ^ "'" ^
	      proc.proc_symbol.ocaml_name in
    let cm =
      match inst with
	| `Client -> "Rpc_client"
	| `Make -> "U'C" in

    (* MLI: *)
    fprintf mli "val @[<hv 4>%s :@ %s.t ->@ %s ->@ %s@]@ "
      proc.proc_symbol.ocaml_name
      cm
      ("t_" ^ pvp ^ "'arg")
      ("t_" ^ pvp ^ "'res");
    fprintf mli "val @[<hv 4>%s'async :@ %s.t ->@ %s ->@ ((unit -> %s) -> unit) ->@ unit@]@ "
      proc.proc_symbol.ocaml_name
      cm
      ("t_" ^ pvp ^ "'arg")
      ("t_" ^ pvp ^ "'res");
    (* ML: *)
    ( match inst with
	| `Client ->
	    ()
	| `Make ->
	    fprintf f "@[<hv 2>";
	    fprintf f "let %s client arg =@ " proc.proc_symbol.ocaml_name;
	    (* fprintf f "assert(Rpc_client.program client == _program);@ "; *)
	    fprintf f "_to_%s'res (U'C.unbound_sync_call client _program \"%s\" (_of_%s'arg arg))"
	      pvp proc.proc_symbol.xdr_name pvp;
	    fprintf f "@]@ @ ";
	    
	    fprintf f "@[<hv 2>";
	    fprintf f "let %s'async client arg pass_reply =@ " proc.proc_symbol.ocaml_name;
	    (* fprintf f "assert(Rpc_client.program client == _program);@ "; *)
	    fprintf f "U'C.unbound_async_call client _program \"%s\" (_of_%s'arg arg)@ "
	      proc.proc_symbol.xdr_name pvp;
	    fprintf f "  (fun g -> pass_reply (fun () -> _to_%s'res (g())))@ " pvp;
	    fprintf f "@]@ @ "
    )
  in

  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
       | Progdef prog ->
	   check_program prog
       | _ ->
	   ())
    dl;

  fprintf mli "@]@\n";
  fprintf f "@]@\n"
;;

(**********************************************************************)
(* Output servers                                                     *)
(**********************************************************************)

type style =
    [ `Create | `Create2 ]

let output_server (style:style)
                  (mli:formatter) (f:formatter) (dl:xdr_def list) auxname =

  let rec check_program prog =
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig@ " prog.prog_symbol.ocaml_name;
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " prog.prog_symbol.ocaml_name;
    (* Both: *)
    List.iter (check_version prog) prog.prog_def;
    (* MLI: *)
    fprintf mli "@]@ ";
    fprintf mli "end@ ";
    fprintf mli "@]@\n";
    (* ML: *)
    fprintf f "@]@ ";
    fprintf f "end@ ";
    fprintf f "@]";

  and check_version prog vers =
    match style with
      | `Create -> check_version1 prog vers
      | `Create2 -> check_version2 prog vers

  and check_version1 prog vers =
    let pv =
      prog.prog_symbol.ocaml_name ^ "'" ^ vers.version_symbol.ocaml_name in
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig" vers.version_symbol.ocaml_name;
    fprintf mli "@ ";
    fprintf mli "open %s@ " auxname;
    fprintf mli "val @[<hv 4>create_server :@ ?limit:int ->@ ?program_number:Netnumber.uint4 ->@ ?version_number:Netnumber.uint4 ->@ ";
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " vers.version_symbol.ocaml_name;
    fprintf f "open %s@ " auxname;
    fprintf f "let _program = program_%s@ " pv;

    fprintf f "@ ";
    fprintf f "@[<hv 2>let create_server@ ";
    fprintf f "?(limit = 20)@ ";
    fprintf f "?program_number@ ";
    fprintf f "?version_number@ ";
    (* Both: *)
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf mli "proc_%s : (@[<hv>t_%s'arg ->@ t_%s'res@]) ->@ "
	   proc.proc_symbol.ocaml_name
	   pvp
	   pvp;
	 fprintf f "~proc_%s@ " proc.proc_symbol.ocaml_name)
      vers.version_def;
    (* MLI: *)
    fprintf mli "Rpc_server.connector ->@ ";
    fprintf mli "Rpc.protocol ->@ ";
    fprintf mli "Rpc.mode ->@ ";
    fprintf mli "Unixqueue.event_system ->@ ";
    fprintf mli "Rpc_server.t@]@ ";
    (* ML: *)
    fprintf f "connector@ ";
    fprintf f "protocol@ ";
    fprintf f "mode@ ";
    fprintf f "esys@ ";
    fprintf f "=@ ";
    fprintf f "  @[<hv 2>";
    fprintf f "Rpc_server.create@   ?program_number ?version_number esys connector protocol mode _program@ ";
    fprintf f "  @[<hv 2>[";
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf f "@ (Rpc_server.Sync { @[<v>Rpc_server.sync_name = \"%s\";@ "
	   proc.proc_symbol.xdr_name;
	 fprintf f "Rpc_server.sync_proc = (fun x -> _of_%s'res (proc_%s (_to_%s'arg x)))@]});"
	   pvp proc.proc_symbol.ocaml_name pvp;
      )
      vers.version_def;
    fprintf f "@]@   ]@ ";
    fprintf f "  limit@]@]";

    fprintf f "@ @ ";

    (* MLI: *)
    fprintf mli "val @[<hv 4>create_async_server :@ ?limit:int ->@ ?program_number:Netnumber.uint4 ->@ ?version_number:Netnumber.uint4 ->@ ";
    (* ML: *)
    fprintf f "@[<hv 2>let create_async_server@ ";
    fprintf f "?(limit = 20)@ ";
    fprintf f "?program_number@ ";
    fprintf f "?version_number@ ";
    (* Both: *)
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf mli "proc_%s : (@[<hv>Rpc_server.session ->@ t_%s'arg ->@ (t_%s'res -> unit) ->@ unit)@] ->@ "
	   proc.proc_symbol.ocaml_name
	   pvp
	   pvp;
	 fprintf f "~proc_%s@ " proc.proc_symbol.ocaml_name)
      vers.version_def;
    (* MLI: *)
    fprintf mli "Rpc_server.connector ->@ ";
    fprintf mli "Rpc.protocol ->@ ";
    fprintf mli "Rpc.mode ->@ ";
    fprintf mli "Unixqueue.event_system ->@ ";
    fprintf mli "Rpc_server.t@]@ ";
    (* ML: *)
    fprintf f "connector@ ";
    fprintf f "protocol@ ";
    fprintf f "mode@ ";
    fprintf f "esys@ ";
    fprintf f "=@ ";
    fprintf f "  @[<hv 2>";
    fprintf f "Rpc_server.create@   ?program_number ?version_number esys connector protocol mode _program@ ";
    fprintf f "  @[<hv 2>[";
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf f "@ (Rpc_server.Async { @[<v>Rpc_server.async_name = \"%s\";@ "
	   proc.proc_symbol.xdr_name;
	 fprintf f "Rpc_server.async_invoke = (fun s x -> proc_%s s (_to_%s'arg x) (fun y -> Rpc_server.reply s (_of_%s'res y)))@]});"
	  proc.proc_symbol.ocaml_name pvp pvp;
      )
      vers.version_def;
    fprintf f "@]@   ]@ ";
    fprintf f "  limit@]@]";

    fprintf f "@ @ ";

    fprintf mli "@]end@ @]";

    fprintf f "@]end@ ";
    fprintf f "@]";

  and check_version2 prog vers =
    let pv =
      prog.prog_symbol.ocaml_name ^ "'" ^ vers.version_symbol.ocaml_name in
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig" vers.version_symbol.ocaml_name;
    fprintf mli "@ ";
    fprintf mli "open %s@ " auxname;
    fprintf mli "val @[<hv 4>bind :@ ?program_number:Netnumber.uint4 ->@ ?version_number:Netnumber.uint4 ->@ ";
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " vers.version_symbol.ocaml_name;
    fprintf f "open %s@ " auxname;
    fprintf f "let _program = program_%s@ " pv;

    fprintf f "@ ";
    fprintf f "@[<hv 2>let bind@ ";
    fprintf f "?program_number@ ";
    fprintf f "?version_number@ ";
    (* Both: *)
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf mli "proc_%s : (@[<hv>t_%s'arg ->@ t_%s'res@]) ->@ "
	   proc.proc_symbol.ocaml_name
	   pvp
	   pvp;
	 fprintf f "~proc_%s@ " proc.proc_symbol.ocaml_name)
      vers.version_def;
    (* MLI: *)
    fprintf mli "Rpc_server.t ->@ ";
    fprintf mli "unit@]@ ";
    (* ML: *)
    fprintf f "srv@ ";
    fprintf f "=@ ";
    fprintf f "  @[<hv 2>";
    fprintf f "Rpc_server.bind@   ?program_number ?version_number _program @ ";
    fprintf f "  @[<hv 2>[";
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf f "@ (Rpc_server.Sync { @[<v>Rpc_server.sync_name = \"%s\";@ "
	   proc.proc_symbol.xdr_name;
	 fprintf f "Rpc_server.sync_proc = (fun x -> _of_%s'res (proc_%s (_to_%s'arg x)))@]});"
	   pvp proc.proc_symbol.ocaml_name pvp;
      )
      vers.version_def;
    fprintf f "@]@   ]@ ";
    fprintf f "  srv@]@]";

    fprintf f "@ @ ";

    (* MLI: *)
    fprintf mli "val @[<hv 4>bind_async :@ ?program_number:Netnumber.uint4 ->@ ?version_number:Netnumber.uint4 ->@ ";
    (* ML: *)
    fprintf f "@[<hv 2>let bind_async@ ";
    fprintf f "?program_number@ ";
    fprintf f "?version_number@ ";
    (* Both: *)
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf mli "proc_%s : (@[<hv>Rpc_server.session ->@ t_%s'arg ->@ (t_%s'res -> unit) ->@ unit)@] ->@ "
	   proc.proc_symbol.ocaml_name
	   pvp
	   pvp;
	 fprintf f "~proc_%s@ " proc.proc_symbol.ocaml_name)
      vers.version_def;
    (* MLI: *)
    fprintf mli "Rpc_server.t ->@ ";
    fprintf mli "unit@]@ ";
    (* ML: *)
    fprintf f "srv@ ";
    fprintf f "=@ ";
    fprintf f "  @[<hv 2>";
    fprintf f "Rpc_server.bind@   ?program_number ?version_number _program @ ";
    fprintf f "  @[<hv 2>[";
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf f "@ (Rpc_server.Async { @[<v>Rpc_server.async_name = \"%s\";@ "
	   proc.proc_symbol.xdr_name;
	 fprintf f "Rpc_server.async_invoke = (fun s x -> proc_%s s (_to_%s'arg x) (fun y -> Rpc_server.reply s (_of_%s'res y)))@]});"
	  proc.proc_symbol.ocaml_name pvp pvp;
      )
      vers.version_def;
    fprintf f "@]@   ]@ ";
    fprintf f "  srv@]@]";

    fprintf f "@ @ ";

    fprintf mli "@]end@ @]";

    fprintf f "@]end@ ";
    fprintf f "@]";

  in

  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
       | Progdef prog ->
	   check_program prog
       | _ ->
	   ())
    dl;

  fprintf mli "@]@\n";
  fprintf f "@]@\n"
;;
