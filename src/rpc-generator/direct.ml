(* $Id: direct.ml 1701 2012-02-14 14:48:46Z gerd $ *)

(* Set the direct flag on declarations only using allowed constructions *)

open Syntax

let mark_decls_suited_for_direct_mapping dl =
  let typemap = Hashtbl.create 100 in
  List.iter
    (function
         Typedef td ->
           if not (Hashtbl.mem typemap td.decl_symbol.xdr_name) then begin
             (* Only get the first type definition *)
             Hashtbl.add
               typemap
               td.decl_symbol.xdr_name
               td
           end
       | _ -> ()
    )
    dl;
  
  let visited = Hashtbl.create 100 in
  let visiting = Hashtbl.create 100 in

  let rec visit_decl td =
    Hashtbl.add visiting td.decl_symbol.xdr_name ();
    td.decl_direct <- visit_type td.decl_type;
    Hashtbl.remove visiting td.decl_symbol.xdr_name;
    Hashtbl.add visited td.decl_symbol.xdr_name ()

  and visit_type t =
    match t with
      | T_opaque_fixed _
      | T_opaque _
      | T_opaque_unlimited
      | T_string _
      | T_string_unlimited
      | T_void
      | T_int _
      | T_uint _
      | T_hyper _
      | T_uhyper _
      | T_double
      | T_float
      | T_bool
      | T_enum _ ->
	  true
      | T_option t1 ->
	  visit_type t1
      | T_struct tdl ->
	  List.for_all (fun td -> visit_type td.decl_type) tdl
      | T_refer_to(_,n) ->
	  not (Hashtbl.mem visiting !n) && (
	    let td = Hashtbl.find typemap !n in
	    if not (Hashtbl.mem visited !n) then
	      visit_decl td;
	    td.decl_direct
	  )
      | T_array_fixed(_,t1)
      | T_array (_,t1)
      | T_array_unlimited t1 ->
	  visit_type t1
      | T_union u ->
	  List.for_all
	    (fun (_,_,td) -> visit_type td.decl_type) 
	    u.cases &&
	  ( match u.default with
	      | None -> true
	      | Some d -> visit_type d.decl_type
	  )
      | T_mstring _
      | T_mstring_unlimited _ ->
	  false
  in

  List.iter
    (function
         Typedef td -> visit_decl td
       | _ -> ()
    )
    dl

	   
