(* $Id: nethttpd_services.ml 1642 2011-07-20 20:46:25Z gerd $
 *
 *)

(*
 * Copyright 2005 Baretta s.r.l. and Gerd Stolpmann
 *
 * This file is part of Nethttpd.
 *
 * Nethttpd is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Nethttpd is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with WDialog; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Nethttpd_services" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Nethttpd_services" Debug.enable

let () =
  Netlog.Debug.register_module "Nethttpd_services" Debug.enable

open Nethttp
open Nethttp.Header
open Nethttpd_types
open Printf

type host =
    { server_pref_name : string option;
      server_pref_port : int option;
      server_names : (string * int) list;
      server_addresses : (Unix.inet_addr * int) list;
    }

type 'a host_distributor =
      ( host * 'a http_service ) list


(* Note: For all objects below we cannot define classes (i.e. "class xy = ..."),
 * but we _must_ fall back to ad-hoc objects (i.e. "let xy = object ... end").
 * The reason is a subtle typing difference: classes must not have open types,
 * but ad-hoc objects can have them. Here, the method [def_term] is usually
 * something like [> `Foo], i.e. an _open_ variant. This is not possible with
 * classes. We need open variants, however, otherwise one could not put
 * several service objects into the same list, i.e. [ service1; service2 ].
 * 
 * Ad-hoc objects are available since O'Caml 3.08. This means this module cannot
 * be type-checked in any earlier version of O'Caml, i.e. this is 
 * "bleeding-edge typing".
 *)

let host_distributor (spec : 'a host_distributor) =
object(self)
  method name = "host_distributor"
  method def_term = `Host_distributor spec
  method print fmt =
    Format.fprintf fmt "@[<hv 4>host_distributor(";
    List.iter
      (fun (host,service) ->
	 Format.fprintf fmt "@,@[<hv 4>host(";
	 ( match host.server_pref_name with
	     | Some n -> Format.fprintf fmt "@ pref_name(%s)" n
	     | None   -> ()
	 );
	 ( match host.server_pref_port with
	     | Some p -> Format.fprintf fmt "@ pref_port(%d)" p
	     | None   -> ()
	 );
	 List.iter
	   (fun (n,p) ->
	      Format.fprintf fmt "@ name(%s:%d)" n p
	   )
	   host.server_names;
	 List.iter
	   (fun (addr,p) ->
	      let n = Unix.string_of_inet_addr addr in
	      Format.fprintf fmt "@ addr(%s:%d)" n p
	   )
	   host.server_addresses;
	 Format.fprintf fmt "@ ";
	 service # print fmt;
	 Format.fprintf fmt "@]@,)";
      )
      spec;
    Format.fprintf fmt "@]@,)"

  method process_header (env : extended_environment) =
    (* For simplicity, just iterate over spec and take the first matching host
     * definition.
     *)
    let def_matches host =
      (* Check server_names first, then server_addresses. Returns (name,port) on
       * success, Not_found otherwise
       *)
      try
	let req_name = env # input_header_field "Host" in
	let (req_host, req_port_opt) = split_host_port req_name in
	let req_host = String.lowercase req_host in
	let req_port = match req_port_opt with Some p -> p | None -> 80 in  (* CHECK *)
	List.find
	  (fun (n,p) -> (n = "*" || String.lowercase n = req_host) && 
	                (p = 0 || p = req_port))
	  host.server_names
      with
	  Not_found ->
	    ( let (req_sockaddr, req_sockport) =
		match env # server_socket_addr with
		  | Unix.ADDR_INET(inet,port) -> (inet,port)
		  | _ -> failwith "Not an Internet socket" in
	      if List.exists
		(fun (n,p) -> 
		   (n = Unix.inet_addr_any || n = req_sockaddr) && 
		   (p = 0 || p = req_sockport))
		host.server_addresses
	      then
		(Unix.string_of_inet_addr req_sockaddr, req_sockport)
	      else
		raise Not_found
	    )
    in
    let rec find_host hosts =
      match hosts with
	| (host, service) :: hosts' ->
	    ( try (host, service, def_matches host) with Not_found -> find_host hosts' )
	| [] ->
	    raise Not_found
    in
    try
      let (m_host, m_service, (m_name, m_port)) = find_host spec in  (* or Not_found *)
      (* Finally, we have found the host [m_host] served by [m_service]. 
       * We must now set the virtual names in [env].
       *)
      let any_name = Unix.string_of_inet_addr Unix.inet_addr_any in
      let (sock_addr, sock_port) =
	match env # server_socket_addr with
	  | Unix.ADDR_INET(inet,port) -> (inet,port)
	  | _ -> failwith "Not an Internet socket" in
      let new_server_name =
	match m_host.server_pref_name with
	  | Some n -> n
	  | None -> 
	      (* No preferred name: Use [m_name] if possible *)
	      if m_name = "*" || m_name = any_name then
		Unix.string_of_inet_addr sock_addr  (* fallback *)
	      else
		m_name in
      let new_server_port =
	match m_host.server_pref_port with
	  | Some p -> string_of_int p
	  | None -> 
	      (* No preferred port: Use [m_port] if possible *)
	      if m_port = 0 then
		string_of_int sock_port  (* fallback *)
	      else
		string_of_int m_port in
      let new_properties =
	update_alist 
	  [ "SERVER_NAME", new_server_name;
	    "SERVER_PORT", new_server_port
	  ]
	  env#cgi_properties in
      let new_env =
	new redirected_environment 
	  ~properties:new_properties
	  ~in_channel:(env # input_channel) env in
      (* Pass control over to the corresponding service: *)
      m_service # process_header new_env

    with
	Not_found ->
	  `Std_response(`Not_found, None, (Some "Nethttpd: no matching host definition"))

end

let default_host ?pref_name ?pref_port () =
  { server_pref_name = pref_name;
    server_pref_port = pref_port;
    server_names = [];
    server_addresses = [ Unix.inet_addr_any, 0 ]
  }

let options_service () =
object(self)
  method name = "options_service"
  method def_term = `Options_service
  method print fmt =
    Format.fprintf fmt "options_service()"
  method process_header env =
    if env # cgi_request_method = "OPTIONS" && env # cgi_request_uri = "*" then
      `Static(`Ok, None, "")
    else
      `Std_response(`Not_found, None, (Some "Nethttpd: This OPTIONS service works only for *"))
end

type 'a uri_distributor =
    ( string * 'a http_service ) list


module StrMap = Map.Make(String)

type 'leaf uri_tree =
    'leaf uri_node StrMap.t
and 'leaf uri_node =
    { leaf : 'leaf option;
      tree : 'leaf uri_tree;
    }

let rec make_uri_tree ( spec : 'a uri_distributor ) 
                      : 'a http_service uri_tree =
  match spec with
    | (uri, service) :: spec' ->
	let uri_list = Neturl.norm_path (Neturl.split_path uri) in
	let tree' = make_uri_tree spec' in
	if uri_list <> [] then
	  merged_uri_tree uri_list tree' service
	else tree'  (* i.e. uri = "" is silently ignored *)
    | [] ->
	StrMap.empty

and merged_uri_tree l t service =  (* merge l into t *)
  match l with
    | [x] ->
	let t_node_at_x =
	  try StrMap.find x t with Not_found -> { leaf = None; tree = StrMap.empty } in
	let new_t_node_at_x =
	  { leaf = Some service;
	    tree = t_node_at_x.tree;
	  } in
	StrMap.add x new_t_node_at_x t    (* replaces old binding, if any *)
    | x :: l' ->
	let t_node_at_x =
	  try StrMap.find x t with Not_found -> { leaf = None; tree = StrMap.empty } in
	let new_t_node_at_x =
	  { leaf = t_node_at_x.leaf;
	    tree = merged_uri_tree l' t_node_at_x.tree service;
	  } in
	StrMap.add x new_t_node_at_x t    (* replaces old binding, if any *)
    | [] ->
	assert false

let rec find_uri_service uri_list uri_tree =
  (* Finds the prefix of [uri_list] in [uri_tree] serving the request *)
  match uri_list with
    | [] ->
	raise Not_found
    | directory :: uri_list' ->
	let node = 
	  try
	    (* Search ..../<directory>: *)
	    StrMap.find directory uri_tree  (* or Not_found *)
	  with
	      Not_found ->
		(* Search ..../: (i.e. trailing slash) *)
		let node' = StrMap.find "" uri_tree in
		if not (StrMap.is_empty node'.tree) then raise Not_found;
		node'
	in

	( match node.leaf with
	    | Some service ->
		(* Try to find a more specific service *)
		( try
		    find_uri_service uri_list' node.tree
		  with
		      Not_found -> service
		)
	    | None ->
		find_uri_service uri_list' node.tree
	)

exception Bad_uri_escaping	
    
let uri_distributor ( spec : 'a uri_distributor ) = 
  let uri_tree = make_uri_tree spec in
object(self)
  method name = "uri_distributor"
  method def_term = `Uri_distributor spec
  method print fmt =
    Format.fprintf fmt "@[<hv 4>uri_distributor(";
    List.iter
      (fun (uri,service) ->
	 Format.fprintf fmt "@ @[<hv 4>uri(%s =>@ " uri;
	 service # print fmt;
	 Format.fprintf fmt "@]@ )";
      )
      spec;
    Format.fprintf fmt "@]@ )"

  method process_header env =
    (* Do path normalization, and if there is something to do, redirect: *)
    try
      let req_path_esc = env # cgi_script_name in
      let req_path = 
	try uripath_decode req_path_esc
	with Failure _ -> raise Bad_uri_escaping
      in
      let req_uri_list = Neturl.split_path req_path in
      let req_uri_list_norm = Neturl.norm_path req_uri_list in
      let req_uri_norm = Neturl.join_path req_uri_list_norm in
      (* Safety checks *)
      ( match req_uri_list_norm with
	  | [] ->
	      (* i.e. "." - but empty URIs are generally forbidden *)
	      `Std_response(`Not_found, None, (Some "Nethttpd: Non-absolute URI"))
	  | [ ".." ] ->
	      (* i.e. URI begins with ".." *)
	      `Std_response(`Not_found, None, (Some "Nethttpd: Non-absolute URI"))
	  | [ ""; ".." ] ->
	      (* i.e. URI begins with "/.." *)
	      `Std_response(`Not_found, None, (Some "Nethttpd: URI begins with /.."))
	  | _ ->
	      (* Everything else is acceptable. Now perform the redirection if
	       * the URI changed by normalization:
	       * CHECK: Maybe it is better not to redirect, but to derive a new
	       * environment.
	       *)
	      if req_uri_norm <> req_path then (
		let qs =  env#cgi_query_string in
		let qm_qs = if qs = "" then "" else "?" ^ qs in
		let req_uri_esc = uripath_encode req_uri_norm ^ qm_qs in
		raise(Redirect_request(req_uri_esc, env # input_header)));
	      (* Search the URI to match: *)
	      ( match
		  ( try
		      Some(find_uri_service req_uri_list_norm uri_tree)
		    with Not_found -> None
		  )
		with
		  | Some service ->
		      service # process_header env
		  | None ->
		      `Std_response(`Not_found, None, (Some "Nethttpd: No service bound to URI"))
	      )
      )
    with
      | Bad_uri_escaping ->
	  `Std_response(`Not_found, None, (Some "Nethttpd: Bad URI escape sequences"))
end


type 'a linear_distributor =
    ( (extended_environment -> bool) * 'a http_service ) list

let linear_distributor ( spec : 'a linear_distributor ) = 
object(self)
  method name = "linear_distributor"
  method def_term = `Linear_distributor spec
  method print fmt =
    Format.fprintf fmt "@[<hv 4>linear_distributor(";
    List.iter
      (fun (_,service) ->
	 Format.fprintf fmt "@ @[<hv 4>conditional(??? =>@ ";
	 service # print fmt;
	 Format.fprintf fmt "@]@ )";
      )
      spec;
    Format.fprintf fmt "@]@ )"

  method process_header env =
    match
      ( try
	  Some (List.find (fun (cond, service) -> cond env) spec)
	with Not_found -> None
      )
    with
      | Some(_, service) ->
	  service # process_header env
      | None ->
	  `Std_response(`Not_found, None, (Some "Nethttpd: No service matches in linear distribution"))
end

type method_filter =
    [ `Limit of string list
    | `Limit_except of string list
    ]

type 'a method_distributor =
   ( method_filter * 'a http_service ) list

let method_distributor ( spec : 'a method_distributor ) = 
object(self)
  method name = "method_distributor"
  method def_term = `Method_distributor spec
  method print fmt =
    Format.fprintf fmt "@[<hv 4>method_distributor(";
    List.iter
      (fun (rule,service) ->
	 Format.fprintf fmt "@ @[<hv 4>method(%s =>@ "
	   (match rule with
	      | `Limit l -> "+" ^ String.concat "," l
	      | `Limit_except l -> "-" ^ String.concat "," l);
	 service # print fmt;
	 Format.fprintf fmt "@]@ )";
      )
      spec;
    Format.fprintf fmt "@]@ )"

  method process_header env =
    let rule_matches =
      function
	| `Limit l ->
	    let req_method = env # cgi_request_method in
	    List.mem req_method l
	| `Limit_except l ->
	    let req_method = env # cgi_request_method in
	    not(List.mem req_method l)
    in
    match
      ( try
	  Some (List.find (fun (rule, _) -> rule_matches rule) spec)
	with Not_found -> None
      )
    with
      | Some(_, service) ->
	  service # process_header env
      | None ->
	  `Std_response(`Not_found, None, (Some "Nethttpd: Method not bound"))
end


type std_activation_options =
   { stdactv_processing : Netcgi.arg_store option;
     stdactv_operating_type : Netcgi.output_type option;
   } 


type std_activation =
  [ `Std_activation of std_activation_options
  | `Std_activation_unbuffered
  | `Std_activation_buffered
  | `Std_activation_tempfile
  ]


type 'a dynamic_service =
    { dyn_handler : extended_environment -> 'a -> unit;
      dyn_activation :  extended_environment -> 'a;
      dyn_uri : string option;
      dyn_translator : string -> string;
      dyn_accept_all_conditionals : bool;
  } constraint 'a = # Netcgi.cgi_activation

let rec strip_prefix ~prefix l =
  match prefix, l with
    | [], l -> l
    | (p :: prefix'), (x :: l') ->
	if p = x then
	  strip_prefix ~prefix:prefix' l'
	else
	  raise Not_found
    | _, [] ->
	raise Not_found


let std_activation tag =
  match tag with
    | `Std_activation opts ->
	(fun env ->
	   let out_type =
	     match opts.stdactv_operating_type with
	       | None -> `Direct ""
	       | Some p -> p in
	   let arg_store =
	     match opts.stdactv_processing with
	       | None -> (fun _ _ _ -> `Automatic)
	       | Some f -> f in
	   Netcgi_common.cgi_with_args 
	     (new Netcgi_common.cgi)
	     (env :> Netcgi.cgi_environment)
	     out_type
	     env#input_channel
	     arg_store
	)
    | `Std_activation_unbuffered ->
	(fun env ->
	   Netcgi_common.cgi_with_args 
	     (new Netcgi_common.cgi)
	     (env :> Netcgi.cgi_environment)
	     (`Direct "")
	     env#input_channel
	     (fun _ _ _ -> `Automatic)
	)
    | `Std_activation_buffered ->
	(fun env ->
	   Netcgi_common.cgi_with_args 
	     (new Netcgi_common.cgi)
	     (env :> Netcgi.cgi_environment)
	     Netcgi.buffered_transactional_outtype
	     env#input_channel
	     (fun _ _ _ -> `Automatic)
	)
    | `Std_activation_tempfile ->
	(fun env ->
	   Netcgi_common.cgi_with_args 
	     (new Netcgi_common.cgi)
	     (env :> Netcgi.cgi_environment)
	     Netcgi.tempfile_transactional_outtype
	     env#input_channel
	     (fun _ _ _ -> `Automatic)
	)


class dynamic_out_channel enabled out_ch : Netchannels.out_obj_channel =
  (* if enabled is set to false, output is suppressed *)
object(self)
  method output s p len =
    if !enabled then
      out_ch # output s p len
    else
      len
  method flush() =
    if !enabled then
      out_ch # flush()
  method close_out() =
    if !enabled then
      out_ch # close_out()
  method pos_out =
    out_ch # pos_out
  method really_output s p len =
    if !enabled then
      out_ch # really_output s p len
  method output_char c =
    if !enabled then
      out_ch # output_char c
  method output_string s =
    if !enabled then
      out_ch # output_string s
  method output_byte b =
    if !enabled then
      out_ch # output_byte b
  method output_buffer b =
    if !enabled then
      out_ch # output_buffer b
  method output_channel ?len ch =
    if !enabled then
      out_ch # output_channel ?len ch
end


class dynamic_env_wrapper (env:extended_environment) properties =
  let in_channel = env#input_channel in
  let out_enabled = ref true in
  let out_channel = new dynamic_out_channel out_enabled env#output_ch in
object(self)
  inherit redirected_environment ~in_channel ~properties env as super

  method send_output_header() =
    (* Check for CGI-type redirection. In this case we have to suppress 
       any output. (see also below)
     *)
    ( try
	let loc = self # output_header_field "Location" in (* or Not_found *)
	if loc = "" || loc.[0] <> '/' then raise Not_found;
	dlogr (fun () -> 
		 sprintf "env-%d dynamic_env_wrapper suppressing output"
		   (Oo.id env));
	out_enabled := false;  (* suppress output *)
      with
	  Not_found ->
	    super # send_output_header()
    )

  method output_ch = out_channel
  method out_channel = out_channel
end


let dynamic_service_impl spec =
object(self)
  method name = "dynamic_service"
  method def_term = `Dynamic_service spec
  method print fmt =
    Format.fprintf fmt "@[<hv 4>dynamic_service(";
    ( match spec.dyn_uri with
	| None -> ()
	| Some uri -> Format.fprintf fmt "@ uri(%s)" uri
    );
    Format.fprintf fmt "@ accept_all_conditionals(%b)" spec.dyn_accept_all_conditionals;
    Format.fprintf fmt "@]@ )"

  method process_header (env : extended_environment) =
    dlogr (fun () ->
	     sprintf "env-%d process_header dynamic_service"
	       (Oo.id env));
    try
      let req_path_esc = env#cgi_script_name in
      let _req_path = 
	try uripath_decode req_path_esc 
	with Failure _ -> raise Not_found in

      let req_method = env # cgi_request_method in
      let allowed = 
	List.map 
	  Netcgi_common.string_of_http_method 
	  (env # config).Netcgi.permitted_http_methods in
      if not (List.mem req_method allowed) then (
	let h = new Netmime.basic_mime_header [] in
	set_allow h allowed;
	raise (Standard_response(`Method_not_allowed,Some h,(Some "Nethttpd: Method not allowed for dynamic service")));
      );

      if not spec.dyn_accept_all_conditionals then (
	if env # multiple_input_header_field "If-match" <> [] then
	  raise(Standard_response(`Precondition_failed,None,None));
	if env # multiple_input_header_field "If-unmodified-since" <> [] then
	  raise(Standard_response(`Precondition_failed,None,None));
      );

      dlogr (fun () ->
	       sprintf "env-%d process_header dynamic_service accepts %s %s"
		 (Oo.id env) req_method req_path_esc);
      (`Accept_body(self :> http_service_receiver) : http_service_reaction)
    with
      | Not_found ->
	  `Std_response(`Not_found, None,(Some "Nethttpd: Cannot decode request"))
      | Standard_response(status,hdr_opt,errmsg_opt) ->
	  `Std_response(status,hdr_opt,errmsg_opt)

  val response_param = None

  method process_body env =
    dlogr (fun () ->
	     sprintf "env-%d process_body dynamic_service"
	       (Oo.id env));
    (* Set PATH_INFO and PATH_TRANSLATED: *)
    let props =
      match spec.dyn_uri with
	| Some dyn_uri ->
	    let req_path_esc = env#cgi_script_name in
	    let req_path = uripath_decode req_path_esc in
	    let req_path_list = Neturl.split_path req_path in
	    let dyn_path_list = Neturl.split_path dyn_uri in
	    let path_list = 
	      try "" :: (strip_prefix ~prefix:dyn_path_list req_path_list)
	      with
		  Not_found -> []
	    in
	    let path = Neturl.join_path path_list in
	    let path_esc = uripath_encode path in
	    let path_translated = spec.dyn_translator path in

	    let properties =
	      update_alist
		[ "PATH_INFO", path_esc;
		  "PATH_TRANSLATED", path_translated;
		  "SCRIPT_NAME", uripath_encode dyn_uri
		]
		env#cgi_properties in

	    properties
	| None -> 
	    env#cgi_properties in
    let fenv = new dynamic_env_wrapper env props in
    let cgi = spec.dyn_activation fenv in

    dlogr (fun () ->
	     sprintf "env-%d process_body dynamic_service cgi=%d fixed_env=%d"
	       (Oo.id env) (Oo.id cgi) (Oo.id fenv));

    (* We cannot set here response_param directly because this object
       is globally used by all incoming requests
     *)
    let self' =
      {< response_param = Some (cgi,fenv) >} in
    (self' :> http_service_generator)

  method generate_response env =

    dlogr (fun () ->
	     sprintf "env-%d generate_response dynamic_service"
	       (Oo.id env));

    match response_param with
      | Some (cgi,fenv) ->
	  dlogr (fun () ->
		   sprintf "env-%d generate_response dynamic_service calling handler with cgi=%d fixed_env=%d"
		     (Oo.id env) (Oo.id cgi) (Oo.id fenv));
	  spec.dyn_handler fenv cgi;
	  dlogr (fun () ->
		   sprintf "env-%d generate_response dynamic_service \
                            back from handler"
		     (Oo.id env));

	  (* Check for CGI-type redirection. In this case we have to do
             the actual redirection (see also dynamic_env_wrapper)
	   *)
	  ( try
	      let loc = 
		fenv # output_header_field "Location" in (* or Not_found *)
	      if loc = "" || loc.[0] <> '/' then raise Not_found;
	      fenv # output_header # set_fields [];  (* Reset output *)
	      raise(Redirect_response(loc, fenv # input_header))
	    with
		Not_found ->
		  ()
	  )

      | _ ->
	  failwith "Activation object is missing"
end

let dynamic_service spec = 
  (dynamic_service_impl spec :> 'a http_service)

type file_option =
    [ `Enable_gzip
    | `Enable_cooked_compression
    | `Override_compression_suffixes of (string * string) list
    | `Enable_index_file of string list
    | `Enable_listings of 	
	extended_environment -> Netcgi.cgi_activation -> file_service -> unit
    ]

and file_service =
    { file_docroot : string;
      file_uri : string;
      file_suffix_types : (string * string) list;
      file_default_type : string;
      file_options : file_option list;
    }

let file_translator spec uri =
  let rem_slash s =
    let s1 =
      if s<>"" && s.[0] = '/' then
	String.sub s 1 (String.length s - 1)
      else
	s in
    let s2 =
      if s1 <> "" && s1.[String.length s1-1] = '/' then
	String.sub s1 0 (String.length s1-1)
      else
	s1 in
    s2 in
  let concat p1 p2 =
    if p2="" then p1 else Filename.concat p1 p2 in
  let rec translate pat_l l =
    match (pat_l, l) with
      | ([], [""]) ->
	  spec.file_docroot
      | ([], path) ->
	  concat spec.file_docroot (rem_slash(Neturl.join_path path))
      | ([""], path) ->
	  concat spec.file_docroot (rem_slash(Neturl.join_path path))
      | (pat_dir :: pat_l', dir :: l') when pat_dir = dir ->
	  translate pat_l' l'
      | _ ->
	  raise Not_found 
  in
  let uri_list = Neturl.norm_path (Neturl.split_path uri) in
  match uri_list with
    | [] ->
	(* i.e. "." - but empty URIs are generally forbidden *)
	raise Not_found
    | [ ".." ] ->
	(* i.e. URI begins with ".." *)
	raise Not_found
    | [ ""; ".." ] ->
	(* i.e. URI begins with "/.." *)
	raise Not_found
    | s :: _ when s <> "" ->
	(* i.e. URI does not begin with "/" *)
	raise Not_found
    | _ ->
	(* ok, translate that *)
	let spec_uri_list = Neturl.norm_path (Neturl.split_path spec.file_uri) in
	translate spec_uri_list uri_list

let ext_re = Netstring_str.regexp ".*\\.\\([^.]+\\)$";;

let get_extension s =
  match Netstring_str.string_match ext_re s 0 with
    | None ->
	None
    | Some m ->
	Some(Netstring_str.matched_group m 1 s)

let merge_byte_ranges st ranges =
  (* Merge the byte [ranges] into a single range. Returns [Some (first,last)] if
   * the range is satisfiable, else [None].
   *)
  let size = st.Unix.LargeFile.st_size in
  let max_pos = Int64.pred size in
  let rec merge ranges =
    match ranges with
      | (first_pos_opt, last_pos_opt) :: ranges' ->
	  let (first_pos, last_pos) =
	    match (first_pos_opt, last_pos_opt) with
	      | (Some fp, Some lp) -> (fp, lp)
	      | (Some fp, None)    -> (fp, max_pos)
	      | (None, Some lp)    -> (Int64.sub size lp, max_pos)
	      | (None, None)       -> assert false
	  in
	  let first_pos' = max 0L (min first_pos max_pos) in
	  let last_pos' = max 0L (min last_pos max_pos) in
	  if first_pos' <= last_pos' then (
	    match merge ranges' with
	      | None -> 
		  Some(first_pos', last_pos')
	      | Some(first_pos'', last_pos'') ->
		  Some(min first_pos' first_pos'', max last_pos' last_pos'')
	  )
	  else
	    (* This range is void, try next range *)
	    merge ranges'
      | [] ->
	  None
  in
  merge ranges

let w32_fix_trailing_slash s =
  (* background: Win32 dislikes Unix.stat "directory/". It is unclear whether
     this function is still required - file_translator is now changed so
     that trailing slashes are normally not returned.
   *)
  if Sys.os_type = "Win32" then (
    if s <> "" && s <> "/" && s.[ String.length s - 1 ] = '/' then
      s ^ "."
    else
      s
  )
  else s


let get_compression_suffixes file_options =
  let rec find opts =
    match opts with
      | `Override_compression_suffixes l :: _ -> l
      | _ :: opts' -> find opts'
      | [] ->
	  [ "gz", "gzip";
	    "bz2", "bzip2";
	    "Z", "compress"
	  ] in
  find file_options


let rec search_cooked_file filename compression_suffixes =
  match compression_suffixes with
    | [] ->
	[ "identity", filename ]
    | (suffix, ce) :: suffixes' ->
	try
	  let filename_ext = filename ^ "." ^ suffix in
	  let fd = Unix.openfile filename_ext [ Unix.O_RDONLY] 0 in
	  (* or Unix_error *)
	  Unix.close fd;
	  (ce, filename_ext) :: search_cooked_file filename suffixes'
	with
	  | Unix.Unix_error(_,_,_) ->
	      search_cooked_file filename suffixes'


let file_service (spec : file_service) =
object(self)
  method name = "file_service"
  method def_term = `File_service spec
  method print fmt =
    Format.fprintf fmt "@[<hv 4>file_service(";
    Format.fprintf fmt "@ docroot(%s)" spec.file_docroot;
    Format.fprintf fmt "@ uri(%s)" spec.file_uri;
    Format.fprintf fmt "@ @[<hv 4>suffix_types(";
    List.iter
      (fun (suff,t) -> Format.fprintf fmt "@ %s => %s" suff t)
      spec.file_suffix_types;
    Format.fprintf fmt "@]@ )";
    Format.fprintf fmt "@ default_type(%s)" spec.file_default_type;
    Format.fprintf fmt "@ @[<hv 4>options(";
    List.iter
      (function
	 | `Enable_gzip       -> Format.fprintf fmt "@ enable_gzip"
	 | `Enable_cooked_compression -> 
	     Format.fprintf fmt "@ enable_cooked_compression"
	 | `Enable_index_file _ -> Format.fprintf fmt "@ enable_index_file"
	 | `Enable_listings _ -> Format.fprintf fmt "@ enable_listings"
	 | `Override_compression_suffixes _ ->
	     Format.fprintf fmt "@ override_compression_suffixes"
      )
      spec.file_options;
    Format.fprintf fmt "@]@ )";
    Format.fprintf fmt "@]@ )";

  method process_header env =
    try
      let req_path_esc = env#cgi_script_name in
      let req_path = 
	try uripath_decode req_path_esc with Failure _ -> raise Not_found in
      let filename =
	file_translator spec req_path in (* or Not_found *)
      let s = Unix.LargeFile.stat (w32_fix_trailing_slash filename) in
      (* or Unix_error *)
      ( match s.Unix.LargeFile.st_kind with
	  | Unix.S_REG ->
	      self # serve_regular_file env filename s
	  | Unix.S_DIR ->
	      self # serve_directory env filename s
	  | _ ->
	      (* other types are illegal *)
	      raise Not_found
      )
    with
      | Not_found ->
	  `Std_response(`Not_found, None,(Some "Nethttpd: Can neither translate to regular file nor to directory") )
      | Unix.Unix_error(Unix.ENOENT,_,_) ->
	  `Std_response(`Not_found, None, (Some "Nethttpd: No such file or directory"))
      | Unix.Unix_error((Unix.EACCES | Unix.EPERM),_,_) ->
	  `Std_response(`Forbidden, None, (Some "Nethttpd: File access denied"))
      | Unix.Unix_error(e,_,_) ->
	  `Std_response(`Internal_server_error, None, (Some ("Nethttpd: Unix error: " ^ Unix.error_message e)))
      | Standard_response(status,hdr_opt,errmsg_opt) ->
	  if status = `Ok then
	    `Static(`Ok,hdr_opt,"")
	  else
	    `Std_response(status,hdr_opt,errmsg_opt)

  method private serve_regular_file env filename s =
    (* Regular file: Check if we can open for reading *)
    let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in  (* or Unix_error *)
    Unix.close fd;
    (* If OPTIONS: Respond now *)
    let req_method = env # cgi_request_method in
    if req_method = "OPTIONS" then (
      env # set_output_header_field "Accept-ranges" "bytes";
      raise(Standard_response(`Ok, None, None));
    );
    (* Check request method: Only GET and HEAD are supported *)
    if req_method <> "GET" && req_method <> "HEAD" then (
      let h = new Netmime.basic_mime_header [] in
      set_allow h [ "GET"; "HEAD"; "OPTIONS" ];
      raise(Standard_response(`Method_not_allowed,Some h,
			      (Some "Nethttpd: Method not allowed for file"))));
    (* Set [Accept-ranges] header: *)
    env # set_output_header_field "Accept-ranges" "bytes";
    (* Figure out file extension, content encoding (compression type)
       and media type
     *)
    let media_type_of_ext ext =
      try List.assoc ext spec.file_suffix_types
      with Not_found -> spec.file_default_type in
    let compression_suffixes =
      get_compression_suffixes spec.file_options in
    let ext_opt = get_extension filename in
    let content_encoding, media_type =
      match ext_opt with
	| Some ext when List.mem_assoc ext compression_suffixes ->
	    let ce = List.assoc ext compression_suffixes in
	    let filename1 = Filename.chop_extension filename in
	    let ext1_opt = get_extension filename1 in
	    ( match ext1_opt with
		| None -> (ce, spec.file_default_type)
		| Some ext1 -> (ce, media_type_of_ext ext1)
	    )
	| Some ext ->
	    "identity", media_type_of_ext ext
	| None ->
	    "identity", spec.file_default_type in
    env # set_output_header_field "Content-type" media_type;
    if content_encoding <> "identity" then
      env # set_output_header_field "Content-Encoding" content_encoding;

    (* Generate the (weak) validator from the file statistics: *)
    let etag =
      `Weak (sprintf "%d-%Lu-%.0f"
	       s.Unix.LargeFile.st_ino
	       s.Unix.LargeFile.st_size
	       s.Unix.LargeFile.st_mtime) in
    set_etag env#output_header etag;
    set_last_modified env#output_header s.Unix.LargeFile.st_mtime;
    (* Check for conditional and partial GET *)
    (* In order of decreasing priority:
     * If-Match: If present, we always respond with code 412. This condition
     *   requires the availablity of strong validators.
     * If-Unmodified-Since: If present, we check the dates, and if passing
     *   the GET will be carried out.
     * If-Modified-Since and If-None-Match: The results of the individual
     *   tests are ORed (accept when either of the tests accepts):
     *   +--------------+---------------+-----------+
     *   | If-Mod-Since | If-None-Match | Behaviour |
     *   +--------------+---------------+-----------+
     *   | modified     | none          | accept    |
     *   | unmodified   | none          | code 304  |
     *   | modified     | match         | accept    |
     *   | unmodified   | match         | code 304  |
     *   | none         | match         | code 304  |
     *   | modified     | no match      | accept    |
     *   | unmodified   | no match      | accept    |
     *   | none         | no match      | accept    |
     *   +--------------+---------------+-----------+
     *  (my interpretation of 14.26 of RFC 2616)
     *
     * If accepted, the second question is whether to return the whole
     * file or only a fragment:
     * If-Range + Range: (only if both headers are present)
     *   If the condition is fulfilled, return only the range, else the
     *   whole document.
     * Only Range: The range is satisfied whenever possible.
     * No Range: Return whole file. `Enable_gzip is only interpreted in this
     *   case.
     * 
     *)
    if env # multiple_input_header_field "If-match" <> [] then (
      raise(Standard_response(`Precondition_failed,None,None));
    );
    ( try
	let d = get_if_unmodified_since env#input_header in (* or Not_found *)
	if s.Unix.LargeFile.st_mtime > d then
	  raise(Standard_response(`Precondition_failed,None,None));
      with
	| Not_found -> ()
	| Bad_header_field _ -> ()
    );
    let accept_if_modified, have_if_modified =
      try
	let d = get_if_modified_since env#input_header in (* or Not_found *)
	s.Unix.LargeFile.st_mtime > d, true
      with
	| Not_found -> false, false
	| Bad_header_field _ -> false, false in
    let accept_if_none_match, have_if_none_match =
      try
	let if_etags = get_if_none_match env#input_header in (* or Not_found *)
	( match if_etags with
	    | None -> (* case: If-None-Match: * *)
		false, true
	    | Some l ->
		not (List.exists (weak_validator_match etag) l), true
	)
      with 
	| Not_found -> false, false
	| Bad_header_field _ -> false, false in
    if (have_if_modified || have_if_none_match) && 
      not (accept_if_modified || accept_if_none_match) 
    then
      raise(Standard_response(`Not_modified,None,None));
    (* Now the GET request is accepted! *)
    let partial_GET =
      try
	let ranges = get_range env#input_header in  (* or Not_found *)
	(* Ok, we can do a partial GET. Now check if this is not needed 
	 * because of If-Range:
	 *)
	( try
	    match get_if_range env#input_header with  (* or Not_found *)
	      | `Etag e ->
		  None
		    (* Because we do not have strong validators *)
	      | `Date d ->
		  if s.Unix.LargeFile.st_mtime <= d then
		    Some ranges
		  else
		    None
		      
	  with
	    | Not_found -> Some ranges
	    | Bad_header_field _ -> Some ranges
	)
      with
	| Not_found -> None
	| Bad_header_field _ -> None in
    (* So either serve partially or fully: *)
    ( match partial_GET with
	| Some(`Bytes ranges) ->
	    (* Partial GET: We do not support multipart/byteranges. Instead,
	     * all requested ranges are implicitly merged into a single one.
	     *)
	    let eff_range_opt = merge_byte_ranges s ranges in  (* TODO *)
	    ( match eff_range_opt with
		| Some ((first_pos,last_pos) as eff_range) ->
		    (* Serve the file fragment: *)
		    let h = env # output_header in
		    set_content_range h 
		      (`Bytes(Some eff_range, Some s.Unix.LargeFile.st_size));
		    let length = Int64.succ(Int64.sub last_pos first_pos) in
		    `File(`Partial_content, Some h, filename, first_pos, length)
		      
		| None ->
		    (* The range is not satisfiable *)
		    let h = env # output_header in
		    set_content_range h (`Bytes(None,
					        (Some s.Unix.LargeFile.st_size)
					       ));
		    `Std_response(`Requested_range_not_satisfiable, Some h, (Some "Nethttpd: Requested range is not satisfiable"))
	    )
	| None ->
	    (* Full GET *)
	    (* Check whether there is a gzip-encoded complementary file *)
	    let encodings_and_files =
	      if (List.mem `Enable_gzip spec.file_options ||
		    List.mem `Enable_cooked_compression spec.file_options) 
	      then
		search_cooked_file filename compression_suffixes
	      else
		[ "identity", filename ] in
	    let supported_encodings =
	      List.map fst encodings_and_files in
	    let encoding = best_encoding env#input_header supported_encodings in
	    let h = env # output_header in
	    ( match encoding with
		| "identity" ->
		    `File(`Ok, None, filename, 0L, s.Unix.LargeFile.st_size)
		| _ ->
		    let fn = List.assoc encoding encodings_and_files in
		    let st_gzip = Unix.LargeFile.stat fn in
		    h # update_field "Content-Encoding" encoding;
		    `File(`Ok, Some h, fn, 0L, st_gzip.Unix.LargeFile.st_size)
		| _ -> assert false
	    )
    )

  method private serve_directory env filename s =
    let index_files =
      (try List.flatten (List.map (function `Enable_index_file l -> l | _ -> []) 
			   spec.file_options)
       with Not_found -> []) in

    let abs_index_file_opt =
      try
	Some (List.find 
		(fun n -> Sys.file_exists(Filename.concat filename n)) 
		index_files)
      with
	  Not_found -> None in

    let gen_listings =
      try Some(List.find
		 (fun opt -> match opt with `Enable_listings _ -> true|_ -> false)
		 spec.file_options)
      with
	  Not_found -> None in

    if abs_index_file_opt <> None || gen_listings <> None then (
      let req_path_esc = env#cgi_script_name in
      let req_path = uripath_decode req_path_esc in
      (* If [req_path] does not end with a slash, perform a redirection: *)
      if req_path <> "" && req_path.[ String.length req_path - 1 ] <> '/' then (
	let h = new Netmime.basic_mime_header
	  [ "Location", 
	    sprintf "http://%s%s%s/"
	      env#cgi_server_name
	      ( match env#cgi_server_port with 
		  | Some p -> ":" ^ string_of_int p
		  | None -> "")
	      env#cgi_request_uri ] in
	raise(Standard_response(`Found, Some h, None));
      )
    );
    match (abs_index_file_opt, gen_listings) with
      | Some name, _ ->
	  (* Ok, redirect to the file *)
	  let req_path_esc = Neturl.split_path env#cgi_script_name in
	  let name_esc = [ uripath_encode name ] in
	  raise(Redirect_request(Neturl.join_path (req_path_esc @ name_esc), 
				 env # input_header))

      | None, Some (`Enable_listings generator) ->
	  (* If OPTIONS: Respond now *)
	  let req_method = env # cgi_request_method in
	  if req_method = "OPTIONS" then (
	    raise(Standard_response(`Ok, None, None));
	  );
	  (* Check request method: Only GET and HEAD are supported *)
	  let req_method = env # cgi_request_method in
	  if req_method <> "GET" && req_method <> "HEAD" then (
	    let h = new Netmime.basic_mime_header [] in
	    set_allow h [ "GET"; "HEAD" ];
	    raise (Standard_response(`Method_not_allowed,Some h,(Some "Nethttpd: Method not allowed for directory listing"))));
	  (* Generate contents: *)
	  let dyn_spec =
	    { dyn_handler = (fun env cgi -> generator env cgi spec);
	      dyn_activation = std_activation `Std_activation_unbuffered;
	      dyn_uri = Some "/";
	      dyn_translator = (fun _ -> filename);
	      dyn_accept_all_conditionals = false;
	    } in
	  let dyn_srv = dynamic_service dyn_spec in
	  dyn_srv # process_header env
	    (*
	      let (listing, listing_hdr) = generator env spec filename in
	    (* Generate the (weak) validator from the file statistics: *)
	      let etag =
	      `Weak (sprintf "%d-%Lu-%.0f"
	      s.Unix.LargeFile.st_ino
	      s.Unix.LargeFile.st_size
	      s.Unix.LargeFile.st_mtime) in
	      set_etag listing_hdr etag;
	    (* Refuse If-match and If-unmodified-since: *)
	      if env # multiple_input_header_field "If-match" <> [] then
	      raise(Standard_response(`Precondition_failed, None, None));
	      if env # multiple_input_header_field "If-unmodified-since" <> [] then
	      raise(Standard_response(`Precondition_failed, None, None));
	    (* Return contents: *)
	      `Static(`Ok, Some listing_hdr, listing)
	     *)

      | _ ->
	  (* Listings are forbidden: *)
	  `Std_response(`Forbidden, None, (Some "Nethttpd: Access to directories not configured") )
	    
end


let simple_listing ?(hide=[ "\\."; ".*~$" ]) env (cgi :Netcgi.cgi_activation) fs =
  let dirname = env # cgi_path_translated in
  let col_name = 30 in
  let col_mtime = 20 in
  let col_size = 10 in
  let regexps = List.map (fun re -> Netstring_str.regexp re) hide in
  let req_path_esc = env#cgi_path_info in
  let req_path = uripath_decode req_path_esc in
  let files = Sys.readdir (w32_fix_trailing_slash dirname) in
  let xfiles =
    Array.map
      (fun name ->
	 if List.exists 
	      (fun re -> Netstring_str.string_match re name 0 <> None) regexps 
	 then
	   `None
	 else
	   try
	     let st = Unix.LargeFile.stat (Filename.concat dirname name) in
	     match st.Unix.LargeFile.st_kind with
	       | Unix.S_REG ->
		   `Reg(name, st.Unix.LargeFile.st_mtime, st.Unix.LargeFile.st_size)
	       | Unix.S_DIR ->
		   `Dir(name, st.Unix.LargeFile.st_mtime)
	       | _ ->
		   `None
	   with
	       Unix.Unix_error(_,_,_) -> `None
      )
      files in
  let params =
    try Netencoding.Url.dest_url_encoded_parameters env#cgi_query_string
    with _ -> [] in
  let sort_param =
    try List.assoc "sort" params with Not_found -> "name" in
  let direction_param =
    try List.assoc "direction" params with Not_found -> "ascending" in
  let direction_factor =
    match direction_param with
      | "ascending" -> 1
      | "descending" -> (-1)
      | _ -> 0 in
  let rev_direction =
    match direction_param with
      | "ascending" -> "descending"
      | "descending" -> "ascending"
      | _ -> "" in
  let query_sort_name =
    if sort_param = "name" then
      "?sort=name&direction=" ^ rev_direction
    else
      "?sort=name&direction=ascending" in
  let query_sort_mtime =
    if sort_param = "mtime" then
      "?sort=mtime&direction=" ^ rev_direction
    else
      "?sort=mtime&direction=ascending" in
  let query_sort_size =
    if sort_param = "size" then
      "?sort=size&direction=" ^ rev_direction
    else
      "?sort=size&direction=ascending" in
  let cmp x y =
    match (x,y) with
      | `None, `None -> 0
      | `None, _ -> (-1)
      | _, `None -> 1
      | `Dir _, `Reg _ -> (-1)
      | `Reg _, `Dir _ -> 1
      | `Reg(xname,xmtime,xsize), `Reg(yname,ymtime,ysize) ->
	  direction_factor *
	  ( match sort_param with
	      | "name"  -> compare xname yname
	      | "mtime" -> compare xmtime ymtime
	      | "size"  -> compare xsize ysize
	      | _       -> 0
	  )
      | `Dir(xname,xmtime), `Dir(yname,ymtime) ->
	  direction_factor *
	  ( match sort_param with
	      | "name"  -> compare xname yname
	      | "mtime" -> compare xmtime ymtime
	      | _       -> 0
	  )
  in
  Array.stable_sort cmp xfiles;

  let esc_html = 
    Netencoding.Html.encode_from_latin1 in

  let link_to href n s =
    let s' = 
      if String.length s > n then
	String.sub s 0 n
      else
	s in
    sprintf "<a href=\"%s\">%s</a>%s"
      (esc_html href)
      s'
      (String.make (n-String.length s') ' ')
  in

  let nolink n s =
    let s' =
      if String.length s > n then
	String.sub s 0 n
      else
	s in
    s' ^ (String.make (n-String.length s') ' ')
  in

  let mkdate f =
    Netdate.format ~fmt:"%Y-%m-%d %H:%M" (Netdate.create f) in

  let mksize n =
    if n >= 1099511627776L then
      sprintf "%8.1fT" (Int64.to_float n /. 1099511627776.0)
    else
      if n >= 1073741824L then
	sprintf "%8.1fG" (Int64.to_float n /. 1073741824.0)
      else
	if n >= 1048576L then
	  sprintf "%8.1fM" (Int64.to_float n /. 1048576.0)
	else
	  if n >= 1024L then
	    sprintf "%8.1fk" (Int64.to_float n /. 1024.0)
	  else
	    if n >= 0L then
	      sprintf "%8.1f" (Int64.to_float n)
	    else
	      "-"
  in

  let out = cgi # output # output_string in
  cgi # set_header ();
  out "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" ";
  out "\"http://www.w3.org/TR/REC-html40/loose.dtd\">\n";
  out (sprintf "<html><head><title>Index of %s</title></head>\n" (esc_html req_path));
  out "<body bgcolor=\"#ffffff\" text=\"#000000\">\n";
  out (sprintf "<h3>Index of %s</h3>\n" (esc_html req_path));
  out "<pre>";
  out (sprintf "      %s %s %s\n\n"
    (link_to query_sort_name  col_name  "Name")
    (link_to query_sort_mtime col_mtime "Last Modified")
    (link_to query_sort_size  col_size  "Size"));
  if req_path <> "/" then
    out (sprintf "[DIR] %s %s %s\n"
      (link_to ".." col_name "Parent Directory")
      (nolink col_mtime "")
      (nolink col_size ""));
  Array.iter
    (function
       | `Reg(name, mtime, size) ->
	   let mtime_str = mkdate mtime in
	   let size_str = mksize size in
	   out (sprintf "[   ] %s %s %s\n"
	     (link_to name col_name name)
	     (nolink col_mtime mtime_str)
	     (nolink col_size size_str));
       | `Dir(name, mtime) ->
	   let mtime_str = mkdate mtime in
	   out (sprintf "[DIR] %s %s %s\n"
	     (link_to name col_name (name ^ "/"))
	     (nolink col_mtime mtime_str)
	     (nolink col_size "-"))
       | `None ->
	   ()
    )
    xfiles;
  out "</pre></body></html>\n";
  cgi # output # commit_work()


type ac_by_host_rule =
    [ `Allow of string list
    | `Deny of string list
    ]

type 'a ac_by_host = ac_by_host_rule * 'a http_service

let prepare_ac_by_host spec =
  let resolve host =
    try
      [ Unix.inet_addr_of_string host ]
    with
      | _ ->
	  ( try
	      let h = Uq_resolver.get_host_by_name host in
	      Array.to_list h.Unix.h_addr_list
	    with
	      | Uq_resolver.Host_not_found _ -> []
	  )
  in

  match spec with
    | `Allow hosts ->
	let ipaddrs = List.flatten (List.map resolve hosts) in
	`Allow_ip ipaddrs
    | `Deny hosts ->
	let ipaddrs = List.flatten (List.map resolve hosts) in
	`Deny_ip ipaddrs

let ac_by_host (spec, (srv : 'a http_service)) =
  let spec' = prepare_ac_by_host spec in
  ( object(self)
      method name = "ac_by_host"
      method def_term = `Ac_by_host (spec,srv)
      method print fmt =
	Format.fprintf fmt "ac_by_host(...)"
      method process_header env =
	let addr = env # remote_socket_addr in
	let allowed =
	  match spec' with
	    | `Allow_ip ipaddrs ->
		( match addr with
		    | Unix.ADDR_INET(ia,_) ->
			List.mem ia ipaddrs
		    | _ ->
			true
		)
	    | `Deny_ip ipaddrs -> 
		( match addr with
		    | Unix.ADDR_INET(ia,_) ->
			not(List.mem ia ipaddrs)
		    | _ ->
			true
		)
	in
	if allowed then
	  srv # process_header env
	else
	  `Std_response(`Forbidden, None, (Some "Nethttpd: Access denied by host rule"))
    end
  )


let ws_re = Netstring_str.regexp "[ \r\t\n]+"

let split_ws s =
  Netstring_str.split ws_re s

let read_media_types_file fname =
  let f = open_in fname in
  let l = ref [] in
  try
    while true do
      let line = input_line f in
      if line = "" || line.[0] <> '#' then (
	let words = split_ws line in
	match words with
	  | [] -> ()
	  | [ mtype ] -> ()
	  | mtype :: suffixes ->
	      l := (List.map (fun s -> (s,mtype)) (List.rev suffixes)) @ !l
      )
    done;
    assert false
  with
    | End_of_file ->
	close_in f;
	List.rev !l
    | error ->
	close_in f;
	raise error
;;
