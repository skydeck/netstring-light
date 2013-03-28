(* netcgi_compat.ml

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(* $Id: netcgi_compat.ml,v 1.12 2005/11/04 00:52:05 chris_77 Exp $ *)

exception Not_implemented of string

module Netcgi_env =
struct
  type input_mode = [ `Standard (* | `Direct *) ]

  type input_state =
      [ `Start
      | `Receiving_header | `Received_header
      | `Receiving_body | `Received_body      ]

  type output_mode = [ `Standard (* | `Direct *) ]

  type output_state =
      [ `Start
      | `Sending_header      | `Sent_header
      | `Sending_body        | `Sent_body
      | `Sending_part_header | `Sent_part_header
      | `Sending_part_body   | `Sent_part_body
      | `End
      ]

  type protocol_version = Nethttp.protocol_version
  type protocol_attribute = Nethttp.protocol_attribute
  type protocol = Nethttp.protocol
  type workaround =
      [ `Work_around_MSIE_Content_type_bug | `Work_around_backslash_bug  ]

  type cgi_config = {
    tmp_directory : string;
    tmp_prefix : string;
    permitted_http_methods : string list;
    permitted_input_content_types : string list;
    input_content_length_limit : int;
    workarounds : workaround list;
  }

  let default_config =
    let default_tmp_directory = Netsys_tmp.tmp_directory() in
    {
      tmp_directory = default_tmp_directory;
      tmp_prefix = "netcgi";
      permitted_http_methods = ["GET"; "HEAD"; "POST"];
      permitted_input_content_types = [ "multipart/form-data";
					"application/x-www-form-urlencoded" ];
      input_content_length_limit = max_int;
      workarounds = [ `Work_around_MSIE_Content_type_bug;
		      `Work_around_backslash_bug ]
    }

  let meth_of_string = function
    | "GET" -> `GET
    | "HEAD" -> `HEAD
    | "POST" -> `POST
    | "DELETE" -> `DELETE
    | "PUT" -> `PUT
    | m -> raise(Not_implemented ("Netcgi_env.of_compat_config: HTTP method name not convertible: " ^ m))


  let string_of_meth = function
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `POST -> "POST"
    | `DELETE -> "DELETE"
    | `PUT -> "PUT"


  let of_compat_config c = {
    Netcgi.tmp_directory = c.tmp_directory;
    Netcgi.tmp_prefix = c.tmp_prefix;
    Netcgi.permitted_http_methods =
      List.map meth_of_string c.permitted_http_methods;

    Netcgi.permitted_input_content_types = c.permitted_input_content_types;

    Netcgi.input_content_length_limit = c.input_content_length_limit;
    Netcgi.max_arguments = 10000;

    Netcgi.workarounds =
      List.map (function
		| `Work_around_MSIE_Content_type_bug -> `MSIE_Content_type_bug
		| `Work_around_backslash_bug -> `Backslash_bug
	       ) c.workarounds;

    Netcgi.default_exn_handler = false
  }


  let to_compat_config c = {
    tmp_directory = c.Netcgi.tmp_directory;
    tmp_prefix = c.Netcgi.tmp_prefix;
    permitted_http_methods =
      List.map string_of_meth c.Netcgi.permitted_http_methods;

    permitted_input_content_types = c.Netcgi.permitted_input_content_types;

    input_content_length_limit = c.Netcgi.input_content_length_limit;

    workarounds =
      List.map (function
		| `MSIE_Content_type_bug -> `Work_around_MSIE_Content_type_bug
		| `Backslash_bug -> `Work_around_backslash_bug
		| ( `Work_around_MSIE_Content_type_bug
		  | `Work_around_backslash_bug ) as x -> x
	       ) c.Netcgi.workarounds
  }


  class type cgi_environment =
  object
    method config : cgi_config
    method cgi_gateway_interface  : string
    method cgi_server_software    : string
    method cgi_server_name        : string
    method cgi_server_protocol    : string
    method cgi_server_port        : int option
    method cgi_request_method     : string
    method cgi_path_info          : string
    method cgi_path_translated    : string
    method cgi_script_name        : string
    method cgi_query_string       : string
    method cgi_remote_host        : string
    method cgi_remote_addr        : string
    method cgi_auth_type          : string
    method cgi_remote_user        : string
    method cgi_remote_ident       : string
    method cgi_property          : ?default:string -> string -> string
    method cgi_properties : (string * string) list
    method cgi_https              : bool
    method cgi_request_uri : string
    method protocol : protocol
    method input_header : Netmime.mime_header
    method input_header_field : ?default:string -> string -> string
    method multiple_input_header_field : string -> string list
    method input_header_fields : (string * string) list
    method user_agent : string
    method cookies : (string * string) list
    method input_content_length : int
    method input_content_type_string : string
    method input_content_type : (string * (string * Mimestring.s_param) list)
    method input_ch : Netchannels.in_obj_channel
    method input_state : input_state
    method set_input_state : input_state -> unit
    method output_header : Netmime.mime_header
    method output_header_field : ?default:string -> string -> string
    method multiple_output_header_field : string -> string list
    method output_header_fields : (string * string) list
    method set_output_header_field : string -> string -> unit
    method set_multiple_output_header_field : string -> string list -> unit
    method set_output_header_fields : (string * string) list -> unit
    method set_status : Nethttp.http_status -> unit
    method send_output_header : unit -> unit
    method output_ch : Netchannels.out_obj_channel
    method output_state : output_state
    method set_output_state : output_state -> unit
    method log_error : string -> unit
  end


  class to_compat_environment (env:Netcgi.cgi_environment) =
    let config = to_compat_config env#config in
  object
    val env = env

    method config = config

    method cgi_gateway_interface  = env#cgi_gateway_interface
    method cgi_server_software    = env#cgi_server_software
    method cgi_server_name        = env#cgi_server_name
    method cgi_server_protocol    = env#cgi_server_protocol
    method cgi_server_port        = env#cgi_server_port
    method cgi_request_method     = env#cgi_request_method
    method cgi_path_info          = env#cgi_path_info
    method cgi_path_translated    = env#cgi_path_translated
    method cgi_script_name        = env#cgi_script_name
    method cgi_query_string       = env#cgi_query_string
    method cgi_remote_host        = env#cgi_remote_host
    method cgi_remote_addr        = env#cgi_remote_addr
    method cgi_auth_type          = env#cgi_auth_type
    method cgi_remote_user        = env#cgi_remote_user
    method cgi_remote_ident       = env#cgi_remote_ident
    method cgi_property           = env#cgi_property
    method cgi_properties 	  = env#cgi_properties
    method cgi_https              = env#cgi_https
    method cgi_request_uri        = env#cgi_property ~default:"" "REQUEST_URI"
    method protocol = env#protocol

    method input_header = env#input_header

    method input_header_field = env#input_header_field
    method multiple_input_header_field = env#multiple_input_header_field
    method input_header_fields = env#input_header_fields
    method user_agent = env#user_agent
    method cookies =
      List.map (fun c -> (Netcgi.Cookie.name c, Netcgi.Cookie.value c))
	env#cookies
    method input_content_length = env#input_content_length
    method input_content_type_string = env#input_content_type_string
    method input_content_type = env#input_content_type()

    method input_ch =
      (raise (Not_implemented "input_ch (of cgi_environment)") 
	 : Netchannels.in_obj_channel)
    method input_state =
      (raise (Not_implemented "input_state (of cgi_environment)")
	 : input_state)
    method set_input_state =
      (raise (Not_implemented "set_input_state (of cgi_environment)")
	 : input_state -> unit)

    method output_header = env#output_header
    method output_header_field = env#output_header_field
    method multiple_output_header_field = env#multiple_output_header_field
    method output_header_fields = env#output_header_fields
    method set_output_header_field = env#set_output_header_field
    method set_multiple_output_header_field =
      env#set_multiple_output_header_field
    method set_output_header_fields = env#set_output_header_fields
    method set_status = env#set_status
    method send_output_header = env#send_output_header

    method output_ch = env#out_channel
    method output_state =
      (raise (Not_implemented "output_state (of cgi_environment)")
	 : output_state)
    method set_output_state =
      (raise (Not_implemented "set_output_state (of cgi_environment)")
	 : output_state -> unit)
    method log_error = env#log_error
  end


  let to_compat_environment =
    new to_compat_environment

  let of_compat_environment (env:cgi_environment) : Netcgi.cgi_environment =
  object(self)
    method config = of_compat_config env#config
    method cgi_gateway_interface  = env#cgi_gateway_interface
    method cgi_server_software    = env#cgi_server_software
    method cgi_server_name        = env#cgi_server_name
    method cgi_server_protocol    = env#cgi_server_protocol
    method cgi_server_port        = env#cgi_server_port
    method cgi_request_method     = env#cgi_request_method
    method cgi_path_info          = env#cgi_path_info
    method cgi_path_translated    = env#cgi_path_translated
    method cgi_script_name        = env#cgi_script_name
    method cgi_query_string       = env#cgi_query_string
    method cgi_remote_host        = env#cgi_remote_host
    method cgi_remote_addr        = env#cgi_remote_addr
    method cgi_auth_type          = env#cgi_auth_type
    method cgi_remote_user        = env#cgi_remote_user
    method cgi_remote_ident       = env#cgi_remote_ident
    method cgi_property           = env#cgi_property
    method cgi_properties 	  = env#cgi_properties
    method cgi_https              = env#cgi_https
    method protocol               = env#protocol

    method input_header           = env#input_header
    method input_header_field     = env#input_header_field
    method multiple_input_header_field = env#multiple_input_header_field
    method input_header_fields    = env#input_header_fields
    method user_agent             = env#user_agent
    method input_content_length   = env#input_content_length
    method input_content_type_string = env#input_content_type_string
    method input_content_type()   = env#input_content_type

    method output_header          = env#output_header
    method output_header_field    = env#output_header_field
    method multiple_output_header_field = env#multiple_output_header_field
    method output_header_fields   = env#output_header_fields
    method set_output_header_field = env#set_output_header_field
    method set_multiple_output_header_field =
                                    env#set_multiple_output_header_field
    method set_output_header_fields = env#set_output_header_fields
    method set_status             = env#set_status

    val mutable header_sent = false
    method send_output_header()   =
      if not header_sent then (
	env#send_output_header();
	header_sent <- true
      )
    method output_ch              = env#output_ch
    method out_channel            = env#output_ch

    method log_error              = env#log_error

    method cookies =
      List.map
	(fun (n,v) -> Netcgi_common.Cookie.make n v)
	env#cookies

    method cookie n = 
      List.find (fun c -> Netcgi_common.Cookie.name c = n) self#cookies
  end
end



module Netcgi_types =
struct
  class type simple_message = Netmime.mime_body

  type store = [ `Memory | `File of string ]
  type representation =
      [ `Simple of simple_message | `MIME of Netmime.mime_message ]

  class type cgi_argument =
  object
    method name : string
    method value : string
    method open_value_rd : unit -> Netchannels.in_obj_channel
    method ro : bool
    method store : store
    method content_type : string
    method content_type_params : (string * Mimestring.s_param) list
    method charset : string
    method filename : string option
    method representation : representation
    method finalize : unit -> unit
    method set_value : string -> unit
    method open_value_wr : unit -> Netchannels.out_obj_channel
  end


  let to_compat_argument (arg:Netcgi.cgi_argument) =
  object
    val arg = arg

    method name = arg#name
    method value = arg#value
    method open_value_rd = arg#open_value_rd
    method ro = true
    method store = arg#store
    method content_type = fst(arg#content_type())
    method content_type_params = snd(arg#content_type())
    method charset = arg#charset
    method filename = arg#filename
    method representation = arg#representation
    method finalize = arg#finalize
    method set_value =
      (raise(Netmime.Immutable "Netcgi_types.cgi_argument"): string -> unit)
    method open_value_wr =
      (raise(Netmime.Immutable "Netcgi_types.cgi_argument"):
	 unit -> Netchannels.out_obj_channel)
  end


  let of_compat_argument (arg:cgi_argument) : Netcgi.cgi_argument =
  object
    val arg = arg
    method name = arg#name
    method value = arg#value
    method open_value_rd = arg#open_value_rd
    method store = arg#store
    method content_type () = (arg#content_type, arg#content_type_params)
    method charset = arg#charset
    method filename = arg#filename
    method representation = arg#representation
    method finalize = arg#finalize
  end

  type cgi_cookie = Nethttp.netscape_cookie = {
    cookie_name : string;
    cookie_value : string;
    cookie_expires : float option;
    cookie_domain : string option;
    cookie_path : string option;
    cookie_secure : bool;
  }

  type status = Nethttp.http_status

  type request_method = [ `GET | `HEAD | `POST | `DELETE
  | `PUT of cgi_argument ]

  type cache_control = [ `No_cache | `Max_age of int | `Unspecified ]

  type query_string_spec =
      [ `Initial | `Current | `Args of cgi_argument list | `None ]

  type other_url_spec = [ `Env | `This of string | `None ]


  class type cgi_activation =
  object
    method environment : Netcgi_env.cgi_environment
    method request_method : request_method
    method initial_arguments : (string * cgi_argument) list
    method initial_argument : string -> cgi_argument
    method initial_argument_value : ?default:string -> string -> string
    method initial_multiple_argument : string -> cgi_argument list
    method arguments : (string * cgi_argument) list
    method argument : string -> cgi_argument
    method argument_value : ?default:string -> string -> string
    method multiple_argument : string -> cgi_argument list
    method set_arguments : ?fin:bool -> cgi_argument list -> unit
    method update_argument : ?fin:bool -> cgi_argument -> unit
    method update_multiple_argument : ?fin:bool -> cgi_argument list -> unit
    method delete_argument : ?fin:bool -> string -> unit
    method url :
      ?protocol:Netcgi_env.protocol ->
      ?with_authority:other_url_spec ->
      ?with_script_name:other_url_spec ->
      ?with_path_info:other_url_spec ->
      ?with_query_string:query_string_spec ->
      unit -> string
    method output : Netchannels.trans_out_obj_channel
    method set_header :
      ?status:status ->
      ?content_type:string ->
      ?cache:cache_control ->
      ?filename:string ->
      ?language:string ->
      ?script_type:string ->
      ?style_type:string ->
      ?set_cookie:cgi_cookie list ->
      ?fields:(string * string list) list ->
      unit -> unit
    method set_redirection_header : string -> unit
    method finalize : unit -> unit
  end


  class to_compat_activation (cgi:Netcgi.cgi) =
  object
    val env = Netcgi_env.to_compat_environment cgi#environment
    val args = List.map to_compat_argument cgi#arguments
      (* FIXME: the curr_args should duplicate the body and header of
	 initial ones -- need to duplicate the files... :( *)
    val mutable curr_args =
      List.map to_compat_argument cgi#arguments
    val cgi = cgi

    method environment = env
    method request_method : request_method =
      match cgi#request_method with
      | `GET | `HEAD | `POST | `DELETE as m -> m
      | `PUT cgi -> `PUT(to_compat_argument cgi)

    method initial_arguments = List.map (fun a -> (a#name, a)) args
    method initial_argument name =
      List.find (fun a -> a#name = name) args
    method initial_argument_value ?default name =
      try (List.find (fun a -> a#name = name) args)#value
      with Not_found -> (match default with
			 | None -> raise Not_found
			 | Some d -> d)
    method initial_multiple_argument name =
      List.filter (fun a -> a#name = name) args


    method arguments = List.map (fun a -> (a#name, a)) curr_args
    method argument name =
      List.find (fun a -> a#name = name) curr_args
    method argument_value ?default name =
      try (List.find (fun a -> a#name = name) curr_args)#value
      with Not_found -> (match default with
			 | None -> raise Not_found
			 | Some d -> d)
    method multiple_argument name =
      List.filter (fun a -> a#name = name) curr_args

    method set_arguments ?(fin=true) new_args =
      if fin then
	List.iter (fun a -> if not(List.mem a new_args) then a#finalize())
	  curr_args;
      curr_args <- new_args

    method update_argument ?(fin=true) new_arg =
      let name = new_arg#name in
      let keep a =
	if fin && a#name = name && a <> new_arg then (a#finalize(); false)
	else a#name <> name in
      curr_args <- new_arg :: (List.filter keep curr_args)

    (* All arguments in [arglist] must have the same name. *)
    method update_multiple_argument ?(fin=true) arglist =
      match arglist with
      | [] -> ()
      | a0 :: tl ->
	  let name = a0#name in
	  if List.exists (fun a -> a#name <> name) tl then
	    invalid_arg "update_multiple_argument";
	  let keep a =
	    if fin && a#name = name && not(List.mem a arglist) then
	      (a#finalize(); false)
	    else a#name <> name in
	  curr_args <- arglist @ List.filter keep curr_args

    method delete_argument ?(fin=true) name =
      let keep a =
	if fin && a#name = name then (a#finalize(); false)
	else a#name <> name in
      curr_args <- List.filter keep curr_args


    method url ?protocol ?with_authority ?with_script_name ?with_path_info
      ?(with_query_string=(`None: query_string_spec)) () =
      cgi#url ?protocol ?with_authority ?with_script_name
	?with_path_info
	~with_query_string:(
	  match with_query_string with
	  | `Initial -> `Env
	  | `Current ->   `This(List.map of_compat_argument curr_args)
	  | `Args args -> `This(List.map of_compat_argument args)
	  | `None -> `None)
	()


    method output = cgi#out_channel

    method set_header ?status ?content_type ?cache ?filename
      ?language ?script_type ?style_type ?(set_cookie=[]) ?fields () =
      let now = Unix.time() in
      let make_cookie c =
	Netcgi.Cookie.make
	  ?domain:c.cookie_domain
	  ?max_age:(match c.cookie_expires with
		    | None -> None
		    | Some t -> Some(truncate(t -. now)))
	  ?path:c.cookie_path
	  ~secure:c.cookie_secure
	  c.cookie_name c.cookie_value in
      cgi#set_header ?status ?content_type
	~set_cookies:(List.map make_cookie set_cookie)
	?cache ?filename ?language ?script_type ?style_type ?fields ()

    method set_redirection_header loc =
      cgi#set_redirection_header loc


    method finalize () =
      List.iter (fun a -> a#finalize()) args;
      List.iter (fun a -> a#finalize()) curr_args;
      cgi#finalize()
  end


  let to_compat_activation = new to_compat_activation


  let of_compat_activation (cgi_act:cgi_activation) =
    let env = Netcgi_env.of_compat_environment(cgi_act#environment) in
  object(self)
    (* We have no idea of the output_type of [cgi_act] so we
       initialize with [`Direct] and override [out_channel]. *)
    inherit Netcgi_common.cgi env (`Direct "")
      (match cgi_act#request_method with
       | `GET | `HEAD | `POST | `DELETE as m -> m
       | `PUT a -> `PUT(of_compat_argument a))
      (List.map (fun (_, a) -> of_compat_argument a) cgi_act#initial_arguments)

    (* Override methods that depend on the unknown output_type *)

    method out_channel = cgi_act#output

    method set_header ?status ?content_type ?content_length
      ?(set_cookie=[]) ?(set_cookies=[])
      ?cache ?filename ?language ?script_type ?style_type ?(fields=[])
      () =
      let now = Unix.time() in
      let old_cookie c =
	{ cookie_name = Netcgi_common.Cookie.name c;
	  cookie_value = Netcgi_common.Cookie.value c;
	  cookie_expires = (match Netcgi_common.Cookie.max_age c with
			    | None -> None
			    | Some t -> Some(float t +. now));
	  cookie_domain = Netcgi_common.Cookie.domain c;
	  cookie_path = Netcgi_common.Cookie.path c;
	  cookie_secure = Netcgi_common.Cookie.secure c;
	} in
      let fields = 
	match content_length with
	  | None -> fields
	  | Some size -> 
	      ("content-length", [string_of_int size]) ::
		(List.filter
		   (fun (n,_) -> String.lowercase n <> "content-length")
		   fields) in
      (* The old [set_header] knows whether the output is
	 transactional or not. *)
      cgi_act#set_header ?status ?content_type ?cache ?filename
	?language ?script_type ?style_type ~fields
	~set_cookie:(set_cookie @ (List.map old_cookie set_cookies))
	()

    method set_redirection_header ?set_cookies ?(fields=[]) loc =
      (* There is no way of getting the old set_redirection_header to
	 accept to set other fields.  Thus, use [set_header].  *)
      self#set_header ?set_cookies
	~fields:(fields @ [("Location", [loc])] )
	()
  end
end


module Netcgi =
struct
  type argument_processing =
      [ `Memory
      | `File
      | `Automatic ]
	
  type operating_type =
      [ `Direct of string (* separator *)
      | `Transactional of
          (Netcgi_env.cgi_config -> Netchannels.out_obj_channel ->
             Netchannels.trans_out_obj_channel)
      ]

  let buffered_transactional_optype =
    `Transactional (fun config ch -> new Netchannels.buffered_trans_channel ch)


  let tempfile_transactional_optype =
    `Transactional
      (fun config ch ->
	 let tmp_directory = config.Netcgi_env.tmp_directory in
	 let tmp_prefix = config.Netcgi_env.tmp_prefix in
	 new Netchannels.tempfile_trans_channel ~tmp_directory ~tmp_prefix ch
      )

  class simple_argument ?(ro=true) name value =
  object
    val super = new Netcgi_common.simple_arg name value

    method name = super#name
    method value = super#value
    method open_value_rd = super#open_value_rd
    method ro = super#ro
    method store = super#store
    method content_type = fst(super#content_type())
    method content_type_params = snd(super#content_type())
    method charset = super#charset
    method filename = super#filename
    method representation = super#representation
    method finalize = super#finalize
    method set_value = super#set_value
    method open_value_wr = super#open_value_wr
  end

  class mime_argument ?work_around_backslash_bug name mime =
  object
    val super =
      new Netcgi_common.mime_arg ?work_around_backslash_bug ~name mime

    method name = super#name
    method value = super#value
    method open_value_rd = super#open_value_rd
    method ro = super#ro
    method store = super#store
    method content_type = fst(super#content_type())
    method content_type_params = snd(super#content_type())
    method charset = super#charset
    method filename = super#filename
    method representation = super#representation
    method finalize = super#finalize
    method set_value = super#set_value
    method open_value_wr = super#open_value_wr
  end

  let split_name_val s =  (* Same as in Netcgi_cgi *)
    try
      let i = String.index s '=' in
      (String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1))
    with Not_found ->
      (s, "")

  class std_activation 
           ?env ?(processing = 
	            fun _ _ -> `Automatic) 
           ?(operating_type = ( `Direct "" : operating_type ) ) () =
    let (new_env, in_obj) =
      match env with
	| None ->
	    if Netcgi_cgi.is_cgi() then
	      (* Following is stolen from Netcgi_cgi: *)
	      let (properties, input_header) =
		Array.fold_left
		  (fun l e -> 
		     Netcgi_common.update_props_inheader (split_name_val e) l)
		  ([], []) (Unix.environment()) in
	      let in_obj = new Netchannels.input_channel stdin in
	      let out_obj = new Netchannels.output_channel stdout in
	      let new_env = 
		new Netcgi_common.cgi_environment 
		      ~config:Netcgi.default_config
		      ~properties ~input_header out_obj in
	      (new_env, in_obj)
	    else
	      raise(Not_implemented "class std_activation: The environment is not CGI")
		(* [cgi] enters test mode in this case *)

	| Some e -> 
	    if e # input_state <> `Received_header then
	      failwith "Netcgi.std_activation: environment indicates the wrong input state";
	    if e # output_state <> `Start then
	      failwith "Netcgi.std_activation: environment indicates the wrong output state";
	    (Netcgi_env.of_compat_environment e,
	     e # input_ch) in

    let new_arg_store n_env arg_name n_arg_hdr = 
      let arg_hdr = new Netmime.basic_mime_header n_arg_hdr#fields in
      let p = processing arg_name arg_hdr in
      (p : argument_processing :> Netcgi_common.arg_store_type) in

    let new_output_type =
      match operating_type with
	| `Direct s -> `Direct s 
	| `Transactional f ->
	    `Transactional
	      (fun n_config out_obj ->
		 let o_config = Netcgi_env.to_compat_config n_config in
		 f o_config out_obj
	      ) in

    let new_cgi =
      Netcgi_common.cgi_with_args 
	(new Netcgi_common.cgi) 
	new_env new_output_type in_obj new_arg_store in

    let () =
      match env with
	| None -> ()
	| Some e ->
	    e # set_input_state `Received_body in

    Netcgi_types.to_compat_activation new_cgi

end
