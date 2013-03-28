(* netcgi_compat.mli

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

(** Compatibility module with the previous version of Netcgi.
 *
 * Opening this module allows a project using the old Netcgi_env and
 * Netcgi_types to compile with minor changes.  For documentation,
 * please see the original modules (in the netcgi1 directory).  If you are
 * a first time user or start a new project, {b do not use this
 * module}.
 *
 * Functions that are useful to migrate a project are emphasized with
 * "{b Portage:}".  Some comments accompany the functions that changed
 * to give some motivation for the new API.  (It is useful to refer to
 * the old documentation to understand everything.)
 *
 * @deprecated {i Use the new API instead.}
 *
 * @version $Id: netcgi_compat.mli,v 1.7 2005/11/04 00:53:05 chris_77 Exp $
 *)

exception Not_implemented of string
  (** Raised by some functions/exceptions when a feature of the old model
    * cannot be implemented in the new model or vice versa
   *)

module Netcgi_env :
sig
  type input_mode = [ `Standard (* | `Direct *) ]
      (** This is not used anywhere.  Moreover this is protocol
	  dependent. *)

  type input_state =
      [ `Start
      | `Receiving_header | `Received_header
      | `Receiving_body | `Received_body      ]
	(** This is not the business of the user.  Rather than to
	    document [#set_input_state] as such, we prefer not to
	    allow the user to have access to it at the first
	    place... *)

  type output_mode = [ `Standard (* | `Direct *) ]
      (** This is not used anywhere.  Moreover "non-parsed header" is
	  not supported by all connectors. *)

  type output_state =
      [ `Start
      | `Sending_header      | `Sent_header
      | `Sending_body        | `Sent_body
      | `Sending_part_header | `Sent_part_header
      | `Sending_part_body   | `Sent_part_body
      | `End
      ]
	(** This is not the business of the user.  In the new API, it
	    is handled transparently. *)

  type protocol_version = Nethttp.protocol_version
  type protocol_attribute = Nethttp.protocol_attribute
  type protocol = Nethttp.protocol
  type workaround =
      [ `Work_around_MSIE_Content_type_bug | `Work_around_backslash_bug  ]
	(** The [Work_around_] part has been dropped as it is clear at
	    the spot of use. *)

  (** Now simply called {!Netcgi.config}. *)
  type cgi_config = {
    tmp_directory : string;
    tmp_prefix : string;
    permitted_http_methods : string list;
    (** Now has type [[`GET | `HEAD | `POST | `DELETE | `PUT] list]
	for coherence with {!Netcgi.cgi}[#request_method]. *)
    permitted_input_content_types : string list;
    input_content_length_limit : int;
    workarounds : workaround list;
  }

  val default_config : cgi_config

  val of_compat_config : cgi_config -> Netcgi.config
    (** {b Portage:} [of_compat_config c] transform the old configuration [c]
	into one suitable for the new interface. *)

  val to_compat_config : Netcgi.config -> cgi_config
    (** {b Portage:} [to_compat_config c] transform the new configuration [c]
	into one suitable for the old interface. *)


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
      (** I rest to be convinced we need this.  Is it provided by web
	  servers different from Apache?  Why is the [#url] method not
	  enough? *)
    method protocol : protocol

    method input_header : Netmime.mime_header

    method input_header_field : ?default:string -> string -> string
    method multiple_input_header_field : string -> string list
    method input_header_fields : (string * string) list
    method user_agent : string
    method cookies : (string * string) list
      (** Cookies are returned in decoded form.  An additional
	  [#cookie] method has been added for convenience and
	  coherence with e.g. {!Netcgi.cgi.arguments}
	  v.s. {!Netcgi.cgi.argument}. *)
    method input_content_length : int
    method input_content_type_string : string
    method input_content_type : (string * (string * Mimestring.s_param) list)
      (** Type now is [unit -> string * (string * Mimestring.s_param)
	  list] to be coherent with {!Netmime.mime_header_ro}. *)

    method input_ch : Netchannels.in_obj_channel
      (** Not the user business. *)
    method input_state : input_state
      (** Not the user business. *)
    method set_input_state : input_state -> unit
      (** Not the user business. *)

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
      (** @deprecated in favor of [out_channel] by analogy with the
	  standard library -- even though it is a "channel object".  *)
    method output_state : output_state
      (** Not the user business. *)
    method set_output_state : output_state -> unit
      (** Not the user business. *)
    method log_error : string -> unit
  end

  val to_compat_environment : Netcgi.cgi_environment -> cgi_environment
    (** {b Portage:} [to_compat_environment e] converts the new environment
	[e] to the old interface. *)

  val of_compat_environment : cgi_environment -> Netcgi.cgi_environment
    (** {b Portage:} [of_compat_environment e] converts the old environment
	[e] to the new interface. *)
end


module Netcgi_types :
sig
  class type simple_message = Netmime.mime_body

  type store = [ `Memory | `File of string ]
      (** Embedded in the single place of use. *)
  type representation =
      [ `Simple of simple_message | `MIME of Netmime.mime_message ]
	(** Embedded in the single place of use. *)

  (** {b Portage:} In addition to defining a type, the following
      [cgi_argument] also defines a conversion function that allows to
      connect old scripts to the new infrastructure.  Finalizing
      [cgi_argument a] will also finalize [a].  *)
  class type cgi_argument =
  object
    method name : string
    method value : string
    method open_value_rd : unit -> Netchannels.in_obj_channel
    method ro : bool
      (** Irrelevant: there are no methods to mutate an argument. *)
    method store : store
    method content_type : string
    method content_type_params : (string * Mimestring.s_param) list
      (** Method [content_type : unit -> string * (string *
	  Mimestring.s_param) list] defined instead of [content_type]
	  and [content_type_params] to be coherent with
	  {!Netmime.mime_header_ro} -- yet as easy to use. *)
    method charset : string
    method filename : string option
    method representation : representation
    method finalize : unit -> unit
    method set_value : string -> unit
      (** Dealing with the arguments is the task of an higher order
	  library.  In fact, it is generally misleading to think
	  arguments as mutable because one must accomodate the back
	  button -- so cache, history,... will use the "initial args".
	  Moreover, the only place where mutability is interesting is
	  the [#url] method which -- as already remarked -- can be
	  dealt with in a purely functional way.

	  The new interface contains commodity functions to update
	  lists of arguments (see {!Netcgi.Argument}).  *)
    method open_value_wr : unit -> Netchannels.out_obj_channel
      (** See [#set_value]. *)
  end

  val to_compat_argument : Netcgi.cgi_argument ->  cgi_argument
    (** {b Portage:} [to_compat_argument a] converts a new style argument
	[a] to an old style one.  Finalizing [to_compat_argument a] will also
	finalize [a].  *)

  val of_compat_argument : cgi_argument -> Netcgi.cgi_argument
    (** {b Portage:} [of_compat_argument a] converts an old style argument
	[a] to a new style one.  Finalizing [of_compat_argument a] will also
	finalize [a].  *)


  type cgi_cookie = Nethttp.netscape_cookie = {
    cookie_name : string;
    cookie_value : string;
    cookie_expires : float option;
    cookie_domain : string option;
    cookie_path : string option;
    cookie_secure : bool;
  }

  type status = Nethttp.http_status

  type request_method =
      [ `GET | `HEAD | `POST | `DELETE | `PUT of cgi_argument ]

  type cache_control = [ `No_cache | `Max_age of int | `Unspecified ]

  type query_string_spec =
      [ `Initial | `Current | `Args of cgi_argument list | `None ]

  type other_url_spec = [ `Env | `This of string | `None ]

  (** {b Portage:} In addition to defining a type, the following
      [cgi_activation] also defines a conversion function that allows
      to connect old scripts to the new infrastructure.  Renamed
      simply [cgi] as this is the main cgi-like abstraction.  *)
  class type cgi_activation =
  object
    method environment : Netcgi_env.cgi_environment
    method request_method : request_method

    method initial_arguments : (string * cgi_argument) list
    method initial_argument : string -> cgi_argument
    method initial_argument_value : ?default:string -> string -> string
    method initial_multiple_argument : string -> cgi_argument list

    (** Mutability of arguments has been dropped.  We indeed believe
	that how to deal with arguments is the role of a higher level
	library, so it is unconvenient that they are mutable (force
	copying even if unecessary).  {!Netcgi.Argument} has a few
	helper functions to help manipulating lists of immutable
	arguments.  Moreover, mutable arguments send the wrong message
	to the user (imagine one wants to keep a cache of args ->
	response) and unnecessarily complicate the interface. *)
    method arguments : (string * cgi_argument) list
      (** It has type [argument list] as the name is redundant and
	  this is better suited for the {!Netcgi.query_string_spec}
	  optional arguments. *)
    method argument : string -> cgi_argument
    method argument_value : ?default:string -> string -> string
    method multiple_argument : string -> cgi_argument list

    method set_arguments : ?fin:bool -> cgi_argument list -> unit
    method update_argument : ?fin:bool -> cgi_argument -> unit
    method update_multiple_argument : ?fin:bool -> cgi_argument list -> unit
    method delete_argument : ?fin:bool -> string -> unit

    method url :
      ?protocol:Netcgi_env.protocol ->
      ?with_authority:other_url_spec ->        (* default: `Env *)
      ?with_script_name:other_url_spec ->      (* default: `Env *)
      ?with_path_info:other_url_spec ->        (* default: `Env *)
      ?with_query_string:query_string_spec ->  (* default: `None *)
      unit -> string

    method output : Netchannels.trans_out_obj_channel
      (** @deprecated in favor of [out_channel] for coherence with the
	  standard library --- this is also to avoid
	  [cgi#output#output ...]  which looks odd. *)

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
      (** Added [?content_length] for user convenience and deprecated
	  [?set_cookie] in favor of [?set_cookies] for coherence
	  e.g. with [?fields]. *)

    method set_redirection_header : string -> unit

    method finalize : unit -> unit
  end

  val to_compat_activation : Netcgi.cgi -> cgi_activation
    (** {b Portage:} [to_compat_activation] converts a new style 
        cgi object to an old cgi_activation object. *)

  val of_compat_activation : cgi_activation -> Netcgi.cgi
    (** {b Portage:} [of_compat_activation] converts an old style 
        cgi_activation to a new CGI-like object. *)
end

module Netcgi :
sig
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

  class simple_argument :
    ?ro:bool -> string -> string -> Netcgi_types.cgi_argument

  class mime_argument :
    ?work_around_backslash_bug:bool ->
  string -> Netmime.mime_message -> Netcgi_types.cgi_argument

  class std_activation :
    ?env:Netcgi_env.cgi_environment ->
    ?processing:(string -> Netmime.mime_header -> argument_processing) ->
    ?operating_type:operating_type ->
    unit ->
      Netcgi_types.cgi_activation

  val buffered_transactional_optype : operating_type

  val tempfile_transactional_optype : operating_type

end



(*
  Local Variables:
  mode: outline-minor
  outline-regexp: " *\\(val\\|module\\|method\\) "
  End:
*)
