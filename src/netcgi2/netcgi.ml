(* netcgi.ml

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



(* Argument
 ***********************************************************************)

(* This is a simplified mix of Netmime.mime_header_ro and
   Netmime.mime_body_ro *)
class type cgi_argument = Netcgi_common.cgi_argument

(* @deprecated *)
class type rw_cgi_argument = Netcgi_common.rw_cgi_argument
class simple_argument = Netcgi_common.simple_arg
class mime_argument ?work_around_backslash_bug name value
  = Netcgi_common.mime_arg ?work_around_backslash_bug ~name value


module Argument =
struct
  type t = cgi_argument

  exception Oversized = Netcgi_common.Oversized

  let simple (name:string) value =
    (new Netcgi_common.simple_arg name value :> cgi_argument)

  let mime ?work_around_backslash_bug ?name (m: Netmime.mime_message) =
    (new Netcgi_common.mime_arg ?work_around_backslash_bug ?name m
     :> cgi_argument)

  (* Manipulation of arguments *)

  let clone ?name ?value ?msg (arg:cgi_argument) =
    let name = match name with None -> arg#name | Some n -> n in
    match arg#representation with
    | `Simple _ ->
	let value = match value with None -> arg#value | Some v -> v in
	simple name value
    | `MIME message ->
	let m = match msg with
	  | Some m -> m
	  | None -> (match value with
		     | None -> message
		     | Some v -> (new Netmime.basic_mime_header [],
				  `Body(new Netmime.memory_mime_body v))) in
	mime ~name m


  let set new_args (args: cgi_argument list) =
    let names = List.map (fun a -> a#name) new_args in
    let rec filter args' = function
      | [] -> args'
      | a :: tl ->
	  if List.mem a#name names then filter args' tl
	  else filter (a :: args') tl  in
    filter new_args args
end


(* Cookies
 ***********************************************************************)

module Cookie = Nethttp.Cookie


(* Config
 ***********************************************************************)

type http_method =
    [`GET | `HEAD | `POST | `DELETE | `PUT]

type config = Netcgi_common.config = {
  tmp_directory : string;
  tmp_prefix : string;
  permitted_http_methods : http_method list;
  permitted_input_content_types : string list;
  input_content_length_limit : int;
  max_arguments : int;
  workarounds :
    [ `MSIE_Content_type_bug | `Backslash_bug
    | `Work_around_MSIE_Content_type_bug
    | `Work_around_backslash_bug ] list;
  default_exn_handler : bool;
}

let default_config =
  {
    tmp_directory = Netsys_tmp.tmp_directory();
    tmp_prefix = "netcgi";
    permitted_http_methods = [`GET; `HEAD; `POST];
    permitted_input_content_types = [ "multipart/form-data";
				      "application/x-www-form-urlencoded" ];
    input_content_length_limit = max_int;
    max_arguments = 10000;
    workarounds = [ `MSIE_Content_type_bug; `Backslash_bug ];
    default_exn_handler = true;
  }


(* Environment
 ***********************************************************************)

class type cgi_environment =
object
  method cgi_gateway_interface      : string
  method cgi_server_name            : string
  method cgi_server_port            : int option
  method cgi_server_protocol        : string
  method cgi_server_software        : string
  method cgi_request_method         : string
  method cgi_script_name            : string
  method cgi_path_info              : string
  method cgi_path_translated        : string
  method cgi_auth_type              : string
  method cgi_remote_addr            : string
  method cgi_remote_host            : string
  method cgi_remote_user            : string
  method cgi_remote_ident           : string
  method cgi_query_string           : string
  method protocol : Nethttp.protocol
  method cgi_property : ?default:string -> string -> string
  method cgi_properties : (string * string) list
  method cgi_https : bool

  method input_header : Netmime.mime_header
  method input_header_field : ?default:string -> string -> string
  method multiple_input_header_field : string -> string list
  method input_header_fields : (string * string) list
  method cookie : string -> Cookie.t
  method cookies : Cookie.t list
  method user_agent : string
  method input_content_length : int
  method input_content_type_string : string
  method input_content_type :
    unit -> string * (string * Mimestring.s_param) list

  method output_header : Netmime.mime_header
  method output_header_field : ?default:string -> string -> string
  method multiple_output_header_field : string -> string list
  method output_header_fields : (string * string) list
  method set_output_header_field : string -> string -> unit
  method set_multiple_output_header_field : string -> string list -> unit
  method set_output_header_fields : (string * string) list -> unit
  method set_status : Nethttp.http_status -> unit
  method send_output_header : unit -> unit

  method output_ch : Netchannels.out_obj_channel (* deprecated *)
  method out_channel : Netchannels.out_obj_channel

  method log_error : string -> unit
  method config : config
end



(* CGI object
 ***********************************************************************)

type other_url_spec = Netcgi_common.other_url_spec
type query_string_spec = Netcgi_common.query_string_spec
type cache_control = Netcgi_common.cache_control


class type cgi =
object
  method argument : string -> cgi_argument
  method argument_value : ?default:string -> string -> string
  method argument_exists : string -> bool
  method multiple_argument : string -> cgi_argument list
  method arguments : cgi_argument list

  method environment : cgi_environment
  method request_method :
    [`GET | `HEAD | `POST | `DELETE | `PUT of cgi_argument]
  method finalize  : unit -> unit

  method url : ?protocol:Nethttp.protocol ->
               ?with_authority:other_url_spec ->        (* default: `Env *)
               ?with_script_name:other_url_spec ->      (* default: `Env *)
               ?with_path_info:other_url_spec ->        (* default: `Env *)
               ?with_query_string:query_string_spec ->  (* default: `None *)
               unit -> string

  method set_header :
    ?status:Nethttp.http_status ->
    ?content_type:string ->
    ?content_length:int ->
    ?set_cookie:Nethttp.cookie list -> (* deprecated *)
    ?set_cookies:Cookie.t list ->
    ?cache:cache_control ->
    ?filename:string ->
    ?language:string ->
    ?script_type:string ->
    ?style_type:string ->
    ?fields:(string * string list) list ->
    unit -> unit

  method set_redirection_header :
    ?set_cookies:Cookie.t list ->
    ?fields:(string * string list) list ->
    string -> unit

  method output : Netchannels.trans_out_obj_channel (* deprecated *)
  method out_channel : Netchannels.trans_out_obj_channel

  method at_exit : (unit -> unit) -> unit
end


class type cgi_activation = cgi


(* Connectors *)

type output_type = Netcgi_common.output_type
type arg_store = Netcgi_common.arg_store
type exn_handler = cgi_environment -> (unit -> unit) -> unit

type connection_directive =
    [ `Conn_close | `Conn_close_linger | `Conn_keep_alive
    | `Conn_error of exn
    ]

let buffered_transactional_outtype =
  `Transactional (fun config ch -> new Netchannels.buffered_trans_channel ch)

let buffered_transactional_optype =
  buffered_transactional_outtype

let tempfile_transactional_outtype =
  `Transactional
    (fun config ch ->
       let tmp_directory = config.tmp_directory in
       let tmp_prefix = config.tmp_prefix in
       new Netchannels.tempfile_trans_channel ~tmp_directory ~tmp_prefix ch
    )

let tempfile_transactional_optype =
  tempfile_transactional_outtype
