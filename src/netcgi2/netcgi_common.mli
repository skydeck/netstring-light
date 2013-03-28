(* netcgi_common.mli

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

(** Functions to develop new connectors.
 *
 * The following module is provided as a set of helper functions to
 * define new connectors.  As a normal user of [Netcgi], {b you should
 * not use this module}.  *)

(** The goal of this module is to factor out common routines to easily
    set up new connectors.  Here is the normal flow of operations:

    - You start by reading the request environment properties as well
    as the input header.  Often both are undistinguished and provided
    through some sort of meta-variables.  The function
    {!Netcgi_common.update_props_inheader} helps you to sort them and
    to normalize the input header fields.  You also need to set up a
    {!Netchannels.out_obj_channel} to the output stream of your
    connector.  Then, {!Netcgi_common.cgi_environment} can create an
    environment object for you.  If [stderr] output is not appropriate
    (e.g. ot is not redirected to the server log), you need to
    override [#log_error].

    - From the environment object and arguments, {!Netcgi_common.cgi}
    creates a CGI object.  Often, arguments are read from the
    environment [#cgi_query_string] (in case of GET) or from an input
    channel (in case of POST).  {!Netcgi_common.cgi_with_args} handles
    this for you: it requires a {!Netchannels.in_obj_channel} from
    which the arguments are read (only used in the case of POST).

    - {!Netcgi_common.exn_handler_default} provides a default error
    page for uncaught exceptions.  It also allows the user to pass his
    own exception handler that has precedence on the default one.

    To see this schema in use, we recommend you have a look to the
    implementation of the CGI connector because it is very simple.
*)

(** {2 Arguments} *)

type representation = [ `Simple of Netmime.mime_body
                      | `MIME of Netmime.mime_message ]

type store = [`Memory | `File of string]

exception Oversized
  (** See {!Netcgi.Argument.Oversized}. *)

exception Too_many_arguments
  (** Hit the limit [max_arguments] *)

(** See {!Netcgi.cgi_argument}. *)
class type cgi_argument =
object
  method name : string
  method value : string
  method open_value_rd : unit -> Netchannels.in_obj_channel
  method store : store
  method content_type : unit -> string * (string * Mimestring.s_param) list
  method charset : string
  method filename : string option
  method representation : representation
  method finalize : unit -> unit
end

(** See {!Netcgi.rw_cgi_argument}.
    @deprecated Arguments are read-only. *)
class type rw_cgi_argument =
object
  inherit cgi_argument
  method ro : bool
  method set_value : string -> unit
  method open_value_wr : unit -> Netchannels.out_obj_channel
end

(** See {!Netcgi.Argument.simple}.  We reveal more of the object than
    {!Netcgi.Argument.simple} for the backward compatibility layer.
*)
class simple_arg : ?ro:bool -> string -> string -> rw_cgi_argument

(** See {!Netcgi.Argument.mime}.  We reveal more of the object than
    {!Netcgi.Argument.mime} for the backward compatibility layer. *)
class mime_arg : ?work_around_backslash_bug:bool -> ?name:string ->
  Netmime.mime_message -> rw_cgi_argument



(** {2 Cookies} *)

(** The cookie implementation has been moved to {!Nethttp.Cookie}.
    New code should directly call the functions defined there.
 *)

module Cookie :
sig
  type t = Nethttp.Cookie.t

  val make :
    ?max_age:int ->
    ?domain:string ->
    ?path:string ->
    ?secure:bool ->
    ?comment:string ->
    ?comment_url:string ->
    ?ports:int list ->
    string -> string -> t
  val name : t -> string
  val value : t -> string
  val max_age : t -> int option
    (** The expiration time of the cookie, in seconds.  [None] means
	that the cookie will be discarded when the browser exits.
	This information is not returned by the browser. *)
  val domain : t -> string option
  val path : t -> string option
  val secure : t -> bool
    (** Tells whether the cookie is secure.
	This information is not returned by the browser. *)
  val comment : t -> string
    (** Returns the comment associated to the cookie or [""] if it
	does not exists.  This information is not returned by the
	browser. *)
  val comment_url : t -> string
    (** Returns the comment URL associated to the cookie or [""] if it
	does not exists.  This information is not returned by the
	browser. *)
  val ports : t -> int list option

  val set_value : t -> string -> unit
  val set_max_age : t -> int option -> unit
  val set_domain : t -> string option -> unit
  val set_path : t -> string option -> unit
  val set_secure : t -> bool -> unit
  val set_comment : t -> string -> unit
  val set_comment_url : t -> string -> unit
  val set_ports : t -> int list option -> unit

  val set : #Netmime.mime_header -> t list -> unit
    (** [set http_header cookies] sets the [cookies] in [http_header]
	using version 0 or version 1 depending on whether version 1
	fields are used.  For better browser compatibility, if
	"Set-cookie2" (RFC 2965) is issued, then a "Set-cookie"
	precedes (declaring the same cookie with a limited number of
	options). 

        {b Deprecated name.} Use {!Nethttp.Header.set_set_cookie_ct}.
     *)
  val get : #Netmime.mime_header -> t list
    (** Decode the cookie header, may they be version 0 or 1.  

        {b Deprecated name.} Use {!Nethttp.Header.get_cookie_ct}.
     *)


  val of_record : Nethttp.cookie -> t
    (** Conversion from the deprecated style of cookie.

        {b Deprecated name.} Use {!Nethttp.Cookie.of_netscape_cookie}.
     *)
  val to_record : t -> Nethttp.cookie
    (** Conversion to the deprecated style of cookie (some parameters
        are dropped).

        {b Deprecated name.} Use {!Nethttp.Cookie.to_netscape_cookie}.
     *)
end


(************************************************************************)

(** {2 Environment} *)

(** See {!Netcgi.config}. *)
type config = {
  tmp_directory : string;
  tmp_prefix : string;
  permitted_http_methods : [`GET | `HEAD | `POST | `DELETE | `PUT] list;
  permitted_input_content_types : string list;
  input_content_length_limit : int;
  max_arguments : int;
  workarounds : [ `MSIE_Content_type_bug | `Backslash_bug
                | `Work_around_MSIE_Content_type_bug (* @deprecated *)
                | `Work_around_backslash_bug (* @deprecated *)
                ] list;
  default_exn_handler : bool;
}

(** See {!Netcgi.output_type}. *)
type output_type =
  [ `Direct of string
  | `Transactional of config ->
      Netchannels.out_obj_channel -> Netchannels.trans_out_obj_channel
  ]

val fix_MSIE_Content_type_bug : string -> string
  (** [fix_MSIE_Content_type_bug ct] transforms the content-type
      string [ct] to fix the MSIE Content-Type bug. *)

val is_MSIE : string -> bool
  (** [is_MSIE user_agent] tells whether the [user_agent] is Microsoft
      Internet Explorer.  Useful to know when to apply
      {!Netcgi_common.fix_MSIE_Content_type_bug}. *)

(* {b Notes about the header}

   The fact that the header is stored in variables of the environment
   and not send to the several advantages:

   - It is possible to change header fields at every moment before
   the commitment happens. For example, it is possible to set the
   content-length field which is normally only known just at the
   time of the commit operation.

   - The [environment] object can process the header; for example
   it can fix header fields.

   - It is simpler to connect to environments which transport the
   header in non-standard ways.  Example: Assume that the
   environment is the web server process (e.g. we are an Apache
   module).  Typically the header must be stored in different
   structures than the body of the message.  *)

(** [new cgi_environment ~config ~properties ~input_header out_obj]
    generates a {!Netcgi.cgi_environment} object, from the arguments.
    The creation of such an object {i does not} raise any exception.
    The method [#out_channel] of the created environment returns
    [out_obj].

    @param config give the configuration options.  Of particular
    interest here is [config.workarounds].  If
    [`MSIE_Content_type_bug] is present, a fix will be applied to
    [input_header].

    @param properties CGI-like properties as (name, value) pairs.
    Examples: [("REQUEST_METHOD", "POST")], [("SERVER_PROTOCOL",
    "HTTP/1.1")].  Note that "CONTENT_TYPE" and "CONTENT_LENGTH" are
    part of the input header.  It is highly recommended to use
    {!Netcgi_common.update_props_inheader} to build this list.

    @param input_header is a list of (field, value) pairs of the HTTP
    input request.  It is ASSUMED that field names in [input_header]
    are lowercase in order to apply a fix to the MSIE Content-Type
    bug.  Also remember that the separator is '-', not '_'.  Both
    requirements will be stafisfied if you use
    {!Netcgi_common.update_props_inheader} to build [input_header].


    {b Notes:} The header is kept into variables and
    [#send_output_header] sents it directly to [out_obj].  This has
    several advantages:

    - It is possible to change header fields at every moment before
    the commitment happens. For example, it is possible to set the
    content-length field which is normally only known just at the
    time of the commit operation.

    - The [environment] object can process the header; for example
    it can fix header fields.

    - It is simpler to connect to environments which transport the
    header in non-standard ways.  Example: Assume that the
    environment is the web server process (e.g. we are an Apache
    module).  Typically the header must be stored in different
    structures than the body of the message.
*)
class cgi_environment :
  config:config ->
  properties:(string * string) list ->
  input_header:(string * string) list ->
  Netchannels.out_obj_channel ->
object
  val mutable header_not_sent   : bool
    (** [true] iff the output headers have not been sent.
	[#send_output_header] must set it to false once it did its
	job. *)

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
  method protocol 		    : Nethttp.protocol
  method cgi_property : ?default:string -> string -> string
  method cgi_properties : (string * string) list
    (** Return the parameter [properties]. *)
  method cgi_https : bool
    (** @raise HTTP if the HTTPS property is not understood. *)

  method input_header : Netmime.mime_header
    (** For special header structures, just override this method. *)
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
    (** For special header structures, override this method and
	maybe [#send_output_header]. *)
  method output_header_field : ?default:string -> string -> string
  method multiple_output_header_field : string -> string list
  method output_header_fields : (string * string) list
  method set_output_header_field : string -> string -> unit
  method set_multiple_output_header_field : string -> string list -> unit
  method set_output_header_fields : (string * string) list -> unit
  method set_status : Nethttp.http_status -> unit
  method send_output_header : unit -> unit

  method output_ch : Netchannels.out_obj_channel
    (** @deprecated Use [#out_channel] instead. *)
  method out_channel : Netchannels.out_obj_channel

  method log_error : string -> unit
    (** You may want to override this with your custom logging method.
	By default, [#log_error msg] adds a timestamp to [msg] and
	sends th result to [stderr]. *)
  method config : config
end


(************************************************************************)

(** {2 CGI} *)

type other_url_spec = [ `Env | `This of string | `None ]
    (** See {!Netcgi.other_url_spec}. *)
type query_string_spec = [ `Env | `This of cgi_argument list | `None
			 | `Args of rw_cgi_argument list ]
    (** See {!Netcgi.query_string_spec}. *)
type cache_control = [ `No_cache | `Max_age of int | `Unspecified ]
    (** See {!Netcgi.cache_control}. *)

type request_method = [`GET | `HEAD | `POST | `DELETE | `PUT of cgi_argument]

val string_of_request_method : request_method -> string

type arg_store_type =
  [`Memory | `File | `Automatic | `Discard
  | `Memory_max of float | `File_max of float | `Automatic_max of float]

type arg_store = cgi_environment -> string -> Netmime.mime_header_ro ->
                   arg_store_type
  (** See {!Netcgi.arg_store}. *)


(** [cgi env op meth args] constructs {!Netcgi.cgi} objects.  The
    environment [#out_channel] is wrapped into a transactional channel
    or a discarding channel according to [op] ([`Direct] or
    [`Transactional]) and [request_method] ([`HEAD] requests must only
    return a header).  For standard cases, when POST and PUT arguments
    are available through a [Netchannels.in_obj_channel], we recommend
    you use {!Netcgi_common.cgi_with_args} that will parse the
    arguments for you and check preconditions.

    Remark: You may think that the argument [meth] is superfluous as
    it can be deduced from [env#cgi_request_method].  While it is true
    for [`DELETE], [`GET], [`HEAD] and [`POST], the [`PUT] takes a
    {!Netcgi_common.cgi_argument} parameter.  Setting correctly this
    parameter and decoding [env#cgi_request_method] is done for you by
    {!Netcgi_common.cgi_with_args}.  *)
class cgi : cgi_environment -> output_type -> request_method ->
  cgi_argument list ->
object
  method argument : string -> cgi_argument
  method argument_value : ?default:string -> string -> string
  method argument_exists : string -> bool
  method multiple_argument : string -> cgi_argument list
  method arguments : cgi_argument list

  method environment : cgi_environment
  method request_method : request_method
  method finalize : unit -> unit

  (** The following environment properties must be available for this
      method to work properly (please make sure your connector
      provides them; the CGI spec make them compulsory anyway):
      - [cgi_server_name]
      - [cgi_server_port]
      - [cgi_script_name]
      - [cgi_path_info] *)
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
    ?set_cookie:Nethttp.cookie list ->
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

  method output : Netchannels.trans_out_obj_channel
    (** @deprecated Use [#out_channel] instead. *)

  method out_channel : Netchannels.trans_out_obj_channel

  method at_exit : (unit -> unit) -> unit
end


val cgi_with_args :
  (cgi_environment -> output_type -> request_method -> cgi_argument list
     -> 'a) ->
  cgi_environment -> output_type -> Netchannels.in_obj_channel -> arg_store
  -> 'a
  (** [cgi_with_args (new cgi) env out op ?put_arg in_chan] constructs
      a {!Netcgi.cgi} object.  However, [new cgi] can be replaced by
      any function, so it is easy to use this to construct extensions
      of the [cgi] class (as needed by some connectors).  The
      arguments of the cgi object are taken from the environment [env]
      (for HEAD, GET, DELETE) or from the [in_chan] (for POST, PUT)
      and processed according to [arg_store].

      @raise HTTP if the data does not conform the standards or it not
      allowed. *)



(** {2 Exceptions} *)

exception HTTP of Nethttp.http_status * string
  (** Exception raised by various functions of this library to return
      to the user agent an appropriate error page with the specified
      http-status (this exception must be caught by the connector and
      a default answer sent).

      The string is a description of the cause of the error.

      This exception is for use by connectors only, users should deal
      with the exceptions in their code by generating a response with
      the usual [#set_header] and [#out_channel] of {!Netcgi.cgi}.  *)


val exn_handler_default : cgi_environment ->
  exn_handler:(cgi_environment -> (unit -> unit) -> unit) ->
  finally:(unit -> unit) ->
  (unit -> exn option) -> unit
  (** [exn_handler_default env ~exn_handler ~finally run_cgi] will
      basically execute [exn_handler env run_cgi].  Provided that the
      environment config [default_exn_handler] is set to [true] (the
      default), any exception [e] not caught by the user provided
      [exn_handler] (or that is raised by it) will be passed to the
      default handler of OCamlNet which will rollback the current
      output, produce a page describing the exception [e], and close
      the output channel of [env].  Note that the default handler
      treats [HTTP] exceptions specially -- for example, the response
      to [HTTP(`Method_not_allowed,...)]  includes an "Allow" header
      (as mandated by HTTP/1.1);...

      Note that, regardless of the value of [default_exn_handler], the
      [Exit] exception is always caught and treated like an
      acceptable early termination (thus produces no error page).

      Whether [run_cgi] terminates normally or by an exception,
      [finally()] is executed last.

      Sometimes, you want that some "special" exceptions (for example
      exceptions internal to the connector) CANNOT to be caught by
      [exn_handler].  In this case, [run_cgi()] catches the exception,
      say [e], and returns it as [Some e].  The exception [e] will "go
      through" [exn_handler_default], it will not even be passed to
      the default handler.  Therefore, you must take care that it is
      handled by the surrounding code or your connector may die
      without an error message.  Of course, [run_cgi] must return
      [None] if no "special" exception is raised.

      REMARK: Stricly speaking, [exn_handler env run_cgi] is obviously
      not possible because the return type of [run_cgi] is not [unit]
      but you can ignore that to understand what this function does.  *)


val error_page : cgi_environment -> Nethttp.http_status -> 
                 (string * string list) list ->
                 string -> string ->
                 unit
  (** [error_page env status fields msg detail]: Logs an error message and
      outputs an error page via [env].

      [status] is the status of the error page, e.g. [`Internal_server_error].
      [fields] are put into the response header of the error page.

      [msg] occurs in the log message and in the error page, and should
      be a concise description without linefeeds. [detail] is only
      printed to the error page, and may be longer than this, and may
      also include HTML markup.
   *)

(************************************************************************)

(** {2 Useful functions}

    The following functions are used in several connectors and are
    gouped here for convenience.
*)


val update_props_inheader : string * string ->
  ((string * string) list * (string * string) list as 'a) -> 'a
  (** [update_props_inheader (name, value) (props, inheader)] returns
      [(props, inheader)] to which the new parameter [name]-[value]
      has been added -- to [props] or [inheader], depending on [name].
      Unless you know what you are going, you {b must} use this
      function to classify parameters as it also performs some
      standardisation.  *)


val rm_htspace : (char -> bool) -> string -> int -> int -> string
  (** [rm_htspace is_space s low up] returns the substring [s.[low
      .. up - 1]] stripped of possible heading and trailing spaces
      identified by the function [is_space].

      @raise Invalid_argument if [low < 0] or [up > String.length s] *)

val rev_split : (char -> bool) -> string -> string list
  (** [split_rev is_delim s] split [s] at each character [is_delim]
      and returns the list of substrings in reverse order.  Several
      consecutive delimiters are treated as a single one.  The
      substrings do not share [s] memory. *)

val is_prefix : string -> string -> bool
  (** [is_prefix pre s] checks whether [pre] is a prefix of [s]. *)

type http_method =
    [`GET | `HEAD | `POST | `DELETE | `PUT]

val string_of_http_method : http_method -> string
  (** Returns the string value of the method *)

