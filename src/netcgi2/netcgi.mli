(* netcgi.mli

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

(** Common data-structures for CGI-like connectors.
 *
 * This library tries to minimize the use of unsafe practices.  It
 * cannot be bullet proof however and you should read about
 * {{:http://www.w3.org/Security/Faq/wwwsf4.html}security}.
 *
 * REMARK: It happens frequently that hard to predict random numbers
 * are needed in Web applications.  The previous version of this
 * library used to include some facilities for that (in the
 * [Netcgi_jserv] module).  They have been dropped in favor of
 * {{:http://pauillac.inria.fr/~xleroy/software.html#cryptokit}Cryptokit}.
 *)


(** {2 Arguments} *)

(** Represent a key-value pair of data passed to the script (including
    file uploads).  *)
class type cgi_argument =
object
  method name : string
    (** The name of the argument. *)
  method value : string
    (** The value of the argument, after all transfer encodings have
        been removed.  If the value is stored in a file, the file will
        be loaded.

        @raise Argument.Oversized if the argument was discarded.
	@raise Failure if the object has been finalized.  *)

  method open_value_rd : unit -> Netchannels.in_obj_channel
    (** Open the contents of the value as an input channel.  This
        works for all kinds of arguments, regardless of their
        [#storage] and [#representation].

        @raise Argument.Oversized if the argument was discarded.
	@raise Failure if the object has been finalized.  *)

  method store : [`Memory | `File of string]
    (** Tells whether the argument is stored in memory (as a string)
        or as a file (the argument of [`File] being the filename). *)
  method content_type : unit -> string * (string * Mimestring.s_param) list
    (** Returns the content type of the header and its parameters as a
	couple [(hdr, params)].  When the header is missing, the
	result is [("text/plain", [])].  Below you will find access
	method for frequently used parameters.  *)
  method charset : string
    (** The [charset] parameter of the content type of the header, or
        [""] when there is no such parameter, or no header.  *)
  method filename : string option
    (** The [filename] parameter found in the header of file uploads.
        When present, [Some name] is returned, and [None] otherwise.
        (This is not to be confused with the possible local file
        holding the data.)  *)

  method representation : [ `Simple of Netmime.mime_body
                          | `MIME of Netmime.mime_message ]
    (** The representation of the argument.

        - [`Simple] the value of the CGI argument is an unstructured
          string value.
        - [`MIME] The argument has a MIME header in addition to the
          value.  The MIME message is read-only.  *)

  method finalize : unit -> unit
    (** Arguments stored in temp files must be deleted when the
        argument is no longer used. You can call [finalize] to delete
        such files.  The method does not have any effect when [store =
        `Memory].  The method never raises any exceptions.  If the
        file no longer exists (e.g. because it was moved away) or if
        there are any problems deleting the file, the error will be
        ignored.

        The [finalize] method is not registered in the garbage
        collector.  You can do that, but it is usually better to call
        this method manually.  *)
    (* FIXME: Should we do that at the end of the request anyway?
       Since the request handler is a callback, this is possible. *)
    (* FIXME: Reference counting? *)
end


(** Operations on arguments and lists of thereof. *)
module Argument :
sig
  exception Oversized

  val simple : string -> string -> cgi_argument
    (** [simple_arg name value] creates an unstructured CGI argument
	called [name] with contents [value].  *)

  val mime : ?work_around_backslash_bug:bool ->
    ?name:string -> Netmime.mime_message -> cgi_argument
    (** [mime_arg ?name msg] creates a MIME-structured CGI argument
	called [name] with contents [msg].  You can create [msg] by
	either {!Netmime.memory_mime_message} or
	{!Netmime.file_mime_message}.

	@param name set the name of the argument.  If it is not given,
	the name is extracted from the "Content-Disposition" field of
	the [msg] header or it [""] if this field is not found.

	@param work_around_backslash_bug Whether to work around a bug
	found in almost all browsers regarding the treatment of
	backslashes.  The browsers do not quote backslashes in file
	names.  This breaks RFC standards, however.  This argument is
	[true] by default.  *)


  (** It is easy to manipulate lists of arguments with the [List]
      module.  For example, [List.filter (fun a -> a#name <> n) args]
      will remove from [args] all occurrences of the argument with
      name [n].  The following functions are helpers for operations
      specific to arguments.  *)

  val clone : ?name:string -> ?value:string -> ?msg:Netmime.mime_message ->
    cgi_argument -> cgi_argument

  val set : cgi_argument list -> cgi_argument list -> cgi_argument list
    (** [set new_args args] creates a list of argument from [args]
	deleting the arguments whose name appears in [new_args] and
	adding the [new_args] arguments. *)
end


(** Old deprecated writable argument type.
    @deprecated Arguments are read-only. *)
class type rw_cgi_argument =
object
  inherit cgi_argument
  method ro : bool
  method set_value : string -> unit
  method open_value_wr : unit -> Netchannels.out_obj_channel
end

class simple_argument : ?ro:bool -> string -> string -> rw_cgi_argument
  (** Old deprecated simple argument class.
      @deprecated Use {!Netcgi.Argument.simple} instead. *)

class mime_argument : ?work_around_backslash_bug:bool ->
  string -> Netmime.mime_message -> rw_cgi_argument
  (**  Old deprecated MIME argument class.
       @deprecated Use {!Netcgi.Argument.mime} instead. *)




(** {2 Cookies} *)

(** Functions to manipulate cookies.

    You should know that besides the [name] and [value] attribute,
    user agents will send at most the [path], [domain] and [port] and
    usually will not send them at all.

    For interoperability, cookies are set using version 0 (by
    Netscape) unless version 1 (RFC 2965 and the older RFC 2109)
    fields are set.  While version 0 is well supported by browsers,
    RFC 2109 requires a recent browser and RFC 2965 is usually not
    supported.  You do not have to worry however, cookies are always
    sent in such a way older browsers understand them -- albeit not
    all attributes of course -- so your application can be ready for
    the time RFC 2965 will be the norm.

    N.B. This module appears also as {!Nethttp.Cookie}.
 *)
module Cookie :
sig
  type t = Nethttp.Cookie.t
    (** Mutable cookie type. *)

  val make :
    ?max_age:int ->
    ?domain:string ->
    ?path:string ->
    ?secure:bool ->
    ?comment:string ->
    ?comment_url:string ->
    ?ports:int list ->
    string -> string -> t
    (** [make ?expires ?domain ?path ?secure name value] creates a new
        cookie with name [name] holding [value].

        @param max_age see {!Netcgi.Cookie.set_max_age}.
                       Default: when user agent exits.
        @param domain see {!Netcgi.Cookie.set_domain}.
	              Default: hostname of the server.
        @param path see {!Netcgi.Cookie.set_path}.
                    Default: script name + path_info.
        @param secure see {!Netcgi.Cookie.set_secure}.  Default: [false].
	@param comment see {!Netcgi.Cookie.set_comment}.  Default: [""].
	@param comment_url see {!Netcgi.Cookie.set_comment_url}.
	                   Default: [""].
	@param ports see {!Netcgi.Cookie.set_ports}.
	             Default: same port the cookie was sent.
    *)

  val name : t -> string
    (** The name of the cookie. *)
  val value : t -> string
    (** The value of the cookie. *)
  val domain : t -> string option
    (** The domain of the cookie, if set. *)
  val path : t -> string option
    (** The path of the cookie, if set. *)
  val ports : t -> int list option
    (** [port c] the ports to which the cookie may be returned or [[]] if
	not set. *)
  val max_age : t -> int option
    (** The expiration time of the cookie, in seconds.  [None] means
        that the cookie will be discarded when the browser exits.
        This information is not returned by the browser. *)
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

  val set_value : t -> string -> unit
    (** [set_value c v] sets the value of the cookie [c] to [v]. *)
  val set_max_age : t -> int option -> unit
    (** [set_max_age c (Some t)] sets the lifetime of the cookie [c]
        to [t] seconds.  If [t <= 0], it means that the cookie should
        be discarded immediately.  [set_expires c None] tells the
        cookie to be discarded when the user agent exits.  (Despite
        the fact that the name is borrowed from the version 1 of the
        specification, it works transparently with version 0.) *)
  val set_domain : t -> string option -> unit
    (** Cookies are bound to a certain domain, i.e. the browser sends
	them only when web pages of the domain are requested:
	- [None]: the domain is the hostname of the server.
	- [Some domain]: the domain is [domain].  *)
  val set_path : t -> string option -> unit
    (** Cookies are also bound to certain path prefixes, i.e. the
        browser sends them only when web pages at the path or below are
        requested.
        - [None]: the path is script name + path_info
        - [Some p]: the path is [p]. With [Some "/"] you can disable the
          path restriction completely.  *)
  val set_secure : t -> bool -> unit
    (** Cookies are also bound to the type of the web server:
        [set_secure false] means servers without SSL, [set_secure
        true] means servers with activated SSL ("https").  *)
  val set_comment : t -> string -> unit
    (** [set_comment c s] sets the comment of the cookie [c] to [s]
	which must be UTF-8 encoded (RFC 2279).  Because cookies can
	store personal information, the comment should describe how
	the cookie will be used so the client can decide whether to
	allow the cookie or not.  To cancel a comment, set it to [""].

	Cookie version 1 (RFC 2109).  *)
  val set_comment_url : t -> string -> unit
    (** [set_comment_url c url] same as {!Netcgi.Cookie.set_comment}
	except that the cookie comment is available on the page
	pointed by [url].  To cancel, set it to [""].

	Cookie version 1 (RFC 2965).  *)
  val set_ports : t -> int list option -> unit
    (** [set ports c (Some p)] says that the cookie [c] must only be
	returned if the server request comes from one of the listed
	ports.  If [p = []], the cookie will only be sent to the
	request-port it was received from.  [set_ports c None] says
	that the cookie may be sent to any port.

	Cookie version 1 (RFC 2965).  *)

  val of_netscape_cookie : Nethttp.netscape_cookie -> t
    (** Convert a Netscape cookie to the new representation *)

  val to_netscape_cookie : t -> Nethttp.netscape_cookie
    (** Convert to Netscape cookie (with information loss) *)
end





(** {2 The environment of a request} *)

type http_method =
    [`GET | `HEAD | `POST | `DELETE | `PUT]

type config = Netcgi_common.config = {
  tmp_directory : string;
  (** The directory where to create temporary files.  This should be
      an absolute path name. *)
  tmp_prefix : string;
  (** The name prefix for temporary files. This must be a non-empty
      string. It must not contain '/'.  *)

  permitted_http_methods : http_method list;
  (** The list of accepted HTTP methods *)
  permitted_input_content_types : string list;
  (** The list of accepted content types in requests.
      Content type parameters (like "charset") are ignored.
      If the list is empty, all content types are allowed. *)
  input_content_length_limit : int;
  (** The maximum size of the request, in bytes. *)
  max_arguments : int;
  (** The maximum number of CGI arguments *)
  workarounds : [ `MSIE_Content_type_bug | `Backslash_bug
                | `Work_around_MSIE_Content_type_bug
                | `Work_around_backslash_bug ] list;
  (** The list of enabled workarounds.

      - [`MSIE_Content_type_bug]

      - [`Backslash_bug] A common error in MIME implementations is not
      to escape backslashes in quoted string and comments.  This
      workaround mandates that backslashes are handled as normal
      characters.  This is important for e.g. DOS filenames because,
      in the absence of this, the wrongly encoded "C:\dir\file" will
      be decoded as "C:dirfile".

      Remark: [`Work_around_MSIE_Content_type_bug] and
      [`Work_around_backslash_bug] are deprecated versions of,
      respectively, [`MSIE_Content_type_bug] and [`Backslash_bug]. *)
  default_exn_handler : bool;
  (** Whether to catch exceptions raised by the script and display an
      error page.  This will keep the connector running even if your
      program has bugs in some of its components.  This will however
      also prevent a stack trace to be printed; if you want this turn
      this option off. *)
}

val default_config : config
  (** The default configuration is:
      - [tmp_directory]: {!Netsys_tmp.tmp_directory()}
      - [tmp_prefix]: "netcgi"
      - [permitted_http_methods]: [`GET], [`HEAD], [`POST].
      - [permitted_input_content_types]: ["multipart/form-data"],
      ["application/x-www-form-urlencoded"].
      - [input_content_length_limit]: [maxint] (i.e., no limit).
      - [max_arguments = 10000] (for security reasons)
      - [workarounds]: all of them.
      - [default_exn_handler]: set to [true].

      To create a custom configuration, it is recommended to use this syntax:
      {[
      let custom_config = { default_config with tmp_prefix = "my_prefix" }
      ]}
      (This syntax is also robust w.r.t. the possible addition of new
      config flields.) *)

(** The environment of a request consists of the information available
    besides the data sent by the user (as key-value pairs).  *)
class type cgi_environment =
object
  (** {3 CGI properties}

      The following properties are standardised by CGI.  The methods
      return [""] (or [None] in the case of the port number) when the
      property is not available.  *)

  method cgi_gateway_interface : string
  method cgi_server_name       : string
  method cgi_server_port       : int option
  method cgi_server_protocol   : string
  method cgi_server_software   : string
  method cgi_request_method    : string
    (** We recommend you to use the method {!Netcgi.cgi.request_method}
	which is more type-safe and informative. *)
  method cgi_script_name       : string
  method cgi_path_info         : string
  method cgi_path_translated   : string
  method cgi_auth_type         : string
  method cgi_remote_addr       : string
  method cgi_remote_host       : string
  method cgi_remote_user       : string
  method cgi_remote_ident      : string
  method cgi_query_string      : string
    (** This is the row query string.  The {!Netcgi.cgi} class gives
	you an easy access to the arguments through the [#arg...]
	methods.  *)

  method protocol : Nethttp.protocol
    (** The server protocol in a decoded form.  It can be either
        - [`Http((major,minor),attributes)] or
        - [`Other]. *)

  method cgi_property : ?default:string -> string -> string
    (** Returns a (possibly non-standard) CGI environment property.
        If the property is not set, [Not_found] is be raised unless
        the [default] argument is passed.  The [default] argument
        determines the result of the function in this case.

        The method takes the case-sensitive name and returns the value
        of the property. Usually, these properties have uppercase
        names.

        For example, [cgi_gateway_interface] returns the same as {[
        cgi_property ~default:"" "GATEWAY_INTERFACE" ]}

        You cannot access the fields coming from the HTTP header.  Use
        the method [input_header_field] instead.  *)

  method cgi_properties : (string * string) list
    (** Return all properties as an associative list. *)

  method cgi_https : bool
    (** A well-known extension is the HTTPS property.  It indicates
        whether a secure connection is used (SSL/TLS).  This method
        interprets this property and returns true if the connection is
        secure.  This method fails if there is a HTTPS property with
        an unknown value.  *)

  (** {3 Header of the incoming HTTP request.}  *)

  method input_header : Netmime.mime_header
    (** The whole HTTP header. *)
  method input_header_field : ?default:string -> string -> string
    (** [#input_header_field ?default f] returns the value of a field
        [f] of the HTTP request header.  The field name [f] is
        case-insensitive; if the name is a compound name, the parts
        are separated by "-", e.g. ["content-length"].  If there are
        several fields with the same name only the first field will be
        returned.

	@raise Not_found if the field does not exist, unless the
        [default] argument is passed.  The [default] argument is the
        result of the function in this case.  *)
  method multiple_input_header_field : string -> string list
    (** Returns the values of all fields with the passed name of the
        request header.  *)
  method input_header_fields : (string * string) list
    (** Returns the input header as (name,value) pairs.  The names may
        consist of lowercase or uppercase letters.  *)

  method cookie : string -> Cookie.t
    (** [#cookie cn] returns the cookie with name [cn].
        @raise Not_found if such a cookie does not exists. *)
  method cookies : Cookie.t list
    (** Returns the list of valid cookies found in the request header.
        Here "valid" means that the decode function does not raise an
        exception. *)

  method user_agent : string
    (** This is a convenience method that returns the ["User-agent"]
        field of the HTTP request header. *)
  method input_content_length : int
    (** Returns the ["Content-length"] request header field.
	@raise Not_found if it is not set.  *)
  method input_content_type_string : string
    (** Returns the ["Content-type"] request header field as a plain
        string or [""] if it is not set. *)
  method input_content_type :
    unit -> string * (string * Mimestring.s_param) list
    (** Returns the parsed ["Content-type"] request header field.
        @raise Not_found if it is not set.
        See also {!Mimestring.scan_mime_type_ep}. *)


  (** {3 Response header} *)

  method output_header : Netmime.mime_header
    (** The whole HTTP response header *)
  method output_header_field : ?default:string -> string -> string
    (** Returns the value of a field of the response header. If the
        field does not exist, [Not_found] will be raised unless the
        [default] argument is passed. The [default] argument determines
        the result of the function in this case.

        If there are several fields with the same name only the first
        field will be returned.

        The anonymous string is the name of the field.  The name is
        case-insensitive, and it does not matter whether it consists
        of lowercase or uppercase letters. If the name is a compound
        name, the parts are separated by "-", e.g. ["content-length"].
    *)
  method multiple_output_header_field : string -> string list
    (** Returns the values of all fields with the passed name of the
        repsonse header.  *)
  method output_header_fields : (string * string) list
    (** Returns the output header as (name,value) pairs.  The names may
        consist of lowercase or uppercase letters.  *)

  method set_output_header_field : string -> string -> unit
    (** Sets the value of a field of the response header.  The
        previous value, if any, is overwritten.  If there have been
        multiple values, all values will be removed and replaced by
        the single new value.  *)
  method set_multiple_output_header_field : string -> string list -> unit
    (** Sets multiple values of a field of the response header.  Any
        previous values are removed and replaced by the new values.
    *)
  method set_output_header_fields : (string * string) list -> unit
    (** Sets the complete response header at once. *)
  method set_status : Nethttp.http_status -> unit
    (** Sets the response status.  This is by definition the same as
	setting the [Status] output header field.  *)
  method send_output_header : unit -> unit
    (** This method will encode and send the output header to the
        output channel.  Note that of the output_channel is
        [`Transactionnal] (as opposed to [`Direct]), no output will
        actually take place before you issue [#commit_work()] -- thus
        a [#rollback_work()] will also rollback the headers as
        expected.  *)


  (** {3 The output channel transferring the response} *)

  method output_ch : Netchannels.out_obj_channel
    (** @deprecated Use [#out_channel] instead. *)

  method out_channel : Netchannels.out_obj_channel
    (** The "raw" output channel.  In general you should use instead
	{!Netcgi.cgi}[#out_channnel] which supports transactions (if you
	choose to).  Access to the "raw" channel is useful however,
	for example for sending images or download of files (for which
	transactions are not interesting but merely more work). *)

  (** {3 Logging} *)

  method log_error : string -> unit
    (** [#log_error msg] appends [msg] to the webserver log. *)


  (** {3 Miscellaneous} *)

  method config : config
    (** The configuration of the request. *)
end



(** {2 CGI object} *)

type other_url_spec = [ `Env | `This of string | `None ]
    (** Determines how an URL part is generated:
        - [`Env]: Take the value from the environment.
        - [`This v]: Use this value [v]. It must already be URL-encoded.
        - [`None]: Do not include this part into the URL.
    *)

type query_string_spec = [ `Env | `This of cgi_argument list | `None
                         | `Args of rw_cgi_argument list ]
    (** Determines how the query part of URLs is generated:
        - [`Env]: The query string of the current request.
        - [`This l]: The query string is created from the specified
                     argument list [l].
        - [`None]: The query string is omitted.
	- [`Args]: {i deprecated}, use [`This]
                   (left for backward compatibility).
    *)

type cache_control = [ `No_cache | `Max_age of int | `Unspecified ]
  (** This is only a small subset of the HTTP 1.1 cache control
      features, but they are usually sufficient, and they work for
      HTTP/1.0 as well.  The directives mean:

      - [`No_cache]: Caches are disabled.  The following headers are
      sent: [Cache-control: no-cache], [Pragma: no-cache], [Expires:]
      (now - 1 second). Note that many versions of Internet Explorer
      have problems to process non-cached contents when TLS/SSL is
      used to transfer the file. Use [`Max_age] in such cases (see
      http://support.microsoft.com/kb/316431).

      - [`Max_age n]: Caches are allowed to store a copy of the
      response for [n] seconds.  After that, the response must be
      revalidated.  The following headers are sent: [Cache-control:
      max-age n], [Cache-control: must-revalidate], [Expires:] (now +
      [n] seconds)

      - [`Unspecified]: No cache control header is added to the
      response.

      Notes:
      - Cache control directives only apply to GET requests; POST
      requests are never cached.
      - Not only proxies are considered as cache, but also the local
      disk cache of the browser.
      - HTTP/1.0 did not specify cache behaviour as strictly as
      HTTP/1.1 does.  Because of this the [Pragma] and [Expires]
      headers are sent, too.  These fields are not interpreted by
      HTTP/1.1 clients because [Cache-control] has higher precedence.
  *)



(** Object symbolizing a CGI-like request/response cycle.

    This is the minimal set of services a connector must provide.
    Additional methods may be defined for specific connectors.  *)
class type cgi =
object
  (** {3 Arguments -- data sent to the script} *)

  method argument : string -> cgi_argument
    (** [#argument name] returns the value of the argument named [name].
        If the argument appears several times, only one of its
        instances is used.
        @raise Not_found if no such argument exists. *)
  method argument_value : ?default:string -> string -> string
    (** [#argument_value] returns the value of the argument as a
        string.  If the argument does not exist, the [default] is
        returned.  @param default defaults to [""].  *)
  method argument_exists : string -> bool
    (** [#argument_exists] returns [false] if the named parameter is
        missing and [true] otherwise.  *)
  method multiple_argument : string -> cgi_argument list
    (** [#multiple_argument name] returns all the values of the
	argument named [name]. *)
  method arguments : cgi_argument list
    (** The complete list of arguments. *)

  method environment : cgi_environment
    (** The environment object.  This object is the "outer layer" of the
        activation object that connects it with real I/O channels.  *)
  method request_method : [ `GET | `HEAD | `POST | `DELETE
                          | `PUT of cgi_argument]
    (** The HTTP method used to make the request. *)

  method finalize  : unit -> unit
    (** This method calls [#finalize] for every CGI argument
        (including the possible one of PUT) to ensure that all files
        are deleted.  It also executes all functions registered with
        [#at_exit].  It does not close the in/out channels, however.
        This method is not registered in the garbage collector, and it
        is a bad idea to do so.  However, all connectors offered in
        Netcgi automatically call [#finalize] at the end of the
        request cycle (even when its terminated by an uncaught exception
        when [#config.default_exn_handler] is true) so you do not have
        to worry much about calling it yourself.  *)


  (** {3 Self-referencing URL} *)

  method url : ?protocol:Nethttp.protocol ->
               ?with_authority:other_url_spec ->        (* default: `Env *)
               ?with_script_name:other_url_spec ->      (* default: `Env *)
               ?with_path_info:other_url_spec ->        (* default: `Env *)
               ?with_query_string:query_string_spec ->  (* default: `None *)
               unit -> string
    (** Returns the URL of the current CGI-like script.  (Note that it
	may differ from the actual URL that requested the script if,
	for example, rewriting rules were specified in the web server
	configuration.)

        @param protocol The URL scheme.  By default, the URL scheme is
        used that is described in the environment

        @param with_authority Whether to include authority part
        (e.g. http or https) of the URL, and if yes, from which
        source.  Default: [`Env].

        @param with_script_name Whether to include the part of the URL
        path identifying the CGI script, and if yes, from which
        source.  Default: [`Env].

        @param with_path_info Whether to include the rest of the URL
        path exceeding the script name, and if yes, from which source.
        Default: [`Env].

        @param with_query_string Whether to include a query string,
        and if yes, which one.  Only arguments with [#store] being
        [`Memory] will be added.  Default: [`None], i.e. no query
        string.  *)


  (** {3 Outputting} *)

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
    (** Sets the header (removing any previous one).  When the output
        channel supports transactions, it is possible to set the
        header (possibly several times) until the [#out_channel] is
        commited for the first time or [#env#send_output_header()] is
        called.  When there is no support for transactions, the header
        must be set before the first byte of output is written.

        If [#set_header] is called a second time, it will overwrite
        {i all} the header fields.

        @param status Sets the HTTP status of the reply according to
	{{:http://www.w3.org/Protocols/rfc2616}RFC 2616}.  Defaults to
	"no status", but the server normally complements an [`Ok]
	status in this case.

        @param content_type Sets the content type.
	Defaults to ["text/html"].

	@param content_length Sets the content length (in bytes).
	Default: No such field.

	@param set_cookie Deprecated, use [set_cookies].

        @param set_cookies Sets a number of cookies.  Default: [[]].
	Remember that the browser may not support more than 20 cookies
	per web server.  You can query the cookies using [env#cookies]
	and [env#cookie].  If you set cookies, you want to think about
	an appropriate [cache] setting.  You may also want to add a
	{{:http://www.w3.org/P3P/}P3P} header (Platform for Privacy
	Preferences) -- otherwise your cookies may be discarded by
	some browsers.

        @param cache Sets the cache behavior for replies to GET
        requests.  The default is [`Unspecified].  {b It is strongly
        recommended to specify the caching behaviour!!!} You are on
        the safe side with [`No_cache], forcing every page to be
        regenerated. If your data do not change frequently, [`Max_age
        n] tells the caches to store the data at most [n] seconds.

        @param filename Sets the filename associated with the page.
        This filename is taken for the "save as..."  dialog.  Default:
        [""], i.e. no filename.  Note: It is bad practice if the
        filename contains problematic characters (backslash, double
        quote, space), or the names of directories.  It is recommended
        that you set [content_type] to "application/octet-stream" for
        this feture to work with most browsers and, if possible, to
        set [content_length] because that usually improves the
        download dialog.)

        @param script_type Sets the language of the script tag (for
        HTML replies).  It is recommended to use this field if there
        are [ONXXX] attributes containing scripts before the first
        [<SCRIPT>] element, because you cannot specify the script
        language for the [ONXXX] attributes otherwise.  [script_type]
        must be a media type, e.g. "text/javascript".  Default: no
        language is specified.

        @param style_type Sets the language of the style tag (for
        HTML replies).  It is recommended to use this field if there
        are [STYLE] attributes containing scripts before the first
        [<STYLE>] element, because you cannot specify the style
        language for the [STYLE] attributes otherwise.  [style_type]
        must be a media type, e.g. "text/css".  Default: no language
        is specified.

        @param fields Sets additional fields of the header.  Default: [[]].
    *)
  method set_redirection_header :
    ?set_cookies:Cookie.t list ->
    ?fields:(string * string list) list ->
    string -> unit
    (** Sets the header such that a redirection to the specified URL
        is performed.  If the URL begins with "http:" the redirection
        directive is passed back to the client, and the client will
        repeat the request for the new location (with a GET method).
        If the URL begins with "/", the server performs the
        redirection, and it is invisible for the client.  *)

  method output : Netchannels.trans_out_obj_channel
    (** @deprecated Use [#out_channel] instead. *)

  method out_channel : Netchannels.trans_out_obj_channel
    (** The output channel to which the generated content is intended
        to be written.  The header is not stored in this channel, so
        [#pos_out] returns the size of the DATA in bytes (useful to
        set Content-Length).  Note that HEAD requests must not send
        back a message body so, in this case, all data sent to this
        channel is discarded.  This allows your scripts to work
        unmodified for GET, POST and HEAD requests.

        The output channel may have transactional semantics, and
        because of this, it is an [trans_out_obj_channel].
        Implementations are free to support transactions or not.

        After all data have been written, the method [#commit_work()]
        {b must} be called, even if there is no support for
        transactions.

        Simple Example:
        {[
        cgi # out_channel # output_string "Hello world!\n";
        cgi # out_channel # commit_work()
        ]}

        Example for an error handler and a transaction buffer: If an
        error happens, it is possible to roll the channel back, and to
        write the error message.
        {[
        try
          cgi # set_header ... ();
          cgi # out_channel # output_string "Hello World!"; ...
          cgi # out_channel # commit_work();
        with err ->
          cgi # out_channel # rollback_work();
          cgi # set_header ... ();
          cgi # out_channel # output_string "Software error!"; ...
          cgi # out_channel # commit_work();
        ]}
    *)

  (* later:
     method send_mime_message : mime_message -> unit
     method begin_multipart_message : XXX -> unit
     method end_multipart_message : XXX -> unit
  *)


  method at_exit : (unit -> unit) -> unit
    (** [#at_exit f] registers the function [f] to be executed when
	[#finalize] is called (which is done automatically when the
	request finishes).  The functions are executed in the reverse
	order in which they were registered. *)
end


class type cgi_activation = cgi
  (** Alternate, more descriptive name for [cgi] *)




(** {2 Connectors} *)

type output_type =
  [ `Direct of string
  | `Transactional of config ->
      Netchannels.out_obj_channel -> Netchannels.trans_out_obj_channel
  ]
  (** The ouput type determines how generated data is buffered.
      - [`Direct sep]: Data written to the output channel of the
        activation object is not collected in a transaction buffer, but
        directly sent to the browser (the normal I/O buffering is still
        active, however, so call [#flush] to ensure that data is really
        sent).  The method [#commit_work] of the output channel is the
        same as [#flush].  The method [#rollback_work] causes that the
        string [sep] is sent, meant as a separator between the already
        generated output, and the now following error message.

      - [`Transactional f]: A transactional channel [tc] is created
        from the real output channel [ch] by calling [f cfg ch] (here,
        [cfg] is the CGI configuration).  The channel [tc] is propagated
        as the output channel of the activation object. This means that
        the methods [commit_work] and [rollback_work] are implemented by
        [tc], and the intended behaviour is that data is buffered in a
        special transaction buffer until [commit_work] is called.  This
        invocation forces the buffered data to be sent to the
        browser. If, however, [rollback_work] is called, the buffer is
        cleared.

      Two important examples for [`Transactional] are:
      - The transaction buffer is implemented in memory:
      {[
      let buffered _ ch = new Netchannels.buffered_trans_channel ch in
      `Transactional buffered
      ]}
      - The transaction buffer is implemented as an external file:
      {[
      `Transactional(fun _ ch -> new Netchannels.tempfile_output_channel ch)
      ]}
  *)


val buffered_transactional_outtype : output_type
  (** The [output_type] implementing transactions with a RAM-based buffer *)

val buffered_transactional_optype : output_type
  (** {b Deprecated} name for [buffered_transactional_outtype] *)

val tempfile_transactional_outtype : output_type
  (** The [output_type] implementing transactions with a tempfile-based
      buffer
   *)

val tempfile_transactional_optype : output_type
  (** {b Deprecated} name for [tempfile_transactional_outtype] *)


type arg_store = cgi_environment -> string -> Netmime.mime_header_ro ->
  [ `Memory | `File | `Automatic | `Discard
  | `Memory_max of float | `File_max of float | `Automatic_max of float]
    (** This is the type of functions [arg_store] so that [arg_store env
        name header] tells whether to [`Discard] the argument or to
        store it into a [`File] or in [`Memory].  The parameters passed
        to [arg_store] are as follows:

        - [env] is the CGI environment.  Thus, for example, you can have
        different policies for different [cgi_path_info].

        - [name] is the name of the argument.

        - [header] is the MIME header of the argument (if any).

        Any exception raised by [arg_store] will be treated like if it
        returned [`Discard].  Note that the [`File] will be treated
        like [`Memory] except for [`POST] "multipart/form-data" and
        [`PUT] queries.

        [`Automatic] means to store it into a file if the header
        contains a file name and otherwise in memory (strictly
        speaking [`Automatic] is not necessary since [arg_store] can
        check the header but is provided for your convenience).

        [`Memory_max] (resp. [`File_max], resp. [`Automatic_max]) is
        the same as [`Memory] (resp. [`File], resp. [`Automatic])
        except that the parameter indicates the maximum size in kB of
        the argument value.  If the size is bigger, the
        {!Netcgi.cgi_argument} methods [#value] and [#open_value_rd]
        methods will raise {!Netcgi.Argument.Oversized}.

        Remark: this allows for fine grained size constraints while
        {!Netcgi.config}[.input_content_length_limit] option is a
        limit on the size of the entire request.  *)


type exn_handler = cgi_environment -> (unit -> unit) -> unit
  (** A function of type [exn_handler] allows to define a custom
      handler of uncaught exceptions raised by the [unit -> unit]
      parameter.  A typical example of [exn_handler] is as follows:
      {[
      let exn_handler env f =
        try f()
        with
        | Exn1 -> (* generate error page *)
            env#set_output_header_fields [...];
            env#send_output_header();
            env#out_channel#output_string "...";
            env#out_channel#close_out()
        | ...
      ]} *)

type connection_directive =
    [ `Conn_close | `Conn_close_linger | `Conn_keep_alive
    | `Conn_error of exn
    ]
  (** Directive how to go on with the current connection:
    * - [`Conn_close]: Just shut down and close descriptor
    * - [`Conn_close_linger]: Linger, shut down, and close descriptor
    * - [`Conn_keep_alive]: Check for another request on the same connection
    * - [`Conn_error e]: Shut down and close descriptor, and handle the
    *   exception [e]
   *)


(** Specific connectors can be found in separate modules.  For example:

    - {!Netcgi_cgi}:  classical CGI.
    - {!Netcgi_fcgi}: FastCGI protocol.
    - {!Netcgi_ajp}:  AJP 1.3 connector (JSERV protocol).
    - {!Netcgi_mod}:  connector binding to Apache API.
    - {!Netcgi_scgi}: SCGI connector.
    - {!Netcgi_test}: special "connector" to test your code.

    A typical use is as follows:
    {[
    open Netcgi

    let main (cgi:cgi) =
       let arg = cgi#argument_value "name" in
       ...
       cgi#out_channel#commit_work()

    let () =
      let buffered _ ch = new Netchannels.buffered_trans_channel ch in
      Netcgi_cgi.run ~output_type:(`Transactional buffered) main
    ]}
*)



(*
  Local Variables:
  mode: outline-minor
  outline-regexp: " *\\(val\\|module\\|class\\|type\\) "
  End:
*)
