(* netcgi_apache.mli

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

(** Netcgi Apache "mod" connector.
 *
 * See the {!Netcgi_mod.setup} section at the end of this file to know
 * how to configure Apache.
 *)


(** Interface to Apache API. *)
module Apache :
sig

  module Table :
  sig
    type t (** Apache [table] structure. *)

    external get : t -> string -> string          = "netcgi2_apache_table_get"
      (** [Table.get tbl key] returns the corresponding entry in the
          table.  The [key] is matched case-insensitively.  The first
          key will retrieved in case there are more than one entry in
          the table for the key.
	  @raise Not_found otherwise. *)
    external get_all : t -> string -> string list
      = "netcgi2_apache_table_get_all"
      (** [Table.get_all tbl key] same as [Table.get tbl key] except
          it returns all values corresponding to the [key]. *)
    external fields : t -> (string * string) list
      = "netcgi2_apache_table_fields"
      (** [Table.fields tbl] returns a list of [(key, value)] pairs
          consisting of all entries of the table [tbl]. *)

    external clear : t -> unit                    = "netcgi2_apache_table_clear"
      (** [Table.clear tbl] removes all key/value pairs from the table
          [tbl]. *)
    external set : t -> string -> string -> unit   = "netcgi2_apache_table_set"
      (** [Table.set tbl key value] sets the [(key, value)] pair in
          the table [tbl].  If one or more key/value pairs already
          exists with the same key they will be deleted and replaced
          with [(key, value)].  *)
    external add : t -> string -> string -> unit   = "netcgi2_apache_table_add"
      (** [Table.add tbl key value] adds the [(key, value)] pair in
          the table [tbl].  This does not erase any existing pairs. *)
    external unset : t -> string -> unit = "netcgi2_apache_table_unset"
      (** [Table.unset tbl key] delete every key/value pair associated
          with the [key] from the table [tbl]. *)
  end

  module Server :
  sig
    type t (** Apache [server_rec] structure. *)

    external hostname : t -> string 	= "netcgi2_apache_server_hostname"
      (** [server_hostname] field (as declared in Apache configuration file).
	  @raise Not_found if NULL. *)
    external admin : t -> string	= "netcgi2_apache_server_admin"
      (** [server_admin] field.
	  @raise Not_found if NULL. *)
    external is_virtual : t -> bool	= "netcgi2_apache_server_is_virtual"
      (** [is_virtual] field.
	  @raise Not_found if NULL. *)
  end

  module Connection :
  sig
    type t (** Apache [conn_rec] structure. *)

    external remote_ip : t -> string 	= "netcgi2_apache_connection_remote_ip"
      (** [conn_rec] [remote_ip] field.
          @raise Not_found if NULL. *)
    external remote_host : t -> string = "netcgi2_apache_connection_remote_host"
      (** [conn_rec] [remote_host] field.
          @raise Not_found if NULL. *)
  end

  module Request :
  sig
    type t (** Apache [request_rec] structure. *)

    external connection : t -> Connection.t
      = "netcgi2_apache_request_connection"
      (** [request_rec] [connection] field. *)
    external server : t -> Server.t 	= "netcgi2_apache_request_server"
      (** [request_rec] [server] field. *)
    external next : t -> t 		= "netcgi2_apache_request_next"
      (** [request_rec] [next] field.
          @raise Not_found if NULL. *)
    external prev : t -> t 		= "netcgi2_apache_request_prev"
      (** [request_rec] [prev] field.
          @raise Not_found if NULL. *)
    external main : t -> t 		= "netcgi2_apache_request_main"
      (** [request_rec] [main] field.
          @raise Not_found if NULL. *)
    external the_request : t -> string 	= "netcgi2_apache_request_the_request"
      (** [request_rec] [the_request] field.
          @raise Not_found if NULL. *)
    external assbackwards : t -> bool	= "netcgi2_apache_request_assbackwards"
      (** [request_rec] [assbackwards] field; [true] if HTTP/0.9,
	  "simple" request. *)

    external header_only : t -> bool 	= "netcgi2_apache_request_header_only"
      (** [request_rec] [header_only] field.  It is [true] when the
	  request method is HEAD. *)
    external protocol : t -> string 	= "netcgi2_apache_request_protocol"
      (** [request_rec] [protocol] field.
	  @raise Not_found if NULL. *)
    external proto_num : t -> int 	= "netcgi2_apache_request_proto_num"
      (** [request_rec] [proto_num] field.  Number version of
	  protocol; 1.1 = 1001 *)
    external hostname : t -> string 	= "netcgi2_apache_request_hostname"
      (** [request_rec] [hostname] field -- hostname to which the
	  request was made.
	  @raise Not_found if NULL. *)
    external request_time : t -> float 	= "netcgi2_apache_request_request_time"
      (** [request_rec] [request_time] field. *)
    external status_line : t -> string 	= "netcgi2_apache_request_status_line"
      (** [request_rec] [status_line] field.
	  @raise Not_found if NULL. *)
    external set_status_line : t -> string -> unit
      = "netcgi2_apache_request_set_status_line"
      (** Set [request_rec] [status_line] field. *)
    external status : t -> int 		= "netcgi2_apache_request_status"
      (** [request_rec] [status] field. *)
    external set_status : t -> int -> unit = "netcgi2_apache_request_set_status"
      (** Set [request_rec] [status] field. *)

    external method_name : t -> string 	= "netcgi2_apache_request_method"
      (** [request_rec] [method] field. *)
    val method_number : t ->
      [ `GET | `PUT | `POST | `DELETE | `CONNECT | `OPTIONS | `TRACE | `PATCH
      | `PROPFIND | `PROPPATCH | `MKCOL | `COPY | `MOVE | `LOCK | `UNLOCK
      | `INVALID ]
      (** [request_rec] [method_number] field (given symbolically
          instead of a number). *)

    external headers_in : t -> Table.t 	= "netcgi2_apache_request_headers_in"
      (** [request_rec] [headers_in] field. *)
    external headers_out : t -> Table.t = "netcgi2_apache_request_headers_out"
      (** [request_rec] [headers_out] field. *)
    external err_headers_out : t -> Table.t
      = "netcgi2_apache_request_err_headers_out"
      (** [request_rec] [err_headers_out] field. *)
    external subprocess_env : t -> Table.t
      = "netcgi2_apache_request_subprocess_env"
      (** [request_rec] [subprocess_env] field. *)
    external notes : t -> Table.t = "netcgi2_apache_request_notes"
      (** [request_rec] [notes] field. *)
    external content_type : t -> string
      = "netcgi2_apache_request_content_type"
      (** [request_rec] [content_type] field.
          @raise Not_found if NULL. *)
    external set_content_type : t -> string -> unit
      = "netcgi2_apache_request_set_content_type"
      (** Set [request_rec] [content_type] field. *)

    external uri : t -> string = "netcgi2_apache_request_uri"
      (** [request_rec] [uri] field.
          @raise Not_found if NULL. *)
    external port : t -> int		= "netcgi2_apache_request_port"
      (** Server port. *)
    external set_uri : t -> string -> unit = "netcgi2_apache_request_set_uri"
      (** Set [request_rec] [uri] field. *)
    external filename : t -> string = "netcgi2_apache_request_filename"
      (** [request_rec] [filename] field.
          @raise Not_found if NULL. *)
    external set_filename : t -> string -> unit
      = "netcgi2_apache_request_set_filename"
      (** Set [request_rec] [filename] field. *)
    external path_info : t -> string = "netcgi2_apache_request_path_info"
      (** [request_rec] [path_info] field.
          @raise Not_found if NULL. *)
    external set_path_info : t -> string -> unit
      = "netcgi2_apache_request_set_path_info"
      (** Set [request_rec] [path_info] field. *)
    external args : t -> string = "netcgi2_apache_request_args"
      (** [request_rec] [args] field.
          @raise Not_found if NULL. *)
    external set_args : t -> string -> unit
      = "netcgi2_apache_request_set_args"
      (** Set [request_rec] [args] field. *)
    external finfo : t -> Unix.stats option = "netcgi2_apache_request_finfo"
      (** [request_rec] [finfo] field. *)


    (** Policy to apply by [setup_client_block] if the request message
        indicates a body. *)
    type read_policy =
        | NO_BODY       (** Send 413 error if message has any body *)
        | CHUNKED_ERROR (** Send 411 error if body without Content-Length *)
        | CHUNKED_DECHUNK (** If chunked, remove the chunks for me. *)
        | CHUNKED_PASS  (** Pass the chunks to me without removal. *)

    val setup_client_block : t -> read_policy -> unit
      (** Setup for reading client request.
	  @raise Netcgi_common.HTTP in case of problems.  *)
    external should_client_block : t -> bool
      = "netcgi2_apache_request_should_client_block"
        (** Returns true if there is any client request data. *)
    val get_client_block : t -> string
      (** Get client request data.
          @raise Netcgi_common.HTTP in case of reading error. *)
    val get_client_block_buf : t -> string -> int -> int -> int
      (** [get_client_block_buf r buf ofs len] read a chunk of data
	  and puts it in [buf.[ofs .. ofs+len-1]].  The return value
	  [i] is the number of bytes actually read -- thus only
	  [buf.[ofs .. ofs+i-1]] is meaningful.

	  @raise Netcgi_common.HTTP in case of reading error. *)
    val discard_request_body : t -> unit
      (** Discard client request body.
          @raise Netcgi_common.HTTP in case of problems. *)

    external user : t -> string = "netcgi2_apache_request_user"
      (** The authenticated user.
          @raise Not_found if NULL. *)
    external auth_type : t -> string = "netcgi2_apache_auth_type"
    external note_auth_failure : t -> unit
      = "netcgi2_apache_request_note_auth_failure"
      (** Set headers to tell browser that authentication failed. *)
    external note_basic_auth_failure : t -> unit
      = "netcgi2_apache_request_note_basic_auth_failure"
      (** Set headers to tell browser that basic authentication failed. *)
    external note_digest_auth_failure : t -> unit
      = "netcgi2_apache_request_note_digest_auth_failure"
      (** Set headers to tell browser that digest authentication failed. *)
    val get_basic_auth_pw : t -> string option
      (** Get the password sent in basic authentication.
          @raise  Netcgi_common.HTTP in case of problems. *)
    external internal_redirect : string -> t -> unit
      = "netcgi2_apache_request_internal_redirect"
      (** Internally redirects immediately to [uri]. *)
    external internal_redirect_handler : string -> t -> unit
      = "netcgi2_apache_request_internal_redirect_handler"
      (** Internally redirects immediately to [uri] using handler specified
          by [r]. *)

    external send_http_header : t -> unit
      = "netcgi2_apache_request_send_http_header"
      (** Send the HTTP headers.  Note that you must set the Status
	  and Content-Type with [set_status] and [set_content_type]
	  respectively. *)

    val rflush : t -> unit
      (** Flush any buffered data waiting to be written to the client.
          @raise End_of_file if it is not possible. *)

    external print_char : t -> char -> unit =
      "netcgi2_apache_request_print_char"
      (** Send a character back to the client.  *)
    val print_string : t -> string -> unit
      (** Send a string back to the client. *)
    val output : t -> string -> int -> int -> int
      (** [output r s ofs len] send [s[ofs .. len-1]] back to the
	  client.  Returns the number of bytes actually written, which
	  is smaller than the number of bytes in the string if there
	  was a failure. *)

    val print_int : t -> int -> unit
      (** Send a decimal number back to the client. *)
    val print_float : t -> float -> unit
      (** Send a floating-point number back to the client. *)
    val print_newline : t -> unit
      (** Send a CR LF back to the client. *)
    val print_endline : t -> string -> unit
      (** Send a string followed by CR LF back to the client. *)
    external register_cleanup : t -> (unit -> unit) -> unit
      = "netcgi2_apache_request_register_cleanup"
      (** Register a cleanup function which is called when the current
          request cycle ends.  *)
  end
end

(* ---------------------------------------------------------------------- *)

(** Registering Apache handlers. *)
module Handler :
sig
  type result =
      | OK	 (** This stage of request processing was handled
                     successfully. *)
      | DECLINED (** No erroneous condition exists, but the module
                     declines to handle the phase; the server tries to
                     find another. *)
      | DONE	 (** The request is completely satisfied. *)
      | HTTP of int (** The request returns the HTTP status code given
                        as argument. *)

  type t = Apache.Request.t -> result
    (** The type of handler functions.  The exception [Exit] is
        considered as a normal way of terminating early.  All other
        exceptions are logged and result in an Internal_server_error
        response by Apache. *)

  val register : t -> string -> unit
    (** Modules may call [register fn name] to register one or more
        handler functions.  The handler functions are then referred to
        in the [Netcgi*Handler] configuration commands as
        [Module_name.name] where [Module_name] is derived from the
        filename (given to the [NetcgiLoad] directive) and [name] is the
        string passed here.  *)
end


(* ---------------------------------------------------------------------- *)

open Netcgi

(** The usual [cgi] class with an additional method to access Apache
    specificities.  *)
class type cgi =
object
  inherit Netcgi.cgi

  method request : Apache.Request.t
    (** The underlying apache request structure. *)
end


val run :
  ?config:config ->
  ?output_type:output_type ->
  ?arg_store:arg_store ->
  ?exn_handler:exn_handler ->
  (cgi -> unit) -> unit
  (** [run f] register the function [f] as a main function of the
      script.  Each call to the script will execute [f cgi].  The code
      outside [f] will be executed only once (when the script is
      loaded into memory) which allows to cache database connections,
      etc.  (The code stays in memory unless you restart the server or
      the file changes on disk.)

      @param config Default: {!Netcgi.default_config}
      @param output_type Default: [`Direct ""]
      @param arg_store Default: [`Automatic] for all arguments.
      @param exn_handler See {!Netcgi.exn_handler}.  Default: delegate
      all exceptions to the default handler.  *)


(* ---------------------------------------------------------------------- *)

(** {2:setup Setup}

    {3 Apache 1.3}

    You need to put in an Apache configuration file (we recommend
    /etc/apache/conf.d/netcgi_apache.conf) the following lines:
    {v
    LoadModule netcgi_module /usr/lib/apache/1.3/mod_netcgi_apache.so
    NetcgiRequire netcgi2-apache
    v}

    {3 Apache 2.2 or later}

    You need to put in an Apache configuration file (we recommend
    /etc/apache2/mods-available/netcgi_apache.load) the following line:
    {v
    LoadModule netcgi_module /usr/lib/apache2/modules/mod_netcgi_apache.so
    NetcgiRequire netcgi2-apache
    v}
    and make a symbolic link from /etc/apache2/mods-enabled/ to it to
    actually enable it.  Subsequent configuration is recommended to be
    in /etc/apache2/mods-available/netcgi_apache.conf (also to be
    linked to /etc/apache2/mods-enabled/).

    {3 Loading libraries}

    If your scripts depend on other libraries, you need to load them
    using [NetcgiLoad].  More specifically, if your library is x.cma and
    is in the subdirectory y of standard OCaml directory (given by
    `ocamlc -where`), use
    {v
    NetcgiLoad y/x.cma
    v}
    If x.cma is not in a subdirectory of `ocamlc -where`, you need to
    specify the full path.

    {3 Interaction with findlib}

    Libraries managed with findlib are specially supported. In order to
    load a library "lib" just use
    {v
    NetcgiRequire lib
    v}
    Findlib-managed libraries are automatically found.

    For special configurations one can also set Findlib predicates:
    {v
    NetcgiPredicates p1,p2,p3,...
    v}

    {3 Multi-threading}

    If you need multi-threading call
    {v
    NetcgiThread
    v}
    as the very first directive after [LoadModule], even before
    [NetcgiRequire netcgi2-apache] (otherwise a number of
    critical sections remain unprotected in Ocamlnet, and you'll experience
    crashes).

    {3 Installing scripts}

    You need also to tell Apache how to detect whether a script is to
    be handled by netcgi_apache, either by putting them in a special
    directory (here /caml-bin/):
    {v
    Alias /caml-bin/ /path/to/your/scripts/
    <Location /caml-bin>
      SetHandler ocaml-bytecode
      NetcgiHandler Netcgi_apache.bytecode
      Options ExecCGI
      Allow from all
    </Location>
    v}
    or by distinguishing them by their extension (here [.cma]):
    {v
    NetcgiHandler Netcgi_apache.bytecode
    AddHandler ocaml-bytecode .cma
    v}

    {3 Compiling scripts}

    If your script reside in the file [x.ml], compile it to [x.cmo] or
    [x.cma].  If your script depends on other libraries, you may
    either load them with [NetcgiLoad] (see above) or include them in
    [x.cma].  You need not include the [netcgi_apache.cma],
    [netcgi.cma], [netstring.cma], [netsys.cma], or [pcre.cma] modules
    as these are already loaded into Apache (see above).
*)

