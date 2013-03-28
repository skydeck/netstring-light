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


module Conf :
sig
  val package : string
  val version : string
  val apache_libdir : string
  val apache_major : int
  val gateway_interface : string
  val server_software : string
end


module Raw_Apache :
sig

  (** See {!Netcgi_apache.Apache.Table} *)
  module Table :
  sig
    type t (** Apache [table] structure. *)
    external get : t -> string -> string          = "netcgi2_apache_table_get"
    external get_all : t -> string -> string list
      = "netcgi2_apache_table_get_all"
    external fields : t -> (string * string) list
      = "netcgi2_apache_table_fields"
    external clear : t -> unit                    = "netcgi2_apache_table_clear"
    external set : t -> string -> string -> unit   = "netcgi2_apache_table_set"
    external add : t -> string -> string -> unit   = "netcgi2_apache_table_add"
    external unset : t -> string -> unit = "netcgi2_apache_table_unset"
  end

  (** See {!Netcgi_apache.Apache.Server} *)
  module Server :
  sig
    type t (** Apache [server_rec] structure. *)
    external hostname : t -> string 	= "netcgi2_apache_server_hostname"
    external admin : t -> string	= "netcgi2_apache_server_admin"
    external is_virtual : t -> bool	= "netcgi2_apache_server_is_virtual"
  end

  (** See {!Netcgi_apache.Apache.Connection} *)
  module Connection :
  sig
    type t (** Apache [conn_rec] structure. *)
    external remote_ip : t -> string 	= "netcgi2_apache_connection_remote_ip"
    external remote_host : t -> string = "netcgi2_apache_connection_remote_host"
  end

  (** See {!Netcgi_apache.Apache.Request} *)
  module Request :
  sig
    type t (** Apache [request_rec] structure. *)

    external connection : t -> Connection.t
      = "netcgi2_apache_request_connection"
    external server : t -> Server.t 	= "netcgi2_apache_request_server"
    external next : t -> t 		= "netcgi2_apache_request_next"
    external prev : t -> t 		= "netcgi2_apache_request_prev"
    external main : t -> t 		= "netcgi2_apache_request_main"
    external the_request : t -> string 	= "netcgi2_apache_request_the_request"
    external assbackwards : t -> bool	= "netcgi2_apache_request_assbackwards"

    external header_only : t -> bool 	= "netcgi2_apache_request_header_only"
    external protocol : t -> string 	= "netcgi2_apache_request_protocol"
    external proto_num : t -> int 	= "netcgi2_apache_request_proto_num"
    external hostname : t -> string 	= "netcgi2_apache_request_hostname"
    external request_time : t -> float 	= "netcgi2_apache_request_request_time"
    external status_line : t -> string 	= "netcgi2_apache_request_status_line"
    external set_status_line : t -> string -> unit
      = "netcgi2_apache_request_set_status_line"
    external status : t -> int 		= "netcgi2_apache_request_status"
    external set_status : t -> int -> unit = "netcgi2_apache_request_set_status"

    external method_name : t -> string 	= "netcgi2_apache_request_method"
    val method_number : t ->
      [ `GET | `PUT | `POST | `DELETE | `CONNECT | `OPTIONS | `TRACE | `PATCH
      | `PROPFIND | `PROPPATCH | `MKCOL | `COPY | `MOVE | `LOCK | `UNLOCK
      | `INVALID ]

    external headers_in : t -> Table.t 	= "netcgi2_apache_request_headers_in"
    external headers_out : t -> Table.t = "netcgi2_apache_request_headers_out"
    external err_headers_out : t -> Table.t
      = "netcgi2_apache_request_err_headers_out"
    external subprocess_env : t -> Table.t
      = "netcgi2_apache_request_subprocess_env"
    external notes : t -> Table.t = "netcgi2_apache_request_notes"
    external content_type : t -> string
      = "netcgi2_apache_request_content_type"
    external set_content_type : t -> string -> unit
      = "netcgi2_apache_request_set_content_type"

    external uri : t -> string            = "netcgi2_apache_request_uri"
    external port : t -> int		  = "netcgi2_apache_request_port"
    external set_uri : t -> string -> unit = "netcgi2_apache_request_set_uri"
    external filename : t -> string = "netcgi2_apache_request_filename"
    external set_filename : t -> string -> unit
      = "netcgi2_apache_request_set_filename"
    external path_info : t -> string = "netcgi2_apache_request_path_info"
    external set_path_info : t -> string -> unit
      = "netcgi2_apache_request_set_path_info"
    external args : t -> string = "netcgi2_apache_request_args"
    external set_args : t -> string -> unit
      = "netcgi2_apache_request_set_args"
    external finfo : t -> Unix.stats option = "netcgi2_apache_request_finfo"

    type read_policy =
        | NO_BODY
        | CHUNKED_ERROR
        | CHUNKED_DECHUNK
        | CHUNKED_PASS

    external setup_client_block : t -> read_policy -> int
      = "netcgi2_apache_request_setup_client_block"
    external should_client_block : t -> bool
      = "netcgi2_apache_request_should_client_block"
    external get_client_block : t -> string
      = "netcgi2_apache_request_get_client_block"
    external get_client_block_buffer : t -> string -> int -> int -> int
      = "netcgi2_apache_request_get_client_block_buffered"
    external discard_request_body : t -> int
      = "netcgi2_apache_request_discard_request_body"

    external user : t -> string = "netcgi2_apache_request_user"
    external auth_type : t -> string = "netcgi2_apache_auth_type"
    external note_auth_failure : t -> unit
      = "netcgi2_apache_request_note_auth_failure"
    external note_basic_auth_failure : t -> unit
      = "netcgi2_apache_request_note_basic_auth_failure"
    external note_digest_auth_failure : t -> unit
      = "netcgi2_apache_request_note_digest_auth_failure"
    external get_basic_auth_pw : t -> int * string option
      = "netcgi2_apache_request_get_basic_auth_pw"

    external send_http_header : t -> unit
      = "netcgi2_apache_request_send_http_header"
    external rflush : t -> int
      = "netcgi2_apache_request_rflush"
    external internal_redirect : string -> t -> unit
      = "netcgi2_apache_request_internal_redirect"
    external internal_redirect_handler : string -> t -> unit
      = "netcgi2_apache_request_internal_redirect_handler"

    external print_char : t -> char -> unit =
      "netcgi2_apache_request_print_char"
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


module Handler :
sig
  (** See {!Netcgi_apache.Handler.result}. *)
  type result =
      | OK
      | DECLINED
      | DONE
      | HTTP of int

  type t = Raw_Apache.Request.t -> result
    (** See {!Netcgi_apache.Handler.t}. *)

  val register : t -> string -> unit
    (** See {!Netcgi_apache.Handler.register}. *)
end
