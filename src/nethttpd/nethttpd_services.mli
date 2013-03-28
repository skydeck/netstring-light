(* $Id: nethttpd_services.mli 1642 2011-07-20 20:46:25Z gerd $
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
 * along with Nethttpd; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** {1 Service Providers for HTTP daemon}
  *
  * This module defines the basic service providers that handle requests and
  * generate responses. The service providers can be used with both the
  * reactive and the event-based daemon encapsulations.
 *)

(* Predefined service providers: *)

open Nethttpd_types

type host =
    { server_pref_name : string option;
        (** The preferred name of the host. This can be a DNS name or an IP address. *)
      server_pref_port : int option;
        (** The preferred port of the host *)
      server_names : (string * int) list;
        (** Names and ports that match this host definition, for
	 * name-based virtual hosting. The name ["*"] matches any name, and the port
	 * 0 matches any port.
	 *)
      server_addresses : (Unix.inet_addr * int) list;
        (** IP addresses and ports that also match this host definition, for
	 * IP-based virtual hosting. The address [Unix.inet_addr_any] matches any
	 * address, and the port 0 matches any port.
	 *)
    }
  (** For name- and IP-based virtual hosting this record describes the individual
    * host. An incoming request matches this host if:
    * - The [Host] header is mentioned in [server_names], or if
    * - The request has been accepted on the port contained in [server_addresses]
    *
    * If [server_pref_name] is set, the name of the host is set to this string for
    * further processing (esp. [cgi_server_name]). If not set, the name of the
    * host is set to the name corresponding to the matching entry of [server_names]
    * or [server_addresses], or if this does not succeed, to the real IP address.
    *
    * If [server_pref_port] is set, the port of the host is set to this string for
    * further processing (esp. [cgi_server_port]). If not set, the port of the
    * host is set to the port corresponding to the matching entry of [server_names]
    * or [server_addresses], or if this does not succeed, to the real port number.
   *)


type 'a host_distributor =
      ( host * 'a http_service ) list
  (** Describes virtual hosting by pairs [(host,service)]: If [host] matches the
    * incoming request, the corresponding [service] is performed to generate the
    * response.
   *)

val host_distributor : 
      'a host_distributor -> [> `Host_distributor of 'a host_distributor ] http_service
  (** Configures virtual hosting *)

val default_host : ?pref_name:string -> ?pref_port:int -> unit -> host
  (** Creates a [host] record that matches any request. [pref_name] and [pref_port]
    * can be used to specify preferred names.
   *)

val options_service : unit -> [> `Options_service ] http_service
  (** This service responds to "OPTIONS *" requests, and nothing else *)

type 'a uri_distributor =
    ( string * 'a http_service ) list
  (** Describes that services are bound to URI prefixes. The strings are URI paths
    * (without URI escaping).
    * For an incoming request URI, the longest matching prefix is selected, and
    * the corresponding service is carried out.
    *
    * If the URI path in the list ends with a slash, it can only be selected if the
    * incoming request URI also includes the slash.
    *
    * If the URI path in the list does not end with a slash, it can only be selected
    * if the incoming request URI is exactly identical, or continues the path with
    * a slash.
   *)

val uri_distributor : 
      'a uri_distributor -> [> `Uri_distributor of 'a uri_distributor ] http_service
  (** Configures URI distribution. The incoming request URI is normalized before
    * being matched, and the request is rewritten to the normalized URI.
    *
    * Normalization includes:
    * - Removal of [.] path components
    * - Resolution of [..] path components
    * 
    * If the path starts with [..] after normalization, the request is rejected.
   *)

type 'a linear_distributor =
    ( (extended_environment -> bool) * 'a http_service ) list
  (** Services are selected by calling the selector function. The first service
    * for which the function returns [true] is selected.
   *)

val linear_distributor :
      'a linear_distributor -> 
        [> `Linear_distributor of 'a linear_distributor ] http_service
  (** Configures linear distribution *)

type method_filter =
    [ `Limit of string list
    | `Limit_except of string list
    ]
  (** The request is only accepted if listed (for [`Limit]), or if not listed
    * (for [`Limit_except]).
   *)

type 'a method_distributor =
   ( method_filter * 'a http_service ) list
  (** The first service is selected for which the method filter accepts the request *)

val method_distributor : 
      'a method_distributor -> 
         [> `Method_distributor of 'a method_distributor ] http_service
  (** Configures method distribution *)

type file_option =
    [ `Enable_gzip
    | `Enable_cooked_compression
    | `Override_compression_suffixes of (string * string) list
    | `Enable_index_file of string list
    | `Enable_listings of 
	extended_environment -> Netcgi.cgi_activation -> file_service -> unit
    ]
  (** Add-on features for file services:
    * - [`Enable_gzip]: Deprecated. Same as [`Enable_cooked_compression].
    * - [`Enable_cooked_compression]: Modifies the way compressed files
    *   are handled. Normally it is required that one accesses the compressed
    *   file (with suffix such as "gz") directly to get it in compressed form.
    *   If this option is enabled, though, the server also compresses
    *   the base file (without suffix such as "gz"), but only if the
    *   base file is accompanied by a compressed version (with suffix).
    *   E.g. if there is "foo" and "foo.gz", this enables that the accesses
    *   to "foo" can make use of compression.
    * - [`Enable_index_file]: If enabled, accesses to directories are redirected
    *   to index files. The possible file names are given in the string list.
    *   E.g. [`Enable_index_file ["index.html"; "index.htm"]]. It is redirected to
    *   these files, so these can be handled by different services if neccessary.
    * - [`Enable_listings]: If enabled, directory listings are generated by calling
    *   the argument function. The [PATH_TRANSLATED] property of the environment
    *   contains the absolute name of the directory to list. The [PATH_INFO] property
    *   is the corresponding URI path. [SCRIPT_NAME] is meaningless.
    * - [`Override_compression_suffixes l]: Tags the file suffixes in
    *   [l] as compression schemes. A pair [(suffix,ce)] sets that the
    *   [suffix] means the content encoding [ce]. Knowing this is important
    *   for determining the media type of the file.
   *)

and file_service =
    { file_docroot : string;
        (** The document root for this file service *)
      file_uri : string;
        (** The URI prefix corresponding to the document root. Escapes are not allowed *)
      file_suffix_types : (string * string) list;
        (** Maps the file suffixes (after the dot) to media types *)
      file_default_type : string;
        (** The media type to use if suffix mapping fails *)
      file_options : file_option list;
        (** Further options for files
	 *)
    }
  (** Describes a file service *)

val file_service : file_service -> [> `File_service of file_service ] http_service
  (** Configures a file service *)

val file_translator : file_service -> string -> string
  (** Translates an URI path to a file name. Raises [Not_found] if not
      possible. It is not checked whether the resulting file exists.

      This function removes a trailing slash of the translated path, if any,
      and if resulting from appending the path info component.
      Trailing slashes must not be used to deduce that directories are
      accessed.
   *)

val simple_listing : ?hide:string list -> 
             extended_environment -> Netcgi.cgi_activation -> file_service -> unit
  (** Simple listing generator for [`Enable_listings] 
    *
    * [hide]: An optional list of Str regular expressions. File names matching one
    * of the regexps are hidden in the listing. Defaults to hiding files starting with
    * a dot, and files ending in a tilde character. (Changed in Ocamlnet-3.3:
    * [hide] uses now Str regexps, and no longer PCRE regexps.)
   *)

type std_activation_options =
   { stdactv_processing : Netcgi.arg_store option;
     stdactv_operating_type : Netcgi.output_type option;
   } 
  (** These are options for [`Std_activation]. For explanations, see the [Netcgi]
    * module.
   *)

type std_activation =
  [ `Std_activation of std_activation_options
  | `Std_activation_unbuffered          (* Shortcut for common case *)
  | `Std_activation_buffered            (* Shortcut for common case *)
  | `Std_activation_tempfile            (* Shortcut for common case *)
  ]
  (** The way the [Netcgi_types.cgi_activation] object is created. For typical
    * usage, just take:
    * - [`Std_activation_unbuffered]: Creates a [Netcgi.std_activation] without
    *   output buffer (type [`Direct ""]) and memory-based argument processing
    * - [`Std_activation_buffered]:  Creates a [Netcgi.std_activation]
    *   with a transactions buffer in memory, and memory-based argument processing
    * - [`Std_activation_tempfile]: Creates a [Netcgi.std_activation]
    *   with a file as transactions buffer, and memory-based argument processing
    *
    * The following option is provided for detailed control:
    * - [`Std_activation opt]: Creates a [Netcgi.std_activation] with the given
    *   options
   *)

type 'a dynamic_service =
    { dyn_handler : extended_environment -> 'a -> unit;
        (** A dynamic service is carried out by calling this function with the environment
	 * and the CGI activation. The function can use all CGI features, including
	 * setting the [Location] handler to redirect responses.
	 *)
      dyn_activation : extended_environment -> 'a;
        (** The way the [Netcgi_types.cgi_activation] is created. Look below
	 * for [std_activation].
	 *)
      dyn_uri : string option;
        (** The URI prefix corresponding to this service. This is only used to
	 * compute [cgi_path]. Leave it to [None] if not needed.
	 *)
      dyn_translator : string -> string;
        (** The function computing [cgi_path_translated] from [cgi_path]. Set it
	 * to [(fun _ -> "")] if not needed.
	 *)
      dyn_accept_all_conditionals : bool;
        (** Whether to pass requests with [If-Match] and [If-Unmodified-Since] headers
          * to this service. If set to [true], the service can optimize the caching
          * behaviour by interpreting these fields. It is even obliged to interpret
	 * them. If [false], requests containing these headers are rejected.
	 *
	 * The other condition fields [If-None-Match], [If-Modified-Since], and
	 * [If-Ranges] are not affected by this option. One can safely ignore these
	 * headers.
          *)
    } constraint 'a = # Netcgi.cgi_activation ;;

val std_activation : std_activation -> extended_environment -> Netcgi.cgi_activation
  (** Create the function for [dyn_activation] from a [std_activation] tag.
    * Example:
    * [ let dyn_actv = std_activation `Std_activation_unbuffered ]
   *)

val dynamic_service : 
       'a dynamic_service -> [> `Dynamic_service of 'a dynamic_service ] http_service
  (** Configures the dynamic service. *)

type ac_by_host_rule =
    [ `Allow of string list
    | `Deny of string list
    ]
  (** Access control by host: 
    * - [`Allow]: Only the listed hosts are allowed; all other are denied
    * - [`Deny]: The listed hosts are denied; all other are allowed
   *)

type 'a ac_by_host = ac_by_host_rule * 'a http_service
  (** The service is protected by the access control rule *)

val ac_by_host : 'a ac_by_host -> [> `Ac_by_host of 'a ac_by_host ] http_service
  (** Configures host-based access control *)

val read_media_types_file : string -> (string * string) list
  (** Reads a text file with two columns where the left column is the
    * media type and the right column the corresponding suffix.
    * Returns the contents as pairs [ (suffix, type) ].
   *)


module Debug : sig
  val enable : bool ref
end
