(* netcgi_common.ml

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


(* module Url = Netencoding.Url *)

open Netchannels
open Printf

(* TODO: Get rid of all the unsafe_get and unsafe_set, or prove that
   their usage is correct. Until then, we enforce using the safe variants
   (for strings). There are also calls to Array.unsafe_(get/set) and
   Char.unsafe_chr
 *)

let unsafe_get s k = s.[k]
let unsafe_set s k c = s.[k] <- c


(* Exceptions and signals
 ***********************************************************************)

exception HTTP of Nethttp.http_status * string

let () =
  Netexn.register_printer
    (HTTP(`Continue, ""))
    (fun e ->
       match e with
	 | HTTP(status, text) ->
	     "Netcgi_common.HTTP(" ^ 
	       Nethttp.string_of_http_status status ^ ", " ^ 
	       "\"" ^ String.escaped text ^ "\")"
	 | _ -> assert false
    )


(* If the socket connection is closed on the client end, the SIGPIPE
   signal will be triggered, aborting the program.  We want to see the
   unix error [EPIPE], so disable the signal (if it exists for the OS)
   The Netsys_signal framework disables this signal by default, so we
   simply initialize the framework.
.  *)
let () =
  Netsys_signal.init()


(* Useful functions
 ***********************************************************************)

(* Specialize [min] to integers for performance reasons (> 150% faster). *)
let min x y = if (x:int) <= y then x else y

let is_prefix =
  let rec is_pre i len pre s =
    if i < len then
      (unsafe_get pre i = unsafe_get s i)
      && is_pre (i+1) len pre s
    else true in
  fun prefix s ->
    (String.length prefix <= String.length s)
    && is_pre 0 (String.length prefix) prefix s

let rev_split =
  Nethttp.rev_split

(* [rm_htspace s] returns the substring [s.[low .. up - 1]] stripped
   of heading and trailing spaces. *)
let rm_htspace is_space =
  let rec trailing_spaces s j = (* assume there is i s.t. s.[i] <> ' ' *)
    if is_space(unsafe_get s j) then trailing_spaces s (j - 1)
    else j + 1 (* first trailing space *) in
  let rec rm_spaces s i up =
    if i >= up then "" else begin
      if is_space(unsafe_get s i) then rm_spaces s (i + 1) up
      else
        (* s.[i] <> space so trailing_spaces will stop and return j >= i. *)
        String.sub s i (trailing_spaces s (up - 1) - i)
    end in
  fun s low up ->
    if low < 0 || up > String.length s
    then invalid_arg "Netcgi_common.rm_htspace"
    else rm_spaces s low up


(* Quote all problematic characters to put the filename in a header
   and to save it on the disk and surround it by '"'. *)
let is_printable c =
  (* TODO: We must not assume any charset here. The world has got larger *)
  (* Latin-1 *)
  (Char.code c >= 32 && Char.code c <= 127) || Char.code c >= 160

let filename_quote s =
  (* Compute the length of the new string. *)
  let n = ref 2 (* the quotes *) in
  for i = 0 to String.length s - 1 do
    n := !n + (match unsafe_get s i with
	       | '"' | '\\' | '\n' | '\t' -> 2
               | c -> if is_printable c then 1 else 4)
  done;
  let s' = String.create !n in
  unsafe_set s' 0 '\"';
  n := 1;
  for i = 0 to String.length s - 1 do
    (match unsafe_get s i with
     | ('"' | '\\') as c ->
         unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
     | '\n' ->
         unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
     | '\t' ->
         unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
     | c ->
         if is_printable c then unsafe_set s' !n c
         else (
           let a = Char.code c in
           unsafe_set s' !n '\\';
           incr n; unsafe_set s' !n (Char.chr (48 + a / 100));
           incr n; unsafe_set s' !n (Char.chr (48 + (a / 10) mod 10));
           incr n; unsafe_set s' !n (Char.chr (48 + a mod 10))
	 )
    );
    incr n
  done;
  unsafe_set s' !n '\"';
  s'

type http_method =
    [`GET | `HEAD | `POST | `DELETE | `PUT]

let string_of_http_method =
  function
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `PUT -> "PUT"
    | `POST -> "POST"
    | `DELETE -> "DELETE"




(* Knuth-Morris-Pratt algorithm
 ***********************************************************************)

(* TODO: Get rid of this. It is only used to search in short strings anyway.
   There is an alternate implementation in Netaux.
 *)

(** Substring search functions using the Knuth-Morris-Pratt algorithm. *)
module type KMP_TYPE =
sig
  val search : string -> (string -> int -> int -> int)
    (** [search pat] define a search function [f] such that [f s i0
	i1] search the string [pat] in [s.[i0 .. i1-1]] and return the
	position of the first match.

	@raise Not_found if [pat] is not found.
	@raise Invalid_argument if [i0 < 0] or [i1 > String.length s].  *)

  val search_case_fold : string -> (string -> int -> int -> int)
    (** [search_case_fold] is the same as [search] except that the
	search is case insensitive.  *)
end

module KMP : KMP_TYPE =
struct
  (* Preprocess the pattern *)
  let preprocess pat len =
    let b = Array.make (len + 1) (-1) in
    (* [b.(i)] = width of the widest border of [pat.[0 .. i-1]]. *)
    let j = ref(-1) in
    for i = 0 to len - 1 do
      while !j >= 0
	&& unsafe_get pat !j <> unsafe_get pat i do
	  j := Array.unsafe_get b !j
      done;
      incr j;
      Array.(*unsafe_*)set b (i+1) !j
    done;
    b


  (* [search pat s i0 i1] search the string [pat] in [s.[i0 .. i1-1]]
     and return the position of the first match.
     @raise Not_found if [pat] is not found. *)
  (* We favored the following imperative code because it is the fastest. *)
  exception Found of int

  let search pat =
    let m = String.length pat in
    let b = preprocess pat m in
    fun s i0 i1 ->
      if i0 < 0 || i1 > String.length s then
	invalid_arg "Netcgi_common.KMP.search";
      let i = ref i0
      and j = ref 0 in
      try
	while !i < i1 do
	  while !j >= 0
	    && unsafe_get s !i <> unsafe_get pat !j do
	      j := Array.(*unsafe_*)get b !j
	  done;
	  incr i;
	  incr j;
	  if !j = m then raise(Found(!i - !j))
	done;
	raise Not_found
      with Found i -> i


  (* [search_case_fold pat s i0 i1] does the same as [search pat s i0
     i1] but in a case insensitive manner. *)
  let search_case_fold pat =
    let m = String.length pat in
    let pat = String.lowercase pat in
    let b = preprocess pat m in
    fun s i0 i1 ->
      if i0 < 0 || i1 > String.length s then
	invalid_arg "Netcgi_common.KMP.search";
      let i = ref i0
      and j = ref 0 in
      try
	while !i < i1 do
	  while !j >= 0 && Char.lowercase(unsafe_get s !i)
	    <> unsafe_get pat !j do
	      j := Array.(*unsafe_*)get b !j
	  done;
	  incr i;
	  incr j;
	  if !j = m then raise(Found(!i - !j))
	done;
	raise Not_found
      with Found i -> i

end


(* Arguments
 ***********************************************************************)

type representation = [ `Simple of Netmime.mime_body
                      | `MIME of Netmime.mime_message ]

type store = [`Memory | `File of string]

class type cgi_argument =
object
  method name : string
  method value : string
  method open_value_rd : unit -> Netchannels.in_obj_channel
  method store : store
  method content_type : unit -> string * (string * Mimestring.s_param) list
    (* [content_type] changed from the previous version to be uniform
       with Netmime. *)
  method charset : string
  method filename : string option
  method representation : representation
  method finalize : unit -> unit
end

class type rw_cgi_argument =
object
  inherit cgi_argument
  method ro : bool
  method set_value : string -> unit
  method open_value_wr : unit -> Netchannels.out_obj_channel
end

(* Breaking with the old Netcgi module, the default for [ro] is
   [true].  This is because the standard arguments are read-only. *)
class simple_arg ?(ro=true) (name0:string) value =
object(self)
  inherit Netmime.memory_mime_body ~ro value

  val name = name0

  method name = name
  method content_type () = ("text/plain",
			    ([] : (string * Mimestring.s_param) list))
  method charset = ""
  method filename = (None: string option)
  method representation = (`Simple(self :> Netmime.mime_body): representation)
end

exception Oversized

class oversized_arg name =
object
  inherit simple_arg name ""
  method value = raise Oversized
  method open_value_rd () = raise Oversized
end


class mime_arg ?(work_around_backslash_bug=true) ?(name: string option)
  ((hdr0, `Body body0) : Netmime.mime_message) =
  let options =
    if work_around_backslash_bug then [ Mimestring.No_backslash_escaping ]
    else [] in
  let name0 = match name with
    | Some n -> n
    | None ->
	(* Try to extract it from the MIME header; it is the "name"
	   parameter of the "Content-Disposition" field.  *)
	try
	  let s = hdr0#field "content-disposition" in
	  let _, params = Mimestring.scan_value_with_parameters_ep s options in
	  Mimestring.param_value(List.assoc "name" params)
	with Not_found -> "" in
object(self)
  val hdr = hdr0
  val body = body0
  val name = name0

  method name = name
  method value = body#value
  method open_value_rd = body#open_value_rd
  method store = body#store

  method content_type () =
    try hdr#content_type() with Not_found -> ("text/plain", [])

  method charset =
    let _, params = self#content_type() in
    try Mimestring.param_value(List.assoc "charset" params)
    with Not_found -> ""

  method filename =
    try
      let s = hdr#field "content-disposition" in
      let _, params = Mimestring.scan_value_with_parameters_ep s options in
      Some(Mimestring.param_value(List.assoc "filename" params))
    with Not_found ->
      None

  method representation = (`MIME(hdr, `Body body) : representation)
  method finalize = body#finalize
  method ro = body#ro
  method set_value = body#set_value
  method open_value_wr = body#open_value_wr
end


(* Cookies
 ***********************************************************************)

(* The cookie implementation has been moved to nethttp.ml *)

module Cookie = struct
  include Nethttp.Cookie

  let set = Nethttp.Header.set_set_cookie_ct

  let get header =
    Nethttp.Header.get_cookie_ct
      (header : #Nethttp.http_header :> Nethttp.http_header_ro)

  let of_record =
    of_netscape_cookie

  let to_record =
    to_netscape_cookie
end

(* Environment
 ***********************************************************************)

type config = {
  tmp_directory : string;
  tmp_prefix : string;
  permitted_http_methods : [`GET | `HEAD | `POST | `DELETE | `PUT] list;
  permitted_input_content_types : string list;
  input_content_length_limit : int;
  max_arguments : int;
  workarounds :
    [ `MSIE_Content_type_bug | `Backslash_bug
    | `Work_around_MSIE_Content_type_bug
    | `Work_around_backslash_bug              ] list;
  default_exn_handler : bool;
}


(* Microsoft Internet Explorer: When used with SSL connections, this
   browser sometimes produces CONTENT_TYPEs like "multipart/form-data;
   boundary=..., multipart/form-data; boundary=...".

   Workaround: Throw away everything after ", ".  We must take care
   that, however unlikely, ',' is allowed in the boundary provided it
   is a quoted string (RFC 2045).
*)
let fix_MSIE_Content_type_bug =
  let bd = KMP.search "boundary" in
  let lenbd = String.length "boundary" in
  fun ct ->
    try
      let len = String.length ct in
      let i = lenbd + bd ct 0 len (* char after "boundary" *) in
      let another_bd = (try ignore(bd ct i len); true
			with Not_found -> false) in
      if another_bd then (
	(* Assume the boundary is not quoted *)
	try
	  let j = String.index_from ct i ',' in
	  String.sub ct 0 j (* cut as ',' *)
	with _ -> ct
      )
      else ct (* fine as is *)
    with Not_found -> ct (* No "boundary" parameter, maybe just a
                            application/x-www-form-urlencoded POST.
                            Leave unchanged. *)


let is_MSIE =
  let search = KMP.search "MSIE" in
  fun u ->
    try ignore(search u 0 (String.length u)); true
    with Not_found -> false



(* This is a good place to deal with the MSIE bug as we have all args
   thus can check the browser -- plus it must be done for all
   connectors.  *)
class cgi_environment ~config ~(properties:(string * string) list)
  ~input_header (out_obj:out_obj_channel)
  =
  (* Work around MSIE Content-Type bug *)
  let work_around =
    (List.mem `MSIE_Content_type_bug config.workarounds
     || List.mem `Work_around_MSIE_Content_type_bug config.workarounds)
    && (try is_MSIE(List.assoc "user-agent" input_header)
	with Not_found -> false) in
  let input_header =
    if work_around then
      List.map (fun ((n,v) as p) ->
		  if n = "content-type" then (n, fix_MSIE_Content_type_bug v)
		  else p
	       ) input_header
    else input_header in
  (* It seems some web servers set SERVER_NAME as name:port.  If so,
     split the value into the two properties.  *)
  let properties =
    try
      let server_name = List.assoc "SERVER_NAME" properties in
      let (host,port) =
	(* socksymbol_of_string can also handle IPv6 addresses *)
	match Netsockaddr.socksymbol_of_string server_name with
	  | `Inet(addr,port) ->
	      (Unix.string_of_inet_addr addr, port)
	  | `Inet_byname(host,port) ->
	      (host,port)
	  | `Unix _ ->
	      failwith "unsupported" in
      let prop_name = ("SERVER_NAME", host) in
      let prop_port = ("SERVER_PORT", string_of_int port) in
      let properties =
        List.filter 
	  (fun (n,_) -> n <> "SERVER_NAME")
	  properties in
      (try
         ignore(int_of_string(List.assoc "SERVER_PORT" properties));
         (* A port number is already given -- leave it *)
         prop_name :: properties
       with Not_found | Failure _ ->
         prop_name :: prop_port :: properties)
    with Not_found | Failure _ -> properties in
object(self)
  val config = config
  val properties_list = properties
  val properties = (Hashtbl.create 20 : (string,string) Hashtbl.t)
  val input_header = new Netmime.basic_mime_header ~ro:true input_header
  val output_header =
    (* The user is supposed to call [cgi#set_header] and that will set
       the Content-Type.  Thus one can set a default one in case he
       forgot -- we use the same default as [cgi#set_header] by the
       principle of leat surprise. *)
    new Netmime.basic_mime_header [("Content-Type", "text/html")]
  val out_channel = out_obj

  val mutable input_content_type = lazy(assert false)
  val mutable cookies = lazy(assert false)
  val mutable header_not_sent = true

  initializer
    (* Build the hashtable for faster access to CGI properties.
       Properties come in no specific order.  We use the
       [properties_list] given at creation instead of
       [self#properties] because this class may be inherited and the
       latter be only initialized then. *)
    List.iter (fun (n,v) -> Hashtbl.add properties n v) properties_list;
    (* To avoid reparsing again and again Content-Type, we do it here.
       However, some request do not have a Content-Type (e.g. GET) and
       this parsing can raise exceptions (which we do not want at the
       creation of this object), thus we do it only on demand. *)
    input_content_type <- lazy(
      Mimestring.scan_mime_type_ep (self#input_header_field "content-type") []
    );
    (* Cache the extracted cookies *)
    cookies <- lazy(Nethttp.Header.get_cookie_ct self#input_header)


  (* CGI properties *)
  method cgi_properties = properties_list

  method cgi_property ?default name =
    match default with
    | None -> Hashtbl.find properties name
    | Some d -> try Hashtbl.find properties name with Not_found -> d

  method cgi_gateway_interface =
    self#cgi_property ~default:"" "GATEWAY_INTERFACE"
  method cgi_server_name =
    self#cgi_property ~default:"" "SERVER_NAME"
  method cgi_server_port =
    try Some(int_of_string(self#cgi_property "SERVER_PORT"))
    with Not_found | Failure _ -> None
  method cgi_server_protocol = self#cgi_property ~default:"" "SERVER_PROTOCOL"
  method cgi_server_software = self#cgi_property ~default:"" "SERVER_SOFTWARE"
  method cgi_request_method = self#cgi_property ~default:"" "REQUEST_METHOD"

  method cgi_script_name = self#cgi_property ~default:"" "SCRIPT_NAME"
  method cgi_path_info = self#cgi_property ~default:"" "PATH_INFO"
  method cgi_path_translated = self#cgi_property ~default:"" "PATH_TRANSLATED"
  method cgi_auth_type = self#cgi_property ~default:"" "AUTH_TYPE"
  method cgi_remote_addr = self#cgi_property ~default:"" "REMOTE_ADDR"
  method cgi_remote_host = self#cgi_property ~default:"" "REMOTE_HOST"
  method cgi_remote_user = self#cgi_property ~default:"" "REMOTE_USER"
  method cgi_remote_ident = self#cgi_property ~default:"" "REMOTE_IDENT"
  method cgi_query_string = self#cgi_property ~default:"" "QUERY_STRING"

  method protocol : Nethttp.protocol =
    let proto = self#cgi_server_protocol in
    if is_prefix "HTTP/" proto then
      try
	let dot = String.index_from proto 5 '.' in
	let dot1 = dot + 1 in
	let len = String.length proto in
	let version = (int_of_string(String.sub proto 5 (dot - 5)),
		       int_of_string(String.sub proto dot1 (len - dot1))) in
	`Http(version, if self#cgi_https then [`Secure_https] else [])
      with _ ->
	`Other
    else `Other

  method cgi_https =
    match String.lowercase(self#cgi_property ~default:"" "HTTPS") with
    | "on"  -> true
    | "off" | "" -> false
    | p -> raise(HTTP(`Bad_request,
		      "Cannot interpret HTTPS property: " ^ p))


  (* Input *)

  method input_header = input_header

  method input_header_field ?default name =
    try self#input_header#field name
    with Not_found -> (match default with
		       | None -> raise Not_found
		       | Some d -> d)

  method multiple_input_header_field name =
    self#input_header#multiple_field name

  method input_header_fields =
    self#input_header#fields

  method cookies = Lazy.force cookies (* init => fun of self#input_header *)

  method cookie name =
    List.find (fun c -> Nethttp.Cookie.name c = name) self#cookies

  method user_agent =
    self#input_header_field ~default:"" "USER-AGENT"

  method input_content_length =
    try int_of_string(self#input_header_field "CONTENT-LENGTH")
    with Failure _ ->
      raise(HTTP(`Bad_request, "Content-Length is not an integer!"))

  method input_content_type_string =
    self#input_header_field ~default:"" "CONTENT-TYPE"

  method input_content_type () = Lazy.force input_content_type


  (* Output *)

  method output_header = output_header

  method output_header_field ?default name =
    try self#output_header#field name
    with Not_found -> (match default with
		       | None -> raise Not_found
		       | Some d -> d)

  method multiple_output_header_field name =
    self#output_header#multiple_field name

  method output_header_fields =
    self#output_header#fields

  method set_output_header_field name value =
    self#output_header#update_field name value

  method set_multiple_output_header_field name values =
    self#output_header#update_multiple_field name values

  method set_output_header_fields fields =
    self#output_header#set_fields fields

  method set_status st =
    let status = string_of_int(Nethttp.int_of_http_status st) in
    self#output_header#update_field "Status" status

  method send_output_header () =
    (* In transactional mode, some body may have been outputted before
       we (re)set the header, thus the header is sent directly to the
       "raw" channel.  Moreover, to set the Content-Length header, one
       would like only the DATA to be sent to the transactional
       [cgi#out_channel].

       Since it is not unreasonable that the user calls
       [send_output_header] before [commit_work], we make sure the
       header is sent only once.  *)
    if header_not_sent then (
      (* Note: ~soft_eol:"" because linear whitespace is illegal in CGI
	 responses.  *)
      Mimestring.write_header ~soft_eol:"" ~eol:"\r\n"
	out_obj self#output_header#fields;
      header_not_sent <- false (* One output header per request *)
    )

  method output_ch = out_channel
  method out_channel = out_channel


  method private format_error msg =
    let zone = Netdate.localzone (* log local time *) in
    "[" ^ Netdate.format "%c" (Netdate.create ~zone (Unix.gettimeofday()))
    ^ "] [OcamlNet] " ^ msg

  method log_error msg = prerr_endline (self#format_error msg)
    (* Rough default but works most of the time. *)

  method config = config
end


(* Creating transactional channels and CGI objects
 ***********************************************************************)

(* Direct & transactional channels. *)

type output_type =
  [ `Direct of string
  | `Transactional of config -> out_obj_channel -> trans_out_obj_channel ]


(* Channel that discards everything written to it. *)
class discarding_channel (before: unit -> unit) : trans_out_obj_channel =
object
  inherit Netchannels.output_null() as super
  val before_commit = before
  val mutable pos_commit = 0  (* position of last "commit" *)
  val mutable pos_delta = 0   (* how much data has been "rollbacked"
				 -- to substract from position *)

  method pos_out = super#pos_out - pos_delta
  method commit_work() =
    before_commit();
    pos_commit <- super#pos_out
  method rollback_work() =
    (* "rollback" can be emulated by increasing the pos_delta value,
       such that pos_out will return the position of the last commit
       operation *)
    pos_delta <- pos_delta + (super#pos_out - pos_commit)
end

(* Has the interface of a transactional channel but in fact, it is not. *)
class no_trans_channel sep (before: unit -> unit) (ch:out_obj_channel)
  : trans_out_obj_channel =
object
  val sep = sep
  val before_commit = before
  val ch = ch
  method output =         ch#output
  method flush =          ch#flush
  method close_out =      ch#close_out
  method pos_out =        ch#pos_out
  method really_output =  ch#really_output
  method output_char =    ch#output_char
  method output_string =  ch#output_string
  method output_byte =    ch#output_byte
  method output_buffer =  ch#output_buffer
  method output_channel = ch#output_channel

  method commit_work ()  = before_commit(); ch#flush()
  method rollback_work() = ch#output_string sep
end

class on_commit_channel (before: unit -> unit) (ch: trans_out_obj_channel)
  : trans_out_obj_channel =
object
  val before_commit = before
  val ch = ch
  method output =         ch#output
  method flush =          ch#flush
  method close_out =      ch#close_out
  method pos_out =        ch#pos_out
  method really_output =  ch#really_output
  method output_char =    ch#output_char
  method output_string =  ch#output_string
  method output_byte =    ch#output_byte
  method output_buffer =  ch#output_buffer
  method output_channel = ch#output_channel

  method commit_work () = before_commit(); ch#commit_work()
  method rollback_work =  ch#rollback_work
end




type other_url_spec = [ `Env | `This of string | `None ]
type query_string_spec = [ `Env | `This of cgi_argument list | `None
			 | `Args of rw_cgi_argument list ]
type cache_control = [ `No_cache | `Max_age of int | `Unspecified ]
type request_method = [`GET | `HEAD | `POST | `DELETE | `PUT of cgi_argument]

let string_of_request_method = function
  | `GET	 -> "GET"
  | `HEAD	 -> "HEAD"
  | `POST	 -> "POST"
  | `DELETE	 -> "DELETE"
  | `PUT _	 -> "PUT"

let last_char s =
  if s = "" then failwith "last_char";
  s.[ String.length s - 1 ]


exception Too_many_arguments


(* CGI abstractions independent of the connector. *)
class cgi (env:cgi_environment) (op:output_type)
  (request_method:request_method) (args:cgi_argument list)
  =
  (* Called just before the out_channel is committed *)
  let before_commit () =
    (* We can call [env#send_output_header] without harm even in
       `Direct mode since it checks the header is sent at most one
       time.  If [#set_header] was not called before, the default
       environment header will be sent. *)
    env#send_output_header()
  in
object(self)
  val args = args (* list of arguments *)
  val arguments = (Hashtbl.create 15 : (string, cgi_argument) Hashtbl.t)
  val out_channel =
    (* Initialize the output channel from the arguments: *)
    if request_method = `HEAD then
      (* For HEAD requests, no body has to be sent. *)
      new discarding_channel before_commit
    else (
      match op with
      | `Direct sep ->
          new no_trans_channel sep before_commit env#out_channel
      | `Transactional f ->
          new on_commit_channel before_commit (f env#config env#out_channel)
    )
  val request_method = request_method
  val env = env
  val mutable do_at_exit = []

  initializer
    (* Add the arguments in the reverse order for [find_all] to give
       expected results.  Allow a redefinition of [#arguments].

       Security note: It is essential for preventing DoS attacks that
       we use here Hashtbl.add and not Hashtbl.replace. Otherwise a
       malicious user could craft arguments specially so that the
       argument names cause lots of collisions in the hash table,
       and the performance of Hashtbl.replace would decrease dramatically
       (to O(n) per operation, leading to O(n^2) for the whole iteration).
     *)
    if List.length args > env#config.max_arguments then
      raise Too_many_arguments;
    List.iter (fun a -> Hashtbl.add arguments a#name a)
      (List.rev self#arguments)


  method argument name = Hashtbl.find arguments name
  method argument_value ?(default="") name =
    try (Hashtbl.find arguments name)#value
    with Not_found -> default
  method argument_exists name = Hashtbl.mem arguments name
  method multiple_argument name = Hashtbl.find_all arguments name
  method arguments = args

  method environment = env
  method request_method = request_method

  method finalize () =
    List.iter (fun a -> a#finalize()) self#arguments;
    (match self#request_method with
     | `PUT a -> a#finalize()
     | _ -> ());
    List.iter (fun f -> f()) do_at_exit


  method url ?(protocol=env#protocol)
    ?(with_authority=(`Env:other_url_spec))
    ?(with_script_name=(`Env:other_url_spec))
    ?(with_path_info=(`Env:other_url_spec))
    ?(with_query_string=(`None:query_string_spec)) () =
    let serv = match with_authority with
      | `This s -> s
      | `None -> ""
      | `Env ->
	  let p_scheme, p_port =
	    match protocol with
	    | `Http(_, atts) ->
		if List.mem `Secure_https atts then "https", 443
		else "http", 80
	    | `Other ->
		raise(HTTP(`Not_implemented,
			   "Netcgi.cgi#url: Cannot cope with the protocol"))
	  in
	  p_scheme ^ "://" ^ env#cgi_server_name
	  ^ (match env#cgi_server_port with
	     | None -> ""
	     | Some port ->
		 if port = p_port then "" else ":" ^ string_of_int port)

    and script = match with_script_name with
      | `Env -> env#cgi_script_name
      | `This s -> s
      | `None -> "" in
    let before_path = serv ^ script in
    let path = match with_path_info with
      | `Env -> env#cgi_path_info
      | `This s ->
	  (* the path-info MUST be separated by "/" *)
	  if (s <> "" && s.[0] <> '/') && 
	     (before_path = "" || last_char before_path <> '/' )
	  then "/" ^ s
	  else s
      | `None -> ""
    and args = match with_query_string with
      | `None -> []
      | `Env -> self#arguments
      | `This args -> args
      | `Args args -> (args :> cgi_argument list) (* deprecated *) in
    let args =   (* consider only `Memory args *)
      List.filter
	(fun a -> match a#store with `Memory -> true | _ -> false) args in
    let url = before_path ^ path in
    if args= [] then url else
      url ^ "?" ^
	(String.concat "&"
	   (List.map (fun a ->
			Netencoding.Url.encode(a#name) ^ "=" ^ 
			  Netencoding.Url.encode(a#value)) args
	   ))



  method set_header
    ?status ?(content_type="text/html") ?content_length
    ?(set_cookie=[]) ?(set_cookies=[])
    ?(cache=(`Unspecified:cache_control)) ?(filename="") ?(language="")
    ?(script_type="") ?(style_type="") ?(fields=[])
    () =
    env#set_output_header_fields []; (* reset *)
    env#set_output_header_field "Content-Type" content_type;
    (match content_length with
     | None -> ()
     | Some size ->
	 env#set_output_header_field "Content-Length" (string_of_int size));
    (match status with
     | None -> ()
     | Some s -> env#set_status s);
    (match cache with
     | `Unspecified -> ()
     | `No_cache ->
	 env#set_output_header_field "Cache-control" "no-cache";
	 env#set_output_header_field "Pragma" "no-cache";
	 let past = Netdate.mk_mail_date(Unix.time() -. 1.0) in
	 env#set_output_header_field "Expires" past;
     | `Max_age n ->
	 env#set_multiple_output_header_field "Cache-control"
	   [ "max-age=" ^ string_of_int n; "must-revalidate" ];
	 let secs = Netdate.mk_mail_date(Unix.time() +. float n) in
	 env#set_output_header_field "Expires" secs;
    );
    if filename <> "" then begin
      env#set_output_header_field "Content-disposition"
	("attachment; filename=" ^ filename_quote filename);
    end;
    if language <> "" then
      env#set_output_header_field "Content-language" language;
    if script_type <> "" then
      env#set_output_header_field "Content-script-type" script_type;
    if style_type <> "" then
      env#set_output_header_field "Content-style-type" style_type;
    (* Convert the deprecated [set_cookie] to the new format. *)
    let cookies =
      List.fold_left (fun l c -> Nethttp.Cookie.of_netscape_cookie c :: l)
        set_cookies set_cookie in
    Nethttp.Header.set_set_cookie_ct env#output_header cookies;
    List.iter (fun (n,v) -> env#set_multiple_output_header_field n v) fields;
    match op with
    | `Direct _ -> env#send_output_header() (* before any other output! *)
    | _ -> () (* For transactional output channels, it must be
		 possible to call this function several times.  Thus
		 no data must be actually written now. *)


  method set_redirection_header ?(set_cookies=[]) ?(fields=[]) loc =
    env#set_output_header_fields [];
    Nethttp.Header.set_set_cookie_ct env#output_header set_cookies;
    List.iter (fun (n,v) -> env#set_multiple_output_header_field n v) fields;
    env#set_output_header_field "Location" loc;
    env#set_status `Found; (* be precise -- necessary for some connectors *)
    match op with
    | `Direct _ -> env#send_output_header()
    | _ -> ()


  method output = out_channel
  method out_channel = out_channel

  method at_exit f = do_at_exit <- f :: do_at_exit
end


(* Reading arguments
 ***********************************************************************)

(* We believe it is better to deal here with the size of arguments --
   instead of leaving it to the user -- because 1. oversized argument
   will not live longer than necessary allowing space for the
   following ones; 2. it is a reminder to the user to think about this
   issue; 3. it is likely to show up in many apps, so having it here
   factors the code and is convenient. *)

type arg_store_type =
  [`Memory | `File | `Automatic | `Discard
  | `Memory_max of float | `File_max of float | `Automatic_max of float]

type arg_store = cgi_environment -> string -> Netmime.mime_header_ro ->
                    arg_store_type


(* [temp_file env] returns a function [unit -> string] to create
   temporary files according to the preferences in [config]. *)
let temp_file_fun config =
  let tmp_directory = config.tmp_directory in
  let tmp_prefix = config.tmp_prefix in
  fun () ->
    let (name, in_chan, out_chan) =
      Netchannels.make_temporary_file ~tmp_directory ~tmp_prefix () in
    close_in in_chan;
    close_out out_chan;
    name


(* Remove all [None] from the list and "flatten" the [Some], keeping
   the order of arguments.  We could avoid this if
   [Mimestring.read_multipart_body] had a "fold" form. *)
let rec remove_discarded_args = function
  | [] -> []
  | None :: tl -> remove_discarded_args tl
  | (Some a) :: tl -> a :: remove_discarded_args tl



let mime_header_string_arg =
  (new Netmime.basic_mime_header ~ro:true ["content-type", "text/plain"]
   :> Netmime.mime_header_ro)

(* Given a query string like [qs], return [None] is the argument is
   oversized or [Some arg].  *)
let args_of_string env arg_store qs =
  let name_val = Netencoding.Url.dest_url_encoded_parameters qs in
  let mk_arg (name, value) =
    let store = (try arg_store env name mime_header_string_arg
                 with _ -> `Discard) in
    if store = `Discard then None
    else begin
      let max_bytes = match store with
        | `Discard -> assert(false)
        | `Memory | `File | `Automatic -> infinity
        | `Memory_max size | `File_max size | `Automatic_max size -> size in
      if float(String.length value) > max_bytes then
        Some(new oversized_arg name :> cgi_argument)
      else
        Some(new simple_arg name value :> cgi_argument)
    end in
  remove_discarded_args (List.map mk_arg name_val)
;;

(* Given the [name] and header [hdr] of an argument, return [None] if
   the argument is discarded or [Some arg] otherwise where [arg]
   body is read from [stream] (the header, if any, is supposed to
   have been read already).  *)
let arg_body_of_stream  env (arg_store:arg_store) name hdr ~has_filename
    ~work_around_backslash_bug temp_file (stream:Netstream.in_obj_stream) =
  let store = (try arg_store env name (hdr :> Netmime.mime_header_ro)
               with _ -> `Discard) in
  if store = `Discard then None
  else begin
    let store, max_bytes = match store with
      | `Discard -> assert(false)
      | `Memory          -> `Memory, infinity
      | `Memory_max size -> `Memory, size
      | `File          -> `File(temp_file()), infinity
      | `File_max size -> `File(temp_file()), size
      | `Automatic ->
	  (if has_filename then `File(temp_file()) else `Memory), infinity
      | `Automatic_max size ->
	  (if has_filename then `File(temp_file()) else `Memory), size  in
    let body, body_ch = Netmime.storage ~ro:true ~fin:true store in
    try
      Netchannels.with_out_obj_channel
        (Netmime.decode_mime_body hdr body_ch)
        (fun body_ch' ->
	   body_ch'#output_channel(stream :> Netchannels.in_obj_channel));
      (* check size *)
      (* FIXME: Unfortunately this check can be done after the whole
         argument has been saved.  Doing otherwise would require
         modifications to Netmime to allow an optional size argument *)
      let size = match store with
        | `Memory -> String.length(body#value)
        | `File temp -> (Unix.stat temp).Unix.st_size in
      if float size > max_bytes then (
        body#finalize(); (* delete file *)
        Some(new oversized_arg name :> cgi_argument)
      )
      else
        Some(new mime_arg ~work_around_backslash_bug ~name (hdr, `Body body)
             :> cgi_argument)
    with
    | Invalid_argument "String.create" (* string too large *)
    | Unix.Unix_error(Unix.ENOSPC,_,_)
    | Sys_error _ ->
        body#finalize();
        Some(new oversized_arg name :> cgi_argument)
    | e ->
        (* Other exceptions are also treated as "oversized" (so we
           keep a trace of the argument existence) but the exn is
           strange so log it.  *)
        env#log_error(sprintf "While reading the body of the CGI argument %S,\
        the exception %S was raised.  The argument has been treated as \
        oversized."
                        name (Netexn.to_string e));
        body#finalize();
        Some(new oversized_arg name :> cgi_argument)
  end



(* Returns [Some arg] created by reading the [stream], or [None] if
   [arg_store] decides that the argument must be discarded.  It is
   made to be called by {!Mimestring.read_multipart_body} on each
   part.  *)
let arg_of_stream env (arg_store:arg_store) temp_file
    ~work_around_backslash_bug (stream:Netstream.in_obj_stream) =
  let hdr = Netmime.read_mime_header ~ro:true stream in
  (* FIXME: Unfortunately raising the exceptions below can leave the
     files of the previous arguments on the disk.  The easier would be
     to have [Mimestring.fold_multipart_body] wich would give access
     to the previous args.  (Incidentally, it would also remove the
     need for [remove_discarded_args].) *)
  let disposition, disp_params =
    try hdr#content_disposition()
    with
    | Not_found ->
	raise(HTTP(`Bad_request,
		   "Content-Disposition field is missing from POST data"))
    | Failure _ ->
	raise(HTTP(`Bad_request,
		   "The browser sent the invalid Content-Disposition field: "
		   ^ hdr#field "Content-Disposition")) in
  if disposition <> "form-data" then
    raise(HTTP(`Bad_request, "Unknown Content-disposition " ^ disposition
		 ^ " in POST request body"));
  let name =
    try Mimestring.param_value(List.assoc "name" disp_params)
    with Not_found ->
      raise(HTTP(`Bad_request,
		 "\"name\" parameter mandatory in Content-Disposition field"))
  in
  let has_filename = List.mem_assoc "filename" disp_params in
  arg_body_of_stream env arg_store name hdr temp_file ~has_filename
    ~work_around_backslash_bug stream



(* Do not care about thread safety of [discard_buffer] -- contains garbage. *)
let discard_buffer = String.create 0x2000

(** [discard_bytes in_obj len] discard at most [len] bytes ([len]
    bytes or until the end of file is reached). *)
let discard_bytes (in_obj: Netchannels.in_obj_channel) len =
  let len = ref len in
  try
    while !len > 0 do
      let r = in_obj#input discard_buffer 0 (min !len 0x2000) in
      if r = 0 then len := 0 (* FIXME: Sys_blocked_io ?? *)
      else len := !len - r
    done
  with End_of_file -> ()


let cgi_with_args new_cgi (env:cgi_environment) (op:output_type)
    in_obj arg_store =
  (* Check that the request method is allowed and get the parameters *)
  let config = env#config in
  (* Get the request-method in the same form it is specified in
     {!config.permitted_http_methods}. *)
  let request_method =
    match env#cgi_request_method with
    | "GET"    -> `GET
    | "HEAD"   -> `HEAD
    | "POST"   -> `POST
    | "DELETE" -> `DELETE
    | "PUT"    -> `PUT
    | r ->
	if r = "" then
	  raise(HTTP(`Bad_request, "Empty request method!\n\
If you are running this program from a shell to test it, use Netcgi_test.run\n\
to execute your main function.  It will help you passing information and\n\
seeing the result."))
	else
	  raise(HTTP(`Not_implemented, "Unknown request method: " ^ r))
  in
  if not(List.mem request_method config.permitted_http_methods) then
    (* Behavior mandated by RFC 2068, section 5.1.1 *)
    raise(HTTP(`Method_not_allowed,
	       "The request method " ^ env#cgi_request_method
	       ^ " is not allowed"));
  (match request_method with
   | `GET | `HEAD | `DELETE as request_method ->
       (* Read the arguments from QUERY_STRING.  Normally DELETE does
	  not receive parameters, but it does not harm. *)
       let args = args_of_string env arg_store env#cgi_query_string in
       new_cgi env op (request_method: request_method) args

   | `POST | `PUT as request_method ->
       (* Check Content-Length *)
       let len =
	 try env#input_content_length
	 with
	 | Not_found -> raise(HTTP(`Length_required,
				   "Content-Length field is mandatory"))
	 | Failure _ -> raise(HTTP(`Bad_request,
				   "Erroneous Content-Length format")) in
       if len > config.input_content_length_limit then (
         (* Ok, the content is too big but we need to read it and
            discard it to get it out of the way.  Some connectors
            complain if we don't (e.g. SCGI). *)
         discard_bytes in_obj len;
	 raise(HTTP(`Request_entity_too_large,
		    "Content-Length=" ^ string_of_int len
		    ^ " bytes is bigger that the allowed limit of "
		    ^ string_of_int config.input_content_length_limit));
       );
       (* Check the Content-Type *)
       let content_type, _ =
	 try env#input_content_type()
	 with
	 | Not_found -> raise(HTTP(`Bad_request,
				   "Content-Type field is required"))
	 | Failure _ -> raise(HTTP(`Bad_request,
				   "Erroneous Content-Type format")) in
       let permitted = config.permitted_input_content_types in
       if permitted <> [] && not(List.mem content_type permitted) then (
         discard_bytes in_obj len;
	 raise(HTTP(`Unsupported_media_type,
		    "Content-Type \"" ^ content_type ^ "\" is not authorized"));
       );
       (* Get CGI arguments *)
       begin match request_method with
       | `POST ->
	   (* Read the arguments from the input channel. *)
	   begin match env#input_content_type() with
	   | "application/x-www-form-urlencoded", _ ->
	       (* As the data is supposed to be quite small (like
		  QUERY_STRING), we will read it entirely into a string.
		  We check the string size limit however. *)
	       if len > Sys.max_string_length then (
                 discard_bytes in_obj len;
		 raise(HTTP(`Request_entity_too_large,
			    "A maximum of "
			    ^ string_of_int Sys.max_string_length
			    ^ " bytes is allowed for urlencoded forms"));
	       );
	       let qs = String.create len in
	       ( try
 	           in_obj#really_input qs 0 len
                 with End_of_file ->
                   raise(HTTP(`Bad_request,
                              "Request body is shorter than announced in \
                               Content-Length"));
               );
	       let args = args_of_string env arg_store qs in
	       new_cgi env op `POST args

	   | "multipart/form-data", params ->
	       let boundary =
		 try Mimestring.param_value(List.assoc "boundary" params)
		 with Not_found ->
		   raise(HTTP(`Bad_request,
			      "Content-Type multipart/form-data needs to \
                                contain a \"boundary\" parameter")) in
	       (* How to create temporary files *)
	       let temp_file = temp_file_fun config in

	       let in_stream = new Netstream.input_stream ~len in_obj in
	       let work_around_backslash_bug =
		 List.mem `Backslash_bug config.workarounds
                 || List.mem `Work_around_backslash_bug config.workarounds in
	       (* Make a list of args by applying [mk_arg] to each part. *)
	       let mk_arg stream = (arg_of_stream env arg_store temp_file
                                      ~work_around_backslash_bug stream) in
	       let args = remove_discarded_args
		 (Mimestring.read_multipart_body mk_arg boundary in_stream)
	       in
	       new_cgi env op `POST args

	   | _ ->
               (* For example [content_type] may be
                  - application/vnd.fdf: Acrobat Reader
                  - multipart/related, application/xml: XForms 1.0
                  - application/x-www-form+xml, text/plain:
                    WHATWG Web Forms 2.0

                  Create a unique argument "BODY" containing all the
                  data available on the input channel (not processing). *)
	       let temp_file = temp_file_fun config in
	       let in_stream = new Netstream.input_stream ~len in_obj in
               let arg =
                 arg_body_of_stream env arg_store "BODY" env#input_header
                   ~has_filename:true (* thus `Automatic -> `File *)
                   ~work_around_backslash_bug:false temp_file in_stream in
               new_cgi env op `POST (remove_discarded_args [arg])
	   end

       | `PUT ->
	   (* Read the normal arguments from QUERY_STRING *)
	   let args = args_of_string env arg_store env#cgi_query_string in

	   (* How to create the possible temporary file (at most one) *)
	   let temp_file = temp_file_fun config in

	   (* Read _the_ PUT argument from stdin.  There is no header. *)
	   let in_stream = new Netstream.input_stream ~len in_obj in
	   let work_around_backslash_bug =
	     List.mem `Backslash_bug config.workarounds
             || List.mem `Work_around_backslash_bug config.workarounds in
	   (* PUT arg does not have name but we use "BODY" for
	      backward compatibility.  *)
	   let hdr = env#input_header in
	   let name = "BODY" in
           let put_arg =
             arg_body_of_stream env arg_store name hdr temp_file
               ~has_filename:true ~work_around_backslash_bug in_stream in
           match put_arg with
           | Some a -> new_cgi env op (`PUT(a)) args
           | None -> raise(HTTP(`Not_implemented,
                                "PUT request body rejected"))
       end
  )




(* Handling exceptions
 ***********************************************************************)

let error_page (env:cgi_environment) status fields msg html =
  (* We do not need to rollback the work because either (1) it has
     been commited (and it is too late), or (2) it has not and then is
     still in the buffer of [cgi#out_channel] and we shall use the raw
     channel. *)
  try
    (* Log the error *)
    let script_name = env#cgi_script_name in
    let code = Nethttp.int_of_http_status status in
    let info = Nethttp.string_of_http_status status in
    env#log_error (sprintf "%s: %s (Status %i)" script_name msg code);
    (* Header

       We have no idea whether a previous header has been sent, so we
       try our luck.  At worst, it will appear within the current
       output.  *)
    env#set_output_header_fields []; (* reset *)
    env#set_output_header_field "Content-type" "text/html";
    env#set_status status;
    (* Cache 1h *)
    let now = Unix.time() in
    env#set_multiple_output_header_field "Cache-control"
      [ "max-age=3600"; (* secs *)
        "must-revalidate" ];
    let secs = Netdate.mk_mail_date(now +. 3600.) in
    env#set_output_header_field "Expires" secs;
    (* Additional fields *)
    List.iter (fun (n,v) -> env#set_multiple_output_header_field n v) fields;
    env#send_output_header();
    (* Body -- unless it is a HEAD request.

       Description of the problem.
    *)
    if env#cgi_request_method <> "HEAD" then (
      let out = env#out_channel#output_string in
      let printf fmt = kprintf out fmt in
      out "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \
	\"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
      out "<html xmlns=\"http://www.w3.org/1999/xhtml\" \
xml:lang=\"en\" lang=\"en\">
<head>
<meta name=\"generator\" content=\"OCamlNet (http://ocamlnet.sf.net)\">
<style>
  p.msg { color: black; background-color: #cccccc; padding: 1ex; }
  h2 { font-size: large; }
</style>
</head>
<body>\n";
      printf "<h1>%i &mdash; %s</h1>\n" code info;
      out "<h2>Cause of the error:</h2>";
      out "<p class=\"msg\">";
      out msg;
      out "</p>\n";
      out "<h2>Additional information:</h2>";
      out "<p>";
      out html;
      out "</p>\n<p>";
      let date = Netdate.create ~zone:Netdate.localzone now in
      printf "Date: %s<br />\n"	(Netdate.format "%c" date);
      printf "Script: %s<br />\n" script_name;
      printf "Request method: <tt>%s</tt><br />\n" env#cgi_request_method;
      printf "Query string: <tt>%s</tt><br />\n" env#cgi_query_string;
      printf "Browser: <tt>%s</tt><br />\n" (String.escaped env#user_agent);
      out "</p>\n";
      (try
	 let referer = env#input_header_field "referer" in
	 printf "<a href=\"%s\">Go back</a>\n" referer (* FIXME: escape *)
       with Not_found -> ());
      out "</body>\n</html>\n";
      env#out_channel#flush();
      env#out_channel#close_out()
    )
  with _ -> ()
    (* We were supposed to report the error but if that itself raised
       an exception (e.g. the output channel was closed), there is not
       much we can do... *)


let handle_uncaught_exn (env:cgi_environment) = function
  | HTTP(`Continue, _)
  | HTTP(`Switching_protocols, _)
      (* 2xx: (successful) *)
  | HTTP(`Ok, _)
  | HTTP(`Created, _)
  | HTTP(`Accepted, _)
  | HTTP(`Non_authoritative, _)
  | HTTP(`No_content, _)
  | HTTP(`Reset_content, _)
  | HTTP(`Partial_content, _)
      (* 3xx: (redirection) *)
  | HTTP(`Multiple_choices, _)
  | HTTP(`Moved_permanently, _)
  | HTTP(`Found, _)
  | HTTP(`See_other, _)
  | HTTP(`Not_modified, _)
  | HTTP(`Use_proxy, _)
  | HTTP(`Temporary_redirect, _) ->
      (* It is s a bit funny to raise an exception not indicating a
	 failure (this library does not do it).  We interpret it as a
	 normal but early termination of the response *)
      env#out_channel#close_out();

  (* 4xx: (client error) *)
  | HTTP(`Bad_request, s) ->
      error_page env `Bad_request [] s
	"You <i>should not</i> repeat the request without modifications."
  | HTTP(`Unauthorized, s) ->
      let challenge = "" in
      error_page env `Unauthorized ["WWW-Authenticate", [challenge]] s ""
  | HTTP(`Payment_required, s) ->
      error_page env `Payment_required [] s ""
  | HTTP(`Forbidden, s) ->
      error_page env `Forbidden [] s
	"The request <i>should not</i> be repeated."
  | HTTP(`Not_found, s) ->
      error_page env `Not_found [] s
	"The server has not found anything matching the Request-URI."
  | HTTP(`Method_not_allowed, s) ->
      (* Allow header must be sent *)
      let meths =
	List.map
	  string_of_http_method 
	  env#config.permitted_http_methods in
      let allow = String.concat ", " meths in
      error_page env `Method_not_allowed ["Allow", [allow]] s
	("Only the following methods are allowed: " ^ allow)
  | HTTP(`Not_acceptable, s) ->
      error_page env `Not_acceptable [] s
	"The resource identified by the request is only capable of
	generating response entities which have content characteristics
	not acceptable according to the accept headers sent in the request."
  | HTTP(`Proxy_auth_required, s) ->
      error_page env `Proxy_auth_required [] s
	"Your browser <i>must</i> first authenticate itself with the proxy."
  | HTTP(`Request_timeout, s) ->
      error_page env `Request_timeout [] s
	"Your browser waited too long to send the data.  You can repeat
	the request without modifications at any later time."
  | HTTP(`Conflict, s) ->
      error_page env `Bad_request [] s
	"The request could not be completed due to a conflict with the
	current	state of the resource.  Please resolve the conflict and
	resubmit the request. "
  | HTTP(`Gone, s) ->
      error_page env `Bad_request [] s
	"The requested resource is no longer available at the server and no
	forwarding address is known."
  | HTTP(`Length_required, s) ->
      error_page env `Bad_request [] s
	"Content-Length is <i>required. You <i>may</i> repeat the request
	with a valid Content-Length."
  | HTTP(`Precondition_failed, s) ->
      error_page env `Bad_request [] s
	"The precondition given in one or more of the request-header fields
	evaluated to false when it was tested on the server. "
  | HTTP(`Request_entity_too_large, s) ->
      error_page env `Request_entity_too_large [] s
	"Please try again with smaller files!"
  | HTTP(`Request_uri_too_long, s) ->
      error_page env `Request_uri_too_long [] s
	"The server is refusing to service the request because the
	Request-URI is bigger than 4Mb."
  | HTTP(`Unsupported_media_type, s) ->
      let ct =
	String.concat ", " env#config.permitted_input_content_types in
      error_page env `Unsupported_media_type [] s
	("The following Content-Type are authorized: " ^ ct)
  | HTTP(`Requested_range_not_satisfiable, s) ->
      error_page env `Requested_range_not_satisfiable [] s ""
  | HTTP(`Expectation_failed, s) ->
      error_page env `Expectation_failed [] s ""

  (* 5xx: (server error) *)
  | HTTP(`Internal_server_error, s) ->
      error_page env `Internal_server_error [] s
	"Please write to the author of the application to tell him!"
  | HTTP(`Not_implemented, s) ->
      error_page env `Not_implemented [] s
	"The server does not support the functionality required to fulfill
	the request."
  | HTTP(`Bad_gateway, s) ->
      error_page env `Bad_gateway [] s
	"The server, while acting as a gateway or proxy, received an invalid
	response from the upstream server it accessed in attempting to
	fulfill the request."
  | HTTP(`Service_unavailable, s) ->
      error_page env `Service_unavailable [] s
	"The server is currently unable to handle the request due to a
	temporary overloading or maintenance of the server.
	Please try again later."
  | HTTP(`Gateway_timeout, s) ->
      error_page env `Gateway_timeout [] s
	"The server, while acting as a gateway or proxy, did not receive a
	timely response from the upstream server it accessed in attempting to
	complete the request."
  | HTTP(`Http_version_not_supported, s) ->
      error_page env `Http_version_not_supported [] s
	"The server does not support, or refuses to support, the HTTP
	protocol version that was used in the request message."

  (* Other exceptions *)
  | Exit ->
      (* FIXME: This is seen as an acceptable way to terminate early. *)
      env#out_channel#close_out()
  | exn ->
      error_page env `Internal_server_error []
	(Netexn.to_string exn)
	"This indicates an error in the application (not in the supporting
	library).  Please report it to the author or company that runs this
	software."



(* REMARK: With this choice of [exn_handler], the user may add some
   code before and after the execution of [run_cgi] but does not have
   to re-raise exceptions it does not care about (as is the case if
   [exn_handler] is passed all exceptions as arg). *)
let exn_handler_default (env:cgi_environment) ~exn_handler ~finally run_cgi =
  let special_exn = ref None in
  (try
     exn_handler env (fun () -> special_exn := run_cgi());
     finally()
   with
   | Exit ->
       (* [Exit] is considered as a proper way to terminate early *)
       finally()
   | exn when env#config.default_exn_handler ->
       (* exception not handled (or raised) by the user provided
          [exn_handler] *)
       handle_uncaught_exn env exn;
       finally()
  );
  match !special_exn with None -> () | Some e -> raise e



(* Accumulate input header and CGI properties
 ***********************************************************************)

(* The MSIE Content-Type bug is dealt with when the environment object
   is created.  The field names of [inheader] MUST be lowercased to
   input to the class [environment]. *)
let shift_to_lowercase = Char.code 'a' - Char.code 'A'

let update_props_inheader ((name, value) as nv) (props, inheader) =
  if name = "CONTENT_TYPE" then
    (props, ("content-type", value) :: inheader)
  else if name = "CONTENT_LENGTH" then
    (props, ("content-length", value) :: inheader)
  else if is_prefix "HTTP_" name then (
    (* Remove the "HTTP_" from the name, lowercase, and convert any
       '_' to '-'. *)
    let len = String.length name - 5 in
    let hname = String.sub name 5 len in
    for i = 0 to len - 1 do
      unsafe_set hname i
	(match unsafe_get hname i with
	 | '_' ->  '-'
	 | 'A' .. 'Z' as c -> (* ASCII *)
	     Char.unsafe_chr(Char.code c + shift_to_lowercase)
	 | c -> c)
    done;
    (props, (hname, value) :: inheader)
  )
  else
    (nv :: props, inheader)
