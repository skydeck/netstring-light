(* $Id: http_fs.mli 1661 2011-08-28 22:45:55Z gerd $ *)

(** HTTP filesystem *)

(** This module makes an HTTP file server accessible via a {!Netfs.stream_fs}
    object, supporting

    - reading files
    - identifying and reading directories (if the server generates
      index pages for directories, see below for more information)
    - writing files (if the server implements PUT)
    - removing files (if the server implements DELETE)

    One has to specify a base URL [b], and the access of a file with path [p]
    is translated to the URL [b ^ p]. Note that [p] cannot include web
    parameters ("?" strings), and hash marks are also not supported.

    A directory is recognized by appending a slash to the URL, and
    checking whether the URL exists.

    The contents of a directory are determined by loading the directory
    URL, analyzing the HTML page, and checking for the presence of
    hyperlinks that do not contain slashes (or at most a trailing slash). 
    Only hyperlinks of "A" elements are accepted.

    Almost every web server can be configured to generate directory
    listings that conform to this format. For example, Apache provides
    this through the module [mod_autoindex].

    There is an extension to this class for WebDAV:
    - {{:http://oss.wink.com/webdav/} Webdav} provides an extension of
      {!Http_fs} for the full WebDAV set of filesystem operations


 *)

(** {2 Examples} *)

(** Read an HTTP directory:

    {[
       let fs = new http_fs ~streaming:true "http://localhost/~gerd"
       let files = fs # readdir [] "/"
    ]}

    Download a file into a string:

    {[
       let ch = fs # read [] "/file"
       let s = Netchannels.string_of_in_obj_channels ch
    ]}

    Copy a file hierarchy to the local disk:

    {[
       let lfs = Netfs.local_fs()
       Netfs.copy_into (fs :> Netfs.stream_fs) "/tree" lfs "/tmp"
    ]}

    Use globbing:

    {[
       let fsys = Netglob.of_stream_fs (fs :> Netfs.stream_fs)
       let files = Netglob.glob ~fsys (`String "/*.gif")
    ]}
 *)

(** {2 Extended [stream_fs] type} *)

(** There is the new flag [`Header] for [read] and [write], and the
    new method [last_response_header] for getting the response
    header of the most recently executed operation.
 *)

type read_flag =
    [ Netfs.read_flag | `Header of (string*string)list ]

type read_file_flag =
    [ Netfs.read_file_flag | `Header of (string*string)list ]

type write_flag =
    [ Netfs.write_flag | `Header of (string*string)list ]

type write_file_flag =
    [ Netfs.write_file_flag | `Header of (string*string)list ]


class type http_stream_fs =
object
  method read : read_flag list -> string -> Netchannels.in_obj_channel
    (** Additional flag:
	- [`Header h]: Set these headers in the submitted GET request
     *)
    
  method read_file : read_file_flag list -> string -> Netfs.local_file
    (** Additional flag:
	- [`Header h]: Set these headers in the submitted GET request
     *)

  method write : write_flag list -> string -> Netchannels.out_obj_channel
    (** Additional flag:
	- [`Header h]: Set these headers in the submitted PUT request
     *)

  method write_file : write_file_flag list -> string -> Netfs.local_file -> unit
    (** Additional flag:
	- [`Header h]: Set these headers in the submitted PUT request
     *)

  method last_response_header : Nethttp.http_header
    (** Returns the header of the HTTP response of the last operation.
	Generally, the
	header is set when the last operation returns normally (no
	exception was raised). In case of [write], the header is first set when
	the stream is closed.

	Raises [Not_found] if the last operation did not receive a header
	(or not fully).
     *)

  method pipeline : Http_client.pipeline
    (** The HTTP pipeline backing this file system *)

  (** The following methods are the same as in {!Netfs.stream_fs}.
      (For formal reasons we cannot inherit from this class type.)
   *)

  method translate : string -> string
    (** Translates a path into a URL *)

  method path_encoding : Netconversion.encoding option
  method path_exclusions : (int * int) list
  method nominal_dot_dot : bool
  method size : Netfs.size_flag list -> string -> int64
  method test : Netfs.test_flag list -> string -> Netfs.test_type -> bool
  method test_list : Netfs.test_flag list -> string -> Netfs.test_type list -> bool list
  method remove : Netfs.remove_flag list -> string -> unit
  method rename : Netfs.rename_flag list -> string -> string -> unit
  method symlink : Netfs.symlink_flag list -> string -> string -> unit
  method readdir : Netfs.readdir_flag list -> string -> string list
  method readlink : Netfs.readlink_flag list -> string -> string
  method mkdir : Netfs.mkdir_flag list -> string -> unit
  method rmdir : Netfs.rmdir_flag list -> string -> unit
  method copy : Netfs.copy_flag list -> string -> string -> unit
  method cancel : unit -> unit
end


(** {2 The class} *)

class http_fs : ?config_pipeline:(Http_client.pipeline -> unit) ->
                ?streaming:bool ->
                ?tmp_directory:string ->
                ?tmp_prefix:string ->
                ?path_encoding:Netconversion.encoding ->
                ?enable_read_for_directories:bool ->
                ?enable_ftp:bool ->
                string -> http_stream_fs
  (** [http_fs base_url]: Accesses the HTTP file system rooted at
      [base_url].

      The following access methods are supported (compare with
      {!Netfs.stream_fs}):
      - [path_encoding]: Returns the passed [path_encoding]. Paths
        are always encoded.
      - [path_exclusions]: is just [0,0; 47,47]
      - [nominal_dot_dot] is true
      - [read]: is supported. All files are considered as binary.
        The [`Skip] flag works, and is translated to a [Range] header.
      - [write]: is supported and translated to [PUT]. It is assumed
        that [PUT] truncates existing files, and creates new files.
        Note that errors are often first reported when the returned
        channel is closed!
      - [size]: this works only if the server includes the [Content-length]
        header in responses to [HEAD] requests.
      - [test] and [test_list]: The tests [`N], [`E], [`D], [`F], and [`S]
        should work. Files are never symlinks. [`R] is handled like [`E],
        and [`X] is handled like [`X] (i.e. it is assumed that all 
        files are readable, and all directories can be entered). The
        [`W] test is never successful.
      - [remove]: is translated to a [DELETE] request.
      - [readdir]: works if index pages are generated (see above)
      
      There is no support for [rename], [symlink], [mkdir], [rmdir], and
      [copy].

      Options:
      - [config_pipeline]: one can enable further features on the pipeline
        object (e.g. authentication, proxies)
      - [streaming]: if true, the [read] method only reads as much data
        from the HTTP connection as requested by the user. This assumes
        that the user does not pause stream accesses for longer periods
        as this would risk a server timeout. Also, there is no way for
        the client to automatically reconnect to the HTTP server after crashes.
        If false (the default),
        files are first downloaded to a temporary file before they are
        made accessible as [in_obj_channel]. Streaming can also be
        enabled for each [read] or [write] by including [`Streaming]
        in the list of flags.
      - [tmp_directory]: directory for temporary files
      - [tmp_prefix]: file prefix for temporary files (w/o directory)
      - [path_encoding]: The encoding that is used for the file names.
        This must match the encoding the server assumes for translating
        file names to hyperlinks. Unfortunately, there is no way to
        query the server for this. The default, [`Enc_utf8], seems to be the
        de-facto standard on the web (e.g. browsers use UTF-8 when
        non-ASCII characters are entered in the address line).
      - [enable_ftp]: This enables anonymous FTP via web proxies. In
        this case the [base_url] is of the form [ftp://host:port/path].
        This works only if the pipeline is configured to contact a
        web proxy understanding FTP URLs.
   *)

(*
      - [is_error_response]: This function is invoked with the current 
        call object as soon as the response header arrives. It 
        looks at the response code, and checks whether the response
        is successful or not. In the latter case the function returns
        the exception to raise. This function can be called several
        times during the execution of an operation. If it is called
        at a moment where only the response header is available but
        not the response body it is ensured that it will be called
        again with the full response later.
        Defaults to {!Http_fs.is_error_response}.
   *)

val http_fs : ?config_pipeline:(Http_client.pipeline -> unit) ->
                ?streaming:bool ->
                ?tmp_directory:string ->
                ?tmp_prefix:string ->
                ?path_encoding:Netconversion.encoding ->
                ?enable_read_for_directories:bool ->
                ?enable_ftp:bool ->
                string -> http_stream_fs
  (** Same as normal function *)

(*
val is_error_response : string -> Http_client.http_call -> exn option
  (** Default implementation: The [status] [`Successful] (code in the range
      200 to 299) is considered as successful, and:
      - code 404 is mapped to [ENOENT]
      - codes 401 and 403 are mapped to [EACCES]
      - other codes from 300 to 599 are mapped to [EPERM]
   *)
 *)

(**/**)

val find_flag : ('a -> 'b option) -> 'a list -> 'b
