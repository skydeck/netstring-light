(* $Id: ftp_fs.mli 1661 2011-08-28 22:45:55Z gerd $ *)

(** FTP filesystem *)

class type ftp_stream_fs =
object
  inherit Netfs.stream_fs

  method ftp_client : Ftp_client.ftp_client
    (** The FTP client backing this filesystem *)

  method last_ftp_state : Ftp_client.ftp_state
    (** The last state of the last operation, or [Not_found] *)

  method translate : string -> string
    (** Translates a path into a URL *)

  method close : unit -> unit
    (** Forces that the FTP client is closed. (See [keep_open] option.)
	The client remains functional - the next operation will re-open
	the connection.
     *)
end


class ftp_fs : ?config_client:(Ftp_client.ftp_client -> unit) ->
               ?tmp_directory:string ->
               ?tmp_prefix:string ->
               ?get_password:(string -> string) ->
               ?get_account:(string -> string) ->
               ?keep_open:bool ->
               string -> ftp_stream_fs
  (** [ftp_fs base_url]: Access the FTP file system rooted at [base_url].

      The [base_url] must follow the [ftp://user\@host:port/path] scheme.
      Passwords in [base_url] are ignored.

      The following access methods are supported (compare with
      {!Netfs.stream_fs}). Note that not all FTP servers can perform
      all operations:
      - [path_encoding]: If the FTP server announces that it uses UTF-8
        as path encoding, this is returned here; else [None].
      - [path_exclusions]: is just [0,0; 47,47]
      - [nominal_dot_dot] is true. "." and ".." are resolved by the client
        before sending paths to the server.
      - [read]: is supported. Note that FTP distinguishes between text
        and binary mode, so don't forget to pass the [`Binary] flag if needed.
        The [`Skip] flag is only emulated - the full
        file is retrieved first, and only the first bytes are hidden
        from the user. There is no support for streaming mode; the
        [`Streaming] flag is ignored. This means that the file is first
        downloaded to a temporary file, and the returned object is just
        a handle for this file.
      - [write]: is supported, but - depending on the server - not all
        flag combinations. The flag list [[`Create; `Truncate]] should
        always work. For other flag lists, the server needs to support
        the [MLST] command for checking whether the file already exists.
        If this support is missing, an FTP error is raised. It is not
        supported to omit both [`Create] and [`Truncate]. Also, the
        [`Exclusive] flag is not supported. As for [read], there is no
        streaming mode, and a temporary file is used as container.
      - [size]: works only if the server supports the [SIZE] command
        (otherwise ENOSYS).
      - [test] and [test_list]: works only if the server supports the
        [MLST] command (otherwise ENOSYS). The tests [`N], [`E], [`D], [`F], 
        [`S], [`R], [`W], and [`X] should work.
        Files are never recognized as symlinks.
      - [remove]: works, but the [`Recursive] flag is not supported
        (and an emulation is considered as too dangerous)
      - [readdir]: works
      - [rename]: works
      - [mkdir]: works, but the flags are ignored
      - [rmdir]: works

      There is no support for [symlink], [readlink], and [copy].

      Options:
      - [config_client]: one can enable further features on the client
        object (e.g. proxies)
      - [tmp_directory]: directory for temporary files
      - [tmp_prefix]: file prefix for temporary files (w/o directory)
      - [get_password]: This function should return the password for 
        the user (passed in as argument). Defaults to "".
      - [get_account]: This function should return the account name for 
        the user (passed in as argument). Defaults to "".
      - [keep_open]: By default, a new FTP connection is started for
        each operation, and it is shut down afterward. By setting [keep_open]
        the connection is kept open. The user has to call [close] when done.
   *)


val ftp_fs : ?config_client:(Ftp_client.ftp_client -> unit) ->
             ?tmp_directory:string ->
             ?tmp_prefix:string ->
             ?get_password:(string -> string) ->
             ?get_account:(string -> string) ->
             ?keep_open:bool ->
             string -> ftp_stream_fs
  (** Same as function *)
