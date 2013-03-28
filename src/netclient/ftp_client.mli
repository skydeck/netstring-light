(* $Id: ftp_client.mli 1614 2011-06-09 15:08:56Z gerd $ *)

(** FTP client
  *
  * Currently implements:
  * - Core FTP (RFC 959), except compressed transfer modes, and except page
  *   files
  * - Negotiation of FTP extensions (RFC 2389)
  * - Common FTP extensions (RFC 3659)
  * - IPv6 (RFC 2428)
  * - Internationalization (RFC 2640)
  * - Directory walking (NVFS) and direct access (TVFS)
  * 
  * The client is written in asynchronous style (using {!Uq_engines}).
  *
  * The interface of this module was changed in Ocamlnet-3.3. Before this
  * release, the module was marked as "experimental". This is no longer
  * the case, as the interface has been updated, and the missing features
  * have been added (e.g. [STOR] support).
 *)

exception FTP_error of exn
  (** Something went wrong, often on socket level *)

exception FTP_protocol_violation of string
  (** The server violates the FTP specification *)

exception FTP_timeout of string
  (** A timeout on the control or data connection (this is a fatal error) *)


type cmd_state =
    [ `Not_connected
    | `Init
    | `Success
    | `Proto_error
    | `Temp_failure
    | `Perm_failure
    | `Rename_seq
    | `Restart_seq
    | `User_pass_seq
    | `User_acct_seq
    | `Pass_acct_seq
    | `Preliminary
    ]
  (** The command state:
    * - [`Not_connected]: Not connected.
    * - [`Init]: Just connected, no greeting message arrived yet
    * - [`Success]: Got the greeting message/last command was successful
    * - [`Proto_error]: {b currently unused}
    * - [`Temp_failure]: Last command was not successful, and the code
    *   was between 400 and 499
    * - [`Perm_failure]: Last command was not successful, and the code
    *   was between 500 and 599
    * - [`Rename_seq]: Used instead of [`Success] after the RNFR command
    * - [`Restart_seq]: Used instead of [`Success] after the REST command
    * - [`User_pass_seq]: Used instead of [`Success] after the USER command
    *   when a password must be typed in
    * - [`User_acct_seq]: Used instead of [`Success] after the USER command
    *   when an account ID must be typed in
    * - [`Pass_acct_seq]: Used instead of [`Success] after the PASS command
    *   when an account iD must be typed in
    * - [`Preliminary]: a reply with code 100 to 199. There will be another
    *   final reply for the command
   *)

type port =
    [ `Active of string * int * Unix.file_descr
    | `Passive of string * int
    | `Ext_active of string * int * Unix.file_descr
    | `Ext_passive of int
    | `Unspecified 
    ]
  (** The port of the data connection: [`Active] means that the server 
    * initiates the data connection to the listening client, and in the
    * case of [`Passive] the client initiates the data connection to the
    * listening server. The string argument is the IP address as dotted
    * quad, the int argument is the port number, and the descriptor 
    * is the listening master socket.
   *)


type form_code =
    [ `Non_print | `Telnet | `ASA ]
  (** The [form_code] has a meaning when FTP is used to print files:
    * - [`Non_print]: This is not the case.
    * - [`Telnet]: Telnet control codes are used for vertical movement
    * - [`ASA]: ASA (Fortran) control codes are used for vertical movement
   *)

type representation =
    [ `ASCII of form_code option
    | `EBCDIC of form_code option
    | `Image
    ]
  (** The representation of the transferred file:
    * - [`ASCII]: An ASCII variant is used, i.e. the server sends files in
    *   ASCII encoding with CR/LF as end-of-line marker. Supported by all
    *   servers.
    * - [`EBCDIC]: An EBCDIC variant is used, i.e. the server sends files in
    *   EBCDIC encoding with NEL as end-of-line marker
    * - [`Image]: The file is transferred in its original binary
    *   representation. Supported by all servers.
    *
    * "Local" representations are not supported.
    *
    * This FTP client does not recode the files such that they match the
    * selected representation. When files are downloaded, they are stored
    * as they are received. When files are uploaded, they are sent as they
    * are. The user of this client must do recodings when necessary
    * (the class {!Ftp_data_endpoint.data_converter} may be useful for this).
    *
    * If no representation is selected, FTP servers assume [`ASCII None]
    * as default.
   *)


type structure =
    [ `File_structure
    | `Record_structure
    ]
  (** The client supports two structures:
   * - [`File_structure]: Files are simply contiguous streams of bytes
   * - [`Record_structure]: Files are sequences of records. FTP does
   *   not make a difference between variable and fixed length records.
   *   It is not forbidden that the records are themselves structured
   *   into lines, in fact it can happen that end-of-line markers are
   *   contained in binary records. Operating systems that support 
   *   record-structured files often store text files in this format, i.e.
   *   every line is stored in its own record, without end-of-line marker.
   *   If record structure is selected by a STRU command, it is recommended
   *   to use the classes {!Ftp_data_endpoint.out_record_channel} and
   *   {!Ftp_data_endpoint.in_record_channel} for the local representation
   *   of the files, otherwise the records may be incorrectly mapped
   *   to the local conventions.
   *
   * Page-structured files (i.e. indexed files) are not supported.
   *
   * If no structure is selected, FTP servers will assume file structure
   * as default.
   *)


type transmission_mode =
    [ `Stream_mode
    | `Block_mode
    ]
  (** The transmission mode selects how the data are encoded in the data
    * connection.
    * - [`Stream_mode]: This is the simple format that is responsible for
    *   all the failed FTP downloads. It is supported by all FTP servers,
    *   actually, you cannot assume a better transmission mode from an
    *   unknown FTP server. It is unreliable because it cannot distinguish
    *   between a transmission failure and the regular end-of-file condition.
    * - [`Block_mode]: This is an improved format using frames to protect
    *   the transmitted data. Unfortunately, almost no FTP server supports
    *   it.
    *
    * Both modes are compatible with both structures, i.e. you can transfer
    * a record-structured file in stream mode and a flat file in block
    * mode. However, in practice this is not the case. Servers that only
    * know flat files are likely to only support stream mode, and servers
    * implementing record structure imply that block transfers base on
    * the record format. So the advice is to use stream mode for flat
    * files, and block mode for record files.
   *)


type ftp_state =
    { cmd_state : cmd_state;        (** the command state *)
      ftp_connected : bool;         (** whether connected with the server *)
      ftp_data_conn : bool;         (** whether there is a clean data conn *)
      ftp_user : string option;     (** successfully sent user identifier *)
      ftp_password : string option; (** successfully sent password *)
      ftp_account : string option;  (** successfully sent account identifier *)
      ftp_logged_in : bool;         (** whether the user is logged in *)
      ftp_port : port;              (** the selected port *)
      ftp_repr : representation;    (** the selected representation *)
      ftp_structure : structure;    (** the selected structure *)
      ftp_trans : transmission_mode; (** the selected trans mode *)
      ftp_dir : string list;
         (** The current directory, expressed as list of CWD changes minus
          * CDUP changes. This is only reasonable if CWD does not include
          * slashes. The list is in reverse order, i.e. deepest directory
          * first.
          *)
      ftp_features : (string * string option) list option;
         (** The list of features returned by the last FEAT command.
           * [None] means that no FEAT command was yet tried. 
           * [Some []] means that there are no features (either FEAT
           * returned an empty list, or the FEAT command is not implemented
           * by the server). Otherwise the list enumerates pairs 
           * [(label,param)] where [label] is the case-sensitive feature
           * label and [param] the optional parameter. There is no
           * defined order for the list of features.
          *)
      ftp_options : (string * string option) list;
         (** Remembers the OPTS commands sent to the server. The list
           * enumerates pairs [(command,optionparam)], where [command]
           * is the uppercase command name the option refers to. Only
           * the last negotiated [optionparam] for the command is remembered.
          *)
    }
  (** The ftp_state reflects the knowledge of the client about what has been
    * agreed upon with the server.
   *)


type cmd =
    [ `Connect of string * int
    | `Disconnect
    | `Dummy
    (* RFC 959 - standard FTP *)
    | `USER of string
    | `PASS of string
    | `ACCT of string
    | `CWD of string
    | `CDUP
    | `SMNT of string
    | `QUIT
    | `REIN
    | `PORT   (* port is automatically chosen *)
    | `PASV
    | `TYPE of representation
    | `STRU of structure
    | `MODE of transmission_mode
    | `RETR of string * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `STOR of string * (ftp_state -> Ftp_data_endpoint.local_sender)
    | `STOU of (ftp_state -> Ftp_data_endpoint.local_sender)
    | `APPE of string * (ftp_state -> Ftp_data_endpoint.local_sender)
    | `ALLO of int * int option
    | `REST of string
    | `RNFR of string
    | `RNTO of string
    | `DELE of string
    | `RMD of string
    | `MKD of string
    | `PWD
    | `LIST of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `NLST of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `SITE of string
    | `SYST
    | `STAT of string option
    | `HELP of string option
    | `NOOP
    (* Extensions *)
    (* RFC 2389 - feature negotiation *)
    | `FEAT
    | `OPTS of string * string option
    (* RFC 2428 *)
    | `EPRT   (* port is automatically chosen *)
    | `EPSV of [ `AF of Unix.socket_domain | `ALL ] option
    (* RFC 2640 *)
    | `LANG of string option
    (* RFC 3659 *)
    | `MDTM of string
    | `SIZE of string
    | `MLST of string option
    | `MLSD of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    ]
  (** An FTP command. Not all commands are implemented by all servers. *)

type reply = int * string
  (** Reply code plus text *)



(** The client protocol interpreter... *)
class type ftp_client_pi =
object

  method exec_e : ?prelim:(ftp_state -> reply -> unit) ->
                  cmd -> (ftp_state * reply) Uq_engines.engine
    (** Run another command as engine. The command is first started when
	the previous command has terminated. The protocol interpreter does 
	not check whether this command is allowed in the current ftp_state
	or not.

	When the command is done, the engine transitions to
	[`Done(st,r)] where [st] is the state after the command, and [r]
	is the final reply of the server.

	Due to the FTP specification there may be several replies for
	a command: First, zero or more replies with [cmd_state = `Preliminary],
	and then exactly one reply with a final state. The preliminary replies
	can be intercepted with the [prelim] callback.
     *)


  method send_abort : unit -> unit
    (** Sends immediately an [ABOR] command, even when a data transfer is
     * in progress.
     *
     * TODO - not yet implemented
     *)

  method run : unit -> unit
    (** Starts the event system; same as [Unixqueue.run] *)

  method ftp_state : ftp_state
    (** Returns the current ftp_state *)

  method state : unit Uq_engines.engine_state
    (** The state in the sense of [Uq_engines]. Possible values are:
      * - [`Working _]: The control connection is still active. The [int]
      *   argument is currently meaningless.
      * - [`Done()]: The control connection has been terminated.
      * - [`Error e]: A violation of the FTP protocol happened, or another
      *   exception [e] occurred
      * - [`Aborted]: The [abort] method was called
     *)

  method abort : unit -> unit
    (** Shuts any active connection immediately down, and changes the
      * state of the engine to [`Aborted].
     *)

  method event_system : Unixqueue.event_system
    (** The used event system *)

  method request_notification : (unit -> bool) -> unit
    (** as in {!Uq_engines.engine} *)

  method is_empty : bool
    (** Whether the queue is empty *)

  method need_ip6 : bool
    (** Whether [`EPSV] or [`EPRT] are required instead of
	[`PASV] and [`PORT], respectively. This is first set after
	connecting to a server (i.e. when the IP address family is known).
	Before, it is always [false].
     *)

  (** Feature tests

      The following methods are first set when the [`FEAT] command is run.
      Use [feat_method] to do so. Until then, always [false] is returned.
   *)

  method supports_tvfs : bool
    (** Whether TVFS filenames are supported *)

  method supports_mdtm : bool
    (** Whether the [`MDTM] command is supported. Note that [`MDTM] is sometimes
	even supported even if the server does not provide the [`FEAT] command
	to test for this feature.
     *)

  method supports_size : bool
    (** Whether the [`SIZE] command is supported. Note that [`SIZE] is sometimes
	even supported even if the server does not provide the [`FEAT] command
	to test for this feature.
     *)

  method supports_mlst : bool
    (** Whether the [`MLST] and [`MLSD] commands are supported *)

  method mlst_facts : string list
    (** All available facts for [`MLST] and [`MLSD] *)

  method mlst_enabled_facts : string list
    (** The enabled facts for [`MLST] and [`MLSD] *)

  method supports_utf8 : bool
    (** Whether the UTF-8 extension is understood by the server (RFC 2640) *)

end


(** An [ftp_method] is a small procedure doing some task *)
type ftp_method =
    ftp_client_pi -> unit Uq_engines.engine


exception FTP_method_temp_failure of int * string
exception FTP_method_perm_failure of int * string
exception FTP_method_unexpected_reply of int * string
  (** These exceptions may be raised during execution by the FTP method.
    * The int is the unexpected FTP control code and the string the
    * corresponding text. A temporary failure has a code between 400 and
    * 499, and a permanent failure has a code between 500 and 599.
   *)

val connect_method : host:string ->
                     ?port:int ->
                     unit -> ftp_method
  (** This method connects to the [host]  *)


val login_method : user:string ->
                   get_password:(unit -> string) ->
                   get_account:(unit -> string) ->
                   unit ->
                       ftp_method
(** This FTP method logs the [user] in. [get_password] is called when
  * the FTP server asks for the password (may be skipped). [get_account]
  * is called when the server asks for the account ID (may be skipped).
 *)

val quit_method : unit -> ftp_method
  (** Quits and disconnects *)


val walk_method : [ `File of string | `Dir of string | `Stay ] ->
                  ftp_method
(** This FTP method walks to the target directory:
  *
  * - [`File name]: The [name] is interpreted as slash-separated path.
  *   It is always interpreted relative to the home directory of the 
  *   user (i.e. the directory after login), even if it begins with a
  *   slash. The FTP command walks to the directory containing [name].
  * - [`Dir name]: The FTP command walks to the directory [name] (same
  *   syntax as for [`File]).
  * - [`Stay]: The FTP command does nothing (stays in the current directory).
 *)

type filename =
    [ `NVFS of string
    | `TVFS of string
    | `Verbatim of string
    ]
  (** There are several methods how to specify filenames:
    * - [`NVFS name]: The "Network Virtual File System" is the normal way
    *   of accessing FTP files. The client walks into the directory containing
    *   the file using [CWD] and [CDUP] commands, and calls the file operation
    *   from this directory. For simplicity, this client interprets slashes
    *   in [name] as path component separators. The FTP server will never
    *   see these slashes.
    * - [`TVFS name]: The optional "Trivial Network File System" avoids the
    *   [CWD] and [CDUP] commands. The tagged [name] is normalized (double
    *   slashed removed etc.), and is passed to the server as-is. Before using
    *   the faster TVFS one should check whether it is supported (feature
    *   "TVFS"). Note that even for TVFS there are no special files "."
    *   and ".."!
    * - [`Verbatim name]: The string [name] is passed to the server without
    *   transforming it in any way.
   *)

val get_method : file:filename ->
                 representation:representation ->
                 store:(ftp_state -> Ftp_data_endpoint.local_receiver) ->
                 unit ->
                     ftp_method
(** This FTP method walks to the right directory and gets [file] from
  * the server. The file is stored in the [local_receiver] that can be
  * obtained by calling the [store] function. The selected [representation]
  * remains unchanged.
 *)

val put_method : ?meth:[ `STOR | `APPE ] ->
                 file:filename ->
                 representation:representation ->
                 store:(ftp_state -> Ftp_data_endpoint.local_sender) ->
                 unit ->
                     ftp_method
(** This FTP method walks to the right directory and puts [file] to
  * the server. The file is taken from the [local_sender] that can be
  * obtained by calling the [store] function. The selected [representation]
  * remains unchanged.
  *
  * [meth] selects the method to use (default [`STOR]).
 *)

val invoke_method : command:cmd ->
                    unit ->
                        ftp_method
(** This FTP method simply invokes [command]. *)

val set_structure_method : structure -> ftp_method
(** Requests a certain structure for future file transfers *)

val set_mode_method : transmission_mode -> ftp_method
(** Requests a certain mode for future file transfers *)

val rename_method : file_from:filename ->
                    file_to:filename ->
                    unit ->
                        ftp_method
(** Renames the [file_from] into [file_to].
  *
  * Both file names must be of the same type, either [`NVFS] or [`Verbatim].
  * If [`NVFS], both names must be in the same directory.
 *)

val mkdir_method : filename -> ftp_method
(** Creates the named directory *)

val rmdir_method : filename -> ftp_method
(** Deletes the named directory *)

val delete_method : filename -> ftp_method
(** Deletes the named file *)

val list_method : dir:filename ->
                  representation:representation ->
                  store:(ftp_state -> Ftp_data_endpoint.local_receiver) ->
                  unit ->
                      ftp_method
(** Lists the contents of the directory [dir] using the LIST command.
  * The [representation] must not be [`Image].
 *)

val nlst_method : dir:filename ->
                  representation:representation ->
                  store:(ftp_state -> Ftp_data_endpoint.local_receiver) ->
                  unit ->
                      ftp_method
(** Lists the contents of the directory [dir] using the NLST command
  * The [representation] must not be [`Image].
 *)

val parse_nlst_document : string -> string list
  (** Returns the filenames contained in the output of [`NLST] *)


val mdtm_method : file:filename ->
                  process_result:(float -> unit) ->
                  unit ->
                      ftp_method
(** Determines the date and time of the last modification of [file].
  * On success, [process_result] is called.
 *)

val size_method : file:filename ->
                  representation:representation ->
                  process_result:(int64 -> unit) ->
                  unit ->
                      ftp_method
(** Determines the size of [file]. On success, [process_result] is called.
    The size depends on [representation].
 *)


val feat_method : ?process_result:((string * string option) list -> unit) ->
                   unit -> ftp_method
  (** Get the list of feature tokens (see also
      {!Ftp_client.ftp_state.ftp_features})
   *)


type entry = string * (string * string) list
    (** A file entry [(name, facts)]. The facts are given as pairs
	[(factname,value)] where [factname] is always lowercase.
	For parsers for common facts see below.
     *)

val mlst_method : file:filename ->
                  process_result:(entry list -> unit) ->
                  unit ->
                      ftp_method
     (** Get the file entry for [file]. *)

val mlsd_method : dir:filename ->
                  store:(ftp_state -> Ftp_data_endpoint.local_receiver) ->
                  unit ->
                      ftp_method
    (** Gets the entries for this directory. *)

val parse_mlsd_document : string -> entry list
  (** Returns the entries contained in the output of [`MLSD] *)


type entry_type =
    [ `File | `Cdir | `Pdir | `Dir | `Other ]
  (** Types:
      - [`File]: entry refers to file
      - [`Cdir]: entry refers to the directory being listed
      - [`Pdir]: entry is a parent of the directory being listed
      - [`Dir]: entry refers to directory
      - [`Other]: entry is neither file nor directory
   *)

type entry_perm =
    [ `Append | `Create | `Delete | `Enter | `Rename | `List | `Mkdir
    | `Delete_member | `Read | `Write 
    ]
  (** Permissions:
      - [`Append]: append to file possible
      - [`Create]: file can be created in this dir
      - [`Delete]: file or dir can be deleted
      - [`Enter]: dir can be entered
      - [`Rename]: file or dir can be renamed
      - [`List]: dir can be listed
      - [`Mkdir]: subdir can be created in this dir
      - [`Delete_member]: a file or dir can be deleted in this directory
      - [`Read]: a file can be retrieved
      - [`Write]: a file can be stored
   *)

(** The following functions extract commonly used facts from entries.
    They may raise [Not_found].
 *)

val get_name : entry -> string
val get_size : entry -> int64
val get_modify : entry -> float
val get_create : entry -> float
val get_type : entry -> entry_type
val get_unique : entry -> string
val get_perm : entry -> entry_perm list
val get_lang : entry -> string
val get_media_type : entry -> string
val get_charset : entry -> string
val get_unix_mode : entry -> int
val get_unix_uid : entry -> string
val get_unix_gid : entry -> string


(** The ftp client is a user session that may even span several connections.
  * However, only one server is connected at once.
 *)
class ftp_client :
        ?event_system:Unixqueue.event_system ->
        unit ->
object
  method exec_e : ftp_method -> unit Uq_engines.engine
    (** Runs the method asynchronously as engine. *)

  method exec : ftp_method -> unit
    (** Runs the method synchronously. Note that this implies a call of
	{!Unixqueue.run}.
     *)

  method pi : ftp_client_pi
    (** The curerent protocol interpreter. It is allowed that a different [pi]
	is created when a new connection is opened.

	The [pi] is first available after running the first method. The
	method fails if [pi] is unavailable.
     *)
  
  method run : unit -> unit
    (** Starts the event system; same as {!Unixqueue.run} *)

  method configure_timeout : float -> unit
    (** Configures a timeout for both the control and the data connection.
	This must be done before connecting to a server.
     *)

  method set_socks5_proxy : string -> int -> unit
      (** Sets that a SOCKS version 5 proxy is used at this host and port.
          There is no support for authentication at the proxy. Only
	  passive mode is supported.
       *)

  method event_system : Unixqueue.event_system

  method reset : unit -> unit
    (** Aborts all current activities if any, and re-initializes the client *)

end


(** {2 Examples and Discussion}
  *
  * To download a single flat file from a server:
  *
  * {[ 
  *   let buffer = Buffer.create 1000 in
  *   let ch = new Netchannels.output_buffer buffer in
  *   let client = new ftp_client() in
  *   client # exec (connect_method ~host:"127.0.0.1" ());
  *   client # exec (login_method ~user:"foo"
  *                                ~get_password:(fun () -> "password")
  *                                ~get_account:(fun () -> "foo") ());
  *   client # exec (get_method ~file:(`NVFS "path/to/file")
  *                             ~representation:`Image
  *                             ~store:(fun _ -> `File_structure ch) ());
  * ]}
  *
  * The file is stored in [buffer]. By using a different netchannel, it
  * can be stored whereever wanted.
  *
  * To download a record-structured text file, use a [store] like:
  *
  * {[ 
  *    let ch = (as above) in
  *    let rec_ch = new Ftp_data_endpoint.write_out_record_channel
  *                       ~repr:(`ASCII_unix `Enc_iso88591)
  *                       ch
  *    ...
  *    ... ~store:(fun _ -> `Record_structure rec_ch)
  * ]}
  *
  * Here, the end-of-record is transcoded to newline. Note that the ASCII
  * variant ([`Enc_iso88591]) is ignored by [write_out_record_channel].
  * {b Open: How to select record structure using an FTP method.}
  *
  * Character conversions: To convert an EBCDIC file to ASCII, use
  * something like
  *
  * {[ 
  *    let ch = (as above) in
  *    let converter = new Ftp_data_endpoint.data_converter
  *                         ~fromrepr:(`EBCDIC `Enc_cp1047)
  *                         ~torepr:(`ASCII_unix `Enc_iso88591) in
  *    let ch_ebcdic = new Netchannels.output_filter converter ch in
  *    ...
  *    ... ~representation:(`EBCDIC None)
  *    ... ~store:(fun _ -> `File_structure ch_ebcdic)
  * ]}
  *
  * The class [data_converter] also performs the transformation of the 
  * end-of-line convention, unlike the similar class
  * [Netconversion.conversion_pipe].
 *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module  *)
end
