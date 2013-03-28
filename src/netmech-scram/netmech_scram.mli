(* $Id: netmech_scram.mli 1560 2011-03-04 22:05:14Z gerd $ *)

(** SCRAM mechanism for authentication (RFC 5802) *)

(** This implements SCRAM-SHA-1 for GSSAPI. Other profiles may be added later.

    As we do not implement SASLprep, usernames and passwords are restricted
    to US-ASCII.
 *)

type ptype = [ `GSSAPI ]
  (** Currently only the variant for [`GSSAPI] is supported *)

type mechanism = [ `SHA_1 ]

type profile =
    { ptype : ptype;
      mechanism : mechanism;       (** Which mechanism *)
      return_unknown_user : bool;  (** Whether servers exhibit the fact that the
				       user is unknown *)
      iteration_count_limit : int; (** Largest supported iteration number *)
    }
  (** Profile *)


type server_error =
    [ `Invalid_encoding
    | `Extensions_not_supported
    | `Invalid_proof
    | `Channel_bindings_dont_match
    | `Server_does_support_channel_binding
    | `Channel_binding_not_supported
    | `Unsupported_channel_binding_type
    | `Unknown_user
    | `Invalid_username_encoding
    | `No_resources
    | `Other_error
    | `Extension of string
    ]
  (** Error codes of this protocol *)

type client_session
  (** Session context for clients *)


type server_session
  (** Session context for servers *)


exception Invalid_encoding of string * string
  (** Raised by clients when something cannot be decoded. First string
      is an error message, the second string the raw message that cannot
      be decoded
   *)

exception Invalid_username_encoding of string * string
  (** Raised by clients when the username does not match the requirements.
      Arguments as for [Invalid_encoding].
   *)

exception Extensions_not_supported of string * string
  (** Raised by clients when the server enables an unsupported extension.
      Arguments as for [Invalid_encoding].
   *)

exception Protocol_error of string
  (** Raised by clients when the server violates the protocol. The argument
      is a message.
   *)

exception Invalid_server_signature
  (** Raised by clients when the signature sent by the server is invalid
      (i.e. the server does not know the client password)
   *)

exception Server_error of server_error
  (** Raised by clients when the server sent an error code *)


val profile : ?return_unknown_user:bool -> ?iteration_count_limit:int ->
              ptype -> profile
  (** Creates a profile *)

val string_of_server_error : server_error -> string
val server_error_of_string : string -> server_error
  (** Conversion *)


(** {2 Clients} *)

(** The idea is to create a client session [s] first. The functions
    [client_emit_flag] and [client_recv_flag] indicate now whether
    the client needs to emit a new message, or whether it needs to
    receive a message, respectively. Emission is done by [client_emit_message],
    reception by [client_recv_message]. If everything goes well, the
    protocol state advances, and finally [client_finish_flag] is true.
    This indicates that the client is authenticated and that the server
    knows the client's password. If an error occurs, an exception is
    raised (see above for possibilities), and [client_error_flag] signals
    [true].
 *)

val create_client_session : profile -> string -> string -> client_session
  (** [create_client_session p username password]: Creates a new client
      session for profile [p] so that the client authenticates as user
      [username], and proves its identify with the given [password].
   *)

val client_configure_channel_binding : client_session -> string -> unit
  (** Instruct the client to require a channel binding. The passed string
      is the [c] parameter (before encoding it via Base64. The function
      needs to be called before sending the second message to the server.
      It fails if called too late.
   *)
(* SASL: The string would have to include the gs2-header. For a SASL-enabled
   profile we would need some additional functions
   (client_negotiate_channel_binding).
 *)

val client_emit_flag : client_session -> bool
  (** Whether [client_emit_message] can now be called *)

val client_recv_flag : client_session -> bool
  (** Whether [client_recv_message] can now be called *)

val client_finish_flag : client_session -> bool
  (** Whether the client is authenticated and the server verified *)

val client_error_flag : client_session -> bool
  (** Whether an error occurred, and the protocol cannot advance anymore *)

val client_channel_binding : client_session -> string
  (** Returns the channel binding ("" of none) *)

val client_emit_message : client_session -> string
  (** Emits the next message to be sent to the server *)

val client_recv_message : client_session -> string -> unit
  (** Receives the next message from the server *)

val client_protocol_key : client_session -> string option
  (** The 128-bit protocol key for encrypting messages. This is available 
      as soon as the second client message is emitted.
   *)

val client_user_name : client_session -> string
  (** The user name *)

val client_export : client_session -> string
val client_import : string -> client_session
  (** Exports a client session as string, and imports the string again.
      Only established sessions are allowed to be exported
      (for which [client_finish_flag] is true).

      The export format is just a marshalled Ocaml value.
   *)


(** {2 Servers} *)

(** The idea is to create a server session [s] first. The functions
    [server_emit_flag] and [server_recv_flag] indicate now whether
    the server needs to emit a new message, or whether it needs to
    receive a message, respectively. Emission is done by [server_emit_message],
    reception by [server_recv_message]. If everything goes well, the
    protocol state advances, and finally [server_finish_flag] is true.
    This indicates that the client could be authenticated.

    If an error occurs, {b no} exception is raised, and the protocol
    advances nevertheless, and finally the server sends an error token
    to the client. After this, [server_error_flag] returns true.
 *)


val create_server_session : 
      profile -> (string -> string * string * int) -> server_session
  (** [create_server_session p auth]: Creates a new server session with
      profile [p] and authenticator function [auth].

      The function is [auth] is called when the credentials of the
      client have been received to check whether the client can be
      authenticated. It is called as

      {[
      let (salted_password, salt, iteration_count) = auth username
      ]}

      where [username] is the user name. The function can now raise
      [Not_found] if the user is unknown, or it can return the
      shown triple. Note that the cleartext password needs not to
      be known. [salt] is a random string, and [iteration_count] a
      security parameter that should be at least 4096. Whereas [salt]
      should be different for each user, the [iteration_count] can be
      chosen as a constant (e.g. 4096). Now [salted_password] can be
      computed from the cleartext password and these two extra parameters.
      See [salt_password] below.
   *)

val create_salt : unit -> string
  (** Creates a random string suited as salt *)

val salt_password : string -> string -> int -> string
  (** [let salted_password = salt_password password salt iteration_count]

      As we do not implement [SASLprep] only passwords consisting of
      US-ASCII characters are accepted ([Invalid_encoding] otherwise).
   *)

val server_emit_flag : server_session -> bool
  (** Whether [server_emit_message] can now be called *)

val server_recv_flag : server_session -> bool
  (** Whether [server_recv_message] can now be called *)

val server_finish_flag : server_session -> bool
  (** Whether the client is authenticated *)

val server_error_flag : server_session -> bool
  (** Whether an error occurred, and the protocol cannot advance anymore *)

val server_emit_message : server_session -> string
  (** Emits the next message to be sent to the client *)

val server_recv_message : server_session -> string -> unit
  (** Receives the next message from the client *)

val server_protocol_key : server_session -> string option
  (** The 128-bit protocol key for encrypting messages. This is available 
      as soon as the second client message has been received.
   *)

val server_channel_binding : server_session -> string option
  (** Returns the channel binding requirement (the "c" parameter). It is
      up to the application to enforce the binding. This information is 
      available as soon as the second client message has been received
   *)

val server_user_name : server_session -> string option
  (** The user name as transmitted from the client. This is returned here
      even before the authentication is completed!
   *)

val server_export : server_session -> string
val server_import : string -> server_session
  (** Exports a server session as string, and imports the string again.
      Only established sessions are allowed to be exported
      (for which [server_finish_flag] is true).

      The export format is just a marshalled Ocaml value.
   *)


(** {2 Confidentiality} *)

type specific_keys =
    { kc : string;
      ke : string;
      ki : string
    }
  (** The specific keys to use *)

(** This module implements AES in Ciphertext Stealing mode (see RFC 3962) *)
module AES_CTS : sig
  val c : int
  val m : int
  val encrypt : string -> string -> string
  val encrypt_mstrings : 
    string -> Xdr_mstring.mstring list -> Xdr_mstring.mstring list
  val decrypt : string -> string -> string
  val decrypt_mstrings : 
    string -> Xdr_mstring.mstring list -> Xdr_mstring.mstring list
  val tests : (string * string * string) list
  val run_tests : unit -> bool
  val run_mtests : unit -> bool
end


(** This is the cryptosystem as defined in RFC 3961, so far needed here.
    This uses [AES_CTS] as cipher, and SHA1-96 for signing.
 *)
module Cryptosystem : sig
  exception Integrity_error

  val derive_keys : string -> int -> specific_keys
    (** [derive_keys protocol_key usage]: Returns the specific keys for
	this [protocol_key] and this [usage] numbers. See RFC 4121 for
	applicable usage numbers
     *)

  val encrypt_and_sign :  specific_keys -> string -> string
    (** Encrypts the plaintext message and adds a signature to the
	ciphertext.

	Returns [ciphertext_with_signature].
     *)

  val encrypt_and_sign_mstrings : 
         specific_keys -> Xdr_mstring.mstring list -> Xdr_mstring.mstring list
    (** Same, but with data representation as [mstring list] *)

  val decrypt_and_verify :  specific_keys -> string -> string
    (** Decrypts the ciphertext and verifies the attached signature.
	Returns the restored plaintext. 

	For very short plaintexts (< 16 bytes) there will be some
	padding at the end ("residue"), as returned as [ec] above.
	We ignore this problem generally,
	because GSS-API adds a 16-byte header to the plaintext anyway,
	so these short messages do not occur.

	If the signature is not valid, the exception [Integrity_error]
	is raised.
     *)

  val decrypt_and_verify_mstrings :
         specific_keys -> Xdr_mstring.mstring list -> Xdr_mstring.mstring list
    (** Same, but with data representation as [mstring list] *)

  val get_ec : specific_keys -> int -> int
    (** [let ec = get_ec e_keys n]:
        Returns the required value for the "extra count" field of
	RFC 4121 if the plaintext message has size [n]. Here,
	[n] is the size of the payload message plus the token
	header of 16 bytes, i.e. the function is always called with
	[n >= 16].

	Here, the returned [ec] value is always 0.
     *)

  val get_mic : specific_keys -> string -> string
    (** Returns a message integrity code *)

  val get_mic_mstrings :
         specific_keys -> Xdr_mstring.mstring list -> string
    (** Same, but with data representation as [mstring list] *)
end


module Debug : sig
  val enable : bool ref
    (** Enable debugging of this module *)
end
