(* $Id: rpc_key_service.mli 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


(** Contact the keyserv daemon to encrypt/decrypt data with the common
 * key.
 *
 * If the keyserv crashes, the current call will raise an exception
 * (e.g. broken pipe), but the next call will try to reconnect.
 *
 * This module works fully synchronous, i.e. waits until the keyserv
 * responds. This is normally not a problem, because the keyserv daemon
 * runs on the same system, and no network latencies can occur.
 *)

exception Netname_unknown
  (** The netname is invalid *)

exception No_secret_key
  (** The user does not have a secret key for the netname *)

exception Key_service_problem of exn
  (** All exceptions (except the exceptions defined above) are wrapped into
   * [Key_service_problem].
   *)

type t
  (** represents a client of the keyserv daemon *)

type connector =
    [ `Direct of (Rpc_client.connector * Rpc.protocol)
    | `Keyenvoy of string                    (* path of keyenvoy program *)
    ]
  (** How to connect to keyserv:
    * - [`Direct(c,p)]: Create a direct RPC connection to the keyserv
    *   program listening at [c] using protocol [p]. This usually only
    *   works if [c] is a local transport like Unix Domain.
    * - [`Keyenvoy path]: Call the [keyenvoy] program installed at [path]
   *)

val create : ?connector:connector ->
             unit ->
	       t
  (** Connects to the keyserv daemon. By default (no [connector]), the local
   * keyserv daemon is contacted in an OS-specific way.
   *)

val generate : t -> string
  (** Generates a new conversation key (a 64 bit random number) *)

val encrypt : t -> string -> string -> string
  (** This function is used if this program is a client and wants to contact
   * a server. The first passed string is the netname of the server.
   * Furthermore, the keyserv daemon automatically determines the netname
   * of this process. The daemon looks up the public key of the server
   * and the secret key of the client, and computes the common key using
   * the Diffie Hellman scheme.
   * The second passed string (exactly 8 characters) is DES-encrypted with the
   * common key in ECB mode, and returned (again 8 characters).
   *)

val decrypt : t -> string -> string -> string
  (** This function is used if this program is a server and wants to check
   * the identity of a contacting client.
   * The first passed string is the netname of the client.
   * Furthermore, the keyserv daemon automatically determines the netname
   * of this process. The daemon looks up the public key of the client
   * and the secret key of the server, and computes the common key using
   * the Diffie Hellman scheme.
   * The second passed string (exactly 8 characters) is DES-decrypted with the
   * common key in ECB mode, and returned (again 8 characters).
   *)

(** {2 Example}
 * - The client is: "unix.100\@domain"
 * - The server is: "unix.mercury\@domain"       (* i.e. root\@mercury *)
 *
 * The client encrypts with
 *   {[ let enc_data = encrypt "unix.mercury\@domain" data ]}
 * This works because the keyserv daemon knows from the OS that the current
 * process is run by "unix.100@domain". So both identities are known.
 *
 * The server decrypts with
 *   {[ let data' = decrypt "unix.100\@domain" enc_data ]}
 * And it is ensured that data = data'.
 *)

val net_get : t -> (string * string * string)
  (* Returns the triple (netname, pubkey, privkey) for the user running this
   * process.
   *)


val shut_down : t -> unit
  (* Closes the file descriptor *)

(* Note: The keyserv daemon has some more functions but we don't need them
 * to implement AUTH_DH.
 *)
