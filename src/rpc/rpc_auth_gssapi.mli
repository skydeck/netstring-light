(* $Id: rpc_auth_gssapi.mli 1556 2011-03-03 16:23:34Z gerd $ *)

(** GSS-API for RPC authentication *)

open Netgssapi

type user_name_format =
    [ `Exported_name
    | `Prefixed_name
    | `Plain_name
    ]
  (** What to return as user name:
      - [`Exported_name]: the exported name in binary format (as described
        in RFC 2078, section 3.2). This format can only be read back by
        the [gss_api] object generating the name.
      - [`Prefixed_name]: the display name in a text format
        "[{<oid>}<namestring>]".
      - [`Plain_name]: the string part of the display name
   *)

val server_auth_method : 
      ?require_privacy:bool ->
      ?require_integrity:bool ->
      ?shared_context:bool ->
      ?acceptor_cred:credential ->
      ?user_name_format:user_name_format ->
      ?seq_number_window:int ->
      gss_api -> oid -> Rpc_server.auth_method
  (** Creates an authentication method from a GSS-API interface.
      The OID selects the desired authentication method.

      Options:
      - [require_privacy]: Whether the messages must be
        encrypted. If not enabled, the server also accepts non-encrypted
        messages that are authenticated via GSS-API.
      - [require_integrity]: Whether integrity checksums must be
        included. If not enabled, the server also accepts non-signed
        messages that are authenticated via GSS-API.
      - [shared_context]: Whether this method maintains only one
        security context for all connections. By default,
        each connection has a security context of its own. For UDP,
        this option needs to be set, because each UDP request is
        considered as creating a new connection.
      - [acceptor_cred]: Overrides the credentials of the server. By
        default, it is left to [gss_api] which credential is
        assumed.
      - [user_name_format]: Defaults to [`Prefixed_name].
      - [seq_number_window]: If set, the server checks for replayed
        requests. The integer is the length of the check window (see
        RFC 2203 section 5.3.3.1). If omitted, no such checks are
        performed (the default). 
   *)

type support_level =
    [ `Required | `If_possible | `None ]

type user_name_interpretation =
    [ `Exported_name
    | `Prefixed_name
    | `Plain_name of oid
    ]

val client_auth_method :
      ?privacy:support_level ->
      ?integrity:support_level ->
      ?user_name_interpretation:user_name_interpretation ->
      gss_api -> oid -> Rpc_client.auth_method
  (** Creates an authentication method from a GSS-API interface.
      The OID selects the desired authentication method.

      Options:
      - [privacy]: Selects whether messages are encrypted. If [`Required],
        the authentication method fails if the GSS-API does not support
        encryption, and it enables encryption if GSS-API supports it.
        If [`If_possible] encryption is enabled if GSS-API supports it
        (the default). If [`None], the messages are not encrypted.
      - [integrity]: Selects whether messages are signed. If [`Required],
        the authentication method fails if the GSS-API does not support
        integrity protection, and it enables this feature if GSS-API supports
        it. If [`If_possible] integrity protection is enabled if GSS-API
        supports it (the default). If [`None], the messages are not signed.
      - [user_name_format]: Defaults to [`Prefixed_name].
   *)

module Debug : sig
  val enable : bool ref
end
