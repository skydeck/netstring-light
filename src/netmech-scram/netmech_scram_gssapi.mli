(* $Id: netmech_scram_gssapi.mli 1546 2011-02-12 17:24:41Z gerd $ *)

(** The SCRAM security mechanism for GSS-API *)

(** See RFC 5802 *)

open Netgssapi

val scram_mech : oid
  (** The OID of SCRAM *)

(** A [client_key_ring] identifies the user on the client side *)
class type client_key_ring =
object
  method password_of_user_name : string -> string
    (** Returns the cleartext password for a user name, or
	raises [Not_found] if the user is unknown
     *)

  method default_user_name : string option
    (** This method may return a default user name *)
end


(** A [server_key_verifier] verifies on the server side that the users
    exist and have the right authentication credentials
 *)
class type server_key_verifier =
object
  method scram_credentials : string -> string * string * int
    (** Returns the triple
	{[ (salted_password, salt, iteration_count) ]}
	for a user, or raises [Not_found]. See
	{!Netmech_scram.create_server_session} for the meaning of this
	triple.
     *)
end


class scram_gss_api : 
        ?client_key_ring:client_key_ring ->
        ?server_key_verifier:server_key_verifier ->
        Netmech_scram.profile ->
          Netgssapi.gss_api
  (** Returns a standard-compliant GSS-API object for the passed SCRAM
      profile. The object can be used on the client side for all
      users whose passwords are available via [client_key_ring].
      By default, the key ring is empty. On the server side, the object
      authenticates all users whose credentials are available via
      [server_key_verifier]. By default, no user can be verified.
      
      SCRAM only allows usernames of type [NT_USER_NAME] for identifying
      users.

      For principals (servers), this SCRAM implementation allows identifiers
      of type [NT_HOSTBASED_SERVICE] and [NT_USER_NAME]. Any such name
      can be used, because the SCRAM protocol does not use principal
      names. The contexts will always return the hostbased service "@" as
      name of the principals.

      This implementation checks whether the messages are verified and
      unwrapped in the same order than generated, and reports this via the
      [`Unseq_token] and [`Gap_token] flags. Support for true replay
      detection ([`Duplicate_token]) is not implemented, though.
      Replayed tokens will also be marked as [`Unseq_token].
   *)
