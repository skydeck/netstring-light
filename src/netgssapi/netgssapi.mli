(* $Id: netgssapi.mli 1562 2011-03-07 16:13:14Z gerd $ *)

(** GSS-API Definition *)

(** This is mainly a translation of RFC 2743/2744 to Ocaml. *)

(** {2 Types} *)

type oid = int array
    (** OIDs like "1.3.6.1.5.6.2" as array of int's. The empty array
	means [GSS_C_NO_OID].
     *)

type oid_set = oid list
    (** A set of OID's. These lists should not contain OID's twice.
	The empty list means [GSS_C_NO_OID_SET].
     *)

type credential = < otype : [ `Credential ] >
    (** A credential is opaque for the caller of the GSS-API.
	The provider of the GSS-API can emit new credential objects,
	and hand them out to the caller. When the caller passes 
	credentials back to the provider, the provider must check
	whether the object is known, and reject any fake objects
	created by the caller by raising [Invalid_argument].
     *)

type context = < otype : [ `Context ]; valid : bool >
    (** A context is also opaque, and the same rules apply as for
	[credential].

	The method [valid] is true as long as the context is not
	deleted.
     *)

type token = string
    (** Authentication tokens. These are also opaque to the caller,
	but have a string representation so that they can be sent
	over the wire.
     *)

type interprocess_token = string
    (** Interprocess tokens. These are also opaque to the caller,
	but have a string representation so that they can be sent
	over the wire.
     *)

type calling_error =
    [ `None
    | `Inaccessible_read
    | `Inaccessible_write
    | `Bad_structure
    ]
    (** Possible errors caused by the caller *)

type routine_error =
    [ `None
    | `Bad_mech
    | `Bad_name
    | `Bad_nametype
    | `Bad_bindings
    | `Bad_status
    | `Bad_mic
    | `No_cred
    | `No_context
    | `Defective_token
    | `Defective_credential
    | `Credentials_expired
    | `Context_expired
    | `Failure
    | `Bad_QOP
    | `Unauthorized
    | `Unavailable
    | `Duplicate_element
    | `Name_not_mn
    ]
    (** Possible errors caused by the provider *)

type suppl_status =
    [ `Continue_needed
    | `Duplicate_token
    | `Old_token
    | `Unseq_token
    | `Gap_token
    ]
    (** Further flags *)

type major_status = calling_error * routine_error * suppl_status list
    (** The major status consists of these three elements. The bits of the
	supplementary status field are represented as list
     *)

type minor_status = int32
    (** The minor status is provider-specific. Note that GSS-API defines
	it as {b unsigned} 32-bit integer whereas [int32] is signed.
     *)

type name = < otype : [ `Name ] >
    (** A name is also opaque, and the same rules apply as for
	[credential].
     *)

type address =
    [ `Unspecified of string
    | `Local of string
    | `Inet of Unix.inet_addr
    | `Nulladdr
    | `Other of int32 * string
    ]
  (** Addresses tagged by address types *)

type channel_bindings = address * address * string
    (** Channel binding as tuple
	[(initiator_address, acceptor_address, application_data)] 
     *)

type cred_usage = [ `Initiate |`Accept | `Both ]

type qop = < otype : [ `QOP ] >
    (** Quality-of-proctection parameters are mechanism-specific *)

type message =
    Xdr_mstring.mstring list
    (** Messages are represented as lists of [mstring] *)

type ret_flag =
    [ `Deleg_flag | `Mutual_flag | `Replay_flag | `Sequence_flag 
    | `Conf_flag | `Integ_flag | `Anon_flag | `Prot_ready_flag
    | `Trans_flag
    ]
    (** Flags for the [accept_sec_context] method *)

type req_flag = 
    [ `Deleg_flag | `Mutual_flag | `Replay_flag | `Sequence_flag 
    | `Conf_flag | `Integ_flag | `Anon_flag
    ]
    (** Flags for the [init_sec_context] method *)


(** {2 Exceptions} *)

(** There are no defined exceptions.

    Errors should be reported using the [major_status] and [minor_status]
    codes as much as possible.

    [Invalid_argument] may be raised for clear violations of calling
    requirements, e.g. when an opaque object is passed to this interface
    that was not returned by it before.
 *)

(** {2 The API} *)

(** The methods have generally a type of the form

    {[ 
       m : 't . arg1 -> ... -> argN -> out:( ret1 -> ... -> retM -> 't ) -> 't 
    ]}

    where [arg]s are input arguments (with the exception of [context] 
    which is in/out), and where outputs are passed back by calling the [out]
    functions with the outputs. The return value of [out] is the return
    value of the method call.

    For example, if only [output_token] of the [accept_sec_context] method
    is needed, one could call this method as in

    {[
      let output_token =
	gss_api # accept_sec_context 
	   ... 
	   ~out:(fun ~src_name ~mech_type ~output_token ~ret_flags
		     ~time_rec ~delegated_cred_handle ~minor_status
		     ~major_status ->
		  output_token
		)
    ]}

    Output values may not be defined when [major_status] indicates
    an error. (But see the RFC for details; especially [init_sec_contect]
    and [accept_sec_context] may emit tokens even when [major_status]
    indicates an error.)

    The names of the parameters are taken from RFC 2744, only
    suffixes like [_handle] have been removed. When the prefixes
    [input_] and [output_] are meaningless, they are also removed.
    All prefixes like "GSS" are removed anyway.
 *)

class type gss_api =
object
  method provider : string
    (** A string name identifying the provider *)

  method no_credential : credential
    (** A substitute credential for [GSS_C_NO_CREDENTIAL] *)

  method no_name : name
    (** A substitute name for [GSS_C_NO_NAME] *)

  method accept_sec_context :
          't . context:context option ->
               acceptor_cred:credential -> 
               input_token:token ->
               chan_bindings:channel_bindings option ->
               out:( src_name:name ->
		     mech_type:oid ->
		     output_context:context option ->
		     output_token:token ->
		     ret_flags:ret_flag list ->
		     time_rec:[ `Indefinite | `This of float] ->
		     delegated_cred:credential ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't 
		   ) -> unit -> 't
    (** On the first call, pass [~context:None]. If successful, the
	function outputs a non-None [~output_context] which should be
	passed as new [~context] in follow-up calls.

	If the [output_token] is non-empty, it must be transmitted to
	the peer - independent of the [major_status].
     *)


  method acquire_cred :
          't . desired_name:name ->
               time_req:[`None | `Indefinite | `This of float] ->
               desired_mechs:oid_set ->
               cred_usage:cred_usage  ->
               out:( cred:credential ->
		     actual_mechs:oid_set ->
		     time_rec:[ `Indefinite | `This of float] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method add_cred :
          't . input_cred:credential ->
               desired_name:name ->
               desired_mech:oid ->
               cred_usage:cred_usage ->
               initiator_time_req:[`None | `Indefinite | `This of float] ->
               acceptor_time_req:[`None | `Indefinite | `This of float] ->
               out:( output_cred:credential ->
		     actual_mechs:oid_set ->
		     initiator_time_rec:[ `Indefinite | `This of float] ->
		     acceptor_time_rec:[ `Indefinite | `This of float] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method canonicalize_name :
          't . input_name:name ->
               mech_type:oid ->
               out:( output_name:name ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method compare_name :
          't . name1:name ->
               name2:name ->
               out:( name_equal:bool ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method context_time :
          't . context:context ->
               out:( time_rec:[ `Indefinite | `This of float] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method delete_sec_context :
          't . context:context ->
               out:( minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't
     (** Output tokens are not supported (this is a deprecated feature of
	 GSSAPI)
      *)

  method display_name :
          't . input_name:name ->
               out:( output_name:string ->
		     output_name_type:oid ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method display_minor_status :
          't . minor_status:minor_status ->
               mech_type: oid ->
               out:( status_strings: string list ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't
    (** Note that [display_minor_status] decodes all status value parts in
	one step and returns the result as [string list]. Also, this
	method is restricted to decoding minor statuses
     *)

  method export_name : 
          't . name:name ->
               out:( exported_name:string ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method export_sec_context :
          't . context:context ->
               out:( interprocess_token:interprocess_token ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method get_mic : 
          't . context:context ->
               qop_req:qop option ->
               message:message ->
               out:( msg_token:token ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method import_name :
          't . input_name:string ->
               input_name_type:oid ->
               out:( output_name:name ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method import_sec_context :
          't . interprocess_token:interprocess_token ->
               out:( context:context option ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method indicate_mechs :
          't . out:( mech_set:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method init_sec_context :
          't . initiator_cred:credential ->
               context:context option ->
               target_name:name ->
               mech_type:oid -> 
               req_flags:req_flag list ->
               time_rec:float option ->
               chan_bindings:channel_bindings option ->
               input_token:token option ->
               out:( actual_mech_type:oid ->
		     output_context:context option ->
		     output_token:token ->
		     ret_flags:ret_flag list ->
		     time_rec:[ `Indefinite | `This of float ] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't
    (** On the first call, pass [~context:None]. If successful, the
	function outputs a non-None [~output_context] which should be
	passed as new [~context] in follow-up calls.

	If the [output_token] is non-empty, it must be transmitted to
	the peer - independent of the [major_status].
     *)

  method inquire_context :
          't . context:context ->
               out:( src_name:name ->
                     targ_name:name ->
		     lifetime_req : [ `Indefinite | `This of float ] ->
		     mech_type:oid ->
		     ctx_flags:ret_flag list ->
		     locally_initiated:bool ->
		     is_open:bool ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_cred :
          't . cred:credential ->
               out:( name:name ->
		     lifetime: [ `Indefinite | `This of float ] ->
		     cred_usage:cred_usage ->
		     mechanisms:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_cred_by_mech :
          't . cred:credential ->
               mech_type:oid -> 
               out:( name:name ->
		     initiator_lifetime: [ `Indefinite | `This of float ] ->
		     acceptor_lifetime: [ `Indefinite | `This of float ] ->
		     cred_usage:cred_usage ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_mechs_for_name :
          't . name:name ->
               out:( mech_types:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_names_for_mech :
          't . mechanism:oid ->
               out:( name_types:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't


  method process_context_token :
          't . context:context ->
               token:token ->
               out:( minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method unwrap :
          't . context:context ->
               input_message:message ->
               output_message_preferred_type:[ `String | `Memory ] ->
               out:( output_message:message ->
		     conf_state:bool ->
		     qop_state:qop ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't
    (** Note that the [output_message] can be a buffer of different type
	(string vs. bigarray) than [input_message]. In 
	[output_message_preferred_type] the called may wish a certain
	representation. It is, however, not ensured that the wish is
	granted.
     *)

  method verify_mic :
          't . context:context ->
               message:message ->
               token:token ->
               out:( qop_state:qop ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method wrap :
          't . context:context ->
               conf_req:bool ->
               qop_req:qop option ->
               input_message:message ->
               output_message_preferred_type:[ `String | `Memory ] ->
               out:( conf_state:bool ->
		     output_message:message ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't
    (** [output_message_preferred_type]: see [unwrap] *)

  method wrap_size_limit :
          't . context:context ->
               conf_req:bool ->
               qop_req:qop option ->
               req_output_size:int ->
               out:( max_input_size:int ->
                     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't
end

(** {2 Utility functions} *)

(** These functions convert values to strings. Useful for generating
    log messages.
 *)

val string_of_calling_error : calling_error -> string
val string_of_routine_error : routine_error -> string
val string_of_suppl_status : suppl_status -> string
val string_of_major_status : major_status -> string


(** {2 Common OID's for name types} *)

(** See RFC 2078, section 4 *)

val nt_hostbased_service : oid
  (** names like "service@hostname" *)

val nt_user_name : oid
  (** names like "username" *)

val nt_machine_uid_name : oid
  (** user ID in host byte order *)
  
val nt_string_uid_name : oid
  (** user ID as string of digits *)

val nt_anonymous : oid
  (** anonymous name *)

val nt_export_name : oid
  (** an export name *)

val parse_hostbased_service : string -> string * string
  (** Returns ([service,host]) for "service@host". Fails if not parseable *)


(** {2 Encodings} *)

(** There is some chance that some of these routines will finally be moved to
    netstring
 *)

val oid_to_string : oid -> string
val string_to_oid : string -> oid
  (** Convert OID's to/from curly brace notation *)

val oid_to_der : oid -> string
val der_to_oid : string -> int ref -> oid
  (** Convert OID's to/from DER. [der_to_oid] takes a cursor as second arg.
   *)


val wire_encode_token : oid -> token -> string
val wire_decode_token : string -> int ref -> oid * token
  (** Encode tokens as described in section 3.1 of RFC 2078. This is usually
      only done for the initiating token.
   *)

val encode_exported_name : oid -> string -> string
val decode_exported_name : string -> int ref -> oid * string
  (** Encode names as described in section 3.2 of RFC 2078 *)

(** {2 Create tokens} *)

(** Format of the tokens: see RFC 4121 *)

val create_mic_token : sent_by_acceptor:bool ->
                       acceptor_subkey:bool ->
                       sequence_number:int64 ->
                       get_mic:(message -> string) ->
                       message:message ->
                         string
  (** Create a MIC token:

      - [sent_by_acceptor]: whether this token comes from the acceptor
      - [acceptor_subkey]: see RFC
      - [sequence_number]: a sequence number
      - [get_mic]: the checksum function
        (e.g. {!Netmech_scram.Cryptosystem.get_mic})
      - [message]: the message to be signed

      The function returns the MIC token
   *)

val parse_mic_token_header : string -> (bool * bool * int64)
  (** Returns the triple
      ([sent_by_acceptor], [acceptor_subkey], [sequence_number]) from
      the header of a MIC token that is passed to this function as
      string. Fails if not parsable 
   *)

val verify_mic_token : get_mic:(message -> string) -> 
                       message:message -> token:string -> bool
  (** Verifies the MIC [token] with [get_mic], and returns true if the
      verification is successful
   *)

val create_wrap_token_conf : sent_by_acceptor:bool ->
                             acceptor_subkey:bool ->
                             sequence_number:int64 ->
                             get_ec:(int -> int) ->
                             encrypt_and_sign:(message -> message) ->
                             message:message ->
                               message
  (** Wraps a [message] so that it is encrypted and signed (confidential).

      - [sent_by_acceptor]: whether this token comes from the acceptor
      - [acceptor_subkey]: see RFC
      - [sequence_number]: a sequence number
      - [get_ec]: This function returns the "extra count" number for
        the size of the plaintext w/o filler (e.g. use
        {!Netmech_scram.Cryptosystem.get_ec}).
      - [encrypt_and_sign]: the encryption function from the cryptosystem.
        The plaintext is passed to this function, and the ciphertext with
        the appended signature must be returned in the string.
      - [message]: the payload message

      The function returns the token wrapping the message.
   *)

val parse_wrap_token_header : 
      message -> (bool * bool * bool * int64)
  (** [let (sent_by_acceptor, sealed, acceptor_subkey, sequence_number) =
      parse_wrap_token_header token]

      Fails if the [token] cannot be parsed.
   *)


val unwrap_wrap_token_conf : decrypt_and_verify:(message -> message) ->
                             token:message ->
                               message
  (** Unwraps the [token] using the decryption function
      [decrypt_and_verify] from the cryptosystem.

      The functions fails if there is a format error, or the integrity
      check fails.

      Non-confidential messages cannot be unwrapped with this function.
   *)


(** Token functions for non-confidential messages are still missing *)
