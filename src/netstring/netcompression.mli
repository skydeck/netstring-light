(* $Id: netcompression.mli 1610 2011-05-30 08:03:45Z gerd $ *)

(** Registry for compression algorithms *)

(** This registry is initially empty. The {!Netgzip} module can be used
    to register the [gzip] algorithm, just run

    {[ Netgzip.init() ]}

    to get this effect.
 *)

val register : iana_name:string ->
               ?encoder:(unit -> Netchannels.io_obj_channel) ->
               ?decoder:(unit -> Netchannels.io_obj_channel) ->
               unit -> unit
  (** Registers a compression algorithm. The algorithm is given as
      a pair of functions returning {!Netchannels.io_obj_channel}.
   *)

val lookup_encoder : iana_name:string -> unit -> Netchannels.io_obj_channel
  (** Returns the encoder, or raises [Not_found] *)

val lookup_decoder : iana_name:string -> unit -> Netchannels.io_obj_channel
  (** Returns the decoder, or raises [Not_found] *)

val all_encoders : unit -> string list
val all_decoders : unit -> string list
  (** The iana names of all encoders and decoders, resp. *)


