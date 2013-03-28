(* $Id: netgzip.mli 1610 2011-05-30 08:03:45Z gerd $ *)

(** Gzip object channels *)

class input_gzip : Gzip.in_channel -> Netchannels.in_obj_channel
  (** A [Gzip.in_channel] turned into a {!Netchannels.in_obj_channel} *)

class output_gzip : Gzip.out_channel -> Netchannels.out_obj_channel
  (** A [Gzip.out_channel] turned into a {!Netchannels.out_obj_channel} *)

class inflating_pipe : unit -> Netchannels.io_obj_channel
  (** An inflating (uncompressing) pipe for gzip data, to be used in filters *)

class deflating_pipe : ?level:int -> unit -> Netchannels.io_obj_channel
  (** A deflating (compressing) pipe for gzip data, to be used in filters *)

class input_inflate : 
        Netchannels.in_obj_channel -> Netchannels.in_obj_channel
  (** [let ch' = new input_inflate ch]: Reading data from [ch'] inflates
      data read from [ch]. Use this for uncompressing data while reading.

      Note that [ch] isn't closed when [ch'] is closed.
   *)

class input_deflate :
        ?level:int ->
        Netchannels.in_obj_channel -> Netchannels.in_obj_channel
  (** [let ch' = new input_deflate ch]: Reading data from [ch'] deflates
      data read from [ch]. Use this for compressing data while reading.

      Note that [ch] isn't closed when [ch'] is closed.
   *)

class output_inflate :
         Netchannels.out_obj_channel -> Netchannels.out_obj_channel
  (** [let ch' = new output_inflate ch]: Data written to [ch'] is inflated
      and written to [ch]. Use this for uncompressing data while writing.

      Note that [ch] isn't closed when [ch'] is closed.
   *)

class output_deflate :
        ?level:int ->
         Netchannels.out_obj_channel -> Netchannels.out_obj_channel
  (** [let ch' = new output_deflate ch]: Data written to [ch'] is deflated
      and written to [ch]. Use this for compressing data while writing.

      Note that [ch] isn't closed when [ch'] is closed.
   *)

val init : unit -> unit
  (** By calling this function it is enforced that the (de)-compression
      algorithms are registered at {!Netcompression}.
   *)
