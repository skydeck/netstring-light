(* $Id: ssl_exts.mli 1745 2012-03-01 17:31:29Z gerd $ *)

(** A few extensions to the ocaml-ssl library [Ssl] *)

exception Shutdown_error of Ssl.ssl_error

val get_shutdown : Ssl.socket -> bool * bool
  (** Returns a pair [ (shutdown_received, shutdown_sent) ]. The first bool
    * is true if a "close notify" alert from the remote peer has been
    * received by th. The second bool is true if a "close notify" alert has
    * been sent.
   *)

val get_rbio_eof : Ssl.socket -> bool
  (** Returns whether EOF has been read by the rbio *)

val single_shutdown : Ssl.socket -> unit
  (** Corresponds to SSL_shutdown: Closes one half of the SSL connection. 
   * If called for the first time, a "close notify" alert is sent to the
   * peer. If called for the second time, it is waited until the
   * peer's "close notify" alert arrives.
   *
   * Use [get_shutdown] to check what [single_shutdown] will do.
   *)

val single_read : Ssl.socket -> string -> int -> int -> int
  (** Deprecated. Is now identical to Ssl.read, *)


val single_write : Ssl.socket -> string -> int -> int -> int
  (** Deprecated. Is now identical to Ssl.write *)


type ssl_mode =
    { enable_partial_write : bool;
      accept_moving_write_buffer : bool;
      auto_retry : bool;
    }

val get_mode : Ssl.socket -> ssl_mode
  (** Returns the current mode *)

val set_mode : Ssl.socket -> ssl_mode -> unit
  (** Sets additional mode bits. It is not possible to clear bits. *)

val certificate_fingerprint : Ssl.certificate -> string
  (** Returns the fingerprint in the cert in ':'-separated hex notation *)

