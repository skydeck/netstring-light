(* $Id: netauth.mli 1543 2011-02-08 02:08:35Z gerd $ *)

(** Some primitives for authentication *)

val hmac : h:(string->string) ->
           b:int ->
           l:int ->
           k:string ->
           message:string ->
             string
  (** The HMAC algorithm of RFC 2104. The function [h] is the hash function.
      [b] and [l] are properties of [h] (see the RFC or below). The string
      [k] is the key, up to [b] bytes. The [message] is authenticated.

      The key [k] should ideally have length [l]. If this cannot be ensured
      by other means, one should pass [k = h any_k].

      Common values of [b] and [l]:
      - For [h=MD5]: [b=64], [l=16]
      - For [h=SHA-1]: [b=64], [l=20]
   *)

type key_type =
    [ `Kc | `Ke | `Ki ]
  (** Key types:
      - [`Kc] is used for computing checksums
      - [`Ke] is used for encrypting confidential messages
      - [`Ki] is used for computing integrity checksums for encrypted
         messages
   *)

val derive_key_rfc3961_simplified : 
      encrypt:(string -> string) ->
      random_to_key:(string -> string) ->
      block_size:int ->
      k:int ->
      usage:int ->
      key_type:key_type ->
        string
  (** Derives a special key from a base key, as described in RFC 3961.
      
      - [encrypt]: Encrypts the argument with the base key and the
        initial cipher state.
      - [random_to_key]: Converts a random string of size [k] to a key
      - [block_size]: The block size of the cipher underlying [encrypt].
        It is ensured that [encrypt] is called with strings having exactly
        this many bits. (The [c] parameter in the RFC text.) Minimum: 40.
      - [k]: The input size for [random_to_key] in bits. Must be divisible
        by 8.
      - [usage]: The usage number (here restricted to 0-255, although the
        RFC would allow 32 bits). Examples for usage numbers can be found
        in RFC 4121 section 2.
      - [key_type]: Which key type to derive

      The output is a key as produced by [random_to_key].
   *)      


(** {2 Bitstring operations} *)

val xor_s : string -> string -> string
  (** Performs the bitwise XOR of these strings (which must have the same
      length)
   *)

val add_1_complement : string -> string -> string
  (** The addition algorithm for 1's-complement numbers. The two numbers to
      add are given as bitstrings (big endian), and must have the same
      length
   *)

val rotate_right : int -> string -> string
  (** Rotate the (big-endian) bitstring to the right by n bits. This also
      works for negative n (left rotation), and for n whose absolute value
      is greater or equal than the bit length of the string.
   *)

val n_fold : int -> string -> string
  (** Blumenthal's n-fold algorithm for an n that is divisible by 8.
      (RFC 3961, section 5.1)
   *)
