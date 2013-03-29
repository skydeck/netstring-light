(** Base64, Quoted Printable, URL encoding, HTML escaping *)

(* *********************************************************************)
(* Several encodings important for the net                             *)
(* *********************************************************************)


(* *********************************************************************)
(* Base 64 encoding                                                    *)
(* *********************************************************************)

(* See RFC 2045 for a description of Base 64 encoding. *)

(* THREAD-SAFETY:
 * All Base64 functions are reentrant and thus thread-safe.
 *)

module Base64 : sig

  (** Base64 encoding as described in RFC 2045 *)

  val encode : ?pos:int -> ?len:int -> ?linelength:int -> ?crlf:bool ->
               string -> string
      (** Compute the "base 64" encoding of the given string argument.
       * Note that the result is a string that only contains the characters
       * a-z, A-Z, 0-9, +, /, =, and optionally spaces, CR and LF characters.
       *
       * If [pos] and/or [len] are passed, only the substring starting at
       * [pos] (default: 0) with length [len] (default: rest of the string)
       * is encoded.
       *
       * The result is divided up into lines not longer than [linelength]
       * (without counting the line separator); default: do not divide lines.
       * If [linelength] is smaller than 4, no line division is performed.
       * If [linelength] is not divisible by 4, the produced lines are a
       * bit shorter than [linelength].
       *
       * If [crlf] (default: false) the lines are ended by CRLF; otherwise
       * they are only ended by LF.
       * (You need the crlf option to produce correct MIME messages.)
       *
       *)

  val decode : ?pos:int -> ?len:int -> ?url_variant:bool ->
               ?accept_spaces:bool -> string -> string
      (** Decodes the given string argument.
       *
       * If [pos] and/or [len] are passed, only the substring starting at
       * [pos] (default: 0) with length [len] (default: rest of the string)
       * is decoded.
       *
       * If [url_variant] (default: [true]) is set, the functions also
       * accepts the characters '-' and '.' as produced by [url_encode].
       *
       * If [accept_spaces] (default: [false]) is set, the function ignores
       * white space contained in the string to decode (otherwise the
       * function fails if it finds white space). Furthermore, the character
       * '>' is considered as "space", too (so you don't have trouble with
       * mbox mailboxes that accidentally quote "From").
       *)
end

(* *********************************************************************)
(* Quoted printable encoding                                           *)
(* *********************************************************************)

(* THREAD-SAFETY:
 * All QuotedPrintable functions are reentrant and thus thread-safe.
 *)

module QuotedPrintable :
  sig
    (** This module implements the "Quoted Printable" encoding as
     * described in RFC 2045.
     *
     * This implementation assumes that the encoded string has a text MIME
     * type. On input both CR/LF and LF are accepted as end-of-line (eol) terminators,
     * but the output normalizes the eol delimiter as the [crlf] argument
     * specifies. Note that this implies that
     * - If [crlf], the output uses CR/LF as line separator as MIME prescribes
     * - the encoding is not invertible for binary data
     *)

    val encode : ?crlf:bool -> ?pos:int -> ?len:int -> string -> string
	(** Encodes the string and returns it.
	 *
	 * Since OcamlNet 0.98, soft line breaks are added to the output
	 * to ensure that all output lines have a length <= 76 bytes.
	 *
	 * Note unsafe characters:
	 *   As recommended by RFC 2045, the characters [!#$\@[]^`{|}~]
	 *   and the double quotes
	 *   are additionally represented as hex tokens.
	 *   Furthermore, the letter 'F' is considered as unsafe if it
	 *   occurs at the beginning of the line, so the encoded text
	 *   never contains the word "From" at the beginning of a line.
	 *
	 * If [pos] and/or [len] are passed, only the substring starting at
	 * [pos] (default: 0) with length [len] (default: rest of the string)
	 * is encoded.
	 *
          * If [crlf] is set (the default), the output text uses CR/LF as
	 * line separator. Otherwise only LF is used.
	 *)

    val decode : ?pos:int -> ?len:int -> string -> string
	(** Decodes the string and returns it.
	 *
	 * Most format errors cause an [Invalid_argument] exception.
	 *
	 * If [pos] and/or [len] are passed, only the substring starting at
	 * [pos] (default: 0) with length [len] (default: rest of the string)
	 * is decoded.
	 *)

  end

(* *********************************************************************)
(* Q encoding                                                          *)
(* *********************************************************************)

(* See RFC 2047.
 * The functions behave similar to those of QuotedPrintable. 
 *)

(* THREAD-SAFETY:
 * All Q functions are reentrant and thus thread-safe.
 *)

module Q :
  sig
    (** The "Q" encoding as described by RFC 2047. *)

    val encode : ?pos:int -> ?len:int -> string -> string
	(** Note:
	 * All characters except alphanumeric characters are protected by
	 * hex tokens.
	 * In particular, spaces are represented as "=20", not as "_".
	 *)

    val decode : ?pos:int -> ?len:int -> string -> string

  end

(* *********************************************************************)
(* B encoding                                                          *)
(* *********************************************************************)

(* The B encoding of RFC 2047 is the same as Base64. *)


(* *********************************************************************)
(* URL-encoding                                                        *)
(* *********************************************************************)


(* THREAD-SAFETY:
 * The Url functions are thread-safe.
 *)

module Url :
  sig
    (** Encoding/Decoding within URLs:
     *
     * The following two functions perform the '%'-substitution for
     * characters that may otherwise be interpreted as metacharacters.
     *
     * According to: RFC 1738, RFC 1630
     *
     * Option [plus]: This option has been added because there are some
     * implementations that do not map ' ' to '+', for example Javascript's
     * [escape] function. The default is [true] because this is the RFC-
     * compliant definition.
     *)


    val decode : ?plus:bool -> ?pos:int -> ?len:int -> string -> string
	(** Option [plus]: Whether '+' is converted to space. The default
	 * is true. If false, '+' is returned as it is.
         *
         * The optional arguments [pos] and [len] may restrict the string
         * to process to this substring.
	 *)
    val encode : ?plus:bool -> string -> string
	(** Option [plus]: Whether spaces are converted to '+'. The default
	 * is true. If false, spaces are converted to "%20", and
	 * only %xx sequences are produced.
	 *)

    (** URL-encoded parameters:
     *
     * The following two functions create and analyze URL-encoded parameters.
     * Format: [name1=val1&name2=val2&...]
     *)

    val mk_url_encoded_parameters : (string * string) list -> string
       (** The argument is a list of (name,value) pairs. The result is the
        * single URL-encoded parameter string.
        *)

    val dest_url_encoded_parameters : string -> (string * string) list
       (** The argument is the URL-encoded parameter string. The result is
        * the corresponding list of (name,value) pairs.
        * Note: Whitespace within the parameter string is ignored.
        * If there is a format error, the function fails.
        *)
  end
