(* $Id: rtypes.mli 1558 2011-03-04 17:15:46Z gerd $ *)

(** Binary encodings of numbers (Legacy) *)

(** Please use {!Netnumber} in new code - which is not restricted to
    big endian
 *)

(* 4- and 8-bytes representation of signed integers *)

type int4 = Netnumber.int4
  (** 32 bit signed integer *)

type int8 = Netnumber.int8
  (** 64 bit signed integer *)

(* 4- and 8-bytes representation of non-negative integers *)

type uint4 = Netnumber.uint4
  (** 32 bit unsigned integer *)

type uint8 = Netnumber.uint8
  (** 64 bit unsigned integer *)

(* Floating-point numbers of single and double precision according to IEEE *)

type fp4 = Netnumber.fp4
  (** single precision float *)

type fp8 = Netnumber.fp8
  (** double precision float *)

exception Cannot_represent of string
  (** raised if a conversion can't be done
      (same as {!Netnumber.Cannot_represent}) *)

exception Out_of_range
  (** raised if string position out of range
      (same as {!Netnumber.Out_of_range}) *)


val mk_int4 : char * char * char * char -> int4
val mk_int8 : char * char * char * char * char * char * char * char -> int8
val mk_uint4 : char * char * char * char -> uint4
val mk_uint8 : char * char * char * char * char * char * char * char -> uint8
  (** [mk_]<t> create integer values from character tuples. In these tuples
    * the MSB is the first component and the LSB the last.
   *)


(* destroy integers and get tuples *)

val dest_int4 : int4 -> char * char * char * char
val dest_int8 : int8 -> char * char * char * char * char * char * char * char
val dest_uint4 : uint4 -> char * char * char * char
val dest_uint8 : uint8 -> char * char * char * char * char * char * char * char
  (** [dest_]<t> destroy integer values and returns the corresponding char
    * tuples.
   *)


(* read integers from strings *)

val read_int4 : string -> int -> int4
val read_int8 : string -> int -> int8
val read_uint4 : string -> int -> uint4
val read_uint8 : string -> int -> uint8
  (** [read_]<t> create integer values from the characters found at a 
    * certain position in the string. Raises [Out_of_range] if the position
    * is bad. Network byte order is assumed.
   *)

val read_int4_unsafe : string -> int -> int4
val read_int8_unsafe : string -> int -> int8
val read_uint4_unsafe : string -> int -> uint4
val read_uint8_unsafe : string -> int -> uint8
  (** Same, but no index check *)

(* write integers into strings *)

(* these functions may raise Out_of_range if the position is out of the
 * allowed range of string positions
 *)

val write_int4 : string -> int -> int4 -> unit
val write_int8 : string -> int -> int8 -> unit
val write_uint4 : string -> int -> uint4 -> unit
val write_uint8 : string -> int -> uint8 -> unit
  (** [write_]<t> copies the characters corresponding to the integer values 
    * into the string at the given positions. Raises [Out_of_range] if the
    * position is bad. Network byte order is assumed.
   *)

(* these functions are unsafe and do not check the range *)

val write_int4_unsafe : string -> int -> int4 -> unit
val write_int8_unsafe : string -> int -> int8 -> unit
val write_uint4_unsafe : string -> int -> uint4 -> unit
val write_uint8_unsafe : string -> int -> uint8 -> unit
  (** [write_]<t>[_unsafe]: Same, but no index check. *)


(* integers as XDR compatible strings (i.e. big endian) *)

val int4_as_string : int4 -> string
val int8_as_string : int8 -> string
val uint4_as_string : uint4 -> string
val uint8_as_string : uint8 -> string
  (** <t>[_as_string]: Returns the corresponding string in network byte
    * order for an integer value
   *)

(** Conversions from int to (u)int and vice versa.
 * On 32-bit computers, the type [int] can hold 31-bit signed integers
 * (including the sign, i.e. one bit cannot be used).
 * On 64-bit computers, the type [int] can hold 63-bit signed integers
 * (including the sign, i.e. one bit cannot be used).
 * The [int_of_xxx] functions raise [Cannot_represent] if the number to
 * convert is too big (or too small) to be represented as [int]. Note
 * that this depends on the word size of your architecture.
 *)

val int_of_int4  : int4  -> int
val int_of_uint4 : uint4 -> int
val int_of_int8  : int8  -> int
val int_of_uint8 : uint8 -> int

val int4_of_int  : int -> int4
val uint4_of_int : int -> uint4
val int8_of_int  : int -> int8
val uint8_of_int : int -> uint8

(** Since O'Caml 3.00, there are the types [int32] and [int64] representing
 * 32-bit and 64-bit signed integers on every architecture.
 *)

val int32_of_int4  : int4  -> int32
val int32_of_uint4 : uint4 -> int32
val int32_of_int8  : int8  -> int32
val int32_of_uint8 : uint8 -> int32

val int4_of_int32  : int32 -> int4
val uint4_of_int32 : int32 -> uint4
val int8_of_int32  : int32 -> int8
val uint8_of_int32 : int32 -> uint8

val int64_of_int4  : int4  -> int64
val int64_of_uint4 : uint4 -> int64
val int64_of_int8  : int8  -> int64
val int64_of_uint8 : uint8 -> int64

val int4_of_int64  : int64 -> int4
val uint4_of_int64 : int64 -> uint4
val int8_of_int64  : int64 -> int8
val uint8_of_int64 : int64 -> uint8

(** Casts from [uint4]/[uint8] to [int32]/[int64]. Here, the sign is ignored and
 * simply considered as a bit.
 *)

val logical_uint4_of_int32 : int32 -> uint4
val logical_int32_of_uint4 : uint4 -> int32
val logical_uint8_of_int64 : int64 -> uint8
val logical_int64_of_uint8 : uint8 -> int64

(** Comparisons *)

val lt_uint4 : uint4 -> uint4 -> bool
  (** [lt_uint4] is true iff the first value is less than the second value 
      as unsigned int
   *)
val le_uint4 : uint4 -> uint4 -> bool
val gt_uint4 : uint4 -> uint4 -> bool
val ge_uint4 : uint4 -> uint4 -> bool
  (** Other comparisons *)


(** Floating-point stuff. The following functions all assume that the
 * system represents fp number in an IEEE-compliant way.
 *)

val fp8_of_fp4 : fp4 -> fp8
val fp4_of_fp8 : fp8 -> fp4
  (** Note [fp4_of_fp8]: This conversion is not exact. It is quite
   * normal that precision is lost. Numbers too small or too large
   * for fp4 are converted to the "infinity" value.
   *)

val float_of_fp4 : fp4 -> float
val float_of_fp8 : fp8 -> float
val fp4_of_float : float -> fp4
val fp8_of_float : float -> fp8

  (** Note fp4_of_float: The same problems as in fp4_of_fp8 may arise *)

(* [mk_fp4 e m2 m1 m0]:
 * e = sign + exponent;
 * m2, m1, m0 = normalized mantissa
 *
 * [mk_fp8 e1 e0m6 m5 m4 m3 m2 m1 m0]:
 * e1: sign + upper 7 bits of exponent
 * e0m6: lower 4 bits of exponent, highest 4 bits of mantissa
 * m5 to m0: lower bits of mantissa
 *)

(** Floating point to bit string and back: *)

val mk_fp4 : char * char * char * char -> fp4
val mk_fp8 : char * char * char * char * char * char * char * char -> fp8
val dest_fp4 : fp4 -> char * char * char * char
val dest_fp8 : fp8 -> char * char * char * char * char * char * char * char

val fp4_as_string : fp4 -> string
val fp8_as_string : fp8 -> string

val read_fp4 : string -> int -> fp4
val read_fp8 : string -> int -> fp8
