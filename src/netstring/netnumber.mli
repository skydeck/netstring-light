(* $Id: netnumber.mli 1701 2012-02-14 14:48:46Z gerd $ *)

(** Binary encodings of numbers *)

(** This is the successor of the {!Rtypes} module *)

(** {2 Numeric types} *)


(* 4- and 8-bytes representation of signed integers *)

type int4
  (** 32 bit signed integer *)

type int8
  (** 64 bit signed integer *)

(* 4- and 8-bytes representation of non-negative integers *)

type uint4
  (** 32 bit unsigned integer *)

type uint8
  (** 64 bit unsigned integer *)

(* Floating-point numbers of single and double precision according to IEEE *)

type fp4
  (** single precision float (IEEE "float") *)

type fp8
  (** double precision float (IEEE "double") *)

exception Cannot_represent of string
  (** raised if a conversion can't be done *)

exception Out_of_range
  (** raised if string position out of range *)



(** {2 Basic encoding/decoding functions} *)

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

val mk_fp4 : char * char * char * char -> fp4
val mk_fp8 : char * char * char * char * char * char * char * char -> fp8
val dest_fp4 : fp4 -> char * char * char * char
val dest_fp8 : fp8 -> char * char * char * char * char * char * char * char


(** {2 Conversions} *)

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

(** {2 Comparisons} *)

(** The comparisons "=" and "<>" work for all numbers.

    For signed integers, the operators "<", "<=", ">", and ">=" work, too.
    The unsigned integer type use representation that are not compatible
    with these operators, and the following functions need to be called.
    
    For [fp4] and [fp8] there are no comparison functions - convert to
    [float] first and compare then.
 *)

val lt_uint4 : uint4 -> uint4 -> bool
  (** [lt_uint4] is true iff the first value is less than the second value 
      as unsigned int
   *)
val le_uint4 : uint4 -> uint4 -> bool
val gt_uint4 : uint4 -> uint4 -> bool
val ge_uint4 : uint4 -> uint4 -> bool
  (** Other comparisons *)

val lt_uint8 : uint8 -> uint8 -> bool
  (** [lt_uint8] is true iff the first value is less than the second value 
      as unsigned int
   *)
val le_uint8 : uint8 -> uint8 -> bool
val gt_uint8 : uint8 -> uint8 -> bool
val ge_uint8 : uint8 -> uint8 -> bool
  (** Other comparisons *)


(** {2 Minimum/maximum values} *)

val min_int4 : int4
val min_uint4 : uint4
val min_int8 : int8
val min_uint8 : uint8

val max_int4 : int4
val max_uint4 : uint4
val max_int8 : int8
val max_uint8 : uint8


module type ENCDEC = sig
  (** Encode/decode numbers as strings. These functions exist in two
      flavors:
      - {!Netnumber.BE} implements network byte order (big endian)
      - {!Netnumber.LE} implements little endian
   *)

  val read_int4 : string -> int -> int4
  val read_int8 : string -> int -> int8
  val read_uint4 : string -> int -> uint4
  val read_uint8 : string -> int -> uint8
    (** [read_]<t> create integer values from the characters found at a
	certain position in the string. Raises [Out_of_range] if the position
	is bad
     *)

  val read_int4_unsafe : string -> int -> int4
  val read_int8_unsafe : string -> int -> int8
  val read_uint4_unsafe : string -> int -> uint4
  val read_uint8_unsafe : string -> int -> uint8
    (** Same, but no index check *)

  val write_int4 : string -> int -> int4 -> unit
  val write_int8 : string -> int -> int8 -> unit
  val write_uint4 : string -> int -> uint4 -> unit
  val write_uint8 : string -> int -> uint8 -> unit
    (** [write_]<t> copies the characters corresponding to the integer values 
	into the string at the given positions. Raises [Out_of_range] if the
	position is bad. 
     *)

  val write_int4_unsafe : string -> int -> int4 -> unit
  val write_int8_unsafe : string -> int -> int8 -> unit
  val write_uint4_unsafe : string -> int -> uint4 -> unit
  val write_uint8_unsafe : string -> int -> uint8 -> unit
    (** [write_]<t>[_unsafe]: Same, but no index check. *)


  val int4_as_string : int4 -> string
  val int8_as_string : int8 -> string
  val uint4_as_string : uint4 -> string
  val uint8_as_string : uint8 -> string
    (** <t>[_as_string]: Returns the corresponding string for an integer value
     *)

  val write_fp4 : string -> int -> fp4 -> unit
  val write_fp8 : string -> int -> fp8 -> unit

  val fp4_as_string : fp4 -> string
  val fp8_as_string : fp8 -> string
    
  val read_fp4 : string -> int -> fp4
  val read_fp8 : string -> int -> fp8
    
end


module BE : ENCDEC
  (** Encoders/decoders for big endian - network byte order *)

module LE : ENCDEC
  (** Encoders/decoders for little endian *)

module HO : ENCDEC
  (** Encoders/decoders for host byte order - which is either little
      endian or big endian, depending on the CPU (or CPU mode)
   *)
