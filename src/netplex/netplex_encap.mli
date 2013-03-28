(* $Id: netplex_encap.mli 1416 2010-02-16 00:25:07Z gerd $ *)

(** Type-safe marshalling between processes of the same executable *)

(** This is a pragmatic approach to type-safe marshalling. We define
    [type encap] as an arbitrary value that carries a type identifier with it.
    The type identifier is generated when the functor
    {!Netplex_encap.Make_encap} is applied. Every instantiation of this
    functor generates a new type identifier.

    The idea is then that an [encap] value can be marshalled to another
    process using [Marshal], and when it is unwrapped the type identifier
    is checked. Unwrapping is only successful when the [unwrap] function
    from the same functor instantiation is used as the [wrap]
    function.

    This module is incompatible with:
     - Marshalling to processes running a different executable
     - Marshalling to processes that are dynamically loading 
       modules
     - The functor must be instantiated at program initialization time.
       Especially this must not happen in [let module] expressions.

    Only some of these assumptions can be checked at runtime by this
    implementation.
 *)

exception Type_mismatch
  (** Raised when an encapulated type is tried to be unwrapped by the
      wrong encapsulation module
   *)

type encap
  (** An encapsulated value with a type identifier *)

module type TYPE = sig type t end
  (** Just a (monomorphic) type [t] *)

module type ENCAP = sig
  type t
  val wrap : t -> encap
  val unwrap : encap -> t
    (** Raises {!Netplex_encap.Type_mismatch} if the value does not fit *)
end

module Make_encap(T:TYPE) : ENCAP with type t = T.t
  (** Create an encapsulation module *)
