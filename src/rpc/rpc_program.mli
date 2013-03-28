(* $Id: rpc_program.mli 1251 2009-05-29 01:57:46Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** RPC programs *)

(** An RPC server offers its procedures as parts of a "program". Here,
 * the notion of programs is introduced.
 * A program is identified by a program number and a version number.
 * The service of a program is a set of procedures that are described
 * by procedure numbers and signatures. The signature of a procedure is a
 * pair (s,u) of XDR types that should be interpreted as function
 * s -> u.
 * To make work easier, programs are encapsulated as an opaque type t.
 * This data structure can store procedure names in addition to the plain
 * numbers, so you can refer to procedures by their names. But note that
 * these names are only local identifiers that are not transferred in the
 * RPC protocol. The signature can be formulated upon a type system
 * (a set of definitions of type names).
 *)

open Rtypes
open Xdr

type t
  (** Type of RPC programs *)

val create :
    uint4 ->                        (* which program *)
    uint4 ->                        (* which version *)
    xdr_type_system ->              (* the type system helping to formulate *)
    ( string * (uint4 * xdr_type_term * xdr_type_term)) list ->
                                      (* (procname, (procnr, input, output)) *)
    t
      (** [create program_nr version_nr type_system procedures] *)

val id : t -> int
      (** The ID identifies the program (used in {!Rpc_client}) *)


val update :
      ?program_number:uint4 ->
      ?version_number:uint4 ->
      t ->
	t
	(** Modifies program and/or version number. The modified program
            is returned. The program keeps its identity, i.e. the [id]
            function returns the same number.
	 *)



val program_number :
    t ->
      uint4
	(** Return the program number *)

val version_number :
    t ->
      uint4
	(** Return the version number *)

val null_proc_name :
    t ->
       string option
	 (** Returns the name of procedure 0 (or [None] if not found) *)

val signature :
    t ->
      string ->                         (* procedure name *)
	(uint4 * xdr_type * xdr_type)   (* procedure number,
				         * input type,
				         * output type
				         *)
	  (** [signature p name] returns the triple [ (proc_nr, in_t, out_t) ]
            * for the procedure [name]. [proc_nr] is the procedure number,
            * [in_t] the argument type and [out_t] the result type.
           *)

val procedure_number :
    t ->
      string ->
	uint4
	  (** [procedure_number p name] returns only the procedure number
            * of the procedure called [name].
           *)
