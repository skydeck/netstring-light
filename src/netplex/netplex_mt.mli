(* $Id: netplex_mt.mli 899 2006-06-04 12:13:10Z gerd $ *)

(** Multi-threading provider *)

class mt : unit -> Netplex_types.parallelizer
  (** Uses [Thread.create] to create new threads *)

val mt : unit -> Netplex_types.parallelizer
  (** Uses [Thread.create] to create new threads *)
