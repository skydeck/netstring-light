(* $Id: netplex_encap.ml 1415 2010-02-15 23:58:25Z gerd $ *)

exception Type_mismatch

type encap = int * string * Obj.t

module type TYPE = sig type t end

module type ENCAP = sig
  type t
  val wrap : t -> encap
  val unwrap : encap -> t
end


let next_id = ref 0

let query_seen = ref false

module Make_encap(T:TYPE) = struct
  type t = T.t

  let (id, verifier) =
    if !query_seen then
      failwith "Netplex_encap.Make_encap: too late for applying this functor";
    let id = !next_id in
    incr next_id;
    let t = Unix.gettimeofday() in
    let gc = Gc.quick_stat() in
    let verifier =
      Printf.sprintf "%d_%f_%f_%f_%f_%d_%d_%d_%d_%d_%d"
	id
	t
	gc.Gc.minor_words
	gc.Gc.promoted_words
	gc.Gc.major_words
	gc.Gc.minor_collections
	gc.Gc.major_collections
	gc.Gc.heap_words
	gc.Gc.heap_chunks
	gc.Gc.compactions
	gc.Gc.top_heap_words in
    (id, verifier)

  let wrap x =
    query_seen := true;
    (id, verifier, Obj.repr x)

  let unwrap (x_id, x_verifier, x_obj) =
    query_seen := true;
    if x_id <> id || x_verifier <> verifier then
      raise Type_mismatch;
    Obj.obj x_obj

end
