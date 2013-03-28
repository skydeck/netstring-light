(* $Id: netmcore_sem.ml 1755 2012-03-24 20:18:30Z gerd $ *)

open Netmcore_heap

type semaphore = Netmcore.res_id * string
    (* An empty string is a dummy sem.  Otherwise, the string is on
       the heap, and it contains the semaphore value *)

let dummy() = (`Resource(-1),"")

let create mut initval =
  let pool = Netmcore_heap.mut_pool mut in
  let c = Netmcore_heap.mut_sem_container mut in
  let s = String.create Netsys_sem.sem_size in
  let sem = (pool,s) in
  let sem_heap = add mut sem in
  let s_addr = Netsys_mem.obj_address (Obj.repr (snd sem_heap)) in
  let m = Netsys_mem.grab s_addr Netsys_sem.sem_size in
  ignore(Netsys_sem.sem_init c m 0 true initval);
  sem_heap

let get_sem sem_heap =
  let (pool, s_heap) = sem_heap in
  if s_heap = "" then
    failwith "Netmcore_sem: cannot access dummy semaphore";
  let s_addr = Netsys_mem.obj_address (Obj.repr s_heap) in
  let m = Netsys_mem.grab s_addr Netsys_sem.sem_size in
  let c = Netmcore_mempool.sem_container pool in
  Netsys_sem.as_sem c m 0

let destroy sem_heap =
  let (pool, s_heap) = sem_heap in
  if s_heap <> "" then (
    let sem = get_sem sem_heap in
    let c = Netmcore_mempool.sem_container pool in
    Netsys_sem.sem_destroy c sem
  )

let getvalue sem_heap =
  let sem = get_sem sem_heap in
  Netsys_sem.sem_getvalue sem

let post sem_heap =
  let sem = get_sem sem_heap in
  Netsys_sem.sem_post sem

let wait sem_heap swb =
  let sem = get_sem sem_heap in
  Netsys.restart (Netsys_sem.sem_wait sem) swb

