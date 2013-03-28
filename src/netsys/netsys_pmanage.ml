(* $Id: netsys_pmanage.ml 1774 2012-04-03 21:28:51Z gerd $ *)

type pm_obj =
    [ `File of string
    | `Posix_shm of string
    | `Posix_sem of string
    | `Sem_cont of string
    ]



class type pmanage =
object
  method register_file : string -> unit
  method unregister_file : string -> unit
  method register_posix_shm : string -> unit
  method unregister_posix_shm : string -> unit
  method register_posix_sem : string -> unit
  method unregister_posix_sem : string -> unit
  method register_sem_cont : string -> unit
  method unregister_sem_cont : string -> unit
  method registered : pm_obj list
  method register : pm_obj list -> unit
  method unlink : unit -> unit
end

module PmSet = Set.Make(struct type t = pm_obj let compare = compare end)


let unlink_this =
  function
    | `File p ->
        ( try Unix.unlink p with _ -> () )
    | `Posix_shm p ->
        ( try Netsys_posix.shm_unlink p with _ -> () )
    | `Posix_sem p ->
        ( try Netsys_posix.sem_unlink p with _ -> () )
    | `Sem_cont p ->
        ( try Netsys_sem.unlink p with _ -> () )


let mutex = !Netsys_oothr.provider # create_mutex()


let read_pm pm_path trunc_flag =
  let fd =
    Unix.openfile pm_path [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
  try
    Unix.lockf fd Unix.F_LOCK 0;
    let file = Unix.in_channel_of_descr fd in
    let set = ref PmSet.empty in
    ( try
        while true do
          let (op, res) =
            Scanf.fscanf file "%c %s %S\n"
              (fun op_s res_kind res_detail ->
                 let op =
                   match op_s with
                     | 'R' -> `Register
                     | 'U' -> `Unregister
                     | _ -> failwith "Netsys_pmanage.read_pm" in
                 let res =
                   match res_kind with
                     | "FILE" -> `File res_detail
                     | "POSIX_SHM" -> `Posix_shm res_detail
                     | "POSIX_SEM" -> `Posix_sem res_detail
                     | "SEM_CONT" -> `Sem_cont res_detail
                     | _ -> failwith "Netsys_pmanage.read_pm" in
                 (op, res)
              ) in
          match op with
            | `Register ->
                set := PmSet.add res !set
            | `Unregister ->
                set := PmSet.remove res !set
        done;
        assert false
      with
        | End_of_file ->
            if trunc_flag then Unix.ftruncate fd 0;
            Unix.close fd;
            !set
    )
  with
    | error ->
        Unix.close fd;
        raise error


let read_pm_mt pm_path =
  Netsys_oothr.serialize
    mutex
    read_pm pm_path


let write_pm pm_path op pm_obj_list =
  let fd =
    Unix.openfile pm_path [Unix.O_RDWR; Unix.O_CREAT; Unix.O_APPEND] 0o600 in
  try
    Unix.lockf fd Unix.F_LOCK 0;
    let file = Unix.out_channel_of_descr fd in
    let op_s =
      match op with
        | `Register -> "R"
        | `Unregister -> "U" in
    List.iter
      (fun pm_obj ->
         let res_kind, res_detail =
           match pm_obj with
             | `File p -> "FILE", p
             | `Posix_shm p -> "POSIX_SHM", p
             | `Posix_sem p -> "POSIX_SEM", p
             | `Sem_cont p -> "SEM_CONT", p in
         Printf.fprintf file "%s %s %S\n" op_s res_kind res_detail;
      )
      pm_obj_list;
    flush file;
    Unix.close fd
  with
    | error ->
        Unix.close fd;
        raise error

let write_pm_mt pm_path op pm_obj_list =
  Netsys_oothr.serialize
    mutex
    (write_pm pm_path op) pm_obj_list


let check p =
  if p = "" || p.[0] <> '/' then
    failwith "Netsys_pmanage: path must be absolute"


let pmanage pm_path : pmanage =
  let reg obj_l =
    write_pm_mt pm_path `Register obj_l in
  let unreg obj_l =
    write_pm_mt pm_path `Unregister obj_l in
object (self)
  method register_file p = 
    check p; reg [`File p]
  method unregister_file p = 
    check p; unreg [`File p]
  method register_posix_shm p = 
    check p; reg [`Posix_shm p]
  method unregister_posix_shm p = 
    check p; unreg [`Posix_shm p]
  method register_posix_sem p = 
    check p; reg [`Posix_sem p]
  method unregister_posix_sem p = 
    check p; unreg [`Posix_sem p]
  method register_sem_cont p = 
    check p; reg [`Sem_cont p]
  method unregister_sem_cont p = 
    check p; unreg [`Sem_cont p]
  method registered =
    let set = read_pm_mt pm_path false in
    PmSet.elements set
  method register l =
    reg l
  method unlink() =
    let set = read_pm_mt pm_path true in
    PmSet.iter unlink_this set
end


let fake_pmanage () : pmanage =
  let set = ref PmSet.empty in
  let reg obj_l =
    List.iter
      (fun obj -> set := PmSet.add obj !set)
      obj_l in
  let unreg obj_l =
    List.iter
      (fun obj -> set := PmSet.remove obj !set)
      obj_l in
object (self)
  method register_file p = 
    check p; reg [`File p]
  method unregister_file p = 
    check p; unreg [`File p]
  method register_posix_shm p = 
    check p; reg [`Posix_shm p]
  method unregister_posix_shm p = 
    check p; unreg [`Posix_shm p]
  method register_posix_sem p = 
    check p; reg [`Posix_sem p]
  method unregister_posix_sem p = 
    check p; unreg [`Posix_sem p]
  method register_sem_cont p = 
    check p; reg [`Sem_cont p]
  method unregister_sem_cont p = 
    check p; unreg [`Sem_cont p]
  method registered =
    PmSet.elements !set
  method register l =
    reg l
  method unlink() =
    PmSet.iter unlink_this !set;
    set := PmSet.empty
    
end
