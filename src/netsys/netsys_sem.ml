(* $Id: netsys_sem.ml 1805 2012-10-19 23:30:09Z gerd $ *)

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netsys_sem" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netsys_sem" Debug.enable

let () =
  Netlog.Debug.register_module "Netsys_sem" Debug.enable

open Printf


type sem_open_flag = Netsys_posix.sem_open_flag =
  | SEM_O_CREAT
  | SEM_O_EXCL

type sem_wait_behavior = Netsys_posix.sem_wait_behavior =
  | SEM_WAIT_BLOCK
  | SEM_WAIT_NONBLOCK

type prefix = string


module Emu = struct
  let n = 16384
    (* We support at most this number of semaphores per container.
       This number can be increased to at most 65535 without changing
       the code.
     *)

  let n_active = Netsys_posix.sysconf_open_max() / 4
    (* max number of open semaphores (normally, each open sem consumes
       a file descr). It is tried not to exceed this value, but in some
       situations this will nevertheless occur.
     *)

  (* Note that there is a basic problem with [mutex]: If the process dies
     while the mutex is in locked state, no other process will ever
     again get the lock. TODO: We could protect against this by recording
     the PID of the lock holder.
   *)

  type anon_semaphore =
      { mutable sem : Netsys_posix.named_semaphore option;
        mutable use_count : int;
	num : int;                              (* number in the container *)
        mutable freshness : int;                (* freshness for lru cache *)
        cont_id : int;                   (* Container ID for cross-lookups *)
      }
      (* The active version of a semaphore. There is only one instance 
         of this record per process and semaphore. [sem] is set to the
         backing named semaphore. [use_counts] counts the number of active uses
         this process (values > 1 are possible in multithreaded programs),
         such as ongoing semaphore operations. When [use_count > 0], the
         semaphore cannot be closed.

         [num] is the number of this semaphore in the container. This means,
         [cont.used.{num} = true].

         The value [freshness] is set to a new maximum whenever the semaphore
         is used.
       *)
         
  type container =
      { prefix : string;
        id : int;
	used : (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t;
	active : (int, anon_semaphore) Hashtbl.t;
	mutex : Netsys_posix.named_semaphore;  (* protects only [used] *)
        mutable open_sems : int;
        mutable maxfresh : int;
      }
      (* The container. There is only one instance of this record per process
         and [prefix]. This instance also gets a unique [id].

         The possible semaphores in the container have numbers 0 to n-1,
         where n is the size of the [used] array. When a number is allocated,
         the [used] array is set to [true] at the corresponding index.

         If a semaphore is opened, it is put into [active]. If a semaphore
         is closed, it is not removed from [active], however. This first
         happens when it is destroyed. The counter [open_sems] contains the
         number of open semaphores in [active] (with [sem<>None]).
       *)

  (* Forks: the construction needs to be compatible with fork(), at least for
     single-threaded apps. Both memory mappings ([used]) and named semaphores
     survive forks, so there should not be any problem.

     In mt apps: there are two problems:
     - [use_count] would have to be reset to 0 after a fork
     - [proc_mutex] would have to be reinitialized

     We do not attempt here to solve these problems.
   *)
     


  let proc_mutex = !Netsys_oothr.provider # create_mutex()

  let cont_by_id = Hashtbl.create 7
  let cont_by_prefix = Hashtbl.create 7

  let sem_name cont k =
    cont.prefix ^ "_sem" ^ string_of_int k


  let lookup_cont_by_id =
    Netsys_oothr.serialize 
      proc_mutex
      (fun id ->
         Hashtbl.find cont_by_id id
      )

  let lookup_cont_by_prefix =
    Netsys_oothr.serialize 
      proc_mutex
      (fun p ->
         Hashtbl.find cont_by_prefix p
      )

  let add_cont =
    Netsys_oothr.serialize 
      proc_mutex
      (fun c ->
         Hashtbl.replace cont_by_id c.id c;
         Hashtbl.replace cont_by_prefix c.prefix c
      )

  let del_cont =
    Netsys_oothr.serialize 
      proc_mutex
      (fun c ->
         Hashtbl.remove cont_by_id c.id;
         Hashtbl.remove cont_by_prefix c.prefix
      )

  let lookup_or_add_cont f =
    Netsys_oothr.serialize 
      proc_mutex
      (fun p ->
         try
           Hashtbl.find cont_by_prefix p
         with
           | Not_found ->
               let c = f p in
               Hashtbl.replace cont_by_id c.id c;
               Hashtbl.replace cont_by_prefix c.prefix c;
               c
      )

  let freshest =
    Netsys_oothr.serialize 
      proc_mutex
      (fun c ->
         let fn = c.maxfresh+1 in
         c.maxfresh <- fn;
         fn
      )

  let mod_use_count d =
    Netsys_oothr.serialize 
      proc_mutex
      (fun usem ->
         usem.use_count <- usem.use_count + d
      )


  let have_anon_semaphores() =
    Netsys_posix.have_named_posix_semaphores()

  let prefix cont = cont.prefix

  let load_container prefix =
    let fd = 
      Netsys_posix.shm_open
	(prefix ^ "_sems")
	[ Netsys_posix.SHM_O_RDWR;
	  Netsys_posix.SHM_O_CREAT
	]
	0o600 in
    let fd_open = ref true in
    ( try
	let st = Unix.fstat fd in
	if st.Unix.st_size = 0 then
	  Unix.ftruncate fd (8 * n);
	let used = 
	  Netsys_mem.memory_map_file fd true n in
	Unix.close fd;
        fd_open := false;
        let mutex =
          Netsys_posix.sem_open
	    (prefix ^ "_contsem")
	    [ Netsys_posix.SEM_O_CREAT ]
	    0o600
	    1 in
        dlogr (fun () ->
                 sprintf "Opened container prefix=%s" prefix
              );
	{ prefix = prefix;
	  used = used;
	  active = Hashtbl.create 47;
	  mutex = mutex;
          open_sems = 0;
          maxfresh = 0;
          id = Oo.id (object end);
	}
      with error ->
        if !fd_open then 
	  Unix.close fd;
	raise error
    )

  let container prefix =
    lookup_or_add_cont
      load_container
      prefix

  let lock cont =
    dlogr (fun () -> sprintf "lock waiting prefix=%s" cont.prefix);
    Netsys_posix.sem_wait cont.mutex Netsys_posix.SEM_WAIT_BLOCK;
    dlogr (fun () -> sprintf "lock acquired prefix=%s" cont.prefix)

  let unlock cont =
    dlogr (fun () -> sprintf "unlock prefix=%s" cont.prefix);
    Netsys_posix.sem_post cont.mutex

  let unlink prefix =
    (* A radical way to get rid of all persistent objects, without acquiring
       mutex
     *)
    dlogr (fun () -> sprintf "unlink prefix=%s" prefix);
    let attempt f arg =
      try f arg
      with _ -> () in
    attempt Netsys_posix.shm_unlink (prefix ^ "_sems");
    attempt Netsys_posix.sem_unlink (prefix ^ "_contsem");
    for k = 0 to n-1 do
      attempt Netsys_posix.sem_unlink (prefix ^ "_sem" ^ string_of_int k)
    done

  let create_container prefix =
    unlink prefix;
    container prefix

  let drop_active_lk cont =
    (* While locked: find the sem with the lowest freshness and close it.
       Consider only sems without users.
     *)
    let old_open_sems = cont.open_sems in
    ( try
        while cont.open_sems+1 > n_active do
          let usem_opt =
            Hashtbl.fold
              (fun _ usem best_usem_opt ->
                 if usem.use_count = 0 && usem.sem <> None then (
                   match best_usem_opt with
                     | None -> (Some usem)
                     | Some b ->
                         if usem.freshness < b.freshness then
                           Some usem
                         else
                           best_usem_opt
                 )
                 else
                   best_usem_opt
              )
              cont.active
              None in
          match usem_opt with
            | None -> raise Not_found (* leave loop *)
            | Some usem ->
                ( match usem.sem with
                    | None -> assert false
                    | Some sem ->
                        dlogr
                          (fun () ->
                             sprintf "drop_active: closing prefix=%s k=%d"
                               cont.prefix usem.num);
                        Netsys_posix.sem_close sem;
                        usem.sem <- None;
                        cont.open_sems <- cont.open_sems - 1;
                )
        done
      with
        | Not_found -> ()
    );
    dlogr
      (fun () ->
         sprintf "drop_active: old_open=%d now_open=%d"
           old_open_sems cont.open_sems
      )

  let drop_active cont =
    Netsys_oothr.serialize 
      proc_mutex
      (fun () -> drop_active_lk cont)
      ()

  let use_sem cont =
    Netsys_oothr.serialize 
      proc_mutex
      (fun usem ->
         match usem.sem with
           | None ->
               dlogr
                 (fun () ->
                    sprintf "Reloading sem prefix=%s k=%d"
                      cont.prefix usem.num
                 );
               if cont.open_sems >= n_active then
                 drop_active cont;
	       let sem =
	         Netsys_posix.sem_open
	           (sem_name cont usem.num)
	           []
	           0
	           0 in
               usem.sem <- Some sem;
               cont.open_sems <- cont.open_sems + 1;
               usem.use_count <- usem.use_count + 1;
               sem
           | Some sem ->
               usem.use_count <- usem.use_count + 1;
               sem
      )    
      

  let lookup cont k =
    (* Look the existing semaphore #k up. ENOENT if not found *)
    let fn = freshest cont in
    Netsys_oothr.serialize 
      proc_mutex
      (fun () ->
         try
           try 
             let usem = Hashtbl.find cont.active k in
             dlogr (fun () -> 
                      sprintf "lookup prefix=%s k=%d active" cont.prefix k);
             (* Might still be closed! *)
             usem
           with Not_found ->
             if cont.open_sems >= n_active then
               drop_active_lk cont;
	     let sem =
	       Netsys_posix.sem_open
	         (sem_name cont k)
	         []
	         0
	         0 in
	     let usem =
	       { sem = Some sem;
	         num = k;
                 freshness = fn;
                 cont_id = cont.id;
                 use_count = 0;
	       } in
             dlogr (fun () -> 
                      sprintf "lookup prefix=%s k=%d opened" cont.prefix k);
	     Hashtbl.add cont.active k usem;
             cont.open_sems <- cont.open_sems + 1;
	     usem
         with error ->
           dlogr (fun () -> 
                    sprintf "lookup prefix=%s k=%d error=%s"
                      cont.prefix k (Netexn.to_string error));
           raise error
      )
      ()


  let find_unused cont =
    (* Find a free k, and mark it as used *)
    lock cont;
    let k = ref 0 in
    while !k < n && cont.used.{ !k } = '\001' do
      incr k
    done;
    if !k < n then (
      cont.used.{ !k } <- '\001';
      dlogr (fun () -> 
               sprintf "find_unused prefix=%s k=%d" cont.prefix !k);
      unlock cont;
      !k
    )
    else (
      unlock cont;
      raise(Unix.Unix_error(Unix.ENOMEM,
			    "Netsys_shm.find_unused (too many semaphores)",
			    ""))
    )

  let mark_unused cont k =
    cont.used.{ k } <- '\000'


  let as_sem cont mem pos =
    let k =
      Char.code mem.{ pos } + 256 * Char.code mem.{ pos+1 } in
    dlogr (fun () -> 
             sprintf "as_sem prefix=%s k=%d" cont.prefix k);
    lookup cont k

  let sem_init cont mem pos pshared init_value =
    let k = find_unused cont in
    let fn = freshest cont in
    Netsys_oothr.serialize 
      proc_mutex
      (fun () ->
         (* First check whether k is still in [active]. This can happen when
            another process destroyed the semaphore, but this process did not
            see it yet. We don't destroy it here again (too error-prone).
          *)
         ( try
             let usem = Hashtbl.find cont.active k in
             if usem.sem <> None then
               cont.open_sems <- cont.open_sems - 1;
             Hashtbl.remove cont.active k
           with Not_found -> ()
         );
         try
           if cont.open_sems >= n_active then
             drop_active_lk cont;
           let sem =
             Netsys_posix.sem_open
	       (sem_name cont k)
	       [ Netsys_posix.SEM_O_CREAT; Netsys_posix.SEM_O_EXCL ]
	       0o600
	       init_value in
           let usem =
             { sem = Some sem;
	       num = k;
               freshness = fn;
               cont_id = cont.id;
               use_count = 0;
             } in
           Hashtbl.replace cont.active k usem;
           cont.open_sems <- cont.open_sems + 1;
           mem.{ pos } <- Char.chr (k land 0xff);
           mem.{ pos+1 } <- Char.chr (k lsr 8);
           dlogr (fun () -> 
                    sprintf "sem_init prefix=%s k=%d" cont.prefix k);
           usem
         with
           | error ->
               dlogr (fun () -> 
                        sprintf "sem_init prefix=%s k=%d error=%s"
                          cont.prefix k (Netexn.to_string error));
               raise error
      )
      ()

  let sem_idestroy cont k =
    ( try
        let usem = Hashtbl.find cont.active k in
        if usem.sem <> None then
          cont.open_sems <- cont.open_sems - 1;
        Hashtbl.remove cont.active k;
      with Not_found -> ()
    );
    ( try
	Netsys_posix.sem_unlink
	  (sem_name cont k)
      with _ -> ()
    )

  let sem_release cont k =
    lock cont;
    mark_unused cont k;
    unlock cont

  let sem_destroy cont =
    Netsys_oothr.serialize 
      proc_mutex
      (fun usem ->
         let w = usem.use_count in
         dlogr (fun () -> 
                  sprintf "sem_destroy prefix=%s k=%d use_count=%d" 
                    cont.prefix usem.num w);
         if w > 0 then
           failwith "Netsys_sem.sem_destroy: there are still threads in sem_wait";
         ( match usem.sem with
             | None -> ()
             | Some sem ->
                 Netsys_posix.sem_close sem;
                 usem.sem <- None
         );
         sem_idestroy cont usem.num;
         sem_release cont usem.num;
      )

  let drop_container cont =
    dlogr (fun () -> 
             sprintf "drop_container start prefix=%s" cont.prefix);
    Netsys_oothr.serialize 
      proc_mutex
      (fun () ->
         lock cont;
         for k = 0 to n-1 do
           if cont.used.{k} = '\001' then
	     sem_idestroy cont k
         done;
         ( try
	     Netsys_posix.shm_unlink (cont.prefix ^ "_sems")
           with _ -> ()
         );
         ( try
	     Netsys_posix.sem_unlink (cont.prefix ^ "_contsem")
           with _ -> ()
         );
         del_cont cont;  (* remove from hash tables *)
         dlogr (fun () -> 
                  sprintf "drop_container finish prefix=%s" cont.prefix);
         unlock cont
      )
      ()


  let get_sem usem =
    let cont = 
      try
        lookup_cont_by_id usem.cont_id
      with
        | Not_found -> assert false in
    let fn = freshest cont in
    usem.freshness <- fn;
    use_sem cont usem   (* also increases use_count by 1 *)

  let sem_getvalue usem =
    let sem = get_sem usem in
    try
      let v = Netsys_posix.sem_getvalue sem in
      mod_use_count (-1) usem;
      v
    with error ->
      mod_use_count (-1) usem;
      raise error

  let sem_post usem =
    dlogr (fun () -> 
             sprintf "sem_post k=%d" usem.num);
    let sem = get_sem usem in
    try
      Netsys_posix.sem_post sem;
      mod_use_count (-1) usem;
    with error ->
      mod_use_count (-1) usem;
      raise error

  let sem_wait usem wb =
    dlogr (fun () -> 
             sprintf "sem_wait waiting k=%d" usem.num);
    let sem = get_sem usem in
    try
      Netsys_posix.sem_wait sem wb;
      mod_use_count (-1) usem;
      dlogr (fun () -> 
               sprintf "sem_wait acquired k=%d" usem.num)
    with
      | error ->
          mod_use_count (-1) usem;
          dlogr (fun () -> 
                   sprintf "sem_wait exn=%s k=%d"
                     (Netexn.to_string error) usem.num);
          raise error
end


module Native = struct
  type container = string (* the prefix *)

  type anon_semaphore =
      Netsys_posix.anon_semaphore

  let have_anon_semaphores() =
    Netsys_posix.have_anon_posix_semaphores()

  let container p = p

  let create_container p = p

  let drop_container _ = ()

  let unlink _ = ()

  let prefix p = p

  let sem_init _ = 
    Netsys_posix.sem_init

  let sem_destroy _ =
    Netsys_posix.sem_destroy

  let as_sem _ =
    Netsys_posix.as_sem

  let sem_getvalue =
    Netsys_posix.sem_getvalue

  let sem_post =
    Netsys_posix.sem_post

  let sem_wait =
    Netsys_posix.sem_wait

end


type container =
  [ `E of Emu.container
  | `N of Native.container
  ]

type anon_semaphore =
  [ `E of Emu.anon_semaphore
  | `N of Native.anon_semaphore
  ]


let force_emu = ref false

let force_emulation() =
  force_emu := true

let emu() =
  !force_emu || 
    (Netsys_posix.have_named_posix_semaphores() && 
       not (Netsys_posix.have_anon_posix_semaphores()))

let have_anon_semaphores() =
  if emu() then
    Emu.have_anon_semaphores()
  else
    Native.have_anon_semaphores()


let container prefix : container =
  if emu() then
    `E(Emu.container prefix)
  else
    `N(Native.container prefix)

let create_container prefix =
  if emu() then
    `E(Emu.create_container prefix)
  else
    `N(Native.create_container prefix)

let unlink prefix =
  if emu() then
    Emu.unlink prefix
  else
    Native.unlink prefix

let prefix =
  function
    | `E c -> Emu.prefix c
    | `N c -> Native.prefix c

let drop cont =
  match cont with
    | `E c -> Emu.drop_container c
    | `N c -> Native.drop_container c

let sem_init cont mem pos pshared init_value =
  match cont with
    | `E c -> `E(Emu.sem_init c mem pos pshared init_value)
    | `N c -> `N(Native.sem_init c mem pos pshared init_value)

let sem_destroy cont sem =
  match (cont,sem) with
    | `E c, `E s -> Emu.sem_destroy c s
    | `N c, `N s -> Native.sem_destroy c s
    | _ -> assert false

let as_sem cont mem pos =
  match cont with
    | `E c -> `E(Emu.as_sem c mem pos)
    | `N c -> `N(Native.as_sem c mem pos)

let sem_getvalue sem =
  match sem with
    | `E s -> Emu.sem_getvalue s
    | `N s -> Native.sem_getvalue s

let sem_post sem =
  match sem with
    | `E s -> Emu.sem_post s
    | `N s -> Native.sem_post s

let sem_wait sem =
  match sem with
    | `E s -> Emu.sem_wait s
    | `N s -> Native.sem_wait s

let sem_value_max =
  Netsys_posix.sem_value_max

let sem_size =
  (* should be a multiple of the word size, so we round it up to the
     next multiple of 8
   *)
  let s = max 4 Netsys_posix.sem_size in
  (((s-1) / 8)+1) * 8

