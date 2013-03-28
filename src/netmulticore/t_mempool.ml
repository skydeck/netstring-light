module Unit_encap = Netplex_encap.Make_encap(struct type t = unit end)


let pool = Netmcore_mempool.create_mempool (1024*1024)

let test() =
  let l = ref [] in
  for k = 1 to 84 do
    let m = Netmcore_mempool.alloc_mem pool 9000 in
    l := (Random.int 100, m) :: !l
  done;
  print_endline (Netmcore_mempool.debug_info pool);
  l := List.sort (fun (k1,_) (k2,_) -> k1-2) !l;
  List.iter
    (fun (_, m) ->
       Netmcore_mempool.free_mem pool m
    )
    !l;
  print_endline (Netmcore_mempool.debug_info pool)


let test_fork, test_join =
  Netmcore.def_process
    (fun _ ->
       test();
       Unit_encap.wrap ()
    )


let () =
  Netmcore.startup
    ~socket_directory:"/tmp/t_mempool"
    ~first_process:(fun () -> 
		      Netmcore.start
			~inherit_resources:`ALl
			test_fork (Unit_encap.wrap()))
    ()
