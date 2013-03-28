#load "netmcore_util.cmo";;
open Netmcore_util.AVL;;
let h = create_header();;
h.nodes <- Array.init 50 (fun _ -> create_node());;
init_header h;;

let add1() =
  add h 1 1;
  add h 2 2;
  add h 10 10;
  add h 5 5

let height k =
  let rec descent k depth =
    if k = (-1) then
      depth
    else (
      let node = h.nodes.(k) in
      let h1 = descent node.left (depth+1) in
      let h2 = descent node.right (depth+1) in
      max h1 h2
    ) in
  descent k 0

let check_bal () =
  let rec descent k =
    if k <> (-1) then (
      let node = h.nodes.(k) in
      let h1 = height node.left in
      let h2 = height node.right in
      assert(node.bal = h2-h1);
      descent node.left;
      descent node.right
    )
  in
  descent h.root

let t1() =
  init_header h;
  for k = 1 to 20 do
    print_endline ("k=" ^ string_of_int k);
    add h k k;
    check_bal()
  done

let t2() =
  init_header h;
  for k = 20 downto 1 do
    print_endline ("k=" ^ string_of_int k);
    add h k k;
    check_bal()
  done

let t3() =
  init_header h;
  for k = 1 to 20 do
    let x = Random.int 100 in
    print_endline ("x=" ^ string_of_int x);
    add h x x;
    check_bal()
  done

let t4() =
  t1();
  for k = 1 to 20 do
    print_endline ("k=" ^ string_of_int k);
    remove h k;
    check_bal()
  done

let t5() =
  t1();
  for k = 20 downto 1 do
    print_endline ("k=" ^ string_of_int k);
    remove h k;
    check_bal()
  done

let t6() =  (* removal in reverse add order *)
  init_header h;
  let keys = ref [] in
  for k = 1 to 20 do
    let x = Random.int 100 in
    print_endline ("x=" ^ string_of_int x);
    add h x x;
    check_bal();
    keys := x :: !keys
  done;
  while !keys <> [] do
    let x = List.hd !keys in
    print_endline ("x=" ^ string_of_int x);
    remove h x;
    print_endline ("  remaining: " ^ as_debug_list h);
    check_bal();
    keys := List.tl !keys
  done

let t7() =  (* removal in add order *)
  init_header h;
  let keys = ref [] in
  for k = 1 to 20 do
    let x = Random.int 100 in
    print_endline ("x=" ^ string_of_int x);
    add h x x;
    check_bal();
    keys := x :: !keys
  done;
  keys := List.rev !keys;
  while !keys <> [] do
    let x = List.hd !keys in
    print_endline ("x=" ^ string_of_int x);
    remove h x;
    print_endline ("  remaining: " ^ as_debug_list h);
    check_bal();
    keys := List.tl !keys
  done


let t8() =  (* removal in random order *)
  init_header h;
  let keys = ref [] in
  for k = 1 to 20 do
    let x = Random.int 100 in
    print_endline ("x=" ^ string_of_int x);
    add h x x;
    check_bal();
    keys := x :: !keys
  done;
  let l = List.map (fun x -> (Random.int 100, x)) !keys in
  let l' = List.sort (fun (a,_) (b,_) -> a-b) l in
  keys := List.map (fun (_,x) -> x) l';
  while !keys <> [] do
    let x = List.hd !keys in
    print_endline ("x=" ^ string_of_int x);
    remove h x;
    print_endline ("  remaining: " ^ as_debug_list h);
    check_bal();
    keys := List.tl !keys
  done

