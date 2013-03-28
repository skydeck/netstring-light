(*
open Netmcore_mempool;;
let p = create_mempool (1024*1024);;
#spawn;;
 *)

#directory "../netsys";;
open Printf
open Netmcore_heap;;
let p = `Resource 2;;
Debug.enable := true;;
let h = create_heap p 5000 (ref None);;
print_endline(debug_info h);;

let upto n =
  let rec loop k =
    if k < n then
      k :: loop (k+1)
    else
      [] in
  loop 0;;
  
modify h (fun add -> (root h) := add (Some (upto 10)));;

let addr x =
  sprintf "%nx" (Netsys_mem.obj_address (Obj.repr x));;

let unopt =
  function
    | None -> failwith "unopt"
    | Some x -> x ;;

with_value h (fun () -> List.tl (unopt (!(root h)))) (fun tl -> modify h (fun mut -> (root h) := add mut (Some tl)));;

(*
modify h (fun mut -> (root h) := add mut (Some "Gerd"));;
 *)

(*
open Netmcore_buffer;;
let b = create p 4096 ();;
add_string b "Gerd";;
add_string b (String.make 4096 '1');;
delete_hd b 500;;
add_string b (String.make 4096 '2');;
 *)

let dump_mem start len =
  let mem = Netsys_mem.grab start len in
  let hex_line = Buffer.create 80 in
  let asc_line = Buffer.create 20 in
  let addr = ref start in

  let print_line n =
    printf "%16nx: %-48s    %s\n"
      !addr (Buffer.contents hex_line) (Buffer.contents asc_line);
    Buffer.clear hex_line;
    Buffer.clear asc_line;
    addr := Nativeint.add !addr (Nativeint.of_int n) in

  for k = 0 to len - 1 do
    let c = mem.{k} in
    bprintf hex_line "%02x " (Char.code c);
    if c >= ' ' && c <= '\x7e' then
      Buffer.add_char asc_line c
    else
      Buffer.add_char asc_line '.';
    if (k+1) mod 16 = 0 then print_line 16
  done;
  if Buffer.length asc_line > 0 then
    print_line 0
;;
