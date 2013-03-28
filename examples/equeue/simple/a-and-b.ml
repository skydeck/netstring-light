#require "equeue";;

type event = 
  A of int
| B

let n = ref 1
let source esys =
  if !n <= 10 then begin
    Equeue.add_event esys (A !n);
    incr n
  end



let handler_a esys e =
  match e with 
    A n ->
      for i = 1 to n do
        Equeue.add_event esys B;
      done
  | _ ->
      raise Equeue.Reject

let handler_b esys e =
  match e with
    B ->
      print_endline "B"
  | _ ->
      raise Equeue.Reject
;;

let esys = Equeue.create source in
Equeue.add_handler esys handler_a;
Equeue.add_handler esys handler_b;
Equeue.run esys
;;
