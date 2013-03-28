(* For comparison: a non-clustered multiplication *)

open Printf

let fill m rows cols =
  for j = 0 to rows-1 do
    for k = 0 to cols-1 do
      m.(j).(k) <- Random.float 1.0
    done
  done


let test lrows rcols rrows =
  let lcols = rrows in
  let lmatrix = Array.make_matrix lrows lcols 0.0 in
  let rmatrix = Array.make_matrix rrows rcols 0.0 in
  fill lmatrix lrows lcols;
  fill rmatrix rrows rcols;
  let res = Array.make_matrix rrows lcols 0.0 in

  for row = 0 to rrows-1 do
    for col = 0 to lcols - 1 do
      let s = ref 0.0 in
      for j = 0 to lrows-1 do
	s := !s +. lmatrix.(j).(col) *. rmatrix.(row).(j)
      done;
      res.(row).(col) <- !s
    done
  done


let main() =
  Random.self_init();

  let lrows = ref 1000 in
  let rcols = ref 1000 in
  let rrows = ref 1000 in
  Arg.parse
    [ "-size", Arg.Tuple [ Arg.Set_int lrows;
			   Arg.Set_int rcols;
			   Arg.Set_int rrows
			 ],
      "<P> <Q> <R>   Size of test: Multiply a PxR with a RxQ matrix"
    ]
    (fun arg -> raise(Arg.Bad("Bad argument: " ^ arg)))
    (sprintf "usage: %s <options>" Sys.argv.(0));

  test !lrows !rcols !rrows


let () =
  main()
