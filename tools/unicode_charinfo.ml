(* $Id$ *)

open Printf

let filename = Sys.argv.(1)

let generate label name =
  let f = open_in filename in
  printf "let %s k =\n" name;
  printf " match k with\n";
  ( try
      while true do
        Scanf.fscanf f "%s %d-%d %d-%d\n"
          (fun l r1s r1e r2s r2e ->
             if l = label then (
               if r1s = r1e then
                 printf "   | %d -> %d\n" r1s r2s
               else (
                 for k = r1s to r1e do
                   printf "   | %d\n" k
                 done;
                 printf "       -> k + (%d)\n" (r2s-r1s)
               )
             )
          )
      done
    with End_of_file -> ()
  );
  printf "   | _ -> k";
  printf ";;\n";
  close_in f

let () =
  generate "L" "to_lower";
  generate "U" "to_upper";
  generate "T" "to_title"
