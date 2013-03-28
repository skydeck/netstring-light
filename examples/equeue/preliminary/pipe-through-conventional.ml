
let buffer_length = 1024 in
let buffer = String.create buffer_length in
try
  while true do
    (* Read up to buffer_length bytes into the buffer: *)
    let n = Unix.read Unix.stdin buffer 0 buffer_length in
    (* If n=0, the end of input is reached. Otherwise we have 
     * read n bytes. 
     *)
    if n=0 then
      raise End_of_file;
    (* Convert: *)
    let buffer' = String.uppercase (String.sub buffer 0 n) in
    (* Write the buffer' contents: *)
    let m = ref 0 in
    while !m < n do
      m := !m + Unix.write Unix.stdout buffer' !m (n - !m)
    done
  done
with 
  End_of_file -> ()
