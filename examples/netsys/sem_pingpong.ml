open Netsys_posix
open Printf

let s1 =
  let n1 = sprintf "/s1_%d" (Unix.getpid()) in
  let s = sem_open n1 [SEM_O_CREAT] 0o666 0 in
  sem_unlink n1;
  s

let s2 =
  let n2 = sprintf "/s2_%d" (Unix.getpid()) in
  let s = sem_open n2 [SEM_O_CREAT] 0o666 0 in
  sem_unlink n2;
  s


let process1 n =
  for k = 1 to n do
    sem_post s1;
    sem_wait s2 SEM_WAIT_BLOCK
  done;
  exit 0

let process2 n =
  for k = 1 to n do
    sem_wait s1 SEM_WAIT_BLOCK;
    sem_post s2
  done;
  exit 0

let main() =
  let n = int_of_string Sys.argv.(1) in
  let pid1 =
    match Unix.fork() with
      | 0 ->
	  process1 n
      | pid ->
	  pid in
  let pid2 =
    match Unix.fork() with
      | 0 ->
	  process2 n
      | pid ->
	  pid in
  ignore(Unix.waitpid [] pid1);
  ignore(Unix.waitpid [] pid2)

let () =
  main()
