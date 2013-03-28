(* $Id: netpop.ml 1017 2006-10-02 13:59:50Z gerd $
 * ----------------------------------------------------------------------
 *
 * This is an implementation of the Post Office Protocol - Version 3 (POP3) 
 * as specifed by RFC-1939.
 *)

open Netchannels
open Printf

type state =
  [ `Authorization
  | `Transaction
  | `Update
  ]

exception Protocol_error
exception Err_status of string
exception Bad_state

let tcp_port = 110

(* Compute the MD5 digest of a string as as a lowercase 
     hexadecimal string *)

let hex_digits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
		    'a'; 'b'; 'c'; 'd'; 'e'; 'f' |]

let md5_string s =
  let d = Digest.string s in
  let d' = String.create 32 in
  for i = 0 to 15 do
    let c = Char.code d.[i] in
    d'.[i*2+0] <- hex_digits.(c lsr 4);
    d'.[i*2+1] <- hex_digits.(c land 15);
  done;
  d'

(* Sending Commands *)
let send_command oc line =
(*  Printf.printf "C: %s\n" line; flush stdout; *)
  oc # output_string line;
  oc # output_string "\r\n";
  oc # flush ();
;;  

(* Receiving Responses *)

let trim s l r =
  String.sub s l (String.length s - r - l)

let word s p0 =
  let len = String.length s in
  let rec skip p =
    if p >= len then raise Not_found
    else
      if s.[p] = ' ' then skip (p + 1)
      else collect p p
  and collect p0 p1 =
    if p1 >= len || s.[p1] = ' ' || s.[p1] = '\r' then
      String.sub s p0 (p1 - p0), p1
    else
      collect p0 (p1 + 1)
  in
  skip p0

let map_fst f (x,y) = (f x, y)

let int s p = map_fst int_of_string (word s p)

let status_response (ic : in_obj_channel) f =
  let line = ic # input_line () in
(*  Printf.printf "S: %s\n" (trim line 0 1); flush stdout; *)
  match word line 0 with
    | "+OK", p  -> f line p
    | "-ERR", p -> raise (Err_status (trim line p 1))
    | _         -> raise Protocol_error
;;

let ignore_status ic = status_response ic (fun _ _ -> ())

let multiline_response ic f init = 
  let rec loop acc = 
    let line = ic # input_line () in
(*    Printf.printf "S: %s\n" (trim line 0 1); flush stdout; *)
    let len = String.length line in
    if len = 0 then raise Protocol_error
    else
      if line.[0] = '.' then begin
	if len = 2 then acc
	else loop (f line 1 acc)
      end else
	loop (f line 0 acc)
  in loop init
;;    

let body_response ic =
  (* make a more efficient implementation *)
  let lines = multiline_response ic (fun s p acc ->
    (trim s p 1) :: acc
  ) [] in
  new input_string (String.concat "\n" (List.rev lines))
;;

class client
  (ic : in_obj_channel)
  (oc : out_obj_channel) =
  let greeting = status_response ic (fun s p -> trim s p 1) in
object (self)

  val mutable state : state = `Authorization

  (* Current State *)
  
  method state = state
  method private check_state state' =
    if state <> state' then raise Bad_state
  method private transition state' =
    state <- state'

  (* General Commands *)

  method quit () =
    send_command oc "QUIT";
    ignore_status ic;

  (* Authorization Commands *)

  method user ~user =
    self#check_state `Authorization;
    send_command oc (sprintf "USER %s" user);
    ignore_status ic;

  method pass ~pass =
    self#check_state `Authorization;
    send_command oc (sprintf "PASS %s" pass);
    ignore_status ic;
    self#transition `Transaction;

  method apop ~user ~pass =
    self#check_state `Authorization;
    let digest = try
      let p0 = String.index_from greeting 0 '<' in
      let p1 = String.index_from greeting (p0+1) '>' in
      let timestamp = String.sub greeting p0 (p1-p0+1) in
      md5_string (timestamp ^ pass)
    with Not_found -> raise Protocol_error
    in
    send_command oc (sprintf "APOP %s %s" user digest);
    ignore_status ic;
    self#transition `Transaction;

  (* Transaction Commands *)

  method stat () =
    self#check_state `Transaction;
    send_command oc "STAT";
    try 
      status_response ic (fun s p ->
	let count, p = int s p in
	let size, p  = int s p in
	let ext      = trim s p 1 in
	(count, size, ext)
      )
    with _ -> raise Protocol_error;

  method list ?msgno () =
    self#check_state `Transaction;
    let parse_line s p set =
      let mesg_num, p  = int s p in
      let mesg_size, p = int s p in
      let ext          = trim s p 1 in
      Hashtbl.add set mesg_num (mesg_size, ext);
      set
    in
    try
      match msgno with
      | None ->
	  send_command oc "LIST";
	  ignore_status ic;
	  multiline_response ic parse_line (Hashtbl.create 1)
	    
      | Some n ->
	  send_command oc (sprintf "LIST %d" n);
	  status_response ic parse_line (Hashtbl.create 31)
    with _ -> raise Protocol_error

  method retr ~msgno =
    self#check_state `Transaction;
    send_command oc (sprintf "RETR %d" msgno);
    ignore_status ic;
    body_response ic;

  method dele ~msgno =
    self#check_state `Transaction;
    send_command oc (sprintf "DELE %d" msgno);
    ignore_status ic;

  method noop () =
    self#check_state `Transaction;
    send_command oc "NOOP";
    ignore_status ic;

  method rset () =
    self#check_state `Transaction;
    send_command oc "RSET";
    ignore_status ic;

  method top ?(lines = 0) ~msgno () =
    self#check_state `Transaction;
    send_command oc (sprintf "TOP %d %d" msgno lines);
    ignore_status ic;
    body_response ic;

  method uidl ?msgno () =
    self#check_state `Transaction;
    let parse_line s p set =
      let mesg_num, p  = int s p in
      let unique_id    = trim s p 1 in
      Hashtbl.add set mesg_num unique_id;
      set
    in
    try
      match msgno with
      | None ->
	  send_command oc "UIDL";
	  ignore_status ic;
	  multiline_response ic parse_line (Hashtbl.create 31)
      | Some n ->
	  send_command oc (sprintf "UIDL %d" n);
	  status_response ic parse_line (Hashtbl.create 1)
    with _ -> raise Protocol_error

  method stat () =
    self#check_state `Transaction;
    send_command oc "STAT";
    try 
      status_response ic (fun s p ->
	let count, p = int s p in
	let size, p  = int s p in
	let ext      = trim s p 1 in
	(count, size, ext)
      )
    with _ -> raise Protocol_error;
end
