(* $Id: netsmtp.ml 1614 2011-06-09 15:08:56Z gerd $
 * ----------------------------------------------------------------------
 *
 * This is an implementation of the Simple Mail Transfer Protocol (SMTP) 
 * as specifed by RFC-2821.
 *)

open Netchannels
open Unix

exception Protocol_error
exception Transient_error of int * string
exception Permanent_error of int * string

let tcp_port = 25

(* Helpers *)

let trim s l r = String.sub s l (String.length s - r - l)

let join = String.concat "\n"

let none = function _ -> false
let void  = function _ -> ()

let ok2 i j x = x = i || x = j
let okl l x = List.mem x l

let read_status ic =
  let rec read acc =
    let l = ic # input_line () in
    if l.[3] = '-' then
      read ((trim l 4 1)::acc)
    else
      (int_of_char l.[0] - int_of_char '0') ,
      int_of_string (String.sub l 0 3) ,
      List.rev ((trim l 4 1)::acc)
  in read []

let handle_answer ic =
  let flag, code, msg = read_status ic in
  match flag with
    | 2 | 3 -> code, msg
    | 4 -> raise (Transient_error (code, join msg))
    | 5 -> raise (Permanent_error (code, join msg))
    | _ -> raise Protocol_error

let ignore_answer ic = ignore (handle_answer ic)

(* class *)

class client
  (ic : in_obj_channel)
  (oc : out_obj_channel) =
object (self)

  initializer
    ignore_answer ic

  method private smtp_cmd cmd =
    oc # output_string cmd;
    oc # output_string "\r\n";
    oc # flush ()

  method helo ?host () =
    oc # output_string "EHLO ";
    self # smtp_cmd (
      match host with
        | None -> (Uq_resolver.get_host_by_name (gethostname ())).h_name
        | Some s -> s
    );
    snd (handle_answer ic)

  method mail email =
    self # smtp_cmd (Printf.sprintf "MAIL FROM: <%s>" email);
    ignore_answer ic
   
  method rcpt email =
    self # smtp_cmd (Printf.sprintf "RCPT TO: <%s>" email);
    try  ignore_answer ic
    with Permanent_error (551, msg) -> self # rcpt msg

  method data (chan:in_obj_channel) =
    self # smtp_cmd "DATA";
    ignore_answer ic;
    ( try
	while true do
          let l = chan # input_line () in
            if String.length l > 0 && l.[0] = '.' then oc # output_char '.';
            oc # output_string l;
            oc # output_string 
	      (if String.length l > 0 && 
		 l.[String.length l - 2] = '\r' then "\n" else "\r\n")
	done;
	assert false
      with End_of_file -> () );
    self # smtp_cmd ".";
    ignore_answer ic
   
  method rset () =
    self # smtp_cmd "RSET";
    ignore_answer ic

  method expn ml =
    oc # output_string "EXPN ";
    self # smtp_cmd ml;
    match handle_answer ic with
      | 250, msg -> Some msg
      | _ -> None

  method help () =
    self # smtp_cmd "HELP";
    snd (handle_answer ic)
      
  method noop () =
    self # smtp_cmd "NOOP";
    ignore_answer ic

  method quit () =
    self # smtp_cmd "QUIT";
    ignore_answer ic
 
end
