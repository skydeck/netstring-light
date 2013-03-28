(* netcgi_scgi.ml

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

(* The protocol is described at http://python.ca/nas/scgi/protocol.txt *)

open Netcgi_common
open Printf


(* This channel reads from the descriptor, but does not forward the
 * close request (fd is kept open)
 *)
class prim_in_channel fd =
object(self)
  inherit Netchannels.input_descr fd

  method close_in() = ()
    (* don't close fd *)
end


(* This channel writes to the descriptor, but does not forward the
 * close request (fd is kept open)
 *)
class prim_out_channel fd =
object(self)
  inherit Netchannels.output_descr fd

  method close_out() = ()
    (* don't close fd *)
end


(* A bidirectional buffered channel for I/O *)
class scgi_channel fd =
  let prim_in_ch = new prim_in_channel fd in
  let prim_out_ch = new prim_out_channel fd in
object(self)
  inherit Netchannels.buffered_raw_in_channel
            ~eol:[ "\000" ]
            ~buffer_size:8192
	    prim_in_ch
    (* A buffered input channel. Additionally, we set that lines end with
       0 bytes, so we can use enhanced_input_line to read 0-terminated
       strings
     *)

  inherit Netchannels.augment_raw_in_channel
    (* reduces additional methods like really_input to the more 
       primitive ones from Netchannels.buffered_raw_in_channel.
       Note that input_line is slow, so we override it.
     *)

  method input_line = 
    self # enhanced_input_line

  method input_until_colon() =
    (* Slow, but only used for a very short string *)
    let b = Buffer.create 100 in
    let c = ref 'X' in
    while !c <> ':' do
      c := self # input_char();
      if !c <> ':' then Buffer.add_char b !c
    done;
    Buffer.contents b

  inherit Netchannels.buffered_raw_out_channel 
            ~buffer_size:8192
	    prim_out_ch

  inherit Netchannels.augment_raw_out_channel
    (* reduces additional methods like really_output to the more 
       primitive ones from Netchannels.buffered_raw_out_channel
     *)

  method safe_close_out() =
    try self # close_out() 
    with Netchannels.Closed_channel -> ()

end

(************************************************************************)

let scgi_log_error msg =
  let zone = Netdate.localzone (* log local time *) in
  let date = Netdate.format "%c" (Netdate.create ~zone (Unix.gettimeofday())) in
  prerr_endline ("[" ^ date ^ "] [Netcgi_scgi] " ^ msg)


(* [input_props_inheader in_obj] reads the netstring
   [len]":"[string]"," from the input object [in_obj] and chunk it
   into the key-value pairs representing the properties.  *)
let rec input_props_inheader_loop in_obj len props_inheader =
  if len < 0 then
    raise(HTTP(`Bad_request, "Netcgi_scgi: Bad header length"));
  if len = 0 then begin
    (* The netstring must finish with a comma *)
    if in_obj#input_char() <> ',' then
      raise(HTTP(`Bad_request, "Netcgi_scgi: The header must end with ','"));
    (* Add some compulsory CGI properties *)
    let (props, inheader) = props_inheader in
    (("GATEWAY_INTERFACE", "CGI/1.1") :: props, inheader)
  end else begin
    (* Note that input_line is redefined so the lines are assumed to be
       0-terminated strings
     *)
    let name = in_obj#input_line() in
    let value = in_obj#input_line() in
    let len = len - String.length name - String.length value - 2 (* \000 *) in
    let props_inheader = update_props_inheader (name, value) props_inheader in
    input_props_inheader_loop in_obj len props_inheader
  end

let input_props_inheader in_obj =
  (* length of the "netstring": *)
  let len =
    try int_of_string(in_obj#input_until_colon())
    with End_of_file | Failure _ ->
      let msg = "Netcgi_scgi: Incorrect length of netstring header" in
      raise(HTTP(`Bad_request, msg)) in
  try
    input_props_inheader_loop in_obj len ([],[])
  with
    | End_of_file->
	raise(HTTP(`Bad_request, "EOF while reading header"))


class scgi_env ?log_error ~config ~properties ~input_header out_obj
  : Netcgi_common.cgi_environment =
object
  inherit cgi_environment ~config ~properties ~input_header out_obj

  (* Override to use the correct channel *)
  method log_error msg = 
    match log_error with
      | None -> scgi_log_error msg
      | Some f -> f msg
end


let handle_request config output_type arg_store exn_handler f ~log fd =
  let scgi_ch = new scgi_channel fd in
  let (properties, input_header) = input_props_inheader scgi_ch in
  let env = new scgi_env ?log_error:log ~config ~properties ~input_header
    (scgi_ch :> Netchannels.out_obj_channel) in

  (* Now that one knows the environment, one can warn about exceptions *)
  try
    exn_handler_default env ~exn_handler
      (fun () ->
	 let cgi = cgi_with_args (new cgi) env output_type
           (scgi_ch :> Netchannels.in_obj_channel) arg_store in
	 (try
            f (cgi:Netcgi.cgi);
            cgi#out_channel#commit_work();
            cgi#finalize()
          with e when config.default_exn_handler ->
            cgi#finalize(); raise e);
	 None (* no "special" internal exception *)
      )
      ~finally:(fun () ->
                  scgi_ch#safe_close_out()
                    (* => flush buffer; it is the user responsability to
                   commit his work. *)
               );
    `Conn_close_linger
  with
    | Unix.Unix_error(Unix.EPIPE,_,_) ->
	`Conn_close_linger
    | error ->
	`Conn_error error



(* [handle_connection fd .. f] handle an accept()ed connection,
   reading incoming records on the file descriptor [fd] and running
   [f] for each incoming request. *)
let handle_connection fd ~config output_type arg_store exn_handler f =
  Netlog.Debug.track_fd 
    ~owner:"Netcgi_scgi"
    ~descr:("connection from " ^ 
	      try Netsys.string_of_sockaddr(Netsys.getpeername fd)
	      with _ -> "(noaddr)")
    fd;
  let log =
    Some scgi_log_error in
  let cdir =
    handle_request config output_type arg_store exn_handler f ~log fd in
  match cdir with
    | `Conn_close_linger ->
	Unix.setsockopt_optint fd Unix.SO_LINGER (Some 15);
        Unix.shutdown fd Unix.SHUTDOWN_ALL;
	Netlog.Debug.release_fd fd;
        Unix.close fd
    | `Conn_error e ->
	Netlog.Debug.release_fd fd;
	Unix.close fd;
	raise e
    (* Othe cdirs not possible *)

let run
    ?(config=Netcgi.default_config)
    ?(allow=fun _ -> true)
    ?(output_type=(`Direct "":Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic))
    ?(exn_handler=(fun _ f -> f()))
    ?sockaddr
    ?port (* no default in the spec *)
    f =
  (* Socket to listen to *)
  let sockaddr = 
    match sockaddr with
      | None -> (
          match port with
              (* Either port or sockaddr need to be specified *)
            | None -> 
                invalid_arg "Netcgi_scgi.run: neither sockaddr not port passed"
            | Some port -> 
                Unix.ADDR_INET(Unix.inet_addr_loopback, port)
        )
      | Some sockaddr -> sockaddr in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 5;
  Netlog.Debug.track_fd
    ~owner:"Netcgi_scgi"
    ~descr:("master " ^ Netsys.string_of_sockaddr sockaddr)
    sock;
  while true do
    let (fd, server) = Unix.accept sock in
    try
      if allow server then
	handle_connection fd ~config output_type arg_store exn_handler f;
    with
      | e when config.default_exn_handler ->
	  ()
  done
