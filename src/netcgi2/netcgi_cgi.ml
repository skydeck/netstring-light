(* netcgi_cgi.ml

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

(* TODO: Some web servers (e.g. iPlanet) do not always write the
   stderr output to the error log.  Is there any workaround?
   [env#server_software] could be used to turn it on automatically. *)


open Netcgi_common

let split_name_val s =
  try
    let i = String.index s '=' in
    (String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1))
  with Not_found ->
    (s, "")



let run ?(config=Netcgi.default_config)
    ?(output_type=(`Direct "":Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic))
    ?(exn_handler=(fun _ f -> f()))
    f =
  (* CGI: get properties and HTTP header from the environment
     variables. *)
  let (properties, input_header) =
    Array.fold_left
      (fun l e -> update_props_inheader (split_name_val e) l)
      ([], []) (Unix.environment()) in

  (* CGI: output channel = stdout *)
  let out_obj = new Netchannels.output_channel stdout in

  (* Environment object *)
  let env = new cgi_environment ~config ~properties ~input_header out_obj in

  (* Now that one knows the environment, one can warn about exceptions *)
  exn_handler_default env ~exn_handler
    (fun () ->
       let in_obj = new Netchannels.input_channel stdin in
       let cgi = cgi_with_args (new cgi) env output_type in_obj arg_store in
       (try
          f (cgi:Netcgi.cgi);
          cgi#out_channel#commit_work();
          cgi#finalize()
        with e when config.default_exn_handler ->
          cgi#finalize(); raise e);
       None (* no "special" internal exception *)
    )
    ~finally:(fun () ->
                (try env#out_channel#close_out() with _ -> ());
                (* => flush buffer; it is the user responsability to
                   commit his work. *)
             );
  exit 0




let is_cgi () =
  let env v = ignore(Sys.getenv v) in
  try
    (* Env vars that MUST be present *)
    env "GATEWAY_INTERFACE";
    env "QUERY_STRING";
    env "REMOTE_ADDR";
    env "REQUEST_METHOD";
    env "SCRIPT_NAME";
    env "SERVER_NAME";
    env "SERVER_PORT";
    env "SERVER_PROTOCOL";
    env "SERVER_SOFTWARE";
    true
  with Not_found ->
    false
