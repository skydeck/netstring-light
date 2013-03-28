(* $Id: netcompression.ml 1610 2011-05-30 08:03:45Z gerd $ *)

type algo =
    { iana_name : string;
      encoder : (unit -> Netchannels.io_obj_channel) option;
      decoder : (unit -> Netchannels.io_obj_channel) option;
    }

let registry = Hashtbl.create 5

let register ~iana_name ?encoder ?decoder () =
  let algo =
    { iana_name = iana_name;
      encoder = encoder;
      decoder = decoder
    } in
  Hashtbl.replace registry iana_name algo

let lookup_encoder ~iana_name =
  let algo = Hashtbl.find registry iana_name in
  match algo.encoder with
    | None -> raise Not_found
    | Some f -> f

let lookup_decoder ~iana_name =
  let algo = Hashtbl.find registry iana_name in
  match algo.decoder with
    | None -> raise Not_found
    | Some f -> f

let all_encoders() =
  Hashtbl.fold
    (fun name algo acc ->
       if algo.encoder <> None then name :: acc else acc
    )
    registry
    []

let all_decoders() =
  Hashtbl.fold
    (fun name algo acc ->
       if algo.decoder <> None then name :: acc else acc
    )
    registry
    []

