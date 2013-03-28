(* This example downloads a URL into a file *)

open Http_client

let download url =
  let pipeline = new pipeline in
  let get_call = new get url in
  get_call # set_response_body_storage (`File (fun () -> "file"));
  pipeline # add get_call;
  pipeline # run()
;;
