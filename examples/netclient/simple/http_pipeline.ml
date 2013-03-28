#require "netclient";;

(* This example shows how to use the HTTP protocol pipeline. The pipeline
 * is a queue of HTTP requests which is tried to carry out in an optimal
 * way. This includes:
 * - The pipeline automatically opens several network connections if
 *   several servers occur in the queue. It also opens several connections
 *   to the same server. All these connections are handled in parallel.
 * - The pipeline sends several requests at once over a connection, if
 *   the other side supports that. This is called "pipeline mode", and
 *   the feature that gave the pipeline class its name. (And it boosts the
 *   performance if you download many small files!)
 * - If network errors occur, the pipeline repeats the request several
 *   times before giving up (the details of this feature can be configured)
 * - Redirections are followed automatically.
 * Once a request could be processed, a callback function is invoked.
 *
 * Note that pipelines should not be too long. It is a good idea to
 * add requests to the pipeline only if the number of open
 * connections is not too high (the best number depends on the quality
 * of the network connection, and the CPU load). For example, it is
 * possible to keep thousands of connections open at the same time,
 * but it does not make sense if the network is saturated because of this.
 *)

open Http_client;;

let got_response message =
  (* This function is called when the response has arrived, or a timeout
   * happened. 
   *)
  let uri = message # get_uri() in
  try
    let (http_version_string, code, text) = message # dest_status() in
    (* Get the status line.
     * http_version_string: The identifier for the protocol, e.g. "HTTP/1.1"
     * code: The status code. code >= 200 && code <= 299 means "ok".
     * text: The explanation for the code.
     * dest_status will raise the exception Http_protocol if there is not
     * a valid response.
     *)
    Printf.printf "Response for %s: %d %s\n" uri code text;
    flush stdout;
    (* Now we could get the contents of the response using:
     * let s = message # get_resp_body(),
     * or we could get the returned MIME header as in
     * let content_type = message # assoc_resp_header "content-type"
     *)
  with
      Http_protocol No_reply ->
	(* For some reason, there was no reply. For example, because the
	 * preceding message in the pipeline caused trouble, and so the
	 * response for this message could not be received.
	 *)
	Printf.printf "No response for %s\n" uri;
	flush stdout;
    | Http_protocol (Failure s) ->
	(* An error occurred *)
	Printf.printf "Error for %s: %s\n" uri s;
	flush stdout;
    | Http_protocol (Bad_message s) ->
	(* An error occurred *)
	Printf.printf "Got bad message for %s: %s\n" uri s;
	flush stdout;
    | Http_protocol (Unix.Unix_error(e,fname,param)) ->
	Printf.printf "Network error for %s: %s\n" uri (Unix.error_message e);
	flush stdout;
    | Http_protocol other ->
	Printf.printf "Other exception for %s: %s\n" uri
	  (Printexc.to_string other);
	flush stdout;
;;


let get_several_urls url_list =
  (* A pipeline is just a container for the requests and responses. *)
  let p = new pipeline in
  (* Configure p: *)
  p # set_proxy_from_environment();  (* Respect "http_proxy", "no_proxy" *)
  (* Set some verbosity: 
   * let opts = p # get_options in
   * p # set_options { opts with verbose_connection = true };
   *)
  (* Add the requests to the pipeline. The requests are only collected,
   * no network I/O happens.
   *)
  List.iter
    (fun url ->
       let message = new get url in
       (* message: a container for the request AND the corresponding response *)
       p # add_with_callback 
	 message
	 got_response;   (* This function is called when the response arrives *)
       (* Note: add_with_callback will raise an exception immediately for
	* DNS errors ("host name lookup failed" etc.). The DNS lookups
	* are done synchronously.
	*)
    )
    url_list;
  (* Now start the pipeline. "run" returns when the job is done, i.e. all
   * requests have been processed. If there is an exception, we can print
   * it and restart "run".
   *)
  let rec go_ahead() =
    try
      p # run()
    with
	err ->
	  print_endline ("Uncaught exception: " ^ Printexc.to_string err);
	  flush stdout;
	  go_ahead()
  in
  go_ahead()
;;
