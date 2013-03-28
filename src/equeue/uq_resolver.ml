(* $Id: uq_resolver.ml 1662 2011-08-29 23:05:06Z gerd $ *)

type 't engine_state =
  [ `Working of int
  | `Done of 't
  | `Error of exn
  | `Aborted
  ]

class type [ 't ] engine = object
  method state : 't engine_state
  method abort : unit -> unit
  method request_notification : (unit -> bool) -> unit
  method event_system : Unixqueue.event_system
end

exception Host_not_found of string

class type resolver =
object
  method host_by_name : 
           string -> Unixqueue.event_system -> Unix.host_entry engine
end


let host_by_addr host =
  let l = String.length host in
  let addr = 
    if l >= 2 && host.[0] = '[' && host.[l-1] = ']' then
      Unix.inet_addr_of_string (String.sub host 1 (l-2))
    else
      Unix.inet_addr_of_string host in
  `Done
    { Unix.h_name = host;
      h_aliases = [| |];
      h_addrtype = Netsys.domain_of_inet_addr addr;
      h_addr_list = [| addr |]
    }


let default_resolver() : resolver =
object (self)
  method host_by_name host esys =
    let state =
      try
	host_by_addr host
      with
	| Failure _ ->
	    try
	      let he = Unix.gethostbyname host in
	      (`Done he)
	    with Not_found ->
	      (`Error(Host_not_found host)) in
    ( object
	method state = state
	method abort() = ()
	method request_notification _ = ()
	method event_system = esys
      end
    )
end

let gai_resolver ?(ipv4=true) ?(ipv6=true) () : resolver =
object (self)
  method host_by_name host esys =
    let state =
      try
	host_by_addr host
      with
	| Failure _ ->
	    try
	      let fam_flags =
		match ipv4, ipv6 with
		  | false, false -> raise Not_found
		  | true, false -> [ Unix.AI_FAMILY Unix.PF_INET ]
		  | false, true -> [ Unix.AI_FAMILY Unix.PF_INET6 ]
		  | true, true -> [] in
	      let l =
		Unix.getaddrinfo host ""
		  (Unix.AI_SOCKTYPE Unix.SOCK_STREAM :: Unix.AI_CANONNAME ::
		     fam_flags) in
	      match l with
		| [] -> raise Not_found
		| ai :: _ ->
		    `Done
		      { Unix.h_name = ai.Unix.ai_canonname;
			h_aliases = [| |];
			h_addrtype = ai.Unix.ai_family;
			h_addr_list = 
			  Array.of_list
			    (List.map
			       (fun ai1 -> 
				  match ai1.Unix.ai_addr with
				    | Unix.ADDR_INET(ip,_) -> ip
				    | _ -> assert false
			       )
			       (List.filter
				  (fun ai1 -> 
				     ai1.Unix.ai_family = ai.Unix.ai_family)
				  l
			       )
			    )
		      }
	    with Not_found ->
	      (`Error(Host_not_found host)) in
    ( object
	method state = state
	method abort() = ()
	method request_notification _ = ()
	method event_system = esys
      end
    )
end


let cur_resolver = ref(default_resolver())

let current_resolver() = !cur_resolver

let set_current_resolver r = cur_resolver := r


let get_host_by_name ?(resolver = !cur_resolver) host =
  let esys = Unixqueue.create_unix_event_system() in
  let eng = resolver # host_by_name host esys in
  let eng_final = ref false in
  try
    Unixqueue.run esys;
    ( match eng#state with
	| `Done he ->
	    he
	| `Error e ->
	    eng_final := true;
	    raise e
	| _ ->
	    assert false
    )
  with
    | e when not !eng_final ->
	eng # abort();
	raise e


let sockaddr_of_socksymbol ?resolver =
  function
    | `Inet(ip,port) ->
	Unix.ADDR_INET(ip,port)
    | `Unix p ->
	Unix.ADDR_UNIX p
    | `Inet_byname(n,port) ->
	let e = get_host_by_name ?resolver n in
	let ip = e.Unix.h_addr_list.(0) in
	Unix.ADDR_INET(ip,port)
