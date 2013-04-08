type socksymbol =
    [ `Inet of Unix.inet_addr * int
    | `Inet_byname of string * int
    | `Unix of string
    ]

let sa_re1 = Nlstring_str.regexp "\\([^:]*\\):\\([0-9]+\\)$"
let sa_re2 = Nlstring_str.regexp "\\[\\([^]]*\\)\\]:\\([0-9]+\\)$"

let socksymbol_of_string s =
  try
    if (s <> "" && s.[0] = '/') ||
       (String.length s >= 2 && s.[0] = '.' && s.[1] = '/')
    then
      `Unix s
    else
      if s <> "" && s.[0] = '[' then (
	match Nlstring_str.string_match sa_re2 s 0 with
	  | None -> raise Not_found
	  | Some m ->
	      let ip_s = Nlstring_str.matched_group m 1 s in
	      let port_s = Nlstring_str.matched_group m 2 s in
	      let ip = Unix.inet_addr_of_string ip_s in
	      let port = int_of_string port_s in
	      if port < 0 || port > 65535 then raise Not_found;
	      `Inet(ip,port)
      )
      else (
	match Nlstring_str.string_match sa_re1 s 0 with
	  | None -> raise Not_found
	  | Some m ->
	      let name_s = Nlstring_str.matched_group m 1 s in
	      let port_s = Nlstring_str.matched_group m 2 s in
	      let port = int_of_string port_s in
	      if port < 0 || port > 65535 then raise Not_found;
	      ( try
		  let ip = Unix.inet_addr_of_string name_s in
		  `Inet(ip,port)
		with _ ->
		  `Inet_byname(name_s,port)
	      )
      )
  with
    | _ ->
	failwith ("Cannot parse socket address: " ^ s)


let string_of_socksymbol =
  function
    | `Inet(ip,port) ->
	let n = Unix.string_of_inet_addr ip in
	( match Netsys.domain_of_inet_addr ip with
	    | Unix.PF_INET ->
		n ^ ":" ^ string_of_int port
	    | Unix.PF_INET6 ->
		"[" ^ n ^ "]:" ^ string_of_int port
	    | _ -> assert false
	)
    | `Inet_byname(n,port) ->
	n ^ ":" ^ string_of_int port
    | `Unix p ->
	if p <> "" && p.[0] = '/' then
	  p
	else
	  if String.length p >= 2 && p.[0] = '.' && p.[1] = '/' then
	    p
	  else
	    "./" ^ p
