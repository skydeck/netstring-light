(** Parsing of socket addresses *)

type socksymbol =
    [ `Inet of Unix.inet_addr * int
    | `Inet_byname of string * int
    | `Unix of string
    ]
  (** Symbolic socket names:

      - [`Inet(ip,port)]: An internet socket with IP [ip] and the given [port]
      - [`Inet_byname(n,port)]: An internet socket with the IP resulting from
        the resolution of the name [n], and with the given [port]
      - [`Unix p]: A Unix Domain socket with the path [p]

      Use {!Uq_resolver.sockaddr_of_socksymbol} to convert to a
      [Unix.sockaddr] (and resolve names).
   *)

val socksymbol_of_string : string -> socksymbol
  (** Parses designations of the forms:
      - [<IPv4>:port]
      - [[<IPv4_or_IPv6]:port]
      - [<name>:port]
      - [/path]
      - [./path]

      Raises [Failure] on parse error.
   *)

val string_of_socksymbol : socksymbol -> string
  (** The reverse function *)
