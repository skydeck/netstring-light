(* 
 * $Id: uq_socks5.mli 1664 2011-08-29 23:40:18Z gerd $
 *)

open Uq_engines;;

(** This module implements a SOCKS version 5 client (see RFC 1928) for
 * use with the {!Uq_engines.connector}, {!Uq_engines.listener}, and
 * {!Uq_engines.datagram_provider} engine factories.
 *
 * This implementation supports IPv4 and IPv6.
 *)

exception Socks_error of string;;
  (** Raised when error messages of the SOCKS proxy are received, or
   * because of other SOCKS-related problems.
   *)


class proxy_client : connect_address ->
object
  inherit Uq_engines.client_endpoint_connector
  inherit Uq_engines.server_endpoint_listener
  inherit Uq_engines.datagram_socket_provider
end ;;
  (** The object created by [new proxy_client addr] can be passed as [proxy] 
   * to the {!Uq_engines.connector}, {!Uq_engines.listener}, and 
   * {!Uq_engines.datagram_provider} functions to use
   * the SOCKS proxy for connections, servers, and datagram socket, 
   * respectively.
   *
   * The SOCKS client supports the following methods:
   * - [connect]: Only TCP connections are supported (UDP could be made
   *   working - TODO)
   * - [listen]: This works (only) for TCP sockets, too
   * - [create_datagram_socket]: Creates unconnected datagram sockets that
   *   can be sent and received with object methods
   *
   * Note that socket addresses are resolved by the proxy host; this applies
   * to both [`Sock_inet] and [`Sock_inet_byname]. For the latter this means
   * that name resolution is done by the proxy.
   *
   * The nature of SOCKS restricts the functionality:
   * - [`Sock_unix] addresses are not supported (i.e. you cannot connect
   *   to a Unix domain socket on the proxy host)
   * - Listening server sockets can only accept one connection; after that
   *   they are "dead" and must be shut down first. The method
   *   [multiple_connections] is [false] because of this.
   * - Further restrictions can be enforced by the SOCKS proxy and
   *   its configuration.
   *
   *)
