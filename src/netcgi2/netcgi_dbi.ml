(* netcgi_dbi.ml

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
(* $Id: netcgi_dbi.ml,v 1.2 2005/10/19 20:21:23 chris_77 Exp $ *)

let cvs_version = "$Revision: 1.2 $"


module type DBI_DRIVER = sig
  type connection
  val connect : ?host:string -> ?port:string ->
    ?user:string -> ?password:string -> string ->
    connection
  val close : connection -> unit
  val closed : connection -> bool
  val commit : connection -> unit
  val ping : connection -> bool
  val rollback : connection -> unit
end

module type DBI_POOL = sig
  type connection
  val get : Netcgi.cgi -> ?host:string -> ?port:string ->
    ?user:string -> ?password:string -> string -> connection
end


(* XXX Think about how to do this in general *)
module DbiPool(Dbi_driver : DBI_DRIVER) = struct

  type connection = Dbi_driver.connection

  (* List of pools. The key is the unique combination of host/port/etc. and
   * the value is a list of unused connections for that pool.
   *
   * This code ought to work even for a multi-threaded Apache server.
   *)
  let pools = Hashtbl.create 8

  let get (cgi:Netcgi.cgi) ?host ?port ?user ?password database_name =
    let key = (host, port, user, password, database_name) in

    (* Get the pool (a connection list). *)
    let dbh_list = try Hashtbl.find pools key with Not_found -> [] in

    (* Search for an unused connection. We actually iterate over the
       pool testing the handles (in case they have timed out or
       something).  *)
    let rec loop = function
      | [] ->
	  (* No handles left. Need to create a new connection. *)
	  let dbh =
	    Dbi_driver.connect ?host ?port ?user ?password database_name in
	  dbh, []
      | dbh :: dbhs ->
	  (* Test if dbh is a working handle. If so, return it. *)
	  if Dbi_driver.ping dbh then
	    dbh, dbhs
	  else (
	    Dbi_driver.close dbh;
	    loop dbhs
	  )
    in
    let dbh, remainder = loop dbh_list in

    (* Update the pool. *)
    Hashtbl.replace pools key remainder;

    (* Register a callback so that we return this handle to the pool
       when the request finishes.  *)
    cgi#at_exit
      (fun () ->
	 if not (Dbi_driver.closed dbh) then (
	   Dbi_driver.rollback dbh;
	   let dbh_list = Hashtbl.find pools key in
	   Hashtbl.replace pools key (dbh_list @ [dbh])
	 ));
    dbh

end
