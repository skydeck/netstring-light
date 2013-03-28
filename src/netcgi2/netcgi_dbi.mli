(* netcgi_dbi.mli

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

(** Pools of connections for the ocamldbi generic database interface.
 *)

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
      (** Example: [module MyPool = DbiPool(Dbi_postgres)]
       *
       * [let dbh = MyPool.get request "database_name"]
       *
       * Returns an unused [Dbi.connection] handle from the pool of
       * database handles. Or it may create a new database connection
       * and return that.
       *
       * The parameters uniquely identify the database name
       * (eg. "comments") and optional parameters. Separate pools are
       * maintained for each combination of parameters.
       *
       * The connection is automatically returned to the pool at the
       * end of the [cgi] request.  After this time the connection may
       * be given away to another user. For this reason, the calling
       * code must NEVER stash the connection across requests
       * (instead, call [get] to get a new handle each time).
       *
       * On returning the handle to the pool, the pool performs a
       * ROLLBACK operation on the handle, thus losing any changes
       * (INSERT, etc.)  made to the database. If the program wants to
       * preserve changes, it must perform a COMMIT operation itself,
       * by calling [Dbi.connection.commit].  *)
end

module DbiPool(Dbi_driver : DBI_DRIVER) :
  (DBI_POOL with type connection = Dbi_driver.connection)
  (** [module MyPool = DbiPool (Dbi_postgres)]
   *
   * creates a pool of PostgreSQL database handles.  To use them:
   *
   * [let dbh = MyPool.get r "database_name"]
   *
   * Gets you a new or recycled database handle [dbh] which is valid until
   * the end of the current Apache request.
   *)
