(* $Id: rpc_auth_local.ml 1671 2011-09-22 22:25:06Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

external get_peer_credentials : Unix.file_descr -> (int * int)
  = "netsys_get_peer_credentials"
;;

(*
external peek_peer_credentials : Unix.file_descr -> (int * int)
  = "netsys_peek_peer_credentials"
;;
*
 *)


class server_auth_method : Rpc_server.auth_method =
object
  method name = "AUTH_LOCAL"
  method flavors = [ ]
  method peek =
    `Peek_descriptor
      (fun d ->
	 match Netsys.getpeername d with
	     Unix.ADDR_UNIX _ ->
	       (* Try now peek_peer_credentials: *)
	       begin try
		 let uid, gid = get_peer_credentials d in
		 let username =
		   string_of_int uid ^ "." ^ string_of_int gid ^ "@localhost" in
		 Some username
	       with
		   Invalid_argument _ ->
		     (* peek_peer_credentials is not available for this OS *)
		     None
		 | Not_found ->
		     (* Some other failure *)
		     None
		 | Unix.Unix_error(Unix.EAGAIN,_,_) ->
		     (* peek_peer_credentials expects that there is a message
                      * to read. EAGAIN is raised if we call it in the wrong
                      * moment.
                      *)
		     None
	       end
	   | _ ->
	       None
      )

  method authenticate _ _ _ _ = ()

end



let server_auth_method() = new server_auth_method
