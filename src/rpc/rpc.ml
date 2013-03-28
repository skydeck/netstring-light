(* $Id: rpc.ml 1547 2011-02-15 01:04:25Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(* TODO:
 * - werden alle Deskriptoren wieder geschlossen?
 * - abort handler einfügen, Abort exceptions erzeugen
 * - beim Schliessen auch clear machen, um handler zu löschen
 * - Testen: Fehlerbehandlung im Server und Client
 * - Testen: Signals
 * - Testen: clear-Funktion
 * - Client: Timeouts behandeln
 * - Portmapper
 * - restl. Module
 * - XDR-Selbstreferenzen
 * - Rest Portmapper
 * - UNIX-Authentifizierung, ggf. Interface um weiter Auths einzuhängen
 *)

open Rtypes

type protocol =
    Tcp          (* means: stream-oriented connection *)
  | Udp;;        (* means: datagram exchange *)

type mode =
    Socket     (* classical server socket *)
  | BiPipe     (* server is endpoint of a bidirectional pipe *)



(* these are error conditions sent to the client: *)

type server_error =
    Unavailable_program                      (* accepted call! *)
  | Unavailable_version of (uint4 * uint4)   (* accepted call  *)
  | Unavailable_procedure                    (* accepted call  *)
  | Garbage                                  (* accepted call  *)
  | System_err
  | Rpc_mismatch of (uint4 * uint4)          (* rejected call  *)
  | Auth_bad_cred                            (* rejected call  *)
  | Auth_rejected_cred                       (* rejected call  *)
  | Auth_bad_verf                            (* rejected call  *)
  | Auth_rejected_verf                       (* rejected call  *)
  | Auth_too_weak                            (* rejected call  *)
  | Auth_invalid_resp                        (* rejected call  *)
  | Auth_failed                              (* rejected call  *)
  | RPCSEC_GSS_credproblem                   (** rejected call  *)
  | RPCSEC_GSS_ctxproblem                    (** rejected call  *)
;;


let string_of_server_error =
  function
    | Unavailable_program -> 
	"Unavailable_program"
    | Unavailable_version(v1,v2) ->
	"Unavailable_version(" ^ 
	  Int64.to_string(Rtypes.int64_of_uint4 v1) ^ ", " ^ 
	  Int64.to_string(Rtypes.int64_of_uint4 v2) ^ ")"
    | Unavailable_procedure ->
	"Unavailable_procedure"
    | Garbage ->
	"Garbage"
    | System_err ->
	"System_err"
    | Rpc_mismatch(v1,v2) ->
	"Rpc_mismatch(" ^ 
	  Int64.to_string(Rtypes.int64_of_uint4 v1) ^ ", " ^ 
	  Int64.to_string(Rtypes.int64_of_uint4 v2) ^ ")"
    | Auth_bad_cred ->
	"Auth_bad_cred"
    | Auth_rejected_cred ->
	"Auth_rejected_cred"
    | Auth_bad_verf ->
	"Auth_bad_verf"
    | Auth_rejected_verf ->
	"Auth_rejected_verf"
    | Auth_too_weak ->
	"Auth_too_weak"
    | Auth_invalid_resp ->
	"Auth_invalid_resp"
    | Auth_failed ->
	"Auth_failed"
    | RPCSEC_GSS_credproblem ->
	"RPCSEC_GSS_credproblem"
    | RPCSEC_GSS_ctxproblem ->
	"RPCSEC_GSS_ctxproblem"


exception Rpc_server of server_error;;

exception Rpc_cannot_unpack of string;;


let () =
  Netexn.register_printer
    (Rpc_server Unavailable_program)
    (fun e ->
       match e with
	 | Rpc_server code ->
	     "Rpc.Rpc_server(" ^ string_of_server_error code ^ ")"
	 | _ ->
	     assert false
    )
