(* $Id: nethttpd_util.mli 1410 2010-02-14 19:44:28Z gerd $ *)

(** Utility functions *)

open Nethttpd_types

val std_error_response : error_response_params -> string
  (** Returns the HTML text of the standard error response *)

val std_error_log_string : request_info -> string -> string
  (** Returns a log line for error logging *)

val std_access_log_string : full_info -> string
  (** Returns a log line for access logging *)

val std_debug_access_log_string : full_info -> string
  (** Returns a log string for extended access logging (multi-line) *)

