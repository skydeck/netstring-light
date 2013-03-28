type t

val create : unit -> t

val restore : string -> t
  (* Restores the database from a file *)

val save : t -> string -> unit
  (* Saves the database into a file *)

val lookup : t -> string -> (int * float * float * string)
  (* lookup db url:
   * Finds the entry for 'url' in 'db' or raises Not_found.
   * It returns a tuple (code, time, mimetype):
   * - code: the HTTP code returned.
   *         0 means: Not yet requested.
   * - time_ok: when the last successful access happened
   *         0.0 means: Never
   * - time_access: when the last access happened
   *         0.0 means: Never
   * - mimetype: the MIME type of the entry
   *         "" means: unknown
   *)

val add : t -> string -> unit
  (* add db url: Adds an entry *)

val update : t -> string -> int -> float -> float -> string -> unit
  (* update db url code time_ok time_access mimetype:
   * Changes the entry for 'url' in 'db'.
   * Raises Not_found if an entry for 'url' does not exist.
   *)

val iter : t -> float -> float -> 
              (string * int * float * float * string) Stream.t 
  (* iter db f age interval:
   * Iterates over all entries of 'db', and includes all entries into
   * the resulting stream that
   * do not have a successful HTTP code, or are older than 'age' seconds;
   * but the access time must not be older than 'interval'.
   * The stream elements are: url,code,time_ok,time_access,mimetype.
   * While the stream is being consumed, it is allowed to modify
   * the contents of 'db'; the changed or added entries
   * will be reached if they match the criterion.
   * The iteration terminates normally if no entry currently matches the
   * criterion. [The returned number is the suggested period of time to wait
   * until the next invocation of 'iter' makes sense. ... TODO ]
   *)
