(* $Id: xdr_mstring.mli 1558 2011-03-04 17:15:46Z gerd $ *)

(** Managed Strings *)

(** Managed strings are used in XDR context for constant strings that
    are stored either as string or as memory (bigarray of char).

    A managed string [ms] is declared in the XDR file as in

    {[
      typedef _managed string ms<>;
    ]}

    In the encoded XDR stream there is no difference between strings and
    managed strings, i.e. the wire representation is identical. Only
    the Ocaml type differs to which the managed string is mapped. This
    type is {!Xdr_mstring.mstring} (below).

    In the RPC context there is often the problem that the I/O backend
    would profit from a different string representation than the user of
    the RPC layer. To bridge this gap, managed strings have been invented.
    Generally, the user can determine how to represent strings (usually
    either as an Ocaml string, or as memory), and the I/O backend
    can request to transform to a different representation when this
    leads to an improvement (i.e. copy operations can be saved).

    Only large managed strings result in a speedup of the program
    (at least several K).

    {2 How to practically use managed strings}

    There are two cases: The encoding case, and the decoding case.
    In the encoding case the [mstring] object is created by the user
    and passed to the RPC library. This happens when a client prepares
    an argument for calling a remote procedure, or when the server
    sends a response back to the caller. In the decoding case the client
    analyzes the response from an RPC call, or the server looks at the
    arguments of an RPC invocation. The difference here is that in the
    encoding case user code can directly create [mstring] objects by
    calling functions of this module, whereas in the decoding case the
    RPC library creates the [mstring] objects.

    For simplicity, let us only look at this problem from the perspective
    of an RPC client.

    {b Encoding.} Image a client wants to call an RPC, and one of the
    arguments is a managed string. This means we finally need an [mstring]
    object that can be put into the argument list of the call.

    This library supports two string representation specially: The normal
    Ocaml [string] type, and {!Netsys_mem.memory} which is actually just
    a bigarray of char's. There are two factories [fac],

     - {!Xdr_mstring.string_based_mstrings}, and
     - {!Xdr_mstring.memory_based_mstrings},

    and both can be used to create the [mstring] to pass to the
    RPC layer. It should be noted that this layer can process the
    [memory] representation a bit better. So, if the original [data]
    value is a string, the factory for [string] should be used, and
    if it is a char bigarray, the factory for [memory] should be used.
    Now, the [mstring] object is created by

     - [let mstring = fac # create_from_string data pos len copy_flag], or by
     - [let mstring = fac # create_from_memory data pos len copy_flag].

    Of course, if [fac] is the factory for strings, the [create_from_string]
    method works better, and if [fac] is for [memory], the [create_from_memory]
    method works better. [pos] and [len] can select a substring of [data].
    If [copy_flag] is [false], the [mstring] object does not copy the data
    if possible, but just keeps a reference to [data] until it is accessed;
    otherwise if [copy_flag] is [true], a copy is made immediately.
    Of couse, delaying the copy is better, but this requires that [data]
    is not modified until the RPC call is completed.

    {b Decoding.} Now, the call is done, and the client looks at the
    result. There is also an [mstring] object in the result. As noted
    above, this [mstring] object was already created by the RPC library
    (and currently this library prefers string-based objects if not
    told otherwise). The user code can now access this [mstring]
    object with the access methods of the [mstring] class (see below).
    As these methods are quite limited, it makes normally only sense
    to output the [mstring] contents to a file descriptor.

    The user can request a different factory for managed strings. The 
    function {!Rpc_client.set_mstring_factories} can be used for this
    purpose. (Similar ways exist for managed clients, and for RPC servers.)

    {b Potential.} Before introducing managed strings, a clean analysis
    was done how many copy operations can be avoided by using this
    technique. Example: The first N bytes of a file are taken as 
    argument of an RPC call. Instead of reading these bytes into a
    normal Ocaml string, an optimal implementation uses now a [memory]
    buffer for this purpose. This gives:

    - Old implementation with strings and ocamlnet-2:
      Data is copied {b six} times from reading it from the file until
      writing it to the socket.
    - New implementation with memory-based mstrings:
      Data is copied only {b twice}! The first copy reads it from the
      file into the input buffer (a [memory] value), and the second copy
      writes the data into the socket.

    Part of the optimization is that [Unix.read] and [Unix.write]
    do a completely avoidable copy of the data which is prevented by
    switching to {!Netsys_mem.mem_read} and {!Netsys_mem.mem_write},
    respectively. The latter two functions exploit an optimization
    that is only possible when the data is [memory]-typed.

    The possible optimizations for the decoding side of the problem
    are slightly less impressive, but still worth doing it.
 *)

(** {2 Interface} *)

open Netsys_mem

(** The object holding the string value *)
class type mstring =
object
  method length : int
    (** The length of the managed string *)

  method blit_to_string :  int -> string -> int -> int -> unit
    (** [blit_to_string mpos s spos len]: Copies the substring of the
	managed string from [mpos] to [mpos+len-1] to the substring of
	[s] from [spos] to [spos+len-1]
     *)

  method blit_to_memory : int -> memory -> int -> int -> unit
    (** [blit_to_string mpos mem mempos len]: Copies the substring of the
	managed string from [mpos] to [mpos+len-1] to the substring of
	[mem] from [mempos] to [mempos+len-1]
     *)

  method as_string : string * int
    (** Returns the contents as string. It is undefined whether the returned
	string is a copy or the underlying buffer. The int is the position
	where the contents start
     *)

  method as_memory : memory * int
    (** Returns the contents as memory. It is undefined whether the returned
	memory is a copy or the underlying buffer. The int is the position
	where the contents start
     *)

  method preferred : [ `Memory | `String ]
    (** Whether [as_memory] or [as_string] is cheaper *)

end


(** The object creating new [mstring] objects *)
class type mstring_factory =
object
  method create_from_string : string -> int -> int -> bool -> mstring
    (** [create_from_string s pos len must_copy]: Creates the [mstring] from the
	sub string of s starting at [pos] with length [len]

	If [must_copy] the mstring object must create a copy. Otherwise
	it can just keep the string passed in.
     *)

  method create_from_memory : memory -> int -> int -> bool -> mstring
    (** [create_from_memory m pos len must_copy]: Creates the [mstring] from the
	sub string of m starting at [pos] with length [len]

	If [must_copy] the mstring object must create a copy. Otherwise
	it can just keep the memory passed in.
     *)

end

val string_based_mstrings : mstring_factory
  (** Uses strings to represent mstrings *)

val string_to_mstring : ?pos:int -> ?len:int -> string -> mstring
  (** Represent a string as mstring (no copy) *)

val memory_based_mstrings : mstring_factory
  (** Uses memory to represent mstrings. The memory bigarrays are allocated
      with [Bigarray.Array1.create]
   *)

val memory_to_mstring : ?pos:int -> ?len:int -> memory -> mstring
  (** Represent memory as mstring (no copy) *)

val paligned_memory_based_mstrings : mstring_factory
  (** Uses memory to represent mstrings. The memory bigarrays are allocated
      with {!Netsys_mem.alloc_memory_pages} if available, and 
      [Bigarray.Array1.create] if not.
   *)

val memory_pool_based_mstrings : Netsys_mem.memory_pool -> mstring_factory
  (** Uses memory to represent mstrings. The memory bigarrays are obtained
      from the pool. The length of these mstrings is limited by the 
      blocksize of the pool.
   *)

val length_mstrings : mstring list -> int
  (** returns the sum of the lengths of the mstrings *)

val concat_mstrings : mstring list -> string
  (** concatenates the mstrings and return them as single string. The returned
      string may be shared with one of the mstrings passed in.
   *)

val prefix_mstrings : mstring list -> int -> string
  (** [prefix_mstrings l n]: returns the first [n] chars of the 
      concatenated mstrings [l] as single string
   *)

val blit_mstrings_to_memory : mstring list -> memory -> unit
  (** blits the mstrings one after the other to the memory, so that
      they appear there concatenated
   *)

val shared_sub_mstring : mstring -> int -> int -> mstring
  (** [shared_sub_mstring ms pos len]: returns an mstring that includes
      a substring of [ms], starting at [pos], and with [len] bytes. 
      The returned mstring shares the buffer with the original mstring [ms]
   *)

val shared_sub_mstrings : mstring list -> int -> int -> mstring list
  (** Same for a list of mstrings *)

val copy_mstring : mstring -> mstring
  (** Create a copy *)

val copy_mstrings : mstring list -> mstring list
  (** Create a copy *)


type named_mstring_factories =
    (string, mstring_factory) Hashtbl.t
