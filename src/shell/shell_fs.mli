(* $Id: shell_fs.mli 1661 2011-08-28 22:45:55Z gerd $ *)

(** Shell filesystem *)

(** This module emulates a filesystem by submitting shell commands.
    For example, a directory listing is retrieved via the [ls]
    utility instead of opening the directory directly. This also
    works when logging in to a remote machine, e.g. via [ssh].

    The following standard POSIX commands are used:
    - [dd] with options [if], [of], [bs], [skip], [conv=notrunc], 
      and optionally [excl] (the latter is a GNU extension)
    - [test] with options [-d], [-e], [-f], [-r], [-s], [-w], [-x]
    - [ls] with options [-1], [-n], [-d], [-a], [-L]
    - [rm] with options [-r] and [-f]
    - [mv] with option [-f]
    - [ln] with option [-s]
    - [mkdir] with options [-p]
    - [rmdir]
    - [cp] with option [-p]

    In addition to this, the commands may be embedded in one-line shell
    scripts.

    Filenames with leading minus chars are not supported.

    Error reporting is less accurate than for a local filesystem.

    {b Example.} List a directory on a remote system:

    {[
      let fs = 
        Shell_fs.shell_fs 
          (Shell_fs.ssh_interpreter ~host:"moon" ())
      let files =
        fs # readdir [] "/this/is/a/directory/on/moon"
    ]}
 *)


type command_context =
    { sfs_command : string;          (** The command line *)
      sfs_stdin : Shell.producer;    (** stdin from here *)
      sfs_stdout : Shell.consumer;   (** stdout goes here *)
      sfs_stderr : Shell.consumer;   (** stderr goes here *)
      mutable sfs_status : Unix.process_status option; (** The exit code is put here *)
    }

type command_interpreter
  (** The interpreter runs the command, and fills in [sfs_status] *)

val local_interpreter : unit -> command_interpreter
  (** Executes commands on the local machine *)

val cmd_interpreter : (command_context -> Shell_sys.command list) -> 
                       command_interpreter
  (** Creates a command interpreter from a function that creates the
      real command (as pipeline) to execute
   *)

val ssh_interpreter : ?options:string list -> ?user:string -> host:string ->
                      unit -> command_interpreter
  (** Executes commands via ssh on the machine [host] as [user] (defaults
      to current user). [options] are further command-line options.
      By default, only [-o BatchMode yes] is passed.
   *)

class type shell_stream_fs =
object
  inherit Netfs.stream_fs

  method last_stderr : string
    (** The stderr output of the last operation *)
end


class shell_fs : ?encoding:Netconversion.encoding -> ?root:string -> 
                 ?dd_has_excl:bool ->
                 ?tmp_directory:string -> ?tmp_prefix:string ->
                 command_interpreter -> shell_stream_fs
val shell_fs : ?encoding:Netconversion.encoding -> ?root:string -> 
               ?dd_has_excl:bool ->
               ?tmp_directory:string -> ?tmp_prefix:string ->
               command_interpreter -> shell_stream_fs
  (** The shell filesystem.

      - [encoding]: the assumed character encoding of the filenames.
        [None] by default.
      - [root]: the root of the file tree that is accessed. This can
        be an absolute path, or a relative path. 
      - [dd_has_excl]: whether the [dd] command support "conv=excl".
        Default is [false]; this is a GNU extension.
      - [tmp_directory] and [tmp_prefix] are only meaningful for
        the [write_file] method which creates a temporary file.
        See {!Netchannels.make_temporary_file} for more information.
     
   *)


(** {2 Utility functions for developing other implementations of 
    [shell_stream_fs]} *)

val execute : command_interpreter -> command_context -> unit
  (** Starts this command. It is not waited until the command is finished.
      One can either call [wait] for this, or one of the adapter
      functions below.

      One can only start one command at a time.
   *)

val wait : command_interpreter -> unit
  (** Waits until the running command is finished *)

val output_stream_adapter : ci:command_interpreter -> 
                           close_in:(unit -> unit) ->
                           skip:int64 ->
                             Shell.consumer * Netchannels.in_obj_channel
  (** Arranges that the output of a shell command is made available as
      an [in_obj_channel]:

      {[ let (c, ch) = output_stream_adapter ~ci ~close_in ~skip ]}

      The consumer [p] can be used in a [command_context] for either
      [sfs_stdout] or [sfs_stderr]. The channel [ch] is an input
      channel, and when reading from it will return the bytes of
      stdout or stderr.

      [close_in] is called as post-hook when the [close_in] method of
      [ch] is called. 

      [skip] bytes of stdout/stderr are skipped at the beginning of the
      stream.
   *)

val input_stream_adapter : ci:command_interpreter ->
                           close_out:(unit -> unit) ->
                             Shell.producer * Netchannels.out_obj_channel
  (** Arranges that the input of a shell command is made available as
      an [out_obj_channel]:

      {[ let (p, ch) = input_stream_adapter ~ci ~close_in ]}

      The producer [p] can be used in a [command_context] for 
      [sfs_stdin]. The channel [ch] is an output
      channel, and bytes written to it will appear in stdin of the
      executed command.

      [close_out] is called as post-hook when the [close_out] method of
      [ch] is called. 
   *)
