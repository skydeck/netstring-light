(* $Id: ftp_data_endpoint.mli 434 2005-07-27 20:32:41Z gerd $ *)

(** Senders and receivers for the FTP data connection *)


(* *********************************************************************)
(** {1 Types and Exceptions} *)


(** An [out_record_channel] can be used to output files with record
 * structure. This is purely abstract, as Unix does not support such
 * files natively, so this kind of channel is usually mapped to a flat
 * representation when stored in a real file, e.g. record boundaries 
 * become newline characters.
 *)
class type out_record_channel =
object
  inherit Netchannels.out_obj_channel
  method output_eor : unit -> unit
    (** Finishes the current record. 
     *
     * The record model is as follows: At the beginning of the channel,
     * or after [output_eor], a new record can be potentially started.
     * However, the record counts only as existing when either at least
     * one byte is output, or the record is explicitly ended with a
     * [output_eor] call.
     *
     * This rule allows it to represent channels containing no records.
     * There is an ambiguity at the end of the channel, however: It is
     * possible that there are characters between the last EOR and the
     * EOF. This could also be represented by printing another EOR just
     * before EOF. The preferred representation is the latter.
     *
     * This model correlates to the usual rules for line-structured
     * files, so think EOR = EOL.
     *)
end

(** An [in_record_channel] can be used to read files with record
 * structure. This is purely abstract, as Unix does not support such
 * files natively, so this kind of channel is usually mapped to a flat
 * representation when stored in a real file, e.g. record boundaries 
 * become newline characters.
 *)
class type in_record_channel =
object
  inherit Netchannels.in_obj_channel
  method input_eor : unit -> unit
    (** Skips over the remaining data of the current record and the
     * record boundary to the next record. Raises [End_of_file] when
     * the current record is the "EOF" record (see below for explanations).
     *
     * A record channel can be read as follows: After opening the channel,
     * one can read the contents of the first record with the [input]
     * method. The end of the record is indicated by an [End_of_file]
     * exception. By calling [input_eor], the next record is opened
     * and can be read.
     *
     * After the last real record, there is always a special 
     * "EOF" record which is empty, and must be ignored by applications
     * processing records. This means, after opening an empty channel,
     * the current record is this "EOF" record, and [input_eor] raises
     * [End_of_file]. After reading a non-empty channel, one can
     * do [input_eor] after the last regular record, and the following
     * [input_eor] raises [End_of_file].
     *)
end

type local_receiver =
    [ `File_structure of Netchannels.out_obj_channel
    | `Record_structure of out_record_channel
    ]
    (** The [local_receiver] is the object that gets the data received
     * over the data connection.
     *
     * Page structure is not supported.
     *)

type local_sender =
    [ `File_structure of Netchannels.in_obj_channel
    | `Record_structure of in_record_channel
    ]
    (** The [local_sender] is the object that provides the data sent
     * over the data connection.
     *
     * Page structure is not supported.
     *)

type transmission_mode =
    [ `Stream_mode
    | `Block_mode
    ]
    (** The transmission mode as described in RFC 959. 
     * Compressed mode is not supported.
     *)


type descr_state =
    [ `Clean
    | `Transfer_in_progress
    | `Down
    ]
    (** Describes the state of the socket used for data transfers.
     * The state [`Clean] means that a new data transfer may be started,
     * either because the socket is new, or the last block transfer was
     * properly finished. The state [`Transfer_in_progress] means that
     * data is being transferred, but also that the transfer is aborted.
     * The state [`Down] means that the socket is already at least half-closed,
     * i.e. EOF was sent in one direction.
     *)

type text_data_repr =
    [ `ASCII      of Netconversion.encoding
    | `ASCII_unix of Netconversion.encoding
    | `EBCDIC     of Netconversion.encoding
    ]
    (** Possible representation of text data:
     * [`ASCII] means an ASCII-compatible encoding where the newline
     * character is represented by CR/LF. [`ASCII_unix] is the same
     * but newline is only LF. [`EBCDIC] is an EBCDIC variant.
     *
     * The argument specifies the exact variant to be used, e.g.
     * [`ASCII `Enc_iso88591] or [`EBCDIC `Enc_cp1047].
     *
     * It is illegal to use [`ASCII] or [`ASCII_unix] with an ASCII-
     * incompatible encoding, as well as combining  [`EBCDIC] with a
     * non-EBCDIC encoding. Wrong conversions would be the result of
     * this.
     *)

(* *********************************************************************)
(** {1 Data Stream Converters} *)


class write_out_record_channel : 
        repr:text_data_repr -> 
	Netchannels.out_obj_channel -> 
	  out_record_channel
  (** Provides an [out_record_channel] that represents EOR as 
   * newline character.
   *
   * @param repr Determines the newline character to use
   *)


class read_in_record_channel :
        repr:text_data_repr ->
	Netchannels.in_obj_channel ->
	  in_record_channel
  (** Provides an [in_record_channel] that takes newline characters as
   * EOR representation.
   *
   * In this implementation, [input_line] can be used to read the full
   * contents of a record (=line). However, [input_line] does not
   * switch to the next record.
   *
   * @param repr Determines the newline character to use
   *)



class data_converter : 
        fromrepr:text_data_repr -> torepr:text_data_repr -> 
	  Netchannels.io_obj_channel
  (** Creates a data conversion pipe converting [fromrepr] to
   * [torepr].
   *
   * For simplicity, in an [`ASCII] input stream the CR characters are
   * discarded, and the LF characters are taken as newline characters.
   * In an output [`ASCII] stream, the CR/LF sequence is correctly
   * created for newline characters.
   *)


(* *********************************************************************)
(** {1 Engines} *)


(** The common type of FTP data engines *)
class type ftp_data_engine =
object
  inherit [unit] Uq_engines.engine
  method descr : Unix.file_descr
    (** The socket to use for data transfers. This class never closes
     * this socket, but it may be shut down at least partially.
     *)

  method descr_state : descr_state
    (** The socket state *)
end    


(** This engine receives data on a FTP data connection, and forwards
 * them to a local receiver. The socket must already be open.
 *
 * It is ensured that [local_receiver] is always closed after operation
 * (whether successful or not). The socket [descr] remains open.
 *)
class ftp_data_receiver : 
        esys:Unixqueue.event_system ->
	mode:transmission_mode ->
	local_receiver:local_receiver ->
	descr:Unix.file_descr ->
	unit ->
object
  inherit ftp_data_engine

  method local_receiver : local_receiver
    (** The local receiver. It is closed when the logical EOF is found in the
     * data connection
     *)

end


(** This engine sends data over a FTP data connection coming from
 * a local sender. The socket must already be open.
 *
 * It is ensured that [local_sender] is always closed after operation
 * (whether successful or not). The socket [descr] remains open.
 *)
class ftp_data_sender :
        esys:Unixqueue.event_system ->
	mode:transmission_mode ->
	local_sender:local_sender ->
	descr:Unix.file_descr ->
	unit ->
object
  inherit ftp_data_engine

  method local_sender : local_sender
    (** The local sender. It is closed after usage.
     *)
end
