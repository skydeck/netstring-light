(* $Id: telnet_client.mli 1612 2011-06-07 23:41:05Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Telnet client
 * 
 * This is a Telnet client providing the basic Telnet services. It
 * supports sending and receiving data (asynchronously), and the
 * negotiation of Telnet options, but it does not implement any option.
 *)

exception Telnet_protocol of exn;;
(** Wrapper for exceptions that already passed the exception handler. *)


type telnet_command =
    Telnet_data of string  (** User data *)
  | Telnet_nop             (** No operation *)
  | Telnet_dm           (** data mark *)
  | Telnet_brk          (** break *)
  | Telnet_ip           (** interrupt process *)
  | Telnet_ao           (** abort output *)
  | Telnet_ayt          (** are you there? *)
  | Telnet_ec           (** erase character *)
  | Telnet_el           (** erase line *)
  | Telnet_ga           (** Go ahead *)
  | Telnet_sb of char   (** Begin of subnegotiation *)
  | Telnet_se           (** End of subnegotation *)
  | Telnet_will of char (** Acknowledges that option is in effect *)
  | Telnet_wont of char (** Acknowledges that option is rejected *)
  | Telnet_do of char   (** Requests to turn on an option *)
  | Telnet_dont of char (** Requests to turn off an option *)
  | Telnet_unknown of char (** Unknown command *)
  | Telnet_eof          (** End of file *)
  | Telnet_timeout      (** Timeout event *)
(** A [telnet_command] is the interpretation of the octets in a Telnet
 * session, i.e. it is one level above the octet stream. See RFC 854
 * for an explanation what the commands mean. [Telnet_data] represents
 * the data chunks between the commands. Note that you do not need
 * to double octets having value 255; this is done automatically.
 * [Telnet_unknown] represents any command not covered by RFC 854, for
 * example the End-of-record-mark (introduced in RFC 885) would be
 * [Telnet_unknown '\239']. [Telnet_eof] represents the end of the octet
 * stream, useable in both directions. [Telnet_timeout] is added to the
 * input queue if I/O has not been happened for the configured period
 * of time.
 *)

type telnet_options =
    { connection_timeout : float;
      verbose_input : bool;
      verbose_output : bool;
    }
(** [telnet_options]: modifies the behaviour of the client. Do not mix these
 * options up with the options negotiated with the remote side.
 *
 * - [connection_timeout]:   After this period of time (in seconds) a
 *                           [Telnet_timeout] pseudo-command is added to
 *                           the input queue, and the connection is
 *                           aborted.
 * - [verbose_input]:        Enables printing of input events to {!Netlog.Debug}.
 * - [verbose_output]:       Enables printing of output events to {!Netlog.Debug}
 *)


type telnet_negotiated_option =
    Telnet_binary       (** see RFC 856 *)
  | Telnet_echo         (** see RFC 857 *)
  | Telnet_suppress_GA  (** see RFC 858 *)
  | Telnet_status       (** see RFC 859 *)
  | Telnet_timing_mark  (** see RFC 860 *)
  | Telnet_ext_opt_list (** see RFC 861 *)
  | Telnet_end_of_rec   (** see RFC 885 *)
  | Telnet_window_size  (** see RFC 1073 *)
  | Telnet_term_speed   (** see RFC 1079 *)
  | Telnet_term_type    (** see RFC 1091 *)
  | Telnet_X_display    (** see RFC 1096 *)
  | Telnet_linemode     (** see RFC 1184 *)
  | Telnet_flow_ctrl    (** see RFC 1372 *)
  | Telnet_auth         (** see RFC 1416 *)
  | Telnet_new_environ  (** see RFC 1572 and 1571 *)
  | Telnet_option of int   (** all other options *)
(** [telnet_negotiated_option]: names for the most common options, and
 * the generic name [Telnet_option] for other options.
 *)

type telnet_option_state =
    Not_negotiated
  | Accepted
  | Rejected  (** *)
(** An option has one of three states:
 * - [Not_negotiated]: There was no negotiation about the option. This means
 *   that the option is turned off (but this client is allowed to reject
 *   it explicitly)
 * - [Accepted]: Both sides have accepted the option.
 * - [Rejected]: One side has rejected the option. This also means that the
 *   option is off, but the client refuses to send further acknoledgements
 *   that the option is off (to avoid endless negotiation loops).
 *)


val char_of_option : telnet_negotiated_option -> char
(** Converts the option name to the character representing it on the
 * octet-stream level.
 *)

val option_of_char : char -> telnet_negotiated_option
(** Converts a character representing an option to the internal option
 * name.
 *)


type telnet_connector =
    Telnet_connect of (string * int)
  | Telnet_socket of Unix.file_descr (** *)
(** Connectors:
 * - [Telnet_connect(host,port)]: The client connects to this port.
 * - [Telnet_socket s]: The client uses an already connected socket.
 *
 * Why [Telnet_socket]? Telnet is a symmetrical protocol; client and servers
 * implement the same protocol features (the only difference is the
 * environment: a client is typically connected with a real terminal; a server
 * is connected with a pseudo terminal). This simply means that this
 * implementation of a client can also be used as a server implementation.
 * You need only to add code which accepts new connections and which passes
 * these connections over to a [telnet_session] object via [Telnet_socket].
 *)


(** A telnet session *)
class telnet_session :
  object
    (** Overwiew
     *
     * The [telnet_session] object has two queues, one for arriving data,
     * one for data to send.
     * Once the session object is attached to an event system, it connects
     * to the remote peer, and processes the queues. Input is appended to
     * the input queue; output found on the output queue is sent to the
     * other side.
     * If input arrives, a callback function is invoked.
     * You may close the output side of the socket by putting [Telnet_eof]
     * onto the output queue.
     * Once the EOF marker has been received, a [Telnet_eof] is appended to
     * the input queue, and the connection is closed (completely). The
     * session object detaches from the event system automatically in this
     * case.
     * 
     * {b Hints}
     *
     * Set an input handler as callback function in the session object.
     * The input handler is called when new input data have been arrived.
     * It should inspect the input queue, process the queue as much as
     * possible, and it should remove the processed items from the queue.
     * While processing, it may add new items to the output queue. 
     *
     * If you are not within the callback function and add items to the
     * output queue, the session object will not detect that there are
     * new items to send - unless you invoke the [update] method.
     *
     * If you want option negotiation, it is the simplest way to use
     * the special option negotiation methods. Configure the options
     * as you want (invoking [enable], [disable] etc), but do not forget
     * to modify the way input is processed. Every [Telnet_will], [_wont],
     * [_do], and [_dont] command must be passed to [process_option_command].
     *)

    method set_connection : telnet_connector -> unit
	(** Sets the host name and the port of the remote server to contact. *)

    method set_event_system : Unixqueue.event_system -> unit
	(** Sets the event system to use. By default, a private event system
	 * is used.
	 *)

    method set_callback : (bool -> unit) -> unit
	(** Sets the callback function. This function is called after new
	 * commands have been put onto the input queue. 
	 * The argument passed to the callback function indicates whether
	 * a 'Synch' sequence was received from the remote side or not.
	 *
	 * {b Note Synch:} If the client sees a data mark command it will assume
	 * that it is actually a Synch sequence. The client automatically
	 * discards any [Telnet_data] commands from the input queue (but not
	 * [Telnet_data]s inside subnegotiations). The data mark command
	 * itself remains on the queue.
	 *)

    method set_exception_handler : (exn -> unit) -> unit
	(** Sets the exception handler. Every known error condition is
	 * caught and passed to the exception handler.
	 * The exception handler can do whatever it wants to do. If it
	 * raises again an exception, the new exception is always propagated
	 * up to the caller (whoever this is). Often the caller is the
	 * event system scheduler (i.e. [Unixqueue.run]); see the documention
	 * there.
         *
	 * If you do not set the exception handler, a default handler is
	 * active. It first resets the session (see method [reset]), and
	 * then wraps the exception into the [Telnet_protocol] exception,
	 * and raises this exception again.
	 *)

    method output_queue : telnet_command Queue.t
        (** The queue of commands to send to the remote side. If you add new
	 * commands to this queue, do not forget to invoke the [update]
	 * method which indicates to the event system that new data to
	 * send is available.
	 * After commands have been sent, they are removed from the queue.
	 *)

    method input_queue : telnet_command Queue.t
        (** The queue of commands received from the remote side. This class
	 * only adds commands to the queue (and invokes the callback 
	 * function). The user of this class is responsible for removing
	 * commands from the queue which have been processed.
	 *)

    method get_options : telnet_options
        (** Get the configuration options. *)

    method set_options : telnet_options -> unit
	(** Set the configuration options. *)

    method reset : unit -> unit 
        (** Closes the connection immediately and empties all queues.
	 * All negotiated options are reset, too.
	 *)

    (** Telnet options
     *
     * The following methods deal with Telnet protocol options. These
     * are negotiated between local and remote side by the Will, Won't,
     * Do and Don't commands. 
     *
     * The "local" options describe the modification of the behaviour
     * of the local side; the "remote" options describe the modifications
     * of the remote side. Both set of options are independent.
     * This object may track the set of accepted and rejected options
     * if the following methods are used; but this works only if
     * the [Telnet_will], [_wont], [_do], and [_dont] commands received from
     * the remote side are processed by [process_option_command]. So
     * you need to invoke this method for the mentioned commands in
     * your command interpretation loop.
     *
     * The idea is: If you {b enable} an option, it is possible to
     * switch it on. If the remote side requests the option to be enabled,
     * the request will be acknowledged. If the remote side does not
     * request the option, it remains off.
     *
     * You can also actively demand an option ([offer_local_option],
     * [request_remote_option]); this is of course only possible if
     * the option is already enabled. In this case the client tries
     * actively to switch it on.
     *
     * You can also {b disable} an option. If the option is on, the
     * client actively rejects the option; following the Telnet protocol
     * this is always possible (rejections cannot be rejected).
     *
     * The [reset] methods are somewhat dangerous. They simply reset
     * the internal state of the client, but do not negotiate. This
     * possibility was added to allow the Timing Mark option to send
     * again timing marks even if the previous timing marks have
     * already been accepted. After [reset], the client thinks the
     * option was never negotiated; but nothing is done to tell
     * the remote side about this.
     *
     * [option_negotiation_is_over]: true if no option negotiation is
     * pending (i.e. nothing has still to be acknowledged).
     *)

    method enable_local_option : telnet_negotiated_option -> unit
    method enable_remote_option : telnet_negotiated_option -> unit
    method disable_local_option : telnet_negotiated_option -> unit
    method disable_remote_option : telnet_negotiated_option -> unit
    method offer_local_option : telnet_negotiated_option -> unit
    method request_remote_option : telnet_negotiated_option -> unit
    method reset_local_option : telnet_negotiated_option -> unit
    method reset_remote_option : telnet_negotiated_option -> unit
    method get_local_option : telnet_negotiated_option -> telnet_option_state
    method get_remote_option : telnet_negotiated_option -> telnet_option_state
    method option_negotiation_is_over : bool
    method process_option_command : telnet_command -> unit

    method fetch_subnegotiation : string option
      (** This method should be called as follows:
       * If you find a [Telnet_sb] at the beginning of the input queue,
       * remove this command [Queue.take], and invoke [fetch_subnegotiation].
       * This method scans the queue and looks for the associated 
       * [Telnet_se] command. If it does not find it, [None] is returned.
       * If [Telnet_se] is found, the parameter enclosed by the two commands
       * is returned as [Some s] where [s] is the parameter string. Furthermore,
       * in the latter case the data items and the closing [Telnet_se] are
       * removed from the queue.
       *)


    (** Running the session *)

    method attach : unit -> unit
        (** Attach to the event system. After being attached, the client
	 * is ready to work.
	 *)

    method run : unit -> unit
	(** Run the event system *)

    method update : unit -> unit
        (** If there are commands in the output queue, the event system is
	 * signaled that this client wants to do network I/O.
	 *)

    method send_synch : telnet_command list -> unit
        (** At the next output oppurtunity, a Synch sequence is sent to
	 * the remote peer. This means that the passed commands, extended
	 * by an additional Data Mark command, are sent to the peer as
	 * urgent data.
         *
	 * Sending a Synch sequence has higher priority than the output
	 * queue; processing of the output queue is deferred until the
	 * Synch sequence has been completely sent.
	 *)

    method expect_input : bool -> unit
      (** Set whether the timeout value is to be applied to the input side
	  of the connection. This is [true] by default.
       *)
  end
;;

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module  By default,
        the exchanged Telnet commands are logged. This can be extended
        by setting the [verbose_input] and [verbose_output] options.
     *)
end
