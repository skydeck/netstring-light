(* $Id: netplex_kit.mli 1652 2011-08-03 21:50:30Z gerd $ *)

(** Netplex toolkit *)

open Netplex_types


(** Same as [processor], but the methods [process] and [supported_ptypes]
  * are flagged as [virtual]
 *)
class type virtual v_processor =
object
  inherit processor_hooks

  method virtual process : 
           when_done:(unit -> unit) ->
           container -> Unix.file_descr -> string -> unit
  method virtual supported_ptypes : parallelization_type list
end


class empty_processor_hooks : unit -> processor_hooks
  (** This is an empty set of processor hooks, i.e. all methods are empty
   *)

class processor_hooks_delegation : processor_hooks -> processor_hooks
  (** Takes a hooks object, and makes a class of it. Useful for overriding
      methods in an object.
   *)

class virtual processor_base :  processor_hooks -> v_processor
  (** A virtual (incomplete) base class for processors. As argument the
    * user-supplied hooks are passed in. Use this class as in:
    *
    * {[
    *    class my_processor hooks =
    *    object(self)
    *      inherit Netplex_kit.processor_base hooks
    *      method process ~when_done container fd proto_name = ...
    *      method supported_ptypes = ...
    *    end
    * ]}
    *
    * In order to run actions from hooks, redefine the hook methods as in:
    *
    * {[
    *    class my_processor hooks =
    *    object(self)
    *      inherit Netplex_kit.processor_base hooks as super
    *      method process ~when_done container fd proto_name = ...
    *      method supported_ptypes = ...
    *      method post_start_hook container =
    *        ... (* my action *);
    *        super # post_start_hook container
    *    end
    * ]}
   *)

class protocol_switch_processor : (string * processor) list -> processor
  (** The arg is a list of pairs [(proto_name, proto_proc)]. All mentioned
      processors are merged into a single processor. When a TCP connection
      arrives, it depends on the protocol which processor is actually
      activated. (Every socket is bound to a protocol, so this can be derived
      from the socket.)

      It is up to the user whether the merge makes sense.
   *)

class protocol_switch_factory : string ->
                                (string * processor_factory) list ->
                                processor_factory
  (** [protocol_switch_factory name merge_list]: Merges the factories
      in [merge_list] to a single factory. Which factory is selected
      depends on the protocol.

      For example:

      {[
          service {
            name = "...";
            protocol {
               name = "A"; ...;
            }
            protocol {
               name = "B"; ...;
            }
            processor {
               type = "merged";
               A {
                  ...
               }
               B {
                  ...
               }
            }
          }
      ]}
                                  
      Here, two protocols [A] and [B] are defined, and there is a
      subsection in [processor] for each of the protocols configuring
      the used service. "merged" is the [name] of the merged factories.

      For example, [A] could be an RPC interface, and [B] could be
      an HTTP interface providing the same service.

      For every protocol in [merge_list] there must be a subsection in
      [processor] for the protocol. This subsection configures then
      the processor. It is not an error not to create sockets for
      a protocol in [merge_list].
   *)


val add_helper_service : controller -> string -> processor_hooks -> unit
  (** [add_helper_service ctrl name hooks]: Adds a helper service [name] to
      the controller
      [ctrl]. The helper service does not have any externally
      accessible socket, but starts a single regular container that looks
      like any other container. Whatever needs to be initialized must be
      done in the [pre_start_hook] or the [post_start_hook].

      This function must be called in controller context, for example
      in the [late_initializer] of {!Netplex_main.startup}, but it can
      also be started later.

      For an example, look at [examples/netplex/helper_container.ml] in
      the distributed source tarball.

      For multi-threaded programs, {!Netplex_cenv.run_in_controller_context}
      is the required companion function to start helper threads at any
      time. Multi-processing programs do not have such an easy way to 
      add helpers. They should it at program startup time.

      {b Known bug.} The the helper component will be in "starting" state as 
      long as the [post_start_hook] runs.
   *)

val create_protocol : ?lstn_backlog:int -> 
                      ?lstn_reuseaddr:bool ->
                      ?so_keepalive:bool -> 
                      ?tcp_nodelay:bool ->
                      ?configure_slave_socket:(Unix.file_descr ->unit) ->
                      string ->
                      extended_address array ->
                        protocol
  (** [create_protocol name addresses]: Creates a [protocol] object
      from the passed arguments
   *)
			
val create_socket_service_config : 
                      ?startup_timeout:float ->
                      ?change_user_to:(int*int) ->
                      ?gc_when_idle:bool ->
                      ?conn_limit:int ->
                      string ->
                      protocol list ->
                      controller_config ->
                        socket_service_config
  (** [create_socket_service_config name protos ctrl_conf]: Creates a
      [socket_service_config] object from the passed arguments
   *)

