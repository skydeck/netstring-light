TOP_DIR=../..

PKGNAME  = rpc
OBJECTS  = rpc.cmo rpc_common.cmo \
           rpc_program.cmo rpc_util.cmo rpc_portmapper_aux.cmo \
           rpc_packer.cmo rpc_transport.cmo rpc_client.cmo \
           rpc_simple_client.cmo rpc_portmapper_clnt.cmo rpc_portmapper.cmo \
           rpc_server.cmo rpc_auth_sys.cmo \
	   rpc_auth_gssapi_aux.cmo rpc_auth_gssapi.cmo \
	   rpc_proxy.cmo
DOBJECTS = rpc.mli \
           rpc_program.mli rpc_util.mli rpc_portmapper_aux.mli \
           rpc_packer.mli rpc_transport.mli rpc_client.mli \
           rpc_simple_client.mli rpc_portmapper_clnt.mli rpc_portmapper.mli \
           rpc_server.mli rpc_auth_sys.mli rpc_auth_gssapi.mli \
	   rpc_proxy.mli \
	   rpc_intro.txt rpc_mapping_ref.txt rpc_intro_gss.txt

# rpc_packer.ml is full of incomplete matchings, but this is ok:
OCAMLC_OPTIONS_FOR_rpc_packer.ml = -w p
OCAMLOPT_OPTIONS_FOR_rpc_packer.ml = -w p

# rpc_portmapper.ml is full of incomplete matchings, but this is ok:
OCAMLC_OPTIONS_FOR_rpc_portmapper.ml = -w p
OCAMLOPT_OPTIONS_FOR_rpc_portmapper.ml = -w p

#OCAMLDEP_OPTIONS = -syntax camlp4o -package camlp4.macro

INCLUDES += $(INC_EQUEUE) $(INC_NETSYS) $(INC_NETSTRING) $(INC_NETGSSAPI)

include $(TOP_DIR)/Makefile.rules

include depend
