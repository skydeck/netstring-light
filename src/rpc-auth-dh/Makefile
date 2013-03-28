TOP_DIR=../..

PKGNAME  = rpc-auth-dh
ARCHIVE  = rpc_auth_dh
OBJECTS  = rpc_time.cmo \
           rpc_key_aux.cmo rpc_key_clnt.cmo rpc_key_service.cmo rpc_auth_dh.cmo
DOBJECTS = rpc_auth_dh.mli rpc_key_service.mli rpc_time.mli

include $(TOP_DIR)/Makefile.rules

INCLUDES += $(INC_RPC) $(INC_EQUEUE) $(INC_NETSYS) $(INC_NETSTRING) \
            -I ../../src/rpc-xti
REQUIRES += cryptgps

include depend
