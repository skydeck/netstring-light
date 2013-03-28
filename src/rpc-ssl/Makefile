TOP_DIR=../..

PKGNAME  = rpc-ssl
ARCHIVE  = rpc_ssl
OBJECTS  = rpc_ssl.cmo
DOBJECTS = rpc_ssl.mli

INCLUDES += $(INC_RPC) $(INC_EQUEUE) $(INC_EQUEUE_SSL) ${INC_NETSYS}

include $(TOP_DIR)/Makefile.rules

REQUIRES += unix ssl

include depend
