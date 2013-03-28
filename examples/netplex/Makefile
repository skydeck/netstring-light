TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = operation.x

RPCGEN_AUX_OPTIONS    = -int int32 -hyper int64
RPCGEN_CLIENT_OPTIONS = -int int32 -hyper int64
RPCGEN_SERVER_OPTIONS = -int int32 -hyper int64

CLEAN_LIST += *.astamp *.cstamp *.s2stamp
CLEAN_LIST += $(XDRFILES:.x=_aux.ml) $(XDRFILES:.x=_aux.mli)
CLEAN_LIST += $(XDRFILES:.x=_clnt.ml) $(XDRFILES:.x=_clnt.mli)
CLEAN_LIST += $(XDRFILES:.x=_srv.ml) $(XDRFILES:.x=_srv.mli)
CLEAN_LIST += test_client helper_container

all:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

gen: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.s2stamp)
