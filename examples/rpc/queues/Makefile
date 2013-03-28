TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = queues.x

RPCGEN_AUX_OPTIONS    = -int int32 -hyper int64
RPCGEN_CLIENT_OPTIONS = -int int32 -hyper int64
RPCGEN_SERVER_OPTIONS = -int int32 -hyper int64

CLEAN_LIST += qclient qserver *.astamp *.cstamp *.s2stamp

all:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

gen: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.s2stamp)

