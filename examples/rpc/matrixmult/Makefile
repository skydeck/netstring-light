TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = mm_proto.x

RPCGEN_AUX_OPTIONS    = -int unboxed -hyper int64
RPCGEN_CLIENT_OPTIONS = -int unboxed -hyper int64
RPCGEN_SERVER_OPTIONS = -int unboxed -hyper int64

CLEAN_LIST += *.astamp *.cstamp *.s2stamp *.run *.opt \
              mm_proto_aux.ml mm_proto_aux.mli \
	      mm_proto_clnt.ml mm_proto_clnt.mli \
	      mm_proto_srv.ml mm_proto_srv.mli

all:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

gen: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.s2stamp)
