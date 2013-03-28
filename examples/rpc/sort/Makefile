TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = sort1_proto.x sort2_proto.x

RPCGEN_AUX_OPTIONS    = -int unboxed -hyper int64
RPCGEN_CLIENT_OPTIONS = -int unboxed -hyper int64
RPCGEN_SERVER_OPTIONS = -int unboxed -hyper int64

CLEAN_LIST += *.astamp *.cstamp *.s2stamp *.run *.opt \
              sort1_proto_aux.ml  sort1_proto_aux.mli \
	      sort1_proto_clnt.ml sort1_proto_clnt.mli \
	      sort1_proto_srv.ml  sort1_proto_srv.mli \
              sort2_proto_aux.ml  sort2_proto_aux.mli \
	      sort2_proto_clnt.ml sort2_proto_clnt.mli \
	      sort2_proto_srv.ml  sort2_proto_srv.mli

all:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile1.code all
	$(MAKE) -f Makefile2.code all

opt:
	$(MAKE) gen
	$(MAKE) depend
	$(MAKE) -f Makefile1.code opt
	$(MAKE) -f Makefile2.code opt

gen: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.s2stamp)
