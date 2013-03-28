TOP_DIR=../..

PKGNAME = equeue
OBJECTS = equeue.cmo unixqueue_util.cmo unixqueue_pollset.cmo \
          unixqueue_select.cmo unixqueue.cmo \
	  uq_resolver.cmo \
          uq_engines.cmo uq_socks5.cmo uq_io.cmo uq_lwt.cmo \
	  uq_libevent.cmo uq_mt.cmo
DOBJECTS = equeue.mli unixqueue.mli unixqueue_pollset.mli \
           unixqueue_select.mli \
	   uq_resolver.mli \
           uq_engines.mli uq_socks5.mli uq_io.mli uq_lwt.mli \
	   uq_libevent.mli uq_mt.mli \
           equeue_intro.txt equeue_howto.txt

INCLUDES += $(INC_NETSYS)
INCLUDES += $(INC_NETSTRING)

include $(TOP_DIR)/Makefile.rules

include depend
