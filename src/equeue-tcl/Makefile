TOP_DIR=../..

PKGNAME  = equeue-tcl
ARCHIVE  = equeue_tcl
OBJECTS  = uq_tcl.cmo
DOBJECTS = uq_tcl.mli
COBJECTS = tclqueue.o

LINK_OPTIONS = $(EQUEUE_TCL_LIBS)
CC_OPTIONS = $(EQUEUE_TCL_DEFS)

INCLUDES += $(INC_EQUEUE)
INCLUDES += $(INC_NETSYS)

include $(TOP_DIR)/Makefile.rules

include depend
