# $Id: Makefile,v 1.4 2005/09/04 22:10:24 chris_77 Exp $
TOP_DIR=../../..

include $(TOP_DIR)/Makefile.rules

INCLUDES += $(INC_NETSTRING) $(INC_NETSYS) $(INC_NETCGI2) $(INC_NETCGI2_APACHE)

LIBS_CMA	= str.cma pcre.cma unix.cma netsys.cma netstring.cma \
  netcgi.cma netcgi_apache.cma
OCAMLC_FLAGS	= -dtypes $(INCLUDES)
OCAMLOPT_FLAGS	= -dtypes $(INCLUDES)


PROGRAMS=hello.ml params.ml


######################################################################

.PHONY: all opt byte
all: byte opt
# Extensions choosen for M$win
byte: $(PROGRAMS:.ml=.exe)
opt: $(PROGRAMS:.ml=.com)



%.exe: %.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) \
	  $(filter-out %.ml, $^) $(filter %.ml, $^)
%.com: %.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMA:.cma=.cmxa) \
	  $(filter-out %.ml, $^) $(filter %.ml, $^)

include depend

clean::
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp *.exe *.com
