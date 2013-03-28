
TOP_DIR=../../..
# No archive to make, only programs:
ARCHIVE_CMA=
ARCHIVE_CMXA=
ARCHIVE=

include $(TOP_DIR)/Makefile.rules

INCLUDES += $(INC_NETSTRING) $(INC_NETSYS) $(INC_NETCGI2) $(INC_NETCGI2_APACHE)

CRYPTOKIT_PATH= $(shell $(OCAMLFIND) query cryptokit)
ifneq "$(CRYPTOKIT_PATH)" ""
CRYPTOKIT_LIBS= nums.cma cryptokit.cma
INCLUDES += -I $(CRYPTOKIT_PATH)
endif

PCRE_PATH= $(shell $(OCAMLFIND) query pcre)
ifneq "$(PCRE_PATH)" ""
PCRE_LIBS= pcre.cma
INCLUDES += -I $(PCRE_PATH)
endif

LIBS_CMA	= str.cma $(PCRE_LIBS) unix.cma bigarray.cma netsys_oothr.cmo netsys.cma netstring.cma \
  $(CRYPTOKIT_LIBS)
LIBS_CMXA1	= $(LIBS_CMA:.cma=.cmxa)
LIBS_CMXA 	= $(LIBS_CMXA1:.cmo=.cmx)
OCAMLC_FLAGS	= -dtypes $(INCLUDES)
OCAMLOPT_FLAGS	= -dtypes $(INCLUDES)


PROGRAMS=counter_cgi.ml counter_fcgi.ml add_cgi.ml add_fcgi.ml \
  icfp2001_cgi.ml

ifneq "$(CRYPTOKIT_PATH)" ""
PROGRAMS += filemanager_cgi.ml filemanager_fcgi.ml filemanager_scgi.ml
endif

ifneq "$(APACHE_MAJOR)" ""
PROGRAMS += counter_apache.cma add_apache.cma
ifneq "$(CRYPTOKIT_PATH)" ""
PROGRAMS += filemanager_apache.cma
endif
endif

counter_apache.cma: counter.cmo counter_apache.cmo
	$(OCAMLC) -a -o counter_apache.cma \
		$(OCAMLC_FLAGS) counter.cmo counter_apache.cmo

######################################################################

.PHONY: all opt byte
all: byte opt
# Extensions choosen for M$win
byte: $(PROGRAMS:.ml=.exe)
opt: $(PROGRAMS:.ml=.com)
mod: $(MOD_PROGRAMS)



%_cgi.exe: %.cmo %_cgi.cmo
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) netcgi.cma $^
%_fcgi.exe: %.cmo %_fcgi.cmo
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) netcgi.cma $^
%_scgi.exe: %.cmo %_scgi.cmo
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) netcgi.cma $^
%_ajp.exe: %.cmo %_ajp.cmo
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) netcgi.cma $^

%_cgi.com: %.cmx %_cgi.cmx
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMXA) netcgi.cmxa $^
%_fcgi.com: %.cmx %_fcgi.cmx
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMXA) netcgi.cmxa $^
%_scgi.com: %.cmx %_scgi.cmx
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMXA) netcgi.cmxa $^
%_ajp.com: %.cmx %_ajp.cmx
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMXA) netcgi.cmxa $^

%.cma: %.cmo
	$(OCAMLC) -a -o $@ $(OCAMLC_FLAGS) $^

include depend

clean::
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp *.exe *.com
