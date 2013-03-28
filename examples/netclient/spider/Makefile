#----------------------------------------------------------------------
# specific rules for this package:

SOURCES  = html.ml scan.ml parse.ml database.ml agent.ml
OBJECTS  = $(SOURCES:.ml=.cmo)
XOBJECTS = $(OBJECTS:.cmo=.cmx)
ARCHIVE  = spider.cma
XARCHIVE = $(ARCHIVE:.cma=.cmxa)
NAME     = spider
REQUIRES = netclient

all: $(ARCHIVE)
	$(OCAMLC) -o spider -custom $(ARCHIVE) main.ml -linkpkg

$(ARCHIVE): $(OBJECTS)
	$(OCAMLC) -a -o $(ARCHIVE) $(OBJECTS)

$(XARCHIVE): $(XOBJECTS) 
	$(OCAMLOPT) -a -o $(XARCHIVE) $(XOBJECTS)

#----------------------------------------------------------------------
# general rules:

OPTIONS   = -syntax camlp4o -package camlp4
OCAMLC    = $(OCAMLFIND) ocamlc $(DEBUG) $(OPTIONS) -package "$(REQUIRES)"
OCAMLOPT  = $(OCAMLFIND) ocamlopt $(OPTIONS)        -package "$(REQUIRES)"
OCAMLDEP  = $(OCAMLFIND) ocamldep $(OPTIONS)
OCAMLLEX  = ocamllex
OCAMLFIND = ocamlfind

DEBUG  =
# Invoke with: make DEBUG=-g

depend: $(SOURCES) *.mli
	$(OCAMLDEP) *.ml *.mli >depend

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa spider

.PHONY: distclean
distclean: clean
	rm -f *~ depend depend.pkg db out 

.SUFFIXES: .cmo .cmi .cmx .ml .mli .mll

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.mll.ml:
	$(OCAMLLEX) $<

*.mli:
	true

include depend
