# make all: 		make program
# make clean: 		remove intermediate files

#----------------------------------------------------------------------
# specific rules for this package:

OBJECTS  = sample.cmo
NAME     = sample
REQUIRES = equeue,threads

all: $(OBJECTS)
	$(OCAMLC) -o sample sample.cmo -linkpkg

#----------------------------------------------------------------------
# general rules:

DEBUG     =
OPTIONS   =
OCAMLC    = ocamlfind ocamlc $(DEBUG) $(OPTIONS) -thread -package "$(REQUIRES)"
OCAMLOPT  = ocamlfind ocamlopt $(OPTIONS) $(ROPTIONS) -thread -package "$(REQUIRES)"
OCAMLDEP  = ocamldep $(OPTIONS)
OCAMLFIND = ocamlfind


.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa sample 

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~ a.old a.new

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

