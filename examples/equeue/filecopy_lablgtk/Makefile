# make all: 		make program
# make clean: 		remove intermediate files

# Call with
#   make all REQUIRES=equeue-gtk2
# to compile for lablgtk2.

#----------------------------------------------------------------------
# specific rules for this package:

OBJECTS  = filecopy.cmo
NAME     = filecopy
REQUIRES = equeue-gtk1

all: $(OBJECTS)
	$(OCAMLC) -o filecopy filecopy.cmo  -linkpkg  -g


#----------------------------------------------------------------------
# general rules:

DEBUG     =
OPTIONS   =
OCAMLC    = ocamlfind ocamlc $(DEBUG) $(OPTIONS) -package "$(REQUIRES)"
OCAMLOPT  = ocamlfind ocamlopt $(OPTIONS) $(ROPTIONS) -package "$(REQUIRES)"
OCAMLDEP  = ocamldep $(OPTIONS)
OCAMLFIND = ocamlfind


.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa filecopy

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~ a.old a.new

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c -g $<

.mli.cmi:
	$(OCAMLC) -c $<

