.PHONY: default all opt doc install uninstall reinstall clean

PACKAGES =
OCAMLC = ocamlfind ocamlc -package "$(PACKAGES)"
OCAMLOPT = ocamlfind opt -package "$(PACKAGES)"
FLAGS = -g -annot

default: all opt
nldate.ml: nldate.mlp
	camlp4o -impl $< > $@
nlstring_str.ml: nlstring_str.mlp
	camlp4o -impl $< > $@
all: nldate.ml nlstring_str.ml
	$(OCAMLC) -c $(FLAGS) nldate.mli
	$(OCAMLC) -c $(FLAGS) nldate.ml
	$(OCAMLC) -c $(FLAGS) nlencoding.mli
	$(OCAMLC) -c $(FLAGS) nlencoding.ml
	$(OCAMLC) -c $(FLAGS) nlbuffer.mli
	$(OCAMLC) -c $(FLAGS) nlbuffer.ml
	$(OCAMLC) -c $(FLAGS) nlchannels.mli
	$(OCAMLC) -c $(FLAGS) nlchannels.ml
	$(OCAMLC) -c $(FLAGS) nlurl.mli
	$(OCAMLC) -c $(FLAGS) nlurl.ml
	$(OCAMLC) -a $(FLAGS) -o netstring-light.cma \
		nldate.cmo nlencoding.cmo nlbuffer.cmo nlchannels.cmo nlurl.cmo
opt: nldate.ml nlstring_str.ml
	$(OCAMLC) -c $(FLAGS) nldate.mli
	$(OCAMLOPT) -c $(FLAGS) nldate.ml
	$(OCAMLC) -c $(FLAGS) nlencoding.mli
	$(OCAMLOPT) -c $(FLAGS) nlencoding.ml
	$(OCAMLC) -c $(FLAGS) nlbuffer.mli
	$(OCAMLOPT) -c $(FLAGS) nlbuffer.ml
	$(OCAMLC) -c $(FLAGS) nlchannels.mli
	$(OCAMLOPT) -c $(FLAGS) nlchannels.ml
	$(OCAMLC) -c $(FLAGS) nlurl.mli
	$(OCAMLOPT) -c $(FLAGS) nlurl.ml
	$(OCAMLOPT) -a $(FLAGS) -o netstring-light.cmxa \
		nldate.cmx nlencoding.cmx nlbuffer.cmx nlchannels.cmx nlurl.cmx
doc:
	mkdir -p ../html
	ocamldoc -html -d ../html \
		nldate.mli nlencoding.mli mlbuffer.mli nlchannels.mli nlurl.mli

netstring-light.cmxa: opt

install:
	ocamlfind install netstring-light META \
          $$(ls *.mli *.ml *.cm[ioxa] *.cmxa *.[oa] 2>/dev/null)
uninstall:
	-ocamlfind remove netstring-light
reinstall:
	$(MAKE) uninstall
	$(MAKE) install
clean:
	rm -f *.cm[ioxa] *.o *.cmxa *.a *~ *.annot
	rm -rf ../html
	rm -f nldate.ml nlstring_str.ml
