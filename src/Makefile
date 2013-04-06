.PHONY: default all opt doc install uninstall reinstall clean
FLAGS = -g -annot
default: all opt
nldate.ml: nldate.mlp
	camlp4o -impl $< > $@
all: nldate.ml
	ocamlc -c $(FLAGS) nldate.mli
	ocamlc -c $(FLAGS) nldate.ml
	ocamlc -c $(FLAGS) nlencoding.mli
	ocamlc -c $(FLAGS) nlencoding.ml
	ocamlc -c $(FLAGS) nlbuffer.mli
	ocamlc -c $(FLAGS) nlbuffer.ml
	ocamlc -c $(FLAGS) nlchannels.mli
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlchannels.ml
	ocamlc -a $(FLAGS) -o netstring-light.cma nldate.cmo nlencoding.cmo nlbuffer.cmo nlchannels.cmo
opt:
	ocamlc -c $(FLAGS) nldate.mli
	ocamlopt -c $(FLAGS) nldate.ml
	ocamlc -c $(FLAGS) nlencoding.mli
	ocamlopt -c $(FLAGS) nlencoding.ml
	ocamlc -c $(FLAGS) nlbuffer.mli
	ocamlopt -c $(FLAGS) nlbuffer.ml
	ocamlc -c $(FLAGS) nlchannels.mli
	ocamlfind ocamlopt -package netsys -c $(FLAGS) nlchannels.ml
	ocamlopt -a $(FLAGS) -o netstring-light.cmxa nldate.cmx nlencoding.cmx nlbuffer.cmx nlchannels.cmx
doc:
	mkdir -p ../html
	ocamldoc -html -d ../html nldate.mli nlencoding.mli mlbuffer.mli nlchannels.mli

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
	rm -f nldate.ml
