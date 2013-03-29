.PHONY: default all opt doc install uninstall clean
FLAGS = -g -annot
default: all opt
nldate.ml: nldate.mlp
	camlp4o -impl $< > $@
all: nldate.ml
	ocamlc -c $(FLAGS) nldate.mli
	ocamlc -c $(FLAGS) nldate.ml
	ocamlc -c $(FLAGS) nlencoding.mli
	ocamlc -c $(FLAGS) nlencoding.ml
	ocamlc -a $(FLAGS) -o netstring-light.cma nldate.cmo nlencoding.cmo
opt:
	ocamlc -c $(FLAGS) nldate.mli
	ocamlopt -c $(FLAGS) nldate.ml
	ocamlc -c $(FLAGS) nlencoding.mli
	ocamlopt -c $(FLAGS) nlencoding.ml
	ocamlopt -a $(FLAGS) -o netstring-light.cmxa nldate.cmx nlencoding.cmx
doc:
	mkdir -p ../html
	ocamldoc -html -d ../html nldate.mli nlencoding.mli

netstring-light.cmxa: opt

install:
	ocamlfind install netstring-light META \
          $$(ls *.mli *.ml *.cm[ioxa] *.cmxa *.[oa] 2>/dev/null)
uninstall:
	ocamlfind remove netstring-light
clean:
	rm -f *.cm[ioxa] *.o *.cmxa *.a *~ *.annot
	rm -rf ../html
	rm -f nldate.ml
