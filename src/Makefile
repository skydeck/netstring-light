.PHONY: default all opt doc install uninstall clean
default: all opt
nldate.ml: nldate.mlp
	camlp4o -impl $< > $@
all: nldate.ml
	ocamlc -c -g nldate.mli
	ocamlc -c -g nldate.ml
	ocamlc -c -g nlencoding.mli
	ocamlc -c -g nlencoding.ml
	ocamlc -a -g -o libnlencoding.cma nldate.cmo nlencoding.cmo
opt:
	ocamlc -c -g nldate.mli
	ocamlopt -c -g nldate.ml
	ocamlc -c -g nlencoding.mli
	ocamlopt -c -g nlencoding.ml
	ocamlopt -a -g -o libnlencoding.cmxa nldate.cmx nlencoding.cmx
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
	rm -f *.cm[ioxa] *.o *.cmxa *.a *~
	rm -rf ../html
	rm -f nldate.ml
