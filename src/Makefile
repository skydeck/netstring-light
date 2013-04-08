.PHONY: default all opt doc install uninstall reinstall clean
FLAGS = -g -annot
default: all opt
nldate.ml: nldate.mlp
	camlp4o -impl $< > $@
nlstring_str.ml: nlstring_str.mlp
	camlp4o -impl $< > $@
all: nldate.ml nlstring_str.ml
	ocamlc -c $(FLAGS) nldate.mli
	ocamlc -c $(FLAGS) nldate.ml
	ocamlc -c $(FLAGS) nlstring_str.mli
	ocamlc -c $(FLAGS) nlstring_str.ml
	ocamlc -c $(FLAGS) nlencoding.mli
	ocamlc -c $(FLAGS) nlencoding.ml
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlbuffer.mli
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlbuffer.ml
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlchannels.mli
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlchannels.ml
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlsockaddr.mli
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlsockaddr.ml
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlurl.mli
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlurl.ml
	ocamlc -a $(FLAGS) -o netstring-light.cma nldate.cmo nlstring_str.cmo nlencoding.cmo nlbuffer.cmo nlchannels.cmo nlsockaddr.cmo nlurl.cmo
opt:
	ocamlc -c $(FLAGS) nldate.mli
	ocamlopt -c $(FLAGS) nldate.ml
	ocamlc -c $(FLAGS) nlstring_str.mli
	ocamlopt -c $(FLAGS) nlstring_str.ml
	ocamlc -c $(FLAGS) nlencoding.mli
	ocamlopt -c $(FLAGS) nlencoding.ml
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlbuffer.mli
	ocamlfind ocamlopt -package netsys -c $(FLAGS) nlbuffer.ml
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlchannels.mli
	ocamlfind ocamlopt -package netsys -c $(FLAGS) nlchannels.ml
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlsockaddr.mli
	ocamlfind ocamlopt -package netsys -c $(FLAGS) nlsockaddr.ml
	ocamlfind ocamlc -package netsys -c $(FLAGS) nlurl.mli
	ocamlfind ocamlopt -package netsys -c $(FLAGS) nlurl.ml
	ocamlopt -a $(FLAGS) -o netstring-light.cmxa nldate.cmx nlstring_str.cmx nlencoding.cmx nlbuffer.cmx nlchannels.cmx nlsockaddr.cmx nlurl.cmx
doc:
	mkdir -p ../html
	ocamldoc -html -d ../html nldate.mli nlstring_str.mli nlencoding.mli nlbuffer.mli nlchannels.mli nlsockaddr.mli nlurl.mli

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
