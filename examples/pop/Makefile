.PHONY: all

APPS=mbox_list

all: $(APPS)

mbox_list: mbox_list.ml
	ocamlfind ocamlc -g -o $@ -package unix -package netstring -package pop -linkpkg $<

.PHONY: clean
clean:
	rm -f $(APPS) *.cmi *.cmo
