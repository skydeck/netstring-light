netplex: add.ml netplex.ml
	ocamlfind ocamlc -g -o netplex -package "netcgi2-plex" -linkpkg \
		add.ml netplex.ml -thread

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cmx *.o
	rm -f netplex
