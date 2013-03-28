all: speed manymult unimult

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa *.o *.a speed manymult unimult

speed: speed.ml
	ocamlfind ocamlopt -o speed -package netcamlbox -linkpkg -g speed.ml

manymult: manymult.ml
	ocamlfind ocamlopt -o manymult -package netcamlbox -linkpkg -g manymult.ml

unimult: unimult.ml
	ocamlfind ocamlopt -o unimult -package netcamlbox -linkpkg -g unimult.ml
