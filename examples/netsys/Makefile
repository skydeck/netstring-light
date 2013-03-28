all: sem_pingpong

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a
	rm -f sem_pingpong

sem_pingpong: sem_pingpong.ml
	ocamlfind ocamlopt -o sem_pingpong -package netsys -linkpkg \
		sem_pingpong.ml 
