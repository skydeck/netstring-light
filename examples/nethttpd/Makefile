all: easy_daemon easy_reactor easy_engine proxy_engine file_reactor \
     file_mt_reactor file_engine netplex netplex_auth

easy_daemon: easy_daemon.ml
	ocamlfind ocamlc -o easy_daemon -package "netstring,unix,nethttpd,netcgi2" \
		easy_daemon.ml -linkpkg -g

easy_reactor: easy_reactor.ml
	ocamlfind ocamlc -o easy_reactor -package "netstring,unix,nethttpd,netcgi2" \
		easy_reactor.ml -linkpkg -g

easy_engine: easy_engine.ml
	ocamlfind ocamlc -o easy_engine -package "netstring,unix,equeue,nethttpd,netcgi2" \
		easy_engine.ml -linkpkg -g

easy_engine.opt: easy_engine.ml
	ocamlfind ocamlopt -o easy_engine.opt -package "netstring,unix,equeue,nethttpd,netcgi2" \
		easy_engine.ml -linkpkg

proxy_engine: proxy_engine.ml
	ocamlfind ocamlc -o proxy_engine -package "netstring,unix,equeue,nethttpd,netcgi2" \
		proxy_engine.ml -linkpkg -g

proxy_engine.opt: proxy_engine.ml
	ocamlfind ocamlopt -o proxy_engine.opt -package "netstring,unix,equeue,nethttpd,netcgi2" \
		proxy_engine.ml -linkpkg 

file_reactor: file_reactor.ml
	ocamlfind ocamlc -o file_reactor -package "netstring,unix,nethttpd,netcgi2" \
		file_reactor.ml -linkpkg -g

file_mt_reactor: file_mt_reactor.ml
	ocamlfind ocamlc -o file_mt_reactor -package "netstring,unix,threads,nethttpd,netcgi2" \
		file_mt_reactor.ml -linkpkg -g -thread

file_mt_reactor.opt: file_mt_reactor.ml
	ocamlfind ocamlopt -o file_mt_reactor.opt -package "netstring,unix,threads,nethttpd,netcgi2" \
		file_mt_reactor.ml -linkpkg -thread

file_engine: file_engine.ml
	ocamlfind ocamlc -o file_engine -package "netstring,unix,equeue,nethttpd,netcgi2" \
		file_engine.ml -linkpkg -g

netplex: netplex.ml
	ocamlfind ocamlc -o netplex -package "netstring,netcgi2,unix,nethttpd-for-netcgi2,netplex" \
		-thread netplex.ml -linkpkg -g
netplex_auth: netplex_auth.ml
	ocamlfind ocamlc -o netplex_auth -package "netstring,netcgi2,unix,nethttpd-for-netcgi2,netplex" \
		-thread netplex_auth.ml -linkpkg -g

clean:
	rm -f *.cmi *.cmo *.cmx *.o
	rm -f test_acceptor easy_daemon easy_reactor easy_engine easy_engine.opt
	rm -f proxy_engine proxy_engine.opt file_reactor file_mt_reactor 
	rm -f file_mt_reactor.opt file_engine netplex netplex_auth
