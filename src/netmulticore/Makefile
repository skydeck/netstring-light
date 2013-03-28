TOP_DIR=../..

PKGNAME  = netmulticore
OBJECTS  = netmcore.cmo netmcore_camlbox.cmo netmcore_util.cmo \
	   netmcore_mempool.cmo netmcore_heap.cmo netmcore_ref.cmo \
	   netmcore_array.cmo netmcore_sem.cmo netmcore_mutex.cmo \
	   netmcore_condition.cmo netmcore_queue.cmo netmcore_buffer.cmo \
	   netmcore_matrix.cmo netmcore_hashtbl.cmo netmcore_process.cmo
DOBJECTS = netmcore.mli netmcore_camlbox.mli netmcore_mempool.mli \
	   netmcore_heap.mli netmcore_ref.mli netmcore_array.mli \
	   netmcore_sem.mli netmcore_mutex.mli netmcore_condition.mli \
	   netmcore_queue.mli netmcore_buffer.mli netmcore_matrix.mli \
	   netmcore_hashtbl.mli netmcore_process.mli \
	   netmcore_tut.txt

INCLUDES += $(INC_EQUEUE) $(INC_RPC) $(INC_NETSTRING) $(INC_NETSYS) \
            $(INC_NETPLEX) $(INC_NETCAMLBOX)


include $(TOP_DIR)/Makefile.rules

t_toploop: t_toploop.ml $(PKGNAME).cma
	OCAMLPATH=.. \
	ocamlfind ocamlc -g -o t_toploop \
	   toplevellib.cma $(PKGNAME).cma t_toploop.ml \
	   -package "equeue,rpc,netstring,netsys,netplex,netcamlbox" \
	   -linkpkg -linkall

t_mempool: t_mempool.ml $(PKGNAME).cma
	OCAMLPATH=.. \
	ocamlfind ocamlc -g -o t_mempool \
	   $(PKGNAME).cma t_mempool.ml \
	   -package "equeue,rpc,netstring,netsys,netplex,netcamlbox" \
	   -linkpkg



include depend
