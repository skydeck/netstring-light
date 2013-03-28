#! /bin/sh
export LD_LIBRARY_PATH=../../../src/netsys:../../../src/netstring
export CAML_LD_LIBRARY_PATH=../../../src/netsys:../../../src/netstring
exec ./netplex -conf netplex.cfg "$@"
