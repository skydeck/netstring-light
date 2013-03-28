TOP_DIR=../..

PKGNAME  = shell
OBJECTS  = shell_sys.cmo shell.cmo shell_uq.cmo shell_fs.cmo
DOBJECTS = shell_sys.mli shell.mli shell_uq.mli shell_fs.mli \
	   shell_intro.txt

INCLUDES += $(INC_EQUEUE) $(INC_NETSYS) $(INC_NETSTRING)

include $(TOP_DIR)/Makefile.rules

include depend
