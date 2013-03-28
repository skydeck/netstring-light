TOP_DIR=../..

PKGNAME = netclient
OBJECTS =  http_client_conncache.cmo http_client.cmo telnet_client.cmo \
	   ftp_data_endpoint.cmo ftp_client.cmo \
	   http_fs.cmo ftp_fs.cmo
DOBJECTS = http_client_conncache.mli http_client.mli telnet_client.mli \
	   ftp_data_endpoint.mli ftp_client.mli \
	   http_fs.mli ftp_fs.mli \
           netclient_tut.txt

INCLUDES = $(INC_EQUEUE) $(INC_NETSTRING) $(INC_NETSYS)

include $(TOP_DIR)/Makefile.rules

include depend
