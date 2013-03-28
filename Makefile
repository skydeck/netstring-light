.PHONY: default all opt doc install uninstall clean

default: all opt
all:
	$(MAKE) -C src all
opt:
	$(MAKE) -C src opt
doc:
	$(MAKE) -C src doc
install:
	$(MAKE) -C src install
uninstall:
	$(MAKE) -C src uninstall
clean:
	$(MAKE) -C src clean
