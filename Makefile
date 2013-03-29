.PHONY: default all opt doc install uninstall reinstall clean

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
reinstall:
	$(MAKE) -C src reinstall
clean:
	$(MAKE) -C src clean
	rm -f *~
