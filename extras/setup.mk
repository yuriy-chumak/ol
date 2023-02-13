# http://www.gnu.org/prep/standards/html_node/DESTDIR.html
# http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_266
# "Multiple successive slashes are considered to be the same as one slash."
DESTDIR?=
PREFIX ?= /usr

install: ol includes/ol/vm.h
	# install Ol executable(s) to $(DESTDIR)$(PREFIX)/bin:
	@echo Installing main binary...
	install -d $(DESTDIR)$(PREFIX)/bin
	install ol $(DESTDIR)$(PREFIX)/bin/ol
	@echo Installing ol virtual machine binary...
	install -d $(DESTDIR)$(PREFIX)/bin
	install vm $(DESTDIR)$(PREFIX)/bin/olvm
	@echo Installing libol.so...
	install -d $(DESTDIR)$(PREFIX)/lib
	install -m 644 libol.so $(DESTDIR)$(PREFIX)/lib/libol.so
	@echo Installing headers...
	install -d $(DESTDIR)$(PREFIX)/include/ol
	install -m 644 includes/ol/vm.h $(DESTDIR)$(PREFIX)/include/ol/vm.h
	install -m 644 includes/ol/ol.h $(DESTDIR)$(PREFIX)/include/ol/ol.h
	# and libraries to $(DESTDIR)$(PREFIX)/lib/ol:
	@echo Installing basic libraries...
	cd libraries && find * -type d -exec install -d "{}" "$(DESTDIR)$(PREFIX)/lib/ol/{}" \;
	cd libraries && find * -type f -exec install -m 644 "{}" "$(DESTDIR)$(PREFIX)/lib/ol/{}" \;
	# install Ol binary REPL to $(DESTDIR)$(PREFIX)/lib/ol:
	@echo Installing REPL...
	install -d $(DESTDIR)$(PREFIX)/lib/ol
	install -m 644 repl $(DESTDIR)$(PREFIX)/lib/ol/repl
	@echo Installing man page...
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	gzip <ol.1 >$(DESTDIR)$(PREFIX)/share/man/man1/ol.1.gz
	@echo Ok.

install-dev:
	@echo Linking main binary...
	install -d $(DESTDIR)$(PREFIX)/bin
	ln -s `pwd`/ol $(DESTDIR)$(PREFIX)/bin/ol
	@echo Installing ol virtual machine binary...
	install -d $(DESTDIR)$(PREFIX)/bin
	ln -s `pwd`/olvm $(DESTDIR)$(PREFIX)/bin/olvm
	@echo Installing libol.so...
	ln -s `pwd`/libol.so $(DESTDIR)$(PREFIX)/lib/libol.so
	@echo Installing headers...
	ln -s `pwd`/includes/ol $(DESTDIR)$(PREFIX)/include/ol
	# and libraries to $(DESTDIR)$(PREFIX)/lib/ol:
	@echo Installing basic libraries...
	ln -s `pwd`/libraries $(DESTDIR)$(PREFIX)/lib/ol
	@echo Installing REPL...
	install -d $(DESTDIR)$(PREFIX)/lib/ol
	ln -s `pwd`/repl $(DESTDIR)$(PREFIX)/lib/ol/repl

uninstall:
	-rm -rf $(DESTDIR)$(PREFIX)/bin/ol
	-rm -rf $(DESTDIR)$(PREFIX)/bin/olvm
	-rm -rf $(DESTDIR)$(PREFIX)/lib/libol.so
	-rm -rf $(DESTDIR)$(PREFIX)/lib/ol/repl
	-rm -rf $(DESTDIR)$(PREFIX)/lib/ol
	-rm -rf $(DESTDIR)$(PREFIX)/include/ol
	-rm -rf $(DESTDIR)$(PREFIX)/share/man/man1/ol.1.gz
