export PATH := .:$(PATH)
$(shell mkdir -p config)

.PHONY: all config recompile install uninstall clean tests

PREFIX ?= /usr
FAILED := $(shell mktemp -u)
CFLAGS += -std=c99 -O2 -DNDEBUG -s
boot.c := bootstrap~
repl.o := src/repl.o

#http://www.gnu.org/prep/standards/html_node/DESTDIR.html
# http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_266
# "Multiple successive slashes are considered to be the same as one slash."
DESTDIR?=


#temp:
#CC := colorgcc

#main
all: ol config


#debug: src/olvm.c src/boot.c
#	$(CC) -std=c99 -O0 -g  src/olvm.c src/boot.c -o ol \
#	   -Xlinker --export-dynamic -ldl
#	@echo Ok.

clean:
	rm -f boot.fasl
	rm -f $(repl.o)
	rm -f ./vm ./ol
	rm -rf ./config

install: ol repl
	# install executables:
	@echo Installing main binary...
	install -d $(DESTDIR)/$(PREFIX)/bin
	install -c -m 755 ol $(DESTDIR)/$(PREFIX)/bin/ol
	# install binary REPL:
	@echo Installing REPL...
	install -d $(DESTDIR)/$(PREFIX)/lib/ol
	install -c -m 644 repl $(DESTDIR)/$(PREFIX)/lib/ol/repl
	# basic libraries:
	@echo Installing libraries...
	@for F in r5rs owl lib etc scheme OpenGL ;do \
	   echo installing $$F libraries... ;\
	   install -d $(DESTDIR)/$(PREFIX)/lib/ol/$$F ;\
	   install -D -m 644 $$F/* $(DESTDIR)/$(PREFIX)/lib/ol/$$F ;\
	done
uninstall:
	-rm -f $(DESTDIR)/$(PREFIX)/bin/ol
	-rm -rf $(DESTDIR)/$(PREFIX)/lib/ol

packages: debian-amd64-package
	@echo "done."

# echo '(display (cdr *version*))' | ./ol
# example: http://lxr.free-electrons.com/source/scripts/package/Makefile
create-debian-package = \
	@printf "Creating $1 DEBIAN package... ";\
	make install DESTDIR=$1/$2;\
	cd $1/$2 ;\
	mkdir DEBIAN;\
	find .$(PREFIX) -type f       > DEBIAN/conffiles;\
	\
	echo Package: ol              > DEBIAN/control;\
	echo Version: 1.0.0           >>DEBIAN/control;\
	echo Architecture: $2         >>DEBIAN/control;\
	echo Maintainer: Yuriy Chumak >>DEBIAN/control;\
	echo Priority: optional       >>DEBIAN/control;\
	echo Description: Otus Lisp - a purely \(mostly\) functional dialect of Lisp \
	                              >>DEBIAN/control;\
	\
	fakeroot dpkg -b . ../ol_1.0_$2.deb

debian-amd64-package:
	$(call create-debian-package,Build,amd64)


# http://mackyle.github.io/blocksruntime/
#clang: src/olvm.c src/boot.c
#	clang-3.5 -fblocks src/olvm.c src/boot.c -ldl -lBlocksRuntime -o ol-c


# this is only container for config targets
config: config/HAS_DLOPEN\
        config/HAS_SOCKETS\
        config/XVisualInfo config/XEvent

exists = \
	@printf "Checking for $2 support... ";\
	if echo "\
	   \#include $1\n\
	   char $2();\
	   \
	   int main() {\
	      return $2();\
	      return 0;\
	   }" | gcc -xc - -ldl -o /dev/null 2>/dev/null; then\
		echo "Ok.";\
		printf 1 > $@;\
	else\
		echo "\033[0;31mNot found.\033[0m";\
		printf 0 > $@;\
	fi

config/HAS_DLOPEN:
	$(call exists, <stdlib.h>,dlopen)
config/HAS_SOCKETS:
	$(call exists, <stdlib.h>,socket)


sizeof = \
	@printf "Determining size of $2 structure... ";\
	if echo "\
	   \#include $1\n\
	   int main() {\
	      return (int)sizeof($2);\
	   }" | gcc -xc - -ldl -o /tmp/$2.$$$$; then\
		echo "Ok."; \
		chmod u+x /tmp/$2.$$$$;\
		/tmp/$2.$$$$; printf "(define sizeof:$2 %d)" $$? >$@;\
		rm -f /tmp/$2.$$$$;\
	else\
		echo "\033[0;31mError.\033[0m";\
		printf 0 > $@;\
	fi

config/XEvent:
	$(call sizeof, <X11/Xutil.h>,XEvent)
config/XVisualInfo:
	$(call sizeof, <X11/Xutil.h>,XVisualInfo)


# ol
ol: src/olvm.c src/olvm.h src/boot.c
	$(CC) $(CFLAGS) src/olvm.c src/boot.c -o $@ \
	   -Xlinker --export-dynamic -ldl
	@echo Ok.


vm: src/olvm.c src/olvm.h
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic -ldl
	@echo Ok.


src/repl.o: repl
	objcopy -B i386 -I binary -O default repl src/repl.o
src/boot.c: repl vm
	vm repl <src/to-c.scm >src/boot.c


recompile: boot.fasl
boot.fasl: vm repl src/ol.scm r5rs/*.scm lang/*.scm owl/*.scm
	@vm repl < src/ol.scm
	@if diff boot.fasl repl>/dev/null ;then\
	   echo '\033[1;32m  `___`  \033[0m' ;\
	   echo '\033[1;32m  (o,o)  \033[0m' ;\
	   echo '\033[1;32m  \)  )  \033[0m' ;\
	   echo '\033[1;32m___"_"___\033[0m' ;\
	   echo '\033[1;32mBuild Ok.\033[0m' ;\
	else \
	   echo `stat -c%s repl` -\> `stat -c%s $@` ;\
	   cp -b $@ repl ;make $@ ;\
	fi


tests: \
   tests/apply.scm\
   tests/banana.scm\
   tests/callcc.scm\
   tests/case-lambda.scm\
   tests/echo.scm\
   tests/ellipsis.scm\
   tests/eval.scm\
   tests/factor-rand.scm\
   tests/factorial.scm\
   tests/fasl.scm\
   tests/ff-call.scm\
   tests/ff-del-rand.scm\
   tests/ff-rand.scm\
   tests/fib-rand.scm\
   tests/hashbang.scm\
   tests/iff-rand.scm\
   tests/library.scm\
   tests/macro-capture.scm\
   tests/macro-lambda.scm\
   tests/mail-order.scm\
   tests/math-rand.scm\
   tests/par-nested.scm\
   tests/par-nested-rand.scm\
   tests/par-rand.scm\
   tests/perm-rand.scm\
   tests/por-prime-rand.scm\
   tests/por-terminate.scm\
   tests/queue-rand.scm\
   tests/record.scm\
   tests/rlist-rand.scm\
   tests/seven.scm\
   tests/share.scm\
   tests/stable-rand.scm\
   tests/str-quote.scm\
   tests/string.scm\
   tests/suffix-rand.scm\
   tests/theorem-rand.scm\
   tests/toplevel-persist.scm\
   tests/utf-8-rand.scm\
   tests/vararg.scm\
   tests/vector-rand.scm\
   tests/numbers.scm
	@rm -f $(FAILED)
	@for F in $^ ;do \
	   echo -n "Testing $$F ... " ;\
	   if ./vm repl <$$F | diff - $$F.ok >/dev/null ;then\
	      echo "Ok." ;\
	   else \
	      echo "\033[0;31mFailed!\033[0m" ;\
	      touch $(FAILED) ;\
	   fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"

sample-embed:
	gcc src/sample-embed.c src/olvm.c src/boot.c -std=c99 -ldl -DEMBEDDED_VM -DHAS_DLOPEN=1 -DHAS_PINVOKE=1 -o sample-embed \
	-Xlinker --export-dynamic
