export PATH := .:$(PATH)
$(shell mkdir -p config)

#do some configure staff
exists = $(shell echo "\
	   \#include $1\n\
	   char $2();\
	   \
	   int main() {\
	      return $2();\
	      return 0;\
	   }" | gcc -xc - $3 -o /dev/null 2>/dev/null && echo 1)
# features
UNAME  := $(shell uname -s)
HAS_SECCOMP := $(call exists, <linux/seccomp.h>, prctl)
HAS_DLOPEN  := $(call exists, <stdlib.h>, dlopen, -ldl)
HAS_SOCKETS := $(call exists, <stdlib.h>, socket)

# body
.PHONY: all config recompile install uninstall clean tests

CC=gcc

# http://ptspts.blogspot.com/2013/12/how-to-make-smaller-c-and-c-binaries.html

PREFIX ?= /usr
FAILED := $(shell mktemp -u)
CFLAGS += -std=c99 $(if $(RPM_OPT_FLAGS), $(RPM_OPT_FLAGS), -O2 -DNDEBUG -s)
boot.c := bootstrap~
repl.o := src/repl.o

CFLAGS += $(if $(HAS_DLOPEN), -DHAS_DLOPEN=1)\
          $(if $(HAS_SOCKETS), -DHAS_SOCKETS=1)\
          $(if $(HAS_SECCOMP),, -DNO_SECCOMP)


ifeq ($(UNAME),Linux)
L := $(if HAS_DLOPEN, -ldl)
endif

ifeq ($(UNAME),FreeBSD)
L := $(if HAS_DLOPEN, -lc)
endif
ifeq ($(UNAME),NetBSD)
L := $(if HAS_DLOPEN, -lc)
endif
ifeq ($(UNAME),OpenBSD)
L := $(if HAS_DLOPEN, -lc -ftrampolines)
endif

# Windows/MinGW
ifeq ($(UNAME),MINGW32_NT-6.1)
L := -lws2_32
endif

# http://www.gnu.org/prep/standards/html_node/DESTDIR.html
# http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_266
# "Multiple successive slashes are considered to be the same as one slash."
DESTDIR?=


#main
all:ol

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
	install -m 755 ol $(DESTDIR)/$(PREFIX)/bin/ol
	# install binary REPL:
	@echo Installing REPL...
	install -d $(DESTDIR)/$(PREFIX)/lib/ol
	install -m 644 repl $(DESTDIR)/$(PREFIX)/lib/ol/repl
	# basic libraries:
	@echo Installing libraries...
	@for F in r5rs otus owl lib etc scheme ;do \
	   echo installing $$F libraries... ;\
	   install -d $(DESTDIR)/$(PREFIX)/lib/ol/$$F ;\
	   install -D -m 644 $$F/* $(DESTDIR)/$(PREFIX)/lib/ol/$$F ;\
	done
	@echo Installing OpenGL libraries...
	install -d $(DESTDIR)/$(PREFIX)/lib/ol/OpenGL
	@for F in OpenGL/ARB OpenGL/EGL OpenGL/ES OpenGL/EXT ;do \
	   echo installing $$F libraries... ;\
	   install -d $(DESTDIR)/$(PREFIX)/lib/ol/$$F ;\
	   install -D -m 644 $$F/* $(DESTDIR)/$(PREFIX)/lib/ol/$$F ;\
	done
	install -D -m 644 OpenGL/*.scm $(DESTDIR)/$(PREFIX)/lib/ol/OpenGL

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

existsW = \
	@printf "Checking for $2 support... ";\
	if echo "\
	   \#include $1\n\
	   char $2();\
	   \
	   int main() {\
	      return $2();\
	      return 0;\
	   }" | gcc -xc - $3 -o /dev/null 2>/dev/null; then\
		echo "Ok.";\
		printf 1 > $@;\
	else\
		echo "\033[0;31mNot found.\033[0m";\
		printf 0 > $@;\
	fi

config/HAS_DLOPEN:
	$(call existsW, <stdlib.h>,dlopen,-ldl)
config/HAS_SOCKETS:
	$(call existsW, <stdlib.h>,socket)


sizeof = \
	@printf "Determining size of $2 structure... ";\
	if echo "\
	   \#include $1\n\
	   int main() {\
	      return (int)sizeof($2);\
	   }" | gcc -xc - $3 -o /tmp/$2.$$$$; then\
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
	   -Xlinker --export-dynamic $(L)
	@echo Ok.


vm: src/olvm.c src/olvm.h
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L)
	@echo Ok.


src/repl.o: repl
	objcopy -B i386 -I binary -O default repl src/repl.o
src/boot.c: repl vm
	vm repl <src/to-c.scm >src/boot.c


recompile: boot.fasl
boot.fasl: vm repl src/ol.scm otus/lisp.scm r5rs/*.scm lang/*.scm owl/*.scm
	@vm repl src/ol.scm --version "`git describe`"
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
