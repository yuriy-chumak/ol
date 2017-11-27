export PATH := .:$(PATH)
$(shell mkdir -p config)

export PATH := $(PATH):/opt/emsdk_portable:/opt/emsdk_portable/clang/fastcomp/build_master_64/bin:/opt/emsdk_portable/node/4.1.1_64bit/bin:/opt/emsdk_portable/emscripten/master

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
.PHONY: all config recompile install uninstall clean tests check

CC ?= gcc

# http://ptspts.blogspot.com/2013/12/how-to-make-smaller-c-and-c-binaries.html

PREFIX ?= /usr
FAILED := $(shell mktemp -u)
CFLAGS += -std=c99 $(if $(RPM_OPT_FLAGS), $(RPM_OPT_FLAGS), -O2 -DNDEBUG -s -fno-exceptions)
boot.c := bootstrap~
repl.o := src/repl.o

CFLAGS += $(if $(HAS_DLOPEN), -DHAS_DLOPEN=1, -DHAS_DLOPEN=0)\
          $(if $(HAS_SOCKETS), -DHAS_SOCKETS=1, -DHAS_SOCKETS=0)\
          $(if $(HAS_SECCOMP),, -DNO_SECCOMP)


ifeq ($(UNAME),Linux)
L := $(if HAS_DLOPEN, -ldl -lm)
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
all: vm ol repl tests

#debug: src/olvm.c src/repl.o
#	$(CC) -std=c99 -O0 -g  src/olvm.c src/repl.o -o ol \
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
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 ol $(DESTDIR)$(PREFIX)/bin/ol
	# install binary REPL:
	@echo Installing REPL...
	install -d $(DESTDIR)$(PREFIX)/lib/ol
	install -m 644 repl $(DESTDIR)$(PREFIX)/lib/ol/repl
	# basic libraries:
	@echo Installing common libraries...
	@for F in r5rs lang otus owl lib etc scheme ;do \
	   echo installing $$F libraries... ;\
	   install -d $(DESTDIR)$(PREFIX)/lib/ol/$$F ;\
	   install -D -m 644 $$F/* $(DESTDIR)$(PREFIX)/lib/ol/$$F ;\
	done
	@echo Installing OpenGL libraries...
	install -d $(DESTDIR)$(PREFIX)/lib/ol/OpenGL
	install -D -m 644 OpenGL/*.scm $(DESTDIR)$(PREFIX)/lib/ol/OpenGL
	@for F in OpenGL/ARB OpenGL/EXT ;do \
	   echo installing $$F libraries... ;\
	   install -d $(DESTDIR)$(PREFIX)/lib/ol/$$F ;\
	   install -D -m 644 $$F/* $(DESTDIR)$(PREFIX)/lib/ol/$$F ;\
	done
	@echo Installing OpenCL libraries...
	install -d $(DESTDIR)$(PREFIX)/lib/ol/OpenCL
	install -D -m 644 OpenCL/*.scm $(DESTDIR)$(PREFIX)/lib/ol/OpenCL
	@echo Installing OpenAL libraries...
	install -d $(DESTDIR)$(PREFIX)/lib/ol/OpenAL
	install -D -m 644 OpenAL/*.scm $(DESTDIR)$(PREFIX)/lib/ol/OpenAL

uninstall:
	-rm -f $(DESTDIR)$(PREFIX)/bin/ol
	-rm -rf $(DESTDIR)$(PREFIX)/lib/ol

packages: debian-amd64-package
	@echo "done."

# echo '(display (cdr *version*))' | ./ol
# example: http://lxr.free-electrons.com/source/scripts/package/Makefile
# howto: https://www.debian.org/doc/manuals/distribute-deb/distribute-deb.html
create-debian-package = \
	@printf "Creating $1 DEBIAN package... ";\
	make install DESTDIR=$1/$2;\
	cd $1/$2 ;\
	mkdir DEBIAN;\
	find .$(PREFIX) -type f       > DEBIAN/conffiles;\
	\
	echo Package: ol              > DEBIAN/control;\
	echo Version: 1.1             >>DEBIAN/control;\
	echo Architecture: $2         >>DEBIAN/control;\
	echo Maintainer: Yuriy Chumak >>DEBIAN/control;\
	echo Priority: optional       >>DEBIAN/control;\
	echo Description: Otus Lisp - a purely* functional dialect of Lisp \
	                              >>DEBIAN/control;\
	\
	fakeroot dpkg -b . ../ol_1.1_$2.deb

debian-amd64-package:
	$(call create-debian-package,Build,amd64)


# http://mackyle.github.io/blocksruntime/
#clang: src/olvm.c src/repl.o
#	clang-3.5 -fblocks src/olvm.c src/repl.o -ldl -lBlocksRuntime -o ol-c


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
ol: src/olvm.c src/olvm.h src/repl.o
	$(CC) $(CFLAGS) src/olvm.c src/repl.o -o $@ \
	   -Xlinker --export-dynamic $(L)
	@echo Ok.

src/olvm.c: src/ffi.c
	touch src/olvm.c

vm: src/olvm.c src/olvm.h
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L)
	@echo Ok.
vm32: src/olvm.c src/olvm.h
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L) -m32
	@echo Ok.
vm64: src/olvm.c src/olvm.h
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L) -m64
	@echo Ok.


olvm.js: src/olvm.c src/olvm.h src/slim.c
	emcc src/olvm.c src/slim.c -o olvm.js -s ASYNCIFY=1 -Oz \
	   -s NO_EXIT_RUNTIME=1 \
	   -fno-exceptions -fno-rtti \
	   --memory-init-file 0 --llvm-opts "['-O3']" -v


talkback: src/olvm.c src/repl.o extensions/talkback/talkback.c extensions/talkback/sample.c
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -DEMBEDDED_VM -DHAS_PINVOKE=1 -o $@ -I src \
	   src/repl.o extensions/talkback/talkback.c extensions/talkback/sample.c -pthread \
	   -Xlinker --export-dynamic $(L)


src/repl.o: repl
	ld -r -b binary -o src/repl.o repl
src/slim.c: repl src/slim.lisp
	vm repl <src/slim.lisp >src/slim.c


recompile: boot.fasl
boot.fasl: vm repl src/ol.scm otus/lisp.scm r5rs/*.scm lang/*.scm owl/*.scm
	@vm repl src/ol.scm --version "`git describe --always`"
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
	@echo "Internal VM testing:"
	   @$(CC) $(CFLAGS) src/olvm.c tests/vm.c -I src -DNAKED_VM -DEMBEDDED_VM -o tests-vm64 $(L) -m64
	   @$(CC) $(CFLAGS) src/olvm.c tests/vm.c -I src -DNAKED_VM -DEMBEDDED_VM -o tests-vm32 $(L) -m32
	@echo ""
	@echo "32-bit:"
	@echo "-------"
	   @./tests-vm32
	@echo ""
	@echo "64-bit:"
	@echo "-------"
	   @./tests-vm64
	@echo ""
	@echo "ffi tests (32- and 64-bit):"
	@echo "---------------------------------------"
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -I src -DNAKED_VM -o ffi32 $(L) -m32 -Xlinker --export-dynamic
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -I src -DNAKED_VM -o ffi64 $(L) -m64 -Xlinker --export-dynamic
	   @echo -n "Testing ffi ... "
	   @if ./ffi32 repl <tests/ffi.scm | diff - tests/ffi.scm.ok >/dev/null && ./ffi64 repl <tests/ffi.scm | diff - tests/ffi.scm.ok >/dev/null; then\
	      echo "Ok.";\
	   else \
	      echo "failed." ;\
	      touch $(FAILED);\
	   fi
	@echo ""
	@echo "common (32- and 64-bit simulatenously):"
	@echo "---------------------------------------"
	@make vm32 vm64
	@for F in $^ ;do \
	   echo -n "Testing $$F ... " ;\
	   if ./vm32 repl <$$F | diff - $$F.ok >/dev/null && ./vm64 repl <$$F | diff - $$F.ok >/dev/null; then\
	      echo "Ok." ;\
	   else \
	      echo "\033[0;31mFailed!\033[0m" ;\
	      touch $(FAILED) ;\
	   fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"

sample-embed:
	gcc src/sample-embed.c src/olvm.c src/repl.o -std=c99 -ldl -DEMBEDDED_VM -DHAS_DLOPEN=1 -DHAS_PINVOKE=1 -o sample-embed \
	-Xlinker --export-dynamic

# simple only target platform size tests
check: $(filter-out tests/ffi.scm,$(wildcard tests/*.scm))
	@rm -f $(FAILED)
	@echo "Internal VM testing:"
	   @$(CC) $(CFLAGS) src/olvm.c tests/vm.c -I src -DNAKED_VM -DEMBEDDED_VM -o vm-check $(L)
	@echo ""
	   @./vm-check
	@echo ""
	@echo "ffi test:"
	@echo "---------------------------------------"
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -I src -DNAKED_VM -o ffi $(L) -Xlinker --export-dynamic
	   @echo -n "Testing ffi ... "
	   @if ./ffi repl <tests/ffi.scm | diff - tests/ffi.scm.ok >/dev/null; then\
	      echo "Ok.";\
	   else \
	      echo "failed." ;\
	      touch $(FAILED);\
	   fi
	@echo ""
	@echo "common:"
	@echo "---------------------------------------"
	@make vm
	@for F in $^ ;do \
	   echo -n "Testing $$F ... " ;\
	   if ./vm repl <$$F | diff - $$F.ok >/dev/null; then\
	      echo "Ok." ;\
	   else \
	      echo "\033[0;31mFailed!\033[0m" ;\
	      touch $(FAILED) ;\
	   fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"