export PATH := .:$(PATH)
$(shell mkdir -p config)
all: vm ol repl

export PATH := $(PATH):/opt/emsdk_portable:/opt/emsdk_portable/clang/fastcomp/build_master_64/bin:/opt/emsdk_portable/node/4.1.1_64bit/bin:/opt/emsdk_portable/emscripten/master
export OL_HOME=libraries

CC ?= gcc

#do some configuration staff
exists = $(shell echo "\
	   \#include $2\n\
	   char $3();\
	   \
	   int main() {\
	      return $3();\
	      return 0;\
	   }" | $(CC) $1 -xc - $4 -o /dev/null 2>/dev/null && echo 1)

# features
UNAME  ?= $(shell uname -s)
LBITS  ?= $(shell getconf LONG_BIT)
HAS_SECCOMP ?= $(call exists,,<linux/seccomp.h>, prctl)
HAS_DLOPEN  ?= $(call exists,,<stdlib.h>, dlopen, -ldl)
HAS_SOCKETS ?= $(call exists,,<stdlib.h>, socket)
HAS_CDEFS   ?= $(call exists,,<sys/cdefs.h>,exit)

HAS_32CDEFS   ?= $(call exists,-m32,<sys/cdefs.h>,exit)

ifeq ($(LBITS),64)
vm64 = echo -n "64 " && ./vm64 repl <$$F | diff - $$F.ok
else
vm64 = true
endif

ifeq ($(HAS_CDEFS),1)
vm32 = echo -n "32 " && ./vm32 repl <$$F | diff - $$F.ok
else
vm32 = true
endif

# body
.PHONY: all config recompile install uninstall clean tests check

# http://ptspts.blogspot.com/2013/12/how-to-make-smaller-c-and-c-binaries.html

PREFIX ?= /usr
FAILED := $(shell mktemp -u)
CFLAGS += -std=c99 $(if $(RPM_OPT_FLAGS), $(RPM_OPT_FLAGS), -O2 -DNDEBUG -s -fno-exceptions)
boot.c := bootstrap~
repl.o := tmp/repl.o

CFLAGS += $(if $(HAS_DLOPEN), -DHAS_DLOPEN=1, -DHAS_DLOPEN=0)\
          $(if $(HAS_SOCKETS), -DHAS_SOCKETS=1, -DHAS_SOCKETS=0)\
          $(if $(HAS_SECCOMP),, -DHAS_SANDBOX=0)

#Debian i586 fix
ifeq ($(CC),gcc)
CFLAGS += -I/usr/include/$(shell gcc -print-multiarch)
endif
#

ifeq ($(UNAME),Linux)
L := $(if HAS_DLOPEN, -ldl -lm)
endif

ifeq ($(UNAME),FreeBSD)
L := $(if HAS_DLOPEN, -lc -lm)
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

# Mac OS X                    Darwin
# Cygwin 32-bit (Win-XP)      CYGWIN_NT-5.1
# Cygwin 32-bit (Win-7 32-bit)CYGWIN_NT-6.1
# Cygwin 32-bit (Win-7 64-bit)CYGWIN_NT-6.1-WOW64
# Cygwin 64-bit (Win-7 64-bit)CYGWIN_NT-6.1
# MinGW (Windows 7 32-bit)    MINGW32_NT-6.1
# MinGW (Windows 10 64-bit)   MINGW64_NT-10.0
# Interix (Services for UNIX) Interix
# MSYS                        MSYS_NT-6.1
# Android                     Linux
# coreutils                   Linux
# CentOS                      Linux
# Fedora                      Linux
# Gentoo                      Linux
# Red Hat Linux               Linux
# Linux Mint                  Linux
# openSUSE                    Linux
# Ubuntu                      Linux
# Unity Linux                 Linux
# Manjaro Linux               Linux
# OpenWRT r40420              Linux
# Debian (Linux)              Linux
# Debian (GNU Hurd)           GNU
# Debian (kFreeBSD)           GNU/kFreeBSD
# FreeBSD                     FreeBSD
# NetBSD                      NetBSD
# DragonFlyBSD                DragonFly
# Haiku                       Haiku
# NonStop                     NONSTOP_KERNEL
# QNX                         QNX
# ReliantUNIX                 ReliantUNIX-Y
# SINIX                       SINIX-Y
# Tru64                       OSF1
# Ultrix                      ULTRIX
# IRIX 32 bits                IRIX
# IRIX 64 bits                IRIX64
# MINIX                       Minix
# Solaris                     SunOS
# UWIN (64-bit Windows 7)     UWIN-W7
# SYS$UNIX:SH on OpenVMS      IS/WB
# z/OS USS                    OS/390
# Cray                        sn5176
# (SCO) OpenServer            SCO_SV
# (SCO) System V              SCO_SV
# (SCO) UnixWare              UnixWare
# IBM AIX                     AIX
# IBM i with QSH              OS400
# HP-UX                       HP-UX

# http://www.gnu.org/prep/standards/html_node/DESTDIR.html
# http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_266
# "Multiple successive slashes are considered to be the same as one slash."
DESTDIR?=


#main
#debug: src/olvm.c tmp/repl.o
#	$(CC) -std=c99 -O0 -g  src/olvm.c tmp/repl.o -o ol \
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
	@for F in r5rs lang libraries/otus libraries/owl libraries/lib libraries/etc libraries/scheme ;do \
	   echo installing $$F libraries... ;\
	   install -d $(DESTDIR)$(PREFIX)/lib/ol/$$F ;\
	   install -D -m 644 $$F/* $(DESTDIR)$(PREFIX)/lib/ol/$$F ;\
	done
	@echo Installing OpenGL libraries...
	install -d $(DESTDIR)$(PREFIX)/lib/ol/OpenGL
	install -D -m 644 libraries/OpenGL/*.scm $(DESTDIR)$(PREFIX)/lib/ol/OpenGL
	@for F in libraries/OpenGL/ARB libraries/OpenGL/EXT ;do \
	   echo installing $$F libraries... ;\
	   install -d $(DESTDIR)$(PREFIX)/lib/ol/$$F ;\
	   install -D -m 644 $$F/* $(DESTDIR)$(PREFIX)/lib/ol/$$F ;\
	done
	@echo Installing OpenCL libraries...
	install -d $(DESTDIR)$(PREFIX)/lib/ol/OpenCL
	install -D -m 644 libraries/OpenCL/*.scm $(DESTDIR)$(PREFIX)/lib/ol/OpenCL
	@echo Installing OpenAL libraries...
	install -d $(DESTDIR)$(PREFIX)/lib/ol/OpenAL
	install -D -m 644 libraries/OpenAL/*.scm $(DESTDIR)$(PREFIX)/lib/ol/OpenAL

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
#clang: src/olvm.c tmp/repl.o
#	clang-3.5 -fblocks src/olvm.c tmp/repl.o -ldl -lBlocksRuntime -o ol-c


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
	   }" | $(CC) -xc - $3 -o /dev/null 2>/dev/null; then\
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
	   }" | $(CC) -xc - $3 -o /tmp/$2.$$$$; then\
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
ol: src/olvm.c include/olvm.h tmp/repl.o
	$(CC) $(CFLAGS) src/olvm.c tmp/repl.o -o $@ \
	   -Xlinker --export-dynamic $(L)
	@echo Ok.

src/olvm.c: extensions/ffi.c
	touch src/olvm.c

vm: src/olvm.c include/olvm.h
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L)
	@echo Ok.
vm32: src/olvm.c include/olvm.h
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L) -m32 -DOLVM_FFI=0
	@echo Ok.
vm64: src/olvm.c include/olvm.h
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L) -m64 -DOLVM_FFI=0
	@echo Ok.


olvm.js: src/olvm.c include/olvm.h extensions/ffi.c
	emcc src/olvm.c -Oz \
	   -D NAKED_VM=1 -D HAS_DLOPEN=1 \
	   -o olvm.js \
	   -s EMTERPRETIFY=1 -s EMTERPRETIFY_ASYNC=1 \
	   -s MAIN_MODULE=1 \
	   -s EXPORTED_FUNCTIONS="['_main', '_OL_ffi']" \
	   --memory-init-file 0

talkback: src/olvm.c tmp/repl.o extensions/talkback/talkback.c extensions/talkback/sample.c
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -DEMBEDDED_VM -DOLVM_FFI=1 -o $@ -Iinclude \
	   tmp/repl.o extensions/talkback/talkback.c extensions/talkback/sample.c -pthread \
	   -Xlinker --export-dynamic $(L)


tmp/repl.o: repl
	ld -r -b binary -o tmp/repl.o repl
src/slim.c: repl src/slim.lisp
	vm repl <src/slim.lisp >src/slim.c


recompile: boot.fasl
boot.fasl: vm repl src/ol.scm r5rs/*.scm lang/*.scm libraries/otus/lisp.scm libraries/owl/*.scm
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

test32: $(wildcard tests/*.scm)
	@echo "-- test32 ----------"
	@rm -f $(FAILED)
	@$(CC) $(CFLAGS) src/olvm.c tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm32d $(L) -m32
	@./vm32d
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -Iinclude -DNAKED_VM -DOLVM_FFI=1 -o ffi32 $(L) -m32 -Xlinker --export-dynamic
	@for F in $^ ;do \
	   echo -n "Testing $$F ... " ;\
	   if ./ffi32 >/dev/null; then\
	      echo "Ok." ;\
	   else \
	      echo "\033[0;31mFailed!\033[0m" ;\
	      touch $(FAILED) ;\
	   fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi

test64: $(wildcard tests/*.scm)
	@echo "-- test64 ----------"
	@rm -f $(FAILED)
	@$(CC) $(CFLAGS) src/olvm.c tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm64d $(L) -m32
	@./vm64d
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -Iinclude -DNAKED_VM -DOLVM_FFI=1 -o ffi64 $(L) -m32 -Xlinker --export-dynamic
	@for F in $^ ;do \
	   echo -n "Testing $$F ... " ;\
	   if ./ffi64 >/dev/null; then\
	      echo "Ok." ;\
	   else \
	      echo "\033[0;31mFailed!\033[0m" ;\
	      touch $(FAILED) ;\
	   fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi

test: test64
	@echo "passed!"


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
	@echo "--------------------"
	@echo "32-bit:"
	@echo "-------"
ifeq ($(HAS_32CDEFS),1)
	$(CC) $(CFLAGS) src/olvm.c tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm32d $(L) -m32
	./vm32d
else
	@echo "No 32-bit support enabled."
	@echo "For ubuntu you can type, for example, 'sudo apt install libc6-dev-i386'"
endif
	@echo ""
ifeq ($(LBITS),64)
	@echo "64-bit:"
	@echo "-------"
	$(CC) $(CFLAGS) src/olvm.c tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm64d $(L) -m64
	./vm64d
	@echo ""
endif
	@echo "ffi tests (32- and 64-bit, if possible):"
	@echo "----------------------------------------"
ifeq ($(HAS_32CDEFS),1)
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -Iinclude -DNAKED_VM -DOLVM_FFI=1 -o ffi32 $(L) -m32 -Xlinker --export-dynamic
	   @echo -n "Testing 32-bit ffi ... "
	   @if ./ffi32 repl <tests/ffi.scm | diff - tests/ffi.scm.ok >/dev/null; then\
	      echo "Ok.";\
	   else \
	      echo "failed." ;\
	      touch $(FAILED);\
	   fi
	@echo ""
endif
ifeq ($(LBITS),64)
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -Iinclude -DNAKED_VM -DOLVM_FFI=1 -o ffi64 $(L) -m64 -Xlinker --export-dynamic
	   @echo -n "Testing 64-bit ffi ... "
	   @if ./ffi64 repl <tests/ffi.scm | diff - tests/ffi.scm.ok >/dev/null; then\
	      echo "Ok.";\
	   else \
	      echo "failed." ;\
	      touch $(FAILED);\
	   fi
	@echo ""
endif
	@echo "common (32- and 64-bit simulatenously):"
	@echo "---------------------------------------"
ifeq ($(HAS_32CDEFS),1)
	@make vm32
endif	
ifeq ($(LBITS),64)
	@make vm64
endif	
	@for F in $^ ;do \
	   echo -n "Testing $$F ... " ;\
	   if $(vm32) >/dev/null && $(vm64) >/dev/null; then\
	      echo "Ok." ;\
	   else \
	      echo "\033[0;31mFailed!\033[0m" ;\
	      touch $(FAILED) ;\
	   fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"

embed: extensions/embed/sample.c
	$(CC) extensions/embed/sample.c src/olvm.c tmp/repl.o -std=c99 -ldl -DEMBEDDED_VM -DHAS_DLOPEN=1 -DOLVM_FFI=1 -o embed \
	-Xlinker --export-dynamic -Iinclude -lm

# simple only target platform size tests
check: $(filter-out tests/ffi.scm,$(wildcard tests/*.scm))
	@rm -f $(FAILED)
	@echo "Internal VM testing:"
	   @$(CC) $(CFLAGS) src/olvm.c tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm-check $(L)
	@echo ""
	   @./vm-check
	@echo ""
	@echo "ffi test:"
	@echo "---------------------------------------"
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -Iinclude -DNAKED_VM -o ffi $(L) -Xlinker --export-dynamic
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
