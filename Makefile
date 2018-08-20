export PATH := .:$(PATH)
$(shell mkdir -p config)
export OL_HOME=libraries

.PHONY: all debug release slim config recompile install uninstall clean tests check rosettacode

all: release

CC ?= gcc
UNAME ?= $(shell uname -s)

# 'configure' part:
# check the library and/or function
exists = $(shell echo "\
	   \#include $2\n\
	   char $3();\
	   \
	   int main() {\
	      return $3();\
	      return 0;\
	   }" | $(CC) $1 -xc - $4 -o /dev/null 2>/dev/null && echo 1)
repl.o := tmp/repl.o

# default platform features
HAS_DLOPEN  ?= $(call exists,,<stdlib.h>, dlopen, -ldl)
HAS_SECCOMP ?= $(call exists,,<linux/seccomp.h>, prctl)
HAS_SOCKETS ?= $(call exists,,<stdlib.h>, socket)

CFLAGS += -std=c99 -fno-exceptions
CFLAGS_DEBUG   := -O0 -g2
CFLAGS_RELEASE := $(if $(RPM_OPT_FLAGS), $(RPM_OPT_FLAGS), -O2 -DNDEBUG -s)

CFLAGS += $(if $(HAS_DLOPEN), -DHAS_DLOPEN=1, -DHAS_DLOPEN=0)\
          $(if $(HAS_SOCKETS), -DHAS_SOCKETS=1, -DHAS_SOCKETS=0)\
          $(if $(HAS_SECCOMP),, -DHAS_SANDBOX=0)

ifeq ($(UNAME),Linux)
L := $(if HAS_DLOPEN, -ldl) -lm

#Debian i586 fix
ifeq ($(CC),gcc)
CFLAGS += -I/usr/include/$(shell gcc -print-multiarch)
endif
#
endif #Linux

ifeq ($(UNAME),FreeBSD)
L := $(if HAS_DLOPEN, -lc) -lm
endif
ifeq ($(UNAME),NetBSD)
L := $(if HAS_DLOPEN, -lc) -lm
endif
ifeq ($(UNAME),OpenBSD)
L := $(if HAS_DLOPEN, -lc) -ftrampolines
endif

# Windows+MinGW
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
PREFIX ?= /usr

clean:
	rm -f boot.fasl
	rm -f $(repl.o)
	rm -f ./vm ./ol
	rm -r tmp/*

install: ol repl
	# install Ol executable to $(DESTDIR)$(PREFIX)/bin:
	@echo Installing main binary...
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m755 ol $(DESTDIR)$(PREFIX)/bin/ol
	# install Ol binary REPL to $(DESTDIR)$(PREFIX)/lib/ol:
	@echo Installing REPL...
	install -d $(DESTDIR)$(PREFIX)/lib/ol
	install -m644 repl $(DESTDIR)$(PREFIX)/lib/ol/repl
	# and libraries to $(DESTDIR)$(PREFIX)/lib/ol:
	@echo Installing basic libraries...
	find libraries -type f -exec bash -c 'install -Dm644 "$$0" "$(DESTDIR)$(PREFIX)/lib/ol/$${0/libraries\/}"' {} \;

uninstall:
	-rm -f $(DESTDIR)$(PREFIX)/bin/ol
	-rm -rf $(DESTDIR)$(PREFIX)/lib/ol

debug: CFLAGS += $(CFLAGS_DEBUG)
debug: vm repl ol

release: CFLAGS += $(CFLAGS_RELEASE)
release: vm repl ol

slim: CFLAGS += -DOLVM_FFI=0 
slim: release

# ol
vm: src/olvm.c include/olvm.h
	$(CC) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L) \
	   $(CFLAGS)
	@echo Ok.

ol: src/olvm.c include/olvm.h $(repl.o)
	$(CC) src/olvm.c $(repl.o) -o $@ \
	   -Xlinker --export-dynamic $(L)\
	   $(CFLAGS)
	@echo Ok.

src/olvm.c: extensions/ffi.c
	touch src/olvm.c

$(repl.o): repl
	ld -r -b binary -o $(repl.o) repl

# please, use emscripten version 1.37.40, because
# fockin' emscripten team broke all again!
olvm.js: src/olvm.c include/olvm.h extensions/ffi.c
	emcc src/olvm.c -Os -m32 \
	   -D NAKED_VM=1 -D HAS_DLOPEN=1 \
	   -o olvm.js \
	   -s EMTERPRETIFY=1 -s EMTERPRETIFY_ASYNC=1 \
	   -s MAIN_MODULE=1 \
	   -s BINARYEN_METHOD='asmjs' \
	   -s EXPORTED_FUNCTIONS="['_main']" \
	   --memory-init-file 0

recompile: boot.fasl
boot.fasl: CFLAGS += $(CFLAGS_RELEASE) # will rebuild in release, for speed
boot.fasl: vm repl src/*.scm lang/*.scm libraries/scheme/*.scm libraries/scheme/r5rs/*.scm libraries/otus/lisp.scm libraries/owl/*.scm
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

embed: extensions/embed/sample.c
	$(CC) extensions/embed/sample.c src/olvm.c $(repl.o) -std=c99 -ldl -DEMBEDDED_VM -DHAS_DLOPEN=1 -DOLVM_FFI=1 -o embed \
	-Xlinker --export-dynamic -Iinclude -lm

# additional targets (like packaging, tests, etc.)
MAKEFILE_MAIN=1
-include tests/Makefile
-include tests/rosetta-code/Makefile
-include config/Makefile
