export PATH := .:$(PATH)
$(shell mkdir -p config)
export OL_HOME=libraries

.PHONY: all debug release slim config recompile install uninstall clean check android

all: release

CC ?= gcc
LD ?= ld
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
L := $(if HAS_DLOPEN, -ldl)

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

NDK_ROOT?=/opt/android/ndk
android: jni/*.c
	$(NDK_ROOT)/ndk-build

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
	$(LD) -r -b binary -o $(repl.o) repl

# please, use emscripten version 1.37.40, because
# fockin' emscripten team broke all again!
olvm.js: src/olvm.c include/olvm.h extensions/ffi.c
	emcc src/olvm.c -Os -m32 \
	   -D NAKED_VM=1 -D HAS_DLOPEN=1 \
	   -D OLVM_BUILTIN_FMATH=1 \
	   -o olvm.js \
	   -s EMTERPRETIFY=1 -s EMTERPRETIFY_ASYNC=1 \
	   -s MAIN_MODULE=1 \
	   -s WASM=0 \
	   -s EXPORTED_FUNCTIONS="['_main']" \
	   --memory-init-file 0

# compiling the Ol language
recompile: boot.fasl
boot.fasl: CFLAGS += $(CFLAGS_RELEASE) # will rebuild in release, for speed
boot.fasl: vm repl src/*.scm lang/*.scm libraries/otus/*.scm libraries/owl/*.scm libraries/scheme/*.scm
	@vm repl src/ol.scm --version "`git describe --tags \`git rev-list --tags --max-count=1\``-`git rev-list HEAD --count`-`git log --pretty=format:'%h' -n 1`"
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

# compiling unicode table
libraries/owl/unicode-char-folds.scm:
	echo "(define char-folds '(" >libraries/owl/unicode-char-folds.scm
	curl https://www.unicode.org/Public/12.1.0/ucd/CaseFolding-12.1.0d2.txt |\
	   grep "[0-9A-F]* [SFC]; " |\
	   sed -re 's/ #.*//' -e 's/( [SFC])?;//g' -e 's/^/ /' -e 's/ / #x/g' -e 's/ /(/' -e 's/$$/)/' |\
	   tr "[A-F]" "[a-f]" >> libraries/owl/unicode-char-folds.scm
	echo '))' >>libraries/owl/unicode-char-folds.scm



#embed sample
embed: tests/embed.c src/olvm.c extensions/embed.h $(repl.o)
	$(CC) tests/embed.c src/olvm.c $(repl.o) -std=c99 -ldl -DEMBEDDED_VM -DHAS_DLOPEN=1 -DOLVM_FFI=1 -o embed \
	-Xlinker --export-dynamic -Iextensions -Iinclude -lm $(CFLAGS_DEBUG)


# --------------------------------------------------------------------
# | ARM Core | Command Line Options                       | multilib |
# |----------|--------------------------------------------|----------|
# |Cortex-M0+| -mthumb -mcpu=cortex-m0plus                | armv6-m  |
# |Cortex-M0 | -mthumb -mcpu=cortex-m0                    |          |
# |Cortex-M1 | -mthumb -mcpu=cortex-m1                    |          |
# |          |--------------------------------------------|          |
# |          | -mthumb -march=armv6-m                     |          |
# |----------|--------------------------------------------|----------|
# |Cortex-M3 | -mthumb -mcpu=cortex-m3                    | armv7-m  |
# |          |--------------------------------------------|          |
# |          | -mthumb -march=armv7-m                     |          |
# |----------|--------------------------------------------|----------|
# |Cortex-M4 | -mthumb -mcpu=cortex-m4                    | armv7e-m |
# |(No FP)   |--------------------------------------------|          |
# |          | -mthumb -march=armv7e-m                    |          |
# |----------|--------------------------------------------|----------|
# |Cortex-M4 | -mthumb -mcpu=cortex-m4 -mfloat-abi=softfp | armv7e-m |
# |(Soft FP) | -mfpu=fpv4-sp-d16                          | /softfp  |
# |          |--------------------------------------------|          |
# |          | -mthumb -march=armv7e-m -mfloat-abi=softfp |          |
# |          | -mfpu=fpv4-sp-d16                          |          |
# |----------|--------------------------------------------|----------|
# |Cortex-M4 | -mthumb -mcpu=cortex-m4 -mfloat-abi=hard   | armv7e-m |
# |(Hard FP) | -mfpu=fpv4-sp-d16                          | /fpu     |
# |          |--------------------------------------------|          |
# |          | -mthumb -march=armv7e-m -mfloat-abi=hard   |          |
# |          | -mfpu=fpv4-sp-d16                          |          |
# |----------|--------------------------------------------|----------|
# |Cortex-R4 | [-mthumb] -march=armv7-r                   | armv7-ar |
# |Cortex-R5 |                                            | /thumb   |
# |Cortex-R7 |                                            |          |
# |(No FP)   |                                            |          |
# |----------|--------------------------------------------|----------|
# |Cortex-R4 | [-mthumb] -march=armv7-r -mfloat-abi=softfp| armv7-ar |
# |Cortex-R5 | -mfpu=vfpv3-d16                            | /thumb   |
# |Cortex-R7 |                                            | /softfp  |
# |(Soft FP) |                                            |          |
# |----------|--------------------------------------------|----------|
# |Cortex-R4 | [-mthumb] -march=armv7-r -mfloat-abi=hard  | armv7-ar |
# |Cortex-R5 | -mfpu=vfpv3-d16                            | /thumb   |
# |Cortex-R7 |                                            | /fpu     |
# |(Hard FP) |                                            |          |
# |----------|--------------------------------------------|----------|
# |Cortex-A* | [-mthumb] -march=armv7-a                   | armv7-ar |
# |(No FP)   |                                            | /thumb   |
# |----------|--------------------------------------------|----------|
# |Cortex-A* | [-mthumb] -march=armv7-a -mfloat-abi=softfp| armv7-ar |
# |(Soft FP) | -mfpu=vfpv3-d16                            | /thumb   |
# |          |                                            | /softfp  |
# |----------|--------------------------------------------|----------|
# |Cortex-A* | [-mthumb] -march=armv7-a -mfloat-abi=hard  | armv7-ar |
# |(Hard FP) | -mfpu=vfpv3-d16                            | /thumb   |
# |          |                                            | /fpu     |
# --------------------------------------------------------------------

arm: src/olvm.c include/olvm.h
	arm-linux-gnueabihf-gcc-5 src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L) \
	   -march=armv7-a -mfpu=neon-vfpv4 \
	   $(CFLAGS)
	@echo Ok.

#-mcpu=cortex-m3
#	   -mfpu=vfp \
 #-mthumb -mno-thumb-interwork -mfpu=vfp -msoft-float -mfix-cortex-m3-ldrd \

# additional targets (like packaging, tests, etc.)
MAKEFILE_MAIN=1
-include tests/Makefile
-include tests/rosettacode/Makefile
-include config/Makefile
