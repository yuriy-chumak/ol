export PATH := .:$(PATH)
$(shell mkdir -p config)
export OL_HOME=libraries

.PHONY: all debug release check slim config recompile install uninstall clean android
.PHONY: describe

all: release
describe: all
	./vm --version
	./ol --version
	echo "(print (syscall 63))"|./vm repl

# default toolchain
CC ?= gcc
LD ?= ld

# cleanup while insuccessfull builds
# ----------------------------------
$(shell [ -s tmp/repl.c ] || rm -rf tmp/repl.c)

# source code dependencies and flags
# ----------------------------------

# posix
vm: src/olvm.c
vm: extensions/ffi.c
ol: src/olvm.c
ol: extensions/ffi.c
ol: tmp/repl.c

# win/wine
ol.exe: src/olvm.c
ol.exe: extensions/ffi.c
ol.exe: tmp/repl.c

# sources
extensions/ffi.c: CFLAGS += -Iincludes

# 
extensions/ffi.c: includes/ol/vm.h
includes/ol/vm.h: src/olvm.c
	sed -n '/__OLVM_C__/q;p' $^ >$@

tmp/repl.c: repl
	xxd --include repl >tmp/repl.c
# or
#	echo '(display "unsigned char repl[] = {") (lfor-each (lambda (x) (for-each display (list x ","))) (file->bytestream "repl")) (display "0};")'| ./vm repl> tmp/repl.c
# or
#	@od -An -vtx1 repl| tr -d '\n'| sed \
#	   -e 's/^ /0x/' -e 's/ /,0x/g' \
#	   -e 's/^/unsigned char repl[] = {/' \
#	   -e 's/$$/};/'> $@


## 'configure' part:
# check the library and/or function
exists = $(shell echo "\
	char $3();\
	\
	int main() {\
	   return $3();\
	}" |$(CC) $1 -xc - $4 -o /dev/null 2>/dev/null && echo 1)

sizeof = $(shell SIZEOF=`mktemp /tmp/sizeof.XXXXXXXXX`; \
	trap "{ rm -f $$SIZEOF; }" EXIT; \
	echo "\
	void main() {\
	   printf(\"%d\", sizeof($1));\
	}" |$(CC) -xc - \
	          -include stdio.h $3\
	          -o $$SIZEOF 2>stderr && $$SIZEOF)

offsetof = $(shell OFFSETOF=`mktemp /tmp/offsetof.XXXXXXXXX`; \
	trap "{ rm -f $$OFFSETOF; }" EXIT; \
	echo "\
	void main() {\
	   printf(\"%d\", offsetof($1,$2));\
	}" |$(CC) -xc - \
	          -include X11/Xlib.h -include stdio.h $3\
	          -o $$OFFSETOF 2>/dev/null && $$OFFSETOF)

doc/olvm.md: src/olvm.c extensions/ffi.c
	cat src/olvm.c extensions/ffi.c| tools/makedoc >doc/olvm.md


# check required libs
# ifneq ($(call exists,,<unistd.h>),1)
#    $(error Looks like you have no gcc-multilib, please install.)
# endif

# default platform features
HAS_DLOPEN  ?= $(call exists,,stdlib.h, dlopen, -ldl)
HAS_SECCOMP ?= $(call exists,,linux/seccomp.h, prctl)
HAS_SOCKETS ?= $(call exists,,stdlib.h, socket)

CFLAGS += -std=gnu99 -fno-exceptions
CFLAGS_CHECK   := -O0 -g2 -Wall -DWARN_ALL
CFLAGS_DEBUG   := -O0 -g2 -Wall
CFLAGS_RELEASE := $(if $(RPM_OPT_FLAGS), $(RPM_OPT_FLAGS), -O2 -DNDEBUG)
#CFLAGS_RELEASE += -DCAR_CHECK=0 -DCDR_CHECK=0

# builtin "sin", "cos", "sqrt", etc. functions support
# can be disabled using -DOLVM_NO_BUILTIN_FMATH=1
ifneq ($(OLVM_BUILTIN_FMATH),0)
   CFLAGS += -lm
#  CFLAGS += -ffast-math -mfpmath=387
endif

#  clang is not a primary compiler and clang have no ability to remove
#  only one warning instance. I don't want to add SEVEN lines of code
#  to disable only ONE warning that in fact is not a warning but fully
#  planned behavior. so disable all same warnings to the release build.
# ifeq ($(CC),clang)
#    CFLAGS_RELEASE += -Wno-tautological-constant-out-of-range-compare
# endif

CFLAGS += -DHAS_SOCKETS=$(if $(HAS_SOCKETS),1,0) \
		  -DHAS_DLOPEN=$(if $(HAS_DLOPEN),1,0)   \
          -DHAS_SANDBOX=$(if $(HAS_SECCOMP),1,0)

# ===============================================
## 'os dependent' part
UNAME ?= $(shell uname -s)

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
# Cygwin 32-bit (Win-XP)      CYGWIN_NT-5.1
# Cygwin 32-bit (Win-7 32-bit)CYGWIN_NT-6.1
# Cygwin 32-bit (Win-7 64-bit)CYGWIN_NT-6.1-WOW64
# Cygwin 64-bit (Win-7 64-bit)CYGWIN_NT-6.1
# Mac OS X                    Darwin
# MinGW (Windows 7 32-bit)    MINGW32_NT-6.1
# MinGW (Windows 10 64-bit)   MINGW64_NT-10.0
# Interix (Services for UNIX) Interix
# MSYS                        MSYS_NT-6.1

# Linux
ifeq ($(UNAME),Linux)
  L := $(if $(HAS_DLOPEN), -ldl) \
       -Xlinker --export-dynamic

#Debian i586 fix
ifeq ($(CC),gcc)
  CFLAGS += -I/usr/include/$(shell gcc -print-multiarch)
endif

endif

# BSD
ifeq ($(UNAME),FreeBSD)
  L := $(if $(HAS_DLOPEN), -lc) -lm \
       -Xlinker --export-dynamic

  LD := ld.bfd
endif
ifeq ($(UNAME),NetBSD)
  L := $(if $(HAS_DLOPEN), -lc) -lm \
       -Xlinker --export-dynamic
endif
ifeq ($(UNAME),OpenBSD)
  L := $(if $(HAS_DLOPEN), -lc) \
       -Xlinker --export-dynamic
endif

# Windows+MinGW
ifeq ($(UNAME),MINGW32_NT-6.1)
  L := -lws2_32
endif

ifeq ($(UNAME),Darwin)
  CFLAGS += -DSYSCALL_SYSINFO=0
  PREFIX ?= /usr/local
endif
# -----------------------------------------------

## 'clean/install' part
# http://www.gnu.org/prep/standards/html_node/DESTDIR.html
# http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_266
# "Multiple successive slashes are considered to be the same as one slash."
DESTDIR?=
PREFIX ?= /usr

clean:
	rm -f boot.fasl
	rm -f ./vm ./ol
	rm -r tmp/*

install: ol includes/ol/vm.h
	# install Ol executable to $(DESTDIR)$(PREFIX)/bin:
	@echo Installing main binary...
	install -d $(DESTDIR)$(PREFIX)/bin
	install ol $(DESTDIR)$(PREFIX)/bin/ol
	# install Ol binary REPL to $(DESTDIR)$(PREFIX)/lib/ol:
	@echo Installing REPL...
	install -d $(DESTDIR)$(PREFIX)/lib/ol
	install -m 644 repl $(DESTDIR)$(PREFIX)/lib/ol/repl
	@echo Installing headers...
	install -d $(DESTDIR)$(PREFIX)/include/ol
	install -m 644 includes/ol/vm.h $(DESTDIR)$(PREFIX)/include/ol/vm.h
	install -m 644 includes/ol/ol.h $(DESTDIR)$(PREFIX)/include/ol/ol.h
	# and libraries to $(DESTDIR)$(PREFIX)/lib/ol:
	@echo Installing basic libraries...
	find libraries -type d -exec bash -c 'install -d "$(DESTDIR)$(PREFIX)/lib/ol/$${0/libraries\/}"' {} \;
	find libraries -type f -exec bash -c 'install -m 644 "$$0" "$(DESTDIR)$(PREFIX)/lib/ol/$${0/libraries\/}"' {} \;
	@echo Installing man page...
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	gzip <ol.1 >$(DESTDIR)$(PREFIX)/share/man/man1/ol.1.gz
	@echo Ok.

uninstall:
	-rm -f $(DESTDIR)$(PREFIX)/bin/ol
	-rm -rf $(DESTDIR)$(PREFIX)/lib/ol
	-rm -rf $(DESTDIR)$(PREFIX)/include/ol
	-rm /usr/share/man/man1/ol.1.gz

## actual 'building' part
debug: CFLAGS += $(CFLAGS_DEBUG)
debug: vm repl ol

release: CFLAGS += $(CFLAGS_RELEASE)
release: vm repl ol

slim: CFLAGS += -DHAS_SOCKETS=0 -DHAS_DLOPEN=0 -DHAS_SANDBOX=0
slim: release

ffi: CFLAGS += $(CFLAGS_DEBUG)
ffi: src/olvm.c extensions/ffi.c
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   tests/ffi.c \
	   $(CFLAGS) $(L)
	@echo Ok.


NDK_ROOT ?=/opt/android/ndk
android: jni/*.c tmp/repl.c
	$(NDK_ROOT)/ndk-build
#jni/repl.c: repl vm
#	echo '(display "unsigned char repl[] = {") (lfor-each (lambda (x) (for-each display (list x ","))) (file->bytestream "repl")) (display "0};")'| ./vm repl> jni/repl.c

# ol
vm:
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   $(CFLAGS) $(L)
	@echo Ok.

ol:
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   $(CFLAGS) $(L) \
	   tmp/repl.c -DREPL=repl
	@echo Ok.


# # emscripten version 1.37.40+
# deprecated repl.js: repl
# 	xxd --include repl >tmp/repl.c
# 	emcc tmp/repl.c -Os \
# 	   -o repl.js -Drepl=binary_repl_start \
# 	   -s WASM=0 -s SIDE_MODULE=1 \
# 	   -s NO_EXIT_RUNTIME=1 \
# 	   --memory-init-file 0

# deprecated oljs.js: extensions/embed.c extensions/embed.h
# 	emcc extensions/embed.c -Os \
# 	   -o oljs.js -Iinclude \
# 	   -s WASM=0 -s SIDE_MODULE=1 \
# 	   -s NO_EXIT_RUNTIME=1 \
# 	   --memory-init-file 0

# deprecated olvm.js: src/olvm.c include/ol/vm.h extensions/ffi.c repl.js oljs.js
# 	emcc src/olvm.c -Os \
# 	   -DNAKED_VM=1 -DEMBED_VM=1 -DHAS_DLOPEN=1 \
# 	   -o olvm.js \
# 	   -s WASM=0 -s SIDE_MODULE=1 \
# 	   -s NO_EXIT_RUNTIME=1 \
# 	   --memory-init-file 0

olvm.wasm: src/olvm.c tmp/repl.c
	emcc src/olvm.c extensions/wasm.c \
	     tmp/repl.c -DREPL=repl -Os \
	   -o olvm.html -Iincludes \
	   -DOLVM_NOMAIN=1 -DHAS_DLOPEN=0 \
	   -s ASSERTIONS=0 \
	   -s ALLOW_MEMORY_GROWTH=1 \
	   -s FORCE_FILESYSTEM=1 \
	   -s WASM=1 \
	   -s "EXTRA_EXPORTED_RUNTIME_METHODS=['cwrap']"\
	   -s "EXPORTED_FUNCTIONS=['_ol_init','_ol_eval']"

# tmp/emscripten.c:
# 	echo "#include <GL/gl.h>"      > tmp/emscripten.c
# 	echo "int main() { return 0;}" >>tmp/emscripten.c
# emscripten.js: tmp/emscripten.c
# 	EMCC_FORCE_STDLIBS=1 \
# 	emcc tmp/emscripten.c -Os \
# 	  -o emscripten.js \
# 	  -s WASM=0 \
# 	  -s MAIN_MODULE=1 -s LINKABLE=1 -s EXPORT_ALL=1 \
# 	  -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' \
# 	  --memory-init-file 0

# You can debug ol using "winedbg --gdb ol.exe"
# require mingw-w64-i686-dev (+ gcc-mingw-w64-i686) or/and mingw-w64-x86-64-dev (+ gcc-mingw-w64-x86-64)
ol32.exe: CC=i686-w64-mingw32-gcc
ol32.exe: ol.exe

ol64.exe: CC=x86_64-w64-mingw32-gcc
ol64.exe: ol.exe

ol.exe: MINGWCFLAGS += -std=gnu99 -fno-exceptions
ol.exe: MINGWCFLAGS += -DHAS_DLOPEN=1
ol.exe: MINGWCFLAGS += -DHAS_SOCKES=1
ol.exe: MINGWCFLAGS += $(CFLAGS_RELEASE)
ol.exe: src/olvm.c extensions/ffi.c tmp/repl.c
	$(CC) src/olvm.c tmp/repl.c -o $@ \
	   -DREPL=repl -DOLVM_FFI=1 \
	   -Iwin32 -Iincludes extensions/ffi.c \
	   $(MINGWCFLAGS) -lws2_32

# compiling the Ol language
recompile: boot.fasl
boot.fasl: vm repl src/*.scm lang/*.scm libraries/otus/*.scm libraries/owl/*.scm libraries/scheme/*.scm
	@vm repl --version="`git describe --tags \`git rev-list --tags --max-count=1\``-`git rev-list HEAD --count`-`git log --pretty=format:'%h' -n 1`" \
	   src/ol.scm
	@if diff boot.fasl repl>/dev/null;then\
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
	curl https://www.unicode.org/Public/12.1.0/ucd/CaseFolding.txt |\
	   grep "[0-9A-F]* [SFC]; " |\
	   sed -re 's/ #.*//' -e 's/( [SFC])?;//g' -e 's/^/ /' -e 's/ / #x/g' -e 's/ /(/' -e 's/$$/)/' |\
	   tr "[A-F]" "[a-f]" >> libraries/owl/unicode-char-folds.scm
	echo '))' >>libraries/owl/unicode-char-folds.scm

# compiling infix math notation
libraries/owl/math/infix.scm: make-math-infix.scm vm
	./vm repl make-math-infix.scm >$@


# computing native x11 variables
ifeq ($(UNAME),Linux)
x11_sizeof = $(shell echo "'|sizeof $1| (if x86? $(call sizeof,$1,,-m32 -include X11/Xlib.h) $(call sizeof,$1,,-m64 -include X11/Xlib.h))")
x11_offsetof = $(shell echo "'|$1.$2| (if x86? $(call offsetof,$1,$2,-m32 -include X11/Xlib.h) $(call offsetof,$1,$2,-m64 -include X11/Xlib.h))")
libraries/lib/x11/config.scm:
	@echo "(define-library (lib x11 config)\n\
	(export config)\n\
	(import (scheme core) (owl ff))\n\
	(begin\n\
	   (setq x86? (eq? (size (vm:cast 0 type-vptr)) 4))\n\
	   (define config {\n\
	      $(call x11_sizeof,XEvent)\n\
	      $(call x11_offsetof,XKeyEvent,keycode)\n\
	      $(call x11_offsetof,XButtonEvent,x)\n\
	      $(call x11_offsetof,XButtonEvent,y)\n\
	      $(call x11_offsetof,XButtonEvent,button)\n\
	      $(call x11_offsetof,XConfigureEvent,width)\n\
	      $(call x11_offsetof,XConfigureEvent,height)\n\
	      $(call x11_offsetof,XConfigureEvent,width)\n\
	   })\n\
	))" >$@
endif

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

arm: src/olvm.c
	arm-linux-gnueabihf-gcc-5 src/olvm.c -o $@ \
	   -Xlinker --export-dynamic $(L) \
	   -march=armv7-a -mfpu=neon-vfpv4 \
	   $(CFLAGS)
	@echo Ok.

#-mcpu=cortex-m3
#	   -mfpu=vfp \
# -mthumb -mno-thumb-interwork -mfpu=vfp -msoft-float -mfix-cortex-m3-ldrd \

# additional targets (like packaging, tests, etc.)
MAKEFILE_MAIN=1
-include tests/Makefile
-include tests/rosettacode/Makefile
-include config/Makefile
