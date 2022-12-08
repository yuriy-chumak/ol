export PATH := .:$(PATH)
$(shell mkdir -p config)
export OL_HOME=libraries

# detect required packages
-include configure.mk

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
ol32.exe: CC := i686-w64-mingw32-gcc
ol64.exe: CC:=x86_64-w64-mingw32-gcc

# cleanup while insuccessfull builds
# ----------------------------------
$(shell [ -s tmp/repl.c ] || rm -rf tmp/repl.c)

# source code dependencies and flags
# ----------------------------------
include dependencies.mk

# autogenerations
# ----------------------------------

includes/ol/vm.h: src/olvm.c
	sed -e '/\/\/ <!--/,/\/\/ -->/d' $^ >$@

tmp/repl.c: repl
	xxd --include repl >tmp/repl.c
# or
#	echo '(display "unsigned char repl[] = {") (lfor-each (lambda (x) (for-each display (list x ","))) (file->bytestream "repl")) (display "0};")'| ./vm repl> tmp/repl.c
# or
#	@od -An -vtx1 repl| tr -d '\n'| sed \
#	   -e 's/^ /0x/' -e 's/ /,0x/g' \
#	   -e 's/^/unsigned char repl[] = {/' \
#	   -e 's/$$/};/'> $@

doc/olvm.md: src/olvm.c extensions/ffi.c
	cat src/olvm.c extensions/ffi.c| tools/makedoc >doc/olvm.md

libraries/owl/unicode-char-folds.scm:
	echo "(define char-folds '(" >libraries/owl/unicode/char-folds.scm
	curl https://www.unicode.org/Public/14.0.0/ucd/CaseFolding.txt |\
	   grep "[0-9A-F]* [SFC]; " |\
	   sed -re 's/ #.*//' -e 's/( [SFC])?;//g' -e 's/^/ /' -e 's/ / #x/g' -e 's/ /(/' -e 's/$$/)/' |\
	   tr "[A-F]" "[a-f]" >> libraries/owl/unicode/char-folds.scm
	echo '))' >>libraries/owl/unicode/char-folds.scm

# compiler flags
# ----------------------------------
## 'os independent' flags

CFLAGS += -std=gnu99 -fno-exceptions
CFLAGS_CHECK   := -O0 -g2 -Wall -DWARN_ALL
CFLAGS_DEBUG   := -O0 -g2 -Wall
CFLAGS_RELEASE := $(if $(RPM_OPT_FLAGS), $(RPM_OPT_FLAGS), -O2 -DNDEBUG)
CFLAGS_RELEASE += -DCAR_CHECK=0 -DCDR_CHECK=0

CFLAGS += -DHAS_SOCKETS=$(if $(HAS_SOCKETS),1,0)
CFLAGS += -DHAS_DLOPEN=$(if $(HAS_DLOPEN),1,0)
CFLAGS += -DHAS_SANDBOX=$(if $(HAS_SECCOMP),1,0)

VERSION ?= $(shell echo `git describe --tags \`git rev-list --tags --max-count=1\``-`git rev-list HEAD --count`-`git log --pretty=format:'%h' -n 1`)

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

## 'os dependent' flags

UNAME ?= $(shell uname -s)

# Linux
ifeq ($(UNAME),Linux)
ifeq ($(CC), tcc)
  L := $(if $(HAS_DLOPEN), -ldl)
else
  L := $(if $(HAS_DLOPEN), -ldl) \
       -Xlinker --export-dynamic
endif

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

ifeq ($(UNAME),Darwin)
  CFLAGS += -DSYSCALL_SYSINFO=0
  PREFIX ?= /usr/local
endif

# -----------------------------------------------

## 'clean/install' part
clean:
	rm -f boot.fasl
	rm -f ./vm ./ol ./olvm ./libol.so
	rm -r tmp/*

-include extras/setup.mk

# -----------------------------------------------
## builds

debug: CFLAGS += $(CFLAGS_DEBUG)
debug: vm ol olvm libol.so

release: CFLAGS += $(CFLAGS_RELEASE)
release: vm ol olvm libol.so

perf: CFLAGS += -O2 -g3 -DNDEBUG -Wall
perf: vm ol olvm libol.so


slim: CFLAGS += -DHAS_SOCKETS=0 -DHAS_DLOPEN=0 -DHAS_SANDBOX=0
slim: release

minimal: CFLAGS += -DOLVM_FFI=0 -DHAS_SOCKETS=0 -DHAS_DLOPEN=0 -DHAS_SANDBOX=0
minimal: release

# ffi test build
ffi: CFLAGS += $(CFLAGS_DEBUG)
ffi: src/olvm.c extensions/ffi.c tests/ffi.c
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   tests/ffi.c \
	   $(CFLAGS) $(L)
	@echo Ok.

## android build
NDK_ROOT ?=/opt/android/ndk
android: jni/*.c tmp/repl.c
	$(NDK_ROOT)/ndk-build

# ol
vm:
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   $(CFLAGS) -DPREFIX=\"$(PREFIX)\" $(L)
	@echo Ok.

ol:
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   $(CFLAGS) -DPREFIX=\"$(PREFIX)\" $(L) \
	   tmp/repl.c -DREPL=repl
	@echo Ok.

libol.so:
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   $(CFLAGS) -DPREFIX=\"$(PREFIX)\" $(L) \
	   tmp/repl.c -DREPL=repl \
	   -DOLVM_NOMAIN -shared -fPIC
	@echo Ok.

# real name of
olvm: vm
	cp vm olvm

# emscripten version 1.37.40+ needed
ol.wasm: src/olvm.c tmp/repl.c
	emcc src/olvm.c \
	     tmp/repl.c -DREPL=repl \
	     extensions/ffi.c \
	     -O3 -o ol.html \
	         -Iincludes \
	   --use-preload-plugins \
	   -DHAS_DLOPEN=1 -DHAS_SOCKETS=0 \
	   -s ASYNCIFY \
	   -s ASSERTIONS=0 \
	   -s ALLOW_MEMORY_GROWTH=1 \
	   -s FORCE_FILESYSTEM=0 \
	   -s WASM=1 && \
	# fix bugs in emscripten code \
	sed -i -r -e 's/(if\(result===undefined&&bytesRead===0\)\{)(throw)/\1bytesRead=-1;\2/g' \
	          -e 's/(Input: "\);if\(result!==null)/\1\&\&result!==undefined/' \
	          -e 's/(if\(!result\)\{return )null/\1result/' \
	    ol.js

#	          -e 's/(symbol=)(UTF8ToString\(symbol\))/\1"_"+\2/' ol.js
#	   extensions/ffi.c
#	   -s MAIN_MODULE \
#	   -s EXPORTED_FUNCTIONS=['_main','_OLVM_ffi','_OLVM_idf','_OLVM_sizeof','_OLVM_mkcb'] \

# important note: fix for emscripten to asyncify stdin:
# @@ -755,6 +755,7 @@ var TTY = {
#                                         throw new FS.ErrnoError(29)
#                                 }
#                                 if (result === undefined && bytesRead === 0) {
# +                                       bytesRead = -1;
#                                         throw new FS.ErrnoError(6)
#                                 }
#                                 if (result === null || result === undefined) break;
# @@ -804,7 +805,7 @@ var TTY = {
#                                         }
#                                 } else if (typeof window != "undefined" && typeof window.prompt == "function") {
#                                         result = window.prompt("Input: ");
# -                                       if (result !== null) {
# +                                       if (result !== null && result !== undefined) {
#                                                 result += "\n"
#                                         }
#                                 } else if (typeof readline == "function") {
# @@ -814,7 +815,7 @@ var TTY = {
#                                         }
#                                 }
#                                 if (!result) {
# -                                       return null
# +                                       return result
#                                 }
#                                 tty.input = intArrayFromString(result, true)
#                         }

# windows

# You can debug ol.exe using "winedbg --gdb ol.exe"
# require mingw-w64-i686-dev (+ gcc-mingw-w64-i686) or/and mingw-w64-x86-64-dev (+ gcc-mingw-w64-x86-64)
%.exe: MINGWCFLAGS += -std=gnu99 -fno-exceptions
%.exe: MINGWCFLAGS += -DHAS_DLOPEN=1
%.exe: MINGWCFLAGS += -DHAS_SOCKES=1
%.exe: MINGWCFLAGS += $(CFLAGS_RELEASE)
%.exe: src/olvm.c extensions/ffi.c tmp/repl.c
	$(CC) src/olvm.c tmp/repl.c -o $@ \
	   -DREPL=repl -DOLVM_FFI=1 \
	   -Iincludes -Iincludes/win32 extensions/ffi.c \
	   $(MINGWCFLAGS) -lws2_32

# compiling the Ol language
recompile: boot.fasl
boot.fasl: vm repl src/*.scm lang/*.scm libraries/otus/*.scm libraries/owl/*.scm libraries/scheme/*.scm
	@vm repl --version="$(VERSION)" --home=.:libraries \
	   src/ol.scm
	@if diff boot.fasl repl>/dev/null;then\
	   echo '\033[1;32m  `___`  \033[0m' ;\
	   echo '\033[1;32m  (o,o)  \033[0m' ;\
	   echo '\033[1;32m  \)  )  \033[0m' ;\
	   echo '\033[1;32m___"_"___\033[0m' ;\
	   echo '\033[1;32mBuild Ok.\033[0m' ;\
	else \
	   echo `stat -c%s repl` -\> `stat -c%s $@` ;\
	   cp -b $@ repl ;$(MAKE) $@ ;\
	fi

# compiling infix math notation
libraries/owl/math/infix.scm: tools/make-math-infix.scm vm
	./vm repl tools/make-math-infix.scm >$@

# additional targets (like packaging, tests, etc.)
MAKEFILE_MAIN=1
-include tests/Makefile
-include tests/rosettacode/Makefile
-include config/Makefile

# documentation samples check
check: ol check-reference
check-reference: $(wildcard doc/reference/*.md)
	@echo "Testing reference samples:"
	@./ol tools/check-reference.lisp $^ && echo $(ok) || echo $(failed)
