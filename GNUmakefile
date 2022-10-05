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
libol.so: src/olvm.c
libol.so: extensions/ffi.c
libol.so: tmp/repl.c

# win/wine
ol%.exe: src/olvm.c
ol%.exe: extensions/ffi.c
ol%.exe: tmp/repl.c

# experimental
olvm: vm
	cp vm olvm

# sources
extensions/ffi.c: CFLAGS += -Iincludes
extensions/ffi.c: includes/ol/vm.h

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

ol32.exe: CC=i686-w64-mingw32-gcc
ol64.exe: CC=x86_64-w64-mingw32-gcc

doc/olvm.md: src/olvm.c extensions/ffi.c
	cat src/olvm.c extensions/ffi.c| tools/makedoc >doc/olvm.md

CFLAGS += -std=gnu99 -fno-exceptions
CFLAGS_CHECK   := -O0 -g2 -Wall -DWARN_ALL
CFLAGS_DEBUG   := -O0 -g2 -Wall
CFLAGS_RELEASE := $(if $(RPM_OPT_FLAGS), $(RPM_OPT_FLAGS), -O2 -DNDEBUG)
#CFLAGS_RELEASE += -DCAR_CHECK=0 -DCDR_CHECK=0

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

CFLAGS += -DHAS_SOCKETS=$(if $(HAS_SOCKETS),1,0) \
		  -DHAS_DLOPEN=$(if $(HAS_DLOPEN),1,0)   \
          -DHAS_SANDBOX=$(if $(HAS_SECCOMP),1,0)

# ===============================================
## 'os dependent' part
UNAME ?= $(shell uname -s)

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
	# install Ol executable(s) to $(DESTDIR)$(PREFIX)/bin:
	@echo Installing main binary...
	install -d $(DESTDIR)$(PREFIX)/bin
	install ol $(DESTDIR)$(PREFIX)/bin/ol
	@echo Installing ol virtual machine binary...
	install -d $(DESTDIR)$(PREFIX)/bin
	install vm $(DESTDIR)$(PREFIX)/bin/olvm
	@echo Installing libol.so...
	install libol.so $(DESTDIR)$(PREFIX)/lib/libol.so
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
	install -d $(DESTDIR)$(PREFIX)/include/ol
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

## actual 'building' part
debug: CFLAGS += $(CFLAGS_DEBUG)
debug: vm repl ol olvm

release: CFLAGS += $(CFLAGS_RELEASE)
release: vm repl ol olvm libol.so

slim: CFLAGS += -DHAS_SOCKETS=0 -DHAS_DLOPEN=0 -DHAS_SANDBOX=0
slim: release

ffi: CFLAGS += $(CFLAGS_DEBUG)
ffi: src/olvm.c extensions/ffi.c tests/ffi.c
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

# emscripten version 1.37.40+
ol.wasm: src/olvm.c tmp/repl.c
	emcc src/olvm.c \
	     tmp/repl.c -DREPL=repl \
	     -O3 -o ol.html \
	         -Iincludes \
	   -DHAS_DLOPEN=0 -DHAS_SOCKETS=0\
	   -s ASYNCIFY \
	   -s ASSERTIONS=0 \
	   -s ALLOW_MEMORY_GROWTH=1 \
	   -s FORCE_FILESYSTEM=0 \
	   -s WASM=1 && \
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

libraries/owl/unicode-char-folds.scm:
	echo "(define char-folds '(" >libraries/owl/unicode/char-folds.scm
	curl https://www.unicode.org/Public/14.0.0/ucd/CaseFolding.txt |\
	   grep "[0-9A-F]* [SFC]; " |\
	   sed -re 's/ #.*//' -e 's/( [SFC])?;//g' -e 's/^/ /' -e 's/ / #x/g' -e 's/ /(/' -e 's/$$/)/' |\
	   tr "[A-F]" "[a-f]" >> libraries/owl/unicode/char-folds.scm
	echo '))' >>libraries/owl/unicode/char-folds.scm

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
