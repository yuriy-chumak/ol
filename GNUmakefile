export PATH := .:$(PATH)
$(shell mkdir -p config)
export OL_HOME=libraries

# default target
all: release

# detect external features
-include configure.mk

.PHONY: all release debug check slim config recompile install uninstall clean android
.PHONY: describe

describe: all
	./vm --version
	./ol --version
	echo "(print (syscall 63))"|./vm repl

# default toolchain(s)
CC ?= gcc
LD ?= ld

# win32 cross-compile
MGCC32 ?= i686-w64-mingw32-gcc
MGCC64?=x86_64-w64-mingw32-gcc

# ansi colors
red=\033[1;31m
green=\033[1;32m
done=\033[0m

# check submodules
# ----------------
ifeq ($(shell ls -A libraries/OpenGL),)
    $(warning $(red)Submodules not loaded. Run 'git submodule update --init --recursive' once.$(done))
endif

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

# note: use 2>/dev/null in "shell command" to avoid
#       make call optimization and really run shell.
tmp/repl.c: olvm repl
# vim
ifneq ($(shell command -v xxd 2>/dev/null),)
	xxd --include repl >tmp/repl.c
else
# coreutils
ifneq ($(shell command -v od 2>/dev/null),)
	od -An -vtx1 repl| tr -d '\n'| sed \
	   -e 's/^ /0x/' -e 's/ /,0x/g' \
	   -e 's/^/unsigned char repl[] = {/' \
	   -e 's/$$/};/'> $@
else
# olvm
	echo '(display "unsigned char repl[] = {") (lfor-each (lambda (x) (display x) (display ",")) (file->bytestream "repl")) (display "0};")'| ./olvm repl> tmp/repl.c
endif
endif

doc/olvm.md: src/olvm.c extensions/ffi.c
	cat src/olvm.c extensions/ffi.c| tools/makedoc >doc/olvm.md

# compiler flags
# ----------------------------------
## os independent flags

CFLAGS += -std=gnu99 -fno-exceptions
CFLAGS += -DHAVE_SOCKETS=$(if $(HAVE_SOCKETS),$(HAVE_SOCKETS),0)
CFLAGS += -DHAVE_DLOPEN=$(if $(HAVE_DLOPEN),$(HAVE_DLOPEN),0)
CFLAGS += -DHAVE_SECCOMP=$(if $(HAVE_SECCOMP),$(HAVE_SECCOMP),0)

ifneq ($(HAVE_MEMFD_CREATE),)
CFLAGS += -DHAVE_MEMFD_CREATE=$(HAVE_MEMFD_CREATE)
endif
ifneq ($(HAVE_SENDFILE),)
CFLAGS += -DHAVE_SENDFILE=$(HAVE_SENDFILE)
endif

# builtin "sin", "cos", "sqrt", etc. functions support
# can be disabled using "-DOLVM_BUILTIN_FMATH=0"
ifneq ($(OLVM_BUILTIN_FMATH),0)
   CFLAGS += -lm
#  CFLAGS += -ffast-math -mfpmath=387
else
   CFLAGS += -DOLVM_BUILTIN_FMATH=0
endif

# 32-bit linux fseek and ftell needs:
CFLAGS += -D_FILE_OFFSET_BITS=64
# -D_LARGEFILE_SOURCE

# ----------------------------------
## debug/release flags
CFLAGS_CHECK   := -O0 -g3 -Wall -DWARN_ALL
CFLAGS_DEBUG   := -O0 -g3 -Wall
CFLAGS_DEBUG   += -DCAR_CHECK=1 -DCDR_CHECK=1
CFLAGS_RELEASE := $(if $(RPM_OPT_FLAGS), $(RPM_OPT_FLAGS), -O2 -DNDEBUG)
CFLAGS_RELEASE += -Wno-unused-result -g3

VERSION ?= $(shell echo `git describe --tags \`git rev-list --tags --max-count=1\``-`git rev-list HEAD --count`-`git log --pretty=format:'%h' -n 1`)

# ------------------------------------------------------
## os dependent flags

UNAME ?= $(shell uname -s)

# Linux
ifeq ($(UNAME),Linux)

ifeq ($(CC), tcc)
  L := $(if $(HAVE_DLOPEN), -ldl)
else
  L := $(if $(HAVE_DLOPEN), -ldl) \
       -Xlinker --export-dynamic
endif

# Debian i586 fix
ifeq ($(CC),gcc)
  CFLAGS += -I/usr/include/$(shell gcc -print-multiarch)
endif

endif #Linux

# BSD
ifeq ($(UNAME),FreeBSD)
  L := $(if $(HAVE_DLOPEN), -lc) \
       -Xlinker --export-dynamic

  LD := ld.bfd
endif
ifeq ($(UNAME),NetBSD)
  L := $(if $(HAVE_DLOPEN), -lc) \
       -Xlinker --export-dynamic
endif
ifeq ($(UNAME),OpenBSD)
  L := $(if $(HAVE_DLOPEN), -lc) \
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
	rm -f tmp/*
	rm -f includes/ol/vm.h

-include extras/setup.mk

# -----------------------------------------------
## builds

debug: CFLAGS += $(CFLAGS_DEBUG)
debug: vm ol olvm libol.so

release: CFLAGS += $(CFLAGS_RELEASE)
release: vm ol olvm libol.so

perf: CFLAGS += -O2 -g3 -DNDEBUG -Wall
perf: vm ol olvm libol.so


slim:
	HAVE_SOCKETS=0 HAVE_DLOPEN=0 HAVE_SANDBOX=1 \
	$(MAKE) -B release olvm

minimal: CFLAGS += -DOLVM_FFI=0 -DHAVE_SOCKETS=1 -DHAVE_DLOPEN=0 -DHAVE_SANDBOX=0
minimal: release

# ffi test build
ffi: CFLAGS += $(CFLAGS_DEBUG)
ffi: src/olvm.c extensions/ffi.c tests/ffi.c
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   tests/ffi.c \
	   $(CFLAGS) $(L)
	@echo Ok.
ffi32: CFLAGS += $(CFLAGS_DEBUG) -m32
ffi32: src/olvm.c extensions/ffi.c tests/ffi.c
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

vm.asm: CFLAGS += $(CFLAGS_RELEASE)
vm.asm:
	$(CC) src/olvm.c -o $@ \
	   -DHAVE_DLOPEN=0  -Iincludes \
	   $(CFLAGS) -DPREFIX=\"$(PREFIX)\" $(L) \
	   -S -fverbose-asm
	@echo Ok.

ol:
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   $(CFLAGS) -DPREFIX=\"$(PREFIX)\" $(L) \
	   tmp/repl.c -DREPL=repl
	@echo Ok.

olp: tmp/pvenv.tar
	CFLAGS="-DOLVM_TARVENV=1 -DOLVM_TARVFOLDERS=1" \
	   $(MAKE) -B release
	objcopy --add-section       .tar=tmp/pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	   ol $@

libol.so:
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   $(CFLAGS) -DPREFIX=\"$(PREFIX)\" $(L) \
	   tmp/repl.c -DREPL=repl \
	   -DOLVM_NOMAIN -shared -fPIC
	@echo Ok.

# real name of
olvm: vm
	cp $< $@
	strip $@
	#strip $@ -R .eh_frame
	#strip $@ -R .eh_frame_hdr

# selfexec feature test
selfexec: ol
	objcopy --add-section .lisp=selfexec.lisp \
	        --set-section-flags .lisp=noload,readonly $^ $@

# windows

# You can debug ol.exe using "winedbg --gdb ol.exe"
# require mingw-w64-i686-dev (+ gcc-mingw-w64-i686) or/and mingw-w64-x86-64-dev (+ gcc-mingw-w64-x86-64)
ol%.exe: MINGWCFLAGS += -std=gnu99 -fno-exceptions
ol%.exe: MINGWCFLAGS += $(CFLAGS_RELEASE)
ol%.exe: src/olvm.c extensions/ffi.c tmp/repl.c
	$(MGCC) \
	   $^ -o $@ \
	   -DREPL=repl \
	   -DHAVE_DLOPEN=1 -DHAS_SOCKES=1 -DOLVM_FFI=1 \
	   -DOLVM_TARVENV=1 \
	   -Iincludes/win32 -Iincludes \
	   $(MINGWCFLAGS) -lws2_32
	#wine tools/cv2pdb.exe $@

ol32.exe: MGCC:=$(MGCC32)
ol64.exe: MGCC:=$(MGCC64)

ol.exe: MINGWCFLAGS += -DOLVM_TARVENV=1 # enable TARVENV definitely
ol.exe: ol64.exe tmp/pvenv.tar # by default 64-bit exe
	cat $< >$@
	x86_64-w64-mingw32-strip $@
	cat tmp/pvenv.tar >>$@

# compiling the Ol language
recompile: boot.fasl
boot.fasl: vm repl src/*.scm lang/*.scm libraries/otus/*.scm libraries/owl/*.scm libraries/scheme/*.scm
	@vm repl --version="$(VERSION)" --home=.:libraries \
	   src/ol.scm
	@if diff boot.fasl repl>/dev/null;then\
	   echo '$(green)  `___`  $(done)' ;\
	   echo '$(green)  (o,o)  $(done)' ;\
	   echo '$(green)  \)  )  $(done)' ;\
	   echo '$(green)___"_"___$(done)' ;\
	   echo '$(green)Build Ok.$(done)' ;\
	else \
	   echo `stat -c%s repl` -\> `stat -c%s $@` ;\
	   cp -b $@ repl ;$(MAKE) $@ ;\
	fi

# compiling infix math notation
libraries/owl/math/infix.scm: tools/make-math-infix.scm vm
	./vm repl tools/make-math-infix.scm >$@

# additional targets (like packaging, tests, etc.)
MAKEFILE_MAIN=1
-include extras/wasm.mk

-include tests/Makefile
-include tests/rosettacode/Makefile
-include config/Makefile

# documentation samples check
check: check-reference
check-reference: release
check-reference: $(wildcard doc/reference/*.md)
	@echo "Testing reference samples:"
	@./ol tools/check-reference.lisp $(filter %.md,$^) && echo $(ok) || echo $(failed)

# win32 extensions
-include extensions/win32/Makefile

# -------------------------------------------------------------
# pvenv
define PVENV_ADD
	cat '{}'| $$olvm $$repl <(echo '(write (read))') >$$library;\
	tar -rf $$tar0 '$$library' \
	    --absolute-names \
	    --transform 's|.*|{}|' \
	    --owner=OL/2.6 \
	    --group=* 
endef
define PVENV_ADD_RAW
	tar -rf $$tar0 \
	    --absolute-names '{}' \
	    --owner=OL/2.6 \
	    --group=* 
endef

tmp/pvenv.tar: $(wildcard libraries/*/*.scm)\
               $(wildcard libraries/*/*/*.scm)\
               $(wildcard libraries/*/*/*/*.scm)\
               $(wildcard libraries/*/*.lisp)\
               $(wildcard libraries/*/*/*.lisp)\
               $(wildcard libraries/*/*/*/*.lisp)
	@$(MAKE) olvm
	tar -cf $@ -T /dev/null

	`# macOS wants the template to be at the end` ;\
	`# so use the same template under all os`     ;\
	export library=`mktemp /tmp/scm.XXXXXXXXX` ;\
	export olvm=${abspath ./olvm} ;\
	export repl=${abspath ./repl} ;\
	`# OpenGL tarfolder` ;\
	export tar0=${abspath ./tmp/OpenGL.tar} ;\
	tar -cf $$tar0 -T /dev/null ;\
	cd libraries/OpenGL ;\
	   find * -name "*.scm"  -exec bash -c "echo '{}'; $(PVENV_ADD)" \;;\
	cd ../.. ;\
	`# lib/gtk-3 tarfolder` ;\
	export tar0=${abspath ./tmp/gtk-3.tar} ;\
	tar -cf $$tar0 -T /dev/null ;\
	cd libraries/lib/gtk-3 ;\
	   find * -name "*.scm"  -exec bash -c "echo '{}'; $(PVENV_ADD)" \;;\
	cd ../.. ;\
	export tar0=${abspath $@} ;\
	cd libraries ;\
	   find . -name "*.scm"  -exec bash -c "echo '{}'; $(PVENV_ADD)" \;;\
	   find . -name "*.lisp" -exec bash -c "echo '{}'; $(PVENV_ADD_RAW)" \;;\
	   for name in "http/server"; do \
	      eval `echo $(PVENV_ADD_RAW) |sed "s|{}|$$name|g"` ;\
	   done ;\
	cd .. ;\
	rm -f $$library ;\
	`# Final integration of all TAR subfolders` \
	tar -vf $$tar0 --wildcards --delete './OpenGL/*' ;\
	tar -rf $$tar0 tmp/OpenGL.tar --transform 's|.*|./OpenGL/|' ;\
	tar -vf $$tar0 --wildcards --delete './lib/gtk-3/*' ;\
	tar -rf $$tar0 tmp/gtk-3.tar --transform 's|.*|./lib/gtk-3/|'
