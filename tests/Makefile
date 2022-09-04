.PHONY: check tests

.PHONY: testing-binaries
.PHONY: testing-embed testing-internal testing-ffi

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

FAILED := $(shell mktemp -u /tmp/failed.XXXXXXXXX)

MINGW32Q := $(shell which i686-w64-mingw32-gcc 1>/dev/null 2>/dev/null && echo 1)
MINGW64Q := $(shell which x86_64-w64-mingw32-gcc 1>/dev/null 2>/dev/null && echo 1)
WINEQ    := $(shell which wine 1>/dev/null 2>/dev/null && echo 1)

ok:="\033[1;32mok\033[0m"
failed:="\033[1;31mfailed\033[0m"

# GNU/Linux (base development system) specific defines:
ifeq ($(UNAME),Linux)
HAS_32CDEFS   ?= $(call exists,-m32,sys/cdefs.h,exit)
HAS_64CDEFS   ?= $(call exists,-m64,sys/cdefs.h,exit)
endif

ifeq ($(HAS_64CDEFS),1)
vm64 = printf "64 " && ./vm64 repl <$$F | diff - $$F.ok
else
vm64 = true
endif

ifeq ($(HAS_32CDEFS),1)
vm32 = printf "32 " && ./vm32 repl <$$F | diff - $$F.ok
else
vm32 = true
endif


vm32: src/olvm.c includes/ol/vm.h
	$(CC) src/olvm.c -o $@ \
	   $(L) -m32 -DOLVM_FFI=0 \
	   $(CFLAGS)
	@echo $(ok)
vm64: src/olvm.c includes/ol/vm.h
	$(CC) src/olvm.c -o $@ \
	   $(L) -m64 -DOLVM_FFI=0 \
	   $(CFLAGS)
	@echo $(ok)

# -------------
# win
define winbuild
	$(CC) src/olvm.c -o $1 \
	   -DOLVM_FFI=1 -Iincludes/win32 -Iincludes \
	   extensions/ffi.c \
	   -std=gnu99 -fno-exceptions -lws2_32 \
	   -DHAS_DLOPEN=1 -DHAS_SOCKES=1 $2
endef

ifeq ($(MINGW32Q),1)
tmp/vm32d.exe: CC=i686-w64-mingw32-gcc
tmp/vm32d.exe: src/olvm.c
	$(call winbuild,$@,$(CFLAGS_DEBUG))
tmp/vm32r.exe: CC=i686-w64-mingw32-gcc
tmp/vm32r.exe: src/olvm.c
	$(call winbuild,$@,$(CFLAGS_RELEASE))
endif
ifeq ($(MINGW64Q),1)
tmp/vm64d.exe: CC=x86_64-w64-mingw32-gcc
tmp/vm64d.exe: src/olvm.c
	$(call winbuild,$@,$(CFLAGS_DEBUG))
tmp/vm64r.exe: CC=x86_64-w64-mingw32-gcc
tmp/vm64r.exe: src/olvm.c
	$(call winbuild,$@,$(CFLAGS_RELEASE))
endif



test32: $(wildcard tests/*.scm)
	@echo "-- test32 ----------"
	@rm -f $(FAILED)
	@$(CC) src/olvm.c $(CFLAGS) tests/vm.c -Iincludes -DOLVM_NOMAIN -o vm32d $(L) -m32
	@./vm32d
	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iincludes \
	   -DOLVM_FFI=1 -Iincludes extensions/ffi.c -o ffi32 $(L) -m32
	@for F in $^ ;do \
	   printf "Testing $$F ... " ;\
	   if OL_HOME=`pwd`/libraries ./ffi32 repl $$F >/dev/null; then\
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
	@$(CC) src/olvm.c $(CFLAGS) tests/vm.c -Iincludes -DOLVM_NOMAIN -o vm64d $(L) -m64
	@./vm64d
	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iincludes \
	   -DOLVM_FFI=1 -Iincludes extensions/ffi.c -o ffi64 $(L) -m64
	@for F in $^ ;do \
	   printf "Testing $$F ... " ;\
	   if OL_HOME=`pwd`/libraries ./ffi64 repl $$F >/dev/null; then\
	      echo "Ok." ;\
	   else \
	      echo "\033[0;31mFailed!\033[0m" ;\
	      touch $(FAILED) ;\
	   fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi

test: test64
	@echo "passed!"

# -=( ffi )=----------------------------------------
test-ffi:
	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iincludes \
	   -DOLVM_FFI=1 -Iincludes extensions/ffi.c -o $(ffi)$(affix)$(bits) $(L) \
	   -fsigned-char # for ffi tests we should use char as signed by default
	@printf "Testing $(ffi)$(affix)$(bits) ... "
	@if $(ffi)$(affix)$(bits) repl <tests/ffi.scm | diff - tests/ffi.scm.ok >/dev/null; then\
	   echo $(ok);\
	else \
	   echo $(failed) ;\
	   touch $(FAILED);\
	fi
ffi=tmp/ffi


test-ffi-debug: CFLAGS += $(CFLAGS_DEBUG)
test-ffi-debug: affix=d
test-ffi-debug: test-ffi
test-ffi-release: CFLAGS += $(CFLAGS_RELEASE)
test-ffi-release: affix=r
test-ffi-release: test-ffi

# special case, maximal testings under main development platform
ifeq ($(UNAME),Linux)
test-ffi-debug-32: CFLAGS += -m32
test-ffi-debug-32: bits=32
test-ffi-debug-32: test-ffi-debug
test-ffi-debug-64: CFLAGS += -m64
test-ffi-debug-64: bits=64
test-ffi-debug-64: test-ffi-debug
test-ffi-release-32: CFLAGS += -m32
test-ffi-release-32: bits=32
test-ffi-release-32: test-ffi-release
test-ffi-release-64: CFLAGS += -m64
test-ffi-release-64: bits=64
test-ffi-release-64: test-ffi-release
endif

# -=( vm )=----------------------------------------
test-vmi:
	$(CC) src/olvm.c $(CFLAGS) tests/vm.c -Iincludes -DOLVM_NOMAIN -o $(vmi)$(affix)$(bits) $(L)
	   @echo "$(vmi)$(affix)$(bits):"
	   @$(vmi)$(affix)$(bits)
vmi=tmp/vmi

test-vmi-debug: CFLAGS += $(CFLAGS_DEBUG)
test-vmi-debug: affix=d
test-vmi-debug: test-vmi
test-vmi-release: CFLAGS += $(CFLAGS_RELEASE)
test-vmi-release: affix=r
test-vmi-release: test-vmi

# special case, maximal testings under main development platform
ifeq ($(UNAME),Linux)
test-vmi-debug-32: CFLAGS += -m32
test-vmi-debug-32: bits=32
test-vmi-debug-32: test-vmi-debug
test-vmi-debug-64: CFLAGS += -m64
test-vmi-debug-64: bits=64
test-vmi-debug-64: test-vmi-debug
test-vmi-release-32: CFLAGS += -m32
test-vmi-release-32: bits=32
test-vmi-release-32: test-vmi-release
test-vmi-release-64: CFLAGS += -m64
test-vmi-release-64: bits=64
test-vmi-release-64: test-vmi-release
endif

vm-debug:
	$(CC) src/olvm.c $(CFLAGS) \
	-DOLVM_FFI=1 -Iincludes extensions/ffi.c -o $(vm)d$(bits) $(L) $(CFLAGS_DEBUG)
vm-release:
	$(CC) src/olvm.c $(CFLAGS) \
	-DOLVM_FFI=1 -Iincludes extensions/ffi.c -o $(vm)r$(bits) $(L) $(CFLAGS_RELEASE)
vm=tmp/vm

# special case, maximal testings under main development platform
ifeq ($(UNAME),Linux)
vm-debug-32: CFLAGS += -m32
vm-debug-32: bits=32
vm-debug-32: vm-debug
vm-debug-64: CFLAGS += -m64
vm-debug-64: bits=64
vm-debug-64: vm-debug
vm-release-32: CFLAGS += -m32
vm-release-32: bits=32
vm-release-32: vm-release
vm-release-64: CFLAGS += -m64
vm-release-64: bits=64
vm-release-64: vm-release
endif

# -- binaries ---------------------
testing-binaries:
	@echo "Building test binaries:"
ifeq ($(UNAME),Linux) # main development platform, try to do a maximal testings
ifeq ($(HAS_32CDEFS),1)
	@printf "    linux 32-bit debug and release versions... "
	@$(MAKE) -s vm-debug-32
	@$(MAKE) -s vm-release-32
	@echo $(ok)
endif
ifeq ($(HAS_64CDEFS),1)
	@printf "    linux 64-bit debug and release versions... "
	@$(MAKE) -s vm-debug-64
	@$(MAKE) -s vm-release-64
	@echo $(ok)
endif
ifeq ($(MINGW32Q),1)
	@printf "    mingw 32-bit debug and release versions... "
	@$(MAKE) -s $(vm)32d.exe
	@$(MAKE) -s $(vm)32r.exe
	@echo $(ok)
endif
ifeq ($(MINGW64Q),1)
	@printf "    mingw 64-bit debug and release versions... "
	@$(MAKE) -s $(vm)64d.exe
	@$(MAKE) -s $(vm)64r.exe
	@echo $(ok)
endif
	@echo "built."
else
	@printf "Building test binaries (debug and release versions both)... "
	@$(MAKE) -s vm-debug
	@$(MAKE) -s vm-release
	@echo "Built."
endif

# -- embed --------------------------------------------------------
testing-embed: tests/embed.c src/olvm.c includes/ol/ol.h tmp/repl.c
	@echo "embed testing"
	@echo "----------------------------------------"
	@printf "tests/embed.c ..."
ifeq ($(UNAME),Linux) # main development platform, try to do a maximal testings
ifeq ($(HAS_32CDEFS),1)
	@printf ", debug-32: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embed32d \
	   -Iincludes -lm $(CFLAGS_DEBUG) -m32 -Wno-unused-function
	@if tmp/embed32d >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release-32: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embed32r \
	   -Iincludes -lm $(CFLAGS_RELEASE) -m32 -Wno-unused-function
	@if tmp/embed32r >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
endif
ifeq ($(HAS_64CDEFS),1)
	@printf ", debug-64: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embed64d \
	   -Iincludes -lm $(CFLAGS_DEBUG) -m64 -Wno-unused-function
	@if tmp/embed64d >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release-64: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embed64r \
	   -Iincludes -lm $(CFLAGS_RELEASE) -m64 -Wno-unused-function
	@if tmp/embed64r >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
endif
else
	@printf ", debug: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embedd \
	   -Iincludes -lm $(CFLAGS_DEBUG) -Wno-unused-function
	@if tmp/embedd >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embedr \
	   -Iincludes -lm $(CFLAGS_RELEASE) -Wno-unused-function
	@if tmp/embedr >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
endif
	@echo "."


# -- internal --------------------------------------
testing-internal:
	@echo "Internal vm testing"
	@echo "---------------------------------------"
ifeq ($(UNAME),Linux) # special case, try to do maximal testings under main development platform
ifeq ($(HAS_32CDEFS),1)
	@$(MAKE) -s test-vmi-debug-32
	@$(MAKE) -s test-vmi-release-32
endif
ifeq ($(HAS_64CDEFS),1)
	@$(MAKE) -s test-vmi-debug-64
	@$(MAKE) -s test-vmi-release-64
endif
else
	@$(MAKE) -s test-vmi-debug
	@$(MAKE) -s test-vmi-release
endif
	@echo " "

testing-ffi:
	@echo "ffi testing"
	@echo "----------------------------------------"
ifeq ($(UNAME),Linux) # special case, try to do maximal testings under main development platform
ifeq ($(HAS_32CDEFS),1)
	@$(MAKE) -s test-ffi-debug-32
	@$(MAKE) -s test-ffi-release-32
endif
ifeq ($(HAS_64CDEFS),1)
	@$(MAKE) -s test-ffi-debug-64
	@$(MAKE) -s test-ffi-release-64
endif
else
	@$(MAKE) -s test-ffi-debug
	@$(MAKE) -s test-ffi-release
endif
	@echo " "


# -- scm -------
define scmtestok
	@if ([ -f $1 ]); then\
		printf ", $2: ";\
		if ([ -f $^.in ] && $3 $1 repl --home=libraries $^ <$^.in 2>&1 || $3 $1 repl --home=libraries $^ 2>&1) | diff --strip-trailing-cr - $^.ok >/dev/null; then\
			printf $(ok) ;\
		else \
			printf $(failed);\
			touch $(FAILED);\
		fi;\
	fi
endef

%.scm.ok: %.scm
	@printf "Testing $^ ..."
	$(call scmtestok,$(vm)d,debug)
	$(call scmtestok,$(vm)d32,debug-32)
	$(call scmtestok,$(vm)d64,debug-64)
	$(call scmtestok,$(vm)r,release)
	$(call scmtestok,$(vm)r32,release-32)
	$(call scmtestok,$(vm)r64,release-64)
	$(call scmtestok,$(vm)32d.exe,win-debug-32,wine)
	$(call scmtestok,$(vm)32r.exe,win-release-32,wine)
	$(call scmtestok,$(vm)64d.exe,win-debug-64,wine)
	$(call scmtestok,$(vm)64r.exe,win-release-64,wine)
	@printf ".\n"

# -- bin -------
define bintestok
	@if ([ -f $1 ]); then\
		printf ", $2: ";\
		if ([ -f $^.in ] && $3 $1 $^ --home=libraries <$^.in || $3 $1 $^ --home=libraries) | diff --strip-trailing-cr - $^.ok >/dev/null; then\
			printf $(ok) ;\
		else \
			printf $(failed);\
			touch $(FAILED);\
		fi;\
	fi
endef

%.bin.ok: %.bin
	@printf "Testing $^ ..."
	$(call bintestok,$(vm)d,debug)
	$(call bintestok,$(vm)d32,debug-32)
	$(call bintestok,$(vm)d64,debug-64)
	$(call bintestok,$(vm)r,release)
	$(call bintestok,$(vm)r32,release-32)
	$(call bintestok,$(vm)r64,release-64)
	$(call bintestok,$(vm)32d.exe,win-debug-32,wine)
	$(call bintestok,$(vm)32r.exe,win-release-32,wine)
	$(call bintestok,$(vm)64d.exe,win-debug-64,wine)
	$(call bintestok,$(vm)64r.exe,win-release-64,wine)
	@printf ".\n"


# -------------------------------------------------
# -=( tests )=-------------------------------------
check: tests
tests: testing-binaries
	@rm -f $(FAILED)
	$(MAKE) -s testing-internal
	$(MAKE) -s testing-ffi
	$(MAKE) -s testing-embed
	@echo " "
	@echo "common tests"
	@echo "---------------------------------------"
	@for F in $(filter-out tests/ffi.scm, $(wildcard tests/*.scm)) $(wildcard tests/*.bin) ;do \
	   $(MAKE) -s -B $$F.ok ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "\033[1;32mpassed!\033[0m"

endif