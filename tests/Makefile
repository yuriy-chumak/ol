ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

.PHONY: check tests

.PHONY: testing-binaries
.PHONY: testing-embed testing-internal testing-ffi
.PHONY: regression-tests

.SILENT: regression-tests

FAILED := $(shell mktemp -u /tmp/failed.XXXXXXXXX)

# win32 tests:
HAS_MINGW32 ?= $(shell which i686-w64-mingw32-gcc 1>/dev/null 2>/dev/null && echo 1)
HAS_MINGW64 ?= $(shell which x86_64-w64-mingw32-gcc 1>/dev/null 2>/dev/null && echo 1)
WINEQ    := $(shell which wine 1>/dev/null 2>/dev/null && echo 1)

ok:="\033[1;32m ok \033[0m"
fail:="\033[1;31mfail\033[0m"

MACHINE ?= $(shell uname -m)

# special case, test 64- and 32-bit both
# (maximal testings under main development platform)
ifeq ($(UNAME)-$(MACHINE),Linux-x86_64)
HAS_32CDEFS  ?= $(call exists,-m32,sys/cdefs.h,exit)
HAS_64CDEFS  ?= $(call exists,-m64,sys/cdefs.h,exit)
ifeq ($(HAS_32CDEFS),1)
DEV_MACHINE  ?= 1
endif
ifeq ($(HAS_64CDEFS),1)
DEV_MACHINE  ?= 1
endif
else
DEV_MACHINE  ?= 0
HAS_32CDEFS  ?= 0
HAS_64CDEFS  ?= 0
endif

# -------------
# win
define winbuild
	$(CC) src/olvm.c -o $1 \
	   -DHAVE_DLOPEN=1 -DHAS_SOCKES=1 -DOLVM_FFI=1 \
	   -Iincludes/win32 -Iincludes \
	   extensions/ffi.c \
	   -std=gnu99 -fno-exceptions -lws2_32 \
	   $2
endef

ifeq ($(HAS_MINGW32),1)
tmp/vm32d.exe: CC=i686-w64-mingw32-gcc
tmp/vm32d.exe: src/olvm.c
	$(call winbuild,$@,$(CFLAGS_DEBUG))
tmp/vm32r.exe: CC=i686-w64-mingw32-gcc
tmp/vm32r.exe: src/olvm.c
	$(call winbuild,$@,$(CFLAGS_RELEASE))
endif
ifeq ($(HAS_MINGW64),1)
tmp/vm64d.exe: CC=x86_64-w64-mingw32-gcc
tmp/vm64d.exe: src/olvm.c
	$(call winbuild,$@,$(CFLAGS_DEBUG))
tmp/vm64r.exe: CC=x86_64-w64-mingw32-gcc
tmp/vm64r.exe: src/olvm.c
	$(call winbuild,$@,$(CFLAGS_RELEASE))
endif


# # -=( test )=----------------------------------------
# test32: $(wildcard tests/*.scm)
# 	@echo "-- test32 ----------"
# 	@rm -f $(FAILED)
# 	@$(CC) src/olvm.c $(CFLAGS) tests/vm.c -Iincludes -DOLVM_NOMAIN -o vm32d $(L) -m32
# 	@./vm32d
# 	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iincludes \
# 	   -DOLVM_FFI=1 -Iincludes extensions/ffi.c -o ffi32 $(L) -m32
# 	@for F in $^ ;do \
# 	   printf "Testing $$F ... " ;\
# 	   if OL_HOME=`pwd`/libraries ./ffi32 repl $$F >/dev/null; then\
# 	      echo "Ok." ;\
# 	   else \
# 	      echo "\033[0;31mFailed!\033[0m" ;\
# 	      touch $(FAILED) ;\
# 	   fi ;\
# 	done
# 	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi

# test64: $(wildcard tests/*.scm)
# 	@echo "-- test64 ----------"
# 	@rm -f $(FAILED)
# 	@$(CC) src/olvm.c $(CFLAGS) tests/vm.c -Iincludes -DOLVM_NOMAIN -o vm64d $(L) -m64
# 	@./vm64d
# 	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iincludes \
# 	   -DOLVM_FFI=1 -Iincludes extensions/ffi.c -o ffi64 $(L) -m64
# 	@for F in $^ ;do \
# 	   printf "Testing $$F ... " ;\
# 	   if OL_HOME=`pwd`/libraries ./ffi64 repl $$F >/dev/null; then\
# 	      echo "Ok." ;\
# 	   else \
# 	      echo "\033[0;31mFailed!\033[0m" ;\
# 	      touch $(FAILED) ;\
# 	   fi ;\
# 	done
# 	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi

# test: test64
# 	@echo "passed!"

# -=( ffi )=----------------------------------------
test-ffi:
	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iincludes \
	   -DOLVM_FFI=1 -Iincludes extensions/ffi.c -o $(ffi)$(affix)$(bits) $(L) \
	   -fsigned-char # for ffi tests we should use char as signed by default
	@printf "Testing $(ffi)$(affix)$(bits) ... "
	@if $(ffi)$(affix)$(bits) repl <tests/ffi.scm | diff - tests/ffi.scm.ok >/dev/null; then\
	   echo $(ok);\
	else \
	   echo $(fail) ;\
	   touch $(FAILED);\
	fi
ffi=tmp/ffi


test-ffi-debug: CFLAGS += $(CFLAGS_DEBUG)
test-ffi-debug: affix=d
test-ffi-debug: test-ffi
test-ffi-release: CFLAGS += $(CFLAGS_RELEASE)
test-ffi-release: affix=r
test-ffi-release: test-ffi

ifeq ($(HAS_32CDEFS),1) # x86_64
test-ffi-debug-32: CFLAGS += -m32
test-ffi-debug-32: bits=32
test-ffi-debug-32: test-ffi-debug
test-ffi-release-32: CFLAGS += -m32
test-ffi-release-32: bits=32
test-ffi-release-32: test-ffi-release
endif
ifeq ($(HAS_64CDEFS),1) # x86_64
test-ffi-debug-64: CFLAGS += -m64
test-ffi-debug-64: bits=64
test-ffi-debug-64: test-ffi-debug
test-ffi-release-64: CFLAGS += -m64
test-ffi-release-64: bits=64
test-ffi-release-64: test-ffi-release
endif

# -=( vmi )=---------------------------------------
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

ifeq ($(HAS_32CDEFS),1) # x86_64
test-vmi-debug-32: CFLAGS += -m32
test-vmi-debug-32: bits=32
test-vmi-debug-32: test-vmi-debug
test-vmi-release-32: CFLAGS += -m32
test-vmi-release-32: bits=32
test-vmi-release-32: test-vmi-release
endif
ifeq ($(HAS_64CDEFS),1) # x86_64
test-vmi-debug-64: CFLAGS += -m64
test-vmi-debug-64: bits=64
test-vmi-debug-64: test-vmi-debug
test-vmi-release-64: CFLAGS += -m64
test-vmi-release-64: bits=64
test-vmi-release-64: test-vmi-release
endif


# -=( vm )=----------------------------------------
# binaries
vm-debug:
	$(CC) src/olvm.c $(CFLAGS) \
	-DOLVM_FFI=1 -Iincludes extensions/ffi.c -o $(vm)d$(bits) $(L) $(CFLAGS_DEBUG)
vm-release:
	$(CC) src/olvm.c $(CFLAGS) \
	-DOLVM_FFI=1 -Iincludes extensions/ffi.c -o $(vm)r$(bits) $(L) $(CFLAGS_RELEASE)
vm=tmp/vm

# vm-debug: CFLAGS += $(CFLAGS_DEBUG)
# vm-debug: affix=d
# vm-debug: vm-debug
# vm-release: CFLAGS += $(CFLAGS_RELEASE)
# vm-release: affix=r
# vm-release: vm-release

ifeq ($(HAS_32CDEFS),1) # x86_64
vm-debug-32: CFLAGS += -m32
vm-debug-32: bits=32
vm-debug-32: vm-debug
vm-release-32: CFLAGS += -m32
vm-release-32: bits=32
vm-release-32: vm-release
endif
ifeq ($(HAS_64CDEFS),1) # x86_64
vm-debug-64: CFLAGS += -m64
vm-debug-64: bits=64
vm-debug-64: vm-debug
vm-release-64: CFLAGS += -m64
vm-release-64: bits=64
vm-release-64: vm-release
endif

# -- binaries ---------------------
testing-binaries: includes/ol/vm.h
	@printf "Building test binaries:"
ifeq ($(DEV_MACHINE),1) # main development platform, special case
	@printf "\n"
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
ifeq ($(HAS_MINGW32),1)
	@printf "    mingw 32-bit debug and release versions... "
	@$(MAKE) -s $(vm)32d.exe
	@$(MAKE) -s $(vm)32r.exe
	@echo $(ok)
endif
ifeq ($(HAS_MINGW64),1)
	@printf "    mingw 64-bit debug and release versions... "
	@$(MAKE) -s $(vm)64d.exe
	@$(MAKE) -s $(vm)64r.exe
	@echo $(ok)
endif
else # regular case
	@printf " (debug and release versions both)... "
	@$(MAKE) -s vm-debug
	@$(MAKE) -s vm-release
endif
	@echo "built."
	@printf "\n"

# -- embed --------------------------------------------------------
testing-embed: tests/embed.c src/olvm.c includes/ol/ol.h tmp/repl.c
	@echo "embed testing"
	@echo "----------------------------------------"
	@printf "tests/embed.c ..."
ifeq ($(DEV_MACHINE),1) # main development platform, special case
ifeq ($(HAS_32CDEFS),1)
	@printf ", debug-32: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embed32d \
	   -Iincludes -lm $(CFLAGS_DEBUG) -m32 -Wno-unused-function
	@if tmp/embed32d >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(fail) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release-32: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embed32r \
	   -Iincludes -lm $(CFLAGS_RELEASE) -m32 -Wno-unused-function
	@if tmp/embed32r >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(fail) ;\
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
	   printf $(fail) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release-64: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embed64r \
	   -Iincludes -lm $(CFLAGS_RELEASE) -m64 -Wno-unused-function
	@if tmp/embed64r >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(fail) ;\
	   touch $(FAILED) ;\
	fi
endif
else # regular case
	@printf ", debug: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embedd \
	   -Iincludes -lm $(CFLAGS_DEBUG) -Wno-unused-function
	@if tmp/embedd >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(fail) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DOLVM_NOMAIN \
	   -DOLVM_FFI=1 extensions/ffi.c -o tmp/embedr \
	   -Iincludes -lm $(CFLAGS_RELEASE) -Wno-unused-function
	@if tmp/embedr >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(fail) ;\
	   touch $(FAILED) ;\
	fi
endif
	@echo "."


# -- internal -------------------------------------
testing-internal:
	@echo "Internal vm testing"
	@echo "----------------------------------------"
ifeq ($(DEV_MACHINE),1) # main development platform, special case
ifeq ($(HAS_32CDEFS),1)
	@$(MAKE) -s test-vmi-debug-32
	@$(MAKE) -s test-vmi-release-32
endif
ifeq ($(HAS_64CDEFS),1)
	@$(MAKE) -s test-vmi-debug-64
	@$(MAKE) -s test-vmi-release-64
endif
else # regular case
	@$(MAKE) -s test-vmi-debug
	@$(MAKE) -s test-vmi-release
endif
	@echo " "

# -- ffi ------------------------------------------
testing-ffi:
	@echo "ffi testing"
	@echo "----------------------------------------"
ifeq ($(DEV_MACHINE),1) # main development platform, special case
ifeq ($(HAS_32CDEFS),1)
	@$(MAKE) -s test-ffi-debug-32
	@$(MAKE) -s test-ffi-release-32
endif
ifeq ($(HAS_64CDEFS),1)
	@$(MAKE) -s test-ffi-debug-64
	@$(MAKE) -s test-ffi-release-64
endif
else # regular case
	@$(MAKE) -s test-ffi-debug
	@$(MAKE) -s test-ffi-release
endif
	@echo " "

# -- scm ------------------------------------------
define scmtestok
	@if ([ -f $1 ]); then\
		if ([ -f $^.in ] && $3 $1 repl --home=libraries $^ <$^.in 2>&1 || $3 $1 repl --home=libraries $^ 2>&1) | diff $4 - $^.ok >/dev/null; then\
			printf \|$(ok) ;\
		else \
			printf \|$(fail);\
			touch $(FAILED);\
		fi;\
	fi
endef

%.scm.ok: %.scm
ifeq ($(DEV_MACHINE),1) # main development platform, special case
ifeq ($(HAS_32CDEFS),1)
	$(call scmtestok,$(vm)d32,debug-32)
	$(call scmtestok,$(vm)r32,release-32)
endif
ifeq ($(HAS_64CDEFS),1)
	$(call scmtestok,$(vm)d64,debug-64)
	$(call scmtestok,$(vm)r64,release-64)
endif
ifeq ($(HAS_MINGW32),1)
	$(call scmtestok,$(vm)32d.exe,win-debug-32,wine,--strip-trailing-cr)
	$(call scmtestok,$(vm)32r.exe,win-release-32,wine,--strip-trailing-cr)
endif
ifeq ($(HAS_MINGW64),1)
	$(call scmtestok,$(vm)64d.exe,win-debug-64,wine,--strip-trailing-cr)
	$(call scmtestok,$(vm)64r.exe,win-release-64,wine,--strip-trailing-cr)
endif
else # default case
	$(call scmtestok,$(vm)d,debug)
	$(call scmtestok,$(vm)r,release)
endif
	@printf "|\n"

# -- bin ------------------------------------------
define bintestok
	@if ([ -f $1 ]); then\
		if ([ -f $^.in ] && $3 $1 $^ --home=libraries <$^.in || $3 $1 $^ --home=libraries) | diff $4 - $^.ok >/dev/null; then\
			printf \|$(ok) ;\
		else \
			printf \|$(fail);\
			touch $(FAILED);\
		fi;\
	fi
endef

%.bin.ok: %.bin
ifeq ($(DEV_MACHINE),1) # main development platform, special case
ifeq ($(HAS_32CDEFS),1)
	$(call bintestok,$(vm)d32,debug-32)
	$(call bintestok,$(vm)r32,release-32)
endif
ifeq ($(HAS_64CDEFS),1)
	$(call bintestok,$(vm)d64,debug-64)
	$(call bintestok,$(vm)r64,release-64)
endif
ifeq ($(HAS_MINGW32),1)
	$(call bintestok,$(vm)32d.exe,win-debug-32,wine,--strip-trailing-cr)
	$(call bintestok,$(vm)32r.exe,win-release-32,wine,--strip-trailing-cr)
endif
ifeq ($(HAS_MINGW64),1)
	$(call bintestok,$(vm)64d.exe,win-debug-64,wine,--strip-trailing-cr)
	$(call bintestok,$(vm)64r.exe,win-release-64,wine,--strip-trailing-cr)
endif
else # default case
	$(call bintestok,$(vm)d,debug)
	$(call bintestok,$(vm)r,release)
endif
	@printf "|\n"

# -------------------------------------------------
# -=( tests )=-------------------------------------
check: tests
tests: testing-binaries
	@rm -f $(FAILED)
	$(MAKE) -s testing-internal
	$(MAKE) -s testing-ffi
	$(MAKE) -s testing-embed
	$(MAKE) -s regression-tests

define table-header
	set -e
	: header line 1
	printf "%-$1s " "  Test"
	if [ "$(DEV_MACHINE)" = "1" ]; \
	then \
	: main development platform, special case;\
		case "`expr $(HAS_32CDEFS) + $(HAS_64CDEFS)`" in \
			1) printf "| Linux   ";;\
			2) printf "| Linux             ";;\
		esac ;\
		case "`expr $(HAS_MINGW32) + $(HAS_MINGW64)`" in \
			1) printf "| Windows ";;\
			2) printf "| Windows           ";;\
		esac ;\
	else \
	: regular platform ;\
		printf "| %-7s " `uname` ;\
	fi
	printf "|\n"

	: header line 1
	printf "%-$1s " `uname`
	if [ "$(DEV_MACHINE)" = "1" ]; \
	then \
	: main development platform, special case;\
	 	if [ $(HAS_32CDEFS) = 1 ]; then printf "|32-d|32-r"; fi ;\
	 	if [ $(HAS_64CDEFS) = 1 ]; then printf "|64-d|64-r"; fi ;\
	 	if [ $(HAS_MINGW32) = 1 ]; then printf "|32-d|32-r"; fi ;\
	 	if [ $(HAS_MINGW64) = 1 ]; then printf "|64-d|64-r"; fi ;\
	else \
	: regular platform ;\
		printf "|dbg.|rel." ;\
	fi
	printf "|\n"
endef

regression-tests: $(filter-out tests/ffi.scm, $(wildcard tests/*.scm)) $(wildcard tests/*.bin)
	echo " "
	echo "regression tests"
	echo "----------------"
	$(eval F1LEN=$(shell for F in $^; do echo $${#F}; done |sort -n| tail -1))
	$(call table-header, $(F1LEN))
	for F in $^; do \
	   printf "%-$(F1LEN)s " "$$F";\
	   $(MAKE) -s -B $$F.ok ;\
	done
	if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	echo "\033[1;32mpassed!\033[0m"

endif
