.PHONY: tests testing-binaries

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

FAILED := $(shell mktemp -u)

# GNU/Linux (base development system) specific defines:
ifeq ($(UNAME),Linux)
HAS_32CDEFS   ?= $(call exists,-m32,<sys/cdefs.h>,exit)
HAS_64CDEFS   ?= $(call exists,-m64,<sys/cdefs.h>,exit)
endif

ifeq ($(HAS_64CDEFS),1)
vm64 = echo -n "64 " && ./vm64 repl <$$F | diff - $$F.ok
else
vm64 = true
endif

ifeq ($(HAS_32CDEFS),1)
vm32 = echo -n "32 " && ./vm32 repl <$$F | diff - $$F.ok
else
vm32 = true
endif


vm32: src/olvm.c include/olvm.h
	$(CC) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L) -m32 -DOLVM_FFI=0 \
	   $(CFLAGS)
	@echo Ok.
vm64: src/olvm.c include/olvm.h
	$(CC) src/olvm.c -DNAKED_VM -o $@ \
	   -Xlinker --export-dynamic $(L) -m64 -DOLVM_FFI=0 \
	   $(CFLAGS)
	@echo Ok.

test32: $(wildcard tests/*.scm)
	@echo "-- test32 ----------"
	@rm -f $(FAILED)
	@$(CC) $(CFLAGS) src/olvm.c tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm32d $(L) -m32
	@./vm32d
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -Iinclude -DNAKED_VM -DOLVM_FFI=1 -o ffi32 $(L) -m32 -Xlinker --export-dynamic
	@for F in $^ ;do \
	   echo -n "Testing $$F ... " ;\
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
	@$(CC) $(CFLAGS) src/olvm.c tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm64d $(L) -m64
	@./vm64d
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -Iinclude -DNAKED_VM -DOLVM_FFI=1 -o ffi64 $(L) -m64 -Xlinker --export-dynamic
	@for F in $^ ;do \
	   echo -n "Testing $$F ... " ;\
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
	@$(CC) $(CFLAGS) src/olvm.c tests/ffi.c -Iinclude -DNAKED_VM -DOLVM_FFI=1 -o $(ffi)$(affix)$(bits) $(L) -Xlinker --export-dynamic
	   @echo -n "Testing $(ffi)$(affix)$(bits) ... "
	   @if $(ffi)$(affix)$(bits) repl <tests/ffi.scm | diff - tests/ffi.scm.ok >/dev/null; then\
	      echo "Ok.";\
	   else \
	      echo "failed." ;\
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
	$(CC) $(CFLAGS) src/olvm.c tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o $(vmi)$(affix)$(bits) $(L)
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
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -DOLVM_FFI=1 -o $(vm)d$(bits) $(L) $(CFLAGS_DEBUG) -Xlinker --export-dynamic
vm-release:
	$(CC) $(CFLAGS) src/olvm.c -DNAKED_VM -DOLVM_FFI=1 -o $(vm)r$(bits) $(L) $(CFLAGS_RELEASE) -Xlinker --export-dynamic
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

testing-binaries:
ifeq ($(UNAME),Linux) # special case, try to do maximal testings under main development platform
	@echo -n "Building test binaries (debug and release, 32-bit and 64-bit versions both)... "
ifeq ($(HAS_32CDEFS),1)
	@$(MAKE) -s vm-debug-32
	@$(MAKE) -s vm-release-32
endif
ifeq ($(HAS_64CDEFS),1)
	@$(MAKE) -s vm-debug-64
	@$(MAKE) -s vm-release-64
endif
	@echo "built."
else
	@echo -n "Building test binaries (debug and release versions both)... "
	@$(MAKE) -s vm-debug
	@$(MAKE) -s vm-release
	@echo "Built."
endif

testing-embed: embed
	@echo -n "Testing embed... "
	@./embed

%.scm.ok: %.scm
	@echo -n "Testing $^ ..."
ifeq ($(UNAME),Linux) # special case, try to do maximal testings under main development platform
ifeq ($(HAS_32CDEFS),1)
	@echo -n ", debug-32: "
	@if $(vm)d32 repl <$^ | diff - $^.ok >/dev/null; then\
	   echo -n "ok" ;\
	else \
	   echo -n "\033[0;31mfailed\033[0m" ;\
	   touch $(FAILED) ;\
	fi
	@echo -n ", release-32: "
	@if $(vm)r32 repl <$^ | diff - $^.ok >/dev/null; then\
	   echo -n "ok" ;\
	else \
	   echo -n "\033[0;31mfailed\033[0m" ;\
	   touch $(FAILED) ;\
	fi
endif
ifeq ($(HAS_64CDEFS),1)
	@echo -n ", debug-64: "
	@if $(vm)d64 repl <$^ | diff - $^.ok >/dev/null; then\
	   echo -n "ok" ;\
	else \
	   echo -n "\033[0;31mfailed\033[0m" ;\
	   touch $(FAILED) ;\
	fi
	@echo -n ", release-64: "
	@if $(vm)r64 repl <$^ | diff - $^.ok >/dev/null; then\
	   echo -n "ok" ;\
	else \
	   echo -n "\033[0;31mfailed\033[0m" ;\
	   touch $(FAILED) ;\
	fi
endif
else
	@echo -n ", debug: "
	@if $(vm)d repl <$^ | diff - $^.ok >/dev/null; then\
	   echo -n "ok" ;\
	else \
	   echo -n "\033[0;31mfailed\033[0m" ;\
	   touch $(FAILED) ;\
	fi
	@echo -n ", release: "
	@if $(vm)r repl <$^ | diff - $^.ok >/dev/null; then\
	   echo -n "ok" ;\
	else \
	   echo -n "\033[0;31mfailed\033[0m" ;\
	   touch $(FAILED) ;\
	fi
endif
	@echo "."


# -=( tests )=-------------------------------------
tests: testing-binaries
tests: testing-embed
tests: $(wildcard tests/*.scm)
	@rm -f $(FAILED)
	@echo " "
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
	@echo "common tests"
	@echo "---------------------------------------"
	@for F in $(filter-out tests/ffi.scm, $(filter %.scm,$^)) ;do \
	   $(MAKE) -s -B $$F.ok ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"

endif