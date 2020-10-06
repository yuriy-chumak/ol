.PHONY: tests testing-binaries

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

FAILED := $(shell mktemp -u)

ok:="\033[1;32mok\033[0m"
failed:="\033[1;31mfailed\033[0m"

# GNU/Linux (base development system) specific defines:
ifeq ($(UNAME),Linux)
HAS_32CDEFS   ?= $(call exists,-m32,<sys/cdefs.h>,exit)
HAS_64CDEFS   ?= $(call exists,-m64,<sys/cdefs.h>,exit)
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


vm32: src/olvm.c include/olvm.h
	$(CC) src/olvm.c -DNAKED_VM -o $@ \
	   $(L) -m32 -DOLVM_FFI=0 \
	   $(CFLAGS)
	@echo Ok.
vm64: src/olvm.c include/olvm.h
	$(CC) src/olvm.c -DNAKED_VM -o $@ \
	   $(L) -m64 -DOLVM_FFI=0 \
	   $(CFLAGS)
	@echo Ok.

test32: $(wildcard tests/*.scm)
	@echo "-- test32 ----------"
	@rm -f $(FAILED)
	@$(CC) src/olvm.c $(CFLAGS) tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm32d $(L) -m32
	@./vm32d
	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iinclude -DNAKED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o ffi32 $(L) -m32
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
	@$(CC) src/olvm.c $(CFLAGS) tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o vm64d $(L) -m64
	@./vm64d
	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iinclude -DNAKED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o ffi64 $(L) -m64
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
	@$(CC) src/olvm.c $(CFLAGS) tests/ffi.c -Iinclude -DNAKED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o $(ffi)$(affix)$(bits) $(L) \
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
	$(CC) src/olvm.c $(CFLAGS) tests/vm.c -Iinclude -DNAKED_VM -DEMBEDDED_VM -o $(vmi)$(affix)$(bits) $(L)
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
	$(CC) src/olvm.c $(CFLAGS) -DNAKED_VM \
	-DOLVM_FFI=1 -Isrc extensions/ffi.c -o $(vm)d$(bits) $(L) $(CFLAGS_DEBUG)
vm-release:
	$(CC) src/olvm.c $(CFLAGS) -DNAKED_VM \
	-DOLVM_FFI=1 -Isrc extensions/ffi.c -o $(vm)r$(bits) $(L) $(CFLAGS_RELEASE)
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
ifeq ($(HAS_32CDEFS)$(HAS_64CDEFS),11)
	@printf "Building test binaries (debug and release, 32-bit and 64-bit versions both)... "
endif
ifeq ($(HAS_32CDEFS)$(HAS_64CDEFS),10)
	@printf "Building test binaries (debug and release, 32-bit versions)... "
endif
ifeq ($(HAS_32CDEFS)$(HAS_64CDEFS),01)
	@printf "Building test binaries (debug and release, 64-bit versions)... "
endif

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
	@printf "Building test binaries (debug and release versions both)... "
	@$(MAKE) -s vm-debug
	@$(MAKE) -s vm-release
	@echo "Built."
endif

testing-embed: tests/embed.c src/olvm.c extensions/embed.h tmp/repl.c
	@printf "Testing embed ... "
ifeq ($(UNAME),Linux) # special case, try to do maximal testings under main development platform
ifeq ($(HAS_32CDEFS),1)
	@printf ", debug-32: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DEMBEDDED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o tmp/embed32d \
	   -Iextensions -Iinclude -lm $(CFLAGS_DEBUG) -m32
	@if tmp/embed32d >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release-32: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DEMBEDDED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o tmp/embed32r \
	   -Iextensions -Iinclude -lm $(CFLAGS_RELEASE) -m32
	@if tmp/embed32r >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
endif
ifeq ($(HAS_64CDEFS),1)
	@printf ", debug-64: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DEMBEDDED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o tmp/embed64d \
	   -Iextensions -Iinclude -lm $(CFLAGS_DEBUG) -m64
	@if tmp/embed64d >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release-64: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DEMBEDDED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o tmp/embed64r \
	   -Iextensions -Iinclude -lm $(CFLAGS_RELEASE) -m64
	@if tmp/embed64r >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
endif
else
	@printf ", debug: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DEMBEDDED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o tmp/embedd \
	   -Iextensions -Iinclude -lm $(CFLAGS_DEBUG)
	@if tmp/embedd >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release: "
	@$(CC) tests/embed.c src/olvm.c tmp/repl.c $(CFLAGS) $(L) -DEMBEDDED_VM \
	   -DOLVM_FFI=1 -Isrc extensions/ffi.c -o tmp/embedr \
	   -Iextensions -Iinclude -lm $(CFLAGS_RELEASE)
	@if tmp/embedr >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
endif
	@echo "."

%.scm.ok: %.scm
	@printf "Testing $^ ..."
ifeq ($(UNAME),Linux) # special case, try to do maximal testings under main development platform
ifeq ($(HAS_32CDEFS),1)
	@printf ", debug-32: "
	@if ([ -f $^.in ] && $(vm)d32 repl $^ <$^.in || $(vm)d32 repl $^) | diff - $^.ok >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release-32: "
	@if ([ -f $^.in ] && $(vm)r32 repl $^ <$^.in || $(vm)r32 repl $^) | diff - $^.ok >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
endif
ifeq ($(HAS_64CDEFS),1)
	@printf ", debug-64: "
	@if ([ -f $^.in ] && $(vm)d64 repl $^ <$^.in || $(vm)d64 repl $^) | diff - $^.ok >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release-64: "
	@if ([ -f $^.in ] && $(vm)r64 repl $^ <$^.in || $(vm)r64 repl $^) | diff - $^.ok >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
endif
else
	@printf ", debug: "
	@if ([ -f $^.in ] && $(vm)d repl $^ <$^.in || $(vm)d repl $^) | diff - $^.ok >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
	   touch $(FAILED) ;\
	fi
	@printf ", release: "
	@if ([ -f $^.in ] && $(vm)r repl $^ <$^.in || $(vm)r repl $^) | diff - $^.ok >/dev/null; then\
	   printf $(ok) ;\
	else \
	   printf $(failed) ;\
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
	@echo "\033[1;32mpassed!\033[0m"

endif