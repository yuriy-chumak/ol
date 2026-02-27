ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# test-matrix
.PHONY: test-matrix
BACKEND_URL?=http://127.0.0.1:8008/ol/test-matrix

test-matrix: SESSION=$(shell curl -s -X POST "$(BACKEND_URL)?build=$(VERSION)" 2>/dev/null)
test-matrix:
	BACKEND_URL=$(BACKEND_URL) SESSION=$(SESSION) \
	$(MAKE) -s check

# ############################################################
# -- main testing primitives --------------
.PHONY: check # run all tests and platforms
.PHONY: check-native # only native platform

# regression tests only
.PHONY: regression-tests
.PHONY: regression-tests-native

regression-tests-native:
	DEV_MODE=0 $(MAKE) regression-tests

# -----------------------------------------
# enable as first part of global testing
check: clean-log
clean-log:
	rm -f tmp/check.log

check: regression-tests

# -----------------------------
# notifications:
ok:="$(green) ok $(done)"
fail:="$(red)fail$(done)"

MACHINE ?= $(shell uname -m)

# set to 0 to disable platforms automatic detection
HAVE_PLATFORM ?= 1

# try to maximize testings under main development platform
# TODO?: change to MULTIPLATFORM_MODE ?= 1
ifeq ($(UNAME)-$(MACHINE),Linux-x86_64)
DEV_MODE ?= 1
endif
DEV_MODE ?= 0

# note: use 2>/dev/null in "shell command" to avoid
#       make call optimization and really run shell.



# ================================================================
# x86/x86_64
-include extras/platforms/i86.mk
# arm linux
-include extras/platforms/arm.mk
-include extras/platforms/aarch64.mk
# mips linux
-include extras/platforms/mips.mk
-include extras/platforms/mipsel.mk
# ppc linux
-include extras/platforms/ppc.mk
-include extras/platforms/ppcle64.mk
# sparc 64
-include extras/platforms/sparc64.mk
# x86 win(e)
-include extras/platforms/win(e).mk

# -------------------------------------------

## android (any)
#ifeq ($(shell adb devices 2>/dev/null | awk 'NR>1 {print}' | grep -q "device" && echo "Device connected" || echo "No device found"),Device connected)
#HAVE_ANDROID ?= 1
#endif

# -------------------------------------------
# zero unassigned platforms (deprecated)
HAVE_ANDROID ?= 0

# -- file list scripts -----
define find
	$(shell find "$1" -maxdepth 1 -name "$2" -print0 | sed 's/^|$/"/g')
endef

# -- build scripts ---------
define build-olvm
	@echo "------------------------------------------------------------------------------"
	@printf "\n$@:\n   "
	$(CC) src/olvm.c \
	   extensions/ffi.c -Iincludes \
	   $2 -o $1
endef

# -- scm <- scm.ok -------------------------------------------
# name, platform, target, status
# runner is a "test group"
define notify
	[ -z "$(BACKEND_URL)" ] || \
	curl -s "$(BACKEND_URL)" -X PUT \
	     -H 'Content-Type: application/json' \
	     -d "{'session':$(SESSION), 'runner':'$(RUNNER)', 'name':'$1', 'os':'$2', 'platform':'$3', 'target':'$4', 'status':$5}"
endef

define test-scm # testfile wine-or-something executable platform debug/release executable-suffix diff-options # TEST=test-filename
	echo "$1:$2:$3:$4:$5:$6:$7" >> tmp/check.log;\
	if ([ -f $3-$4-$5$6 ]); then\
	    echo "    $2 $3-$4-$5$6 repl --home=libraries:$(TEST_HOME) $1" >> tmp/check.log;\
	    if ([ -f $1.in ] && $2 $3-$4-$5$6 repl --home=libraries:$(TEST_HOME) $1 <$1.in 2>&1 \
	                     || $2 $3-$4-$5$6 repl --home=libraries:$(TEST_HOME) $1 2>&1) \
	            | diff $7 - $1.ok >/dev/null; then\
	        printf \|$(ok) ;\
	        $(call notify,$1,$(or $(OS),Linux),$(or $(PLATFORM),$4),$5,1) ;\
	    else \
	        printf \|$(fail);\
	        echo $3-$4-$5$6: $1 >> $(FAILMARK);\
	        $(call notify,$1,$(or $(OS),Linux),$(or $(PLATFORM),$4),$5,0) ;\
	    fi;\
	    true; \
	fi
endef

.PHONY: scmtest

%.scm.ok: %.scm
ifeq ($(DEV_MODE),0)   # native binaries
	$(call test-scm,$^,,tmp/$(EXECUTABLE),native,debug)
	$(call test-scm,$^,,tmp/$(EXECUTABLE),native,release)
	printf "|"
else                # multiplatform mode
	TEST=$^ $(MAKE) -s scmtest
endif
	printf "\n"

# -- bin <- bin.ok ------------------------------------------
# define bintestok
# 	@if ([ -f $1 ]); then\
# 		if ([ -f $^.in ] && $3 $1 $^ --home=libraries <$^.in || $3 $1 $^ --home=libraries) | diff $4 - $^.ok >/dev/null; then\
# 			printf \|$(ok) ;\
# 		else \
# 			printf \|$(fail);\
# 			touch $(FAILMARK);\
# 		fi;\
# 	fi
# endef

# %.bin.ok: %.bin
# ifeq ($(DEV_MODE),1) # main development platform, special case
# ifeq ($(HAS_32CDEFS),1)
# 	$(call bintestok,$(vm)d32,debug-32)
# 	$(call bintestok,$(vm)r32,release-32)
# endif
# ifeq ($(HAS_64CDEFS),1)
# 	$(call bintestok,$(vm)d64,debug-64)
# 	$(call bintestok,$(vm)r64,release-64)
# endif
# ifeq ($(HAVE_MINGW32)$(HAVE_WINE),11)
# 	$(call bintestok,$(vm)32d.exe,win-debug-32,$(WINE),--strip-trailing-cr)
# 	$(call bintestok,$(vm)32r.exe,win-release-32,$(WINE),--strip-trailing-cr)
# endif
# ifeq ($(HAVE_MINGW64)$(HAVE_WINE),11)
# 	$(call bintestok,$(vm)64d.exe,win-debug-64,$(WINE),--strip-trailing-cr)
# 	$(call bintestok,$(vm)64r.exe,win-release-64,$(WINE),--strip-trailing-cr)
# endif
# else # default case
# 	$(call bintestok,$(vm)d,debug)
# 	$(call bintestok,$(vm)r,release)
# endif
# 	@printf "|\n"

# -------------------------------------------------
# -=( tests )=-------------------------------------
.PHONY: tests
.SILENT: tests

define table-header
	set -e
	: header line 1
	printf "| %-$1s" `uname`
	if [ "$(DEV_MODE)" = "1" ]; \
	then \
	: main development platform, special case;\
		$(MAKE) -s test-matrix-header;\
	else \
	: regular platform ;\
		printf "| %-7s " Native ;\
	fi
	printf "|\n"

	: header line 2
	printf "|%-$1s " "Test"
	if [ "$(DEV_MODE)" = "1" ]; \
	then \
	: main development platform, special case;\
		$(MAKE) -s test-matrix-subheader;\
	else \
	: regular platform ;\
		printf "|dbg.|rel." ;\
	fi
	printf "\n"
endef

# return 1 if test failed, check test running with unique id
# fail flag

tests:
	rm -f $(FAILMARK)
	$(eval F1LEN=$(shell for F in $(TEST_FILES); do echo $${#F}; done |sort -n| tail -1))
	$(call table-header, $(F1LEN))
	for F in $(filter %.scm %.bin,$(TESTS)); do \
	   if [ -e $$F.ok ] ;then \
	      printf "|%-$(F1LEN)s " "$$F" |sed 's/ /*/; s/ /./g; s/*/ /' ;\
	      FAILMARK=$(FAILMARK) EXECUTABLE=$(EXECUTABLE) RUNNER=$(RUNNER) SESSION=$(SESSION) \
	         $(MAKE) -s -B $$F.ok;\
	   fi ;\
	done
	case `test -f $(FAILMARK); echo $$?` in\
	   0) echo "$(red)FAILED!$(done)";;\
	   1) echo "$(green)passed!$(done)";;\
	esac

endif
