ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# ############################################################
# -- main testing primitives --------------
.PHONY: check # run all tests and platforms
.PHONY: check-native # only native platform

.PHONY: regression-tests # regression tests only
.PHONY: regression-tests-native

regression-tests-native:
	DEV_MODE=0 make regression-tests

# enable as part of global testing
check: regression-tests

# -----------------------------
# notifications:
ok:="$(green) ok $(done)"
fail:="$(red)fail$(done)"

# # win32 tests:
# # apt install gcc-mingw-w64-i686 gcc-mingw-w64-x86-64

# ifeq ($(shell command -v $(MGCC32) 2>/dev/null),)
# HAVE_MINGW32 ?= 0
# else
# HAVE_MINGW32 ?= 1
# endif
# ifeq ($(shell command -v $(MGCC64) 2>/dev/null),)
# HAVE_MINGW64 ?= 0
# else
# HAVE_MINGW64 ?= 1
# endif

MACHINE ?= $(shell uname -m)

# set to 0 to disable platforms automatic detection
HAVE_PLATFORM ?= 1

# try to maximal testings under main development platform
# TODO: change to MULTIPLATFORM_MODE ?= 1
ifeq ($(UNAME)-$(MACHINE),Linux-x86_64)
DEV_MODE ?= 1
endif
DEV_MODE ?= 0

# note: use 2>/dev/null in "shell command" to avoid
#       make call optimization and really run shell.

# -------------------------------------------
# i386 linux
# sudo apt-get install gcc-multilib
ifeq ($(call exists,-m32,sys/cdefs.h,exit),1)
HAVE_X86 ?= $(HAVE_PLATFORM)
endif
HAVE_X86 ?= 0

# x86_64 linux
ifeq ($(call exists,-m64,sys/cdefs.h,exit),1)
HAVE_X86_64 ?= $(HAVE_PLATFORM)
endif
HAVE_X86_64 ?= 0

# -------------------------------------------
# aarch64 linux
# sudo apt-get install gcc-aarch64-linux-gnu
# qemu-system-aarch64, qemu-efi-aarch64
ifneq ($(shell command -v aarch64-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-aarch64 2>/dev/null),)
HAVE_AARCH64 ?= $(HAVE_PLATFORM)
endif
endif
HAVE_AARCH64 ?= 0

# -------------------------------------------
# MIPS/MIPSEL linux
include extras/platforms/mips.mk
include extras/platforms/mipsel.mk

# -------------------------------------------
# ppc64 linux
# sudo apt-get install gcc-powerpc64-linux-gnu
# qemu-system-ppc64, qemu-efi-aarch64
ifneq ($(shell command -v powerpc64-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-ppc64 2>/dev/null),)
HAVE_PPC64 ?= $(HAVE_PLATFORM)
endif
endif
# -------------------------------------------

## android (any)
#ifeq ($(shell adb devices 2>/dev/null | awk 'NR>1 {print}' | grep -q "device" && echo "Device connected" || echo "No device found"),Device connected)
#HAVE_ANDROID ?= 1
#endif

# -------------------------------------------
# i386 win32
# apt install gcc-mingw-w64-i686
ifneq ($(shell command -v $(MGCC32) 2>/dev/null),)
HAVE_MINGW32 ?= $(HAVE_PLATFORM)
endif

# x86_64 win64
# apt install gcc-mingw-w64-x86-64
ifneq ($(shell command -v $(MGCC64) 2>/dev/null),)
HAVE_MINGW64 ?= $(HAVE_PLATFORM)
endif

# dpkg --add-architecture i386 && apt-get update
# apt install wine32
# note: if you need to disable com ports under wine, then navigate to
#       HKLM\Software\Wine and create a new empty String named 'com33' (or smth)
ifneq ($(shell command -v wine 2>/dev/null),)
HAVE_WINE ?= $(HAVE_PLATFORM)
endif

# -------------------------------------------
# zero unassigned platforms
HAVE_X86     ?= 0
HAVE_X86_64  ?= 0

HAVE_ARMV7   ?= 0
HAVE_AARCH64 ?= 0
HAVE_ANDROID ?= 0

HAVE_MINGW32 ?= 0
HAVE_MINGW64 ?= 0

HAVE_PPC64   ?= 0

# qemu
AARCH64 ?= qemu-aarch64 -L /usr/aarch64-linux-gnu
PPC64 ?= qemu-ppc64 -L /usr/powerpc64-linux-gnu


# wine
HAVE_WINE ?= 0

# wine is not required under WSL, so:
ifdef WSL_DISTRO_NAME
WINE ?=
else  # disable any wine logging
WINE ?= WINEDEBUG=-all wine cmd /c 
endif

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
define notify
	[ -z "$(BACKEND_URL)" ] || \
	curl -s "$(BACKEND_URL)" -X PUT \
	     -H 'Content-Type: application/json' \
	     -d "{'session':$(SESSION), 'runner':'$(RUNNER)', 'name':'$1', 'platform':'$2', 'target':'$3', 'status':$4}"
endef

define scmtestok
	@if ([ -f $1-$2-$3$6 ]); then\
		if ([ -f $^.in ] && $4 $1-$2-$3$6 repl --home=libraries:$(TEST_HOME) $^ <$^.in 2>&1 \
		                 || $4 $1-$2-$3$6 repl --home=libraries:$(TEST_HOME) $^ 2>&1) \
				| diff $5 - $^.ok >/dev/null; then\
			printf \|$(ok) ;\
			$(call notify,$^,$2,$3,1) ;\
		else \
			printf \|$(fail);\
			echo $1-$2-$3$6: $^ >> $(FAILMARK);\
			$(call notify,$^,$2,$3,0) ;\
		fi;\
		true; \
	fi
endef

%.scm.ok: %.scm
# i386
ifeq ($(DEV_MODE)$(HAVE_X86),11)
	$(call scmtestok,tmp/$(EXECUTABLE),x86,debug)
	$(call scmtestok,tmp/$(EXECUTABLE),x86,release)
endif
# x86_64
ifeq ($(DEV_MODE)$(HAVE_X86_64),11)
	$(call scmtestok,tmp/$(EXECUTABLE),x86_64,debug)
	$(call scmtestok,tmp/$(EXECUTABLE),x86_64,release)
endif
# aarch64
ifeq ($(DEV_MODE)$(HAVE_AARCH64),11)
	$(call scmtestok,tmp/$(EXECUTABLE),aarch64,debug,$(AARCH64))
	$(call scmtestok,tmp/$(EXECUTABLE),aarch64,release,$(AARCH64))
endif
# mips/mipsel
ifeq ($(DEV_MODE)$(HAVE_MIPS),11)
	$(call scmtestok,tmp/$(EXECUTABLE),mips,debug,$(MIPS))
	$(call scmtestok,tmp/$(EXECUTABLE),mips,release,$(MIPS))
endif
ifeq ($(DEV_MODE)$(HAVE_MIPS64),11)
	$(call scmtestok,tmp/$(EXECUTABLE),mips64,debug,$(MIPS64))
	$(call scmtestok,tmp/$(EXECUTABLE),mips64,release,$(MIPS64))
endif
ifeq ($(DEV_MODE)$(HAVE_MIPSEL),11)
	$(call scmtestok,tmp/$(EXECUTABLE),mipsel,debug,$(MIPSEL))
	$(call scmtestok,tmp/$(EXECUTABLE),mipsel,release,$(MIPSEL))
endif
ifeq ($(DEV_MODE)$(HAVE_MIPS64EL),11)
	$(call scmtestok,tmp/$(EXECUTABLE),mips64el,debug,$(MIPS64EL))
	$(call scmtestok,tmp/$(EXECUTABLE),mips64el,release,$(MIPS64EL))
endif
# ppc64
ifeq ($(DEV_MODE)$(HAVE_PPC64),11)
	$(call scmtestok,tmp/$(EXECUTABLE),ppc64,debug,$(PPC64))
	$(call scmtestok,tmp/$(EXECUTABLE),ppc64,release,$(PPC64))
endif
# win
ifeq ($(DEV_MODE)$(HAVE_WINE),11)
	printf "| "
endif
# win32
ifeq ($(DEV_MODE)$(HAVE_MINGW32)$(HAVE_WINE),111)
	$(call scmtestok,tmp/$(EXECUTABLE),win32,debug,$(WINE),--strip-trailing-cr,.exe)
	$(call scmtestok,tmp/$(EXECUTABLE),win32,release,$(WINE),--strip-trailing-cr,.exe)
endif
# win64
ifeq ($(DEV_MODE)$(HAVE_MINGW64)$(HAVE_WINE),111)
	$(call scmtestok,tmp/$(EXECUTABLE),win64,debug,$(WINE),--strip-trailing-cr,.exe)
	$(call scmtestok,tmp/$(EXECUTABLE),win64,release,$(WINE),--strip-trailing-cr,.exe)
endif
# native binaries
ifeq ($(DEV_MODE),0)
	$(call scmtestok,tmp/$(EXECUTABLE),native,debug)
	$(call scmtestok,tmp/$(EXECUTABLE),native,release)
endif
	@printf "|\n"

# -- bin <- bin.ok ------------------------------------------
define bintestok
	@if ([ -f $1 ]); then\
		if ([ -f $^.in ] && $3 $1 $^ --home=libraries <$^.in || $3 $1 $^ --home=libraries) | diff $4 - $^.ok >/dev/null; then\
			printf \|$(ok) ;\
		else \
			printf \|$(fail);\
			touch $(FAILMARK);\
		fi;\
	fi
endef

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
		case "`expr    $(HAVE_X86) + $(HAVE_X86_64) `" in \
			1) printf "| %-8s" `uname -m`;;\
			2) printf "| %-18s" `uname -m`;;\
		esac ;\
		case "`expr    $(HAVE_ARMV7) `" in \
			1) printf "| armv7   ";;\
		esac ;\
		case "`expr    $(HAVE_AARCH64) `" in \
			1) printf "| aarch64 ";;\
		esac ;\
		case "`expr    $(HAVE_MIPS) + $(HAVE_MIPS64)`" in \
			1) printf "| %-8s" 'mips';;\
			2) printf "| %-18s" 'mips';;\
		esac ;\
		case "`expr    $(HAVE_MIPSEL) + $(HAVE_MIPS64EL)`" in \
			1) printf "| %-8s" 'mipsel';;\
			2) printf "| %-18s" 'mipsel';;\
		esac ;\
		case "`expr    $(HAVE_PPC64) `" in \
			1) printf "| ppc64   ";;\
		esac ;\
		case "`expr \( $(HAVE_MINGW32) + $(HAVE_MINGW64) \) \* $(HAVE_WINE)`" in \
			1) printf "| | windows ";;\
			2) printf "| | windows           ";;\
		esac ;\
	else \
	: regular platform ;\
		printf "| %-7s " Native ;\
	fi
	printf "|\n"

	: header line 1
	printf "|%-$1s " "Test"
	if [ "$(DEV_MODE)" = "1" ]; \
	then \
	: main development platform, special case;\
		if [ "$(HAVE_X86)"      = "1" ]; then printf "|32-d|32-r"; fi ;\
		if [ "$(HAVE_X86_64)"   = "1" ]; then printf "|64-d|64-r"; fi ;\
		if [ "$(HAVE_ARMV7)"    = "1" ]; then printf "|32-d|32-r"; fi ;\
		if [ "$(HAVE_AARCH64)"  = "1" ]; then printf "|64-d|64-r"; fi ;\
		if [ "$(HAVE_MIPS)"     = "1" ]; then printf "|32-d|32-r"; fi ;\
		if [ "$(HAVE_MIPS64)"   = "1" ]; then printf "|64-d|64-r"; fi ;\
		if [ "$(HAVE_MIPSEL)"   = "1" ]; then printf "|32-d|32-r"; fi ;\
		if [ "$(HAVE_MIPS64EL)" = "1" ]; then printf "|64-d|64-r"; fi ;\
		if [ "$(HAVE_PPC64)"    = "1" ]; then printf "|64-d|64-r"; fi ;\
		if [ "$(DEV_MODE)$(HAVE_WINE)"     = "11" ]; then printf "| "; fi ;\
		if [ "$(HAVE_MINGW32)$(HAVE_WINE)" = "11" ]; then printf "|32-d|32-r"; fi ;\
		if [ "$(HAVE_MINGW64)$(HAVE_WINE)" = "11" ]; then printf "|64-d|64-r"; fi ;\
	else \
	: regular platform ;\
		printf "|dbg.|rel." ;\
	fi
	printf "|\n"
endef

# return 1 if test failed, check test running with unique id
# fail flag

tests:
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