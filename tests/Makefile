ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

TEST_FILES += $(wildcard tests/*.scm)

check: regression-tests

check-simple:
	DEV_MACHINE=0 $(MAKE) check

# main:
.PHONY: check   # includes all other checks
.PHONY: regression-tests # regression tests

# notifications:
ok:="$(green) ok $(done)"
fail:="$(red)fail$(done)"

# fail flag
FAILED := $(shell mktemp -u /tmp/failed.XXXXXXXXX)



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

# try to maximal testings under main development platform
ifeq ($(UNAME)-$(MACHINE),Linux-x86_64)
DEV_MACHINE  ?= 1
endif

# note: use 2>/dev/null in "shell command" to avoid
#       make call optimization and really run shell.

# DEV_MACHINE == 1
ifeq ($(DEV_MACHINE),1)

# i386 linux
# sudo apt-get install gcc-multilib
HAVE_32CDEFS ?= $(call exists,-m32,sys/cdefs.h,exit)

# x86_64 linux
HAVE_64CDEFS ?= $(call exists,-m64,sys/cdefs.h,exit)

# aarch64 linux
# sudo apt-get install gcc-aarch64-linux-gnu
# qemu-system-aarch64, qemu-efi-aarch64
ifneq ($(shell command -v aarch64-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-aarch64 2>/dev/null),)
HAVE_AARCH64 ?= 1
endif
endif

# ppc64 linux
# sudo apt-get install gcc-powerpc64-linux-gnu
# qemu-system-ppc64, qemu-efi-aarch64
ifneq ($(shell command -v powerpc64-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-ppc64 2>/dev/null),)
HAVE_PPC64 ?= 1
endif
endif

# android (any)
ifeq ($(shell adb devices | awk 'NR>1 {print}' | grep -q "device" && echo "Device connected" || echo "No device found"),Device connected)
HAVE_ANDROID ?= 1
endif

# i386 win32
# apt install gcc-mingw-w64-i686
ifneq ($(shell command -v $(MGCC32) 2>/dev/null),)
HAVE_MINGW32 ?= 1
endif

# x86_64 win64
# apt install gcc-mingw-w64-x86-64
ifneq ($(shell command -v $(MGCC64) 2>/dev/null),)
HAVE_MINGW64 ?= 1
endif

# dpkg --add-architecture i386 && apt-get update
# apt install wine32
# note: if you need to disable com ports under wine, then navigate to
#       HKLM\Software\Wine and create a new empty String named 'com33' (or smth)
ifneq ($(shell command -v wine 2>/dev/null),)
HAVE_WINE ?= 1
endif

endif # DEV_MACHINE

# only platform inner tests
DEV_MACHINE ?= 0
HAVE_64CDEFS ?= 0
HAVE_32CDEFS ?= 0
HAVE_ARMV7   ?= 0
HAVE_AARCH64 ?= 0
HAVE_ANDROID ?= 0
HAVE_MINGW32 ?= 0
HAVE_MINGW64 ?= 0
HAVE_PPC64   ?= 0

# wine
HAVE_WINE ?= 0
# wine is not required under WSL, so:
ifdef WSL_DISTRO_NAME
WINE ?=
else  # disable any wine logging
WINE ?= WINEDEBUG=-all wine cmd /c 
endif

# ============================================================
# -- olvm binaries -------------------------------------------
define olvm-
	@echo "------------------------------------------------------------------------------"
	@printf "\n$@:\n   "
	$(CC) src/olvm.c \
	   extensions/ffi.c -Iincludes \
	   $2 -o $1
endef
olvm-binaries:

ifeq ($(DEV_MACHINE),1)

OLVM_CFLAGS += -std=gnu99 -Wno-int-to-pointer-cast
OLVM_CFLAGS += -fno-exceptions -fno-unwind-tables -fno-asynchronous-unwind-tables
OLVM_CFLAGS += -DHAVE_DLOPEN=1 -DHAS_SOCKES=1 -DOLVM_FFI=1
OLVM_CFLAGS += -D_FILE_OFFSET_BITS=64
OLVM_CFLAGS += -lm

# i386 linux
# ----------------------------
ifeq ($(HAVE_32CDEFS),1)
# 32-bit debug
olvm-binaries: tmp/olvm-i386-debug

tmp/olvm-i386-debug: CC=gcc
tmp/olvm-i386-debug: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_DEBUG) -m32) # -z noexecstack

# 32-bit release
olvm-binaries: tmp/olvm-i386-release

tmp/olvm-i386-release: CC=gcc
tmp/olvm-i386-release: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_RELEASE) -m32)

endif

# x86_64 linux
# ----------------------------
ifeq ($(HAVE_64CDEFS),1)
# 64-bit debug
olvm-binaries: tmp/olvm-x86_64-debug

tmp/olvm-x86_64-debug: CC=gcc
tmp/olvm-x86_64-debug: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_DEBUG))

# 64-bit release
olvm-binaries: tmp/olvm-x86_64-release

tmp/olvm-x86_64-release: CC=gcc
tmp/olvm-x86_64-release: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_RELEASE))

endif

# aarch64 linux
# ----------------------------
ifeq ($(HAVE_AARCH64),1)
# 64-bit debug
olvm-binaries: tmp/olvm-aarch64-debug

tmp/olvm-aarch64-debug: CC=aarch64-linux-gnu-gcc
tmp/olvm-aarch64-debug: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_DEBUG))

# 64-bit release
olvm-binaries: tmp/olvm-aarch64-release

tmp/olvm-aarch64-release: CC=aarch64-linux-gnu-gcc
tmp/olvm-aarch64-release: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_RELEASE))

endif

# ppc64 linux
# ----------------------------
ifeq ($(HAVE_AARCH64),1)
# 64-bit debug
olvm-binaries: tmp/olvm-ppc64-debug

tmp/olvm-ppc64-debug: CC=powerpc64-linux-gnu-gcc
tmp/olvm-ppc64-debug: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_DEBUG))

# 64-bit release
olvm-binaries: tmp/olvm-ppc64-release

tmp/olvm-ppc64-release: CC=powerpc64-linux-gnu-gcc
tmp/olvm-ppc64-release: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_RELEASE))

endif


# # riscv64-unknown-elf-gcc
# tmp/olvm-riscv64-debug: CC=riscv64-unknown-elf-gcc
# tmp/olvm-riscv64-debug: src/olvm.c extensions/ffi.c
# 	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_DEBUG))


# win32
# ----------------------------
ifeq ($(HAVE_MINGW32),1)
# 32-bit debug
olvm-binaries: tmp/olvm-win32-debug.exe

tmp/olvm-win32-debug.exe: CC=$(MGCC32)
tmp/olvm-win32-debug.exe: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_DEBUG) -Iincludes/win32 -lws2_32)

# 32-bit release.exe
olvm-binaries: tmp/olvm-win32-release.exe

tmp/olvm-win32-release.exe: CC=$(MGCC32)
tmp/olvm-win32-release.exe: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_RELEASE) -Iincludes/win32 -lws2_32)

endif

# win64
# ----------------------------
ifeq ($(HAVE_MINGW64),1)
# 64-bit debug
olvm-binaries: tmp/olvm-win64-debug.exe

tmp/olvm-win64-debug.exe: CC=$(MGCC64)
tmp/olvm-win64-debug.exe: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_DEBUG) -Iincludes/win32 -lws2_32)

# 64-bit release
olvm-binaries: tmp/olvm-win64-release.exe

tmp/olvm-win64-release.exe: CC=$(MGCC32)
tmp/olvm-win64-release.exe: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(OLVM_CFLAGS) $(CFLAGS_RELEASE) -Iincludes/win32 -lws2_32)

endif

else # DEV_MACHINE == 0
# ------------------------------------------------------------

# 64-bit debug
# ----------------------------
olvm-binaries: tmp/olvm-native-debug

tmp/olvm-native-debug: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(CFLAGS) $(CFLAGS_DEBUG))

# 64-bit release
olvm-binaries: tmp/olvm-native-release

tmp/olvm-native-release: src/olvm.c extensions/ffi.c
	$(call olvm-,$@,$(CFLAGS) $(CFLAGS_RELEASE))

endif # DEV_MACHINE

# -- scm <- scm.ok -------------------------------------------
define scmtestok
	@if ([ -f $1 ]); then\
		if ([ -f $^.in ] && $2 $1 repl --home=libraries $^ <$^.in 2>&1 || $2 $1 repl --home=libraries $^ 2>&1) | diff $3 - $^.ok >/dev/null; then\
			printf \|$(ok) ;\
		else \
			printf \|$(fail);\
			touch $(FAILED);\
		fi;\
	fi
endef

%.scm.ok: %.scm
ifeq ($(DEV_MACHINE),1)
ifeq ($(HAVE_32CDEFS),1)
	$(call scmtestok,tmp/olvm-i386-debug)
	$(call scmtestok,tmp/olvm-i386-release)
endif
ifeq ($(HAVE_64CDEFS),1)
	$(call scmtestok,tmp/olvm-x86_64-debug)
	$(call scmtestok,tmp/olvm-x86_64-release)
endif
# AArch64
ifeq ($(HAVE_AARCH64),1)
	$(call scmtestok,tmp/olvm-aarch64-debug,qemu-aarch64 -L /usr/aarch64-linux-gnu)
	$(call scmtestok,tmp/olvm-aarch64-release,qemu-aarch64 -L /usr/aarch64-linux-gnu)
endif
# ppc64
ifeq ($(HAVE_AARCH64),1)
	$(call scmtestok,tmp/olvm-ppc64-debug,qemu-ppc64 -L /usr/powerpc64-linux-gnu)
	$(call scmtestok,tmp/olvm-ppc64-release,qemu-ppc64 -L /usr/powerpc64-linux-gnu)
endif
ifeq ($(HAVE_MINGW32)$(HAVE_WINE),11)
	$(call scmtestok,tmp/olvm-win32-debug.exe,$(WINE),--strip-trailing-cr)
	$(call scmtestok,tmp/olvm-win32-release.exe,$(WINE),--strip-trailing-cr)
endif
ifeq ($(HAVE_MINGW64)$(HAVE_WINE),11)
	$(call scmtestok,tmp/olvm-win64-debug.exe,$(WINE),--strip-trailing-cr)
	$(call scmtestok,tmp/olvm-win64-release.exe,$(WINE),--strip-trailing-cr)
endif

else # default case
	$(call scmtestok,tmp/olvm-native-debug)
	$(call scmtestok,tmp/olvm-native-release)
endif
	@printf "|\n"

# -- bin <- bin.ok ------------------------------------------
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

# %.bin.ok: %.bin
# ifeq ($(DEV_MACHINE),1) # main development platform, special case
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

# # -------------------------------------------------
# # -=( tests )=-------------------------------------
# check: tests
# tests: testing-binaries
# 	@rm -f $(FAILED)
# 	$(MAKE) testing-internal
# 	$(MAKE) check-ffi
# 	$(MAKE) testing-embed
# 	$(MAKE) regression-tests

define table-header
	set -e
	: header line 1
	printf "| %-$1s" `uname`
	if [ "$(DEV_MACHINE)" = "1" ]; \
	then \
	: main development platform, special case;\
		case "`expr    $(HAVE_32CDEFS) + $(HAVE_64CDEFS) `" in \
			1) printf "| %-8s" `uname -m`;;\
			2) printf "| %-18s" `uname -m`;;\
		esac ;\
		case "`expr    $(HAVE_ARMV7) `" in \
			1) printf "| ARMv7   ";;\
		esac ;\
		case "`expr    $(HAVE_AARCH64) `" in \
			1) printf "| AArch64 ";;\
		esac ;\
		case "`expr    $(HAVE_PPC64) `" in \
			1) printf "| ppc64   ";;\
		esac ;\
		case "`expr \( $(HAVE_MINGW32) + $(HAVE_MINGW64) \) \* $(HAVE_WINE)`" in \
			1) printf "| Windows ";;\
			2) printf "| Windows           ";;\
		esac ;\
	else \
	: regular platform ;\
		printf "| %-7s " Native ;\
	fi
	printf "|\n"

	: header line 1
	printf "|%-$1s " "Test"
	if [ "$(DEV_MACHINE)" = "1" ]; \
	then \
	: main development platform, special case;\
	 	if [ "$(HAVE_32CDEFS)" = "1" ]; then printf "|32-d|32-r"; fi ;\
	 	if [ "$(HAVE_64CDEFS)" = "1" ]; then printf "|64-d|64-r"; fi ;\
	 	if [ "$(HAVE_ARMV7)"   = "1" ]; then printf "|32-d|32-r"; fi ;\
	 	if [ "$(HAVE_AARCH64)" = "1" ]; then printf "|64-d|64-r"; fi ;\
		if [ "$(HAVE_PPC64)"   = "1" ]; then printf "|64-d|64-r"; fi ;\
	 	if [ "$(HAVE_MINGW32)$(HAVE_WINE)" = "11" ]; then printf "|32-d|32-r"; fi ;\
	 	if [ "$(HAVE_MINGW64)$(HAVE_WINE)" = "11" ]; then printf "|64-d|64-r"; fi ;\
	else \
	: regular platform ;\
		printf "|dbg.|rel." ;\
	fi
	printf "|\n"
endef

# ------ regression tests -------------------------------------
.SILENT: regression-tests
regression-tests: olvm-binaries
regression-tests: $(wildcard tests/*.scm) $(wildcard tests/*.bin)
	echo " "
	echo "REGRESSION TESTS"
	$(eval F1LEN=$(shell for F in $(TEST_FILES); do echo $${#F}; done |sort -n| tail -1))
	$(call table-header, $(F1LEN))
	for F in $(filter %.scm %.bin,$^); do \
	   printf "|%-$(F1LEN)s " "$$F" |sed 's/ /*/; s/ /./g; s/*/ /' ;\
	   $(MAKE) -s -B $$F.ok ;\
	done
	if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	echo "$(green)passed!$(done)"

regression-tests: olvm-binaries

endif
