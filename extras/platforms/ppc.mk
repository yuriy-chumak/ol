# apt install qemu-system-ppc, qemu-user

# apt install gcc-powerpc-linux-gnu
ifneq ($(shell command -v powerpc-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-ppc 2>/dev/null),)
HAVE_PPC ?= $(HAVE_PLATFORM)
endif
endif
HAVE_PPC ?= 0
PPC ?= qemu-ppc -L /usr/powerpc-linux-gnu

# apt install gcc-powerpc64-linux-gnu
ifneq ($(shell command -v powerpc64-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-ppc64 2>/dev/null),)
HAVE_PPC64 ?= $(HAVE_PLATFORM)
endif
endif
HAVE_PPC64 ?= 0
PPC64 ?= qemu-ppc64 -L /usr/powerpc64-linux-gnu

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-ppc
test-matrix-header-ppc:
	case "`expr $(HAVE_PPC) + $(HAVE_PPC64)`" in \
		1) printf "| %-8s" 'ppc';;\
		2) printf "| %-18s" 'ppc';;\
	esac

test-matrix-subheader: test-matrix-subheader-ppc
test-matrix-subheader-ppc:
	if [ "$(HAVE_PPC)"   = "1" ]; then printf "|32-d|32-r"; fi
	if [ "$(HAVE_PPC64)" = "1" ]; then printf "|64-d|64-r"; fi

# ----------------------------------------------------------------
scmtest: scmtest-ppc
scmtest-ppc:
ifeq ($(DEV_MODE)$(HAVE_PPC),11)
	$(call scmtestok,tmp/$(EXECUTABLE),ppc,debug,$(PPC))
	$(call scmtestok,tmp/$(EXECUTABLE),ppc,release,$(PPC))
endif
ifeq ($(DEV_MODE)$(HAVE_PPC64),11)
	$(call scmtestok,tmp/$(EXECUTABLE),ppc64,debug,$(PPC64))
	$(call scmtestok,tmp/$(EXECUTABLE),ppc64,release,$(PPC64))
endif

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_PPC),11)
# ppc debug
olvm-binaries: tmp/olvm-ppc-debug

tmp/olvm-ppc-debug: CC=powerpc-linux-gnu-gcc
tmp/olvm-ppc-debug: $(FFI_DEPS)
	$(call build-olvm,$@,$(FFI_CFLAGS_DEBUG) $(OLVM_EXPORT))

# ppc release
olvm-binaries: tmp/olvm-ppc-release

tmp/olvm-ppc-release: CC=powerpc-linux-gnu-gcc
tmp/olvm-ppc-release: $(FFI_DEPS)
	$(call build-olvm,$@,$(FFI_CFLAGS_RELEASE) $(OLVM_EXPORT))

endif

ifeq ($(DEV_MODE)$(HAVE_PPC64),11)
# ppc64 debug
olvm-binaries: tmp/olvm-ppc64-debug

tmp/olvm-ppc64-debug: CC=powerpc64-linux-gnu-gcc
tmp/olvm-ppc64-debug: $(FFI_DEPS)
	$(call build-olvm,$@,$(FFI_CFLAGS_DEBUG) $(OLVM_EXPORT))

# ppc64 release
olvm-binaries: tmp/olvm-ppc64-release

tmp/olvm-ppc64-release: CC=powerpc64-linux-gnu-gcc
tmp/olvm-ppc64-release: $(FFI_DEPS)
	$(call build-olvm,$@,$(FFI_CFLAGS_RELEASE) $(OLVM_EXPORT))

endif
