# apt install qemu-system-ppc, qemu-user

# apt install gcc-powerpc64le-linux-gnu
ifneq ($(shell command -v powerpc64le-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-ppc64le 2>/dev/null),)
HAVE_PPC64LE ?= $(HAVE_PLATFORM)
endif
endif
HAVE_PPC64LE ?= 0
PPC64LE ?= qemu-ppc64le -L /usr/powerpc64le-linux-gnu

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-ppcle
test-matrix-header-ppcle:
	case "`expr $(HAVE_PPC64LE)`" in \
		1) printf "| %-8s" 'ppcle';;\
	esac

test-matrix-subheader: test-matrix-subheader-ppcle
test-matrix-subheader-ppcle:
	if [ "$(HAVE_PPC64LE)" = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-ppcle
scmtest-ppcle:
ifeq ($(DEV_MODE)$(HAVE_PPC64LE),11)
	$(call scmtestok,tmp/$(EXECUTABLE),ppc64le,debug,$(PPC64LE))
	$(call scmtestok,tmp/$(EXECUTABLE),ppc64le,release,$(PPC64LE))
endif

# ----------------------------------------------------------------
# ppc64le debug
tmp/%-ppc64le-debug: CC=powerpc64le-linux-gnu-gcc
tmp/%-ppc64le-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# ppc64 release
tmp/%-ppc64le-release: CC=powerpc64le-linux-gnu-gcc
tmp/%-ppc64le-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_PPC64),11)
olvm-binaries: tmp/olvm-ppc64le-debug
olvm-binaries: tmp/olvm-ppc64le-release
endif
