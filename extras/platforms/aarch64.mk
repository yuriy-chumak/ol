# apt install qemu-system-aarch64 qemu-user

# apt install gcc-aarch64-linux-gnu
ifneq ($(shell command -v aarch64-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-aarch64 2>/dev/null),)
HAVE_AARCH64 ?= $(HAVE_PLATFORM)
endif
endif
HAVE_AARCH64 ?= 0
AARCH64 ?= qemu-aarch64 -L /usr/aarch64-linux-gnu

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-aarch64
test-matrix-header-aarch64:
	case "`expr $(HAVE_AARCH64) `" in \
		1) printf "| aarch64 ";;\
	esac

test-matrix-subheader: test-matrix-subheader-aarch64
test-matrix-subheader-aarch64:
	if [ "$(HAVE_AARCH64)"  = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-aarch64
scmtest-aarch64:
ifeq ($(DEV_MODE)$(HAVE_AARCH64),11)
	$(call scmtestok,tmp/$(EXECUTABLE),aarch64,debug,$(AARCH64))
	$(call scmtestok,tmp/$(EXECUTABLE),aarch64,release,$(AARCH64))
endif

# ----------------------------------------------------------------
tmp/%-aarch64-debug: CC=aarch64-linux-gnu-gcc
tmp/%-aarch64-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

tmp/%-aarch64-release: CC=aarch64-linux-gnu-gcc
tmp/%-aarch64-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_AARCH64),11)
olvm-binaries: tmp/olvm-aarch64-debug
olvm-binaries: tmp/olvm-aarch64-release
endif
