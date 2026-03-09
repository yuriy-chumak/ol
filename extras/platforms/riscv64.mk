# apt install qemu-system-riscv64 qemu-user

# apt install gcc-riscv64-linux-gnu
ifneq ($(shell command -v riscv64-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-riscv64 2>/dev/null),)
HAVE_RISCV64 ?= $(HAVE_PLATFORM)
endif
endif
HAVE_RISCV64 ?= 0
RISCV64 ?= qemu-riscv64 -L /usr/riscv64-linux-gnu

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-riscv64
test-matrix-header-riscv64:
	case "`expr $(HAVE_RISCV64)`" in \
		1) printf "| %-8s" 'riscv64';;\
	esac

test-matrix-subheader: test-matrix-subheader-riscv64
test-matrix-subheader-riscv64:
	if [ "$(HAVE_RISCV64)" = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-riscv64
scmtest-riscv64:
ifeq ($(DEV_MODE)$(HAVE_RISCV64),11)
	$(call test-scm,$(TEST),$(RISCV64),tmp/$(EXECUTABLE),riscv64,debug)
	$(call test-scm,$(TEST),$(RISCV64),tmp/$(EXECUTABLE),riscv64,release)
endif

# ----------------------------------------------------------------
# ppc64le debug
tmp/%-riscv64-debug: CC=riscv64-linux-gnu-gcc
tmp/%-riscv64-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# ppc64 release
tmp/%-riscv64-release: CC=riscv64-linux-gnu-gcc
tmp/%-riscv64-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_RISCV64),11)
olvm-binaries: tmp/olvm-riscv64-debug
olvm-binaries: tmp/olvm-riscv64-release
endif
