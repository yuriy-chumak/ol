# apt install qemu-system-mips qemu-user
# use qemu version 6.2.0 (latest 8.2.2 is broken, returns 'Bus error')

# apt install gcc-mips-linux-gnu
ifneq ($(shell command -v mips-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-mips 2>/dev/null),)
HAVE_MIPS ?= $(HAVE_PLATFORM)
endif
endif
HAVE_MIPS ?= 0
MIPS ?= qemu-mips -L /usr/mips-linux-gnu

# apt install gcc-mips64-linux-gnuabi64
ifneq ($(shell command -v mips64-linux-gnuabi64-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-mips64 2>/dev/null),)
HAVE_MIPS64 ?= $(HAVE_PLATFORM)
endif
endif
HAVE_MIPS64 ?= 0
MIPS64 ?= qemu-mips64 -L /usr/mips64-linux-gnuabi64

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-mips
test-matrix-header-mips:
	case "`expr $(HAVE_MIPS) + $(HAVE_MIPS64)`" in \
		1) printf "| %-8s" 'mips';;\
		2) printf "| %-18s" 'mips';;\
	esac

test-matrix-subheader: test-matrix-subheader-mips
test-matrix-subheader-mips:
	if [ "$(HAVE_MIPS)"     = "1" ]; then printf "|32-d|32-r"; fi
	if [ "$(HAVE_MIPS64)"   = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-mips
scmtest-mips:
ifeq ($(DEV_MODE)$(HAVE_MIPS),11)
	$(call test-scm,$(TEST),$(MIPS),tmp/$(EXECUTABLE),mips,debug)
	$(call test-scm,$(TEST),$(MIPS),tmp/$(EXECUTABLE),mips,release)

endif
ifeq ($(DEV_MODE)$(HAVE_MIPS64),11)
	$(call test-scm,$(TEST),$(MIPS64),tmp/$(EXECUTABLE),mips64,debug)
	$(call test-scm,$(TEST),$(MIPS64),tmp/$(EXECUTABLE),mips64,release)
endif

# ----------------------------------------------------------------
# mips debug
tmp/%-mips-debug: CC=mips-linux-gnu-gcc
tmp/%-mips-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# mips release
tmp/%-mips-release: CC=mips-linux-gnu-gcc
tmp/%-mips-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# mips64 debug
tmp/%-mips64-debug: CC=mips64-linux-gnuabi64-gcc
tmp/%-mips64-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# mips64 release
tmp/%-mips64-release: CC=mips64-linux-gnuabi64-gcc
tmp/%-mips64-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_MIPS),11)
olvm-binaries: tmp/olvm-mips-debug
olvm-binaries: tmp/olvm-mips-release
endif

ifeq ($(DEV_MODE)$(HAVE_MIPS64),11)
olvm-binaries: tmp/olvm-mips64-debug
olvm-binaries: tmp/olvm-mips64-release
endif
