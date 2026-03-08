# apt install qemu-system-alpha qemu-user

# apt install gcc-alpha-linux-gnu
ifneq ($(shell command -v alpha-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-alpha 2>/dev/null),)
HAVE_ALPHA ?= $(HAVE_PLATFORM)
endif
endif
HAVE_ALPHA ?= 0
ALPHA ?= qemu-alpha -L /usr/alpha-linux-gnu

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-alpha
test-matrix-header-alpha:
	case "`expr $(HAVE_ALPHA)`" in \
		1) printf "| %-8s" 'alpha';;\
	esac

test-matrix-subheader: test-matrix-subheader-alpha
test-matrix-subheader-alpha:
	if [ "$(HAVE_ALPHA)" = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-alpha
scmtest-alpha:
ifeq ($(DEV_MODE)$(HAVE_ALPHA),11)
	$(call test-scm,$(TEST),$(ALPHA),tmp/$(EXECUTABLE),alpha,debug)
	$(call test-scm,$(TEST),$(ALPHA),tmp/$(EXECUTABLE),alpha,release)
endif

# ----------------------------------------------------------------
# debug
tmp/%-alpha-debug: CC=alpha-linux-gnu-gcc
tmp/%-alpha-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# release
tmp/%-alpha-release: CC=alpha-linux-gnu-gcc
tmp/%-alpha-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_ALPHA),11)
olvm-binaries: tmp/olvm-alpha-debug
olvm-binaries: tmp/olvm-alpha-release
endif
