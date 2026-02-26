# apt install qemu-system-sparc64 qemu-user

# apt install gcc-sparc64-linux-gnu
ifneq ($(shell command -v sparc64-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-sparc64 2>/dev/null),)
HAVE_SPARC64 ?= $(HAVE_PLATFORM)
endif
endif
HAVE_SPARC64 ?= 0
SPARC64 ?= qemu-sparc64 -L /usr/sparc64-linux-gnu

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-sparc64
test-matrix-header-sparc64:
	case "`expr $(HAVE_SPARC64)`" in \
		1) printf "| %-8s" 'sparc64';;\
	esac

test-matrix-subheader: test-matrix-subheader-sparc64
test-matrix-subheader-sparc64:
	if [ "$(HAVE_SPARC64)" = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-sparc64
scmtest-sparc64:
ifeq ($(DEV_MODE)$(HAVE_SPARC64),11)
	$(call test-scm,$(TEST),$(SPARC64),tmp/$(EXECUTABLE),sparc64,debug)
	$(call test-scm,$(TEST),$(SPARC64),tmp/$(EXECUTABLE),sparc64,release)
endif

# ----------------------------------------------------------------
# ppc64le debug
tmp/%-sparc64-debug: CC=sparc64-linux-gnu-gcc
tmp/%-sparc64-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# ppc64 release
tmp/%-sparc64-release: CC=sparc64-linux-gnu-gcc
tmp/%-sparc64-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_SPARC64),11)
olvm-binaries: tmp/olvm-sparc64-debug
olvm-binaries: tmp/olvm-sparc64-release
endif
