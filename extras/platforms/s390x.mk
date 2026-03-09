# apt install qemu-system-s390x qemu-user

# apt install gcc-s390x-linux-gnu
ifneq ($(shell command -v s390x-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-s390x 2>/dev/null),)
HAVE_S390X ?= $(HAVE_PLATFORM)
endif
endif
HAVE_S390X ?= 0
S390X ?= qemu-s390x -L /usr/s390x-linux-gnu

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-s390x
test-matrix-header-s390x:
	case "`expr $(HAVE_S390X)`" in \
		1) printf "| %-8s" 's390x';;\
	esac

test-matrix-subheader: test-matrix-subheader-s390x
test-matrix-subheader-s390x:
	if [ "$(HAVE_S390X)" = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-s390x
scmtest-s390x:
ifeq ($(DEV_MODE)$(HAVE_S390X),11)
	$(call test-scm,$(TEST),$(S390X),tmp/$(EXECUTABLE),s390x,debug)
	$(call test-scm,$(TEST),$(S390X),tmp/$(EXECUTABLE),s390x,release)
endif

# ----------------------------------------------------------------
# debug
tmp/%-s390x-debug: CC=s390x-linux-gnu-gcc
tmp/%-s390x-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# release
tmp/%-s390x-release: CC=s390x-linux-gnu-gcc
tmp/%-s390x-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_S390X),11)
olvm-binaries: tmp/olvm-s390x-debug
olvm-binaries: tmp/olvm-s390x-release
endif
