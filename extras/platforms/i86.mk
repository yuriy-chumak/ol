# apt install gcc-multilib

# x86 linux
ifeq ($(call exists,-m32,sys/cdefs.h,exit),1)
HAVE_X86 ?= $(HAVE_PLATFORM)
endif
HAVE_X86 ?= 0

# x86_64 linux
ifeq ($(call exists,-m64,sys/cdefs.h,exit),1)
HAVE_X86_64 ?= $(HAVE_PLATFORM)
endif
HAVE_X86_64 ?= 0

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-x86
test-matrix-header-x86:
	case "`expr $(HAVE_X86) + $(HAVE_X86_64) `" in \
		1) printf "| %-8s" `uname -m`;;\
		2) printf "| %-18s" `uname -m`;;\
	esac

test-matrix-subheader: test-matrix-subheader-x86
test-matrix-subheader-x86:
	if [ "$(HAVE_X86)"    = "1" ]; then printf "|32-d|32-r"; fi
	if [ "$(HAVE_X86_64)" = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-i86
scmtest-i86:
# i386
ifeq ($(DEV_MODE)$(HAVE_X86),11)
	$(call test-scm,$(TEST),,tmp/$(EXECUTABLE),x86,debug)
	$(call test-scm,$(TEST),,tmp/$(EXECUTABLE),x86,release)
endif
# x86_64
ifeq ($(DEV_MODE)$(HAVE_X86_64),11)
	$(call test-scm,$(TEST),,tmp/$(EXECUTABLE),x86_64,debug)
	$(call test-scm,$(TEST),,tmp/$(EXECUTABLE),x86_64,release)
endif

# ----------------------------------------------------------------
# x86 debug
tmp/%-x86-debug: CC=gcc
tmp/%-x86-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT) -m32)

# x86 release
tmp/%-x86-release: CC=gcc
tmp/%-x86-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT) -m32)

# x86_64 debug
tmp/%-x86_64-debug: CC=gcc
tmp/%-x86_64-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# x86_64 release
tmp/%-x86_64-release: CC=gcc
tmp/%-x86_64-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_X86),11)
olvm-binaries: tmp/olvm-x86-debug
olvm-binaries: tmp/olvm-x86-release
endif

ifeq ($(DEV_MODE)$(HAVE_X86_64),11)
olvm-binaries: tmp/olvm-x86_64-debug
olvm-binaries: tmp/olvm-x86_64-release
endif
