# apt install qemu-system-mips qemu-user

# apt install gcc-mipsel-linux-gnu
ifneq ($(shell command -v mipsel-linux-gnu-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-mipsel 2>/dev/null),)
HAVE_MIPSEL ?= $(HAVE_PLATFORM)
endif
endif
HAVE_MIPSEL ?= 0
MIPSEL ?= qemu-mipsel -L /usr/mipsel-linux-gnu

# apt install gcc-mips64el-linux-gnuabi64
ifneq ($(shell command -v mips64el-linux-gnuabi64-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-mipsel 2>/dev/null),)
HAVE_MIPS64EL ?= $(HAVE_PLATFORM)
endif
endif
HAVE_MIPS64EL ?= 0
MIPS64EL ?= qemu-mips64el -L /usr/mips64el-linux-gnuabi64

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-mipsel
test-matrix-header-mipsel:
	case "`expr $(HAVE_MIPSEL) + $(HAVE_MIPS64EL)`" in \
		1) printf "| %-8s" 'mipsel';;\
		2) printf "| %-18s" 'mipsel';;\
	esac

test-matrix-subheader: test-matrix-subheader-mipsel
test-matrix-subheader-mipsel:
	if [ "$(HAVE_MIPSEL)"   = "1" ]; then printf "|32-d|32-r"; fi
	if [ "$(HAVE_MIPS64EL)" = "1" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-mipsel
scmtest-mipsel:
ifeq ($(DEV_MODE)$(HAVE_MIPSEL),11)
	$(call test-scm,$(TEST),$(MIPSEL),tmp/$(EXECUTABLE),mipsel,debug)
	$(call test-scm,$(TEST),$(MIPSEL),tmp/$(EXECUTABLE),mipsel,release)
endif
ifeq ($(DEV_MODE)$(HAVE_MIPS64EL),11)
	$(call test-scm,$(TEST),$(MIPS64EL),tmp/$(EXECUTABLE),mips64el,debug)
	$(call test-scm,$(TEST),$(MIPS64EL),tmp/$(EXECUTABLE),mips64el,release)
endif

# ----------------------------------------------------------------
# mipsel debug
tmp/%-mipsel-debug: CC=mipsel-linux-gnu-gcc
tmp/%-mipsel-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# mipsel release
tmp/%-mipsel-release: CC=mipsel-linux-gnu-gcc
tmp/%-mipsel-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# mips64el debug
tmp/%-mips64el-debug: CC=mips64el-linux-gnuabi64-gcc
tmp/%-mips64el-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT))

# mips64el release
tmp/%-mips64el-release: CC=mips64el-linux-gnuabi64-gcc
tmp/%-mips64el-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT))

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_MIPSEL),11)
olvm-binaries: tmp/olvm-mipsel-debug
olvm-binaries: tmp/olvm-mipsel-release
endif

ifeq ($(DEV_MODE)$(HAVE_MIPS64EL),11)
olvm-binaries: tmp/olvm-mips64el-debug
olvm-binaries: tmp/olvm-mips64el-release
endif
