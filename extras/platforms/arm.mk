# apt install qemu-system-arm qemu-user

# apt install gcc-arm-linux-gnueabi gcc-arm-linux-gnueabihf
ifneq ($(shell command -v arm-linux-gnueabi-gcc 2>/dev/null),)
ifneq ($(shell command -v qemu-arm 2>/dev/null),)
HAVE_ARM4 ?= $(HAVE_PLATFORM)
HAVE_ARM7 ?= $(HAVE_PLATFORM)
endif
endif
HAVE_ARM4 ?= 0
HAVE_ARM7 ?= 0

ARM4t ?= qemu-arm -L /usr/arm-linux-gnueabi -cpu arm926
ARM7soft ?= qemu-arm -L /usr/arm-linux-gnueabi -cpu cortex-a8,vfp=off
ARM7hard?= qemu-arm -L /usr/arm-linux-gnueabihf -cpu cortex-a9

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-arm
test-matrix-header-arm:
	case "`expr $(HAVE_ARM4) + $(HAVE_ARM7) + $(HAVE_ARM7)`" in \
		1) printf "| arm v4t ";;\
		2) printf "| v7 soft : v7 hard ";;\
		3) printf "| arm v4t : v7 soft : v7 hard ";;\
	esac

test-matrix-subheader: test-matrix-subheader-arm
test-matrix-subheader-arm:
	if [ "$(HAVE_ARM4)"  = "1" ]; then printf "|32-d:32-r"; fi
	if [ "$(HAVE_ARM7)"  = "1" ]; then printf "|32-d:32-r|32-d:32-r"; fi

scmtest: scmtest-arm
scmtest-arm:
ifeq ($(DEV_MODE)$(HAVE_ARM4),11)
	$(call test-scm,$(TEST),$(ARM4t),tmp/$(EXECUTABLE),armv4t,debug)
	$(call test-scm,$(TEST),$(ARM4t),tmp/$(EXECUTABLE),armv4t,release)
endif
ifeq ($(DEV_MODE)$(HAVE_ARM7),11)
	$(call test-scm,$(TEST),$(ARM7soft),tmp/$(EXECUTABLE),armv7soft,debug)
	$(call test-scm,$(TEST),$(ARM7soft),tmp/$(EXECUTABLE),armv7soft,release)
	$(call test-scm,$(TEST),$(ARM7hard),tmp/$(EXECUTABLE),armv7hard,debug)
	$(call test-scm,$(TEST),$(ARM7hard),tmp/$(EXECUTABLE),armv7hard,release)
endif

# ----------------------------------------------------------------
tmp/%-armv4t-debug: CC=arm-linux-gnueabi-gcc
tmp/%-armv4t-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT) -march=armv4t)

tmp/%-armv4t-release: CC=arm-linux-gnueabi-gcc
tmp/%-armv4t-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT) -march=armv4t)

# v7 soft
tmp/%-armv7soft-debug: CC=arm-linux-gnueabi-gcc
tmp/%-armv7soft-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT) -march=armv7-a -mfloat-abi=soft)

tmp/%-armv7soft-release: CC=arm-linux-gnueabi-gcc
tmp/%-armv7soft-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT) -march=armv7-a -mfloat-abi=soft)

# v7 hard
tmp/%-armv7hard-debug: CC=arm-linux-gnueabihf-gcc
tmp/%-armv7hard-debug: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) $(OLVM_EXPORT) -march=armv7-a -mfloat-abi=hard -mfpu=vfpv3-d16)

tmp/%-armv7hard-release: CC=arm-linux-gnueabihf-gcc
tmp/%-armv7hard-release: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) $(OLVM_EXPORT) -march=armv7-a -mfloat-abi=hard -mfpu=vfpv3-d16)

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_ARM4),11)
olvm-binaries: tmp/olvm-armv4t-debug
olvm-binaries: tmp/olvm-armv4t-release
endif
ifeq ($(DEV_MODE)$(HAVE_ARM7),11)
olvm-binaries: tmp/olvm-armv7soft-debug
olvm-binaries: tmp/olvm-armv7soft-release
olvm-binaries: tmp/olvm-armv7hard-debug
olvm-binaries: tmp/olvm-armv7hard-release
endif

# 1. -march=armv4t (самый голый)                                    : -march=armv4t -mfloat-abi=soft
# 2. -mfloat-abi=soft + ARMv7 (базовая математика через libgcc)     : -march=armv7-a -mfloat-abi=soft
# 3. -mfloat-abi=hard + ARMv7 (железная математика), __ARM_PCS_VFP  : -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard
