# apt install qemu-system-mips, qemu-user

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
