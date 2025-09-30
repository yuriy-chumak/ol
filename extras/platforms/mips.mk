# apt install qemu-system-mips, qemu-user

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
