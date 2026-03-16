ifeq ($(DEV_MODE)$(HAVE_RISCV64),11)
ffi-binaries: tmp/ffi-riscv64-debug
ffi-binaries: tmp/ffi-riscv64-release
endif
