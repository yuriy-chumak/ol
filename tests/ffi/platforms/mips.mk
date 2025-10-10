ifeq ($(DEV_MODE)$(HAVE_MIPS),11)
ffi-binaries: tmp/ffi-mips-debug
ffi-binaries: tmp/ffi-mips-release
endif

ifeq ($(DEV_MODE)$(HAVE_MIPS64),11)
ffi-binaries: tmp/ffi-mips64-debug
ffi-binaries: tmp/ffi-mips64-release
endif
