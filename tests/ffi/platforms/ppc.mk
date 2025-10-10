ifeq ($(DEV_MODE)$(HAVE_MIPSEL),11)
ffi-binaries: tmp/ffi-ppc-debug
ffi-binaries: tmp/ffi-ppc-release
endif

ifeq ($(DEV_MODE)$(HAVE_MIPS64EL),11)
ffi-binaries: tmp/ffi-ppc64-debug
ffi-binaries: tmp/ffi-ppc64-release
endif
