ifeq ($(DEV_MODE)$(HAVE_AARCH64),11)
ffi-binaries: tmp/ffi-aarch64-debug
ffi-binaries: tmp/ffi-aarch64-release
endif
