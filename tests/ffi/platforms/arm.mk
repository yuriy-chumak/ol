ifeq ($(DEV_MODE)$(HAVE_ARM4),11)
ffi-binaries: tmp/ffi-armv4t-debug
ffi-binaries: tmp/ffi-armv4t-release
endif

ifeq ($(DEV_MODE)$(HAVE_ARM7),11)
ffi-binaries: tmp/ffi-armv7soft-debug
ffi-binaries: tmp/ffi-armv7soft-release
ffi-binaries: tmp/ffi-armv7hard-debug
ffi-binaries: tmp/ffi-armv7hard-release
endif
