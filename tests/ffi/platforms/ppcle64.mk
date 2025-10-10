ifeq ($(DEV_MODE)$(HAVE_PPC64LE),11)
ffi-binaries: tmp/ffi-ppc64le-debug
ffi-binaries: tmp/ffi-ppc64le-release
endif
