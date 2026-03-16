ifeq ($(DEV_MODE)$(HAVE_SPARC64),11)
ffi-binaries: tmp/ffi-sparc64-debug
ffi-binaries: tmp/ffi-sparc64-release
endif
