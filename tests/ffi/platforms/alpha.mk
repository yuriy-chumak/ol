ifeq ($(DEV_MODE)$(HAVE_ALPHA),11)
ffi-binaries: tmp/ffi-alpha-debug
ffi-binaries: tmp/ffi-alpha-release
endif
