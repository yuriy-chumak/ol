ifeq ($(DEV_MODE)$(HAVE_X86),11)
ffi-binaries: tmp/ffi-x86-debug
ffi-binaries: tmp/ffi-x86-release
endif

ifeq ($(DEV_MODE)$(HAVE_X86_64),11)
ffi-binaries: tmp/ffi-x86_64-debug
ffi-binaries: tmp/ffi-x86_64-release
endif
