ifeq ($(DEV_MODE)$(HAVE_S390X),11)
ffi-binaries: tmp/ffi-s390x-debug
ffi-binaries: tmp/ffi-s390x-release
endif
