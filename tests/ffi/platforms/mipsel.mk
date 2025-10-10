ifeq ($(DEV_MODE)$(HAVE_MIPSEL),11)
ffi-binaries: tmp/ffi-mipsel-debug
ffi-binaries: tmp/ffi-mipsel-release
endif

ifeq ($(DEV_MODE)$(HAVE_MIPS64EL),11)
ffi-binaries: tmp/ffi-mips64el-debug
ffi-binaries: tmp/ffi-mips64el-release
endif
