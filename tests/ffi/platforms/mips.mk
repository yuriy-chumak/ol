ifeq ($(DEV_MODE)$(HAVE_MIPS),11)
ffi-binaries: tmp/ffi-mips-debug
ffi-binaries: tmp/ffi-mips-release
endif

ifeq ($(DEV_MODE)$(HAVE_MIPS64),11)
ffi-binaries: tmp/ffi-mips64-debug
ffi-binaries: tmp/ffi-mips64-release
endif

tmp/ffi-mips-debug: $(TEST_DEPS)
tmp/ffi-mips-release: $(TEST_DEPS)
tmp/ffi-mips64-debug: $(TEST_DEPS)
tmp/ffi-mips64-release: $(TEST_DEPS)
