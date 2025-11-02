ifeq ($(DEV_MODE)$(HAVE_MINGW32),11)
ffi-binaries: tmp/ffi-win32-debug.exe
ffi-binaries: tmp/ffi-win32-release.exe
endif

ifeq ($(DEV_MODE)$(HAVE_MINGW64),11)
ffi-binaries: tmp/ffi-win64-debug.exe
ffi-binaries: tmp/ffi-win64-release.exe
endif

# win(e) deps
tmp/ffi-%-debug.exe: TEST_DEPS += $(FFI_DEPS)
tmp/ffi-%-debug.exe: TEST_CFLAGS_DEBUG += tests/ffi/src.c
tmp/ffi-%-debug.exe: tests/ffi/src.c tests/ffi/src.inc src/olvm.c extensions/ffi.c

tmp/ffi-%-release.exe: TEST_DEPS += $(FFI_DEPS)
tmp/ffi-%-release.exe: TEST_CFLAGS_RELEASE += tests/ffi/src.c
