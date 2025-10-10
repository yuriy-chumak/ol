ifeq ($(DEV_MODE)$(HAVE_MINGW32),11)
ffi-binaries: tmp/ffi-win32-debug.exe
ffi-binaries: tmp/ffi-win32-release.exe
endif

ifeq ($(DEV_MODE)$(HAVE_MINGW64),11)
ffi-binaries: tmp/ffi-win64-debug.exe
ffi-binaries: tmp/ffi-win64-release.exe
endif
