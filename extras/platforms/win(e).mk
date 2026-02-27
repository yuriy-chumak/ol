# dpkg --add-architecture i386 && apt-get update

# i386 win32
# apt install gcc-mingw-w64-i686
ifneq ($(shell command -v $(MGCC32) 2>/dev/null),)
HAVE_MINGW32 ?= $(HAVE_PLATFORM)
endif
HAVE_MINGW32 ?= 0

# x86_64 win64
# apt install gcc-mingw-w64-x86-64
ifneq ($(shell command -v $(MGCC64) 2>/dev/null),)
HAVE_MINGW64 ?= $(HAVE_PLATFORM)
endif
HAVE_MINGW64 ?= 0

# apt install wine32
# note: if you need to disable com ports under wine, then navigate to
#       HKLM\Software\Wine and create a new empty String named 'com33' (or smth)
ifneq ($(shell command -v wine 2>/dev/null),)
HAVE_WINE ?= $(HAVE_PLATFORM)
endif
HAVE_WINE ?= 0

# wine is not required under WSL, so:
ifdef WSL_DISTRO_NAME
WINE ?=
else  # disable any wine logging
WINE ?= WINEDEBUG=-all wine cmd /c 
endif

# ----------------------------------------------------------------
test-matrix-header: test-matrix-header-win(e)
test-matrix-header-win(e):
	case "`expr \( $(HAVE_MINGW32) + $(HAVE_MINGW64) \) \* $(HAVE_WINE)`" in \
		1) printf "| | windows ";;\
		2) printf "| | windows           ";;\
	esac

test-matrix-subheader: test-matrix-subheader-win(e)
test-matrix-subheader-win(e):
	if [ "$(DEV_MODE)$(HAVE_WINE)"     = "11" ]; then printf "| "; fi
	if [ "$(HAVE_MINGW32)$(HAVE_WINE)" = "11" ]; then printf "|32-d|32-r"; fi
	if [ "$(HAVE_MINGW64)$(HAVE_WINE)" = "11" ]; then printf "|64-d|64-r"; fi


scmtest: scmtest-win(e)
scmtest-win(e): OS:="Win\(e\)"
scmtest-win(e):
# win(e)
ifeq ($(DEV_MODE)$(HAVE_WINE),11)
	printf "| "
endif
# win32
ifeq ($(DEV_MODE)$(HAVE_MINGW32)$(HAVE_WINE),111)
	$(call test-scm,$(TEST),$(WINE),tmp/$(EXECUTABLE),win32,debug,.exe,--strip-trailing-cr)
	$(call test-scm,$(TEST),$(WINE),tmp/$(EXECUTABLE),win32,release,.exe,--strip-trailing-cr)
endif
# win64
ifeq ($(DEV_MODE)$(HAVE_MINGW64)$(HAVE_WINE),111)
	$(call test-scm,$(TEST),$(WINE),tmp/$(EXECUTABLE),win64,debug,.exe,--strip-trailing-cr)
	$(call test-scm,$(TEST),$(WINE),tmp/$(EXECUTABLE),win64,release,.exe,--strip-trailing-cr)
endif

# ----------------------------------------------------------------
# 32-bit debug.exe
tmp/%-win32-debug.exe: CC=$(MGCC32)
tmp/%-win32-debug.exe: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) -Iincludes/win32 -lws2_32)

# 32-bit release.exe
tmp/%-win32-release.exe: CC=$(MGCC32)
tmp/%-win32-release.exe: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) -Iincludes/win32 -lws2_32)

# 64-bit debug
tmp/%-win64-debug.exe: CC=$(MGCC64)
tmp/%-win64-debug.exe: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_DEBUG) -Iincludes/win32 -lws2_32)

# 64-bit release
tmp/%-win64-release.exe: CC=$(MGCC32)
tmp/%-win64-release.exe: $(TEST_DEPS)
	$(call build-olvm,$@,$(TEST_CFLAGS_RELEASE) -Iincludes/win32 -lws2_32)

# ----------------------------------------------------------------
ifeq ($(DEV_MODE)$(HAVE_MINGW32),11)
olvm-binaries: tmp/olvm-win32-debug.exe
olvm-binaries: tmp/olvm-win32-release.exe
endif

ifeq ($(DEV_MODE)$(HAVE_MINGW64),11)
olvm-binaries: tmp/olvm-win64-debug.exe
olvm-binaries: tmp/olvm-win64-release.exe
endif
