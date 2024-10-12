ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# default is a 64-bit build
ol-uname.dll: MGCC?=$(MGCC64)

ol-uname.dll: MINGWCFLAGS += -std=gnu99 -fno-exceptions
ol-uname.dll: MINGWCFLAGS += -Wno-shift-count-overflow
ol-uname.dll: MINGWCFLAGS += $(CFLAGS_RELEASE)
ol-uname.dll: extensions/win32/uname.c
	$(MGCC) $^ -o $@ -shared \
	   $(MINGWCFLAGS)

endif
