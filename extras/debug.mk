# DEBUG binaries

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

ffi: CFLAGS += $(CFLAGS_TRACE)
ffi: src/olvm.c extensions/ffi.c tests/ffi/src.c tests/ffi/src.inc
	$(CC) $(filter %.c,$^) -o $@ \
	   -Iincludes \
	   src/repl.S -DREPL=repl \
	   $(CFLAGS) $(L)
	@echo Ok.

ffi32: CFLAGS += $(CFLAGS_TRACE) -m32
ffi32: src/olvm.c extensions/ffi.c tests/ffi.c
	$(CC) src/olvm.c -o $@ \
	   extensions/ffi.c -Iincludes \
	   tests/ffi.c \
	   $(CFLAGS) $(L)
	@echo Ok.

# win
ffi32.exe:
ffi32.exe: src/olvm.c extensions/ffi.c tests/ffi.c src/repl.S
	$(MGCC32) $^ -o $@ \
	   -DREPL=repl \
	   -DHAVE_DLOPEN=1 -DHAS_SOCKES=1 -DOLVM_FFI=1 \
	   -Iincludes/win32 -Iincludes \
	   -Wno-int-to-pointer-cast \
	   $(MINGWCFLAGS) -lws2_32

ffi64.exe:
ffi64.exe: src/olvm.c extensions/ffi.c tests/ffi.c src/repl.S
	$(MGCC64) $^ -o $@ \
	   -DREPL=repl \
	   -DHAVE_DLOPEN=1 -DHAS_SOCKES=1 -DOLVM_FFI=1 \
	   -Iincludes/win32 -Iincludes \
	   $(MINGWCFLAGS) -lws2_32

endif