local:
	make -C ../.. libbt

libbt: $(wildcard extras/libbt/*.c) \
       $(wildcard extras/libbt/misc/*.c) \
       $(wildcard extras/libbt/evloop/*.c) \
	   $(wildcard extras/libbt/cli/*.c) \
       \
	   $(wildcard extras/libbt/boringssl/crypto/sha/*.cc) \
	   $(wildcard extras/libbt/boringssl/crypto/fipsmodule/sha/*.cc) \
	   \
	   src/olvm.c tmp/repl.c extensions/ffi.c
	$(CC) $^ -o $@ -Iextras/libbt/misc \
	               -Iextras/libbt/evloop \
	   -lm -lpthread \
	   -DHAVE_CLOCK_MONOTONIC=1 -DEVLOOP_POLL \
	   -DPACKAGE_NAME=\"btpd\" -DPACKAGE_VERSION=\"0.16\" \
	   -DOLVM_TARVENV=1 -DOLVM_TARVFOLDERS=1 \
	   -DREPL=repl -DOLVM_FFI=1 \
	   $(CFLAGS_DEBUG) $(L)
