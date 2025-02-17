libbt: $(wildcard extras/libbt/*.c) \
       $(wildcard extras/libbt/misc/*.c) \
       $(wildcard extras/libbt/evloop/*.c)
	$(CC) $^ -o $@ -Iextras/libbt/misc \
	               -Iextras/libbt/evloop \
	   -lcrypto -lm -lpthread \
	   -DHAVE_CLOCK_MONOTONIC=1 -DEVLOOP_POLL \
	   -DPACKAGE_NAME=\"btpd\" -DPACKAGE_VERSION=\"0.16\"
