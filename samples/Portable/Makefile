all: ol

clean:
	rm -f ol ol.exe pvenv.tar

# Linux/macOS/etc.
../../ol:
	rm -f $@
	CFLAGS="-DOLVM_TARVENV=1" \
	$(MAKE) -C ../.. ol

# Win64
../../ol64.exe:
	rm -f $@
	MINGWCFLAGS="-DOLVM_TARVENV=1" \
	$(MAKE) -C ../.. ol64.exe

ol: ../../ol pvenv.tar
	$(MAKE) $< -B
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	        ../../ol $@

ol.exe: ../../ol64.exe pvenv.tar
	$(MAKE) $< -B
	x86_64-w64-mingw32-strip $<
	cat $^ >$@

pvenv.tar:
	tar -cvf pvenv.tar --owner=OL/2.5 --group= $(addprefix ./,$^) \
	    ./main \
	    --directory=../../libraries .
