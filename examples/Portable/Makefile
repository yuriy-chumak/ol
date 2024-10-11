all: ol

clean:
	rm -f ol ol.exe pvenv.tar

# Linux/macOS/etc.
../../ol:
	$(MAKE) -C ../.. ol

ol: ../../ol pvenv.tar
	cp $< $@
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	        ol $@
	@echo Ok.

# Windows
../../ol64.exe:
	$(MAKE) -C ../.. ol64.exe

ol.exe: ../../ol64.exe pvenv.tar
	cp $< $@
	x86_64-w64-mingw32-strip $<
	cat pvenv.tar >>$@

pvenv.tar:
	$(MAKE) -C ../.. tmp/pvenv.tar
	cp ../../tmp/pvenv.tar pvenv.tar
	# integrate "./main"
	tar -rf pvenv.tar --owner=OL --group=* --transform 's|.*|./main|' ./main.lisp
