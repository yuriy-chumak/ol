all: ol gol

clean:
	rm -f ol ol.exe pvenv.tar

# just ensure main binaries are built
../../ol:
	$(MAKE) -C ../.. ol

../../tmp/pvenv.tar:
	$(MAKE) -C ../.. tmp/pvenv.tar

../../ol64.exe:
	$(MAKE) -C ../.. ol64.exe


# Linux/Unix/macOS
ol: ../../ol ol-pvenv.tar
	cp $< $@
	objcopy --add-section       .tar=ol-pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	        ol $@
	@echo Ok.

ol-pvenv.tar: ../../tmp/pvenv.tar usage.lisp
	cp $< $@
	# integrate "usage.lisp" as "./main"
	tar -rf $@ --owner=OL \
                   --group=* \
                   --transform 's|.*|./main|' \
            usage.lisp

gol: ../../ol gol-pvenv.tar
	cp $< $@
	objcopy --add-section       .tar=gol-pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	        ol $@
	@echo Ok.

gol-pvenv.tar: ../../tmp/pvenv.tar gtk-gl.lisp
	cp $< $@
	# integrate "usage.lisp" as "./main"
	tar -rf $@ --owner=OL \
                   --group=* \
                   --transform 's|.*|./main|' \
            gtk-gl.lisp

# Windows

ol.exe: ../../ol64.exe pvenv.tar
	cp $< $@
	x86_64-w64-mingw32-strip $<
	cat pvenv.tar >>$@

