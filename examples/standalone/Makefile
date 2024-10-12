all: out/ol

clean:
	rm -f out/ol pvenv.tar

# just ensure main binaries are built
../../ol:
	$(MAKE) -C ../.. ol

../../tmp/pvenv.tar:
	$(MAKE) -C ../.. tmp/pvenv.tar


# cooking
out/ol: ../../ol pvenv.tar
	mkdir -p out
	cp $< $@
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	   $@ $@
	@echo Ok.

pvenv.tar: ../../tmp/pvenv.tar program.lisp
	cp $< $@
	# integrate "program.lisp" as "./main"
	tar -rf $@ --owner=OL --group=* \
	           --transform 's|.*|./main|' \
	   program.lisp
