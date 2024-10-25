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
	CFLAGS="-DOLVM_TARVENV=1 -DOLVM_TARVFOLDERS=1" \
	   $(MAKE) -B release -C ../..
	
	mkdir -p out
	cp $< $@
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	   $@ $@
	@echo Ok.

pvenv.tar: ../../tmp/pvenv.tar autorun.lisp
	cp $< $@
	# integrate "program.lisp" as "."
	tar -rf $@ --owner=OL/2 --group=* \
	           --transform 's|.*|.|' \
	   autorun.lisp
