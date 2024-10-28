all: out/ol

clean:
	rm -f out/ol pvenv.tar

# just ensure main binary is built
../../olp:
	$(MAKE) -C ../.. olp


# cooking
out/ol: ../../olp pvenv.tar
	mkdir -p out
	cp $< $@
	objcopy --remove-section    .tar $@
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	   $@ $@
	@echo Ok.

pvenv.tar: ../../olp autorun.lisp
	objcopy --dump-section .tar=pvenv.tar $<
	tar -rf $@ autorun.lisp --transform 's|.*|.|'
