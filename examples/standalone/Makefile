all: standalone

clean:
	rm -f standalone pvenv.tar

# just ensure main binary is built
../../olp:
	$(MAKE) -C ../.. olp


# cooking
standalone: ../../olp pvenv.tar
	cp $< $@
	objcopy --remove-section    .tar $@
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	   $@ $@
	@echo Ok.

pvenv.tar: ../../olp run.scm
	objcopy --dump-section .tar=pvenv.tar $<
	tar -rf $@ run.scm --transform 's|.*|.|'
