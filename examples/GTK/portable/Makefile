all: portable

portable: ../../../olp pvenv.tar
	cp ../../../olp $@
	objcopy --remove-section    .tar $@
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	        $@ $@

../../../olp:
	$(MAKE) -C ../../../ olp

pvenv.tar: ../../../olp ui.glade gl-gtk-gladeui.lisp
	objcopy --dump-section .tar=pvenv.tar $<
	tar -rf $@ ./ui.glade # integrate any neede files
	tar -rf $@ gl-gtk-gladeui.lisp --transform 's|.*|.|' # this is our "autorun"

clean:
	rm pvenv.tar portable
