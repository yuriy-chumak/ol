all: ../../../ol pvenv.tar
	$(MAKE) -C ../../../ol
	# prepare tar portable venv
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	        ../../../ol portable

pvenv.tar: main *.glade *.lisp
	tar -cvf $@ --owner=OL/2.5 --group= $(addprefix ./,$^) --directory=../../../libraries/ .
