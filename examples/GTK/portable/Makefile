all: ../../../ol pvenv.tar
	# enble feature and compile
	CFLAGS="-DOLVM_TARVENV=1" \
	$(MAKE) -C ../../.. -B
	# prepare tar portable venv
	objcopy --add-section       .tar=pvenv.tar \
	        --set-section-flags .tar=noload,readonly \
	        ../../../ol portable

pvenv.tar: main *.glade *.lisp
	tar -cvf $@ --owner=OL/2.5 --group= $(addprefix ./,$^) --directory=../../../libraries/ .
