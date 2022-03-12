all: src/ol.c tmp.bin
	kodev run

tmp.bin: integers.scm
	ol integers.scm
src/ol.c: tmp.bin
	@xxd --include tmp.bin >src/ol.c
	@echo '#define OLVM_NOMAIN 1'
	@echo '#include "../../../../src/olvm.c"' >>src/ol.c
	@echo '#include "../../../../extensions/ffi.c"' >>src/ol.c
	@echo ok
clean:
	rm -rf .objs/
	rm integers.so
