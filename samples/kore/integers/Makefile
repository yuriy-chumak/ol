all: src/tmp.c tmp.bin
	kodev run

tmp.bin: integers.scm
	ol integers.scm
src/tmp.c: tmp.bin
	xxd --include tmp.bin >src/tmp.c
clean:
	rm -rf .objs/
