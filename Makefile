DESTDIR=
PREFIX=/usr
BINDIR=/bin
INSTALL=install

CFLAGS=-Wall -g -mno-sse
# -O2
#CC=gcc

owl: fasl/ol.fasl

## fasl (plain bytecode) image boostrap

fasl/boot.fasl: fasl/init.fasl
	# start bootstrapping with the bundled init.fasl image
	echo "cp fasl/init.fasl -> fasl/boot.fasl"
	cp fasl/init.fasl fasl/boot.fasl

fasl/ol.fasl: fasl/boot.fasl owl/*.scm scheme/*.scm
	# selfcompile boot.fasl until a fixed point is reached
	toolchain/vm fasl/boot.fasl --run owl/ol.scm -s none -o fasl/compiled.fasl
	ls -la fasl/compiled.fasl
	# check that the new image passes tests
	#sh tests/run all toolchain/vm fasl/compiled.fasl
	# copy new image to ol.fasl if it is a fixed point, otherwise recompile
	diff -q fasl/boot.fasl fasl/compiled.fasl && mv fasl/compiled.fasl fasl/ol.fasl || mv fasl/compiled.fasl fasl/boot.fasl && make fasl/ol.fasl
#	cp fasl/compiled.fasl fasl/boot.fasl
#	mv fasl/compiled.fasl fasl/ol.fasl

## building just the virtual machine to run fasl images

#c/vm.c: c/ovm.c
#	# make a vm without a bundled heap
#	echo "unsigned char *heap = 0;" > c/vm.c
#	cat c/ovm.c >> c/vm.c


## building standalone image out of the fixed point fasl image

#c/ol.c: fasl/ol.fasl
#	# compile the repl using the fixed point image 
#	bin/vm fasl/ol.fasl --run owl/ol.scm -s some -o c/ol.c

#bin/ol.exe: c/ol.c
#	# compile the real owl repl binary
#	$(CC) $(CFLAGS) -o bin/olp.exe c/ol.c lib/libws2_32.a
#	#tests/run all bin/olp
#	test -f bin/ol && mv bin/ol bin/ol-old || true
#	mv bin/olp bin/ol


## data 

owl/unicode-char-folds.scm:
	echo "(define char-folds '(" > owl/unicode-char-folds.scm
	curl http://www.unicode.org/Public/6.0.0/ucd/CaseFolding.txt | grep "[0-9A-F]* [SFC]; " | sed -re 's/ #.*//' -e 's/( [SFC])?;//g' -e 's/^/ /' -e 's/ / #x/g' -e 's/ /(/' -e 's/$$/)/' | tr "[A-F]" "[a-f]" >> owl/unicode-char-folds.scm
	echo "))" >> owl/unicode-char-folds.scm

clean:
	-rm fasl/boot.fasl fasl/compiled.fasl fasl/ol.fasl
	-rm c/vm.c c/ol.c
	-rm bin/ol bin/vm

# make a standalone binary against dietlibc for relase
standalone:
	-rm bin/ol.exe
	make CFLAGS="-O2 -DNO_SECCOMP" CC="diet gcc" bin/ol.exe

fasl-update: fasl/ol.fasl
	cp fasl/ol.fasl fasl/init.fasl

todo: bin/vm.exe
	bin/vm fasl/ol.fasl -n owl/*.scm | less

.PHONY: todo test fasltest owl standalone
