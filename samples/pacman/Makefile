all: pacman1 pacman2 pacman3
clean:
	rm -f pacman1 pacman2 pacman3 \
	      tmp.bin binf.c repl.o

repl.o: ../../repl
	ld -r -b binary -o repl.o ../../repl

# OLVM_NOMAIN disables 'main' function of ol module

pacman1: main.c embed1.c texturing.c repl.o
	gcc $^ -I. -lGL -lglut -lpng \
	    -I../../includes -DOLVM_NOMAIN $(CFLAGS) \
	    -DREPL=_binary_______repl_start \
	    ../../src/olvm.c -ldl -lm -pthread \
	    -o $@

pacman2: main.c embed2.c texturing.c repl.o
	gcc $^ -I. -lGL -lglut -lpng \
	    -I../../includes -DOLVM_NOMAIN $(CFLAGS) \
	    -DREPL=_binary_______repl_start \
	    ../../src/olvm.c -ldl -lm -pthread \
	    -o $@

pacman3: main.c embed3.c texturing.c binf.c
	gcc $^ -I. -lGL -lglut -lpng \
	    -I../../includes -DOLVM_NOMAIN $(CFLAGS) \
	    ../../src/olvm.c -ldl -lm -pthread \
	    -o $@

tmp.bin: precompile.scm main.scm
	ol precompile.scm
binf.c: tmp.bin
	xxd --include tmp.bin >binf.c
