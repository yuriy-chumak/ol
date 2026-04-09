all: pacman1 pacman2 pacman3 pacman3s
clean:
	rm -f pacman1 pacman2 pacman3 pacman3s \
	      embed3.bin embed3.inc repl.o

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

pacman3: main.c embed3.c texturing.c embed3.inc
	gcc main.c embed3.c texturing.c \
	    -I. -lGL -lglut -lpng \
	    -I../../includes -DOLVM_NOMAIN $(CFLAGS) \
	    ../../src/olvm.c -ldl -lm -pthread \
	    -o $@

pacman3s: main.c embed3.c texturing.c embed3.inc
	gcc main.c embed3.c texturing.c \
	    -I. -lGL -lglut -lpng \
	    -I../../includes -DOLVM_NOMAIN $(CFLAGS) \
	    -lol -ldl -lm -pthread \
	    -o $@

embed3.bin: embed3.scm main.scm
	ol --compile --entry $< >$@
embed3.inc: embed3.bin
	xxd --include $^ >$@
