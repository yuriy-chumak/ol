EMBEDDING OL
============

Otus Lisp, an extensible embedded language.

## Simplest example

```c
#include <ol/ol.h>
int main(int argc, char **argv)
{
	ol_t ol;
	OL_new(&ol, NULL);
	OL_eval(&ol, new_string(&ol, "(print \"hello\")"), 0);
	OL_delete(&ol);
}
```
```shell
$ gcc file-above.c -lol -o main -O0 -g3
$ ./main
hello
```

## Complex example
This folder demonstrates a usage of Otus Lisp as part of a pacman-like game.

The A* path searching algorithm and Blinky state completely implemented in Otus Lisp (main.scm file). The OpenGL rendering and keyboard processing implemented in C (main.c file).

![Screenshot.](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/pacman.png "screenshot")

Files:
* **main.c** - main "C" program with opengl, keyboard and win/lose logic.
* **main.scm** - main "Lisp" program module with level and A* blinky brains.
* **Makefile** - build script for all three embedding options.
* **resources/*.png** - game resources (background, sprites, etc.).
* **texturing.c** - png texture reader.
* **embed1.c** - embedding option 1, just a way.
* **embed2.c** - embedding option 2, simpler way with few oddish C-macro.
* **embed3.c** - embedding option 3, fastest and smallest way with precompiled lisp code (required deeper knowledge of otus lisp language).
* **precompile.scm** - lisp compiler for option 3.

Temporary files:
* **repl.o** - Otus Lisp REPL binary (400 KB).
* **tmp.bin** - precompiled by precompile.scm game lisp binary (64 KB).
* **binf.c** - tmp.bin, converted into unsigned char* variable (using xxd tool) (400 KB) to be included in embed3.c.

Output files:
* **pacman1** - compiled embedding option 1 (consumes about 3 MB of memory).
* **pacman2** - compiled embedding option 2 (consumes same as pacman1).
* **pacman3** - compiled embedding option 3 (consumes only 200 KB of memory and much more faster than pacman1 and pacman2).

IN DETAILS
----------

Please read the
[embed1.c](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/embed1.c),
[embed2.c](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/embed2.c), and
[embed3.c](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/embed3.c) complete source codes for a better usage understanding.

COMPARISON
----------

Just compare all three embedding options usage.

## embedding option 1

Basic library usage:
```c
// olvm:
ol_t ol;

extern unsigned char REPL[];
void ol_new_ol()
{
	OL_new(&ol, REPL);
	
	OL_eval(&ol, new_string(&ol, "(import (main))"), 0);
	OL_eval(&ol, new_string(&ol, "(born-blinky)"), 0);
}

...

void ol_get_blinky(int* x, int* y)
{
	uintptr_t xy = OL_eval(&ol,
		new_string(&ol, "(get-blinky)"),
		0);
	assert (is_pair(xy));

	*x = ol2int(car(xy));
	*y = ol2int(cdr(xy));
}
...
void ol_blinky_move(int x, int y)
{
	OL_eval(&ol,
		new_string(&ol, "blinky-move"),
		make_integer(x),
		make_integer(y),
		0);
}

```

## embedding option 2

Smart macro, simpler usage:
```c
// olvm:
ol_t ol;

extern unsigned char REPL[];
void ol_new_ol()
{
	OL_new(&ol, REPL);
	eval("(import (main)) (born-blinky)");
}

...

void ol_get_blinky(int* x, int* y)
{
	uintptr_t xy = eval("(get-blinky)");
	assert (is_pair(xy));

	*x = ol2int(car(xy));
	*y = ol2int(cdr(xy));
}
...
void ol_blinky_move(int x, int y)
{
	eval("blinky-move", x, y);
}

```

## embedding option 3

Precompiled ol code, fastest (near native) execution:
```c
// olvm:
struct olvm_t* vm;
int get_blinky;
int blinky_move;

extern unsigned char tmp_bin[];
void ol_new_ol()
{
	vm = OLVM_new(tmp_bin);
	OLVM_userdata(vm, &vm);

	uintptr_t
	r = OLVM_run(vm, 0, 0);
	// well, we have our "smart" script prepared,
	//  now save functions for feature use
	assert (is_vector(r));

  ...
	get_blinky = ol2int(ref(r, 2));
  ...
	blinky_move = ol2int(ref(r, 6));
  ...
}

...

void ol_get_blinky(int* x, int* y)
{
	uintptr_t xy = OLVM_evaluate(vm,
		OLVM_deref(vm, get_blinky),
		0, NULL);
	assert (is_pair(xy));

	*x = ol2int(car(xy));
	*y = ol2int(cdr(xy));
}
...
void ol_blinky_move(int x, int y)
{
	uintptr_t args[] = {
		make_integer(x),
		make_integer(y)
	};

	OLVM_evaluate(vm,
		OLVM_deref(vm, blinky_move),
		2, args);
}

```
