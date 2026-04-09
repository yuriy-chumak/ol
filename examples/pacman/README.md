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

The A* path searching algorithm and Blinky (the yellow monster) state completely implemented in Ol (main.scm library file).
The OpenGL rendering and keyboard processing implemented in C (main.c file).

![Screenshot.](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/pacman.png "screenshot")

Files:
* **main.c** - main "C" program with opengl, keyboard and win/lose logic.
* **main.scm** - main "Lisp" program module with level and A* blinky brains.
* **Makefile** - build script for all embedding options.
* **resources/*.png** - game resources (background, sprites, etc.).
* **texturing.c** - png texture reader.
* **embed1.c** - embedding option 1, just a way.
* **embed2.c** - embedding option 2, simpler way with few *oddish* C-macro.
* **embed3.c** - embedding option 3, fastest and smallest way with precompiled lisp code (requires a bit deeper knowledge of otus lisp language).
  * **embed3.scm** - additional lisp code to option 3.

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
[embed1.c](https://github.com/yuriy-chumak/ol/blob/master/examples/pacman/embed1.c),
[embed2.c](https://github.com/yuriy-chumak/ol/blob/master/examples/pacman/embed2.c), and
[embed3.c](https://github.com/yuriy-chumak/ol/blob/master/examples/pacman/embed3.c) complete source codes for a better usage understanding.

COMPARISON
----------

Just compare all three embedding options usage.

## embedding option 1

Basic embed usage. We call lisp functions as a direct source code calls.

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

Smart macro, simpler usage. We don't need to call `new_string` and other conversion functions manually, the macro `eval` will do this for us.

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

Precompiled ol code, fastest (near native) execution.
We need to prepare some kind of "startup" code to return vector of lisp functions for us. These functions we can call later directly using.
```scheme
(return [
   (lambda () points)
   get-blinky
   ...
   blinky-move
   get-level
])
```


```c
#include "embed3.inc"

// olvm:
struct olvm_t* vm;
int get_blinky;
int blinky_move;

extern unsigned char tmp_bin[];
void ol_new_ol()
{
	vm = OLVM_new(embed3_bin);
	OLVM_userdata(vm, &vm);

	uintptr_t
	r = OLVM_run(vm, 0, 0);
	assert (is_vector(r));

  ...
	get_blinky = OLVM_pin(ref(r, 2));
  ...
	blinky_move = OLVM_pin(ref(r, 6));
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
