EMBEDDING OL
============

Ol: an extensible embedded language.

This folder demonstrates a usage of Otus Lisp as part of a pacman-like game.

![Screenshot.](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/pacman.png "screenshot")

Files:
* **main.c** - main "C" program with opengl, keyboard and win/lose logic.
* **main.scm** - main "Lisp" program module with level and A* blinky brains.
* **Makefile** - build script for all three embedding options.
* **resources/*.png** - game resources (background, sprites, etc.).
* **texturing.c** - png file into opengl texture reader.
* **embed1.c** - embedding option 1, just a way.
* **embed2.c** - embedding option 2, simplest way with few oddish c-macroses.
* **embed3.c** - embedding option 3, fastest and smallest way with precompiled lisp code (which require more deep knowledge of otus lisp language).
* **precompile.scm** - lisp compiler for option 3.
* **repl.o** - Otus Lisp REPL binary (400 KB).

Temporary files:
* **tmp.bin** - precompiled by precompile.scm game lisp binaries (66 KB).
* **binf.c** - converted into unsigned char* variable tmp.bin (using xxd tool).

Output files:
* **pacman1** - compiled embedding option 1 (consumes about 3 MB of memory).
* **pacman2** - compiled embedding option 2 (consumes same as pacman1).
* **pacman3** - compiled embedding option 3 (consumes only 200 KB of memory and much more faster than pacman1 and pacman2).

IN DETAILS
----------

Please check the
[embed1.c](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/embed1.c),
[embed2.c](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/embed2.c), and
[embed3.c](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/embed3.c) for better usage understanding.

The example code with comments.

```c
// embedded olvm usage example
#include <ol/ol.h>

// ol.h header provides few simplified macros and functions
// to easy work with ol virtual machine.
// For example, make_integer(x) converts C integer into internal
// olvm format. Other cases please check in ol.h header or
// tests/embed.c test file.

// ol embed instance
ol_t ol;

unsigned char repl[]; // repl is otus lisp language
int main(int argc, char** argv)
{
    // result of ol functions call
    uintptr_t r;

    // create new embedded olvm instance
    OL_new(&ol, repl);

    // our embed extension can work in different manners:
    // 1) if OL_eval got only one string parameter it returns the value of this parameter.
    //    This can be used to evaluate variables, constants, or any expressions
    //    from the string (i.e. you can read whole lisp file into string and send
    //    it to execute by olvm)
    // Note: if you want to call expression, but you have no arguments
    //    just use parenthesis, like "(print)" instead of "print"
    //
    // 2) if OL_eval got one string parameter and more than 0 arguments it makes
    //    'apply' to them - (apply arg0 args),
    //    It can be used as quick way to evaluate function by name with parameters.
    //
    // 3) if OL_eval got one numerical parameter and maybe more than 0 arguments,
    //    this case means case 2, but with function referenced not by name but by
    //    'pinned' id. So function must be pinned object.
    //    It provides way to speedup the execution (removes string parsing and
    //    compiling by ol).
    //
    // 4) at last if OL_eval receives bytevector it tries to decode it as fasl object
    //    and evaluate in 2) or 3) manner.

    // embed2.c provides some additional staff for further simplification of ol
    // virtual machine communications. I mean macro "eval" that automatically
    // converts function arguments to olvm representation.

    ... include portion of embed2.c from "EVAL0" macro to "eval" macro lines ...

    // So, let's try all this cases:

    // new function "f" (factorial) declaration:
    r = eval("(define (f x) (if (eq? x 0) 1 (* x (f (- x 1)))))");
    assert (r != IFALSE);

    // call the factorial with number argument "7"
    r = eval("f", 7);
    assert (r == make_integer(5040)); // and check the result

    // the sample without nice macro 'eval' to demonstrate usual way of calling ol functions
    // working with precompiled scripts:
    char causar[] = {2, 16, 26, 11, 1, 0, 21, 1, 1, 3, 4, 1, 1, 2, 5, 5, 5, 8, 4, 5, 5, 3, 4, 8, 3, 2, 5, 3, 17, 2, 16,
                     16, 11, 1, 0, 11, 1, 1, 3, 4, 1, 1, 2, 3, 2, 4, 1, 17, 2, 16, 35, 11, 1, 0, 30, 1, 1, 2, 4, 1, 1,
                     3, 5, 1, 1, 4, 6, 4, 4, 2, 2, 5, 6, 7, 9, 4, 5, 5, 3, 4, 7, 3, 2, 5, 3, 17, 2, 16, 30, 11, 2, 0,
                     10, 14, 19, 5, 19, 2, 5, 4, 6, 24, 6, 11, 3, 0, 11, 14, 19, 6, 19, 3, 6, 4, 5, 7, 24, 7, 17, 2, 16,
                     12, 11, 3, 0, 7, 40, 4, 5, 6, 7, 24, 6, 17, 1, 17, 2, 3, 4, 2, 16, 61, 11, 1, 0, 56, 1, 1, 2, 4, 1,
                     1, 3, 5, 10, 4, 5, 3, 6, 16, 5, 30, 0, 1, 2, 2, 7, 1, 1, 4, 8, 1, 1, 5, 9, 4, 4, 2, 3, 8, 9, 3, 14,
                     1, 11, 5, 5, 4, 11, 5, 2, 7, 3, 1, 1, 4, 7, 205, 3, 2, 7, 1, 17, 1, 17, 3, 1, 3, 7, 2, 16, 40, 11,
                     3, 0, 35, 1, 1, 2, 6, 47, 6, 4, 7, 1, 2, 2, 8, 1, 1, 4, 9, 3, 6, 2, 3, 9, 4, 3, 5, 3, 1, 1, 3, 5,
                     9, 7, 4, 2, 8, 3, 17, 1, 17, 3, 1, 5, 2, 2, 16, 46, 11, 1, 0, 41, 1, 1, 3, 4, 1, 1, 4, 5, 3, 5, 2,
                     4, 4, 5, 3, 6, 36, 4, 4, 1, 2, 2, 8, 1, 1, 2, 9, 3, 5, 2, 3, 6, 3, 9, 3, 14, 1, 5, 2, 8, 3, 17, 1,
                     17, 4, 1, 7, 6, 2, 2, 16, 25, 11, 3, 0, 20, 36, 4, 6, 1, 1, 2, 7, 6, 5, 3, 3, 4, 5, 3, 9, 6, 4, 2,
                     7, 2, 17, 1, 17, 3, 1, 10, 2, 0};
    // and we have an encoded message
    char message[] = "Dolfh#dqg#Ere#zdqw#wr#iols#d#frlq#e|#whohskrqh1";

    // let's decode it manually without simplification syntax:
    r = embed_eval(&ol, new_bytevector(&ol, causar, sizeof(causar)),
                        new_bytevector(&ol, message, sizeof(message)),
                        make_integer(3), 0);
    assert (is_bytevector(r));
    assert (bytevector_length(r) == sizeof(message));
    assert (strncmp(bytevector_value(r), "Alice and Bob want to flip a coin by telephone.", sizeof(message)-1) == 0); // it's ok

    // finally, please check embed1.c, embed2.c, and embed3.c code for more usage examples
    return 0;
}
```

Just compile with -DOLVM_NOMAIN, and link with repl binary.

Additional sample can be found in automated tests folder - [embed.c](https://github.com/yuriy-chumak/ol/blob/master/tests/embed.c).

Feel free to ask questions in [Gitter Ol channel](https://gitter.im/otus-lisp/Lobby) and [Freenode](https://webchat.freenode.net/) #otus-lisp channel.