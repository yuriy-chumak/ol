EMBEDDING OL
============

Ol: an extensible embedded language.

This folder demonstrates a usage of Otus Lisp as part of a pacman-like game.

There are three different 

![Screenshot.](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/screenshot.png "screenshot")


Ol can be embed in your project. There are two ways of embedding: low level embedding and high level embedding.

The low-level uses ol/vm.h header, and high-level uses ol/ol.h header.

Please check the next code as a sample:

TBD.

```c
// // embedded olvm usage example
// #include <ol/ol.h>

// // ol.h header provides few simplified macros and functions
// // to easy work with ol virtual machine.
// // For example, make_integer(x) converts C integer into internal
// // olvm format. Other cases please check in ol.h header or
// // tests/embed.c test file.

// // ol embed instance
// ol_t ol;

// unsigned char repl[]; // repl is otus lisp language
// int main(int argc, char** argv)
// {
//     // result of ol functions call
//     uintptr_t r;

//     // create new embedded olvm instance
//     OL_new(&ol);

//     // our embed extension can work in different manners:
//     // 1) if eval got only one string parameter it returns the value of parameter,
//     //    this can be used to evaluate variables, constants or any expressions
//     //    from the string (i.e. you can read whole lisp file info string and send
//     //    it to execute by olvm)
//     // Note: if you want to call expression, but you have no arguments,
//     //    just use parenthesis, like "(print)" instead of "print"
//     //
//     // 2) if eval got one string parameter and more than 0 arguments it makes
//     //    'apply' to them - (apply arg0 args),
//     //    it can be used as quick way to evaluate function by name with parameters.
//     //
//     // 3) if eval got one numerical parameter and maybe more than 0 arguments,
//     //    this case means case 2, but with function referenced not by name but by
//     //    'pin' id. So function must be pinned object.
//     //    it provides way to speedup the execution (removes string parsing and
//     //    compiling by ol)
//     //
//     // 4) at last if eval receives bytevector it tries to decode it as fasl object
//     //    and evaluate in 2) or 3) manner.

//     // tests/embed.c provides some additional staff to further simplification of ol
//     // virtual machine communication. I mean function "eval" that automatically
//     // converts function arguments to ol representation.

//     ... include portion of tests/embed.c from "--cut-->" to "<--cut--" lines ...

//     // So, let's try all this cases:

//     // new function declaration:
//     r = eval("(define (f x) (if (eq? x 0) 1 (* x (f (- x 1)))))");
//     assert (r != IFALSE);

//     // call the function with arguments
//     r = eval("f", 7);
//     assert (r == make_integer(5040)); // and check the result

//     // working with precompiled scripts:
//     char causar[] = {2, 16, 26, 11, 1, 0, 21, 1, 1, 3, 4, 1, 1, 2, 5, 5, 5, 8, 4, 5, 5, 3, 4, 8, 3, 2, 5, 3, 17, 2, 16,
//                      16, 11, 1, 0, 11, 1, 1, 3, 4, 1, 1, 2, 3, 2, 4, 1, 17, 2, 16, 35, 11, 1, 0, 30, 1, 1, 2, 4, 1, 1,
//                      3, 5, 1, 1, 4, 6, 4, 4, 2, 2, 5, 6, 7, 9, 4, 5, 5, 3, 4, 7, 3, 2, 5, 3, 17, 2, 16, 30, 11, 2, 0,
//                      10, 14, 19, 5, 19, 2, 5, 4, 6, 24, 6, 11, 3, 0, 11, 14, 19, 6, 19, 3, 6, 4, 5, 7, 24, 7, 17, 2, 16,
//                      12, 11, 3, 0, 7, 40, 4, 5, 6, 7, 24, 6, 17, 1, 17, 2, 3, 4, 2, 16, 61, 11, 1, 0, 56, 1, 1, 2, 4, 1,
//                      1, 3, 5, 10, 4, 5, 3, 6, 16, 5, 30, 0, 1, 2, 2, 7, 1, 1, 4, 8, 1, 1, 5, 9, 4, 4, 2, 3, 8, 9, 3, 14,
//                      1, 11, 5, 5, 4, 11, 5, 2, 7, 3, 1, 1, 4, 7, 205, 3, 2, 7, 1, 17, 1, 17, 3, 1, 3, 7, 2, 16, 40, 11,
//                      3, 0, 35, 1, 1, 2, 6, 47, 6, 4, 7, 1, 2, 2, 8, 1, 1, 4, 9, 3, 6, 2, 3, 9, 4, 3, 5, 3, 1, 1, 3, 5,
//                      9, 7, 4, 2, 8, 3, 17, 1, 17, 3, 1, 5, 2, 2, 16, 46, 11, 1, 0, 41, 1, 1, 3, 4, 1, 1, 4, 5, 3, 5, 2,
//                      4, 4, 5, 3, 6, 36, 4, 4, 1, 2, 2, 8, 1, 1, 2, 9, 3, 5, 2, 3, 6, 3, 9, 3, 14, 1, 5, 2, 8, 3, 17, 1,
//                      17, 4, 1, 7, 6, 2, 2, 16, 25, 11, 3, 0, 20, 36, 4, 6, 1, 1, 2, 7, 6, 5, 3, 3, 4, 5, 3, 9, 6, 4, 2,
//                      7, 2, 17, 1, 17, 3, 1, 10, 2, 0};
//     // and we have an encoded message
//     char message[] = "Dolfh#dqg#Ere#zdqw#wr#iols#d#frlq#e|#whohskrqh1";

//     // let's decode it manually without simplification syntax:
//     r = embed_eval(&ol, new_bytevector(&ol, causar, sizeof(causar)),
//                         new_bytevector(&ol, message, sizeof(message)),
//                         make_integer(3), 0);
//     assert (is_bytevector(r));
//     assert (bytevector_length(r) == sizeof(message));
//     assert (strncmp(bytevector_value(r), "Alice and Bob want to flip a coin by telephone.", sizeof(message)-1) == 0); // it's ok

//     // finally, please check extensions/embed/sample.c code for more embed examples
//     return 0;
// }
```

Just compile with -DOLVM_NOMAIN and link with repl binary.

Another sample can be found in samples/pacman/ folder.


EMBEDDED OL IN DETAIL
---------------------

Please refer to the [project page](https://yuriy-chumak.github.io/ol/)
