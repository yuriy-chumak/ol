Trying into bootstrapping
=========================

Fasl format is just a binary representation of transpiled Lisp S-exp.
Only purpose of fasl file is the fast deserialization without parsing and compiling into basic lisp primitives of human readable source code.

This folder provides two files:
* `fasl-2-sexp.lisp` - translation from binary format into text s-exp,
* `sexp-2-fasl.lisp` - translation from text s-exp into binary format.
* `repl.sexp` - current s-exp representation of Ol REPL (Ol language).

This is fully workable reference implementations, currently is not R7RS compliant. Scheme implementation will be provided a bit later.

If you want to get the minimal actual distribution of Otus Lisp from text source files only, you have to do (from the root of Ol repository):  
(I use linux mint environment)

1. compile olvm from the olvm.c with gcc/clang/tcc.
   ```bash
   $ $(CC) src/olvm.c -o olvm -lm -ldl
   ```
1. create binary representation of REPL, which can be directly run by ol virtual machine as `./olvm repl`:
   ```bash
   $ $(Ol) samples/bootstrapping/sexp-2-fasl.lisp <samples/bootstrapping/repl.sexp >repl
   ```
1. generate olvm header from the olvm source code:
   ```bash
   $ sed -e '/\/\/ <!--/,/\/\/ -->/d' src/olvm.c >includes/ol/vm.h
   ```
1. create Ol binary with integrated REPL:
   ```bash
   $ xxd --include repl >tmp/repl.c
   $ $(CC) src/olvm.c tmp/repl.c -DREPL=repl -o ol -lm -ldl
   ```

Notes:
------

Now, if you want to recompile Ol REPL with Ol source code changes, try:
```bash
$ make recompile
```

If you want to compile full Ol version (with ffi, sockets, etc.), try:
```bash
$ $(CC) src/olvm.c tmp/repl.c \
        -DREPL=repl -DHAS_DLOPEN=1 -DHAS_SOCKETS=1 \
        extensions/ffi.c -Iincludes \
        -DOLVM_FFI=1 \
        -o ol -lm -ldl  -Xlinker --export-dynamic
```
