Trying into bootstrapping
=========================

Fasl format is just a binary representation of transpiled Lisp S-exp.
Only purpose of fasl file is the fast deserialization without parsing and compiling into basic lisp primitives of human readable source code.

This folder provides two files:
* `repl.sexp` - current s-exp representation of Ol REPL (Ol language),
* `ol/sexp-2-fasl.lisp` - translation from text s-exp into text bytecode format (ol version),
* `awk/sexp-2-fasl.awk` - translation from text s-exp into text bytecode format (awk version),
* `txt2bin.awk` - text bytecode to binary converter,
* `fasl-2-sexp.lisp` - sample translation from binary format into s-exp again.

So, using `awk/sexp-2-fasl.awk`, then `txt2bin.awk`, then `fasl-2-sexp.lisp` makes full cycle converting s-exp into binary and into s-exp again.

If you want to get the minimal actual distribution of Otus Lisp from text source files only, you have to do (from the root of Ol repository):

1. compile olvm (otus lisp virtual machine) from the olvm.c with gcc/clang/tcc
   ```bash
   $ ${CC:=gcc} src/olvm.c -o olvm -lm -ldl
   ```

1. create text bytecode REPL representation
   ```bash
   $ awk -f samples/bootstrapping/awk/sexp-2-fasl.awk <samples/bootstrapping/repl.sexp >repl.code
   ```
   note: the reference Ol code is present in `samples/bootstrapping/ol/sexp-2-fasl.lisp`.

1. create REPL binary representation, which can be directly run by ol virtual machine as `./olvm repl`
   ```bash
   $ LC_ALL=C awk -f samples/bootstrapping/txt2bin.awk <repl.code >repl
   ```
   note: "LC_ALL=C" is important to disable utf-8 bytecodes encoding.

1. create final Ol binary with integrated REPL
   ```bash
   $ xxd --include repl >tmp/repl.c
   $ ${CC:=gcc} src/olvm.c tmp/repl.c -DREPL=repl -o ol -lm -ldl -O3 -g0
   ```kv

Notes:
------

Now, if you want to recompile Ol REPL with Ol source code changes, try:
```bash
$ make recompile
```

If you want to compile full Ol version (with ffi, sockets, etc.), try:
```bash
# generate olvm header from the olvm source code (required by ffi)
$ sed -e '/\/\/ <!--/,/\/\/ -->/d' src/olvm.c >includes/ol/vm.h
# compile full Ol version with REPL and FFI
$ xxd --include repl >tmp/repl.c
$ ${CC:=gcc} src/olvm.c tmp/repl.c \
      -DREPL=repl -DHAS_DLOPEN=1 -DHAS_SOCKETS=1 \
      extensions/ffi.c -Iincludes \
      -DOLVM_FFI=1 \
      -o ol -lm -ldl  -Xlinker --export-dynamic
```
