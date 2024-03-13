Portable Ol
===========

You can create a standalone Ol executable with all required libraries and autorun main program.

Run `make ol` to create a linux, macOS, etc. portable OpenGL program.  
Run `make ol.exe` to create the same for windows.

How it's made
-------------
1. `OLVM_TARVENV` feature must be enabled.
1. `pvenv.tar` is a tar archive with virtual environment files,
   all files must start from "**./**" to override the global files.
1. Write startup code into "./main" (this file must be included into pvenv.tar).
1. Embed `pvenv.tar` into ol executable as ".tar" section for Linux/macOS/etc.,
   or just append to ol.exe for Windows.

How it's working
----------------
Just run `./ol` or `ol.exe`.  
```
$ ./ol
OpenGL version: 4.6 (Compatibility Profile) Mesa 23.0.4-0ubuntu1~22.04.1
OpenGL vendor: AMD
OpenGL renderer: RENOIR (renoir, LLVM 15.0.7, DRM 3.42, 5.15.0-91-generic)
```
![https://imgur.com/3HVK2HG.png](https://imgur.com/3HVK2HG.png)

Notes
-----

* REPL still available, use `./ol -` (or `ol.exe -`) to run.
  ```
  $ ./ol -
  Welcome to Otus Lisp 2.5.1
  type ',help' to help, ',quit' to end session.

  > ,libs
  '((otus core) (lang embed) (lang threading) (lang eval) (lang macro) (lang fixedp
  oint) (lang alpha) (lang cps) (lang gensym) (lang rtl) (lang closure) (lang assem
  ble) (lang register) (lang ast) (scheme cxr) (lang env) (otus lisp) (scheme misc)
   (lang sexp) (lang intern) (lib system) (owl regex) (owl parse) (owl time) (schem
  e base) (scheme process-context) (scheme inexact) (owl math-extra) (owl sort) (ow
  l iff) (owl io) (otus fasl) (owl render) (owl rlist) (owl symbol) (otus blobs) (o
  wl queue) (scheme exceptions) (scheme string) (scheme bytevector) (scheme vector)
   (owl string) (owl unicode) (owl lazy) (owl list-extra) (otus async) (lang error)
   (lang primop) (owl list) (srfi 1) (owl math) (owl math fp) (owl ff) (scheme list
  ) (scheme core) (srfi 71) (srfi 87) (srfi 16) (src vm))
  
  > (print "hello")
  hello
  #true
  
  > ,quit
  bye-bye.
  ```

* Other command line options works too, sure.
  ```
  $ ./ol --version
  ol (Otus Lisp) 2.5.1
  Copyright (c) 2014-2023 Yuriy Chumak
  License LGPLv3+: GNU LGPL version 3 or later <http://gnu.org/licenses/>
  License MIT: <https://en.wikipedia.org/wiki/MIT_License>
  This is free software: you are free to change and redistribute it.
  There is NO WARRANTY, to the extent permitted by law.
  ```
