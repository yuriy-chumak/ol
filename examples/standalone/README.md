Standalone Executable
=====================

You can create a standalone Ol executable with all required libraries and an automatically running main program.

Run `make` to create a linux (macOS, Unix) portable binaries.  
Run `make ol.exe gol.exe` to create the same for windows.

How it's made
-------------
1. `OLVM_TARVENV` feature must be enabled (is enabled by default since ver. 2.6).
1. `*-pvenv.tar` are tar archives with virtual environment files,
   all files must start from "./" to override the global files.
1. Startup code must be put in "./main" inside the *-pvenv.tar.
1. Embed `*-pvenv.tar` into ol executable as ".tar" section for Linux/macOS/etc.,
   or just append to ol.exe for Windows.

How it's working
----------------

Just run `./ol` or `ol.exe`.  
```
$ ./ol
Usage: ol [OPTION]... [--] [input-file [file-options]]

   --help              print full options list
          ............
use 'main' for 'input-file' to run default main program.

Running just 'ol' without any options same as 'ol main'.
```

Second one example `./gol` (and `gol.exe`) showing GTK and OpenGL usage inside standalone executable.
![https://imgur.com/3HVK2HG.png](https://imgur.com/3HVK2HG.png)

Notes
-----

* REPL still available, use `./ol -` (or `ol.exe -`) to run.
  ```scheme
  $ ./ol -
  Welcome to Otus Lisp 2.6
  type ',help' to help, ',quit' to end session.

  ; let's check included libraries
  > ,libs
  '((otus core) (lang embed) (lang threading) (lang eval) (lang macro) (lang fixedp
  oint) (lang alpha) (lang cps) (lang gensym) (lang rtl) (lang closure) (lang assem
  ble) (lang register) (lang ast) (scheme cxr) (lang env) (otus lisp) (scheme read)
  (scheme misc) (lang sexp) (lang intern) (lib system) (owl time) (scheme base) (sc
  heme process-context) (scheme inexact) (owl math-extra) (owl sort) (owl io) (otus
  fasl) (otus format) (owl rlist) (owl symbol) (owl regex) (owl iff) (owl parse) (o
  tus blobs) (owl queue) (scheme exceptions) (scheme string) (scheme bytevector) (s
  cheme vector) (owl string) (owl unicode) (owl lazy) (owl list-extra) (otus async)
  (lang error) (lang primop) (owl list) (srfi 1) (owl math) (owl math fp) (owl ff) 
  (scheme list) (scheme core) (srfi 71) (srfi 87) (srfi 16) (src vm))

  > (print "hello")
  hello
  #true
  
  > ,quit
  bye-bye.
  ```

* Other command line options works too, sure.
  ```
  $ ./ol --version
  ol (Otus Lisp) 2.6
  Copyright (c) 2014-2024 Yuriy Chumak
  License LGPLv3+: GNU LGPL version 3 or later <http://gnu.org/licenses/>
  License MIT: <https://en.wikipedia.org/wiki/MIT_License>
  This is free software: you are free to change and redistribute it.
  There is NO WARRANTY, to the extent permitted by law.
  ```
