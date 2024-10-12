Standalone Executable
=====================

You can create a standalone Ol executable with all the necessary libraries and an automatically running main program.

We prepared simple Makefile, so just type `make` to create a example linux standalone binary `ol`.

Check the [../standalone-advanced](#../standalone-advanced) example folder for advanced standalone topics like **windows** builds, integrating **precompiled binaries**, etc.


How it's made
-------------
1. `OLVM_TARVENV` feature must be enabled (is enabled by default since ver. 2.6).
1. `pvenv.tar` is a tar archive with virtual environment files.
1. Startup code must be put as `./main` file inside the `pvenv.tar`.
1. Finally, embed `pvenv.tar` into ol executable as a ".tar" section.


How it's working
----------------

Just go to "out" folder and run `./ol`. You will see the provided "program.lisp" has been ran.
```
$ ./ol
hello :)
Running 'ol' without any options is same as 'ol ./main'.

Usage: ol [OPTION]... [--] [input-file [file-options]]
...............
```

Any other ol options are available, sure.
* So you can type `./ol --version` to get a version
  ```
  $ ./ol --version
  ol (Otus Lisp) 2.6
  Copyright (c) 2014-2024 Yuriy Chumak
  License LGPLv3+: GNU LGPL version 3 or later <http://gnu.org/licenses/>
  License MIT: <https://en.wikipedia.org/wiki/MIT_License>
  This is free software: you are free to change and redistribute it.
  There is NO WARRANTY, to the extent permitted by law.
  ```
* You can type `./ol -` to start a REPL session ("-" means stdin)
  ```scheme
  $ ./ol -
  Welcome to Otus Lisp 2.6
  type ',help' to help, ',quit' to end session.

  > (+ 1 2 3)
  6
  > ,quit
  bye-bye.
  ```
* You can do anything with standalone Ol as if it's part of an installed package, like running a built-in simple web server (don't forget the "-" to avoid auto run the "main")
```
$ echo ",load http/server" |./ol -
server binded to 4000
server listening to http://0.0.0.0:4000
```
