Standalone Executable
=====================

You can create a standalone Ol executable with all the necessary libraries and an automatically running main program.

We prepared simple Makefile, so just type `make` to create an example linux standalone binary `ol`.

Check the [../standalone-advanced](#../standalone-advanced) example folder for advanced standalone topics like **windows** builds, integrating **precompiled binaries**, etc.


How to made
-----------
1. `OLVM_TARVENV` feature must be enabled.
1. `pvenv.tar` is a tar archive with virtual environment files.
1. Startup code must be put as `.` (just a dot) file inside the `pvenv.tar`.
1. Finally, embed `pvenv.tar` into ol executable as a ".tar" section.


How to run
----------

Go to "out" folder and run `./ol`.  
You will see the provided "`autorun.lisp`" has been ran.
```
$ cd out
$ ./ol
It is a standalone executable file that does not require
any external libraries.

Running sandboxed: #false
Command line options: ()

Usage: ol [OPTION]... [--] [input-file|. [file-options]]
...............
```

If you want to run with ol options (like `--sandbox`, for example), use "`.`" as a filename.
```
$ ./ol --sandbox .
It is a standalone executable file that does not require
any external libraries.

Running sandboxed: #true
Command line options: ()
...............
```
Same for autorun options.
```
$ ./ol . 1 2 "hello" 3 4
It is a standalone executable file that does not require
any external libraries.

Running sandboxed: #false
Command line options: (1 2 hello 3 4)
...............
```


Full featured Ol command line in available, sure.
So you can type `./ol --version` to get a version
```
$ ./ol --version
ol (Otus Lisp) 2.6
Copyright (c) 2014-2026 Yuriy Chumak
License LGPLv3+: GNU LGPL version 3 or later <http://gnu.org/licenses/>
License MIT: <https://en.wikipedia.org/wiki/MIT_License>
...............
```

You can type `./ol -` to start a REPL session ("-" means stdin)
```scheme
$ ./ol -
Welcome to Otus Lisp 2.6
type ',help' to help, ',quit' to end session.

> (+ 1 2 3)
6
> ,quit
bye-bye.
```

You can do anything with standalone Ol as if it's part of an installed package, like running a built-in simple web server (don't forget the "-" to avoid auto run the "." program)
```
$ echo ",load http/server" |./ol -
server binded to 4000
server listening to http://0.0.0.0:4000
```
or simply `./ol http/server` (works only for standalone Ol versions)
```
$ ./ol http/server
server binded to 4000
server listening to http://0.0.0.0:4000
```

