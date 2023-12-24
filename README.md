```
                  Small,
        `___`         Embeddable
        (O,O)             and
        \)  )          purely
      ---"-"---     Functional!
  O t u s   L i s p
```
*Based on Aki Helin's [Owl-Lisp](https://gitlab.com/owl-lisp/owl)*

[![Github build linux status](https://github.com/yuriy-chumak/ol/workflows/build%20linux/badge.svg)](https://github.com/yuriy-chumak/ol/actions/workflows/build-linux.yml)
[![Github build macos status](https://github.com/yuriy-chumak/ol/workflows/build%20macos/badge.svg)](https://github.com/yuriy-chumak/ol/actions/workflows/build-macos.yml)
[![Github build windows status](https://github.com/yuriy-chumak/ol/workflows/crossbuild%20windows/badge.svg)](https://github.com/yuriy-chumak/ol/actions/workflows/build-windows.yml)
<a href="https://twitter.com/otus_lisp"><img align="right" src="https://img.shields.io/twitter/url/https/twitter.com/otus_lisp.svg?style=social&label=Follow%20%40otus_lisp"></a></br>

Otus Lisp, Version 2.5
======================
[![Visit the project page](https://yuriy-chumak.github.io/ol/assets/view-project-page.svg)](https://yuriy-chumak.github.io/ol/)

Otus Lisp (**Ol** in short) is a purely functional dialect of Lisp.

Ol implements an extended subset of the R<sup>7</sup>RS Scheme
([PDF](https://small.r7rs.org/attachment/r7rs.pdf)), including
but not limited to some SRFIs. It's tiny (~ 64KB), embeddable,
and cross-platform;  provides a portable, high-level interface
to call code written in another language (c, python, lua, etc).

You can use Ol in Linux, Windows, macOS, Android, Chromebook*,
(Open/Free/Net) BSD, Solaris and other operating systems based
on various hardware architectures (intel, arm, ppc, mips, etc).

Also, Ol is ported to the web (in WebAssembly form) and can be
used in Chrome, Safari, Firefox, Edge, Opera, etc.

* credits to:
[the-man-with-a-golden-mind](https://github.com/the-man-with-a-golden-mind) (ideas, usage, lot of tests),
[nullscm](https://github.com/nullscm) (usage, tests),
Odysseus (tests, ideas, math corrections),
mt (tests, ideas).
* note: please check the [differences](#r7rs-differences) between Ol and Scheme R<sup>7</sup>RS.


PACKAGING
---------

[![Packaging status](https://repology.org/badge/vertical-allrepos/ol.svg)](https://repology.org/project/ol/versions)
[![Packaging status](https://repology.org/badge/tiny-repos/ol.svg)](https://repology.org/project/ol/versions)
[![latest packaged version(s)](https://repology.org/badge/latest-versions/ol.svg)](https://repology.org/project/ol/versions)

~~**CentOS**, **Debian**, **openSUSE**, **RHEL**, **SL**, **SLE**, **Ubuntu**, **Univention** precompiled packages: [OpenSUSE Build Service](https://software.opensuse.org/download.html?project=home%3Ayuriy-chumak&package=ol).~~

Some additional libraries can be installed using 'kiss' package manager. Usage instruction available at [**ol-packages** repository](https://github.com/yuriy-chumak/ol-packages).


Q/A
---

1. Q. Some folders **are empty** (i.e. "libraries/OpenGL"), is it ok?  
   A. Yes, it's ok. Some parts of Ol project are separated into their own independent repositories.  
      Use `git clone --recursive` to get a full project. Or `git submodule init; git submodule update` to update existing one.

1. Q. Why no **arrow keys** processing and a history in Ol command line?  
   A. For the gods of simplicity. I recommend to use an [rlwrap](https://github.com/hanslub42/rlwrap) tool  
      (run as `rlwrap ol` in terminal, or add an `alias ol="rlwrap /usr/bin/env ol"` line to your *~/.bashrc* and run as just `ol`).

1. Q. *.. **fatal error**: stdlib.h: No No such file or directory.*  
   Q. *.. **fatal error**: bits/libc-header-start.h: No such file or directory.*  
   A. Install gcc `multilib`, i.e. *sudo apt install gcc-multilib*.

1. Q. You reference to licenses **MIT and LGPL**. Can I freely choose between these two licenses?  
   A. Yes, you are free to choose an MIT or LGPL license.

1. Q. I want to run Ol in **a venv** (Virtual ENVironment). Can I?  
   A. Yes. Use `--home=yours-venv-folder` command line option ([more about](#virtual-env) venv).  
      Additionally, you can embed such a venv into the Ol executable itself (the [portable](#portable-form) form).

1. Q. Anything else interesting?  
   A. Yes, Ol provides **simplest HTTP web-server** for sharing a local folder over an inter/intra-net.  
      Just type `echo ,load http/server| ol` in command line (or `,l http/server` inside Ol session),  
      change `ol` to `ol - --port 8080` to use the custom port.

1. Q. Why do you call the characters not "characters" but "runes"?  
   A. Because they are *runes* - letters in a wide set of alphabets :)  

1. Q. Do you have something like "sleep mode"?  
   A. You can store the current REPL session with `,save "filename"`, just run later `ol filename` to continue saved session.

1. Q. I'm lost in prefix math notation, can you help me?  
   A. Ol has a special math library that provides infix math notation. Use the `(math infix-notation)` library.  
   ```scheme
   > (import (math infix-notation))
   > (print (infix-notation
        1 + 2 * 3 - sqrt(4)
     ))
   5
   ; '\\' is a short for infix-notation
   > (print (\\  1 + 2 * 3 - sqrt(4) ))
   5

   ; you can view result of transformations with ",expand"
   > ,expand (\\  1 + 2 * 3 - sqrt(4) )
   (- (+ 1 (* 2 3)) (sqrt 4))
   ```


Join the online [gitter.im chat](https://gitter.im/otus-lisp/Lobby).
Alternatively the Libera.Chat [#otus-lisp](https://web.libera.chat/#otus-lisp) (alternate [lightweight](https://web.libera.chat/gamja/#otus-lisp) web-client) channel is available (the previous Freenode channel is closed).

[The Issues](https://github.com/yuriy-chumak/ol/issues) github page waiting for your bug reports and issues.


Table of Contents
-----------------
1. [Build/Install](#build--install)
   * [Advanced](doc/BUILD.md)
   * [Cross-Compilation](doc/CROSS-COMPILATION.md)
   * [Embedding](#embedding-ol)
1. [Packaging](#packaging)
1. [Running](#running)
1. [Language Reference](doc/reference/README.md)
   * [R<sup>7</sup>RS Differences](#r7rs-differences)
   * [Samples, Tests, Learning](#learning)
1. [Deprecations](#deprecations)
1. [Hacking](#hacking)
1. [Lisp sources in binary form](#binary-scripts)
1. [Files](#files), [Docs](#documentation)
1. [License](#license)


BUILD / INSTALL
---------------

#### BUILD REQUIREMENTS

* GCC 3.2+ / CLANG 3.5+
* GNU MAKE


#### BUILD

```
$ make
```
Advanced build instructions: [doc/BUILD.md](doc/BUILD.md)


##### RUN
```
$ ./ol
Welcome to Otus Lisp 2.4
type ',help' to help, ',quit' to end session.
>
```

The Ol binary includes a rich set of features (lists, vectors and bytevectors, ansi and unicode strings, infinite precision math, associative arrays aka "ff"s, i/o streams, lazy evaluations, regular expressions, continuations, exceptions, lexer parsers, async functions and actors, etc.) and can be used as a completely standalone.  


##### INSTALL
```
$ sudo make install
```

Note: by default used */usr/bin/ol* for Ol binary, and */usr/lib/ol/* for Ol libraries.


##### UNINSTALL
```
$ sudo make uninstall
```


CHANGELOG
---------

[doc/CHANGELOG.md](doc/CHANGELOG.md)
<a href="https://twitter.com/otus_lisp"><img align="right"
src="https://img.shields.io/twitter/url/https/twitter.com/otus_lisp.svg?style=social&label=Follow%20the%20%40otus_lisp%20development%20process%20on%20Twitter"
alt="Follow the @otus_lisp development process on Twitter."></a>


LEARNING
--------

The Otus Lisp language is based on [Scheme R<sup>7</sup>RS](https://small.r7rs.org/) ([PDF](https://small.r7rs.org/attachment/r7rs.pdf)) with minor changes and useful extensions.

You can find Ol samples at:
* [Standard procedures](doc/reference/) list (under construction).
* [RosettaCode](http://rosettacode.org/wiki/Category:Ol) Ol page.
* [Samples](samples/) and [Tests](tests/) repository folders.
* Embed usage sample available as toy [pacman game](samples/pacman/) sample.
* Android sample available at [android](samples/android/) sample.


For example:

* "LogicWire" and "Digital rain" samples demonstrates native libraries direct usage (the [OpenGL](http://www.opengl.org/)):
  * https://github.com/yuriy-chumak/ol/tree/master/samples/LogicWire ([idea source](https://realhet.wordpress.com/2015/09/02/bitmap-logic-simulator/))
    <img src="https://raw.githubusercontent.com/yuriy-chumak/ol/master/doc/img/2023-04-10-21-47.gif" width="50%" height="50%">
  * https://github.com/yuriy-chumak/ol/tree/master/samples/Matrix
    !["digital rain" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/digital-rain.png)

* "Pacman" sample demonstrates embedding Ol scripts in native "C" code - https://github.com/yuriy-chumak/ol/tree/master/samples/pacman

!["pacman" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/pacman.png)


* "Newton dynamics" sample demonstrates extended native libraries usage (the [newton-dynamics](http://newtondynamics.com), physical simulation engine) with callbacks (C to Lisp automatic translation) - https://github.com/yuriy-chumak/ol/tree/master/samples/Newton.
You should have compiled [newton-dynamics.so](https://github.com/MADEAPPS/newton-dynamics) core library.

!["newton" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/newton.png)


R<sup>7</sup>RS DIFFERENCES
---------------------------

The most important differences are:

* Ol is definitely **case sensitive**.
* `Numbers without precision` considered to be **exact** in Ol, but *inexact* in Scheme.
  - `integer?` for inexact numbers always returns **#false** in Ol.
* **No** `set!` in Ol (because Ol is purely functional),
  - note: Use `define` instead.
  - note: Limited support of `set-car!`, `set-cdr!`, and `set-ref!` functions [are provided](doc/reference/pairs-and-lists.md#set-car).
  - note: Dynamic variables are available via `(scheme dynamic-bindings)` [library](doc/reference/dynamic-bindings.md).
* CHARACTERS in Ol is a **small numbers** (aka 'enums'), but a *characters* in Scheme.
  - note: Ol supports full Unicode 14.0.0 (2021 Sep 14) character set.
  - note: To write a character use `write-char`, otherwise you'll write a number.
* NEGATIVE indices in `substring` is **valid** in Ol (means "from the end of string").
* NEGATIVE indices in a vector functions are **valid** in Ol (means "from the end of vector").
* **Extended form** of `case`,
* **Extended form** of `if` (with `then` and `else` [keywords]((doc/reference/)),
* Ol has builtin **regular expressions**.
* `apply` arguments count is **limited to 256** in Ol (use `fold` otherwise).
* `(eqv? +nan.0 +nan.0)` is **#true** in Ol.

The full list can be read in [doc/R7RS-DIFFERENCES.md](doc/R7RS-DIFFERENCES.md).


DEPRECATIONS
------------

* 2.3 -> 2.4
  - `(ilist ...)` is deprecated. Use `(cons* ...)` instead.
  - `(interact ...)` from (owl ~~interop~~ async) is deprecated. Use `(await (mail ...))` instead.
  - `(fork ...)`, `(fork-named ...)`, `(fork-server ...)` is deprecated. Use `(async ...)`, `(async 'name ...)`, `(actor ...)` instead.

* 2.4 -> 2.5
  - `(OpenGL version-X.Y)` libraries changed to `(OpenGL X.Y)`.


HACKING
-------

Ol contains built-in tool for inspecting the Otus Lisp language.

You can use the REPL `,expand` command to expand high-level Ol instructions into low-level (core) Otus Lisp.
```scheme
> ,expand (assert (+ 1 2) = 3)
'(ifeq (equal? ((lambda (g1) g1) (+ 1 2)) 3) #true #true (runtime-error "assertion error:" (cons (quote (+ 1 2)) (cons "must be" (cons (quote 3) '())))))
```

You can use the REPL `,disassembly` (or `,dis`, or `,d`) command to disassemble Otus Lisp functions to the Ol virtual machine instructions.
```scheme
> ,dis (lambda () 1)
type: bytecode
code: (11 1 0 5 14 1 4 24 4 17)
disassembly '(length command . args):
(4 JAF 1 0 5)
(3 LD 1 4)
(2 RET 4)
(1 ARITY-ERROR)

> ,dis (lambda (x y) (+ x y))
type: procedure
code: #((11 3 0 7 1 1 2 6 2 6 3 17) #<+>)
disassembly '(length command . args):
(4 JAF 3 0 7)
(4 REFI 1 2 6)
(3 GOTO 6 3)
(1 ARITY-ERROR)
```

RUNNING
-------

You can use basic Ol functionality without any installation - just copy the `ol` (`ol.exe` for Windows) binary to any user-accessible path.

Basic functionality includes a rich set of features: lists, vectors and bytevectors, numbers math with unlimited accuracy, strings, associative arrays (named `ff`), i/o streams and files, lazy calculations, regular expressions, asyncs and actors, etc.

Advanced functionality (i.e. OpenGL support) requires a complete installation of the Ol package:
  * You can use precompiled binaries and/or installation packages that can be found at the [Releases](https://github.com/yuriy-chumak/ol/releases) announcement page.
  * or You can manually copy required [libraries](https://github.com/yuriy-chumak/ol/tree/master/libraries) to your OL_HOME or current directory,


Ol command line is:
`ol [options] [filename] [arguments]`

* if no filename given ol will use stdin as source
* if you want to use stdin as source but must provide an arguments, use "-" instead
  * i.e. `echo '(print *vm-args*)' | ol - arg1 arg2`
* if you want to break vm-options scanning and provide filename like option (i.e. '--version' as a real file name), use "--" for 'end-of-option' flag and then a filename
  * i.e. `echo '(print *vm-args*)' > --version; ol -- --version arg1 arg2 arg3`

Olvm command line options available:
* `-v`: print olvm version then exit
* `--version`: print olvm version and licensing information then exit

Ol command line options available:
* `-v`: print ol version then exit
* `--version`: print ol version and licensing information then exit
* `--version=...`: overwrite ol version string
* `--home=...`: overwrite path where to search for the ol libraries
* `--sandbox`: enable execution in the sandbox (if OS supports)
* `--sandbox=...`: execution in the sandbox with "..." Megs of memory preallocated
* `--interactive`: force REPL interactive mode
* `--no-interactive`: disable REPL interactive mode
* `--embed`: run special reduced REPL for embed usage
* `--`: end-of-options sign

Ol can be executed interactively or in the unattended mode.

#### Interactive mode

```scheme
$ ol
Welcome to Otus Lisp 2.2,
type ',help' to help, ',quit' to end session.
; now you in REPL and can play with in
> (+ 1 2 3)
6
; or let's make some factorial calculations?
> (let factorial ((n 17))
   (if (= n 0)
      1
      (* n (factorial (- n 1)))))
355687428096000
; this ends interactive session
> ,quit
bye bye :/
```

#### Unattended mode

GNU/Linux, Unixes, *BSDs, macOS, ...
```bash
$ ol scriptname.ol                # text script
$ ol scriptname.bl                # binary (compiled) script
$ echo '(print (+ 1 2 3))' | ol
```

Windows:

```cmd
> ol scriptname.ol
> ol scriptname.bl
> echo (print (+ 1 2 3)) | ol
```


BINARY SCRIPTS
--------------

OL can execute precompiled scripts. You can compile your script
using this code:

```scheme
(define (main . args)
   (print "hello !")) ; anything you want to compile

(fasl-save main "out.bl")
```

where "out.bl" is your binary output file name.
This code creates binary script that can be executed directly by ol or vm:

```bash
$ ./vm out.bl
hello !

$ ./ol out.bl
hello !
```

:exclamation: Note: Since version 2.2.1 Ol supports "constructors" - functions that are automatically executed when loading the source. This is experimental feature with no name yet, but feature is tested and will be presented as a normal feature in the next build.

Constructors are called in "use" order. The order of independent constructors is undefined. If you want to specify the order of such constructors, create a function that uses them as variables in the correct order.

So, starting from version 2.2.1 you should do:
```scheme
(define (main . args)
   (print "hello !")) ; anything you want to compile

(fasl-save (vm:new 63 main) "out.bl")
```

VIRTUAL ENV
-----------

A virtual environment is an Ol environment such that the libraries and scripts installed into it are isolated from those installed in other virtual environments and your operating system.

Use `--home=folder` or `--home=folder1:folder2:...:folderN` Ol command line option, where ':' is a folder divider. Folder names '.' and '..' are allowed.

These folders are linked to the Ol's `*path*` global symbol and can be used and changed at runtime freely.

PORTABLE FORM
-------------

FILES
-----

* `repl`  - the compiled ol binary interpreter/compiler (olvm bytecode)
* `src/olvm.c`  - the ol virtual machine source code (C)
* `includes/ol/ol.h`  - the common ol header (C, not required, just for use as embed)
* `includes/ol/vm.h`  - the ol virtual machine header (C, not required, just for use as embed)
* `extensions/ffi.c`  - FFI implementation (C)
* `lang/*.scm`  - ol repl and compiler source codes (Lisp)
* `libraries/**.scm` - various OL libraries (Lisp):
  * `libraries/scheme/core.scm` - r7rs core implementation
  * `libraries/owl/*.scm` - legacy basic libraries
  * `libraries/lib/*.scm` - external native library mappings
  * etc.
* `tests/**` - some basic automation tests (Lisp, C, Txt)
* `tests/rosettacode/*.scm` - additional automation tests (Lisp), described at the [Rosetta Code](https://rosettacode.org/wiki/Category:Ol) programming chrestomathy site.


EMBEDDING OL
------------

Please refer to the [embedding sample README](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/README.md).


DOCUMENTATION
-------------

Please refer to the [project page](https://yuriy-chumak.github.io/ol/)
or check the source codes - libraries/scheme/core.scm


LICENSE
-------

Otus Lisp is available under 2 licenses:
[MIT License](LICENSE) and
[GNU ](COPYING)([L](COPYING.LESSER))[GPLv3 License](COPYING).

Copyright (c) 2011-2014 Aki Helin                         <br/>
Copyright (c) 2014-2023 Yuriy Chumak                      <br/>


----------------------------------------------------------------------

Grew out of the Owl Lisp by Aki Helin: https://gitlab.com/owl-lisp/owl

Thanks to:
* [the-man-with-a-golden-mind](https://github.com/the-man-with-a-golden-mind) for the tests, ideas, and questions.

Resources:
* http://groups.csail.mit.edu/mac/projects/scheme/
* http://people.csail.mit.edu/jaffer/Scheme
* http://r7rs.org
