```
                  small,
        `___`         embeddable
        (O,O)             and
        \)  )          purely
      ---"-"---     functional!
   O t u s L i s p
```
*Based on Aki Helin's [Owl-Lisp](https://gitlab.com/owl-lisp/owl)*

[![Github build linux status](https://github.com/yuriy-chumak/ol/workflows/build%20linux/badge.svg)](https://github.com/yuriy-chumak/ol/actions)
[![Github build macos status](https://github.com/yuriy-chumak/ol/workflows/build%20macos/badge.svg)](https://github.com/yuriy-chumak/ol/actions)
[![Github build windows status](https://github.com/yuriy-chumak/ol/workflows/build%20windows/badge.svg)](https://github.com/yuriy-chumak/ol/actions)
<a href="https://twitter.com/otus_lisp"><img align="right" src="https://img.shields.io/twitter/url/https/twitter.com/otus_lisp.svg?style=social&label=Follow%20%40otus_lisp"></a></br>

Otus Lisp, Version 2.3
======================
[![Visit the project page](https://yuriy-chumak.github.io/ol/assets/view-project-page.svg)](https://yuriy-chumak.github.io/ol/)

Otus Lisp (Ol in short) is a purely functional dialect of Lisp.

It implements an extended subset of the R<sup>7</sup>RS Scheme
([PDF](https://small.r7rs.org/attachment/r7rs.pdf)), including
but not limited to some SRFIs. It is tiny (~ 64KB), embeddable
and cross-platform.  Provides a portable, high-level interface
to call code written in another language.

You can use Ol in Linux, Windows, macOS, Android, Chromebook*,
(Open/Free/Net) BSD, Solaris and other operating systems based
on various hardware architectures (intel, arm, ppc, mips, etc).

Also, Ol is ported to the Web (in WebAssembly form) and can be
used in Chrome, Firefox, Opera, Iceweasel, Epiphany, SeaMonkey,
Luakit, Iceape, etc.


* thx to [the-man-with-a-golden-mind](https://github.com/the-man-with-a-golden-mind) for the tests.

Q/A
---

1. Q. Why no **arrow keys** processing and a history in Ol command line?<br/>
   A. For the simplicity. I recommend to use an [rlwrap](https://github.com/hanslub42/rlwrap) tool: `$ rlwrap ol` .

1. Q. You reference to licenses **MIT and LGPL**. Can I freely choose between these two licenses?  
   A. Yes, you are free to choose an MIT **or** LGPL license.

1. Q. I want to have a Virtual Environment. Can I?  
   A. Yes. A virtual environment is an Ol environment such that the libraries and scripts installed into it are isolated from those installed in other virtual environments and your operating system. Use `--home=the-yours-venv-path` Ol command line option.

1. Q. Anything else interesting?  
   A. Yes, Ol provides **simplest HTTP web-server** for sharing a local folder over an inter/intra-net.  
      Just type `$ echo ,load http/server| ol`.

1. Q. Why do you call characters runes?  
   A. Because these are runes - letters in a wide set of alphabets :)  
      The Plan 9 operating system (a flavor of Unix), for example, uses UTF-8 as character encoding,
      and its wide character type is called "Rune", not "wide char".


Join the online [gitter.im chat](https://gitter.im/otus-lisp/Lobby).
Alternatively the Libera.Chat [#otus-lisp](https://web.libera.chat/#otus-lisp) (alternate [lightweight](https://web.libera.chat/gamja/#otus-lisp) web-client) channel is available (the previous Freenode channel is closed).

[The Issues](https://github.com/yuriy-chumak/ol/issues) github page waiting for your bug reports and issues.


BUILD / INSTALL
---------------

#### BUILD REQUIREMENTS

* GCC 3.2+ / CLANG 3.5+
* GNU MAKE
* XXD for a full build ("xxd" is usually part of "vim" package), not required for olvm.


#### BUILD

```
$ make
$ ./ol
Welcome to Otus Lisp 2.3
type ',help' to help, ',quit' to end session.
>
```

The Ol binary includes a rich set of features (lists, vectors and byte vectors, ansi and unicode strings, infinite precision math, associative arrays aka "ff"s, i/o streams, lazy evaluations, regular expressions, continuations, exceptions, lexer parsers, asyncs and actors, etc.) and can be used as a completely standalone.

Advanced build instructions for Windows / Linux / macOS / Android / Web / etc.:
[doc/BUILD.md](doc/BUILD.md)


#### Install (*/usr/bin/ol* and */usr/lib/ol/* by default)
```
$ sudo make install
```

#### Uninstall
```
$ sudo make uninstall
```


Table of Contents
-----------------
1. [Build / Install](#build--install) ([Advanced](doc/BUILD.md))
1. [Cross-Compilation](doc/CROSS-COMPILATION.md)
1. [Packaging](#packaging)
1. [Changelog](doc/CHANGELOG.md)
1. [Learning](#learning) / [Language Reference](doc/reference/)
1. [R<sup>7</sup>RS Differences](#r7rs-differences)
1. [Deprecations](#deprecations)
1. [Running](#running)
1. [Lisp sources in binary form](#binary-scripts)
1. [Files](#files)
1. [Embedding](#embedding-ol)
1. [Docs](#documentation)
1. [License](#license)


PACKAGING
---------

[![Packaging status](https://repology.org/badge/vertical-allrepos/ol.svg)](https://repology.org/project/ol/versions)
[![Packaging status](https://repology.org/badge/tiny-repos/ol.svg)](https://repology.org/project/ol/versions)
[![latest packaged version(s)](https://repology.org/badge/latest-versions/ol.svg)](https://repology.org/project/ol/versions)

**CentOS**, **Debian**, **openSUSE**, **RHEL**, **SL**, **SLE**, **Ubuntu**, **Univention** precompiled packages: [OpenSUSE Build Service](https://software.opensuse.org/download.html?project=home%3Ayuriy-chumak&package=ol).

Some additional libraries can be installed using 'kiss' package manager. Usage instruction available at [**ol-packages** repository](https://github.com/yuriy-chumak/ol-packages).


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

* "Pacman" sample demonstrates embedding Ol scripts in native "C" code - https://github.com/yuriy-chumak/ol/tree/master/samples/pacman

!["pacman" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/pacman.png)

* "Digital rain" sample demonstrates native libraries direct usage (the [OpenGL](http://www.opengl.org/)) - https://github.com/yuriy-chumak/ol/tree/master/samples/Matrix

!["digital rain" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/digital-rain.png)

* "Newton dynamics" sample demonstrates extended native libraries usage (the [newton-dynamics](http://newtondynamics.com), physical simulation engine) with callbacks (C to Lisp automatic translation) - https://github.com/yuriy-chumak/ol/tree/master/samples/Newton.
You should have compiled [newton-dynamics.so](https://github.com/MADEAPPS/newton-dynamics) core library.

!["newton" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/newton.png)


R<sup>7</sup>RS DIFFERENCES
---------------------------

* 2.1. Identifiers
  * `|\t\t|` and `|\x9;\x9;|` are **different** in Ol, but *the same* in Scheme.
  * Ol is definitely **case sensitive**, but Sheme is *configurable with #!fold-case and #!no-fold-case*.

* 4.1.5. Conditionals
  * Ol provides **extended *if*** with `then` and `else` keywords in forms:
    * `(if <condition> then <then1-clause>..<thenN-clause>)`,
    * `(if <condition> <then-clause> else <else1-clause>..<elseN-clause>)`,
    * `(if <condition> then <then1-clause>..<thenN-clause> else <else1-clause>..<elseN-clause>)`.

* 4.1.6. Assignments
  * **No** `set!` in Ol.
    - explanation: Ol is a purely functional language.
    - note: Use `define` instead.
    - note: Limited support of `set-car!`, `set-cdr!`, and `set-ref!` functions is provided.
    - note: Dynamic variables is available via `(scheme dynamic-bindings)` library. With some speed impact, sure.

* 4.1.7. Inclusion
  * **No** `include` and `include-ci` in Ol.
    - note: Use libraries (`import`, `define-library`, `export`, etc.) instead.
    - note: Top level ",load" REPL command is available.

* 4.2.1. Conditionals
  * Result of the `when` expression is value **returned by the last expression** in Ol, but *unspecified* in Scheme.
  * Result of the `unless` expression is value **returned by the last expression** in Ol, but *unspecified* in Scheme.
  * Extended form of `case` is available in Ol.

* 4.2.5. Delayed evaluation
  * **No** `delay-force`, `promise?` in Ol.
    - note: `delay` and `force` exists, sure.
    - note: `make-promise` exist too.

* 4.2.7. Exception handling
  * **No** `guard` in Ol.

* 4.3. Macros
  * **No** `let-syntax` and `letrec-syntax` in Ol.
    - note: Use `define-syntax` instead.

* 5.5. Record-type definitions
  * **No** `define-record-type` in Ol.

* 6.1. Equivalence predicate `eqv?`
  * `(eqv? +nan.0 +nan.0)` is **#true** in Ol, but *unspecified* in Scheme. The same is for `+inf.0` and `-inf.0`.

* 6.2.5. Syntax of numerical constants
  * `Numbers without precision` considered to be **exact** in Ol, but *inexact* in Scheme.
    - explanation: Inexactness can be disabled by compiler features or/and unsupported by platform. Ol designed with respect for the same functionality of the program, independently of inexactness support (unless we directly use inexact numbers, sure).

* 6.2.6. Numerical operations
  * note: `complex?` is the same as `number?`, like in Scheme.
  * `integer?` for inexact numbers always returns **#false** in Ol, but can be *#true* in Scheme when `(= number (round number))`.
    - explanation: Inexactness is an inexactness, we may lose the fractional part and not to be noticed about. So let's be a little paranoid.
  * `sqrt` is **included** in base library profile while *not included* in Scheme
    - explanation: due to frequent use.

* 6.4. Pairs and lists
  * `memq` and `assq` behavior with 'short' numbers (aka 'enumerations') as first argument is fully **specified** in Ol, but *unspecified* in Scheme.
    - *note: those numbers processed by memq and assq as usual elements.*

* 6.6. Characters
  * CHARACTERS in Ol is a **small numbers** (aka 'enumerations'), but a *characters* in Scheme.
    - explanation: This is for a historical reason.
      Ol supports two types of numbers - 'small' numbers (enumerations) and 'long' numbers (just numbers). 'Small' numbers are used as 'glyphs' (or 'runes' in other word) inside strings for Unicode support. An additional 'character' type with requirement to use the char->integer and integer->char functions every time is too boring and slow. Thanks.
    - note: Ol supports full Unicode 14.0.0 (2021 Sep 14) character set.
    - note: If you want to print a character in the form of a letter (or a digit, etc.), use a function 'string', i.e. instead of `(print #\λ)` use `(print (string #\λ))`, otherwise you will get a number 955.

* 6.7. Strings
  * NEGATIVE indices of `substring` is **valid** in Ol, but *invalid* in Scheme.
    - note: "-1" is the last rune of the string, "-2" is before the last element, etc.

* 6.8. Vectors
  * NEGATIVE indices of a vector is **valid** in Ol, but *invalid* in Scheme.
    - note: Negative vector indices can be used to access the n-th element from the end of the vector. This means that "-1" is the last element of the vector, "-2" is before the last element, "-n" is the n-th element from the end of the vector.

* 6.10. Control features
  * `apply` arguments count is **limited to 256** in Ol, but *unlimited* in Scheme.
    - note: Use `fold` instead, like `(apply + '(1 2 3))` -> `(fold + '(1 2 3))`.

* 6.11. Exceptions
  * **No** exceptions handling in Ol.
    - note: Yet.

* 6.13. Input and output
  * `current-input-port`, `current-output-port`, and `current-error-port` always return **stdin**, **stdout**, and **stderr** in Ol respectively, but *a parameter objects* in Scheme.
    - note: due to a large performance impact.

* Ol has builtin **regular expressions** while Scheme *not*.
  * note: you can use `m/<pattern>/`, `s/<expression>/<new-expression>/` with optional 'g' suffix, and `c/<pattern>/` as functions to match, change and split the string.


DEPRECATIONS
------------

* `(ilist ...)` is deprecated. Use `(cons* ...)` instead.
* `(interact ...)` from (owl ~~interop~~ async) is deprecated. Use `(await (mail ...))` instead.
* `(fork ...)`, `(fork-named ...)`, `(fork-server ...)` is deprecated. Use `(async ...)`, `(async 'name ...)`, `(actor ...)` instead.


### DISASSEMBLY

Ol 2.3 contains built-in tool for inspecting the Otus Lisp language.

You can use the REPL `,expand` command to expand high-level Ol instructions into low-level (core) Otus Lisp.
```scheme
> ,expand (assert (+ 1 2) = 3)
(ifeq (equal? ((lambda (g1) g1) (+ 1 2)) 3) #true #true (runtime-error assertion error: (cons (quote (+ 1 2)) (cons must be (cons (quote 3) ())))))
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
code: (11 3 0 7 1 1 2 6 2 6 3 17)
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
$ ol [options] [filename] [arguments]

* if no filename given ol will use stdin as source
* if you want to use stdin as source but must provide an arguments, use "-" instead
* if you want to break vm-options scanning and provide filename like option (i.e. '--version' as a real file name), use "--" for 'end-of-option' flag and then a filename

Olvm command line options available:
* '-v': print olvm version then exit
* '--version': print olvm version and licensing information then exit

Ol command line options available:
* '-v': print ol version then exit
* '--version': print ol version and licensing information then exit
* '--version=...": overwrite ol version string
* '--sandbox': enable execution in the sandbox (if OS supports)
* '--interactive': force REPL interactive mode
* '--no-interactive': disable REPL interactive mode
* '--embed': run special reduced REPL for embed usage
* '--home=...': overwrite path where to search for the ol libraries
* '--': end-of-options sign


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


FILES
-----

* repl  - the compiled ol binary interpreter/compiler
* src/olvm.c  - the ol virtual machine source code (in C)
* includes/ol/ol.h  - the common ol header (not required by compiler, just for use as embed)
* includes/ol/vm.h  - the ol virtual machine header (not required by compiler, just for use as embed)
* extensions/ffi.c  - FFI implementation
* lang/*.scm  - ol repl and compiler source codes (in Lisp)
* libraries/**.scm - various OL libraries (in Lisp):
  * libraries/scheme/core.scm - r7rs core implementation
  * libraries/owl/*.scm - legacy basic libraries
  * libraries/lib/*.scm - some external native library mappings
  * etc.
* tests/** - some basic automation tests (in Lisp and C)
* tests/rosettacode/*.scm - additional automation tests (in Lisp) that described at the [Rosetta Code](http://rosettacode.org/) programming chrestomathy site.


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
Copyright (c) 2014-2022 Yuriy Chumak                      <br/>


----------------------------------------------------------------------

Grew out of the Owl Lisp by Aki Helin: https://gitlab.com/owl-lisp/owl

Thanks to:
* [the-man-with-a-golden-mind](https://github.com/the-man-with-a-golden-mind) for the tests, ideas, and questions.

Resources:
* http://groups.csail.mit.edu/mac/projects/scheme/
* http://people.csail.mit.edu/jaffer/Scheme
* http://r7rs.org
