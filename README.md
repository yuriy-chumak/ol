```
                  small,
        `___`         embeddable
        (O,O)             and
        \)  )          purely
      ---"-"---     functional!
   O t u s L i s p

 /Based on Aki Helin's Owl-Lisp/
```

[![Travis-CI project page](https://travis-ci.org/yuriy-chumak/ol.svg)](https://travis-ci.org/yuriy-chumak/ol)
[![Visit the project page](https://yuriy-chumak.github.io/ol/assets/view-project-page.svg)](https://yuriy-chumak.github.io/ol/)
[![Github build ol status](https://github.com/yuriy-chumak/ol/workflows/build%20ol/badge.svg)](https://github.com/yuriy-chumak/ol/actions)


Otus Lisp, Version 2.2
======================

Otus Lisp (Ol in short) is a purely functional dialect of Lisp.

It implements an extended subset of the R<sup>7</sup>RS Scheme, including
but not limited to some SRFIs. It is tiny (< 64KB), embeddable
and cross-platform.  Provides a portable, high-level interface
to call code written in another language.

You can use Ol on Linux, Windows, macOS, Android, BSD (and its
descendants), webOS, Solaris and other operating systems based
on various hardware architectures (intel, arm, ppc, mips, etc).


Also Ol is ported to the Web and can be used in Chrome, Firefox, Opera, Iceweasel, Epiphany, Luakit, SeaMonkey, Iceape, etc.

### Otus Lisp, Version 2.2.1 RC1

The build command line has been changed. See the "BUILD" section.

LICENSE
-------

Otus Lisp is available under 2 licenses:
[MIT License](LICENSE) and
[GNU ](COPYING)([L](COPYING.LESSER))[GPLv3 License](COPYING).

Copyright (c) 2011-2014 Aki Helin                              <br/>
Copyright (c) 2014-2021 Yuriy Chumak                           <br/>


SUPPORT
-------

Join the online [![Join the chat at https://gitter.im/otus-lisp/Lobby](https://badges.gitter.im/otus-lisp/Lobby.svg)](https://gitter.im/otus-lisp/Lobby), additionally [Freenode](https://webchat.freenode.net) channel *#otus-lisp* is available.

Post bugs and issues at [the issues](https://github.com/yuriy-chumak/ol/issues) page. If your architecture is not supported, post an issue too.


LEARNING
--------

You can find Ol samples at:
* [RosettaCode](http://rosettacode.org/wiki/Category:Ol) Ol page.
* [Samples](https://github.com/yuriy-chumak/ol/tree/master/samples) and [Tests](https://github.com/yuriy-chumak/ol/tree/master/tests) repository folders.
* Embed usage available as toy [pacman game](https://github.com/yuriy-chumak/ol/tree/master/samples/pacman) sample.


DOWNLOAD / INSTALLATION
-----------------------

You can use basic Ol functionality without any installation - just copy the `ol` (`ol.exe` for Windows) binary to any user-accessible path.

Basic functionality includes a rich set of features: lists, vectors and bytevectors, numbers math with unlimited accuracy, strings, associative arrays (named `ff`), i/o streams and files, lazy calculations, regular expressions, coroutines, etc.

Advanced functionality (i.e. OpenGL support) requires a complete installation of the Ol package:
  * You can use precompiled binaries and/or installation packages that can be found at the [Releases](https://github.com/yuriy-chumak/ol/releases) announcement page.
  * or You can manually copy required [libraries](https://github.com/yuriy-chumak/ol/tree/master/libraries) to your OL_HOME or current directory,

Some libraries can be installed using 'kiss' package manager. Usage instruction available at [ol-packages repository](https://github.com/yuriy-chumak/ol-packages).


R<sup>7</sup>RS DIFFERENCES
---------------------------
* 2.1. Identifiers
  * `|\t\t|` and `|\x9;\x9;|` are **different** in Ol, but *the same* in Scheme.
  * Ol is definitely **case sensitive**, but Sheme is *configurable with #!fold-case and #!no-fold-case*.
* 4.1.5. Conditionals
  * Ol provides **extended *if*** in forms
  `(if <condition> <then-clause> else <else1-clause>..<elseN-clause>)` and
  `(if <condition> then <then1-clause> <then2-clause> else <else1-clause>..<elseN-clause>)`,
  while Scheme is *not*.
* 4.1.6. Assignments
  * **No** `set!` in Ol.
    - *note: Ol is purely functional language.*
* 4.1.7. Inclusion
  * **No** `include` and `include-ci` in Ol.
    - *note: Use ",load" instead.*
* 4.2.1. Conditionals
  * Result of the `when` expression is value **returned by the last expression** in Ol, but *unspecified* in Scheme.
  * Result of the `unless` expression is value **returned by the last expression** in Ol, but *unspecified* in Scheme.
* 4.2.5. Delayed evaluation
  * **No** `delay-force`, `promise?` and `make-promise` in Ol.
    - *note: But DELAY and FORCE exists, sure.*
* 4.2.7. Exception handling
  * **No** `guard` and `raise` in Ol.
* 5.5. Record-type definitions
  * **No** `define-record-type` in Ol.
* 6.1. Equivalence predicate `eqv?`
  * `(eqv? +nan.0 +nan.0)` is **#true** in Ol, but *unspecified* in Scheme. The same for `+inf.0` and `-inf.0`.
* 6.2.5. Syntax of numerical constants
  * NUMBERS WITHOUT PRECISION considered to be **exact** in Ol, but *inexact* in Scheme.
    - *explanation: Inexactness can be disabled by compiler features or/and unsupported by platform. But we should expect the same behavior of the program independently of inexactness support (unless we use inexact numbers, sure).*
* 6.2.6. Numerical operations
  * Just Note: `complex?` is the same as `number?`, like in Scheme.
  * `integer?` for inexact numbers always returns **#false** in Ol, but can be *#true* in Scheme when (= number (round number)).
    - *explanation: Inexactness is an inexactness - we may lose the fractional part and not to be noticed about. So let's be a little paranoid.*
  * `sqrt` is **included** in base library profile while *not included* in Scheme
    - *explanation: due to frequent use.*
* 6.4. Pairs and lists
  * `memq` and `assq` behavior with 'short' numbers (aka 'enumerations') as first argument is fully **specified** in Ol, but *unspecified* in Scheme.
    - *note: those numbers processed by memq and assq as usual elements.*
* 6.6. Characters
  * CHARACTERS in Ol is a **small numbers** (aka 'enumerations'), but a *characters* in Scheme.
    - *explanation: This is for a historical reason. Ol supports two types of numbers - 'small' numbers and 'long' numbers. 'Small' numbers are used as 'glyphs' (or 'runes' in other word) inside strings for better Unicode support. An additional 'character' type with requirements to use the char->integer and integer->char functions every time is too boring and slow. Thanks.*
    - *note: Ol supports full Unicode 12.1.0 (2020 Jun 13) character set.*
    - *note: If you want to print a character in the form of a letter (or a digit, etc.), use a function 'string', i.e. instead of (print #\λ) use (print (string #\λ)), otherwise you will get a number 955.*
* 6.8. Vectors
  * NEGATIVE indices of a vector is **valid** in Ol, but *invalid* in Scheme.
    - *note: Negative indices of a vector can be used to access to the n-th element from the end of a vector. I mean "-1" is the last vector element, "-2" - before the last element, "-N" - N-th element from the end of a vector.*
* 6.11. Exceptions
  * **No** exceptions handling in Ol.
    - *note: Yet.*
* Ol has builtin **regular expressions** while Scheme *not*.
  * *note: you can use `m/<pattern>/`, `s/<expression>/<new-expression>/` (with optional 'g' suffix) and `c/<pattern>/` as functions to match, change and split the string.*


BUILD
-----

> Note: Since version 2.2.1, the build command line has been changed.
> The variable NAKED_VM is no longer supported. Instead, a new REPL build variable is provided.

### BUILD REQUIREMENTS

You should have GCC 3.2+ (with gcc-multilib) or CLANG 3.5+ installed.

MacOS users should have xcode-tools installed.

Windows support requires MinGW installed (with GCC). Wine cross-compilation is also supported.

WebAssembly binary compilation requires Emscripten 1.37.40+.

### BUILD IN SIMPLEST WAY

```bash
$ make; make install
```
* Note: use *gmake* for unix clients
* Note: use *make uninstall* to completely uninstall Ol.

### BUILD IN REGULAR WAY

#### GNU/Linux:

##### Build olvm (ol virtual machine):

```bash
$ cc src/olvm.c  -std=gnu99 -O2  -lm -ldl  -o vm
```

##### Build ol (with integrated REPL):

```bash
$ xxd --include repl >tmp/repl.c
OR
$ echo '(display "unsigned char repl[] = {") (lfor-each (lambda (x) (for-each display (list x ","))) (file->bytestream "repl")) (display "0};")'| ./vm repl >tmp/repl.c
THEN
$ cc src/olvm.c tmp/repl.c  -DREPL=repl  -std=gnu99 -O2  -lm -ldl  -o ol
```

For some platforms, build can be done without using the `xxd` tool or vm itself.
```bash
$ ld -r -b binary -o repl.o repl
$ cc src/olvm.c -DREPL=_binary_repl_start  repl.o  -std=gnu99 -O2  -lm -ldl  -o ol
```

##### Build WebAssembly Binaries (in wasm form):

```bash
$ source {your-emsdk-path}/emsdk_env.sh
$ make olvm.wasm
```

This should create olvm.wasm and olvm.js - web assembly ol representation.
An example usage of Ol as a built-in web application you can check at the official [project page](https://yuriy-chumak.github.io/ol/).

#### Windows:

##### Build olvm (ol virtual machine):
```cmd
> set PATH=%PATH%;C:\MinGW\bin
> gcc.exe src\olvm.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -std=gnu99 -O2  -lws2_32  -o ol
```
##### Build ol (with integrated REPL):
```cmd
> set PATH=%PATH%;C:\MinGW\bin
> ld -r -b binary -o tmp/repl.o repl
> gcc.exe src\olvm.c tmp\repl.o -DREPL=repl -IC:\MinGW\include\ -LC:\MinGW\lib\ -std=gnu99 -O2  -lws2_32  -o ol
```

#### \*BSDs:

You should include "c" library instead of "dl":

##### Build only olvm (ol virtual machine):
```bash
$ cc src/olvm.c  -std=gnu99 -O2  -lc -lm  -o vm
```
##### Build ol (with integrated REPL):
```bash
$ ld -r -b binary -o tmp/repl.o repl
$ cc src/olvm.c tmp/repl.o  -std=gnu99 -O2  -lc -lm  -o ol -DREPL=_binary_repl_start
```

#### Android:

```bash
$ ndk-build
```

#### Open webOS:

Put toolchain/ol.bb bitbake recipe into any place of open webOs
recipes folder (i.e. ./meta-oe/) and run "make ol" from root
open webOs folder.

Upload ol executable from BUILD/work/<build-name>/ol/<version>/build
to machine /bin.

Now you cat execute ol under webos command line or other way you
would like.


SAMPLES
-------

You can check some [sample](https://github.com/yuriy-chumak/ol/tree/master/samples) lisp programs.
For example:

* "Pacman" sample demonstrates embedding Ol scripts in native "C" code - https://github.com/yuriy-chumak/ol/tree/master/samples/pacman

!["pacman" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/pacman.png)

* "Digital rain" sample demonstrates native libraries direct usage (the [OpenGL](http://www.opengl.org/)) - https://github.com/yuriy-chumak/ol/tree/master/samples/Matrix

!["digital rain" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/digital-rain.png)

* "Newton dynamics" sample demonstrates extended native libraries usage (the [newton-dynamics](http://newtondynamics.com), physical simulation engine) with callbacks ("C" to Lisp automatic translation) - https://github.com/yuriy-chumak/ol/tree/master/samples/Newton.
You should have compiled [newton-dynamics.so](https://github.com/MADEAPPS/newton-dynamics) core library.

!["newton" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/newton.png)


CUSTOMIZATION
-------------

If you want to enable/disable some olvm features you can use -Dxxx or -Dxxx=y gcc syntax. This is a list of currently accessible customizations:

|Variable      |Value            |Meaning |
|--------------|-----------------|--------|
|REPL          | undefined       |Which source code binary data is a REPL|
|OLVM_NOMAIN   | 1\|0, default 0 |Disable 'main' function, make olvm embed|
|OLVM_FFI      | 1\|0, default 1 |Enable FFI support|
|OLVM_CALLABLES| 1\|0, default 1 |Enable FFI callbacks support|
|OLVM_INEXACTS | 1\|0, default 1 |Enable inexact math support|
|OLVM_BUILTIN_FMATH| 1\|0, default 1 |Enable builtin vm floating-point math|
|CAR_CHECK     | 1\|0, default 1 |Enable car arguments check|
|CDR_CHECK     | 1\|0, default 1 |Enable cdr arguments check|

This variables are automatically set by the Makefile (like `configure` script). You can override those values, sure:

|Variable      |Value            |Meaning |
|--------------|-----------------|--------|
|HAS_SOCKETS   | 1\|0, default 1 |Enable sockets support (bind, listen, socket, etc.)|
|HAS_DLOPEN    | 1\|0, default 1 |Enable dlopen/dlsym functions support|
|HAS_UNSAFES   | 1\|0, default 1 |Enable "unsafe" external and internal functions|
|HAS_SANDBOX   | 1\|0, default 0 |Enable internal sandbox support (depends on OS kernel)|
|HAS_STRFTIME  | 1\|0, default 1 |Enable strftime function support|

Please note that external libraries (like opengl, sqlite, etc.) support require HAS_DLOPEN and OLVM_FFI enabled.

Additionally you can disable the following olvm features by setting the variables to 0 (-Dxxx=0):

|Variable         |Meaning |
|-----------------|--------|
|SYSCALL_SYSINFO  | sysinfo() function usage |
|SYSCALL_PIPE     | pipe() function usage |
|SYSCALL_GETRLIMIT| getrlimit() function usage |
|SYSCALL_GETRUSAGE| getrusage() function usage |


CHANGING THE LANGUAGE
---------------------

You can change the Otus Lisp language as a language (yes, you can) by editing the sources in lang/ and libraries/ subfolders.
Additionally, you can change the virtual machine itself by editing the src/vm.scm and src/olvm.c source files.

To build the Otus Lisp language (not olvm) named REPL do:

```bash
$ make recompile
```

This will create a new (in successful way) REPL binary `./repl` containing the compiled ol code.


RUNNING
-------

Ol command line is:
$ ol [[vm-options] [filename]] [arguments]]

* if no filename given all options is ol options, not an olvm
* if no filename given ol will use stdin as source
* if you want to use stdin as source but must provide a filename and/or arguments, use "-"
* if you want to break vm-options scanning and provide filename like option (i.e. '--version'), use "--" as 'end-of-option' flag

Current olvm command line options available:
* '--version': print olvm version and exit


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


SOME NOTES
----------

Register interpreter in the linux: start you script with

```
#!/usr/bin/env ol
```

Register interpreter in the ms windows:

```
assoc .ol=OLisp.File
ftype OLisp.File=ol "%1" %*
assoc .bl=OLisp.Binary.File
ftype OLisp.Binary.File=ol "%1" %*
```


EMBEDDING OL
------------

Please refer to the [embedding sample README](https://github.com/yuriy-chumak/ol/blob/master/samples/pacman/README.md).


DOCUMENTATION
-------------

Please refer to the [project page](https://yuriy-chumak.github.io/ol/)
or check the source codes - libraries/scheme/core.scm


RELATED
----------------------------------------------------------------------

<br>Copyright (c) 2014 Aki Helin
<br>Copyright (c) 2014 - 2021 Yuriy Chumak

Grew out of the Owl Lisp by Aki Helin: https://gitlab.com/owl-lisp/owl

Thanks to:
* http://groups.csail.mit.edu/mac/projects/scheme/
* http://people.csail.mit.edu/jaffer/Scheme
* http://r7rs.org

----------------------------------------------------------------------
[![Packaging status](https://repology.org/badge/vertical-allrepos/ol.svg)](https://repology.org/project/ol/versions)
[![Packaging status](https://repology.org/badge/tiny-repos/ol.svg)](https://repology.org/project/ol/versions)
[![latest packaged version(s)](https://repology.org/badge/latest-versions/ol.svg)](https://repology.org/project/ol/versions)
