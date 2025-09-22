```
                  small,
        `___`         embeddable
        (o,O)             and
        \)  )          purely
      ---"-"---     functional!
  O t u s   L i s p
```
*Based on Aki Helin's [Owl-Lisp](https://gitlab.com/owl-lisp/owl)*

[![Github build linux status](https://github.com/yuriy-chumak/ol/workflows/linux%20x86%2Fx64/badge.svg)](https://github.com/yuriy-chumak/ol/actions/workflows/ubuntu-x86+x64.yml)
[![Github build linux status](https://github.com/yuriy-chumak/ol/workflows/linux%20aarch64/badge.svg)](https://github.com/yuriy-chumak/ol/actions/workflows/ubuntu-aarch64.yml)
[![Github build macos status](https://github.com/yuriy-chumak/ol/workflows/macos%20intel%2Fm1/badge.svg)](https://github.com/yuriy-chumak/ol/actions/workflows/macos-intel+m1.yml)
[![Github build windows status](https://github.com/yuriy-chumak/ol/workflows/win32%2fwin64/badge.svg)](https://github.com/yuriy-chumak/ol/actions/workflows/cross-win32+64.yml)
<a href="https://twitter.com/otus_lisp"><img align="right" src="https://img.shields.io/twitter/url/https/twitter.com/otus_lisp.svg?style=social&label=Follow%20%40otus_lisp"></a></br>
<a href="https://mastodon.social/@otus_lisp"><img align="right" src="https://img.shields.io/mastodon/follow/111901537687270934"></a></br>


Otus Lisp, Version 2.6
======================
[![Visit the project page](https://yuriy-chumak.github.io/ol/assets/view-project-page.svg)](https://yuriy-chumak.github.io/ol/)

Otus Lisp (Ol in short) is a purely functional, multi-platform Lisp dialect with built-in FFI, regular expressions and infix math notation support.

Ol implements an extended subset of the R<sup>7</sup>RS Scheme
([PDF](https://small.r7rs.org/attachment/r7rs.pdf)), including
but not limited to some SRFIs. It is tiny (~ 64KB), embeddable,
and cross-platform.  Provides a portable, high-level interface
to call code written in another language (C, Python, Lua, etc).
You can call native OS functions directly from your Ol program.

You can use Ol on Linux, Windows, macOS, Android, Chromebook*,
(Open/Free/Net) BSD, Solaris and other operating systems based
on various hardware architectures (x86, arm, ppc, mips, etc.),
and directly in the web browsers (in WebAssembly form).

* [TOC](#table-of-contents)
* [Otus Lisp Language Reference](doc/reference/README.md)
* [FFI Examples](#ffi)

credits to
----------
[the-man-with-a-golden-mind](https://github.com/the-man-with-a-golden-mind) (ideas, usage, lot of tests),
[nullscm](https://github.com/nullscm) (usage, tests),
Odysseus (tests, ideas, math corrections),
mt (tests, ideas).

Breaking changes (master branch), as part of Ol 2.7 preparation
---------------------------------------------------------------
The following breaking changes have been added to the master branch as part of 2.7 version preparation:
* ❗ internal type names are changed. please, don't use internal type names in your code, use predicates instead
  * `type-string-displatch` type name changed to `type-superstring` (the old name caused confusing)️
    * you shoud use `string?` predicate to check the string,
    * if you want to use older name, add `(define type-string-displatch type-superstring)` as compat layer
  * `type-enum+` changed to `type-value+` (accent on integer as a value)
  * `type-enum-` changed to `type-value-` (same)
  * `type-int+` changed to `type-integer+` (the fewer name trancations, the better)
  * `type-int-` changed to `type-integer-` (same)
* ❗ `values` with an empty argument list now returns *#false* instead of the string "no vals"
* ❗ `sqlite:value` no longer returns the result of *sqlite3_changes()*
  but simply *#false* if nothing was returned
  * use the "RETURNING" statement with INSERT and UPDATE if you want to return something.
* ❗ Welcome invitation changed
* ❗ `procedure?` made r7rs compatible (will now return *#false* for ff's)
* ❗ arity errors changed (made more clear and descriptive)

PACKAGING
---------

[![Packaging status](https://repology.org/badge/vertical-allrepos/ol.svg)](https://repology.org/project/ol/versions)
[![Packaging status](https://repology.org/badge/tiny-repos/ol.svg)](https://repology.org/project/ol/versions)
[![latest packaged version(s)](https://repology.org/badge/latest-versions/ol.svg)](https://repology.org/project/ol/versions)

* Ubuntu (and it's descendants such as Mint, Pop!_OS, Xubuntu, etc.) users may use the official [project ppa](https://launchpad.net/~yuriy-chumak/+archive/ubuntu/ol).
  ```shell
  $ sudo add-apt-repository ppa:yuriy-chumak/ol
  $ sudo apt update
  $ sudo apt install ol
  ```

* ~~**CentOS**, **Debian**, **openSUSE**, **RHEL**, **SL**, **SLE**, **Ubuntu**, **Univention** precompiled packages: [OpenSUSE Build Service](https://software.opensuse.org/download.html?project=home%3Ayuriy-chumak&package=ol).~~

Some additional libraries can be installed using 'kiss' package manager. Instructions available at [**ol-packages** repository](https://github.com/yuriy-chumak/ol-packages).


Q/A
---

1. Q. Some folders **are empty** (i.e. "libraries/OpenGL"), is it ok?  
   A. Yes, it's ok. Some parts of Ol project are separated into their own independent repositories.  
      Use `git clone --recursive` to get a full project. Or `git submodule init; git submodule update` to update existing one.

1. Q. Why no **arrow keys** processing and a history in Ol command line?  
   A. For the gods of simplicity. I recommend to use an [rlwrap](https://github.com/hanslub42/rlwrap) tool  
      (run as `rlwrap ol` in terminal, or add an `alias ol="rlwrap /usr/bin/env ol"` line to your *~/.bashrc* and run as just `ol`).

1. Q. *.. **fatal error**: stdlib.h: No such file or directory.*  
   Q. *.. **fatal error**: bits/libc-header-start.h: No such file or directory.*  
   A. Install gcc `multilib`, i.e.
      * `apt install gcc-multilib` for debian-based,
      * `apk add musl-dev` for alpine-based,
      * etc.

1. Q. */usr/include/linux/errno.h:1:10: fatal error: asm/errno.h: No such file or directory*
   A. Install `linux-libc-dev:i386`

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
   A. Because they are *runes* - universal naming for letters for a wide set of alphabets.

1. Q. Do you have something like "sleep mode"?  
   A. You can store the current REPL session with `,save "filename"` and exit Ol. Then just run `ol filename` later to continue the saved session.

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

   ; you can review result of transformations with ",expand"
   > ,expand (\\  1 + 2 * 3 - sqrt(4) )
   (- (+ 1 (* 2 3)) (sqrt 4))
   ```


Join the online [gitter.im chat](https://gitter.im/otus-lisp/Lobby).
Alternatively the Libera.Chat [#otus-lisp](https://web.libera.chat/#otus-lisp) (alternate [lightweight](https://web.libera.chat/gamja/#otus-lisp) web-client) channel is available (the previous Freenode channel is closed).

[The Issues](https://github.com/yuriy-chumak/ol/issues) github page waiting for your bug reports and issues.


TABLE OF CONTENTS
-----------------
1. [Packaging](#packaging)
1. [Build/Run/Install](#build--run--install)
   * [Advanced](doc/BUILD.md)
   * [Cross-Compilation](doc/CROSS-COMPILATION.md)
   * [Embedding](#embedding-ol)
1. [Running](#running)
1. [Language Reference](doc/reference/README.md)
   * [R<sup>7</sup>RS Differences](#r7rs-differences)
   * [Samples, Tests, Learning](#learning)
   * [Deprecations](#deprecations)
1. [FFI](#ffi)
   * [Advanced](extensions/FFI.md)
1. [Hacking](#hacking)
1. [Lisp sources in binary form](#binary-scripts)
1. [Files](#files), [Docs](#documentation)
1. [License](#license)


BUILD / RUN / INSTALL
---------------------

#### BUILD REQUIREMENTS

* GCC 3.2+ / CLANG 3.5+
* GNU MAKE


#### BUILD

```
$ make
```
Advanced build instructions: [doc/BUILD.md](doc/BUILD.md)


#### RUN
```
$ ./ol
Welcome to Otus Lisp 2.6
type ',help' to help, ',quit' to end session.
> 
```

The Ol binary includes a rich set of features (lists, vectors and bytevectors, ansi and unicode strings, infinite precision math, associative arrays aka "ff"s, i/o streams, lazy evaluations, regular expressions, continuations, exceptions, lexer parsers, async functions and actors, etc.) and can be used as a completely standalone.


#### INSTALL
```
$ sudo make install
```

Note: by default we use */usr/bin/ol* for Ol binary, and */usr/lib/ol/* folder for Ol libraries.


#### UNINSTALL
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

You can find working Ol examples at:
* [Standard procedures](doc/reference/) list (constantly improving),
* [RosettaCode](http://rosettacode.org/wiki/Category:Ol) Ol page,
* [Tests](tests/) and [Examples](examples/) repository folders,
* Android example code available at [android](examples/android/) folder.


Additionally,
* "LogicWire" and "Digital rain" examples demonstrates native libraries direct usage (the [OpenGL](http://www.opengl.org/)):
  * https://github.com/yuriy-chumak/ol/tree/master/examples/LogicWire ([idea source](https://realhet.wordpress.com/2015/09/02/bitmap-logic-simulator/))
    <img src="https://raw.githubusercontent.com/yuriy-chumak/ol/master/doc/img/2023-04-10-21-47.gif" width="50%" height="50%">
  * https://github.com/yuriy-chumak/ol/tree/master/examples/Matrix
    !["digital rain" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/digital-rain.png)

* "Pacman" sample demonstrates embedding Ol scripts in native "C" code - https://github.com/yuriy-chumak/ol/tree/master/examples/pacman  
  !["pacman" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/pacman.png)

* "Newton dynamics" sample demonstrates extended native libraries usage (the [newton-dynamics](http://newtondynamics.com), physical simulation engine) with callbacks (C to Lisp automatic translation) - https://github.com/yuriy-chumak/ol/tree/master/examples/Newton.
You should have compiled [newton-dynamics.so](https://github.com/MADEAPPS/newton-dynamics) core library.  
  !["newton" screenshot](https://raw.githubusercontent.com/yuriy-chumak/ol/gh-pages/assets/ol/newton.png)


R<sup>7</sup>RS DIFFERENCES
---------------------------

The most important differences are:

* Ol is definitely **case sensitive**.
* Numbers WITHOUT PRECISION considered to be **exact** in Ol, but *inexact* in Scheme.
  - `integer?` for inexact numbers always returns **#false** in Ol.
  - note: Use `inexact` function to convert number into inexact form, or prefix number with `#i` directly (like `#i0.123` for inexact 0.123).
* **No** `set!` in Ol (Ol is purely functional!),
  - note: Use `define`, `define-values`, `let`, `let*`, `letrec`, and `letrec*` instead.
  - note: Limited support of `set-car!`, `set-cdr!`, and `set-ref!` functions [are provided](doc/reference/pairs-and-lists.md#set-car), but is not recommended to be used.
  - note: Inexact numbers can be changed with `vm:set!`, but is not recommended to be used.
  - note: Dynamic variables are available via `(scheme dynamic-bindings)` [library](doc/reference/dynamic-bindings.md).
* CHARACTERS in Ol are **small numbers** (aka 'enums'), but special *character* type in Scheme.
  - note: Ol supports the full Unicode 15.0.0 (2022 Sep 13) character set.
  - note: To write a character use `write-char`, otherwise you'll write a number.
* NEGATIVE indices in `substring` are **valid** in Ol (means "from the end of string", -1 means a last *rune*).
* NEGATIVE vector indices are **valid** in Ol (means "from the end of vector", -1 means a last element).
* Ol has **extended form** of `case` (with vectors support),
* Ol has **extended form** of `if` (with `then` and `else` [keywords](doc/reference/)),
* Ol has **builtin dictionary** numeric and symbolic keys (the [reference](doc/reference/ffs.md) page),
* Ol has builtin **regular expressions** (the [reference](doc/reference/regex.md) page),
* Ol has an awfully **powerful macro system** in addition to Scheme's hygienic one (a brief [notes and examples](doc/reference/macros.md)).
* `apply` arguments count is **limited to 249** in Ol (use a `fold` otherwise).

The nearly full differences list can be found in [doc/R7RS-DIFFERENCES.md](doc/R7RS-DIFFERENCES.md).

### Supported SRFI
* srfi-0 - `cond-expand`, builtin
* srfi-16 - `case-lambda`, builtin
* srfi-71 - `(let* ((a b (values..`, builtin
* srfi-87 - `<=` in `case`, builtin


DEPRECATIONS
------------

* 2.6 -> 2.7 (which is planned, but i'm not sure about)
  - i'm thinking about changing `(wait ms)` to `(wait s)`, it means the seconds usage instead of milliseconds.

* 2.5 -> 2.6
  - feature `ol-2.5` changed to `ol-2.6`.
  - removed deprecated libraries `(scheme srfi-Nnn)`, use `(srfi Nnn)` instead,
  - i/o scheduler enabled by default for reads,
  - `write` function made r7rs-compliant,
  - `(owl format)` moved to `(otus format)`.

* 2.4 -> 2.5
  - feature `ol-2.4` changed to `ol-2.5`.
  - `(system args port...)` changed to `(execvp args port...)`. New `(system command)` introduced.
  - `(OpenGL version-X.Y)` libraries changed to `(OpenGL X.Y)`.
  - `fft-size-t` changed to `fft-size_t`.
  - `HAS_...` build variables changed to convenient `HAVE_...`.

* 2.3 -> 2.4
  - `(ilist ...)` is deprecated. Use `(cons* ...)` instead.
  - `(interact ...)` from (owl ~~interop~~ async) is deprecated. Use `(await (mail ...))` instead.
  - `(fork ...)`, `(fork-named ...)`, `(fork-server ...)` is deprecated. Use `(async ...)`, `(async 'name ...)`, `(actor ...)` instead.


FFI
---

Using external "asin" function instead of built-in one:
```scheme
; import ffi library
> (import (otus ffi))

; load binary shared module (so or dll)
> (define LIBM (load-dynamic-library "libm.so.6"))

; declare external function prototype
> (define asin (LIBM fft-double "asin" fft-double))

; use the external function
> (print (asin 0.5))
0.523598775
```

Windows UI example:
```scheme
> (import (otus ffi))
> (define USER32 (load-dynamic-library "user32"))
> (define MessageBox (USER32 fft-int "MessageBoxA" type-vptr type-string type-string fft-int))

; use the external function
> (MessageBox #f "hello world" "message box" 1)
1  ; 2 if you clicked "Cancel"
```

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
* `--sandbox=Nnn`: execution in the sandbox with "Nnn" Megs of memory preallocated
* `--interactive`: force REPL interactive mode
* `--non-interactive`: disable REPL interactive mode
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

OL can execute precompiled scripts. While text lisp programs require REPL (400KB), libraries, and time to compile,
the binary code needs only olvm (90K with FFI, 60K without FFI) and is ready to run the code immediately.

You can compile your script using next code as a template:

`template.scm`:
```scheme
; put anything you want to compile in a lambda
(define (main args)
   (print "arguments: " args)

   (define out
   (let faktr ((x (if (null? args)
                     13
                     (string->number (first args)))))
      (if (= x 1)
         1
         (* x (faktr (- x 1))))))

   (print "factorial: " out)

   ; return execution result
   ; (let it be number of digits in the out)
   (ilog 10 out))

; compile and save this lambda into binary file
(fasl-save main "out.bl")
```

Let's compile and check the output:
```bash
$ ol template.scm

$ ls -l out.bl
-rw------- 1 user user 55549 Jul 21 23:13 out.bl

$ xxd ./out.bl
00000000: 0203 012b 0203 012d 0203 013d 0203 012a  ...+...-...=...*
00000010: 0203 013c 0203 023c 3c02 0302 3e3e 0203  ...<...<<...>>..
00000020: 0474 7970 6502 0303 6164 6401 0401 0901  .type...add.....
00000030: 0401 0901 0401 0801 0401 0a01 0401 0901  ................
                      ..........
0000d8c0: 0d03 0206 0111 0111 0501 c50e 0c08 0202  ................
0000d8d0: 1022 0b02 001d 0101 0205 0312 0401 0304  ."..............
0000d8e0: 0306 0101 0407 0505 0804 0505 0603 0704  ................
0000d8f0: 0208 0311 0111 0401 9a01 0210 00         .............
```

Now you can use this binary code anywhere without changes, even under another OS and/or platform, even with embed olvm code.

```bash
# fastrun with ol virtual machine
$ olvm ./out.bl
arguments: ()
factorial: 6227020800

# try with arguments and print execution result
$ ol ./out.bl 42; echo returned: $?
arguments: (42)
factorial: 1405006117752879898543142606244511569936384000000000
returned: 52

# regular ol can do it too
$ ol ./out.bl 7; echo returned: $?
arguments: (7)
factorial: 5040
returned: 4
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

Please refer to the [embedding sample README](https://github.com/yuriy-chumak/ol/blob/master/examples/pacman/README.md).


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
Copyright (c) 2014-2025 Yuriy Chumak                      <br/>


----------------------------------------------------------------------

Grew out of the Owl Lisp by Aki Helin: https://gitlab.com/owl-lisp/owl

Thanks to:
* [the-man-with-a-golden-mind](https://github.com/the-man-with-a-golden-mind) for the tests, ideas, and questions.

Resources:
* http://groups.csail.mit.edu/mac/projects/scheme/
* http://people.csail.mit.edu/jaffer/Scheme
* http://r7rs.org
