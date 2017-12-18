Otus Lisp
=========

[![Join the chat at https://gitter.im/otus-lisp/Lobby](https://badges.gitter.im/otus-lisp/Lobby.svg)](https://gitter.im/otus-lisp/Lobby)
[![Visit the project page](https://yuriy-chumak.github.io/ol/assets/view-project-page.svg)](https://yuriy-chumak.github.io/ol/)

Yet another pet lisp. Small, embeddable and purely functional.

VERSION 1.2

LICENSE
--------------------------------------------------------------

This program is free software;  you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU GPL along with this
program. If not, see <http://www.gnu.org/licenses/>.

Additionally, Lesser GNU General Public License is applicable.

OVERVIEW
--------

Otus Lisp (Ol in short) is a purely functional dialect of Lisp.

It implements an extended subset of R5RS Scheme including, but
not limited to, some of the SRFIs. It's tiny(42kb), embeddable
and crossplatform; can run in own sandbox; provides a portable,
highlevel way for using the code written in another languages.

You can use Otus Lisp in Linux, Windows, Unix, Android and lot
of any other operation systems with various (x86, x86_64, mips,
arm, aarch64, ppc, etc.) architectures.


REQUIREMENTS
------------

For unix you should have GCC >3.2 installed.

For windows you should have MINGW installed.

For unix you maybe want MAKE installed also.


DOWNLOAD / INSTALLATION
-----------------------

You can use basic Ol functionality without any installation - just copy the `ol` binary
to any user accessible path. Basic functionality includes a rich set: lists, ff, io, lazies,
strings, symbols, vectors, math, regex and tuples. For extended functionality you should
install the whole Ol package.

Installation packages for
CentOS 6 (x86, amd64), CentOS 7 (amd64),
Debian 7 (x86, amd64), Debian 8 (x86, amd64),
Fedora 22 (x86, amd64), Fedora 23 (x86, amd64),
RHEL 5 (x86, amd64), RHEL 6 (x86, amd64), RHEL 7 (amd64),
SLE 10 (x86, amd64), SLE 11 (x86, amd64), SLE 12 (amd64),
ScientificLinux 6 (x86, amd64), ScientificLinux 7 (amd64),
Univention 3.2 (x86, amd64), Univention 4.0 (x86, amd64),
openSUSE 13.1 (x86, amd64), openSUSE 13.2 (x86, amd64),
openSUSE Factory (aarch64, armv7l),
openSUSE Leap 42.1 (amd64),
openSUSE Tumbleweed (x86, amd64),
Ubuntu 12.04 (x86, amd64), Ubuntu 14.04 (x86, amd64, aarch64, armv7l), Ubuntu 16.04 (x86, amd64) can be found at
[openSUSE Build Service](https://software.opensuse.org/download.html?project=home%3Ayuriy-chumak&package=ol)

Installation packages for Windows (x86, amd64) on the way.

Precompiled binaries for Android (arm64-v8a, armeabi, armeabi-v7a,
mips, mips64, x86, x86_64) on the way.

Precompiled binary for Odroid (Ubuntu 14.04, armv7l) on the way.


BUILD
-----

### SIMPLE WAY

   $ make; make install

* gmake for unix clients


### MORE INTERESTING WAYS

#### Linux way

To build only olvm (virtual machine)

    $ gcc src/olvm.c -DNAKED_VM  -std=c99 -O2  -o vm -ldl

To build full OL

    $ ld -r -b binary -o src/repl.o repl
    $ gcc src/olvm.c src/repl.o  -std=c99 -O2  -o ol -ldl


Olvm can execute only precompiled OL scripts (see BINARY SCRIPTS
section) and is very small (about 35KB).
Full OL with interpreter, that can execute text lisp scripts, more
fat (about 400KB executable).


#### Windows way

To build only olvm (virtual machine)

    > set PATH=%PATH%;C:\MinGW\bin
    > gcc.exe src\olvm.c -DNAKED_VM -IC:\MinGW\include\ -LC:\MinGW\lib\ -std=c99 -O2  -o ol -lws2_32

To build OL

    > set PATH=%PATH%;C:\MinGW\bin
    > ld -r -b binary -o src/repl.o repl
    > gcc.exe src\olvm.c src\boot.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -std=c99 -O2  -o ol -lws2_32


#### FreeBSD/OpenBSD/NetBSD way

BSD require including "c" library instead of dl:

    $ gcc src/olvm.c -DNAKED_VM  -std=c99 -O2  -o vm -lc

    $ ld -r -b binary -o src/repl.o repl
    $ gcc src/olvm.c src/boot.c  -std=c99 -O2  -o ol -lc


#### Android way

    $ ndk-build

#### MacOS/iOS way

   tbd.

#### Open webOS way

Put toolchain/ol.bb bitbake receipe into any place of open webOs
receipes folder (i.e. ./meta-oe/) and run "make ol" from root
open webOs folder.

Upload ol executable from BUILD/work/<build-name>/ol/1.0.0-1-r0/build
to machine /bin.

Now you cat execute ol under webos command line or other way you
would like.


### VERY INTERESTING WAY

You can change OL scheme language (yes, you can) by editing sources in
lang/ and owl/ subfolders. Additionally you can change virtual machine
by editing src/vm.scm and src/olvm.c source files.

To build OL language (not OL virtual machine)

    $ make recompile

This will create new (in successfull way) repl binary that contains ol
interpreter code

Few words about OL can be found in documentation on the project page.


SPEEDUP
-------

You can disable most internal checks for speedup by editing the olvm.h
header file - for example you can uncomment "#define CAR_CHECK(arg) 1"
macro to disable the virtual machine internal checks for car validness.
Please, be aware - this can make olvm less stable!


ADDITIONAL OL VM FEATURES
-------------------------

To disable sockets support (linux and windows) you can please add
-DHAS_SOCKETS=0 to gcc command line or set appropriate define in
src/olvm.h to 0. You can remove -lws2_32 for Windows from command
line additionally.

To disable dlopen/dlsym (LoadLibrary/GetProcAddress for windows)
support you can add -DHAS_DLOPEN=0 to gcc command line or set
appropriate define in src/olvm.h to 1. You can remove -ldl for
Linux from command line additionally.

Please note that external libraries (like opengl, sqlite, etc.)
support require HAS_DLOPEN enabled.

To disable ffi (Foreign Function Interface) support you can add
-DOLVM_FFI=0 to gcc command line or set appropriate define in
src/olvm.h to 0.

Please note that external libraries (like opengl, sqlite, etc.)
support require OLVM_FFI enabled.

For embedding OL into your project check the EMBEDDED OL section.


RUNNING
-------

There are few ways to execute ol

#### Interactive mode

    $ ol
    You see a prompt.
    Type ',help' to help, ',quit' to end session
    > (+ 1 2 3)                      # now you in REPL and can play with in
    6
    > ,quit                          # this ends interactive session
    bye bye :/

#### Unattended mode

    $ ol scriptname.ol                # text script
    $ ol scriptname.bl                # binary (compiled) script
    $ echo '(print (+ 1 2 3))' | ol

For Windows

    > echo (print (+ 1 2 3)) | ol


BINARY SCRIPTS
--------------

OL can execute precompiled scripts. You can compile your script
by using this code

    --- cut ---
    (define (main)
       #true ; anything you want to compile
    )

    (vector->file (list->vector (fasl-encode (lambda (args)
       (main)))) "out.bl")
    --- cut ---

where "out.bl" your output file name

As result will be create binary script, that can be executed
directly by ol or vm:

    ---cut ---
    0000000000: 02 10 0C 19 02 00 07 01 | 01 02 05 02 05 01 11 02
    0000000010: 10 09 19 01 00 04 8D 04 | 18 04 11 01 11 02 02 01
    0000000020: 00                      |
    ---cut ---

Now you can execute this code by OL as full OL or as "naked" OLvm

    $ ol out.bl


FILES
-----

* src/olvm.h  - the OL virtual machine header
* src/olvm.c  - the OL vm
* src/boot.c  - the OL interpreter/compiler
* lang/*.scm  - implementation of OL repl and compiler
* owl/*.scm   - implementation of OL libraries
* r5rs/*.scm  - r5rs compliant OL language part
* tests/*.scm - some automated tests


TRICKS
------

Register interpreter in the ms windows:

    assoc .ol=OLisp.File
    ftype OLisp.File=ol "%1" %*
    assoc .bl=OLisp.Binary.File
    ftype OLisp.Binary.File=ol "%1" %*

Register interpreter in the linux:

    #!/usr/bin/ol


USAGE
-----

OL (Otus Lisp) can be used either interactively, or to interpret code
from files, or compile programs to fasl-images. The difference between
an ol program and a plain script is that the program should just have
a function of one argument as the last value, which will be called with
the command line argument list when the program is executed.


EMBEDDED OL
-----------

(please, this section is outdated, please wait to updates)

OL can be embedded in your project. It supports direct calls for C functions
for 32- and 64-bit platforms with full callables support.

Please check the next code for sample usage:

    --- cut ---
    // embedded example
    #include "olvm.h"

    #include <stdio.h>

    #if _WIN32
    __declspec(dllexport)
    #else
    __attribute__((__visibility__("default")))
    #endif
    int sample_add(int fa, int fb)
    {
	    // math
	    int a = fa;    fprintf(stderr, "DEBUG: a = %d\n", a);
	    int b = fb;    fprintf(stderr, "DEBUG: b = %d\n", b);
	    int r = a + b; fprintf(stderr, "DEBUG: r = %d\n", r);

	    // result
	    return r;
    }

    int main(int argc, char** argv)
    {
        OL* ol = OL_new(
            "(import (otus ffi) (owl io))"
            "(define $ (dlopen))" // get own handle
            "(define sample_add (dlsym $ type-int+ \"sample_add\" type-int+ type-int+))"

            "(print \"sample_add: \""
            "   (sample_add 1 2))");
        OL_run(ol, 0, 0);

        OL_free(ol);
    }
    --- cut ---

You need:

a. compile OL with -DHAS_DLOPEN and -DOLVM_FFI options
      (don't forget for -DEMBEDDED_VM)

b. notify vm about using ffi library: "(import (otus ffi))"
      "otus/ffi.scm" file must be accessible from your executable or from
      default library search path

c. load function exporter library as "(define any-variable (dload))" for self
      executable or "(define any-variable (dload \"libmy.so\"))" for "my"
      dynamic library (my.dll for windows)

d. notify vm about function prototypes that you want to call. ffi support mechanism
      provides smart translation from internal OL data format into machine
      specific that used by common languages (C in sample).

FFI described in the [Project Page](http://yuriy-chumak.github.io/ol/?ru/ffi) (still in progress), so for now function interface declaration from sample in details:

'(define sample_add (dlsym $ type-int+ "sample_add" type-int+ type-int+))':

 * sample_add - any appropriate variable name for your internal OL function name.
      This variable will be associated with lambda that can call the native function.
 * second argument in (dlsym) - type-int+ - is return type of native function, it
      can be type-int+ for integers, type-string for string, etc.
      available types you can check in otus/ffi.scm
 * third argument is function name string. you must export this
      function from your code to make it accessible from OL
 * next dlsym args is argument types for native function, ffi
      will try to convert arguments of sample_add lambda to this type.

e) well, now you can freely use your native function like this for example
      '(print (sample_add 1 2))'

More information about ffi you can get in source files
    lib/sqlite.scm
    lib/opengl.scm and OpenGL/version-x-x.scm
    lib/winapi.scm
    lib/x11.scm

* All embedded OL API in progress and can be changed in feature.


DOCUMENTATION
-------------

Please refer to the [project page](yuriy-chumak.github.io/ol/)

Or check the source codes - r5rs/core.owl

RELATED
--------------------------------------------------------------

http://groups.csail.mit.edu/mac/projects/scheme/

http://people.csail.mit.edu/jaffer/Scheme (r5rs)

Copyright (c) 2014 Aki Helin,
Copyright (c) 2014 - 2017 Yuriy Chumak

Grew out of the Owl Lisp by Aki Helin.
Original Owl Lisp project can be found at
https://github.com/aoh/owl-lisp

SOME NOTES
----------

* env *special-forms* list of all special forms
* vm *primitives* list of all primitives
* lang *owl-core* basic OL language elements
