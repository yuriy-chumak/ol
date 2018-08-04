```
     `___`  
     (o,o)  
     \)  )  
   ___"_"___
   Otus Lisp

```
[![Join the chat at https://gitter.im/otus-lisp/Lobby](https://badges.gitter.im/otus-lisp/Lobby.svg)](https://gitter.im/otus-lisp/Lobby)
[![Visit the project page](https://yuriy-chumak.github.io/ol/assets/view-project-page.svg)](https://yuriy-chumak.github.io/ol/)
[![Travis-CI project page](https://travis-ci.org/yuriy-chumak/ol.svg)](https://travis-ci.org/yuriy-chumak/ol)


Otus Lisp aka Ol
================

Yet another pet Lisp. Small, embeddable and purely functional!

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

You can use Ol in Linux, Windows, Unixes (macOS, kinds of BSD),
Android, webOS and lot of any other operation systems based on
various hardware architectures (x86, x86_64, arm, aarch64, ppc,
mips, etc).


REQUIREMENTS
------------

You should have GCC >3.2 or CLANG >3.5 installed.
For Windows you should have MINGW.


DOWNLOAD / INSTALLATION
-----------------------

You can use basic Ol functionality without any installation -
just copy the `ol` binary to any user accessible path. Basic
functionality includes a rich set of functions: lists, ff, io,
lazies, strings, symbols, vectors, math, regex, tuples etc.

For extended functionality you should install the whole Ol package.

Installation packages for

* CentOS 6 (x86, amd64), CentOS 7 (amd64)
* Debian 7 (x86, amd64), Debian 8 (x86, amd64),
* Fedora 22 (x86, amd64), Fedora 23 (x86, amd64),
* RHEL 5 (x86, amd64), RHEL 6 (x86, amd64), RHEL 7 (amd64),
* SLE 10 (x86, amd64), SLE 11 (x86, amd64), SLE 12 (amd64),
* ScientificLinux 6 (x86, amd64), ScientificLinux 7 (amd64),
* Univention 3.2 (x86, amd64), Univention 4.0 (x86, amd64),
* openSUSE 13.1 (x86, amd64), openSUSE 13.2 (x86, amd64),
  openSUSE Factory (aarch64, armv7l),
  openSUSE Leap 42.1 (amd64),
  openSUSE Tumbleweed (x86, amd64),
* Ubuntu 12.04 (x86, amd64), Ubuntu 14.04 (x86, amd64, aarch64, armv7l), Ubuntu 16.04 (x86, amd64)

can be found at
[openSUSE Build Service](https://software.opensuse.org/download.html?project=home%3Ayuriy-chumak&package=ol)

Installation packages for

* Windows (x86, amd64)
* Android (arm64-v8a, armeabi, armeabi-v7a,
mips, mips64, x86, x86_64)

can be found at the [![Releases](https://img.shields.io/github/downloads/yuriy-chumak/ol/total.svg)](https://github.com/yuriy-chumak/ol/releases) announcment page.

Precompiled binary for Odroid (Ubuntu 14.04, armv7l) on the way.


BUILD
-----

### BUILD IN SIMPLE WAY

   $ make; make install

* gmake for unix clients


### BUILD IN MORE INTERESTING WAYS

#### Linux way

To build only olvm (virtual machine)

    $ gcc src/olvm.c -DNAKED_VM  -std=c99 -O2  -o vm -ldl

To build full OL

    $ ld -r -b binary -o tmp/repl.o repl
    $ gcc src/olvm.c tmp/repl.o  -std=c99 -O2  -o ol -ldl


Olvm can execute only precompiled OL scripts (see BINARY SCRIPTS
section) and is very small (about 35KB).
Full OL with interpreter, that can execute text lisp scripts is more
fat (about 400KB executable).


#### Windows way

To build only olvm (virtual machine)

    > set PATH=%PATH%;C:\MinGW\bin
    > gcc.exe src\olvm.c -DNAKED_VM -IC:\MinGW\include\ -LC:\MinGW\lib\ -std=c99 -O2  -o ol -lws2_32

To build OL

    > set PATH=%PATH%;C:\MinGW\bin
    > ld -r -b binary -o tmp/repl.o repl
    > gcc.exe src\olvm.c tmp\repl.o -IC:\MinGW\include\ -LC:\MinGW\lib\ -std=c99 -O2  -o ol -lws2_32


#### macOS/FreeBSD/OpenBSD/NetBSD/\*BSD way

It require including "c" library instead of dl:

    $ gcc src/olvm.c -DNAKED_VM  -std=c99 -O2  -o vm -lc

    $ ld -r -b binary -o tmp/repl.o repl
    $ gcc src/olvm.c tmp/repl.o  -std=c99 -O2  -o ol -lc


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


### BUILD IN VERY INTERESTING WAY

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

For embedding OL into your project check the EMBEDDING OL section.


RUNNING
-------

There are few ways to execute ol

#### Interactive mode

    $ ol
    Welcome to Otus Lisp 1.2,
    type ',help' to help, ',quit' to end session.
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
    (define (main . args)
       (print "hello :)") ; anything you want to compile
    )

    (vector->file (list->vector (fasl-encode (lambda (args)
       (apply main args)))) "out.bl")
    --- cut ---

where "out.bl" your output file name

As result will be create binary script, that can be executed
directly by ol or vm:
Now you can execute this code by Ol as full Ol or as "naked" OLvm

    $ ./vm out.bl


FILES
-----

* repl  - the OL interpreter/compiler
* src/olvm.h  - the OL virtual machine header
* src/olvm.c  - the OL vm
* src/ffi.c src/ffi.h  - foreign functions interface implementation
* lang/*.scm  - OL repl and compiler source codes
* owl/*.scm   - various OL libraries
* r5rs/*.scm  - r5rs compliant OL language part
* lib/*.scm   - different external native libraries support (via ffi)
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

OL can be embedded in your project. It supports direct calls for C functions
for 32- and 64-bit platforms with full callables support.

Please check the next code as a sample:

    --- cut ---
    // embedded olvm usage example
    #include "embed.h"

    // embed.h header provides few simplified macroses and functions
    // to easy work with ol virtual machine.
    // For example, make_integer(x) converts C integers into internal
    // olvm format. For other cases please check enbed.h header.

    // olvm embed instance
    ol_t ol;

    int main(int argc, char** argv)
    {
        // result of ol functions call
        uintptr_t r;
        // create new embedded olvm instance
        embed_new(&ol);

        // our embed extension can work in two manner:
        // if eval got only one parameter it returns the value of parameter
        // in other case it makes apply the arguments to first parameter
        // Note: if you want to call expression, but you have not arguments,
        // just use parenthesis, like "(print)" instead of "print"

        // new function declaration:
        r = eval("(define (plus a b) (+ a b))");

        // call the function with arguments
        r = eval("plus", 7, 11);

        assert (r == make_integer(18)); // and check the result

        return 0;
    }
    --- cut ---

Just compile with -DEMBEDDED_VM and link with repl binary.

The complex sample can be found in samples/pacman.


EMBEDDED OL IN DETAIL
---------------------

TBD.


DOCUMENTATION
-------------

Please refer to the [project page](https://yuriy-chumak.github.io/ol/)

Or check the source codes - r5rs/core.owl


RELATED
--------------------------------------------------------------

Copyright (c) 2014 Aki Helin,
Copyright (c) 2014 - 2017 Yuriy Chumak

Grew out of the Owl Lisp by Aki Helin: https://github.com/aoh/owl-lisp

http://groups.csail.mit.edu/mac/projects/scheme/

http://people.csail.mit.edu/jaffer/Scheme (r5rs)


SOME NOTES
----------

* env *special-forms* list of all special forms
* vm *primitives* list of all primitives
* lang *owl-core* basic OL language elements


```
Have a nice day,

   U  ___ u   _      
    \/"_ \/  |"|     
    | | | |U | | u   
.-,_| |_| | \| |/__  
 \_)-\___/   |_____| 
      \\     //  \\  
     (__)   (_")("_) 
```