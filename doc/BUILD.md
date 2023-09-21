```
                  small,
        `___`         embeddable
        (O,O)             and
        \)  )          purely
      ---"-"---     functional!
   O t u s L i s p
```
*Based on Aki Helin's [Owl-Lisp](https://gitlab.com/owl-lisp/owl)*

BUILD
=====

> Note: Since version 2.2.1, the build command line has been changed.
> The variable NAKED_VM is no longer supported. Instead, a new REPL build variable is provided.

### BUILD REQUIREMENTS

You should have gnu make installed.
You should have GCC 3.2+ (with gcc-multilib and binutils) or CLANG 3.5+ installed.

MacOS users should have xcode-tools installed.

Windows support requires MinGW installed. Wine cross-compilation is also supported.

WebAssembly binary compilation requires Emscripten 1.37.40+.

If you want to build 32-bit binaries under 64-bit linux system, do `sudo apt-get install gcc-multilib`.

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

Note: for some platforms, build can be done without using the `xxd` tool or vm itself.
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

|Variable         |Value                          |Meaning |
|-----------------|-------------------------------|--------|
|OLVM_INEXACT_TYPE| float\|double, default double |Internal inexact type representation|

This variables are automatically set by the Makefile (like `configure` script). You can override those values, sure:

|Variable      |Value            |Meaning |
|--------------|-----------------|--------|
|HAVE_SOCKETS  | 1\|0, default 1 |Enable sockets support (bind, listen, socket, etc.)|
|HAS_UNSAFES   | 1\|0, default 1 |Enable "unsafe" external and internal functions|
|HAVE_DLOPEN    | 1\|0, default 1 |Enable dlopen/dlsym functions support|
|HAS_SANDBOX   | 1\|0, default 0 |Enable internal sandbox support (depends on OS kernel)|
|HAVE_STRFTIME  | 1\|0, default 1 |Enable strftime function support|
|HAVE_SENDFILE  | 1\|0, default 1 |Enable sendfile function support|

Please note that external libraries (like opengl, sqlite, etc.) support require HAS_DLOPEN and OLVM_FFI enabled.

Additionally you can disable the following olvm features by setting the variables to 0 (-Dxxx=0):

|Variable         |Meaning |
|-----------------|--------|
|SYSCALL_SYSINFO  | sysinfo() function usage |
|SYSCALL_SLEEP    | nanosleep() function usage |
|SYSCALL_GETRLIMIT| getrlimit() function usage |
|SYSCALL_GETRUSAGE| getrusage() function usage |


CHANGING THE LANGUAGE
---------------------

You can change the Otus Lisp language itself by editing the sources in the lang/ and libraries/ subfolders.

You can change the virtual machine itself by editing the src/vm.scm and src/olvm.c source files.

To build the Otus Lisp language (aka REPL) do:

```bash
$ make recompile
```

This will create a new (in successful way) REPL binary `./repl` containing the compiled ol code.


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
