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

Ol makefiles contain many targets (for building, for testing, etc.) and can be difficult to understand.
Below are listed simple recipes for manual olvm and ol buildings.


### BUILD REQUIREMENTS

You should have GCC 3.2+ (with gcc-multilib and binutils) or CLANG 3.5+ installed.

* MacOS users should have xcode-tools installed.
* Windows support requires MinGW (only cross-compilation is supported, use WSL if you'r building under Windows).
* WebAssembly binary compilation requires llvm.
* 32-bit linux builds requires gcc-multilib.

For the automated builds you should have gnu make, not bsd make.

### BUILD IN SIMPLE WAY

```bash
$ make; make install
```
* Note: use *make uninstall* to completely uninstall Ol.

### BUILD IN MANUAL WAY

#### GNU/Linux

##### Build olvm (ol virtual machine)

```bash
$ cc src/olvm.c  -std=gnu99 -O2  -lm -ldl  -o olvm
```

##### Build ol (with integrated REPL and compiler)

```bash
$ cc src/olvm.c  -std=gnu99 -O2  -lm -ldl  -o ol \
     src/repl.S -DREPL=repl
```

Note: for some cases build can be done in alternate way:
```bash
$ ld -r -b binary -o repl.o repl
$ cc src/olvm.c  -std=gnu99 -O2  -lm -ldl  -o ol \
     repl.o -DREPL=_binary_repl_start
```

##### ~~Build WebAssembly Binaries (in wasm form)~~

~~\$ source {your-emsdk-path}/emsdk_env.sh~~  
~~\$ make olvm.wasm~~

~~This should create olvm.wasm and olvm.js - web assembly ol representation.
An example usage of Ol as a built-in web application you can check at the official [project page](https://yuriy-chumak.github.io/ol/).~~

#### Windows (cross-compile under Linux for Windows)

##### Build olvm (ol virtual machine)

Use `i686-w64-mingw32-gcc` for 32-bit, `x86_64-w64-mingw32-gcc` for 64-bit.
```bash
$ x86_64-w64-mingw32-gcc src/olvm.c  -std=gnu99 -O2 -lm -lws2_32  -o olvm.exe \
                         -Iincludes/win32
```

##### Build ol (with integrated REPL and compiler)
```bash
$ x86_64-w64-mingw32-gcc src/olvm.c  -std=gnu99 -O2 -lm -lws2_32  -o ol.exe \
                         src/repl.S -DREPL=repl -Iincludes/win32
```

#### \*BSDs

You should include "c" library instead of "dl":

##### Build olvm (ol virtual machine)

```bash
$ cc src/olvm.c  -std=gnu99 -O2  -lm -lc  -o olvm
```

##### Build ol (with integrated REPL and compiler)

```bash
$ cc src/olvm.c  -std=gnu99 -O2  -lm -lc  -o ol \
     src/repl.S -DREPL=repl
```


#### Android

```bash
$ ndk-build
```


CUSTOMIZATION
-------------

If you want to enable/disable next olvm features, you can use -Dxxx or -Dxxx=y gcc/llvm syntax.  
You can check such flags at runtime using `(olvm features)` library.

```scheme
> (import (olvm features))

> (vm:feature? HAVE_DLOPEN)  ; can we load shared libraries?
#true
> (vm:feature? OLVM_NOMAIN)  ; are we in embed mode?
#false
```

### Main Olvm Customizations

These variables enable or disable main parts of olvm.

|Variable          |Value            |Meaning                                 |
|------------------|-----------------|----------------------------------------|
|REPL              |no default value |Which binary data file is a REPL        |
|OLVM_NOMAIN       | 1\|0, default 0 |Disable 'main' function                 |
|OLVM_FFI          | 1\|0, default 1 |Enable FFI support                      |
|OLVM_CALLABLES    | 1\|0, default 1 |Enable FFI callbacks support            |
|OLVM_INEXACTS     | 1\|0, default 1 |Enable inexact math support             |
|OLVM_BUILTIN_FMATH| 1\|0, default 1 |Enable builtin vm floating-point math   |
|OLVM_UNSAFES      | 1\|0, default 0 |Enable "unsafe" functionality           |

Next variables depends on OS support and automatically set by the Makefile (like the `configure` script does).
You can override those values, sure.

|Variable      |Value            |Meaning                                     |
|--------------|-----------------|--------------------------------------------|
|HAVE_SOCKETS  | 1\|0, default 1 |Enable sockets support (bind, listen, etc.) |
|HAVE_DLOPEN   | 1\|0, default 1 |Enable dlopen/dlsym functions support       |
|HAVE_SANDBOX  | 1\|0, default 0 |Enable sandbox support (depends on OS)      |
|HAVE_STRFTIME | 1\|0, default 1 |Enable strftime function support            |
|HAVE_SENDFILE | 1\|0, default 1 |Enable sendfile function support            |

Please note that external libraries (like opengl, sqlite, etc.) support require HAVE_DLOPEN and OLVM_FFI enabled.


Next options valid only if OLVM_UNSAFES is enabled

|Variable      |Value                          |Meaning |
|--------------|-------------------------------|--------|
|OLVM_UNSAFE_TVPTR_CAST | 1\|0, default 0 |Enable unsafe vm:cast integer to vptr|

Floating point olvm customization

|Variable         |Value                          |Meaning |
|-----------------|-------------------------------|--------|
|~~OLVM_INEXACT_TYPE~~| float\|double, default double |Internal inexact type representation|

Additionally you can disable the following olvm features by setting the variables to 0 (-Dxxx=0):

|Variable         |Meaning |
|-----------------|--------|
|SYSCALL_SYSINFO  | sysinfo() function usage |
|SYSCALL_SLEEP    | nanosleep() function usage |
|SYSCALL_GETRLIMIT| getrlimit() function usage |
|SYSCALL_GETRUSAGE| getrusage() function usage |


### Debug options

|Variable      |Value            |Meaning                   |
|--------------|-----------------|--------------------------|
|CAR_CHECK     | 1\|0, default 1 |Enable car arguments check|
|CDR_CHECK     | 1\|0, default 1 |Enable cdr arguments check|


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
