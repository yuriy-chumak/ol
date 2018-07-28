---
layout: page
title:  Summary
categories: en
---
> Any sufficiently complicated program contains an ad-hoc, informally-specified, bug-ridden, slow implementation of half of some Lisp dialect.
> <br/> <span style="float: right;">Greenspun's tenth rule</span>
<br/>

**Otus Lisp** (Ol in short) is a purely[\*](#pure) functional dialect of Lisp.

It implements an extended subset of [R<sup>5</sup>RS](http://www.schemers.org/Documents/Standards/R5RS/) Scheme including, but not limited to, some of the [SRFI](http://srfi.schemers.org/). It's small (only 42kb[\*](#42kb)), embeddable and crossplatform; can run in own sandbox (for systems with sandboxing support); provides a portable, high level way for calling the code written in another languages. You can use Otus Lisp on Linux, Windows, Unix, Android and many other operation systems with various (i.e. x86, x86_64, arm, aarch64, mips, ppc) architectures.


### Downloads
Release **1.2** available to [download](https://github.com/yuriy-chumak/ol/releases). Happy LISPing!

Preparing version **2.0** which moving to r7rs with changes in ffi and embed.


### Already tested platforms
- [x] x86: 80486, pentium, pentium 2, pentium 3, athlon, core 2 quad, core i3, core i5, core i7.
- [x] x86_64: core 2 quad, core i3, core i5, core i7.
- [x] aarch64: cortex-a53, cortex-a57.
- [x] arm: arm920t, snapdragon 801.
  - nearly planned, but not yet tested: arm926t, arm1136, cortex-a7, cortex-a9, cortex-a15, cortex-m3, cortex-m4.
- [x] mips32: algor (P-5064)
- fully supported, but not yet tested: m68k, microblaze, mips64, or1k, ppc, ppc64, sh4, spark, spark64, ztensa.

### Tested operation systems/devices
- [x] GNU/Linux: CentOS, Debian, Fedora, RHEL, SLE, ScientificLinux, Uninvention, openSUSE, Ubuntu.
- [x] Windows: Windows 95, Windows 98, Windows ME, Windows NT, Windows 2000, Windows XP, Windows Vista, Windows 7, Windows 8, Windows 10.
- [x] Unix: OpenBSD, FreeBSD, NetBSD.
- [x] Android: all versions up to Nougat.
- [x] webOS: 2.0.
- [x] Odroid: C1.


### About project
Distributed under [LGPLv3](https://github.com/yuriy-chumak/ol/blob/master/COPYING.LESSER) license.

This is incomplete list of Ol features:

* small and fast virtual machine (only 42kb[\*](#42kb) in binary code)
* high efficient (thanks for pure language functionality) garbage collector
* cross-platform (Linux, Windows, Unix, different BSD flavors, Android, etc.)
* cross-architecture (i586, amd64, arm, mips, etc.)
* 32- and 64-bit platforms support
* text scripts can be compiled into binary form; can execute this binaries directly by virtual machine
* dynamic calls of "native" functions (from system libraries or other languages); can be invoked from this functions using transparent callback mechanism
* can work in own sandbox
* full range of interesting and useful features from function world:
  * continuations
  * tail recursion
  * subprograms
  * function as first class objects
* limited mutators as small pleasant addition from imperative world
* and, at least, it's real Lisp!

You can immediately try Otus Lisp in the provided terminal on the left of this page without downloading and installing any binaries. For the more information please refer to the "Learn" paragraph at the bottom of this page.


### Development status <a name="news"></a>
* 2018
  * a lot of changes, full list can be obtained [on github](https://github.com/yuriy-chumak/ol/commits/master).

* Thr 28 Dec 2017
  * prepared 1.2 build!
  * added NewtonSetSolverModel
  * fix for talkback (moved to new ffi type system)

* Wed 27 Dec 2017
  * lib/newton moved to new ffi type system
  * newton tutorial update
  * added travis-ci integration, now you can see current "buildable" state
  * removed tempnam warning

* Mon 18 Dec 2017
  * added badge for project page into readme, updated download/installation section
  * added utf-8 in lib/http requests processing
  * rebuilt repl - decreased size for 2к
  * added unicode support to lib/sqlite

* Thr 14 Dec 2017
  * added into opengl sample exit for 'Q' key for windows
  * now sqlite understands NULL
  * added function sqlite:map to sqlite
  * now sqlite returns #false in case of query fail

* Wed 13 Dec 2017
  * now #false in universal value for "i don't know" for ffi parameters
  * lib/x11 moved to new ffi type system
  * opengl 2.0 moved to new ffi type system
  * added new ffi type system more tests
  * removed ffi32 from "clean" builds vm32 and vm64
  * cleaned up opengl/version-1-0 from opengl 1.1 content
  * add opengl-1-1 constants declared
  * change opengl extension loader example
  * universal lib/opengl library moved from opengl 1.0 to opengl 1.1
  * added opengl 1.2
  * disabled unused tests
  * change sqlite usage sample

* Mon 11 Dec 2017
  * continue moving project to new ffi type system
  * renamed ffi to OL_ffi
  * added new experimental 8-bits ffi type
  * changed defines in build scripts
  * large update of lib/sqlite - it moved to new ffi type system, more function, more tests
  * added sqlite:for-each into lib/sqlite, it's very good to have an amount of lisp-like proimitives in database
  * added gl:CreateProgram into OpenGL
  * updated sdl2 by events
  * and moved sdl2 to new ffi types system
  * and moved opengl to new ffi types system
  * slowly, accurate and carefully OpenGL/version-1-0 made official specification compliant

* Wed 29 Nov 2017
  * large ffi change - now ffi correctly works with large signed and unsigned values

* Tue 28 Nov 2017
  * large ffi change - new type system, described by using fft- (Foreing Funtion Types) prefix and new working logic.
  * added reference type to the fft
  * moving old types into new
  * type-void* renamed to fft-void*
  * type-float renamed to fft-float, type-double to fft-double, type-unkown and type-any now fft-unknown and fft-any respectively.

* Mon 27 Nov 2017
  * added inexacts into ffi - now we can directly export the floats
  * added glGenLists into OpenGL

* Sun 26 Nov 2017
  * removed '() from type-vptr.
  * continue removing the magic numbers
  * added glNormal3fv to OpenGL

* Sat 25 Nov 2017
  * cleand up (fd->exp-stream), few bytes economy, few code clearness
  * removed old checking for empty lists logic from ffi - will use #false

* Fri 24 Nov 2017
  * fix lib/sdl2 for ubuntu
  * added basic srfi-33 (binary operation)

* Wed 22 Nov 2017
  * into lib/sdl2 added new functions to load images

* Tue 21 Nov 2017
  * added SDL2 support (lib/sdl2)
  * continue polishing the ffi (still something wrong with it, i'm feeling)

* Tue 14 Nov 2017
  * huh, virtual machine speedup approx 5% - just splitted the JAF instruction (with branch in it) to two and without branching (JAF and JAFX).
  * regilar project cleanup

* Fri 10 Nov 2017
  * just a break - added tick-tack-toe sample
  * changed welcome message - now ol show the itself version in welcome

* Thr 9 Nov 2017
  * in talkback added usage sample - pacman like screen

* Tue 7 Nov 2017
  * finally update android build scripts - now it build under all supported by android platforms. can say that android.mk implementation is very shitty

* Mon 6 Nov 2017
  * synchronized license texts
  * now ffi can be included into talkback
  * fix for win32 sockets again. why posix sockets support works fine from the first implementation, and a lot of win32 changes required?
  * into talkback added example of errors processing

* Fri 3 Nov 2017
  * sontinue simplifying the talkback
  * removed from ffi type system type-userdata (as unneded), added added type-unkown (as needed) - last one type does not transform the argument, just send it value to recipient
  * changes sys/getenv function
  * added more doxygen commants
  * continue removing the mugic numbers from the code

* Thr 2 Nov 2017
  * win32 fix for async pipes - win32..., win32... again
  * talkback interface changed to the async pipes, thinking about including the talkback into main ol distibution

* Sun 29 Oct 2017
  * large math fix - now gcd not use lookup table. speed of math does not changed, but image size decreased for 2k and no more problems with 32/64 bit large math
  * tests now can work for 32- and 64-bit simulatenously

* Tue 24 Oct 2017
  * fix for  write for Win32 - win32..., win32...
  * renamed ifary into either and bind into letq, just check what we will have
  * srfi-87 integrated into r5rs/core
  * vm fxmax renamed to vm:maxvalue and fxmbits renamed to vm:valuewidth
  * ffi tests enabled again

* Mon 23 Oct 2017
  * fir for 64-bit math
  * more talkback changes

* Wed 18 Oct 2017
  * continue changing the talkback interface

* Fri 13 Oct 2017
  * added dosygen, hope now comments will collect into documentation (comments still in progress, sure)

* Wed 11 Oct 2017
  * updated build scripts for windows
  * renamed C:\\Program Files\\OL from paths, now path fully gets from OL_HOME system variable
  * returned possibility to work in browser for Ol (via asm.js)

* Mon 3 Oct 2017
  * license changed from GPLv3 to LGPLv3

* Mon 2 Oct 2017
  * major bug fix: resource consumption decreased, it was blocking issue before release 1.2, so let's prepare the 1.2 release

* Sun 1 Oct 2017
  * c++ interface fix
  * new std handles changing mechanism

* Thr 28 Sep 2017
  * updated android compatibility

* Thr 21 Sep 2017
  * (list-ref) now a part of (r5rs core) library
  * couple of code cleanups and function movings

* Wed 20 Sep 2017
  * more inexact functions - fmax fmin

* Tue 19 Sep 2017
  * now dlopen works silently under windows
  * removed src/boot.c file from repository, this will require one more step to compile ol, but save 1,5M in repository every time that language was changed. why I do not do this at project begin?
  * new srfi-1 (xcons) implementation
  * native api change: removed optional destructor from OL_new() call
  * pinvoke renamed to more usual ffi, good by c# associations :)
  * additionally type-callback renamed to type-callable, this is more convenient name, sure
  * couple of inexact numbers support fixes

* Mon 18 Sep 2017
  * GC improvement (gc clears the used registers, that allows to free additionally up to 1.5% of freed memory)
  * new srfi-1 (filter) implementation

* Mon 7 Aug 2017
  * added inexact fsqrt, fsin, fcos, fadd, fsub, fmul, fdiv functions
  * ol can print inexact numbers in human readable format (limited functionality)

* Mon 31 Jul 2017
  * added basic inexact numbers support, enabling using OLVM_INEXACTS=1 define

* Mon 25 Jul 2017
  * added internal pinvoke test

* Mon 24 Jul 2017
  * basic API changed!!!
    * apply-value changed to values-apply, to the right name
    * ol:let changed to bind
  * public API changed!!!
    * OL_eval changed to OL_run
  * vm:run moved from lang/thread to basic library src/vm
  * lang/thread renamed to lang/threading

* Fri 21 Jul 2017
  * fix for broken library support test
  * small code refactoring, clenaup, less magic numbers
  * basic API changed!!!
    * vm:raw renamed to vm:new-raw-object
    * unreel renamed to vm:new-object
    * raw? renamed to vm:raw?

* Tue 18 Jul 2017
  * small gc changes, increased gc stability
  * added system call pipe (for win32 too)
  * now ol prints the port number like #[fd 123]
  * small sandbox logic changes in source code (not the logic, only source code)

* Mon 17 Jul 2017
  * basic tcl/tk bindings now for linux too
  * changes public API!!!
    * OL_eval returns raw data, now caller can analyze the result by itself
  * added more internal VM tests
  * added sqlite extensions support, now it can be written in ol
  * added example of sqlite extension in ol
  * internal API changed!!!
    * callback creation syscall number changed from 175 to 85

* Thr 6 Jul 2017
  * tested ol under aarch64 and arm versatile, it works

* Fri 30 Jun 2017
  * added support of [Minoca OS](https://www.minocacorp.com/) - interesting small OS for connected devices

* Thr 22 Jun 2017
  * started process of integration in OL inrational (inexact) numbers.

* Fri 12 Jun 2017
  * started new project with based on OL backend, this will help me improve the ol vm and libraries, but i'll spent less time to the ol. but will see)

* Fri 12 Jun 2017
  * fixed function (read)

* Thr 11 Jun 2017
  * now sqlite can print error text

* Wed 10 Jun 2017
  * now http parser can parse vectors in query line
  * added null setter to the sqlite

* Tue 9 Jun 2017
  * more error codes in sqlite
  * new json printer
  * into sqlite library added complex function sqlite:query from test project, will see

* Mon 8 Jun 2017
  * added constraints support into sqlite library

* Sat 6 Jun 2017
  * new syscall "unlink", now ol can remove files

* Tue 2 Jun 2017
  * changed http parser, now it can parse command line and create hash-table with pair key-value
  * fix for syscall "sendfile", now sendfile works proper
  * new function yield

* Wed 31 May 2017
  * vm unit-tests enabled, added to common automation testing place

* Fri 26 May 2017
  * common project cleanup, disabled and invalid tests moved to "disabled" folder, etc.

* Wed 24 May 2017
  * create vm codes table

* Tue 23 May 2017
  * talkback interface moved to "extensions" folder
  * cleanup of callback support code
  * fixed ticket "OLVM doesn't close file descriptors after error" (talkback related)

* Mon 22 May 2017
  * added unit-tests (stubs) to virtual machine

* Thr 27 Apr 2017
  * in talkback sample added sending to OL and receiving from the byte buffer
  * thinking about moving the talkback into separate library

* Wed 26 Apr 2017
  * added hook for imports to the OL, after testing will merge in mainline - now import behaviour can be changed at user level

* Mon 24 Apr 2017
  * added error handling to the talkback - now talkback returns error code with error description

* Fri 21 Apr 2017
  * more samples (added coroutine sample)

* Mon 17 Apr 2017
  * extent talkback sample
  * talkback now can do "import" and ",load"
  * warnings cleanup

* Fri 14 Apr 2017
  * fixed warning for new glibc
  * another memory manager fix related to the large objects allocation
  * added hook for exit() that gives ability to catch the OL exiting appilcation - use by OL_atexit()
  * seems to fixed bug with 100% resources usage in idle mode

* Thr 13 Apr 2017
  * first implementation of experimental talkback interface - linkage C and OL modules, that helps to easily run OL scripts from C
  * fixed bug with possible heap corruption when requested a very large object via vm:raw

* Wed 12 Apr 2017
  * small fixes for embed functionality

* Wed 12 Apr 2017
  * huh, got first thirdparty tickets!
  * fixed embed docs and functionality
  * fixed master branch error for ol recompiling

* Tue 28 Mar 2017
  * more win32 sysinfo

* Tue 21 Mar 2017
  * more openal (pcm, a-law decoders)

* Mon 20 Mar 2017
  * changes set-ref! primop
  * fixed vm:raw primop, added to vm:raw more parameter - now vm:raw can allocate required space without initializing
  * added new vm command endianness that returns 1 for little- and 2 for big- system endianness
  * openal functionality moved from samples to the library

* Fri 17 Mar 2017
  * added basic OpenAL example.

* Thr 16 Mar 2017
  * map speedup and r5rs compliant fold

* Wed 15 Mar 2017
  * created new repository yuriy-chumak/meala with implementation in ol of visual novel (external producer), looks like it's good instrument for creating such kind of games. additionally added sample opengl visualization. one more thing to ol gems )

* Mon 13 Mar 2017
  * added ports to pinvoke
  * added wasm support to build scripts (not yet enough tested)
  * added new (lib rlutil) library, now we can use terminal portable (win32/linux)
  * in (lib opengl) gl:SwapBuffers made universal

* Mon 27 Feb 2017
  * fixed stat syscall

* Fri 24 Feb 2017
  * changed initializator name in (owl io) module, planned to add "auto" initializators for modules (autoexecutabled functions).

* Thr 23 Feb 2017
  * fixed environment initialization in "slim" version, got less binary size
  * added environment indicators for srfi - for now as test feature

* Wed 22 Feb 2017
  * trying to implement fork for win32, no so happy

* Mon 20 Feb 2017
  * khe-khe, we got own brainf**k interpreter!, simultaneously updated [rosetta code](http://rosettacode.org/wiki/Category:Ol) page.
  * fixed set-ref and set-ref!, now they correcly works with signed numbers
  * bf works - tested with bf self interpreter

* Fri 17 Feb 2017
  * added compatible with r5rs (r5rs characters) module
  * updated license text in ol.scm

* Mon 13 Feb 2017
  * from this point will post news without time
  * JF2 renamed to JAF (Jump if Arity Failed)

* Tue 07 Feb 2017 18:19 CET
  * released project works and works fine.
  * ol ported under web, so it can run under web-browsers and run fast.

* *Fri 30 Dec 2016 17:06 EET*
  * *release 1.1 ready to download.*

* Wed 28 Dec 2016 18:15 EET
  * preparing the release 1.1
  * made great work with decreasing memory usage by three-five times and twise boost of execution speed (thanks to new gc memory management strategy).

* Thr 01 Dec 2016 18:06 EET
  * [build.opensuse.org](https://build.opensuse.org/package/show/home:yuriy-chumak/ol) works again, you can get prepared packages for the x86 CentOS 6, x86 Debian 7, x86 Debian 8, x86 Fedora 22, x86 Fedora 23, x86 RHEL 5, x86 RHEL 6, x86 ScientificLinux 6, x86 openSUSE 13.1, x86 openSUSE 13.2, armv7l openSUSE Factory, aarch64 openSUSE Factory, x86 Ubuntu 12.04, x86 Ubuntu 14.04, x86 Ubuntu 16.04

* Wed 30 Nov 2016 21:53 EET
  * ol recompiled for odroid (please remember that ol works in "wild" under the odroid c1+ platform and provides web access to the server with remote terminal support)

* Wed 30 Nov 2016 17:19 EET
  * added full android support to the ol, now full list of supported platforms is armeabi, armeabi-v7a, arm64-v8a, mips, mips64, x86, x86-64
  * successfully tested sockets under android armeabi, it works fine:

* Mon 28 Nov 2016 17:32 EET
  * changed assembler code for the x86 and x86-64 pinvoke mechanism, decreased side and increased speed. for testing use neton-dynamics and opengl libraries
      ![screenshot 1](assets/newton3.png)
  * now "native" function can return float and double values, ol understands it and correctly receives

* Thr 10 Nov 2016 19:23 EET
  * nothing happend, i'm continuing the project, simply no time to the commenting all steps

* Previous news records can be found in [russian translated](?ru) part of this site


### Learn

   You can immediately try Otus Lisp (ol) in the provided terminal on the left of this page. For example, type
<pre><code id="sample1" data-language="scheme">(+ 1 2 3 4 5 6 7)</code><button class="doit" onclick="doit(sample1.textContent)">send to the terminal</button></pre>
   After pressing "Enter" button you will receive *28*.

   Or you can try more interesting example
<pre><code id="sample2" data-language="scheme">(fold * 1 (iota 99 1 1))</code><button class="doit" onclick="doit(sample2.textContent)">send to the terminal</button></pre>
which is another way of
<pre><code id="sample3" data-language="scheme">(let factorial ((n 99))
   (if (= n 0)
      1
      (* n (factorial (- n 1)))))</code><button class="doit" onclick="doit(sample3.textContent)">send to the terminal</button></pre>

   This will produce factorial of 99 for you
<pre><code>> (let factorial ((n 99))
   (if (= n 0)
      1
      (* n (factorial (- n 1)))))
933262154439441526816992388562667004
907159682643816214685929638952175999
932299156089414639761565182862536979
208272237582511852109168640000000000
000000000000

>
</code></pre>


### p.s.

* <a name="pure"></a>In fact, Otus Lisp is slightly extended towards traditional imperative programming, namely, limited mutators have been introduced. It helps to organize non-repetitive random number generator and to speed-up some imperative-oriented operations.
* <a name="42kb"></a>Real virtual machine size depends on parget platform and enabled features. 42kb is typical size for linux without including debug information, ffi and experimental inexact library, but including couple of internal safe checks, sockets, native library calls and sandbox. For other operation systems size can be as bigger as lesser (i.e. 79kb for win64 x86_64, 30kb for ubuntu64 armhf).
