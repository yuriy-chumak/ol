---
layout: html
title:  Summary
categories: index
---
> Project news can be found in [russian translated](?ru) part of this site that frequently updates.
> Please, enable JavaScript to full project site functionality.
<br/>

   Otus Lisp is a purely[*](#pure) functional dialect of Lisp.
It implements an extended subset of [R<sup>5</sup>RS](http://www.schemers.org/Documents/Standards/R5RS/) Scheme including, but not limited to, some of the [SRFI](http://srfi.schemers.org/). It is small, embeddable and crossplatform.

   You can use it on Linux (CentOS, Debian, Fedora, RHEL, SLE, ScientificLinux, Uninvention, openSUSE, Ubuntu, etc.), Windows, *BSD, Android, webOS, Odroid. It runs on x86, arm, aarch64, mips, ppc architectures with 32- and 64-bit platforms.


### News

Preparing the release 1.2

Release **1.1** available to [download](https://github.com/yuriy-chumak/ol/releases). Happy LISPing!

### Tested platforms

 *  :heavy_check_mark: aarch64:
   * cortex-a53 :heavy_check_mark:, cortex-a57 :heavy_check_mark:
 *  :grey_question: arm:
   * arm920t :heavy_check_mark:, arm926t :grey_question:, arm1136 :grey_question:, cortex-a7 :grey_question:, cortex-a9 :grey_question:, cortex-a15 :grey_question:, cortex-m3 :grey_question:, cortex-m4 :grey_question: 
 *  :grey_question: m68k
   * mcf5208evb: m68040 :grey_question:, m5208 :grey_question:
 *  :grey_question: microblaze
   * TBD.
 *  :grey_question: mips32
   * TBD.
 *  :grey_question: mips64
   * TBD.
 *  :grey_question: or1k
   * TBD.
 *  :grey_question: ppc
   * TBD.
 *  :grey_question: ppc64
   * TBD.
 *  :grey_question: sh4
   * TBD.
 *  :grey_question: spark
   * TBD.
 *  :grey_question: spark64
   * TBD.
 *  :grey_question: x86
   * TBD.
 *  :grey_question: x86_64
   * TBD.
 *  :grey_question: ztensa
   * TBD.

### Summary (continue)

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

### Download

   Available binary builds for:

  * CentOS, Debian, Fedora, RHEL, SLE, ScientificLinux, Univention, OpenSUSE, Ubuntu: from [openSUSE Build Service](https://software.opensuse.org/download.html?project=home%3Ayuriy-chumak&package=ol)


   I'm working for add more prebuilts.

### Project news
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

  * Fri 30 Dec 2016 17:06 EET
    * release 1.1 ready to download.

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
<pre><code>
C:\>adb shell
# cd /data/local/tmp
# ./ol
You see a prompt.
Type ',help' to help, ',quit' to end session
> (import (lib http))
> ;; Library (lib http) added
> ;; Imported (lib http)
> (http:run 8080 (lambda (fd request headers send close)
   (print ":: " (syscall 51 fd #f #f))
   (send "HTTP/1.0 200 OK\n"
         "Connection: close\n"
         "Content-Type: text/html; charset=UTF-8\n"
         "Server: " (car *version*) "/" (cdr *version*)
         "\n\n"
         "200: OK")
   (close #t)
))
Server binded to 8080


# Wed Nov 30 16:56:01 2016 : new request from (10.0.2.2 . 53275)
:: (10.0.2.2 . 53275)
socket closed, on-accept done.
# Wed Nov 30 16:56:02 2016 : request processed in 772ms.
</code></pre>
  * Mon 28 Nov 2016 17:32 EET
    * changed assembler code for the x86 and x86-64 pinvoke mechanism, decreased side and increased speed. for testing use neton-dynamics and opengl libraries
      ![screenshot 1](assets/newton3.png)
    * now "native" function can return float and double values, ol understands it and correctly receives
  * Thr 10 Nov 2016 19:23 EET
    * nothing happend, i'm continuing the project, simply no time to the commenting all steps
  * Previous news records can be found in [russian translated](?ru) part of this site

### Learn

   Current news can be found in [russian translated](?ru) part of this site that frequently updates.

   I'm writing docs right now. It will be available very soon. You can check <b><a href="?en/examples">SAMPLES</a></b> for now.

<small style="float: right">Copyright (c) 2016 Yuriy Chumak</small>
