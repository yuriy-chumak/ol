---
layout: page
title:  Summary
categories: en
---
> Any sufficiently complicated program contains an ad-hoc, informally-specified, bug-ridden, slow implementation of half of some Lisp dialect.
> <br/> <span style="float: right;">Greenspun's tenth rule</span>


**Otus Lisp** (Ol in short) is a purely[\*](#pure) functional dialect of Lisp.

It implements an extended subset of [R<sup>7</sup>RS](https://www.r7rs.org) Scheme including, but not limited to, some of the [SRFI](http://srfi.schemers.org/)s. It's small, embeddable and crossplatform; can run in own sandbox (for systems with sandboxing support); provides a portable, high level way for calling the code written in another languages. You can use Otus Lisp on GNU/Linux, Windows, different Unixes, kinds of BSDs, Android, webOS, Minoca and many other operation systems with various (i.e. x86/x86_64, arm, aarch64, mips, ppc) architectures and platforms (ODroid, LattePanda, etc.).


### Downloads
Release **2.0** available to [download](https://github.com/yuriy-chumak/ol/releases). Happy LISPing!

[Sep 2019]: Preparing release **2.1** which provides nice syntactic sugar for vectors (using []) and dictionaries (using {}), updated embed interface, better Web implementation (through asm.js) and other.


### Already tested platforms
- [x] x86: 80486, pentium, pentium 2, pentium 3, athlon, core 2 quad, core i3, core i5, core i7.
- [x] x86_64: core 2 quad, core i3, core i5, core i7.
- [x] aarch64: cortex-a53, cortex-a57.
- [x] arm: armv5tejl, armv7l, arm920t, snapdragon 801.
  - nearly planned, but not yet tested: arm926t, arm1136, cortex-a7, cortex-a9, cortex-a15, cortex-m3, cortex-m4.
- [x] powerpc: Mac G4
- [x] mips32: algor (P-5064)
- [x] mips64
- fully supported, but not yet tested: m68k, microblaze, or1k, ppc, ppc64, sh4, spark, spark64, ztensa.

### Tested operation systems/devices
- [x] GNU/Linux: CentOS, Debian, Fedora, RHEL, SLE, ScientificLinux, Uninvention, openSUSE, Ubuntu.
- [x] Windows: Windows 95, Windows 98, Windows ME, Windows NT, Windows 2000, Windows XP, Windows Vista, Windows 7, Windows 8, Windows 10.
- [x] Unix: OpenBSD, FreeBSD, NetBSD.
- [x] macOS: High Sierra
- [x] Android: all versions up to Nougat.
- [x] webOS: 2.0.
- [x] Odroid: [C1+](https://www.hardkernel.com/main/products/prdt_info.php?g_code=G143703355573).
- [x] Minoca [OS](https://www.minocacorp.com/product/).
- [x] [LattePanda](https://www.lattepanda.com/)

### Tested web browsers (asm.js based Ol version)
- [x] Iceweasel 38.6.0 / Debian 6.0
- [x] Epiphany 3.4.2 / Debian 6.0
- [x] Chrome (37, 48, etc.) / Debian 6.0
- [x] Luakit 1.8.0 / Ubuntu 9.10
- [x] Opera 58.0.3135.65 / Windows 2008 R2
- [x] SeaMonkey 2.33 / Debian 6.0
- [x] Firefox (28, 36, 44, etc. ) / Debian 6.0
- [x] Chrome (39, 44, 51, 71, etc.) / Windows 2008 R2
- [x] Iceape 2.7.12 / Debian 6.0


### About project
Otus Lisp is available under 2 licenses:
[MIT License](https://github.com/yuriy-chumak/ol/blob/master/LICENSE) and
[GNU ](https://github.com/yuriy-chumak/ol/blob/master/COPYING)([L](https://github.com/yuriy-chumak/ol/blob/master/COPYING.LESSER))[GPLv3 License](https://github.com/yuriy-chumak/ol/blob/master/COPYING).

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
* 2019
  * version of builtin terminal ol updated to 2.1-2222
  * a lot of changes, full list can be obtained [on github](https://github.com/yuriy-chumak/ol/commits/master).

* [News archive](?en/news)


### Learn

   You can immediately try Otus Lisp (ol) in the provided terminal on the left of this page. For example, type
<pre><code id="sample1" data-language="ol">(+ 1 2 3 4 5 6 7 8)</code><button class="doit" onclick="doit(sample1.textContent)">send to the terminal</button></pre>
   After pressing "Enter" button you will receive *36*.

   Or you can try more interesting example
<pre><code id="sample2" data-language="ol">(fold * 1 (iota 99 1 1))</code><button class="doit" onclick="doit(sample2.textContent)">send to the terminal</button></pre>
which is another way of
<pre><code id="sample3" data-language="ol">(let factorial ((n 99))
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

   And a bit more complex example: how about Pi? I mean the calculation of the first 200 digits of Pi?
   Please be patient - on slower PCs this may take some time.
<pre><code id="samplePi" data-language="ol">(define (pi total-digits)
   (let loop ((q 1) (r 0) (t 1) (k 1) (n 3) (l 3) (digits total-digits))
      (unless (eq? digits 0)
         (if (< (- (+ (* 4 q) r) t) (* n t))
            (begin
               (display n) (if (eq? digits total-digits) (display "."))
               (loop (* q  10)
                     (* 10 (- r (* n t)))
                     t
                     k
                     (- (div (* 10 (+ (* 3 q) r)) t) (* 10 n))
                     l
                     (- digits 1)))
            (begin
               (loop (* q k)
                     (* (+ (* 2 q) r) l)
                     (* t l)
                     (+ k 1)
                     (div (+ (* q (* 7 k)) 2 (* r l)) (* t l))
                     (+ l 2)
                     digits)))))
    (print))

(pi 200)</code><button class="doit" onclick="doit(samplePi.textContent)">send to the terminal</button></pre>


### p.s.

* <a name="pure"></a>In fact, Otus Lisp is slightly extended towards traditional imperative programming, namely, limited mutators have been introduced. It helps to organize non-repetitive random number generator and to speed-up some imperative-oriented operations.
* <a name="42kb"></a>Real virtual machine size depends on parget platform and enabled features. 42kb is typical size for Linux without including debug information, ffi and experimental inexact library, but including couple of internal safe checks, sockets, native library calls and sandbox. For other operation systems size can be as bigger as lesser (i.e. 79kb for win64 x86_64, 30kb for ubuntu64 armhf).
* <a name="sandbox"></a>Sandboxing works only under OS with [SECCOMP](https://en.wikipedia.org/wiki/Seccomp) (secure computing mode) - it's GNU/Linux (and different Linux disctributives, Ubuntu, Debian, Suse, etc.). Supporting sandboxing under *BSD is in progress.
