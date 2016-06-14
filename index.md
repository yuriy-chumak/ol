---
layout: html
title:  Otus Lisp
date: 2016-06-14 15:51:52 UTC
categories: index
---

   Otus Lisp is a purely (*mostly*) functional dialect of Lisp.
It implements an extended subset of [R<sup>5</sup>RS](http://www.schemers.org/Documents/Standards/R5RS/) Scheme.
It is small, embeddable and crossplatform.

   You can use it on Linux, Windows, BSD, Android. It runs on 32-bit and 64-bit platforms, on x86 and arm architectures.

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
   
  * CentOS, Fedora, OpenSUSE, RHEL: https://build.opensuse.org/package/show/home:yuriy-chumak/ol
  * Debian, Ubuntu: https://build.opensuse.org/package/show/home:yuriy-chumak/ol
  
   I'm working for add more prebuilts.

### Learn

   Current news can be found in [russian translated](?ru) part of this site that frequently updates.

   I'm writing docs right now. It will be available very soon. You can check <b><a href="?en/examples">SAMPLES</a></b> for now.

<small style="float: right">Copyright (c) 2015 Yuriy Chumak</small>