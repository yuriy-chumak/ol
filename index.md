---
layout: html
title:  Otus Lisp
date: 2016-03-15 16:56:57 UTC
categories: index
---
### Summary

   Otus Lisp (ol) is a purely (*mostly*) functional dialect of Lisp.
It implements an extended subset of [R<sup>5</sup>RS](http://www.schemers.org/Documents/Standards/R5RS/) Scheme.

   You can immediately try ol in the provided terminal on this page. For example, type:
<pre><code id="sample1" data-language="scheme">(+ 3/7 2+4i)</code><button class="doit" onclick="doit(sample1.textContent)">send to the terminal</button></pre>

   Or you can try more interesting example
<pre><code id="sample2" data-language="scheme">(fold * 1 (iota 99 1 1))</code><button class="doit" onclick="doit(sample2.textContent)">send to the terminal</button></pre>
which is another way of
<pre><code id="sample3" data-language="scheme">(let factorial ((n 99))
   (if (= n 0)
      1
      (* n (factorial (- n 1)))))</code><button class="doit" onclick="doit(sample3.textContent)">send to the terminal</button></pre>

   This must calculate factorial of 99 (933262154439441526816992388562667004907159682643816214685929638952175999932299156089414639761565182862536979208272237582511852109168640000000000000000000000) for you.

### Learn

   Current news can be found in [russian translated](?ru) part of this site that frequently updates.

   I'm writing docs right now. It will be available very soon. You can check <b><a href="?en/examples">SAMPLES</a></b> for now.

<small style="float: right">Copyright (c) 2015 Yuriy Chumak</small>