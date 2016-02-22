---
layout: page
title:  Summary
date:   пт, 27-лис-2015 19:40:51 +0200
categories: en
---

   Otus Lisp (ol) is a purely (*mostly*) functional dialect of Lisp.
It implements an extended subset of [R<sup>5</sup>RS](http://www.schemers.org/Documents/Standards/R5RS/) Scheme.

   You can immediately try ol in the provided terminal on this page. For example, type:
<pre><code id="sample1" data-language="scheme">(+ 3/7 2+4i)</code><button class="doit" onclick="doit(sample1.textContent)">send to the terminal</button></pre>
      
   Or you can try more interesting example
<pre><code id="sample2" data-language="scheme">(fold * 1 (iota 1 1 100))</code><button class="doit" onclick="doit(sample2.textContent)">send to the terminal</button></pre>
which is another way of
<pre><code id="sample3" data-language="scheme">(let factorial ((n 99))
   (if (= n 1)
      1
      (* n (factorial (- n 1)))))</code><button class="doit" onclick="doit(sample3.textContent)">send to the terminal</button></pre>
      
   This must calculate factorial of 99 (99!) for you.

### Learn

   Current news can be found in [russian translated](?ru) part of this site.

   I'm writing docs right now. It will be available very soon. You can check <b><a href="?en/examples">SAMPLES</a></b> for now.
