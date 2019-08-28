---
layout: page
title:  Samples
date: 2016-03-15 16:56:57 UTC
categories: en
---
#### Simplest Web Server

   This web server listens 8080 port, and for any correct request (GET, POST, etc.) returns "200 OK" and web page with echoed headers from this request.

<pre><button class="doit" onclick="function(){return false}">This sample will not work in terminal</button>
<code data-language="ol">#!/bin/ol

(import (lib http))

(http:run 8080 (lambda (fd request headers send close)
   (send "HTTP/1.0 200 OK\n"
         "Connection: close\n"
         "Content-Type: text/html; charset=UTF-8\n"
         "Server: " (car *version*) "/" (cdr *version*) "\n\n"

         "<h1>200: OK</h1>"
         (ref request 1) ": " (ref request 2)
         "<hr><small>" headers
         "</small>")
   (close #t)
))
</code></pre>

#### SHA1 (Secure Hash Algorithm 1)

   Simple hash function that used, for example, by WebSocket auth algorithm. Examples of usage at the end of source.

<pre><button class="doit" onclick="doit(sha1.textContent)">send to the terminal</button>
<code data-language="ol" id="sha1">; https://en.wikipedia.org/wiki/SHA-1

; band - binary AND operation
; bor - binary OR operation
; bxor - binary XOR operation
; >>, << - binary shift operations
; runes->string - convert byte list to string /(runes->string '(65 66 67 65)) => "ABCA"/


(define (sha1-padding-size n)
   (let ((x (mod (- 56 (rem n 64)) 64)))
      (if (= x 0) 64 x)))

(define (sha1-pad-message message)
   (let*((message-len (string-length message))
         (message-len-in-bits (* message-len 8))
         (buffer-len (+ message-len 8 (sha1-padding-size message-len)))
         (message (string-append message (runes->string '(#b10000000))))
         (zeroes-len (- buffer-len message-len 1 4)) ; for ending length encoded value
         (message (string-append message (make-string zeroes-len 0)))
         (message (string-append message (runes->string (list
            (band (>> message-len-in-bits 24) #xFF)
            (band (>> message-len-in-bits 16) #xFF)
            (band (>> message-len-in-bits  8) #xFF)
            (band (>> message-len-in-bits  0) #xFF))))))
;      (print "message-len: " message-len)
;      (print "message-len-in-bits: " message-len-in-bits)
;      (print "buffer-len: " buffer-len)
;      (print "zeroes-len: " zeroes-len)
;      (print "message: " message)
;      (print "length(message): " (string-length message))
      message))

(define XOR (lambda args (fold bxor 0 args))) ; bxor more than 2 arguments
(define OR (lambda args (fold bor 0 args))) ; bor more than 2 arguments
(define NOT (lambda (arg) (bxor arg #xFFFFFFFF))) ; binary not operation

; to 32-bit number
(define (->32 i)
   (band i #xFFFFFFFF))

; binary cycle rotate left
(define (rol bits x)
   (->32
      (bor
         (<< x bits)
         (>> x (- 32 bits)))))

(define (word->list x)
   (list
      (band (>> x 24) #xFF)
      (band (>> x 16) #xFF)
      (band (>> x  8) #xFF)
      (band (>> x  0) #xFF)))

(define (message->words message)
   (let cycle ((W
               (let loop ((t (lrange 0 1 16)))
                  (if (null? t)
                     null
                  (let*((p (* (car t) 4)))
                     (cons (OR
                              (<< (string-ref message (+ p 0)) 24)
                              (<< (string-ref message (+ p 1)) 16)
                              (<< (string-ref message (+ p 2))  8)
                              (<< (string-ref message (+ p 3))  0))
                           (loop (cdr t)))))))
               (t 16))
      (if (eq? t 80)
         W
         (cycle (append W (list
            (XOR
               (rol 1 (list-ref W (- t 3)))
               (rol 1 (list-ref W (- t 8)))
               (rol 1 (list-ref W (- t 14)))
               (rol 1 (list-ref W (- t 16))))))
            (+ t 1)))))

(define (sha1:digest message)
   (let*((h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476)
         (h4 #xC3D2E1F0)
         (K '(#x5A827999 #x6ED9EBA1 #x8F1BBCDC #xCA62C1D6))
         (padded-message (sha1-pad-message message))
         (n (/ (string-length padded-message) 64)))

      (let main ((i 0)
                 (A h0) (B h1) (C h2) (D h3) (E h4))
         (if (= i n)
            (fold append null
               (list (word->list A) (word->list B) (word->list C) (word->list D) (word->list E)))
            (let*((message (substring padded-message (* i 64) (+ (* i 64) 64)))
                  (W (message->words message)))
               (let*((a b c d e ; round 1
                        (let loop ((a A) (b B) (c C) (d D) (e E) (t 0))
                           (if (< t 20)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (OR (band b c) (band (NOT b) d))
                                             e
                                             (list-ref W t)
                                             (list-ref K 0)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 2
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 20))
                           (if (< t 40)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (XOR b c d)
                                             e
                                             (list-ref W t)
                                             (list-ref K 1)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 3
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 40))
                           (if (< t 60)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (OR (band b c) (band b d) (band c d))
                                             e
                                             (list-ref W t)
                                             (list-ref K 2)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 4
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 60))
                           (if (< t 80)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (XOR b c d)
                                             e
                                             (list-ref W t)
                                             (list-ref K 3)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e)))))

                  (main (+ i 1)
                     (->32 (+ A a))
                     (->32 (+ B b))
                     (->32 (+ C c))
                     (->32 (+ D d))
                     (->32 (+ E e)))))))))

(define (->string value)
   (runes->string
   (let ((L "0123456789abcdef"))
   (let loop ((v value))
      (if (null? v) null
      (cons
         (string-ref L (>> (car v) 4))
         (cons
         (string-ref L (band (car v) #xF))
            (loop (cdr v)))))))))

; examples:
(define (example message)
   (print message " ==> " (->string (sha1:digest message))))

(example "Hello, Lisp!")
(example "abc")
(example "Night
THE sun descending in the west
The evening star does shine;
The birds are silent in their nest.

And I must seek for mine.")
</code></pre>

   This will output:

<pre><code>
Hello, Lisp! ==> 24e16c7ab8a9a2defdc9cad300dbb4c4df053f40
abc ==> a9993e364706816aba3e25717850c26c9cd0d89d
Night
THE sun descending in the west
The evening star does shine;
The birds are silent in their nest.

And I must seek for mine. ==> 0860bc39e3ae96947eeace44f3788f31ec43af1e
</code></pre>

#### Simple Neural Network

   This code demonstrates learning process and working results for neural network that can detect numbers from bitmaps.

<pre><button class="doit" onclick="doit(neural.textContent)">send to the terminal</button>
<code data-language="ol" id="neural">
; this is main learning configuration parameter,
;  higher is better, but slower
(define THRESHOLD 30)

; ANSWER sad #t/#f, PANSWER returns numerical probability
(define (ANSWER axon-weights signal)
   (> (fold + 0 (zip * signal axon-weights)) THRESHOLD))
(define (PANSWER axon-weights signal)
   (/ (fold + 0 (zip * signal axon-weights)) THRESHOLD))


; Let's encode some sample numbers:
; 0:     1:     2:     3:     4:     5:     6:     7:     8:     9:
; x x x  . . x  x x x  x x x  x . x  x x x  x x x  x x x  x x x  x x x
; x . x  . . x  . . x  . . x  x . x  x . .  x . .  . . x  x . x  x . x
; x . x  . . x  x x x  . x x  x x x  x x x  x x x  . . x  x x x  x x x
; x . x  . . x  x . .  . . x  . . x  . . x  x . x  . . x  x . x  . . x
; x x x  . . x  x x x  x x x  . . x  x x x  x x x  . . x  x x x  x x x
(define pattern-0 '(1 1 1  1 0 1  1 0 1  1 0 1  1 1 1))
(define pattern-1 '(0 0 1  0 0 1  0 0 1  0 0 1  0 0 1))
(define pattern-2 '(1 1 1  0 0 1  1 1 1  1 0 0  1 1 1))
(define pattern-3 '(1 1 1  0 0 1  0 1 1  0 0 1  1 1 1))
(define pattern-4 '(1 0 1  1 0 1  1 1 1  0 0 1  0 0 1))
(define pattern-5 '(1 1 1  1 0 0  1 1 1  0 0 1  1 1 1))
(define pattern-6 '(1 1 1  1 0 0  1 1 1  1 0 1  1 1 1))
(define pattern-7 '(1 1 1  0 0 1  0 0 1  0 0 1  0 0 1))
(define pattern-8 '(1 1 1  1 0 1  1 1 1  1 0 1  1 1 1))
(define pattern-9 '(1 1 1  1 0 1  1 1 1  0 0 1  1 1 1))

; Let's teach two neurons simulateonusly, one for detecting
;  number "one" and second for detecting number "two"
; 1 means "ok" for "one" and for "two" neurons respectively

; this is our learning matrix:
(define patterns (list
   (cons pattern-0 '(0 0))
   (cons pattern-1 '(1 0))
   (cons pattern-2 '(0 1))
   (cons pattern-3 '(0 0))
   (cons pattern-4 '(0 0))
   (cons pattern-5 '(0 0))
   (cons pattern-6 '(0 0))
   (cons pattern-7 '(0 0))
   (cons pattern-8 '(0 0))
   (cons pattern-9 '(0 0))
   ; some random noise...
   '((0 0 0  0 0 0  0 0 0  0 1 0  0 0 0) . (0 0))
   '((1 1 1  0 0 0  0 0 0  0 0 0  0 0 0) . (0 0))
   '((0 0 0  1 1 1  0 0 0  0 0 0  0 0 0) . (0 0))
   '((1 1 1  0 0 0  1 1 1  0 0 0  1 1 1) . (0 0))
   '((0 0 0  0 0 0  1 1 1  0 0 0  0 0 0) . (0 0))
   '((0 0 0  0 0 0  0 0 0  1 1 1  0 0 0) . (0 0))
   '((0 0 0  0 0 0  1 1 1  1 1 1  0 0 0) . (0 0))
   '((0 0 0  0 0 0  0 0 0  0 0 0  1 1 1) . (0 0))
   '((1 1 1  1 1 1  1 1 1  1 1 1  1 1 1) . (0 0))
   '((0 0 0  1 1 1  1 1 1  0 0 0  0 0 0) . (0 0))
   '((1 0 1  1 0 1  1 0 1  1 1 1  0 0 0) . (0 0))
   '((1 1 1  0 0 0  1 1 1  1 1 1  0 0 0) . (0 0))
   '((0 0 0  0 0 0  0 0 0  0 0 0  1 1 1) . (0 0))
))

; this is main learning function:
(define (learn pattern answer matrix)
   (if (null? pattern) ; паттерны закончились, вернем подкорретированную матрицу
      matrix
      (let ((sensor (car pattern))
            (matrix-result (ANSWER (car pattern) matrix))
            (is-pattern-good (car answer)))
         (for-each display (list
            "testing pattern "
            (car pattern)
            " : "
            (car answer)
            "> "))

         ; sad "ok" while "not ok", let's repeat leaning
         (if (and matrix-result
                  (= is-pattern-good 0))
            (begin
               (print "- bad pattern good answer, matrix: " matrix).
               (learn pattern answer (zip - matrix sensor)))
         ; sad "no" while is "ok", let's repeat leaning
         (if (and (not matrix-result)
                  (= is-pattern-good 1))
            (begin
               (print "- good pattern bad answer, matrix: " matrix).
               (learn pattern answer (zip + matrix sensor)))
         ; all ок, can continue to the next pattern
         (begin
            (print "- ok: " matrix)
            (learn (cdr pattern) (cdr answer) matrix)))))))

; and small looper for it
(define (times counter matrix patterns answers)
  (let time ((counter counter) (matrix matrix))
   (if (= counter 0)
      matrix
      (time (- counter 1) (learn patterns answers matrix)))))

; well done. now we can learn our neurons:
(define zero-input-vector '(1 0 0  0 1 0  0 0 0  1 1 1  0 1 0))

; пройдемся по этому списку, скажем, 200 раз - 200 процессов обучения
(define one-v (times 200 zero-input-vector (map car patterns) (map (lambda (list) (car list)) (map cdr patterns))))
(define two-v (times 200 zero-input-vector (map car patterns) (map (lambda (list) (cadr list)) (map cdr patterns))))

(print "result matrix is: " one-v)
(print "result matrix is: " two-v)

; Let's do some tests...
(print "one-v on one: " (ANSWER one-v pattern-1))
(print "one-v on two: " (ANSWER one-v pattern-2))
(print "one-v on six: " (ANSWER one-v pattern-6))
(print)
(print "two-v on one: " (ANSWER two-v pattern-1))
(print "two-v on two: " (ANSWER two-v pattern-2))
(print "two-v on six: " (ANSWER two-v pattern-6))

; And, at least, this network can answer "maybe"!
;  let's check for pattern "one" when some of
;  points are changed

(print "at least: " (PANSWER one-v '(0 0 1  0 0 1  0 0 1  0 0 1  0 0 1)))
(print "at least: " (PANSWER one-v '(0 0 1  0 0 1  0 0 0  0 0 1  0 0 1)))
(print "at least: " (PANSWER one-v '(0 0 1  0 1 1  0 0 1  0 0 1  0 0 1)))
(print "at least: " (PANSWER one-v '(0 0 1  0 0 1  0 0 1  0 0 1  1 0 1)))

</code></pre>

   And results of this "smart" newrons:
<pre><code>
testing pattern (1 1 1 1 0 1 1 0 1 1 0 1 1 1 1) : 0> - ok: (1 0 0 0 1 0 0 0 0 1 1 1 0 1 0)
testing pattern (0 0 1 0 0 1 0 0 1 0 0 1 0 0 1) : 1> - good pattern bad answer, matrix: (1 0 0 0 1 0 0 0 0 1 1 1 0 1 0)
testing pattern (0 0 1 0 0 1 0 0 1 0 0 1 0 0 1) : 1> - good pattern bad answer, matrix: (1 0 1 0 1 1 0 0 1 1 1 2 0 1 1)
testing pattern (0 0 1 0 0 1 0 0 1 0 0 1 0 0 1) : 1> - good pattern bad answer, matrix: (1 0 2 0 1 2 0 0 2 1 1 3 0 1 2)
...
testing pattern (1 0 1 1 0 1 1 0 1 1 1 1 0 0 0) : 0> - ok: (4 3 3 -6 0 5 3 3 3 4 0 -5 3 4 3)
testing pattern (1 1 1 0 0 0 1 1 1 1 1 1 0 0 0) : 0> - ok: (4 3 3 -6 0 5 3 3 3 4 0 -5 3 4 3)
testing pattern (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) : 0> - ok: (4 3 3 -6 0 5 3 3 3 4 0 -5 3 4 3)
result matrix is: (-1 -2 6 -1 0 6 -1 -2 6 0 0 7 -2 -1 6)
result matrix is: (4 3 3 -6 0 5 3 3 3 4 0 -5 3 4 3)

one-v on one: #true
one-v on two: #false
one-v on six: #false

two-v on one: #false
two-v on two: #true
two-v on six: #false

at least: 31/30
at least: 5/6
at least: 16/15
at least: 1
</code></pre>

   Some works more.
