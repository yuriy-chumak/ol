---
layout: page
title:  Samples
date:   пн, 21-гру-2015 15:36:58 +0200
categories: en
---
#### Simplest Web Server

   This web server listens 8080 port, and for any correct request (GET, POST, etc.) returns "200 OK" and web page with echoed headers from this request.

<pre><button class="doit" onclick="function(){return false}">This sample will not work in terminal</button>
<code data-language="scheme">#!/bin/ol

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

<pre><button class="doit" onclick="doit(numbers.textContent)">send to the terminal</button>
<code data-language="scheme" id="numbers">; https://en.wikipedia.org/wiki/SHA-1

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
               (let loop ((t (iota 0 1 16)))
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

Hello, Lisp! ==> JOFseripot79ycrTANu0xN8FP0A=
abc ==> qZk+NkcGgWq6PiVxeFDCbJzQ2J0=
Night
THE sun descending in the west  
The evening star does shine; 
The birds are silent in their nest.
 
And I must seek for mine. ==> 3A5t2MMPNp8lCUX0v7QNt5jZC9k=
</code></pre>

   Some works more.
