#!/usr/bin/env ol

(define-macro BZ (lambda (n)
   `(list
      ,@(map (lambda (i)
            `(if (eq? q 0) q 55))
         (iota n)))
))

(define q 42)
(define f (lambda args
   (BZ 300)
))
(print (f))