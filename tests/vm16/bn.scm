#!/usr/bin/env ol

(define-macro BN (lambda (n)
   `(list
      ,@(map (lambda (i)
            `(if (eq? q #n) q 33))
         (iota n)))
))

(define q 42)
(define f (lambda args
   (BN 300)
))
(print (f))
