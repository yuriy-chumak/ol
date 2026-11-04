#!/usr/bin/env ol

(define-macro BF (lambda (n)
   `(list
      ,@(map (lambda (i)
            `(if (eq? q #f) q 22))
         (iota n)))
))

(define q 42)
(define f (lambda args
   (BF 300)
))
(print (f))
