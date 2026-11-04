#!/usr/bin/env ol

(define-macro BT (lambda (n)
   `(list
      ,@(map (lambda (i)
            `(if (eq? q #t) q 44))
         (iota n)))
))

(define q 42)
(define f (lambda args
   (BT 300)
))
(print (f))