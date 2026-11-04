#!/usr/bin/env ol

(define-macro BE (lambda (n)
   `(list
      ,@(map (lambda (i)
            `(if (eq? q #e) q 11))
         (iota n)))
))

(define q 42)
(define f (lambda args
   (BE 300)
))
(print (f))