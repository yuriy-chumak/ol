(import (otus case-apply))

(define (f0)
   0)
(define (f1 x)
   x)
(define (f2 x y)
   (cons x y))

(define (test f)
   (case-apply f
      '(1 . (1))
      '(2 . (7 2))))

(print (test f0))
(print (test f1))
(print (test f2))
