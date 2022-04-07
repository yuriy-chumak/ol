(import (otus case-apply))

(define (f0)
   (cons "0" 0))
(define (f1 x)
   (cons "2" (* x 2)))
(define (f2 x y)
   (cons "3" (+ x y)))

(define (test f)
   (case-apply f
      '(1 . (1))
      '(2 . (7 2))))

(print "test arity 0 function: " (test f0))
(print "test arity 1 function: " (test f1))
(print "test arity 2 function: " (test f2))
