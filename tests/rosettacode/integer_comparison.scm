; http://www.rosettacode.org/wiki/Integer_comparison

(define (compare a b)
  (cond ((< a b) "A is less than B")
        ((> a b) "A is greater than B")
        ((= a b) "A equals B")))

(print (compare 1 2))
(print (compare 2 2))
(print (compare 3 2))
