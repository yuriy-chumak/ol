; http://www.rosettacode.org/wiki/Sum_digits_of_an_integer

(define (sum n base)
   (if (zero? n)
      n
      (+ (mod n base) (sum (div n base) base))))

(print (sum 1 10))
(print (sum 1234 10))
(print (sum #xfe 16))
(print (sum #xf0e 16))
