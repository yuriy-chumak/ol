; http://www.rosettacode.org/wiki/Arithmetic/Complex

(define A 0+1i) ; manually entered numbers
(define B 1+0i)

(print (+ A B))
(print (- A B))
(print (* A B))
(print (/ A B))

(define C (complex 2/7 -3)) ; functional way

(print "real part of " C " is " (car C))
(print "imaginary part of " C " is " (cdr C))
