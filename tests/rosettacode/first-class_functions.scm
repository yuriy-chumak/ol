; http://www.rosettacode.org/wiki/First-class_functions

; creation of new function from preexisting functions at run-time
(define (compose f g) (lambda (x) (f (g x))))

; storing functions in collection
(define (quad x) (* x x x x))
(define (quad-root x) (sqrt (sqrt x)))

(define collection [quad quad-root])

; use functions as arguments to other functions
; and use functions as return values of other functions
(define identity (compose (ref collection 2) (ref collection 1)))
(print (identity 11211776))
