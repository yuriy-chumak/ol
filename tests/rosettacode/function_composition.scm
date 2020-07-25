; http://www.rosettacode.org/wiki/Function_composition#Ol

(define (compose f g)
   (lambda (x) (f (g x))))

;; or:

(define ((compose f g) x) (f (g x)))
