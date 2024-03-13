;;;; random number generator
(define-library (otus random!)
(export
   ; (rand! n)
   ; generate random number in range [0 .. n) - from 0 to n-1
   rand!
)

(import (otus lisp))

(begin

(define rand!
   (let* ((ss ms (clock))
          (seed (band (+ ss ms) #xffffffff))
          (seed (cons (band seed #xffffff) (>> seed 24))))
      (lambda (limit)
         (let*((next (+ (car seed) (<< (cdr seed) 24)))
               (next (+ (* next 1103515245) 12345)))
            (set-car! seed (band     next     #xffffff))
            (set-cdr! seed (band (>> next 24) #xffffff))

            (mod (mod (floor (/ next 65536)) 32768) limit)))))

))
