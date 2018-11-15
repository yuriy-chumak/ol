; http://www.rosettacode.org/wiki/Loops/Break
;(import (otus random!))
; temporary (rand!) function for deterministic testing purposes
(define rand!
   (let* ((ss ms (values 1234 5678))
          (seed (band (+ ss ms) #xffffffff))
          (seed (cons (band seed #xffffff) (>> seed 24))))
      (lambda (limit)
         (let*((next (+ (car seed) (<< (cdr seed) 24)))
               (next (+ (* next 1103515245) 12345)))
            (set-car! seed (band     next     #xffffff))
            (set-cdr! seed (band (>> next 24) #xffffff))

            (mod (mod (floor (/ next 65536)) 32768) limit)))))


(call/cc (lambda (break)
   (let loop ()
      (if (= (rand! 20) 10)
         (break #t))
      (print (rand! 20))
      (loop))))
