#!/usr/bin/env ol

; http://www.codeproject.com/Articles/69941/Best-Square-Root-Method-Algorithm-Function-Precisi
(define (sqrt m)
   (let*((i (let loop ((i 0))
               (if (<= (* i i) m)
                  (loop (+ i 1))
                  (- i 1))))
         (d (- m (* i i)))
         (p (/ d (* i 2)))
         (a (+ p i)))
      (- a (/ (* p p) (* a 2)))))

(print (sqrt 11))

