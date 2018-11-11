#!/usr/bin/ol

(import (otus random!))
;; (define rand!
;;    (let* ((ss ms (values 1234 5678))
;;           (seed (band (+ ss ms) #xffffffff))
;;           (seed (cons (band seed #xffffff) (>> seed 24))))
;;       (lambda (limit)
;;          (let*((next (+ (car seed) (<< (cdr seed) 24)))
;;                (next (+ (* next 1103515245) 12345)))
;;             (set-car! seed (band     next     #xffffff))
;;             (set-cdr! seed (band (>> next 24) #xffffff))

;;             (mod (mod (floor (/ next 65536)) 32768) limit)))))
(define (randint a b) (+ a (rand! (- b a -1)))) ; a <= x <= b

(import (lib rlutil))

(define WIDTH 47) ; should be odd
(define HEIGHT 29) ; should be odd
(define FILLING 0.4)

(define (shuffle! o) ; перемешивалка tuple
   (for-each (lambda (i)
         (let ((a (ref o i))
               (j (+ 1 (rand! i))))
            (set-ref! o i (ref o j))
            (set-ref! o j a)))
      (reverse (iota (size o) 1)))
   o)

; пустой уровень (заполненный скальными породами)
(define level (map
   (lambda (?)
      (make-bytevector WIDTH #\#))
   (iota HEIGHT)))

(define neighbors (tuple '(-1 . 0) '(0 . -1) '(+1 . 0) '(0 . +1)))

(let loop ((x (floor (/ WIDTH 2))) (y (floor (/ HEIGHT 2))) (n (floor (* FILLING WIDTH HEIGHT))))
   (set-ref! (lref level y) x #\ )
   (if (> n 0)
      (let*((neighbor (ref neighbors (randint 1 4)))
            (nx (+ x (car neighbor)))
            (ny (+ y (cdr neighbor))))
         (if (and (<= 1 nx (- WIDTH 2)) (<= 1 ny (- HEIGHT 2)))
            (loop nx ny (if (eq? (ref (lref level ny) nx) #\#) (- n 1) n))
            (loop x y n)))))

; рисуем
(cls)
(for-each (lambda (v) (print (vm:cast v type-string))) level)
