;;;; random number generator
(define-library (otus random!)
(export
   ; (rand! n)
   ; generate random number in range [0 .. n) - from 0 to n-1
   rand!

   ; (rand-range! a b)
   ; generate random number in range [a .. b) - from a to b-1
   rand-range!

   ; (shuffle! t)
   ; shuffle a tuple, list (destructive)
   shuffle!

   
)

(import (otus lisp))

(begin

; (rand limit)
;(define rand!
;   (let* ((ss ms (clock))
;          (seed (cons ms ss)))
;      (lambda (limit)
;         (let* ((x (car seed))
;                (a _ (vm:mul x 214013))
;                (b _ (vm:add a 2531011))
;                (c _ (vm:shr b 16))
;                (o p d (vm:div 0 c limit)))
;            (set-car! seed c)
;            d))))

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

(define (rand-range! lo hi)
   (if (< lo hi)
      (lets ((o (rand! (- hi lo))))
         (+ o lo))
      (runtime-error "rnd-range: bad range: " (list lo hi))))

(define (shuffle! o) ; перемешивалка для tuple
   (cond
      ((tuple? o)
         (for-each (lambda (i)
               (let ((a (ref o i))
                     (j (+ 1 (rand! i))))
                  (set-ref! o i (ref o j))
                  (set-ref! o j a)))
            (reverse (iota (size o) 1)))
         o)
      ((list? o)
         (tuple->list (shuffle! (list->tuple o))))
      ((string? o)
         (runes->string (shuffle! (string->runes o))))))


; based on Marsaglia's letter: http://www.cse.yorku.ca/~oz/marsaglia-rng.html

; The MWC generator concatenates two 16-bit multiply-
;   with-carry generators, x(n)=36969x(n-1) + carry,
;   y(n)=18000y(n-1)+carry mod 2^16, has period about
;   2^60 and seems to pass all tests of randomness. A
;   favorite stand-alone generator.
; #define znew (z=36969*(z&65535)+(z>>16))
; #define wnew (w=18000*(w&65535)+(w>>16))
; #define MWC ((znew<<16)+wnew)

;      (define rand!
;         (let* ((ss ms (clock))
;                (seed (cons ms ss)))
;            (lambda (limit)
;               (let* ((x (car seed))
;                      (a _ (vm:mul x 214013))
;                      (b _ (vm:add a 2531011))
;                      (c _ (vm:shr b 16))
;                      (o p d (vm:div 0 c limit)))
;                  (set-car! seed c)
;                  d))))


))
