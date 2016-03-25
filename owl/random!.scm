;;;; random number generator
(define-library (owl random!)

   (export
      rand!)

   (import
      (r5rs core)
      (owl math)
      (owl time))

   (begin
      ; (rand limit)
      (define rand!
         (let* ((ss ms (clock))
                (seed (cons ms ss)))
            (lambda (limit)
               (let* ((x (car seed))
                      (a _ (vm:mul x 214013))
                      (b _ (vm:add a 2531011))
                      (c _ (vm:shr b 16))
                      (o p d (vm:div 0 c limit)))
                  (set-car! seed c)
                  d))))

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
