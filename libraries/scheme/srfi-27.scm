(define-library (scheme srfi-27)
; http://srfi.schemers.org/srfi-27/srfi-27.html

;; Abstract
;
; An interface to sources of random bits, or "random sources" for brevity.


;; Rationale
;
; This SRFI defines an interface for sources of random bits computed by a pseudo random number
; generator. The interface provides range-limited integer and real numbers. It allows accessing
; the state of the underlying generator. Moreover, it is possible to obtain a large number of
; independent generators and to invoke a mild form of true randomization.

; -----
(export
   random-integer
   random-real
   default-random-source

   make-random-source
   random-source?
   random-source-state-ref
   random-source-state-set!
   random-source-randomize!
   random-source-pseudo-randomize!

   random-source-make-integers
   random-source-make-reals)

(import
   (scheme core)
   (owl math) (owl math fp)
   (owl list)
   (scheme bytevector))

(begin
   (unless (eq? (band (vm:features) #o20) #o20)
      (runtime-error "Required inexactness support (vm must be built with the OLVM_INEXACTS build variable)" #null))

   (setq norm (inexact 2.3283065495728e-10))
   (setq m1   (inexact 4294967087))
   (setq m2   (inexact 4294944443))
   (setq a12  (inexact    1403580))
   (setq a13n (inexact     810728))
   (setq a21  (inexact     527612))
   (setq a23n (inexact    1370589))

   (define-values (ss ms) (clock))
   (setq M1 4294967087);(exact m1)

   (setq s10 (inexact (mod ss M1)))
   (setq s11 (inexact 0))
   (setq s12 (inexact 0))
   (setq s20 (inexact (mod ms M1)))
   (setq s21 (inexact 0))
   (setq s22 (inexact 0))

   ; 
   (setq MRG32k3a (lambda (s10 s11 s12 s20 s21 s22)
      (let*(; component 1
            (p1 (- (* a12 s11) (* a13n s10)))
            (k (floor (/ p1 m1)))
            (p1 (- p1 (* m1 k)))
            (p1 (if (fless? p1 0.0) (+ p1 m1) p1))

            ; component 2
            (p2 (- (* a21 s22) (* a23n s20)))
            (k (floor (/ p2 m2)))
            (p2 (- p2 (* m2 k)))
            (p2 (if (fless? p2 0.0) (+ p2 m2) p2)))

         ; shift
         (bytevector-copy! s10 0 s11 0)
         (bytevector-copy! s11 0 s12 0)
         (bytevector-copy! s12 0 p1 0)

         (bytevector-copy! s20 0 s21 0)
         (bytevector-copy! s21 0 s22 0)
         (bytevector-copy! s22 0 p2 0)

         ; combination
         (if (fless? p2 p1)
            (* norm (- p1 p2))
         else
            (* norm (+ m1 (- p1 p2)))))))

   (define (random-real)
      (MRG32k3a s10 s11 s12 s20 s21 s22))

   (define (random-integer n)
      (exact (floor (* (random-real) n))))

   (define default-random-source (cons ss ms))

   ; -----------------------------------------
   (define (make-random-source)
      (let*((ss ms (clock)))
      (let ((s10 (inexact (mod ss M1)))
            (s11 (inexact 0))
            (s12 (inexact 0))
            (s20 (inexact (mod ms M1)))
            (s21 (inexact 0))
            (s22 (inexact 0)))
         ['random-source (list s10 s11 s12 s20 s21 s22)])))

   (define (random-source? s)
      (and
         (vector? s) (eq? (size s) 2)
         (eq? (ref s 1) 'random-source)
         (all (lambda (x) (eq? (type x) type-inexact)) (ref s 2))))

   (define (random-source-state-ref s)
      (if (random-source? s)
         (map bytevector-copy (ref s 2))))

   (define (random-source-state-set! s state)
      (when (random-source? s)
         (for-each (lambda (a b)
               (bytevector-copy! a 0 b))
            (ref s 2)
            state)
         #true))

   (define (random-source-randomize! s)
      (when (random-source? s)
         (let*((ss ms (clock))
               (state (ref s 2)))
            (bytevector-copy! (list-ref state 0) 0 (inexact (mod ss M1)))
            (bytevector-copy! (list-ref state 3) 0 (inexact (mod ms M1))))
         #true))

   (define (random-source-pseudo-randomize! s i j)
      (when (random-source? s)
         (let ((state (ref s 2)))
            (bytevector-copy! (list-ref state 0) 0 (inexact (mod i M1)))
            (bytevector-copy! (list-ref state 1) 0 (inexact 0))
            (bytevector-copy! (list-ref state 2) 0 (inexact 0))
            (bytevector-copy! (list-ref state 3) 0 (inexact (mod j M1)))
            (bytevector-copy! (list-ref state 4) 0 (inexact 0))
            (bytevector-copy! (list-ref state 5) 0 (inexact 0)))
         #true))

   (define (random-source-make-integers s)
      (when (random-source? s)
         (define state (ref s 2))
         (lambda (n)
            (let ((real (apply MRG32k3a state)))
               (exact (floor (* real n)))))))

   (define random-source-make-reals (case-lambda
      ((s)
         (when (random-source? s)
            (define state (ref s 2))
            (lambda ()
               (apply MRG32k3a state))))
      ((s unit)
         (when (random-source? s)
            (define state (ref s 2))
            (lambda ()
               (apply MRG32k3a state))))))

))
