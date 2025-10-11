#!/usr/bin/env -S ../../ffi ../../repl
,load "definitions"

(define (try tag function args)
   (for-each display (list "   " (cons tag args) " --> "))
   (let ((out (apply function args)))
      (print " = " out)))

(define -primes (map negate primes))
(define iota24 (iota 24 1))
(define -iota24 (map negate iota24))
;; (define a-24zeros (repeat 24 0))

; -----------------------------------------------------------------------------
(let ((MAX-ARGS-COUNT 4))
(print "
---------------------------------------------------------------
void v_cc..c(n)(type a1, type a2, .., type an)
{
   printf('{{ %d %d .. %d(n) }}', a1, a2, .., an); fflush(stdout);
}, n = (1 .. " MAX-ARGS-COUNT ")")

(for-each (lambda (index typename Sn)
      (define N MAX-ARGS-COUNT)
      ;; (define N (if (m/Q|q/ index) (/ MAX-ARGS-COUNT 2) MAX-ARGS-COUNT))
      (for-each (lambda (n)
            (define name (|v_cc..c(n)| index n))
            (define rtty (repeat typename n)) ; args types
            (define function (apply this (cons*
               fft-void name rtty)))

            (define args (take Sn n)) ; 1 2 .. n
            (try name function args))

         (iota N 1)))
   ;      unsigned types      signed types                                 floating points
   (list "f"    "d"   )
   (list float  double)
   (list iota24 iota24)
))
