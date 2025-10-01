#!/usr/bin/env -S ../../ffi ../../repl
,load "definitions"

(define (try tag function args)
   (for-each display (list "   " (cons tag args) " --> "))
   (let ((out (apply function args)))
      (print " = " out)))

(define -primes (map negate primes))
(define iota24 (iota 24 1))
(define -iota24 (map negate iota24))

(print "
---------------------------------------------------------------
void v_cc..c(n)(type a1, type a2, .., type an)
{
   printf('{{ %d %d .. %d(n) }}', a1, a2, .., an); fflush(stdout);
}, n = (1 .. " MAX-ARGS-COUNT ")")

(for-each (lambda (index typename Sn)
      (define N MAX-ARGS-COUNT)
      (for-each (lambda (n)
            (define name (|v_cc..c(n)| index n))
            (define rtty (repeat typename n)) ; args types
            (define function (apply this (cons*
               fft-void name rtty)))

            (define args (take Sn n)) ; 1 2 .. n
            (try name function args))

         (iota N 1)))
   ;      unsigned types      signed types                                 floating points
   (list "f"    "f"     "d"    "d"    )
   (list float  float   double double )
   (list primes -primes primes -primes)
)

(for-each (lambda (comment n)
      (print comment)
      (for-each (lambda (index typename)
            ;; (define N (if (m/L|l/ index) (/ MAX-ARGS-COUNT 2) MAX-ARGS-COUNT))
            (define N MAX-ARGS-COUNT)
            (define name (|v_cc..c(n)| index N))
            (define rtty (repeat typename N)) ; args types
            (define function (apply this (cons* void name rtty)))

            (define args (repeat n N))
            (try name function args))
         ;      unsigned types   signed types   floating points
         (list "f"   "d"   )
         (list float double) ))
   '("zeroes:" "defaults (#f):" "42s:")
   '( 0         #false           42)
)
