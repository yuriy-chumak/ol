#!/usr/bin/env -S ../../ffi ../../repl
,load "definitions"

(define (try tag function args)
   (for-each display (list "   " (cons tag args) " --> "))
   (let ((out (apply function args)))
      (print " = " out)))

(print "
---------------------------------------------------------------
type c_cc..c(n)(type a1, type a2, .., type aN)
{
   type y = a1 + a2 + .. + aN;
   printf('{{ %u %u .. %u(n) => format }}', a1, a2, .., aN, y); fflush(stdout);
   return y;
}, n = (1 .. 24)")

(for-each (lambda (index typename Sn N)
      (for-each (lambda (n)
            (define name (|c_cc..c(n)| index n))
            (define rtty (repeat typename n)) ; args types
            (define function (apply this (cons*
               typename name rtty)))

            (define args (take Sn n)) ; 1 2 .. n
            (try name function args))

         (iota N 1)))
   ; unsigned types                                          ; signed types                                      ; floating points
   `("C"                "S"                 "I"                "c"              "s"               "i"              "f"        "d"        )
   `(,fft-unsigned-char ,fft-unsigned-short ,fft-unsigned-int  ,fft-signed-char ,fft-signed-short ,fft-signed-int  ,fft-float ,fft-double)
   `(,(iota 22 1)       ,primes             ,primes            ,(iota 15 1)     ,primes           ,primes          ,primes    ,primes    )
   `(22                 24                  24                 15               23                24               24         24         )
)
