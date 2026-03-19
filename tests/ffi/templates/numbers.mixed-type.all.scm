#!/usr/bin/env -S ../../ffi ../../repl
,load "definitions"

(define -primes (map negate primes))
(define iota24 (iota 24 1))
(define -iota24 (map negate iota24))

(print "
---------------------------------------------------------------
all numeric types together")

; -----------------------------------------------------------------------------
(print "
-------------------------------------------------------------------------------
void v_cc..d[i]..c(n)(typeA a1, typeA a2, .., typeB b, .., type an)
{
   printf('{{ %d %d .. %u .. %d(n) }}', a1, a2, .., b , .., aN); fflush(stdout);
}, n = " MAX-ARGS-COUNT "")

(define indices '("C" "S" "I" "Q" "c" "s" "i" "q" "f" "d"))
(define typenames (list fft-unsigned-char fft-unsigned-short fft-unsigned-int fft-unsigned-long-long fft-signed-char fft-signed-short fft-signed-int fft-long-long fft-float fft-double))
(assert (= (length indices) (length typenames)))

(define M (max MIN-ARGS-COUNT 2))

(for-each (lambda (N)
(for-each (lambda (index typename)
      (for-each (lambda (indexA typeA indexB typeB)
            (unless (= indexA indexB)
            (define words (+
                  (* (- N 1) (if (m/Q|q|d/ indexA) 2 1))
                  (if (m/Q|q|d/ indexB) 2 1)))
            (unless (> words MAX-ARGS-COUNT)
            (for-each (lambda (n)
                  (define name (|v_cc..d[i]..c(n)| indexA indexB n N))
                  ;; (print words " " indexA " " indexB " " n " -> " name)
                  (define function (apply this (cons*
                     void name (map (lambda (x)
                                       (if (= x n) typeB typeA))
                                  (iota N 1)))))
                  (try name function (iota N 1))
                  #t)
               (iota N 1)))))
         ; unsigned types, signed types, floating points
         (repeat index (length indices))
         (repeat typename (length typenames))
         indices
         typenames))
   indices
   typenames
)) (iota (- MAX-ARGS-COUNT M -1) M))
