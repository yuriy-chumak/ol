(import (otus ffi))

(define this (load-dynamic-library #f))
(define (copy x) (vm:cast x (type x)))

;; legend:
;;    C - unsigned char
;;    S - unsigned short
;;    I - unsigned int
;;    L - unsigned long
;;    Q - unsigned long long
;;    c - signed char
;;    s - signed short
;;    i - signed int
;;    l - signed long
;;    q - signed long long
;;    f - float
;;    d - double

;; disable testcases:
;; tbd.
(define T #t)
(define |cN_()| T)
(define |c_cc..c(n)(1..P)| T)

;; note:
;;    fft-char is signed by default
;;    we don't support rational numbers as fft&
(print "Notes:
  * no tests for type 'long' because 'long' is either 'int' or 'long long' depends on OS
    and is completely covered by other tests ('int' and 'long long').
  * type legend: 'c' for char, 's' for short, 'i' for int, 'q' for long long,
    capital chars for signed prefixes;  'f' for float, 'd' for double.
  * output inside {{ .. }} are native code output
")

(when (or
         (and (eq? fft-long fft-int)
            (eq? fft-unsigned-long fft-unsigned-int)
            (eq? fft-signed-long fft-signed-int))
         (and (eq? fft-long fft-long-long)
            (eq? fft-unsigned-long fft-unsigned-long-long)
            (eq? fft-signed-long fft-signed-long-long)) )
   (print "yes, all longs are all ints or all longlongs. it's tested right now!"))

; - tests ---------------------------------------------------
,load "ffi.patterns"

(define (try tag function args)
   (for-each display (list "   " (cons tag args) " --> "))
   (let ((out (apply function args)))
      (print " = " out)))

;; (define Pn '( ; 114 prime numbers
(define primes '(
       1     2     3     5     7    11   101   131   151   181   191   313
     353   373   383   727   757   787   797   919   929 10301 10501 10601
   11311 11411 12421 12721 12821 13331 13831 13931 14341 14741 15451 15551
   16061 16361 16561 16661 17471 17971 18181 18481 19391 19891 19991 30103
   30203 30403 30703 30803 31013 31513 32323 32423 33533 34543 34843 35053
   35153 35353 35753 36263 36563 37273 37573 38083 38183 38783 39293 70207
   70507 70607 71317 71917 72227 72727 73037 73237 73637 74047 74747 75557
   76367 76667 77377 77477 77977 78487 78787 78887 79397 79697 79997 90709
   91019 93139 93239 93739 94049 94349 94649 94849 94949 95959 96269 96469
   96769 97379 97579 97879 98389 98689
))
;; (define Sn ; 114 consecutive numbers 1..n
;;    (iota 114 1))

(print "
---------------------------------------------------------------
function returning numeric types by value (neutral values, type limits)
type cN_()
{
   type y = X;
   printf('{{ () => format }}', y); fflush(stdout);
   return y;
}")
(when |cN_()|
(for-each (lambda (index typename Nn)
      (for-each (lambda (n)
            (define name (string-append index (s/-/m/ (number->string n)) "_"))
            (define function (this typename name))

            (try name function '()))
         Nn))
   ; unsigned types                                       ; signed types                                   ; floating points
   '("C"                "S"                 "I"                "L"                     "c"                          "s"                            "i"                            "l"                             "f"                 "d"                )
   `(,fft-unsigned-char ,fft-unsigned-short ,fft-unsigned-int  ,fft-unsigned-long-long ,fft-signed-char             ,fft-signed-short              ,fft-signed-int                ,fft-signed-long-long           ,fft-float          ,fft-double        )
   `((0 1 ,UINT8_MAX)   (0 1 ,UINT16_MAX)   (0 1 ,UINT32_MAX)  (0 1 ,UINT64_MAX)       (,INT8_MIN -1 0 1 ,INT8_MAX) (,INT16_MIN -1 0 1 ,INT16_MAX) (,INT32_MIN -1 0 1 ,INT32_MAX) (,INT64_MIN -1 0 1 ,INT64_MAX)  (-1e10 -1 0 1 1e10) (-1e42 -1 0 1 1e42))
))

(print "
---------------------------------------------------------------
type c_cc..c(n)(type a1, type a2, .., type aN)
{
   type y = a1 + a2 + .. + aN;
   printf('{{ %u %u .. %u(n) => format }}', a1, a2, .., aN, y); fflush(stdout);
   return y;
}, n = (1 .. 24)")

(when |c_cc..c(n)(1..P)|
(for-each (lambda (index typename Sn N)
      (for-each (lambda (n)
            (define name (|c_cc..c(n)| index n))
            (define rtty (repeat typename n)) ; args types
            (define function (apply this (cons*
               typename name rtty)))

            (define args (take Sn n)) ; 1 2 .. n
            (try name function args))

         (iota N 1)))
       ; unsigned types                                       ; signed types                                   ; floating points
   (list "C"               "S"                "I"               "c"             "s"              "i"             "f"       "d"       )
   (list fft-unsigned-char fft-unsigned-short fft-unsigned-int  fft-signed-char fft-signed-short fft-signed-int  fft-float fft-double)
   (list (iota 22 1)       primes             primes            (iota 15 1)     primes           primes          primes    primes    )
   (list 22                24                 24                15              23               24              24        24        )
))

