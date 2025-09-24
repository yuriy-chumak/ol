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

(print "
---------------------------------------------------------------
void v_cc..c(n)(type a1, type a2, .., type aN)
{
   printf('{{ %u %u .. %u(n) }}', a1, a2, .., aN); fflush(stdout);
}, n = (1 .. " MAX-ARGS-COUNT ")")

(let ((N MAX-ARGS-COUNT))
(for-each (lambda (index typename Sn)
      (for-each (lambda (n)
            (define name (|v_cc..c(n)| index n))
            (define rtty (repeat typename n)) ; args types
            (define function (apply this (cons*
               fft-void name rtty)))

            (define args (take Sn n)) ; 1 2 .. n
            (try name function args))

         (iota N 1)))
   ;      unsigned types      signed types                                 floating points
   (list "C"    "S"    "I"    "c"    "c"     "s"    "s"     "i"    "i"     "f"    "f"     "d"    "d"    )
   (list uchar  ushort uint   char   char    short  short   int    int     float  float   double double )
   (list iota24 primes primes iota24 -iota24 primes -primes primes -primes primes -primes primes -primes)
))

(let ((N MAX-ARGS-COUNT))
(for-each (lambda (name n)
   (print name)
   (for-each (lambda (index typename)
         (define name (|v_cc..c(n)| index N))
         (define rtty (repeat typename N)) ; args types
         (define function (apply this (cons* void name rtty)))

         (define args (repeat n N))
         (try name function args))
      ;      unsigned types                                        signed types                                                                                     floating points
      (list "C"            "S"               ); "I"               "c"             "c"             "s"              "s"              "i"            "i"             "f"       "f"       "d"        "d"       )
      (list uchar          ushort            ) ));  uint  char char fft-signed-short fft-signed-short fft-signed-int fft-signed-int  fft-float fft-float fft-double fft-double)
   '("zeroes:" "defaults (#f):")
   '(0          #false)
))

(print "limits:")
(let ((N MAX-ARGS-COUNT))
(for-each (lambda (index typename n)
      (define name (|v_cc..c(n)| index N))
      (define rtty (repeat typename N)) ; args types
      (define function (apply this (cons* void name rtty)))

      (try name function (repeat n N)))
   ;      unsigned types                          signed types
   (list "C"       "S"        "I"        "c"      "c"      "s"       "s"       "i"       "i"      )
   (list uchar     ushort     uint       char     char     short     short     int       int      )
   (list UINT8_MAX UINT16_MAX UINT32_MAX INT8_MIN INT8_MAX INT16_MIN INT16_MAX INT32_MIN INT32_MAX)
))

;; ; special case:
;; "L"          
;; ulonglong    
;; (,UINT64_MAX)

;; ulonglong