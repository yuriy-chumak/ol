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
all numeric types together")

(define name "v_CSIQcsiqfd")
(define function (this void name uchar ushort uint ullong char short int llong float double))
(define F #f)

(try name function (list 0 0 0 0 0 0 0 0 0 0))
(try name function (list 1 1 1 1 1 1 1 1 1 1))
(try name function (list F F F F F F F F F F))
(try name function (list F F F F -1 -1 -1 -1 -1 -1))
(try name function (list UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX  INT8_MAX INT16_MAX INT32_MAX INT64_MAX 1e10 1e48))
(try name function (list F F F F  INT8_MIN INT16_MIN INT32_MIN INT64_MIN -1e10 -1e48))


; -----------------------------------------------------------------------------
(let ((MAX-ARGS-COUNT 16))
(print "
-------------------------------------------------------------------------------
void v_cc..d[i]..c(n)(typeA a1, typeA a2, .., typeB b, .., type an)
{
   printf('{{ %d %d .. %u .. %d(n) }}', a1, a2, .., b , .., aN); fflush(stdout);
}, n = " MAX-ARGS-COUNT "")

(define indices '("C" "S" "I" "Q" "c" "s" "i" "q" "f" "d"))
(define typenames (list fft-unsigned-char fft-unsigned-short fft-unsigned-int fft-unsigned-long-long fft-signed-char fft-signed-short fft-signed-int fft-long-long fft-float fft-double))
(assert (= (length indices) (length typenames)))

(for-each (lambda (N)
(for-each (lambda (index typename)
      (for-each (lambda (indexA typeA indexB typeB)
            (unless (= indexA indexB)
            (for-each (lambda (n)
                  (define name (|v_cc..d[i]..c(n)| indexA indexB n N))
                  (define function (apply this (cons*
                     void name (map (lambda (x)
                                       (if (= x n) typeB typeA))
                                  (iota N 1)))))
                  (try name function (iota N 1))

                  #f)
               (iota N 1))))
         ; unsigned types, signed types, floating points
         (repeat index (length indices))
         (repeat typename (length typenames))
         indices
         typenames))
   indices
   typenames
)) (iota (- MAX-ARGS-COUNT 1) 2)))


;; (for-each (lambda (index typename Sn)
;;       (define N (if (m/Q|q/ index) (/ MAX-ARGS-COUNT 2) MAX-ARGS-COUNT))
;;       (for-each (lambda (n)
;;             (define name (|v_cc..c(n)| index n))
;;             (define rtty (repeat typename n)) ; args types
;;             (define function (apply this (cons*
;;                fft-void name rtty)))

;;             (define args (take Sn n)) ; 1 2 .. n
;;             (try name function args))

;;          (iota N 1)))
;;    ;      unsigned types      signed types                                 floating points
;;    (list "C"    "S"    "I"    "L"    "c"    "c"     "s"    "s"     "i"    "i"     "l"    "l"     "f"    "f"     "d"    "d"    )
;;    (list uchar  ushort uint   ullong char   char    short  short   int    int     llong  llong   float  float   double double )
;;    (list iota24 primes primes primes iota24 -iota24 primes -primes primes -primes primes -primes primes -primes primes -primes)
;; )

;; (for-each (lambda (comment n)
;;       (print comment)
;;       (for-each (lambda (index typename)
;;             (define N (if (m/Q|q/ index) (/ MAX-ARGS-COUNT 2) MAX-ARGS-COUNT))
;;             (define name (|v_cc..c(n)| index N))
;;             (define rtty (repeat typename N)) ; args types
;;             (define function (apply this (cons* void name rtty)))

;;             (define args (repeat n N))
;;             (try name function args))
;;          ;      unsigned types   signed types   floating points
;;          (list "C"   "S"    "I"  "L"    "c"  "s"   "i" "l"   "f"   "d"   )
;;          (list uchar ushort uint ullong char short int llong float double) ))
;;    '("zeroes:" "defaults (#f):")
;;    '(0          #false)
;; )

;; (print "limits:")
;; (for-each (lambda (index typename n)
;;       (define N (if (m/Q|q/ index) (/ MAX-ARGS-COUNT 2) MAX-ARGS-COUNT))
;;       (define name (|v_cc..c(n)| index N))
;;       (define rtty (repeat typename N)) ; args types
;;       (define function (apply this (cons* void name rtty)))

;;       (try name function (repeat n N)))
;;    ;      unsigned types                          signed types
;;    (list "C"       "S"        "I"        "L"        "c"      "c"      "s"       "s"       "i"       "i"       "l"       "l"      )
;;    (list uchar     ushort     uint       ullong     char     char     short     short     int       int       llong     llong    )
;;    (list UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX INT8_MIN INT8_MAX INT16_MIN INT16_MAX INT32_MIN INT32_MAX INT64_MIN INT64_MAX)
;; )
