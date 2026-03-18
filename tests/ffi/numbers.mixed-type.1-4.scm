#!/usr/bin/env -S ../../ffi ../../repl
,load "definitions"

(define -primes (map negate primes))
(define iota24 (iota 24 1))
(define -iota24 (map negate iota24))

(print "
---------------------------------------------------------------
all numeric types together")

; -----------------------------------------------------------------------------
(let ((MAX-ARGS-COUNT 4))
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
