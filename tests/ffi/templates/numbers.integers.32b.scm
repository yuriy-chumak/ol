,load "definitions"

(define -primes (map negate primes))
(define iota24 (iota 24 1))
(define -iota24 (map negate iota24))
;; (define a-24zeros (repeat 24 0))

; -----------------------------------------------------------------------------
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

         (iota (- N MIN-ARGS-COUNT -1) MIN-ARGS-COUNT)))
   ;      unsigned types      signed types
   (list "C"    "S"    "I"    "c"    "c"     "s"    "s"     "i"    "i"    )
   (list uchar  ushort uint   char   char    short  short   int    int    )
   (list iota24 iota24 iota24 iota24 -iota24 iota24 -iota24 iota24 -iota24)
)

(for-each (lambda (comment n)
      (print comment)
      (for-each (lambda (index typename)
            ;; (define N (if (m/Q|q/ index) (/ MAX-ARGS-COUNT 2) MAX-ARGS-COUNT))
            (define N MAX-ARGS-COUNT)
            (define name (|v_cc..c(n)| index N))
            (define rtty (repeat typename N)) ; args types
            (define function (apply this (cons* void name rtty)))

            (define args (repeat n N))
            (try name function args))
         ;     unsigned types      signed types
         (list "C"    "S"    "I"   "c"  "s"   "i" )
         (list uchar  ushort uint  char short int ) ))
   '("zeroes:" "defaults (#f):" "42s:")
   '( 0         #false           42)
)