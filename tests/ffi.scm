(import (otus ffi))

; tests for 16-bit:
(define INT16_MAX 32767)
(define INT32_MAX 2147483647)
(define INT64_MAX 9223372036854775807)

(define UINT16_MAX 65535)
(define UINT32_MAX 4294967295)
(define UINT64_MAX 18446744073709551615)

(define (try tag function number)
   (print "(" tag " " number "):")
   (print (function number)))

; ----------------------------------------------------------------
(print "### 16-bit:\n")
(begin
   (print "*** short fft_16i(short i) ***")
   (define fft_16i ((load-dynamic-library #f) fft-int16 "fft_16i" fft-int16))
   (try "fft_16i" fft_16i 0)
   (try "fft_16i" fft_16i 1)
   (try "fft_16i" fft_16i -1)
   (try "fft_16i" fft_16i INT16_MAX)
   (try "fft_16i" fft_16i (- INT16_MAX))
   ;(try "incorrect call for fft_16i" fft_16i UINT16_MAX)
   ;(try "incorrect call for fft_16i" fft_16i (- UINT16_MAX))
   (print "\n"))

(begin
   (print "*** unsigned short fft_16u(unsigned short i) ***")
   (define fft_16u ((load-dynamic-library #f) fft-uint16 "fft_16u" fft-uint16))
   (try "fft_16u" fft_16u 0)
   (try "fft_16u" fft_16u 1)
   ;(try "incorrect call for fft_16u" fft_16u -1)
   (try "fft_16u" fft_16u INT16_MAX)
   ;(try "incorrect call for fft_16u" fft_16u (- INT16_MAX))
   (try "fft_16u" fft_16u UINT16_MAX)
   ;(try "incorrect call for fft_16u" fft_16u (- UINT16_MAX))
   (print "\n"))

; ----------------------------------------------------------------
(print "### 32-bit:\n")

(begin
   (print "*** int fft_32i(int i) ***")
   (define fft_32i ((load-dynamic-library #f) fft-int32 "fft_32i" fft-int32))
   (try "fft_32i" fft_32i 0)
   (try "fft_32i" fft_32i 1)
   (try "fft_32i" fft_32i -1)
   (try "fft_32i" fft_32i INT32_MAX)
   (try "fft_32i" fft_32i (- INT32_MAX))
   ;(try "incorrect call for fft_32i" fft_32i UINT32_MAX)
   ;(try "incorrect call for fft_32i" fft_32i (- UINT32_MAX))
   (print "\n"))

(begin
   (print "*** unsigned int fft_32u(unsigned int i) ***")
   (define fft_32u ((load-dynamic-library #f) fft-uint32 "fft_32u" fft-uint32))
   (try "fft_32u" fft_32u 0)
   (try "fft_32u" fft_32u 1)
   ;(try "incorrect call fft_32u" fft_32u -1)
   (try "fft_32u" fft_32u INT32_MAX)
   ;(try "incorrect call fft_32u" fft_32u (- INT32_MAX))
   (try "for fft_32u" fft_32u UINT32_MAX)
   ;(try "incorrect call for fft_32u" fft_32u (- UINT32_MAX))
   (print "\n"))

; ----------------------------------------------------------------
(print "### 64-bit:\n")

(begin
   (print "*** long long fft_64i(long long i) ***")
   (define fft_64i ((load-dynamic-library #f) fft-int64 "fft_64i" fft-int64))
   (try "fft_64i" fft_64i 0)
   (try "fft_64i" fft_64i 1)
   (try "fft_64i" fft_64i -1)
   (try "fft_64i" fft_64i INT64_MAX)
   (try "fft_64i" fft_64i (- INT64_MAX))
   ;(try "incorrect call for fft_64i" fft_64i UINT64_MAX)
   ;(try "incorrect call for fft_64i" fft_64i (- UINT64_MAX))
   (print "\n"))

(begin
   (print "*** unsigned long long fft_64u(unsigned long long i) ***")
   (define fft_64u ((load-dynamic-library #f) fft-uint64 "fft_64u" fft-uint64))
   (try "fft_64u" fft_64u 0)
   (try "fft_64u" fft_64u 1)
   ;(try "incorrect call fft_64u" fft_64u -1)
   (try "fft_64u" fft_64u INT64_MAX)
   ;(try "incorrect call fft_64u" fft_64u (- INT64_MAX))
   (try "for fft_64u" fft_64u UINT64_MAX)
   ;(try "incorrect call for fft_64u" fft_64u (- UINT64_MAX))
   (print "\n"))

,quit



; testing the ffi:
; f: floats
; i: ints
; d: doubles
; result of function should be sum of all arguments

(define (assert text result value)
   (if (< (/ (abs (- result value)) (abs value)) 0.0001)
      (print text " = " "ok.")
      (print text " = " result " instead of " value)))

(define i_i ((load-dynamic-library #f) type-int+ "i_i" type-int+))
(define f_f ((load-dynamic-library #f) fft-float "f_f" fft-float))
(define d_d ((load-dynamic-library #f) fft-double "d_d" fft-double))

(define fi ((load-dynamic-library #f) fft-float "fi" fft-float type-int+))
(define fii ((load-dynamic-library #f) fft-float "fii" fft-float type-int+ type-int+))
(define fiiii ((load-dynamic-library #f) fft-float "fiiii" fft-float type-int+ type-int+ type-int+ type-int+))
(define ifiii ((load-dynamic-library #f) fft-float "ifiii" type-int+ fft-float type-int+ type-int+ type-int+))
(define iiiif ((load-dynamic-library #f) fft-float "iiiif" type-int+ type-int+ type-int+ type-int+ fft-float))
(define fiiif ((load-dynamic-library #f) fft-float "fiiif" fft-float type-int+ type-int+ type-int+ fft-float))


(assert "i_i" (i_i 1)   1)
(assert "f_f" (f_f 1.1) 1.1)
(assert "d_d" (d_d 2.2) 2.2)


(assert "fi" (fi 1.1 2)     (+ 1.1 2))
(assert "fii" (fii 1.1 2 3) (+ 1.1 2 3))

(define fiiii ((load-dynamic-library #f) fft-float "fiiii" fft-float type-int+ type-int+ type-int+ type-int+))
(define ifiii ((load-dynamic-library #f) fft-float "ifiii" type-int+ fft-float type-int+ type-int+ type-int+))
(define iiiif ((load-dynamic-library #f) fft-float "iiiif" type-int+ type-int+ type-int+ type-int+ fft-float))
(define fiiif ((load-dynamic-library #f) fft-float "fiiif" fft-float type-int+ type-int+ type-int+ fft-float))

(assert "fiiii" (fiiii 1.1 2 3 4 5)   (+ 1.1 2 3 4 5))
(assert "ifiii" (ifiii 1 2.2 3 4 5)   (+ 1 2.2 3 4 5))
(assert "iiiif" (iiiif 1 2 3 4 5.5)   (+ 1 2 3 4 5.5))
(assert "fiiif" (fiiif 1.1 2 3 4 5.5) (+ 1.1 2 3 4 5.5))
(assert "fiiif" (fiiif 1.1 2 3 4 -1.1)(+ 1.1 2 3 4 -1.1))


(define iffiiiifiiffffff ((load-dynamic-library #f) fft-float "iffiiiifiiffffff"
   type-int+
   fft-float fft-float 
   type-int+ type-int+ type-int+ type-int+
   fft-float
   type-int+ type-int+
   fft-float fft-float fft-float fft-float fft-float fft-float))

;(import (otus random!))
;(define (ri) (rand! (vm:maxvalue)))
;(define (rf) (/ (ri) (ri)))
;(let loop ((n 10))
;   (let ((args (list
;      (ri) (rf) (rf)
;      (ri) (ri) (ri) (ri)
;      (rf)
;      (ri) (ri)
;      (rf) (rf) (rf) (rf) (rf) (rf))))
;   (assert args
;      (apply iffiiiifiiffffff args)
;      (apply + args))))




;(define test4 (dlsym (dlopen) type-int+ "test4" type-int+ type-int+ type-int+ type-int+))
;(define test5 (dlsym (dlopen) type-int+ "test5" type-int+ type-int+ type-int+ type-int+ type-int+))
;(define test6 (dlsym (dlopen) type-int+ "test6" type-int+ type-int+ type-int+ type-int+ type-int+ type-int+))

;(print (test6 1 2 3 4 5 6))
;(print (test4 1 2 3 4))
;(print (test5 1 2 3 4 5))
;(print "test5 = " (test5 1 2 3 4 5))
