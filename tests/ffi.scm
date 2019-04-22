(import (otus ffi))

; testing constants for 16-bit:
(define INT16_MAX 32767)
(define -INT16_MAX -32767)
(define INT32_MAX 2147483647)
(define -INT32_MAX -2147483647)
(define INT64_MAX 9223372036854775807)
(define -INT64_MAX -9223372036854775807)

(define UINT16_MAX 65535)
(define UINT32_MAX 4294967295)
(define UINT64_MAX 18446744073709551615)

(define (try tag function . numbers)
   (for-each display (list "   " tag " " numbers ":"))
   (print (apply function numbers)))

(define this (load-dynamic-library #f))

; - simple type->type mirroring functions ------------------------
(print "basic simple type>type test:")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name type)))
         (try name function 1)
         (try name function 9)
         (try name function 0)))
   `( ("c2c" . ,fft-unsigned-char)
      ("s2s" . ,fft-unsigned-short)
      ("i2i" . ,fft-unsigned-int)
      ("l2l" . ,fft-unsigned-long)
      ("q2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name type)))
         (try name function 1)
         (try name function 9)
         (try name function 0)
         (try name function -9)
         (try name function -1)))
   `( ("C2C" . ,fft-signed-char)
      ("S2S" . ,fft-signed-short)
      ("I2I" . ,fft-signed-int)
      ("L2L" . ,fft-signed-long)
      ("Q2Q" . ,fft-signed-long-long)))


; ----------------------------------------------------------------
(print "type limits checking:")

(define mirror (this fft-short "S2S" fft-short))
   (try "short" mirror INT16_MAX)
   (try "short" mirror -INT16_MAX)
   ;(try "incorrect call for fft_16i" fft_16i UINT16_MAX)
   ;(try "incorrect call for fft_16i" fft_16i (- UINT16_MAX))
(define mirror (this fft-unsigned-short "s2s" fft-unsigned-short))
   (try "unsigned short" mirror INT16_MAX)
   (try "unsigned short" mirror UINT16_MAX)

(define mirror (this fft-int "I2I" fft-int))
   (try "int" mirror INT32_MAX)
   (try "int" mirror -INT32_MAX)
(define mirror (this fft-unsigned-int "i2i" fft-unsigned-int))
   (try "unsigned int" mirror INT32_MAX)
   (try "unsigned int" mirror UINT32_MAX)

(define mirror (this fft-long-long "Q2Q" fft-long-long))
   (try "long long" mirror INT64_MAX)
   (try "long long" mirror -INT64_MAX)
(define mirror (this fft-unsigned-long-long "q2q" fft-unsigned-long-long))
   (try "unsigned long long" mirror INT64_MAX)
   (try "unsigned long long" mirror UINT64_MAX)


; ----------------------------------------------------------------
(print "floating points type>type test:")

(define mirror (this fft-float "f2f" fft-float))
   (try "float" mirror 0.0)
   (try "float" mirror 1.1)
   (try "float" mirror (inexact 1.1))
   (try "float" mirror -1.1)
   (try "float" mirror (inexact -1.1))

(define mirror (this fft-double "d2d" fft-double))
   (try "double" mirror 0.0)
   (try "double" mirror 1.1)
   (try "double" mirror (inexact 1.1))
   (try "double" mirror -1.1)
   (try "double" mirror (inexact -1.1))


; ----------------------------------------------------------------
(print "16 integer arguments type>type test:")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name type type type type type type type type type type type type type type type type)))
         (try name function 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
         (try name function 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))
   `( ("cccccccccccccccc2c" . ,fft-unsigned-char)
      ("ssssssssssssssss2s" . ,fft-unsigned-short)
      ("iiiiiiiiiiiiiiii2i" . ,fft-unsigned-int)
      ("llllllllllllllll2l" . ,fft-unsigned-long)
      ("qqqqqqqqqqqqqqqq2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name type type type type type type type type type type type type type type type type)))
         (try name function 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
         (try name function 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)
         (try name function 0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15)
         (try name function -15 -14 -13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0)))
   `( ("CCCCCCCCCCCCCCCC2C" . ,fft-signed-char)
      ("SSSSSSSSSSSSSSSS2S" . ,fft-signed-short)
      ("IIIIIIIIIIIIIIII2I" . ,fft-signed-int)
      ("LLLLLLLLLLLLLLLL2L" . ,fft-signed-long)
      ("QQQQQQQQQQQQQQQQ2Q" . ,fft-signed-long-long)))


; ----------------------------------------------------------------
(print "16 floating point arguments type>type test:")

(define summ (this fft-float "ffffffffffffffff2f" fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float))
   (try "16 floats" summ 0.0 1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 10.10 11.11 12.12 13.13 14.14 15.15)
   (try "16 floats" summ 15.15 14.14 13.13 12.12 11.11 10.10 9.9 8.8 7.7 6.6 5.5 4.4 3.3 2.2 1.1 0.0)
   ;; (try "16 floats" summ (inexact 1.1) (inexact 2.2) (inexact 3.3) (inexact 4.4)
   ;;                       (inexact 5.5) (inexact 6.6) (inexact 7.7) (inexact 8.8)
   ;;                       (inexact 9.9) (inexact 10.10) (inexact 11.11) (inexact 12.12)
   ;;                       (inexact 13.13) (inexact 14.14) (inexact 15.15) (inexact 16.16))

; ----------------------------------------------------------------
(print "12 mixed type variables test:")
(define summ (this fft-double "cCsSiIlLqQfd2d" fft-signed-char fft-unsigned-char fft-signed-short fft-unsigned-short
                                               fft-signed-int fft-unsigned-int fft-signed-long fft-unsigned-long
                                               fft-signed-long-long fft-unsigned-long-long
                                               fft-float fft-double))
   (try "12 arguments" summ 0 1 2 3 4 5 6 7 8 9 1234.56789 123456.789)

; ---------------------------------------------------------------
; callbacks
(define cb1 (vm:pin (cons
   (list fft-int)
   (lambda (i)
      (for-each display (list "[" i "]"))
      (* i i)))))

(define callback_call_i ((load-dynamic-library #f) fft-void "callback_call_i" type-callable))

(let ((callback (make-callback cb1)))
   (if callback
      (callback_call_i callback)))

; ============
(print "done.")
,quit

;; (define fiiii ((load-dynamic-library #f) fft-float "fiiii" fft-float type-int+ type-int+ type-int+ type-int+))
;; (define ifiii ((load-dynamic-library #f) fft-float "ifiii" type-int+ fft-float type-int+ type-int+ type-int+))
;; (define iiiif ((load-dynamic-library #f) fft-float "iiiif" type-int+ type-int+ type-int+ type-int+ fft-float))
;; (define fiiif ((load-dynamic-library #f) fft-float "fiiif" fft-float type-int+ type-int+ type-int+ fft-float))

(try "i_i" i_i 1)

; testing floats
(try "f_f" f_f 1.1)
(try "f_f" f_f (inexact 1.1))
(try "f_f" f_f 1111.1)
(try "f_f" f_f (inexact 1111.1))

; testing doubles
(try "d_d" d_d 2.2)
(try "d_d" d_d (inexact 2.2))
(try "d_d" d_d 2222.2)
(try "d_d" d_d (inexact 2222.2))


(define fi ((load-dynamic-library #f) fft-float "fi" fft-float fft-int))
(define fii ((load-dynamic-library #f) fft-float "fii" fft-float fft-int fft-int))

(try "fi" fi 1.1 2)
(try "fii" fii 1.1 2 3)

,quit

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
