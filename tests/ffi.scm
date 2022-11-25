(import (otus ffi))

(define (try tag function . numbers)
   (for-each display (list "   " tag " " numbers ">"))
   (display (apply function numbers))
   (print))
(define (try* tag function . numbers)
   (for-each display (list "   " tag " " numbers ">"))
   (display (apply function numbers))
   (print))
(define (try& tag function . numbers)
   (for-each display (list "   " tag " " numbers ">"))
   (display (apply function numbers))
   (print " -> " numbers))

(define this (load-dynamic-library #f))
(define (copy x) (vm:cast x (type x)))

;; legend:
;;    c - unsigned char
;;    s - unsigned short
;;    i - unsigned int
;;    l - unsigned long
;;    q - unsigned long long
;;    C - signed char
;;    S - signed short
;;    I - signed int
;;    L - signed long
;;    Q - signed long long
;;    f - float
;;    d - double

(print "type limits checking:")

(define INT8_MIN -128)
(define INT8_MAX +127)

(define INT16_MIN -32768)
(define INT16_MAX +32767)

(define INT32_MIN -2147483648)
(define INT32_MAX +2147483647)

(define INT64_MIN -9223372036854775808)
(define INT64_MAX +9223372036854775807)

(define UINT8_MAX  255)
(define UINT16_MAX 65535)
(define UINT32_MAX 4294967295)
(define UINT64_MAX 18446744073709551615)

; note: char is signed by default
(for-each (lambda (v)
      (cond
      ((eq? (size v) 5)
         (vector-apply v (lambda (type-name type-value name min max)
               (define mirror (this type-value name type-value))
               (try type-name mirror min)
               (try type-name mirror max))))
      ((eq? (size v) 4)
         (vector-apply v (lambda (type-name type-value name v)
               (define mirror (this type-value name type-value))
               (try type-name mirror v))))))
   (list
      ["char"                 fft-char "C2C" INT8_MIN INT8_MAX]
      ;; ["invalid! char (must got -1)" fft-char "C2C" UINT8_MAX]
      ;; ["invalid! char"         fft-unsigned-char "C2C" UINT8_MAX]
      ["unsinged char"        fft-unsigned-char "c2c" INT8_MAX UINT8_MAX]
      ;; ["invalid! unsinged char (must got max)" fft-unsigned-char "c2c" -1]
      ;; ["invalid! unsinged char (must got positive)" fft-unsigned-char "c2c" INT8_MIN]
      ;; ["invalid! unsinged char" fft-char "c2c" -1]
      ;; ["invalid! unsinged char" fft-char "c2c" INT8_MIN]

      ["short"                fft-short "S2S" INT16_MIN INT16_MAX]
      ["unsinged short"       fft-unsigned-short "s2s" INT16_MAX UINT16_MAX]

      ["int"                  fft-int "I2I" INT32_MIN INT32_MAX]
      ["unsinged int"         fft-unsigned-int "i2i" INT32_MAX UINT32_MAX]

      ["long long"            fft-long-long "Q2Q" INT64_MIN INT64_MAX]
      ["unsinged long long"   fft-unsigned-long-long "q2q" INT64_MAX UINT64_MAX]
   ))

; - simple type -> type mirroring functions ------------------------
(print "type function(type arg) { return arg; }")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name type)))
         (for-each (lambda (arg)
               (try name function arg))
            '(1 125 0))))
   `( ("c2c" . ,fft-unsigned-char)
      ("s2s" . ,fft-unsigned-short)
      ("i2i" . ,fft-unsigned-int)
      ("l2l" . ,fft-unsigned-long)
      ("q2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name type)))
         (for-each (lambda (arg)
               (try name function arg))
            '(1 125 0 -125 -1))))
   `( ("C2C" . ,fft-signed-char)
      ("S2S" . ,fft-signed-short)
      ("I2I" . ,fft-signed-int)
      ("L2L" . ,fft-signed-long)
      ("Q2Q" . ,fft-signed-long-long)))

(define mirror (this fft-float "f2f" fft-float))
(for-each (lambda (arg)
      (try "f2f" mirror arg))
   '(0.0 #i0.0 125.125 #i125.125 -125.125 #i-125.125))

(define mirror (this fft-double "d2d" fft-double))
(for-each (lambda (arg)
      (try "d2d" mirror arg))
   '(0.0 #i0.0 125.125 #i125.125 -125.125 #i-125.125))

; - simple type* -> type mirroring functions ------------------------
(print "type function(type* arg) { return *arg; }")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name (fft* type))))
         (for-each (lambda (args)
               (try& name function args)
               (try& name function (list->vector args)))
            '((1) (125) (0)))))
   `( ("pc2c" . ,fft-unsigned-char)
      ("ps2s" . ,fft-unsigned-short)
      ("pi2i" . ,fft-unsigned-int)
      ("pl2l" . ,fft-unsigned-long)
      ("pq2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name (fft* type))))
         (for-each (lambda (args)
               (try& name function args)
               (try& name function (list->vector args)))
            '((1) (125) (0) (-125) (-1)))))
   `( ("pC2C" . ,fft-signed-char)
      ("pS2S" . ,fft-signed-short)
      ("pI2I" . ,fft-signed-int)
      ("pL2L" . ,fft-signed-long)
      ("pQ2Q" . ,fft-signed-long-long)))

(define mirror (this fft-float "pf2f" (fft* fft-float)))
(for-each (lambda (arg)
      (try& "pf2f" mirror arg))
   '((0.0) (#i0.0) (125.125) (#i125.125) (-125.125) (#i-125.125)))

(define mirror (this fft-double "pd2d" (fft* fft-double)))
(for-each (lambda (arg)
      (try& "pd2d" mirror arg))
   '((0.0) (#i0.0) (125.125) (#i125.125) (-125.125) (#i-125.125)))

; - simple type* -> type mirroring functions ------------------------
(print "type function(type* arg) { *arg -= 1; return *arg + 2; }")

;; note: we don't support rational numbers as fft&

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name (fft& type))))
         (for-each (lambda (args)
               (define list-args (map copy args))
               (define vector-args (list->vector args))
               (try& name function list-args)
               (try& name function vector-args))
            '((1) (125)))))
   `( ("rc2c" . ,fft-unsigned-char)
      ("rs2s" . ,fft-unsigned-short)
      ("ri2i" . ,fft-unsigned-int)
      ("rl2l" . ,fft-unsigned-long)
      ("rq2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name (fft& type))))
         (for-each (lambda (args)
               (define list-args (map copy args))
               (define vector-args (list->vector args))
               (try& name function list-args)
               (try& name function vector-args))
            '((1) (125) (0) (-125) (-1)))))
   `( ("rC2C" . ,fft-signed-char)
      ("rS2S" . ,fft-signed-short)
      ("rI2I" . ,fft-signed-int)
      ("rL2L" . ,fft-signed-long)
      ("rQ2Q" . ,fft-signed-long-long)))

(define mirror (this fft-float "rf2f" (fft& fft-float)))
(for-each (lambda (arg)
      (try& "rf2f" mirror arg))
   '((0) (#i0.0) (#i125.125) (#i-125.125)))

(define mirror (this fft-double "rd2d" (fft& fft-double)))
(for-each (lambda (arg)
      (try& "rd2d" mirror arg))
   '((0) (#i0.0) (#i125.125) (#i-125.125)))

;; ;; ; ----------------------------------------------------------------
;; ;; ;(print "returning a structure test:")
;; ;; ;
;; ;; ;(define iiv2struct12 (this (cons type-bytevector 12) "iiv2struct12" fft-int fft-int fft-int))
;; ;; ;(try "iiv2struct12" iiv2struct12 1 2 123)
;; ;; ;(define iiv2struct20 (this (cons type-bytevector 20) "iiv2struct20" fft-int fft-int fft-int))
;; ;; ;(try "iiv2struct20" iiv2struct20 1 2 123)

; ----------------------------------------------------------------
(print "16 integer arguments test:")

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
;; todo: this test IS working but we need to find a numbers that will be same in x64 and x32 platforms
(print "16 floating point arguments test:")

(define summ (this fft-float "ffffffffffffffff2f" fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float))
   (try "16 floats" summ 0.0 1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 10.10 11.11 12.12 13.13 14.14 15.15)
   (try "16 floats" summ 15.15 14.14 13.13 12.12 11.11 10.10 9.9 8.8 7.7 6.6 5.5 4.4 3.3 2.2 1.1 0.0)
   (try "16 floats" summ #i0.0 #i1.1 #i2.2 #i3.3
                         #i4.4 #i5.5 #i6.6 #i7.7
                         #i8.8 #i9.9 #i1.2 #i2.3
                         #i3.4 #i4.5 #i5.6 #i6.7)

; ----------------------------------------------------------------
(print "12 mixed type variables test:")
(define summ (this fft-double "cCsSiIlLqQfd2d" fft-signed-char fft-unsigned-char
                                               fft-signed-short fft-unsigned-short
                                               fft-signed-int fft-unsigned-int
                                               fft-signed-long fft-unsigned-long
                                               fft-signed-long-long fft-unsigned-long-long
                                               fft-float fft-double))
   (try "12 arguments" summ 0 1 2 3 4 5 6 7 8 9 1234.625 123456.25)

; ----------------------------------------------------------------
(print "22 mixed type variables test:")
(define summ (this fft-double "cCfdsSfdiIfdlLfdqQfdfd2d"
                   fft-signed-char fft-unsigned-char
                   fft-float fft-double
                   fft-signed-short fft-unsigned-short
                   fft-float fft-double
                   fft-signed-int fft-unsigned-int
                   fft-float fft-double
                   fft-signed-long fft-unsigned-long
                   fft-float fft-double
                   fft-signed-long-long fft-unsigned-long-long
                   fft-float fft-double
                   fft-float fft-double))
   (try "22 integer arguments" summ  1  2  3  4
                                     5  6  7  8
                                     9 10 11 12
                                    13 14 15 16
                                    17 18 19 20
                                    21 22)
   (try "22 mixed arguments" summ  1  2  3.0  4.5
                                   5  6  7.25 8.75
                                   9 10 11.125 12.875
                                  13 14 15.9375 16.6875
                                  17 18 19.71875 20.96875
                                  21.90625 22.875)
   (try "22 mixed inexact arguments" summ  1  2  #i3.0  #i4.5
                                   5  6 #i7.25 #i8.75
                                   9 10 #i11.125 #i12.875
                                  13 14 #i15.9375 #i16.6875
                                  17 18 #i19.71875 #i20.96875
                                  #i21.90625 #i22.875)

; ----------------------------------------------------------------
(print "too much arguments:") ; 16 arguments for only 8 required
(let ((function (this fft-unsigned-long-long "qqqqqqqqqqqqqqqq2q"
      fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long
      fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long
      fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long
      fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long
      fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long
      fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long
      fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long
      fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long fft-unsigned-long-long)))
   (try "too much arguments" function
      1 2 3 4
      5 6 7 8
      1 2 3 4
      5 6 7 8
      1 2 3 4
      5 6 7 8
      1 2 3 4
      5 6 7 8))

;; ; ---------------------------------------------------------------
;; ; callbacks
;; (define (test-callback name types)
;;    (define cb (vm:pin (cons
;;       types
;;       (lambda args
;;          (for-each display (list "callback: [ " args " ]"))
;;          (apply * args)))))
;;    (define callback_call ((load-dynamic-library #f) fft-void name type-callable))

;;    (let ((callback (make-callback cb)))
;;       (if callback
;;          (callback_call callback)))
;;    (vm:unpin cb))


;; ;; (test-callback "callback_call_i" (list fft-int fft-int))
;; ;; (test-callback "callback_call_ii" (list fft-int fft-int fft-int))
;; ;; (test-callback "callback_call_iii" (list fft-int fft-int fft-int fft-int))
;; ;; (test-callback "callback_call_iiii" (list fft-int fft-int fft-int fft-int fft-int))
;; ;; (test-callback "callback_call_iiiii" (list fft-int fft-int fft-int fft-int fft-int fft-int))
;; ;; (test-callback "callback_call_iiiiii" (list fft-int fft-int fft-int fft-int fft-int fft-int fft-int))
;; ;; (test-callback "callback_call_iiiiiii" (list fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int))
;; ;; (test-callback "callback_call_iiiiiiii" (list fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int))
;; ;; (test-callback "callback_call_iiiiiiiii" (list fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int))
;; ;; (test-callback "callback_call_iiiiiiiiii" (list fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int fft-int))

;; (test-callback "callback_call_f" (list fft-float fft-float))
;; (test-callback "callback_call_ifif" (list fft-float fft-int fft-float fft-int fft-float))
;; ;; (test-callback "callback_call_d" (list fft-double fft-double))
;; ;; (test-callback "callback_call_ifid" (list fft-double fft-int fft-float fft-int fft-double))

; ------------------------------------
; wide characters
(define reverse_string ((load-dynamic-library #f) type-string "reverse_string" type-string))
(define reverse_string_wide ((load-dynamic-library #f) type-string-wide "reverse_string_wide" type-string-wide))

(for-each (lambda (str)
      (define s (reverse_string str))
      (print "reverse_string(" str "): " s " - "
         (if (string-eq? str (list->string (reverse (string->list s))))
            "ok. " "fail."))
      (define w (reverse_string_wide str))
      (print "reverse_string_wide(" str "): " w " - "
         (if (string-eq? str (list->string (reverse (string->list w))))
            "ok. " "fail.")))
   '("hello"
     "привет"
     "Совы (Strigiformes) суть релатівно чісленым рядом класы птахів обсягуюча веце як 200 видів."
     "ბუსნაირნი (ლათ. Strigiformes) — ფრინველთა რიგი. ფართოდაა გავრცელებული მსოფლიოში (ანტარქტიკული და ზოგიერთი ოკეანური კუნძულის გამოკლებით)."
     "フクロウ目（フクロウもく、梟目、学名 Strigiformes）は鳥類の1目である。"))

;; ; --------

;; ;; (define cb1 (vm:pin (cons
;; ;;    (list fft-int)
;; ;;    (lambda (i)
;; ;;       (for-each display (list "[" i "]"))
;; ;;       (* i i)))))

;; ;; (define callback_call_i ((load-dynamic-library #f) fft-void "callback_call_i" type-callable))

;; ;; (let ((callback (make-callback cb1)))
;; ;;    (if callback
;; ;;       (callback_call_i callback)))


;; ;; (define cb-ii (vm:pin (cons
;; ;;    (list fft-int fft-int)
;; ;;    (lambda (i j)
;; ;;       (for-each display (list "[" i ", " j "]"))
;; ;;       (* i j)))))
;; ;; (define callback_call_ii ((load-dynamic-library #f) fft-void "callback_call_ii" type-callable))

;; ;; (let ((callback (make-callback cb-ii)))
;; ;;    (if callback
;; ;;       (callback_call_ii callback)))

; -----------------------------
; -=( fft-any )=---------------
; - simple type->type mirroring functions ------------------------

; special print case - we hide "(fft* type)" from output
; because fft-long value are different for x32 and x64.
(define (try-a tag function . numbers)
   (for-each display (list "   " tag " " (map cdr numbers) ":"))
   (display (apply function numbers))
   (print))
(define (try*-a tag function . numbers)
   (for-each display (list "   " tag " " (map cdr numbers) ":"))
   (display (apply function numbers))
   (print))
(define (try&-a tag function . numbers)
   (for-each display (list "   " tag " " (map cdr numbers) ":"))
   (display (apply function numbers))
   (print " -> " (cdar numbers)))


(print "simple fft-any > type test:")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try-a name function (cons type 1))))
   `( ("c2c" . ,fft-unsigned-char)
      ("s2s" . ,fft-unsigned-short)
      ("i2i" . ,fft-unsigned-int)
      ("l2l" . ,fft-unsigned-long)
      ("q2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try-a name function (cons type 1))
         (try-a name function (cons type -1))))
   `( ("C2C" . ,fft-signed-char)
      ("S2S" . ,fft-signed-short)
      ("I2I" . ,fft-signed-int)
      ("L2L" . ,fft-signed-long)
      ("Q2Q" . ,fft-signed-long-long)))

(define mirror (this fft-float "f2f" fft-any))
   (try-a "float" mirror (cons fft-float #i125.125))
   (try-a "float" mirror (cons fft-float #i-125.125))

(define mirror (this fft-double "d2d" fft-any))
   (try-a "double" mirror (cons fft-double #i125.125))
   (try-a "double" mirror (cons fft-double #i-125.125))

;; (fft* ...)
(print "simple fft-any (fft*)) > type test:")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try*-a name function (cons (fft* type) '(1)))))
   `( ("pc2c" . ,fft-unsigned-char)
      ("ps2s" . ,fft-unsigned-short)
      ("pi2i" . ,fft-unsigned-int)
      ("pl2l" . ,fft-unsigned-long)
      ("pq2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try*-a name function (cons (fft* type) '(1)))
         (try*-a name function (cons (fft* type) '(-1)))))
   `( ("pC2C" . ,fft-signed-char)
      ("pS2S" . ,fft-signed-short)
      ("pI2I" . ,fft-signed-int)
      ("pL2L" . ,fft-signed-long)
      ("pQ2Q" . ,fft-signed-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try*-a name function (cons (fft& type) (map inexact '(125.125))))
         (try*-a name function (cons (fft& type) (map inexact '(-125.125))))))
   `( ("rf2f" . ,fft-float)
      ("rd2d" . ,fft-double)))

;; (fft& ...)
(print "extended fft-any (fft&)) > type test:")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try&-a name function (cons (fft& type) (map copy '(1 2 0))))
         (try&-a name function (cons (fft& type) (vector-map copy [1 2 0])))
         (try&-a name function (cons (fft& type) (map copy '(125 7 0))))
         (try&-a name function (cons (fft& type) (vector-map copy [125 7 0])))
         (try&-a name function (cons (fft& type) (map copy '(7 125 0))))
         (try&-a name function (cons (fft& type) (vector-map copy [7 125 0])))))
   `( ("rpc2c3" . ,fft-unsigned-char)
      ("rps2s3" . ,fft-unsigned-short)
      ("rpi2i3" . ,fft-unsigned-int)
      ;("rpl2l3" . ,fft-unsigned-long)
      ;("rpq2q3" . ,fft-unsigned-long-long)
      ))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try&-a name function (cons (fft& type) (map copy '(1 2 0))))
         (try&-a name function (cons (fft& type) (vector-map copy [1 2 0])))
         (try&-a name function (cons (fft& type) (map copy '(125 7 0))))
         (try&-a name function (cons (fft& type) (vector-map copy [125 7 0])))
         (try&-a name function (cons (fft& type) (map copy '(7 125 0))))
         (try&-a name function (cons (fft& type) (vector-map copy [7 125 0])))))
   `( ("rpC2C3" . ,fft-unsigned-char)
      ("rpS2S3" . ,fft-unsigned-short)
      ("rpI2I3" . ,fft-unsigned-int)
      ;("rpl2l3" . ,fft-unsigned-long)
      ;("rpq2q3" . ,fft-unsigned-long-long)
      ))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try&-a name function (cons (fft& type) (map inexact '(1 2 0))))
         (try&-a name function (cons (fft& type) (vector-map inexact [1 2 0])))
         (try&-a name function (cons (fft& type) (map inexact '(125.125 7 0))))
         (try&-a name function (cons (fft& type) (vector-map inexact [125.125 7 0])))
         (try&-a name function (cons (fft& type) (map inexact '(7 125.125 0))))
         (try&-a name function (cons (fft& type) (vector-map inexact [7 125.125 0])))
         (try&-a name function (cons (fft& type) (map inexact '(-125.125 -125.125 0))))
         (try&-a name function (cons (fft& type) (vector-map inexact [-125.125 -125.125 0])))
         (try&-a name function (cons (fft& type) (map inexact '(125.125 -125.125 0))))
         (try&-a name function (cons (fft& type) (vector-map inexact [125.125 -125.125 0])))))
   `( ("rpf2f3" . ,fft-float)
      ("rpd2d3" . ,fft-double)))

; ============
; vararg support
(define format (this fft-int "format" type-string))
(format "[%i %f %i %f]\n"
   (cons fft-int 42)
   (cons fft-double 43.34)
   (cons fft-int 44)
   (cons fft-double 45.54))

;=============
(print "done.")
