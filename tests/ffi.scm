(import (otus ffi))

(define (try tag function . numbers)
   (for-each display (list "   " tag " " numbers ">"))
   (let ((out (apply function numbers)))
      (print " = " out)))
(define (try* tag function . numbers)
   (for-each display (list "   " tag " " numbers ">"))
   (let ((out (apply function numbers)))
      (print " = " out)))
(define (try& tag function . numbers)
   (for-each display (list "   " tag " " numbers ">"))
   (let ((out (apply function numbers)))
      (print " = " out ", " numbers)))

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

;; note:
;;    fft-char is signed by default
;;    we don't support rational numbers as fft&
(print "Notes:
  * no tests for type 'long' because 'long' is either 'int' or 'long long', depends on OS
    and is completely covered by other tests ('int' and 'long long').
  * type legend: 'c' for char, 's' for short, 'i' for int, 'q' for long long,
    capital chars for signed prefixes;  'f' for float, 'd' for double.
")
;;   * internal inexact numbers representation is '"
;;       (case (size #i1)
;;          (4 "float")
;;          (8 "double")
;;          (else "unknown")) "'

(when (or
         (and (eq? fft-long fft-int)
            (eq? fft-unsigned-long fft-unsigned-int)
            (eq? fft-signed-long fft-signed-int))
         (and (eq? fft-long fft-long-long)
            (eq? fft-unsigned-long fft-unsigned-long-long)
            (eq? fft-signed-long fft-signed-long-long)) )
   (print "yes, all longs are all ints or all longlongs. it's tested right now!"))

; --------------------------------------
(print "
basic numeric limits checking:")

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
      ["good char"            fft-char "C2C" INT8_MIN INT8_MAX]
      ["unsinged char"        fft-unsigned-char "c2c" INT8_MAX UINT8_MAX]

      ["short"                fft-short "S2S" INT16_MIN INT16_MAX]
      ["unsinged short"       fft-unsigned-short "s2s" INT16_MAX UINT16_MAX]

      ["int"                  fft-int "I2I" INT32_MIN INT32_MAX]
      ["unsinged int"         fft-unsigned-int "i2i" INT32_MAX UINT32_MAX]

      ["long long"            fft-long-long "Q2Q" INT64_MIN INT64_MAX]
      ["unsinged long long"   fft-unsigned-long-long "q2q" INT64_MAX UINT64_MAX]
   ))

; - type -> type mirroring functions ------------------------
(print "
// simple type to type tests:
type function(type arg)
{
    return arg;
}")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name type)))
         (for-each (lambda (arg)
               (try name function arg))
            '(1 125 0))))
   `( ("c2c" . ,fft-unsigned-char)
      ("s2s" . ,fft-unsigned-short)
      ("i2i" . ,fft-unsigned-int)
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
      ("Q2Q" . ,fft-signed-long-long)))
; float
(define mirror (this fft-float "f2f" fft-float))
(for-each (lambda (arg)
      (try "f2f" mirror arg))
   '(0.0 #i0.0 125.125 #i125.125 -125.125 #i-125.125))
; double
(define mirror (this fft-double "d2d" fft-double))
(for-each (lambda (arg)
      (try "d2d" mirror arg))
   '(0.0 #i0.0 125.125 #i125.125 -125.125 #i-125.125))

; - type* -> type mirroring functions ------------------------
(print "
type function(type* arg)
{
    return *arg;
}")

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
      ("pQ2Q" . ,fft-signed-long-long)))
; float (todo: vectors)
(define mirror (this fft-float "pf2f" (fft* fft-float)))
(for-each (lambda (arg)
      (try& "pf2f" mirror arg))
   '((0.0) (#i0.0) (125.125) (#i125.125) (-125.125) (#i-125.125)))
; double (todo: vectors)
(define mirror (this fft-double "pd2d" (fft* fft-double)))
(for-each (lambda (arg)
      (try& "pd2d" mirror arg))
   '((0.0) (#i0.0) (125.125) (#i125.125) (-125.125) (#i-125.125)))

; - type* -> type changing functions ------------------------
(print "
type function(type* arg)
{
    *arg -= 1;
    return *arg + 2;
}")

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
      ("rQ2Q" . ,fft-signed-long-long)))
; float, todo: vectors
(define mirror (this fft-float "rf2f" (fft& fft-float)))
(for-each (lambda (arg)
      (try& "rf2f" mirror arg))
   '((0) (#i0.0) (#i125.125) (#i-125.125)))
; double, todo: vectors
(define mirror (this fft-double "rd2d" (fft& fft-double)))
(for-each (lambda (arg)
      (try& "rd2d" mirror arg))
   '((0) (#i0.0) (#i125.125) (#i-125.125)))




; some mixed types
;; todo: 2 arguments, 3 arguments, 4 agruments, 5 arguments, ... 8 arguments

; ----------------------------------------------------------------
(print "
// 16 arguments test:
type function(type a0, type a1, type a2, type a3,
              type a4, type a5, type a6, type a7, 
              type a8, type a9, type aA, type aB, 
              type aC, type aD, type aE, type aF)
{
    return a0+a1+a2+a3+a4+a5+a6+a7+a8+a9+aA+aB+aC+aD+aE+aF;
}")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name type type type type type type type type type type type type type type type type)))
         (try name function 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
         (try name function 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))
   `( ("cccccccccccccccc2c" . ,fft-unsigned-char)
      ("ssssssssssssssss2s" . ,fft-unsigned-short)
      ("iiiiiiiiiiiiiiii2i" . ,fft-unsigned-int)
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
      ("QQQQQQQQQQQQQQQQ2Q" . ,fft-signed-long-long)))

;; this test IS working
;; please, use only numbers that prints exactly the same output for x64 and x32 platforms
(print "
// extended test for all floats (rational and inexacts):")
(define summ (this fft-float "ffffffffffffffff2f" fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float
                                                  fft-float fft-float fft-float fft-float))
   (try "16 rationals" summ 0.0 1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 10.10 11.11 12.12 13.13 14.14 15.15)
   (try "16 rationals" summ 15.15 14.14 13.13 12.12 11.11 10.10 9.9 8.8 7.7 6.6 5.5 4.4 3.3 2.2 1.1 0.0)
   (try "16 inexacts"  summ #i0.0   #i1.1   #i2.2   #i3.3
                            #i4.4   #i5.5   #i6.6   #i7.7
                            #i8.8   #i9.9   #i10.10 #i11.11
                            #i12.12 #i13.13 #i14.14 #i15.15)
   (try "16 mixed floats" summ
                            #i15.15 14.14   #i13.13 12.12
                            #i11.11 10.10   #i9.9   8.8
                            #i7.7   6.6     #i5.5   4.4
                            #i3.3   2.2     #i1.1   0.0)

   (try "16 rationals" summ -0.0 -1.1 -2.2 -3.3 -4.4 -5.5 -6.6 -7.7 -8.8 -9.9 -10.10 -11.11 -12.12 -13.13 -14.14 -15.15)
   (try "16 inexacts"  summ #i-0.0   #i-1.1   #i-2.2   #i-3.3
                            #i-4.4   #i-5.5   #i-6.6   #i-7.7
                            #i-8.8   #i-9.9   #i-10.10 #i-11.11
                            #i-12.12 #i-13.13 #i-14.14 #i-15.15)

; ----------------------------------------------------------------
(print "
// 8 mixed integers, 1 float, 1 double:
double cCsSiIqQfd2d(char c, ..., double d)
{
   return d+ ... +c;
}")
(define summ (this fft-double "cCsSiIqQfd2d" fft-unsigned-char fft-signed-char
                                             fft-unsigned-short fft-signed-short
                                             fft-unsigned-int fft-signed-int
                                             fft-unsigned-long-long fft-signed-long-long
                                             fft-float fft-double))
   (try "10 mixed arguments" summ  1  2  3  4  5  6  7  8  9  10)
   (try "10 mixed arguments" summ  1 -2  3 -4  5 -6  7 -8  9 -10)

; ----------------------------------------------------------------
(print "
20 mixed type variables test:
double cCfdsSfdiIfddfqQfddf2d(char c, ..., float f)
{
    return f+ ... +c;
}")
(define summ (this fft-double "cCfdsSfdiIfddfqQfddf2d"
                   fft-unsigned-char  fft-signed-char  fft-float fft-double
                   fft-unsigned-short fft-signed-short fft-float fft-double
                   fft-unsigned-int   fft-signed-int   fft-float fft-double
                   fft-double fft-float                fft-unsigned-long-long fft-signed-long-long
                   fft-float fft-double                fft-double fft-float))
   (try "20 integer arguments"
                            summ  1  2  3  4
                                  5  6  7  8
                                  9 10 11 12
                                 13 14 15 16
                                 17 18 19 20)
   (try "20 mixed exact arguments"
                            summ  1  2  3.0    4.5
                                  5  6  7.25   8.75
                                  9 10 11.125 12.875
                                 13.9375  14.6875  15 16
                                 17.71875 18.96875 19 20)
   (try "20 mixed inexact arguments"
                            summ  #i1  #i2  #i3.0    #i4.5
                                  #i5  #i6  #i7.25   #i8.75
                                  #i9 #i10 #i11.125 #i12.875
                                 #i13.9375  #i14.6875  #i15 #i16
                                 #i17.71875 #i18.96875 #i19 #i20)

; ----------------------------------------------------------------
(print "
// too much arguments (4 needed, more provided):
long long function(char c, short s, int i, long long q)
{
   return c+s+i+q;
}")
(let ((function (this fft-long-long "csiq2q" fft-char fft-short fft-int fft-long-long)))
   (try "16 arguments" function
      1 2 3 4 5 6 7 8
      1 2 3 4 5 6 7 8)
   (try "99 arguments" function
      1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8
      1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8
      1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8  7 7 7))

; ============
(print "
// variable arguments test (returns count of printed arguments):
int format(const char *format, ...)
{
   printf(format, ...);
}")

(let*((format (this fft-int "format" type-string))
      (typename (lambda (id)
         (case id
            (fft-int 'fft-int)
            (fft-long-long 'fft-long-long)
            (fft-float 'fft-float)
            (fft-double 'fft-double)
            (else (typename id 'unknown)))))
      (try (lambda (tag . args)
               (for-each display (list "   " tag " ("))
               (write (car args))
               (for-each (lambda (arg)
                     (if (pair? arg)
                        (for-each display (list
                           " (" (typename (car arg)) " . " (cdr arg) ")"))
                     else
                        (display " ") (write arg)))
                  (cdr args))
               (display ")>")
               (let ((out (apply format args)))
                  (print " = " out)))))

   (try "numbers" "%i %f %i %f"
      (cons fft-int 42)
      (cons fft-double 43.34)
      (cons fft-int 44)
      (cons fft-double 45.54))

   (try "utf-8 strings/symbols" "<%s/%s/%s/%s>"
      "ansi"
      "юнікод"
      '|σύμβολο|
      "ユニコード")
   (try "empty strings/symbols" "<%s/%s/%s>"
      "" (substring "λ" 1 1) '||)
)

; ------------------------------------
; wide characters
(print "
// reverse strings:")
(define reverse_string ((load-dynamic-library #f) type-string "reverse_string" type-string))
(define reverse_string_wide ((load-dynamic-library #f) type-string-wide "reverse_string_wide" type-string-wide))

(for-each (lambda (str)
      (define s (reverse_string str))
      (print "   reverse_string(" str "): " s " - "
         (if (string-eq? str (list->string (reverse (string->list s))))
            "ok. " "fail."))
      (define w (reverse_string_wide str))
      (print "   reverse_string_wide(" str "): " w " - "
         (if (string-eq? str (list->string (reverse (string->list w))))
            "ok. " "fail.")))
   '("hello"
     "привіт"
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
; structures
; -----------------------------
(print "
// structure by reference:
struct args_t
{
	int argc;
	struct {
		char** argv;
	} x;
	char c;
};")
; by reference
(define struct_t (struct
   fft-int ; argc
   (struct
      (fft* type-string)) ; array of strings
   fft-char)) ; char
(define struct_t* (fft* struct_t))

; struct by reference
(define debug (this fft-void "debug_args" struct_t*))
(debug      (list 3
                  (list
                     (list "only" "three" "arguments" "from" "the" "six"))
                  #\C))

(define debug (this fft-void "debug_args" fft-any))
(debug (cons struct_t* ;(list fft-int (list (fft* type-string)) fft-char)
            (list 6
                  (list
                     (list "a" "lot" "of" "аргументов" (string-append "incl" "uding")
                           "very-very-very-very-very-very-very-very-long-argument"))
                  #\C)))

(print "
// structure by reference with writeback:
struct csicisc_t
{
   char c1;
   short s1;
   int i1;
   struct {
      char c2;
      int i2;
      short s2;
   } substruct;
   char c3;
};")
; by reference
(define csicisc_t (struct
   fft-char
   fft-short
   fft-int
   (struct
      fft-char
      fft-int
      fft-short
      fft-short) ; substructure padding
   fft-char))
(define csicisc_t& (fft& csicisc_t))

; struct by reference
(define debug (this fft-void "debug_csicisc" csicisc_t&))
(define var (list #\A 2 3
               (list
                  #\B 5 6 0) ; 0 for substructure padding
               #\C))
(print "> " var)
(debug var)
(print "< " var)

; struct by value
(print "
// (small) structure by value:
int function(struct { type x, type y } a)
{
   return a.x + a.y;
}")
(for-each (lambda (name type)
         (for-each (lambda (subname subtype)
               (define realname (string-append "_" name subname "_2i"))
               (define function (this fft-int realname (list type subtype)))
               (for-each (lambda (arg)
                     (try realname function arg))
                  '((1 2))))
            (list    "c"      "s"       "i"     "q"           "f"       "d")
            (list fft-char fft-short fft-int fft-long-long fft-float fft-double)))
   (list    "c"      "s"       "i"     "q"           "f"       "d")
   (list fft-char fft-short fft-int fft-long-long fft-float fft-double))

(print "
// (small) structure by value:
int function(type a, struct { type x, type y } b)
{
   return a + b.x + b.y;
}")
(for-each (lambda (prename pretype)
      (for-each (lambda (name type)
               (for-each (lambda (subname subtype)
                     (define realname (string-append "_" prename "_" name subname "_2i"))
                     (define function (this fft-int realname pretype (list type subtype)))
                     (for-each (lambda (arg)
                           (apply try (cons* realname function arg)))
                        (list '(1 (2 3)))))
                  (list    "c"      "s"       "i"     "q"           "f"       "d")
                  (list fft-char fft-short fft-int fft-long-long fft-float fft-double)) )
         (list    "c"      "s"       "i"     "q"           "f"       "d")
         (list fft-char fft-short fft-int fft-long-long fft-float fft-double)) )
   (list    "c"      "s"       "i"     "q"           "f"       "d")
   (list fft-char fft-short fft-int fft-long-long fft-float fft-double))

;; ;; ; ----------------------------------------------------------------
;; ;; ;(print "returning a structure test:")
;; ;; ;
;; ;; ;(define iiv2struct12 (this (cons type-bytevector 12) "iiv2struct12" fft-int fft-int fft-int))
;; ;; ;(try "iiv2struct12" iiv2struct12 1 2 123)
;; ;; ;(define iiv2struct20 (this (cons type-bytevector 20) "iiv2struct20" fft-int fft-int fft-int))
;; ;; ;(try "iiv2struct20" iiv2struct20 1 2 123)


; -----------------------------
; -=( fft-any )=---------------

; special print case - we hide "(fft* type)" from output
; because fft-long value are different for x32 and x64.
(define (try-a tag function . numbers)
   (for-each display (list "   " tag " " (map cdr numbers) ">"))
   (let ((out (apply function numbers)))
      (print " = " out)))
(define (try*-a tag function . numbers)
   (for-each display (list "   " tag " " (map cdr numbers) ">"))
   (let ((out (apply function numbers)))
      (print " = " out)))
(define (try&-a tag function . numbers)
   (for-each display (list "   " tag " " (map cdr numbers) ">"))
   (let ((out (apply function numbers)))
      (print " = " out ", " (cdar numbers))))

;; '(type . ...)
(print "
// qualified '(type . ...) for fft-any to type test:
type function(type arg)
{
    return arg;
}")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try-a name function (cons type 1))))
   `( ("c2c" . ,fft-unsigned-char)
      ("s2s" . ,fft-unsigned-short)
      ("i2i" . ,fft-unsigned-int)
      ("q2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try-a name function (cons type 1))
         (try-a name function (cons type -1))))
   `( ("C2C" . ,fft-signed-char)
      ("S2S" . ,fft-signed-short)
      ("I2I" . ,fft-signed-int)
      ("Q2Q" . ,fft-signed-long-long)))
; float
(define mirror (this fft-float "f2f" fft-any))
   (try-a "float" mirror (cons fft-float #i125.125))
   (try-a "float" mirror (cons fft-float #i-125.125))
; double
(define mirror (this fft-double "d2d" fft-any))
   (try-a "double" mirror (cons fft-double #i125.125))
   (try-a "double" mirror (cons fft-double #i-125.125))

;; '((fft* type) . ...)
(print "
// qualified '((fft* type) . ...) for fft-any to type test:
type function(type* arg)
{
    return *arg;
}")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try*-a name function (cons (fft* type) '(1)))))
   `( ("pc2c" . ,fft-unsigned-char)
      ("ps2s" . ,fft-unsigned-short)
      ("pi2i" . ,fft-unsigned-int)
      ("pq2q" . ,fft-unsigned-long-long)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try*-a name function (cons (fft* type) '(1)))
         (try*-a name function (cons (fft* type) '(-1)))))
   `( ("pC2C" . ,fft-signed-char)
      ("pS2S" . ,fft-signed-short)
      ("pI2I" . ,fft-signed-int)
      ("pQ2Q" . ,fft-signed-long-long)))
; float/double
(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try*-a name function (cons (fft& type) (map inexact '(125.125))))
         (try*-a name function (cons (fft& type) (map inexact '(-125.125))))))
   `( ("rf2f" . ,fft-float)
      ("rd2d" . ,fft-double)))

;; (fft& ...)
(print "
// qualified '((fft& type) . ...) for fft-any to type test:
type function(type* arg)
{
    type out = arg[2];
    arg[2] = arg[0] + arg[1];
    return out;
}")

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try&-a name function (cons (fft& type) (map copy '(1 2 42))))
         (try&-a name function (cons (fft& type) (vector-map copy [1 2 42])))
         (try&-a name function (cons (fft& type) (map copy '(125 7 42))))
         (try&-a name function (cons (fft& type) (vector-map copy [125 7 42])))
         (try&-a name function (cons (fft& type) (map copy '(7 125 42))))
         (try&-a name function (cons (fft& type) (vector-map copy [7 125 42])))))
   `( ("rpc2c3" . ,fft-unsigned-char)
      ("rps2s3" . ,fft-unsigned-short)
      ("rpi2i3" . ,fft-unsigned-int)
      ("rpq2q3" . ,fft-unsigned-long-long)
      ))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try&-a name function (cons (fft& type) (map copy '(1 2 42))))
         (try&-a name function (cons (fft& type) (vector-map copy [1 2 42])))
         (try&-a name function (cons (fft& type) (map copy '(125 7 42))))
         (try&-a name function (cons (fft& type) (vector-map copy [125 7 42])))
         (try&-a name function (cons (fft& type) (map copy '(7 125 42))))
         (try&-a name function (cons (fft& type) (vector-map copy [7 125 42])))))
   `( ("rpC2C3" . ,fft-unsigned-char)
      ("rpS2S3" . ,fft-unsigned-short)
      ("rpI2I3" . ,fft-unsigned-int)
      ("rpq2q3" . ,fft-unsigned-long-long)
      ))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type name fft-any)))
         (try&-a name function (cons (fft& type) (map inexact '(1 2 42))))
         (try&-a name function (cons (fft& type) (vector-map inexact [1 2 42])))
         (try&-a name function (cons (fft& type) (map inexact '(125.125 7 42))))
         (try&-a name function (cons (fft& type) (vector-map inexact [125.125 7 42])))
         (try&-a name function (cons (fft& type) (map inexact '(7 125.125 42))))
         (try&-a name function (cons (fft& type) (vector-map inexact [7 125.125 42])))
         (try&-a name function (cons (fft& type) (map inexact '(-125.125 -125.125 42))))
         (try&-a name function (cons (fft& type) (vector-map inexact [-125.125 -125.125 42])))
         (try&-a name function (cons (fft& type) (map inexact '(125.125 -125.125 42))))
         (try&-a name function (cons (fft& type) (vector-map inexact [125.125 -125.125 42])))))
   `( ("rpf2f3" . ,fft-float)
      ("rpd2d3" . ,fft-double)))

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

; ---------------------------------------------------------------
(define ffi:idf (make-vptr))
(define (cast x t) ; cast vptr to the type t
   (ffi ffi:idf (list t type-vptr) (list x)))

(for-each (lambda (x)
      (let*((name type (uncons x #f))
            (function (this type-vptr name)))
         (define ptr (function))
         (try name cast ptr (fft& type))))
   `( ("i8_1_ptr" . ,fft-int8)
      ("i16_1_ptr" . ,fft-int16)
      ("i32_1_ptr" . ,fft-int32)
      ("i64_1_ptr" . ,fft-int64)
      ("u8_1_ptr" . ,fft-uint8)
      ("u16_1_ptr" . ,fft-uint16)
      ("u32_1_ptr" . ,fft-uint32)
      ("u64_1_ptr" . ,fft-uint64)
      ("f32_1_ptr" . ,fft-float)
      ("f64_1_ptr" . ,fft-double)
   ))

;=============
(print "done.")
