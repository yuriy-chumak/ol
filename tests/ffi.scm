(import (otus ffi))

; testing the ffi:
; f: floats
; i: ints
; d: doubles
; result of function should be sum of all arguments

(define (assert text result value)
   (if (< (/ (abs (- result value)) (abs value)) 0.0001)
      (print text " = " "ok.")
      (print text " = " result " instead of " value)))

(define i_i (dlsym (dlopen) type-int+ "i_i" type-int+))
(define f_f (dlsym (dlopen) type-float "f_f" type-float))
(define d_d (dlsym (dlopen) type-double "d_d" type-double))

(define fi (dlsym (dlopen) type-float "fi" type-float type-int+))
(define fii (dlsym (dlopen) type-float "fii" type-float type-int+ type-int+))
(define fiiii (dlsym (dlopen) type-float "fiiii" type-float type-int+ type-int+ type-int+ type-int+))
(define ifiii (dlsym (dlopen) type-float "ifiii" type-int+ type-float type-int+ type-int+ type-int+))
(define iiiif (dlsym (dlopen) type-float "iiiif" type-int+ type-int+ type-int+ type-int+ type-float))
(define fiiif (dlsym (dlopen) type-float "fiiif" type-float type-int+ type-int+ type-int+ type-float))


(assert "i_i" (i_i 1)   1)
(assert "f_f" (f_f 1.1) 1.1)
(assert "d_d" (d_d 2.2) 2.2)


(assert "fi" (fi 1.1 2)     (+ 1.1 2))
(assert "fii" (fii 1.1 2 3) (+ 1.1 2 3))

(define fiiii (dlsym (dlopen) type-float "fiiii" type-float type-int+ type-int+ type-int+ type-int+))
(define ifiii (dlsym (dlopen) type-float "ifiii" type-int+ type-float type-int+ type-int+ type-int+))
(define iiiif (dlsym (dlopen) type-float "iiiif" type-int+ type-int+ type-int+ type-int+ type-float))
(define fiiif (dlsym (dlopen) type-float "fiiif" type-float type-int+ type-int+ type-int+ type-float))

(assert "fiiii" (fiiii 1.1 2 3 4 5)   (+ 1.1 2 3 4 5))
(assert "ifiii" (ifiii 1 2.2 3 4 5)   (+ 1 2.2 3 4 5))
(assert "iiiif" (iiiif 1 2 3 4 5.5)   (+ 1 2 3 4 5.5))
(assert "fiiif" (fiiif 1.1 2 3 4 5.5) (+ 1.1 2 3 4 5.5))
(assert "fiiif" (fiiif 1.1 2 3 4 -1.1)(+ 1.1 2 3 4 -1.1))


(define iffiiiifiiffffff (dlsym (dlopen) type-float "iffiiiifiiffffff"
   type-int+
   type-float type-float 
   type-int+ type-int+ type-int+ type-int+
   type-float
   type-int+ type-int+
   type-float type-float type-float type-float type-float type-float))

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
