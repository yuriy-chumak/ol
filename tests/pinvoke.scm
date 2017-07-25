(import (otus pinvoke))

; testing the pinvoke:
; f: floats
; i: ints
; d: doubles
; result of function should be sum of all arguments


(define i_i (dlsym (dlopen) type-int+ "i_i" type-int+))
(define f_f (dlsym (dlopen) type-float "f_f" type-float))
(define d_d (dlsym (dlopen) type-double "d_d" type-double))


(print "i_i = " (i_i 1))
(print "f_f = " (f_f 1.1))
(print "d_d = " (d_d 1.1))


,quit

(define fii (dlsym (dlopen) type-float "fii" type-float type-int+ type-int+))
(print "fii = " (fii 1.1 2 3))
(define fi (dlsym (dlopen) type-float "fi" type-float type-int+))
(print "fi = " (fi 1.1 2))

(print "> " (- (fi 1.1 2) (fii 1.1 2 3)))

(define fiiii (dlsym (dlopen) type-float "fiiii" type-float type-int+ type-int+ type-int+ type-int+))
(print "fiiii = " (fiiii 1.1 2 3 4 5))
(define ifiii (dlsym (dlopen) type-float "ifiii" type-int+ type-float type-int+ type-int+ type-int+))
(print "ifiii = " (ifiii 1 2.2 3 4 5))
(define iiiif (dlsym (dlopen) type-float "iiiif" type-int+ type-int+ type-int+ type-int+ type-float))
(print "iiiif = " (iiiif 1 2 3 4 5.5))
(define fiiif (dlsym (dlopen) type-float "fiiif" type-float type-int+ type-int+ type-int+ type-float))
(print "fiiif = " (fiiif 1.1 2 3 4 5.5))
(print "fiiif = " (fiiif 1.1 2 3 4 -1.1))


;(define test4 (dlsym (dlopen) type-int+ "test4" type-int+ type-int+ type-int+ type-int+))
;(define test5 (dlsym (dlopen) type-int+ "test5" type-int+ type-int+ type-int+ type-int+ type-int+))
;(define test6 (dlsym (dlopen) type-int+ "test6" type-int+ type-int+ type-int+ type-int+ type-int+ type-int+))

;(print (test6 1 2 3 4 5 6))
;(print (test4 1 2 3 4))
;(print (test5 1 2 3 4 5))
;(print "test5 = " (test5 1 2 3 4 5))
,quit