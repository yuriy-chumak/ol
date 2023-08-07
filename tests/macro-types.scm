(print "legacy macro system:")
(define q 5)
(define-macro square (lambda (x) `(* ,x ,x)))
(define-macro define-q-square (lambda (x)
   `(define ,x ,(square q))))

(define q 3)
(define-q-square a)
(write a) (newline)

(define q 4)
(define-q-square a)
(write a) (newline)

(define-macro square (lambda (x) `(+ ,x ,x)))
(define-q-square a)
(write a) (newline)

; -----
(print "modern macro system (lazy-macro):")
(define-lazy-macro square (lambda (x) `(* ,x ,x)))
(define-lazy-macro define-q-square (lambda (x)
   `(define ,x ,(square q))))

(define q 3)
(define-q-square a)
(write a) (newline)

(define q 4)
(define-q-square a)
(write a) (newline)

(define-macro square (lambda (x) `(+ ,x ,x)))
(define-q-square a)
(write a) (newline)

; -----
(print "modern macro system (instant-macro):")
(define-instant-macro square (lambda (x) `(* ,x ,x)))
(define-instant-macro define-q-square (lambda (x)
   `(define ,x ,(square q))))

(define q 3)
(define-q-square a)
(write a) (newline)

(define q 4)
(define-q-square a)
(write a) (newline)

(define-macro square (lambda (x) `(+ ,x ,x)))
(define-q-square a)
(write a) (newline)
