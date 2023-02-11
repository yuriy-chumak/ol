(print "legacy macro system:")
(define-macro sq (lambda (x) `(* ,x ,x)))
(define-macro do (lambda (x) `(define ,x ,(sq q))))

(define q 3)
(do a)
(write a) (newline)

(define q 4)
(do a)
(write a) (newline)

(define-macro sq (lambda (x) `(+ ,x ,x)))
(do a)
(write a) (newline)

(print "modern macro system (lazy-macro):")
(define-lazy-macro (sq x) `(* ,x ,x))
(define-lazy-macro (do x) `(define ,x ,(sq q)))

(define q 3)
(do a)
(write a) (newline)

(define q 4)
(do a)
(write a) (newline)

(define-macro sq (lambda (x) `(+ ,x ,x)))
(do a)
(write a) (newline)

(print "modern macro system (fast-macro):")
(define-fast-macro (sq x) `(* ,x ,x))
(define-fast-macro (do x) `(define ,x ,(sq q)))

(define q 3)
(do a)
(write a) (newline)

(define q 4)
(do a)
(write a) (newline)

(define-macro sq (lambda (x) `(+ ,x ,x)))
(do a)
(write a) (newline)
