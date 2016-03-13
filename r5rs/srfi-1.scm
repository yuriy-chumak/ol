(define-library (r5rs srfi-1)

; http://srfi.schemers.org/srfi-1/srfi-1.html

; reference code at
;   https://github.com/scheme-requests-for-implementation/srfi-1/blob/master/srfi-1-reference.scm
(import
   (r5rs base)
   (owl math)
   (owl list))

   (export
      first second third ;fourth fifth sixth seventh eighth ninth tenth

      iota
)

(begin
   (define (first li)
      (car li))
   (define (second li)
      (car (cdr li)))
   (define (third li)
      (car (cdr (cdr li))))


   (define iota 
      (let ((iota (lambda (count start step)
         (let loop ((i 0) (n start) (r '()))
            (if (eq? i count)
               (reverse r)
               (receive (fx:+ i 1) (lambda (i overflow)
                  (loop i (+ n step) (cons n r)))))))))
      (case-lambda
         ((count)
            (iota count 0 1))
         ((count start)
            (iota count start 1))
         ((count start step)
            (iota count start step)))))
))
