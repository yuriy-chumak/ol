(define-library (r5rs srfi-1)

; http://srfi.schemers.org/srfi-1/srfi-1.html

; reference code at
;   https://github.com/scheme-requests-for-implementation/srfi-1/blob/master/srfi-1-reference.scm
(import
   (r5rs core)
   (owl math)
   (owl list))

   (export
      srfi-1
      first second third fourth fifth sixth seventh eighth ninth tenth

      iota
)

(begin
   (define srfi-1 #true)

   (define (first li)
      (car li))
   (define (second li)
      (car (cdr li)))
   (define (third li)
      (car (cdr (cdr li))))
   (define (fourth li)
      (car (cdr (cdr (cdr li)))))
   (define (fifth li)
      (car (cdr (cdr (cdr (cdr li))))))
   (define (sixth li)
      (car (cdr (cdr (cdr (cdr (cdr li)))))))
   (define (seventh li)
      (car (cdr (cdr (cdr (cdr (cdr (cdr li))))))))
   (define (eighth li)
      (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr li)))))))))
   (define (ninth li)
      (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr li))))))))))
   (define (tenth li)
      (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr li)))))))))))


   (define iota
      (let ((iota (lambda (count start step)
         (let loop ((i 0) (n start) (r '()))
            (if (eq? i count)
               (reverse r)
               (apply-values (vm:add i 1) (lambda (i carry)
                  (loop i (+ n step) (cons n r)))))))))
      (case-lambda
         ((count)
            (iota count 0 1))
         ((count start)
            (iota count start 1))
         ((count start step)
            (iota count start step)))))
))
