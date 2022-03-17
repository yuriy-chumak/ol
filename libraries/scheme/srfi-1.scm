(define-library (scheme srfi-1)

; http://srfi.schemers.org/srfi-1/srfi-1.html

; reference code at
;   https://github.com/scheme-requests-for-implementation/srfi-1/blob/master/srfi-1-reference.scm
(import
   (scheme core)
   (owl math)
   (owl list))

   (export
      ; Constructors
      cons list
      xcons iota

      first second third fourth fifth sixth seventh eighth ninth tenth

      filter
)

(begin
   ; cons a d -> pair   *[r5rs]
   (assert (cons 'a '())        ===> '(a))
   (assert (cons '(a) '(b c d)) ===> '((a) b c d))
   (assert (cons "a" '(b c))    ===> '("a" b c))
   (assert (cons 'a 3)          ===> '(a . 3))
   (assert (cons '(a b) 'c)     ===> '((a b) . c))

   ; list object ... -> list   *[r5rs]
   (assert (list 'a (+ 3 4) 'c) ===>  '(a 7 c))
   (assert (list)               ===>  '())

   ; xcons d a -> pair
   ;;; Occasionally useful as a value to be passed to a fold or other
   ;;; higher-order procedure.
   (define (xcons d a) (cons a d))


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
      (define (iota count start step)
         (let loop ((i 0) (n start) (r '()))
            (if (eq? i count)
               (reverse r)
               (values-apply (vm:add i 1) (lambda (i carry)
                  (loop i (+ n step) (cons n r)))))))

      (case-lambda
         ((count)
            (iota count 0 1))
         ((count start)
            (iota count start 1))
         ((count start step)
            (iota count start step))))

   ;; filter pred list -> list
   ; Return all the elements of list that satisfy predicate pred.
   ; The list is not disordered -- elements that appear in the result
   ; list occur in the same order as they occur in the argument list.
   ; The returned list may share a common tail with the argument list.
   ; The dynamic order in which the various applications of pred are
   ; made is not specified.
   ; (filter even? '(0 7 8 8 43 -4)) => (0 8 8 -4)

   (define (filter p l)
      (foldr (lambda (x tl) (if (p x) (cons x tl) tl)) null l))
   ;   (let loop ((l l) (o '()))
   ;      (if (null? l)
   ;         (reverse o)
   ;         (let ((e (car l)))
   ;            (loop (cdr l)
   ;                  (if (p e)
   ;                     (cons e o) o))))))

))
