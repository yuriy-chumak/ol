(define-library (file parser)

(export
   (exports (owl parse))

   uint16
   uint32
;  string
   times
   float)

(import
   (otus lisp)
   (owl parse))
(begin

   (define uint16
      (let-parse* (
            (a0 byte)
            (a1 byte))
         (+ a0
            (<< a1 8))))
   (define uint32
      (let-parse* (
            (a0 byte)
            (a1 byte)
            (a2 byte)
            (a3 byte))
         (+     a0
            (<< a1  8)
            (<< a2 16)
            (<< a3 24))))

   ;; (define (times n parser)
   ;;    (lambda (l r pos ok)
   ;;       (let loop ((n n) (val '()) (left l) (right r) (position pos))
   ;;          (print "loop: " n " " val " " left " " right " " position)
   ;;          (if (eq? n 0)
   ;;             (begin
   ;;                (print "DONE")
   ;;                (ok left right position val)
   ;;             )
   ;;             (cond
   ;;                ((null? r)
   ;;                   (backtrack left right position "end of file"))
   ;;                ((pair? r)
   ;;                   (parser left right position
   ;;                      (lambda (ll rr p v)
   ;;                         (print "ok")
   ;;                         (loop (- n 1) (cons v val) (cons (car rr) ll) (cdr rr) (+ p 1))))
   ;;                   (backtrack l r pos "required more steps"))
   ;;                (else
   ;;                   (print "else")
   ;;                   (loop n val left (right) position)))))))

      ;; (lambda (lst ok fail pos)
      ;;    (let loop ((ll lst) (rvals #null) (pos pos) (n n))
      ;;       (if (eq? n 0)
      ;;          (ok ll fail (reverse rvals) pos)
      ;;          (cond
      ;;             ((null? ll) (fail pos "end of input")) ; will always be the largest value
      ;;             ((pair? ll)
      ;;                (parser ll
      ;;                   ;ok
      ;;                   (lambda (ll fail output pos)
      ;;                      (loop ll (cons output rvals) pos (- n 1)))
      ;;                   ;fail
      ;;                   fail
      ;;                   pos))
      ;;             (else
      ;;                (loop (ll) rvals pos n)))))))

   (define string
      (let-parse* (
            (len uint16)
            (value (times len byte)))
         (bytes->string value)))

   (define float ; 4-bytes float -> inexact
      (let-parse* (
            (a0 byte)
            (a1 byte)
            (a2 byte)
            (a3 byte))
         (vm:cast (bytevector a0 a1 a2 a3) type-inexact)))

))
