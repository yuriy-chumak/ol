(define-library (file parser)

(export
   (exports (owl parse))
   get-uint16
   get-uint32
   get-string
   get-n-times
   get-float)

(import
   (otus lisp)
   (owl parse))
(begin

   (define get-uint16
      (let-parses((a0 get-byte)
                  (a1 get-byte))
         (+ a0
            (<< a1 8))))
   (define get-uint32
      (let-parses((a0 get-byte)
                  (a1 get-byte)
                  (a2 get-byte)
                  (a3 get-byte))
         (+ a0
            (<< a1 8)
            (<< a2 16)
            (<< a3 24))))

   (define (get-n-times n parser)
      (lambda (lst ok fail pos)
         (let loop ((ll lst) (rvals #null) (pos pos) (n n))
            (if (eq? n 0)
               (ok ll fail (reverse rvals) pos)
               (cond
                  ((null? ll) (fail pos "end of input")) ; will always be the largest value
                  ((pair? ll)
                     (parser ll
                        ;ok
                        (lambda (ll fail output pos)
                           (loop ll (cons output rvals) pos (- n 1)))
                        ;fail
                        fail
                        pos))
                  (else
                     (loop (ll) rvals pos n)))))))

   (define get-string
      (let-parses (
            (len get-uint16)
            (value (get-n-times len get-byte)))
         (bytes->string value)))

   (define get-float ; 4-bytes float -> inexact
      (let-parses((a0 get-byte)
                  (a1 get-byte)
                  (a2 get-byte)
                  (a3 get-byte))
         (vm:cast (bytevector a0 a1 a2 a3) type-inexact)))

))
