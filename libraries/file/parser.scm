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

(define (times n parser)
   (lambda (l r p ok)
      (let loop ((l l) (r r) (p p) (n n) (v #null))
         (cond
            ((null? r) (backtrack l r p "end-of-file"))
            ((pair? r)
               (let* ((l r p val (parser l r p (lambda (l r p v) (values l r p v)))))
                  (if (eq? n 1)
                     (ok l r p (reverse (cons val v)))
                     (loop l r p (- n 1) (cons val v)))))
            (else
               (loop l (r) p n v))))))

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
