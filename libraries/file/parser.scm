(define-library (file parser)

(export
   (exports (owl parse))

   uint16
   uint32
;  string
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
