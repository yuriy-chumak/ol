(define-library (owl error)
   (export error)

   (import
      (r5rs base)
      (owl primop)
      (owl interop))

   (begin

      (define (error reason info)
         (interop 5 reason info))
))
