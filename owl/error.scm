(define-library (owl error)
   (export error)

   (import
      (owl defmac)
      (owl primop)
      (owl interop))

   (begin

      (define (error reason info)
         (interop 5 reason info))
))
