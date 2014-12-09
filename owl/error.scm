(define-library (owl error)
   (export error)

   (import
      ;(owl r5rs) or
      ;(owl scheme) or
      ;(owl language)
      (owl defmac)
      (owl primop))

   (begin

      (define (interop op a b)
         (call/cc (λ (resume) (sys resume op a b))))

      (define (error reason info)
         (interop 5 reason info))
))
