(define-library (owl error)
   (export error)

   (import
      ;(owl r5rs) or
      ;(owl scheme) or
      ;(owl language)
      (owl defmac)
      (owl primop))

   (begin

      (define (error reason info)
         (call/cc (λ (resume) (sys resume 5 reason info))))
))
