(define-library (scheme lazy)
   (export
      make-promise)

   (import
      (scheme core))

(begin

   (define (make-promise obj)
      (λ () obj))
))
