(define-library (scheme lazy)
   (export
      delay force
      delay-force

      promise?
      make-promise)

   (import
      (scheme core)
      (otus case-apply))

(begin

   (define (make-promise obj)
      (λ () obj))

   ; delay <expression>  * (scheme core)
   ; force <expression>  * (scheme core)

   (define-syntax delay-force
      (syntax-rules ()
         ((delay-force . expression) (delay (force . expression)))))

   (define (promise? obj)
      (eq? (arity obj) 0))

   (define (make-promise obj)
      (lambda () obj))
   
))
