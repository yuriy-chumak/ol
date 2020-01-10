(define-library (scheme inexact)
   (export 
      finite? infinite? nan?)
      ; exp log sin cos tan asin acos atan

   (import
      (scheme core))
(begin

   (define (finite? z)
      (runtime-error "No finite? is implemented." #null))

   (define (infinite? z)
      (runtime-error "No finite? is implemented." #null))

   (define (nan? z)
      (or
         (equal? z +nan.0)
         (when (eq? (type z) type-complex)
            (or (equal? (car z) +nan.0)
                  (equal? (car z) +nan.0)))))

))
