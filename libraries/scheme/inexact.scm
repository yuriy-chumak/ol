(define-library (scheme inexact)
   (export 
      finite? infinite? nan?)
      ; exp log sin cos tan asin acos atan

   (import
      (scheme core))
(begin
   (define (nan-or-inf? z)
      (or (equal? z +inf.0)
          (equal? z -inf.0)
          (equal? z +nan.0)))


   (define (nan? z)
      (or
         (equal? z +nan.0)
         (when (eq? (type z) type-complex)
            (or (equal? (car z) +nan.0)
                (equal? (cdr z) +nan.0)))))

   (assert (nan? +nan.0)      ===> #true)
   (assert (nan? 32)          ===> #false)
   (assert (nan? +nan.0+5.0i) ===> #true)
   (assert (nan? 5.0++nan.0i) ===> #true)
   (assert (nan? 1+2i)        ===> #false)

   (define (infinite? z)
      (or
         (nan-or-inf? z)
         (when (eq? (type z) type-complex)
            (or (nan-or-inf? (car z))
                (nan-or-inf? (cdr z))))))

   (define (finite? z)
      (when (number? z)
         (not (infinite? z))))

))
