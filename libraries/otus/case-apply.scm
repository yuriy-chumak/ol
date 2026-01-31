(define-library (otus case-apply)
   (export
      arity
      case-apply)
   (import
      (src vm)
      (scheme core))
(begin

   (define (get-arity func)
      (case (type func)
         (type-bytecode
            (case (ref func 0)
               (11 ; BNA
                  (-- (ref func 1)))
               (12 ; BNAV (indefinite arity)
                  -1)
               (else
                  #false)))
         (type-procedure
            (get-arity (ref func 1)))
         (type-closure
            (get-arity (ref func 1)))
         (else
            #false)))
   (define arity get-arity)

   (define (case-apply f . args)
      (define arity (get-arity f))
      (if arity
         (let loop ((args args))
            (unless (null? args)
               (define arg (car args))
               (if (and (pair? arg) (eq? arity (car arg)))
                  (apply f (cdr arg))
                  (loop (cdr args)))))))

))
