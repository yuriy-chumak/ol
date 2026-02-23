(define-library (scheme read)
(export
   read
   )

(import
   (scheme core)
   (owl io)
   (only (data s-exp) sexp))

(begin

   ; * internal function
   (define (sexp-reader port)
      (let* ((l r p val ((sexp) #null
                           (unbuffered-input-stream port) ; (owl io)
                           0 ; not used, any value
                           (λ (l r p v) ; ok
                              (values l r p v)))))
         (when l val)))

   ; read is not interning symbols
   (define read (case-lambda
      ((port)
         (sexp-reader port))
      (()
         (sexp-reader stdin))))

))
