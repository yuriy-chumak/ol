(define-library (scheme read)
(export
   read
   )

(import
   (scheme core)
   (only (lang sexp) sexp)
   (owl io)
   (owl io scheduler)
   (owl parse)
   (owl string)
   (otus async))

(begin

   ; * internal function
   (define (sexp-reader port)
      (let* ((l r p val ((sexp) #null
                           (unbuffered-input-stream port) ; (owl io)
                           0 ; not used, any value
                           (λ (l r p v) ; ok
                              (values l r p v)))))
         (when l val)))

   (define read (case-lambda
      ((port)
         (sexp-reader port))
      (()
         (sexp-reader stdin))))

))
