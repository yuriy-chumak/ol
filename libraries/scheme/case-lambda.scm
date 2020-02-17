(define-library (scheme case-lambda)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme case-lambda srfi-16))
   (description
      "r7rs case-lambda library.")
(export
   case-lambda)

(begin
   ; makes a list of options to be compiled to a chain of code bodies w/ jumps
   ; note, could also merge to a jump table + sequence of codes
   (define-syntax case-lambda
      (syntax-rules (arity-error)
         ((case-lambda)
            (lambda () arity-error))
         ((case-lambda (formals . body))
            (lambda formals . body))
         ((case-lambda (formals . body) . rest)
            (either (lambda formals . body)
               (case-lambda . rest)))))
))
