(define-library (otus embed)
   (export
      make-entry
      return)

   (import
      (scheme base)
      (otus threading))

(begin
   (define make-entry make-entry)
   (define return vm:exit)
))