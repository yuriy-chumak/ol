(define-library (lang embed)
   (export
      make-entry)

   (import
      (scheme base)
      (owl ff) (owl list)
      (scheme vector)
      (owl io) (owl math)
      (otus symbols)
      (lang assemble)
      (otus threading))

(begin
   (print-to stderr "(lang embed) is deprecated and will be removed. use (otus embed) instead.")

   (define make-entry make-entry)
))