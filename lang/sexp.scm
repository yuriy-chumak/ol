(define-library (lang sexp)
   (export
      (exports (data s-exp)))

   (import
      (scheme core)
      (owl io)
      (data s-exp))

(begin
   (print-to stderr "(lang sexp) is deprecated and will be removed. use (data s-exp) instead.")
))
