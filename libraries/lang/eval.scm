(define-library (lang eval)
   (export
      (exports (otus eval)))

   (import
      (scheme core)
      (owl io)
      (otus eval))

(begin
   (print-to stderr "(lang eval) is deprecated and will be removed. use (otus eval) instead.")

))
