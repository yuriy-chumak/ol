(define-library (otus 2.6)
   (export
      halt)

   (import
      (scheme base)
      (otus async))

(begin
   (define halt die)

))
