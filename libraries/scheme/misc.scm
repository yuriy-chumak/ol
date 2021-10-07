(define-library (scheme misc)
   (export
      string->integer
      string->number)

   (import
      (scheme core)
      (owl list)
      (owl string)
      (only (lang sexp) list->number)
      (owl math))

(begin

   (define string->number (case-lambda
      ((str base)
         (list->number (string->list str) base))
      ((str)
         (list->number (string->list str) 10))))

   (define (string->integer str)
      (let ((n (string->number str 10)))
         (and (integer? n) n)))

))
