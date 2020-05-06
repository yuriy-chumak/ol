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

   (define (string->number str base)
      (list->number (string->list str) base))

   (define (string->integer str)
      (let ((n (string->number str 10)))
         (cond
            ((eq? (type n) type-enum+) n)
            ((eq? (type n) type-enum-) n)
            ((eq? (type n) type-int+) n)
            ((eq? (type n) type-int-) n)
            (else #false))))

))
