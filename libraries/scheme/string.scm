(define-library (scheme string)
   (version 1.1)
   (license MIT/LGPL3)
   (keywords (otus ol scheme string))
   (description "
      Scheme string library.")

   (export
      string-map
   )

   (import
      (scheme core)
      (scheme list)
      (owl string))

(begin

   (define (string-map f . strings)
      (list->string
         (apply map (cons f (map string->list strings)))))

   (assert (string-map (lambda (x) (++ x)) "string") ===> "tusjoh")

))
