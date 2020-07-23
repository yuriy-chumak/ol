; http://www.rosettacode.org/wiki/Call_a_foreign-language_function#Ol

(import (otus ffi))

(define self (load-dynamic-library #f))
(define strdup (or
   (self type-string "strdup" type-string)
   (lambda (str)
      (list->string (string->list str)))))

(define strduX (or
   (self type-string "strduX" type-string)
   (lambda (str)
      (list->string (string->list str)))))

(print (strdup "Hello World!"))
(print (strduX "Hello World!"))
