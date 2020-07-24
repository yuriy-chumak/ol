; http://www.rosettacode.org/wiki/Call_a_foreign-language_function#Ol

(import (otus ffi))

(define self (load-dynamic-library #f))
(define strdup (or
   (let ((function (self type-string "strdup" type-string)))
      (if function (lambda (str) (function (c-string str)))))
   (lambda (str)
      (list->string (string->list str)))))

(define strduX (or
   (let ((function (self type-string "strduX" type-string)))
      (if function (lambda (str) (function (c-string str)))))
   (lambda (str)
      (list->string (string->list str)))))

(print (strdup "Hello World!"))
(print (strduX "Hello World!"))
