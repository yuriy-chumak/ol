; http://www.rosettacode.org/wiki/Call_a_foreign-language_function#Ol

(import (otus ffi))

(define self (load-dynamic-library #f))
(define strdup (self type-string "strdup" type-string))

(print (strdup (c-string "Hello World!")))
