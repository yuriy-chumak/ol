; http://www.rosettacode.org/wiki/Call_a_foreign-language_function#Ol

(import (otus ffi))

(define self (load-dynamic-library (if (has? *features* 'Windows) "shlwapi.dll" #f)))
(define strdup (self type-string (if (has? *features* 'Windows) "StrDupA" "strdup") type-string))

(print (strdup "Hello World!"))
