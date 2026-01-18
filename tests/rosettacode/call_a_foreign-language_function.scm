; http://www.rosettacode.org/wiki/Call_a_foreign-language_function#Ol

(import (otus ffi))

(define libc (or
   (load-dynamic-library "libc.so") ; General Posix
   (load-dynamic-library "libc.so.6") ; Linux
   (load-dynamic-library "libc.so.7") ; Latest *BSD
   (load-dynamic-library "libSystem.B.dylib") ; Mac
   (load-dynamic-library "shlwapi.dll") )) ; Windows

(define strdup (or
   (libc type-string "strdup" type-string) ; Posix
   (libc type-string "StrDupA" type-string) )) ; Win

(print (strdup "Hello World!"))

; strdup with free
(define lib2
   (load-dynamic-library "kernel32.dll")) ; Windows

(define strdup 
   (let ((strdup (or
            (libc type-vptr "strdup" type-string) ; Posix
            (libc type-vptr "StrDupA" type-string))) ; Win
         (free (or 
            (libc fft-void "free" type-vptr) ; Posix
            (lib2 fft-void "LocalFree" type-vptr)))) ; Win
      (lambda (str)
         (let*((dupped (strdup str))
               (result (vptr->string dupped)))
            (free dupped)
            result))))

(print (strdup "Hello World!"))
