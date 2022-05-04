(import (otus ffi portable))

(define GLIBC (load-dynamic-library "libc.so.6"))
(define puts (GLIBC fft-int "puts" type-string))

; main function
(define (main args)
   (puts "Hello, World!\n"))

; let's compile
(fasl-save (make-portable-entry main) "bytecode")
