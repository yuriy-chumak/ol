; http://www.rosettacode.org/wiki/Call_a_function_in_a_shared_library#Ol

(import (otus ffi))

(define self (load-dynamic-library #f))

(define strdup
   (let ((strdup (self type-vptr "strdup" type-string))
         (free (self fft-void "free" type-vptr)))
      (if (and strdup free)
         ; we have both native functions
         (lambda (str)
            (let*((dupped (strdup str))
                  (result (vptr->string dupped)))
               (free dupped)
               result))
         ; windows doesn't have strdup and free, so...
         (lambda (str)
            (list->string (string->list str))))))

(print (strdup "Hello World!"))
