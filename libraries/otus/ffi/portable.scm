(define-library (otus ffi portable)
   (export
      (exports (otus ffi))
      ; updated functions:
      load-dynamic-library
      make-portable-entry)

      ;; ; Usage example:
      ;;
      ;; (import (otus ffi portable))
      ;; (define GLIBC (load-dynamic-library "libc.so.6"))
      ;; (define puts (GLIBC fft-int "puts" type-string))
      ;; 
      ;; (define (main args)
      ;;    (puts "Hello, World!\n"))
      ;; (fasl-save (make-portable-entry main) "bytecode")

   (import
      (otus lisp)
      (otus ffi)
      (otus async)
      (lang embed)
      (scheme dynamic-bindings))

(begin
   (actor 'functions (lambda ()
      (let loop ((functions '()))
         (let*((envelope (wait-mail))
               (sender msg envelope))
            (if msg
               (loop (cons msg functions))
            else
               (mail sender functions)
               (loop functions))))))

   (define (load-dynamic-library library)
      (let ((dll (dlopen library)))
         (if dll
            (lambda (type name . prototype)
               (let ((rtti (cons type prototype))
                     (function (dlsym dll name)))
                  (when function
                     (define fn1 (lambda args
                                    library name
                                    (ffi function rtti args)))
                     (mail 'functions fn1)
                     fn1))))))

   ; todo: collect used by main functions, not all
   (define (make-portable-entry entry)
      (define functions (await (mail 'functions #f)))
      ;(vm:new type-constructor (lambda (args)
      (make-entry (lambda (args)
         ; update `ffi`
         (vm:set! ffi (dlsym (dlopen) "OLVM_ffi"))
         ; update all functions
         (for-each (lambda (function)
               (define lib (ref function 2))
               (define name (ref function 3))
               (vm:set! (ref function 5) (dlsym (dlopen lib) name)))
            functions)

         ; finally, run entry
         (entry args))))
))
