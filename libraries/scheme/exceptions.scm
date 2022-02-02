(define-library (scheme exceptions)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme exceptions))
   (description
      "Otus-Lisp scheme exceptions library.")

   (export
      with-exception-handler)

   (import
      (scheme core)
      (otus async))

(begin
   (define (with-exception-handler handler thunk)
      (define name ['with-exception-handler]) ; anonymous
      (coroutine-linked name thunk)

      (case (await name)
         ;; ok
         (['finished result]
            result)

         ; (VM::FAIL ...), vm pushed an error
         (['crashed opcode a b]
            (runtime-error "vm exception"
               (list opcode a b)))

         ; (raise info)
         ; note, these could easily be made resumable by storing cont
         (['error cont reason info]
            (handler info))

         (else is foo
            (runtime-error "something wrong" foo))))

))
