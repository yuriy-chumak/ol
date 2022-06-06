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
      (otus async)
      (only (lang eval) verbose-ol-error))

(begin
   (define (with-exception-handler handler thunk)
      (define name ['with-exception-handler]) ; anonymous
      (actor-linked name thunk)

      (case (await name)
         ;; ok
         (['finished result]
            result)

         ; (VM::FAIL ...), vm pushed an error
         (['crashed opcode a b]
            (handler (verbose-ol-error opcode a b)))

         ; (raise info)
         ; note, these could easily be made resumable by storing cont
         (['error code reason info]
            (handler
               (if (eq? reason 'runtime-error)
                  info
                  (handler (verbose-ol-error code reason info)))))

         (else is foo
            (runtime-error "something wrong" foo))))

))
