(define-library (scheme exceptions)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme exceptions))
   (description
      "Otus-Lisp scheme exceptions library.")

   (export
      raise raise-continuable
      with-exception-handler)

   (import
      (scheme core) (src vm)
      (otus async)
      (lang error))

(begin
   (setq flag ['exception])

   (define (raise obj)
      (call-with-current-continuation
         (lambda (resume)
            (vm:mcp resume 5 flag obj))))

   ; temporary is equal to 'raise'
   (define raise-continuable raise)

   ; 
   (define (with-exception-handler handler thunk)
      (define name ['with-exception-handler]) ; anonymous
      (actor-linked name thunk)

      (case (await name)
         ;; ok
         (['finished result]
            result)

         ; (VM::FAIL ...), vm pushed an error
         (['crash opcode a b]
            (handler (verbose-ol-error opcode a b)))

         ; (raise info)
         ; note, these could easily be made resumable by storing cont
         (['error code reason info]
            (handler
               (if (eq? reason flag)
                  info
                  (verbose-ol-error code reason info))))

         (else is foo
            (runtime-error "something wrong" foo))))

))
