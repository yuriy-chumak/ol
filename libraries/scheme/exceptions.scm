(define-library (scheme exceptions)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme exceptions))
   (description
      "Otus-Lisp scheme exceptions library.")

   (export
      raise raise-continuable
      error-object?
      error-object-message     ; * macro
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

   ; equal to 'raise'
   (define raise-continuable raise)

   (define (error-object? obj)
      (and
         (vector? obj)
         (eq? (size obj) 4)
         (or (eq? (ref obj 1) 'error)
             (eq? (ref obj 1) 'crash))))

   (define-syntax error-object-message
      (syntax-rules (verbose-ol-error interaction-environment)
         ((error-object-message err)
            (vector-apply err (lambda (class code reason info)
               (verbose-ol-error
                  (interaction-environment) code reason info))))))

   ; error classes: 'error, 'crash (todo: rename 'crash to 'critical?)
   (define (with-exception-handler handler thunk)
      (define issue (await
         (actor-linked ['with-exception-handler] thunk)))
      (case issue
         ;; ok, no issues
         (['finished result]
            result)

         ; olvm critical errors (todo: rename to 'critical?)
         (['crash code a b]
            (handler issue))

         ; (runtime-error code info) or (raise info)
         (['error code reason info]
            (handler (if (eq? reason flag) info issue)))

         ; should not be happened
         (else is foo
            (runtime-error 'with-exception-handler foo))))

))
