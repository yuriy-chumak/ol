(define-library (scheme process-context)
;; An interface to the system environment, command
;; line, and process exit status is available in the (scheme
;; process-context) library.

   (export
      command-line
      exit
      emergency-exit
      get-environment-variable
      get-environment-variables)

   (import
      (scheme core)
      (otus async)
      (owl string))

   (begin
      (define-syntax command-line
         (syntax-rules (*command-line*)
            ((command-line)
               *command-line*)))

      (define exit (case-lambda
         ((code)
            (shutdown (case code
               (#true 0)
               (#false -1)
               (else code))))
         (()
            (shutdown 0))))
      
      (define emergency-exit (case-lambda
         ((code)
            (vm:exit (case code
               (#true 0)
               (#false -1)
               (else code))))
         (()
            (vm:exit 0))))

      (define (get-environment-variable name)
         (let ((name (c-string name)))
            (if name
               (syscall 1016 name))))

      (define (get-environment-variables)
         (syscall 1015))
))