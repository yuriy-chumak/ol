(define-library (scheme process-context)
;; An interface to the system environment, command
;; line, and process exit status is available in the (scheme
;; process-context) library.

   (export
      command-line
      exit
      emergency-exit
      get-environment-variable
      get-environment-variables
      set-environment-variable!
      unset-environment-variable)

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
            (die code))
         (()
            (die 0))))

      (define emergency-exit (case-lambda
         ((code)
            (vm:exit code))
         (()
            (vm:exit 0))))

      (define (get-environment-variable name)
         (let ((name (c-string name)))
            (if name
               (syscall 1016 name))))

      (define (get-environment-variables)
         (syscall 1015))

      (define set-environment-variable! (case-lambda
         ((name value)
            (syscall 1014 name value))
         ((name value overwrite)
            (syscall 1014 name value overwrite))))

      (define (unset-environment-variable name)
         (syscall 1014 name))

))
