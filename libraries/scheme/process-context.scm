(define-library (scheme process-context)
   (export 
      command-line
      exit
      emergency-exit
      get-environment-variable
      get-environment-variables
      )

   (import
      (scheme core))

   (begin
      (define (command-line)
         *vm-args*)

      (define exit (case-lambda
         ((code)
            (vm:exit (case code
               (#true 0)
               (#false -1)
               (else code))))
         (()
            (vm:exit 0))))
      
      (define emergency-exit exit)

      (define (get-environment-variable name)
         (let ((name (c-string name)))
            (if name
               (syscall 1016 name))))

      (define (get-environment-variables)
         (runtime-error 'get-environment-variables #f))
))