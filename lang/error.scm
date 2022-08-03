(define-library (lang error)
   (export
      verbose-ol-error)

   (import
      (scheme core)
      (src vm)
      (lang primop))

(begin
   (setq expecting-2-3-arguments "expecting 2-3 arguments, but got")

   (define (verbose-ol-error code a b)
      (if (eq? (type code) type-closure) ; continuation?
         (list a b)
      else
         (list "error" code "->"
            (case code
               (0
                  `(unsupported vm code ,a))
               (20 ; (apply ...)
                  (if (eq? a 0)
                     '(empty apply)
                     '(too large apply)))
               (50 ; (vm:run ...)
                  '(invalid vm:run state))
               (ARITY-ERROR
                  `(,a did not accept ,(-- b) arguments))

               ((23 18 82) ; VMNEW, VMMAKE, VMALLOC
                  `(memory allocation error))

               (52 ; (car not-a-pair)
                  `(trying car of a non-pair ,a))
               (53 ; (cdr not-a-pair)
                  `(trying cdr of a non-pair ,a))

               (259 ; ff direct call
                  `(,expecting-2-3-arguments ,b))
               (260 ; (ff not-existent-key)
                  `(key ,b not found in ,a))

               ((261 258) ; (not-an-executable-object)
                  `(illegal invocation of ,a))

               ; ------------------------------------------------------------
               ; syscall errors:
               (62000
                  `(too ,(if (less? a b) 'few 'many) arguments to syscall))

               (62001
                  `(syscall argument ,a is not a port))
               (62002
                  `(syscall argument ,a is not a number))
               (62003
                  `(syscall argument ,a is not a reference))
               (62004
                  `(syscall argument ,a is not a binary sequence))
               (62005
                  `(syscall argument ,a is not a string))
               (62006
                  `(syscall argument ,a is not a string or port))
               (62006
                  `(syscall argument ,a is not a positive number))


               ;; (62000 ; syscall number is not a number
               ;;    `(syscall "> " ,a is not a number))
               ;; ;; 0, read
               ;; (62001 ; too few/many arguments given to
               ;;    `(syscall "> " too ,(if (> a b) 'many 'few) arguments given to))
               ;; (62002 ;
               ;;    `(syscall "> " ,a is not a port))
               (else
                  (if (less? code 256)
                     `(,(primop-name code) reported error ": " ,a " " ,b)
                     `(,code " .. " ,a " " ,b)))))))
   ;   ;((eq? opcode 52)
   ;   ;   `(trying to get car of a non-pair ,a))
   ;   (else
   ;      `("error: instruction" ,(primop-name opcode) "reported error: " ,a " " ,b)))
))
