(define-library (lang error)
   (export
      typename
      verbose-ol-error)

   (import
      (scheme core)
      (src vm) (owl ff)
      (lang primop))

(begin
   (setq expecting-2-3-arguments "expecting 2-3 arguments, but got")

   (define typename {
      type-pair 'type-pair
      type-vector 'type-vector
      type-string 'type-string
      type-string-wide 'type-string-wide
      type-string-dispatch 'type-string-dispatch
      type-symbol 'type-symbol
      type-port 'type-port
      type-const 'type-const
      type-bytecode 'type-bytecode
      type-procedure 'type-procedure
      type-closure 'type-closure
      type-bytevector 'type-bytevector
      type-constructor 'type-constructor
      type-thread-state 'type-thread
      type-vptr 'type-vptr

      type-enum+ 'type-enum+
      type-enum- 'type-enum-
      type-int+ 'type-int+
      type-int- 'type-int-
      type-rational 'type-rational
      type-complex 'type-complex
      type-inexact 'type-inexact
   })

   (define (enum? x)
      (let ((x-type (type x)))
         (or
            (eq? x-type type-enum+)
            (eq? x-type type-enum-) )))

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
               (22 ; (vm:cast ...)
                  `(invalid cast to ,(typename b (cons 'type b)) for ,a))
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

               ; invalid vector indexer
               (262
                  (if (enum? b)
                     `("vector index out of range:" ,b) ; todo?: print vector sizes
                     `(,b "cannot be a vector index")))

               ; ------------------------------------------------------------
               ; syscall errors:
               (62000
                  `(too ,(if (less? a b) 'few 'many) arguments to syscall))

               (62001 ; port
                  `(syscall argument ,a is not a port))
               (62002 ; number
                  `(syscall argument ,a is not a number))
               (62003 ; positive number
                  `(syscall argument ,a is not a positive number))
               (62004 ; reference
                  `(syscall argument ,a is not a reference))
               (62005 ; rawstream
                  `(syscall argument ,a is not a binary sequence))
               (62006 ; string
                  `(syscall argument ,a is not a string))
               (62007 ; vptr
                  `(syscall argument ,a is not a vptr))
               (62008 ; #t/#f
                  `(syscall argument ,a is not a boolean))

               (62020
                  `(syscall argument ,a is not a number or #false))
               (62030
                  `(syscall argument ,a is not a positive number or #false))
               (62016
                  `(syscall argument ,a is not a port or string))
               (62076
                  `(syscall argument ,a is not a vptr or string))
               (62026
                  `(syscall argument ,a is not a number or string))
               (62060
                  `(syscall argument ,a is not a string or #false))


               (else
                  (if (less? code 256)
                     `(,(primop-name code) reported error ": " ,a " " ,b)
                     `(,code " .. " ,a " " ,b)))))))
))
