(define-library (lang error)
   (export
      typename
      error-description)

   (import
      (scheme core)
      (scheme list)
      (src vm) (owl ff)
      (owl math)
      (lang primop))

(begin
   (setq expecting-2-3-arguments "expecting 2-3 arguments, but got")

   (define typename {
      type-pair 'type-pair
      type-vector 'type-vector
      type-string 'type-string
      type-string-wide 'type-string-wide
      type-superstring 'type-superstring
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

      type-value+ 'type-value+
      type-value- 'type-value-
      type-integer+ 'type-integer+
      type-integer- 'type-integer-
      type-rational 'type-rational
      type-complex 'type-complex
      type-inexact 'type-inexact
   })

   (define (enum? x)
      (let ((x-type (type x)))
         (or
            (eq? x-type type-value+)
            (eq? x-type type-value-) )))

; todo: collect all names of similar function
(define (procedure->name env a)
   (vm:cast
      (call/cc (lambda (return)
         (define (walk lib env)
            (ff-for-each (lambda (key value)
                  (define procedure (ref (ref value 2) 2))
                  (if (eq? procedure a) ; #(defined #(value #function))
                     ; todo: add synonims search
                     (return (if lib [key 'from lib 'library] [key]))))
               env))

         ; search for toplevel symbol
         (walk #f env)
         ; then search all libraries.
         (for-each (lambda (lib)
               (if (ff? (cdr lib))
                  (walk (car lib) (cdr lib))))
            (ref (ref (env '*libraries* [#f [#f ()]]) 2) 2))
         ; no procedure name found
         ['lambda]))
      9))

   (define (op-name env op)
      (case (type op)
         (type-value+
            (if (less? op 256)
               (vm:cast [(primop-name op)] 9)
            else
               (vm:cast [op] 9)))
         (type-symbol
            (vm:cast [op] 9))
         (type-vector
            (vm:cast op 9))
         (type-pair ; sexp
            (vm:cast op 9))
         ;;    ['vector]) ; todo: procedure->name or vector (if no name)
         ;; ((object? op)
         ;;    ['object]) ; todo: procedure->name or object (if no name)
         (else
            (procedure->name env op)) ))

   (define (but x)
      ; (string-append (number->string x) ", but"))
      (vm:alloc type-string
         (format-number x (list #\, #\space #\b #\u #\t) 10)))

   ; a - function, f - function bytecode, b - arguments count
   ; b = one of:
   ;  given,
   ;  (given . takes)
   ;  (given . (takes-from . takes-to)
   (define (arity-error-description env a f b)
      (case (type f)
         (type-closure
            (arity-error-description env a (ref f 1) b))
         (type-procedure
            (arity-error-description env a (ref f 1) b))
         (type-bytecode
            (define name (procedure->name env a))
            (if (or (eq? (ref f 0) BNA) (eq? (ref f 0) BNAV))
               (let cycle ((ip 0) (expects #n))
                  (case (ref f ip)
                     (BNA (cycle
                              (+ ip 4 (<< (ref f (+ ip 2)) 8) (ref f (+ ip 3)))
                              (cons (-- (ref f (+ ip 1))) expects)))
                     (BNAV (cycle
                              (+ ip 4 (<< (ref f (+ ip 2)) 8) (ref f (+ ip 3)))
                              (cons (list 'at 'least (-- (ref f (+ ip 1)))) expects)))
                     (else
                        (cons* (but b) name 'expects
                           (if (null? (cdr expects))
                              (if (pair? (car expects)) (car expects) expects)
                              (list 'one 'of (make-vector (reverse expects))))))))
               (list b 'for name)))
         ; primop, vector, etc.
         (else
            (define name (op-name env a))
            (if (null? b)
               (list name) ; just show error
            else
               (if (pair? b) ; (given . ...)
                  (cons* (but (car b)) name 'expects
                     (if (pair? (cdr b)) ; (given . (takes-from . takes-to)
                        (if (cddr b)
                           (list 'from (cadr b) 'to (cddr b))
                           (list 'at 'least (cadr b)))
                     else                ; (given . takes)
                        (list (cdr b))))
               else ; just given
                  (case (type a)
                     (type-value+ ; primop, find expected arguments count
                        (list (but b) name 'expects
                           (call/cc (lambda (found) ; expected count
                              (for-each (lambda (prim)
                                    (if (eq? a (ref prim 2))
                                       (found (ref prim 3))))
                                 *primops*)
                              'error)))) ; this should not be happen
                     ; todo: handle vectors, etc.
                     (else
                        (list b 'for name))))))))

   ; returns detailed description of error as list
   (define (error-description env code a b)
      (define (describe code a b)
         (list "error" code "->"
            (case code
               (0
                  `(unsupported vm code ,a))

               (ARITY-ERROR ; 17
                  (cons* "wrong number of arguments:"
                     (arity-error-description env a a b)))

               ; not a procedure
               ; bug: 258 library fail
               ((258 261)
                  `("operator is not a procedure:" ,a))

               (1049 ; FFAPPLY+1000
                  `("key not found:" ,b in ,a))

               ; invalid vector indexer
               (1032 ; VECTORAPPLY+1000
                  (if (enum? b)
                     `("vector index out of range:" ,b) ; todo?: print vector sizes
                     `(,b "cannot be a vector index")))

               (22 ; (vm:cast ...)
                  `(invalid cast to ,(typename b (cons 'type b)) for ,a))
               (50 ; (vm:run ...)
                  '(invalid vm:run argument))

               ((23 18 82) ; VMNEW, VMMAKE, VMALLOC
                  `(memory allocation error))

               (52 ; (car not-a-pair)
                  `(trying car of a non-pair ,a))
               (53 ; (cdr not-a-pair)
                  `(trying cdr of a non-pair ,a))

               ; ------------------------------------------------------------
               ; syscall errors:
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
                     `(,code " .. " ,a " " ,b))))))

      (if (eq? (type code) type-closure) ; continuation, is a (runtime-error)
         (if (eq? a ARITY-ERROR)
            (describe a (car b) (cdr b))
            (list a b)) ; just show runtime-error
      else ; generate our description
         (describe code a b)))
))
