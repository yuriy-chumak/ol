(import (scheme repl))

; ------------------------------------------------
; you can't handle compile-time arity errors, like:
; 1. direct invalid primop calls
;   - (cons 1), (car), (clock 1 2 3 4), etc.
; 2. wrong "let" recursions
;   - (let loop ((x 1)) (loop 1 2 3 4))
; 3. direct lambda calls
;   - ((lambda (x) x) 1 2 3)
;   - ((lambda (x y . z) x) 1)

; but you can handle runtime arity errors

; - wrong "let" recursions
(with-exception-handler
   (lambda (x)
      (print "error detected. " x)
      (if (error-object? x)
         (print "  " (error-object-message x))))
   (lambda ()
      (let loop ((x 1))
         (define q loop)
         (q 1 2 3))
))

; - direct lambda calls
(define f (lambda (x) 777))

(with-exception-handler
   (lambda (x)
      (print "error detected. " x)
      (if (error-object? x)
         (print "  " (error-object-message x))))
   (lambda ()
      (f 1 2 3)
))

; - case-lambdas
(define g (case-lambda
   ((a) 1)
   ((a b) 2)
   ((a b c d . e) 4) ))

(with-exception-handler
   (lambda (x)
      (print "error detected. " x)
      (if (error-object? x)
         (print "  " (error-object-message x))))
   (lambda ()
      (g 1 2 3)
))

; - olvm critical errors (error class is 'crash)
(with-exception-handler
   (lambda (x)
      (print "error detected. " x)
      (if (error-object? x)
         (print "  " (error-object-message x))))
   (lambda ()
      ; don't repeat, this is a dirty hack!
      ((vm:cast (bytevector 0) type-bytecode) 1 2 3)
))

; - manual errors
(with-exception-handler
   (lambda (x)
      (print "error detected. " x)
      (if (error-object? x)
         (print "  " (error-object-message x))))
   (lambda ()
      (raise "hello, i'm error!")
))

; - syscalls
(with-exception-handler
   (lambda (x)
      (print "error detected. " x)
      (if (error-object? x)
         (print "  " (error-object-message x))))
   (lambda ()
      (syscall)
))

(with-exception-handler
   (lambda (x)
      (print "error detected. " x)
      (if (error-object? x)
         (print "  " (error-object-message x))))
   (lambda ()
      (syscall 0 1 2 3 4 5 6 7 8 9)
))

; - some math (case-lambda too)
(with-exception-handler
   (lambda (x)
      (print "error detected. " x)
      (if (error-object? x)
         (print "  " (error-object-message x))))
   (lambda ()
      (/)
))
