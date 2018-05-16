; r5rs
; 4.2.4. Iteration
(define-library (scheme r5rs iteration)
   (export
      do)
   (import
      (r5rs core))
(begin

; library syntax: do ((<variable1> <init1> <step1>) ...) (<test> <expression> ...) <command> ...
(define-syntax do ; ?
   (syntax-rules ()
      ((do ((var init step) ...) (test expr ...) command ...)
         (let loop ((var init) ...)
            (if test
               (begin expr ...)
               (loop step ...))))))
   
; end.
))