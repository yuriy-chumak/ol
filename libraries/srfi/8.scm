(define-library (srfi 8)
; http://srfi.schemers.org/srfi-8/srfi-8.html

; Title

; Abstract
;
; -----
(export
   receive)
(import
   (scheme core))

(begin
   (define-syntax receive
      (syntax-rules ()
         ((receive formals expression body ...)
            (call-with-values (lambda () expression)
                              (lambda formals body ...)))))
))
