(define-library (scheme srfi-16)
; http://srfi.schemers.org/srfi-16/srfi-16.html

;; Abstract
;
; CASE-LAMBDA, a syntax for procedures with  a variable number of arguments, is introduced.


;; Rationale
;
; CASE-LAMBDA reduces  the clutter of  procedures that execute  different code depending on
; the number of arguments they were passed; it is a pattern-matching mechanism that matches
; on the number of arguments. CASE-LAMBDA is available in some Scheme systems.
;
; While CASE-LAMBDA can  be implemented as a macro  using only facilities available in R5RS
; Scheme, it admits considerable implementation-specific optimization. 


; NOTE: srfi-16 fully included into scheme core profile, you should not include it manually!
; -----
(export
   srfi-16
   case-lambda)

(begin
   (setq srfi-16 #true)

   ; makes a list of options to be compiled to a chain of code bodies w/ jumps
   ; note, could also merge to a jump table + sequence of codes
   (define-syntax case-lambda
      (syntax-rules (arity-error)
         ((case-lambda)
            (lambda () arity-error)) ; arity-error (todo: change to syntax-error (?))
         ((case-lambda (formals . body))
            (lambda formals . body))
         ((case-lambda (formals . body) . rest)
            (either (lambda formals . body)
               (case-lambda . rest)))))

))
