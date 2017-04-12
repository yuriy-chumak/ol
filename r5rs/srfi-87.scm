(define-library (r5rs srfi-87)

; http://srfi.schemers.org/srfi-87/srfi-87.html

;; Abstract
; This SRFI proposes an extension to the case syntax to allow the => clauses as in cond. 

;; Rationale
; case is introduced as a syntax sugar based on cond, which helps to save a explicit
; calling to let. But without the => clause, if the result expression needs the value
; of key, the let can't be saved. For an easy example, suppose we want the following:
;
;    (case (get-symbol)
;      ((true) #t)
;      ((false) #f)
;      (else => (lambda (x) x)))
;
; Without the => clause in case, we have to write:
;
;    (let ((key (get-symbol)))
;      (cond ((eq? key 'true) #t)
;            ((eq? key 'false) #f)
;            (else key)))

; -----
(import
   (r5rs core))

(export
   srfi-87)

(begin
   (define srfi-87 #true)
   
))
