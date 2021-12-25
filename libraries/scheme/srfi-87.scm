(define-library (scheme srfi-87)
; http://srfi.schemers.org/srfi-87/srfi-87.html

;; Abstract
;
; This SRFI proposes an extension to the case syntax to allow the => clauses as in cond.


;; Rationale
;
; Case is introduced as a syntax sugar based on cond,  which helps to save a explicit
; calling to let. But without the => clause, if the result expression needs the value
; of key, the let can't be saved. For an easy example, suppose we want the following:


; NOTES: srfi-87 fully included into (scheme core) profile,
; you should not include it manually!
; -----
(export))
