(define-library (scheme srfi-0)
; http://srfi.schemers.org/srfi-0/srfi-0.html

; Title
;  Feature-based conditional expansion construct

; Abstract
;
;  It is desirable that programs which depend on additions to standard Scheme name
;  those additions. SRFIs provide the specifications of these additions ("features"),
;  and SRFI 0 provides the means to actually check that these features are present
;  in the Scheme system by means of the cond-expand construct.

; NOTES: srfi-0 fully included into lang/eval.scm library
; -----
(export
   srfi-0)
(import
   (scheme core))

(begin
   (setq srfi-0 #true)
))
