(define-library (scheme srfi-13)
; http://srfi.schemers.org/srfi-13/srfi-13.html

;; Abstract
;
; This SRFI proposes a coherent and comprehensive set of string-processing procedures; it is
; accompanied by a reference implementation of the spec.


;; Rationale
;
; This SRFI defines two libraries that provide a rich set of operations for manipulating strings.
; These are frequently useful for scripting and other text-manipulation applications.
;
; The library's design was influenced by the string libraries found in MIT Scheme, Gambit, RScheme,
; MzScheme, slib, Common Lisp, Bigloo, guile, Chez, APL, Java, and the SML standard basis.
;
; All procedures involving character comparison are available in both case-sensitive and case-insensitive forms.
;
; All functionality is available in substring and full-string forms.

(export
   make-kmp-restart-vector)

(import
   (scheme core)
   (owl vector)
   (owl string)
   (owl math))

(begin
   ; Build a Knuth-Morris-Pratt "restart vector," which is useful for quickly searching
   ; character sequences for the occurrence of string p.
   (define (make-kmp-restart-vector p)
      (let*((m (string-length p))
            (next (make-vector m 0)))
         (let loop ((i 1) (j 0))
            (cond
             ((>= i (- m 1))
                next)
             ((eq? (string-ref p i) (string-ref p j))
                (let ((i (+ i 1))
                      (j (+ j 1)))
                   (set-ref! next i j)
                   (loop i j)))
             ((= j 0)
                (let ((i (+ i 1)))
                   (set-ref! next i 0)
                   (loop i j)))
             (else
                (loop i (ref next j)))))))

))
