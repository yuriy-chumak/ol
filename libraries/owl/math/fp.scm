;;;
;;; math module for inexact (floating point) numbers
;;;

(define-library (owl math fp)
   (export
      fless? fadd fsub fmul fdiv) ; 44 38 40 39 26

   (import
      (scheme core))

   (begin
      (define (fless? a b)(vm:fp2 #xD9 a b))

      (define (fadd a b)  (vm:fp2 #xC1 a b))
      (define (fsub a b)  (vm:fp2 #xE9 a b))
      (define (fmul a b)  (vm:fp2 #xC9 a b))
      (define (fdiv a b)  (vm:fp2 #xF9 a b))
))
