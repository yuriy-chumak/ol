;;;
;;; math module for inexact (floating point) numbers
;;;

(define-library (owl math fp)
   (export 
      fsqrt fsin fcos

      fadd fmul fsub fdiv
      )

   (import 
      (r5rs core))

   (begin
      (define (fsqrt num) (vm:fpu1 0 num))
      (define (fsin num)  (vm:fpu1 1 num))
      (define (fcos num)  (vm:fpu1 2 num))
           
      (define (fadd a b) (vm:fpu2 0 a b))
      (define (fmul a b) (vm:fpu2 1 a b))
      (define (fsub a b) (vm:fpu2 2 a b))
      (define (fdiv a b) (vm:fpu2 3 a b))
))
