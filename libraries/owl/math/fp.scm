;;;
;;; math module for inexact (floating point) numbers
;;;

(define-library (owl math fp)
   (export 
      fsqrt fsin fcos
      ffloor fceil ffrac

      fadd fmul fsub fdiv
      fmax fmin
      )

   (import 
      (scheme core))

   (begin
      (define (fsqrt num) (vm:fpu1 0 num))
      (define (fsin num)  (vm:fpu1 1 num))
      (define (fcos num)  (vm:fpu1 2 num))
      (define (ffloor num) (vm:fpu1 3 num))
      (define (fceil num) (vm:fpu1 4 num))
      (define (ffrac num) (vm:fpu1 5 num))
           
      (define (fadd a b) (vm:fpu2 0 a b))
      (define (fmul a b) (vm:fpu2 1 a b))
      (define (fsub a b) (vm:fpu2 2 a b))
      (define (fdiv a b) (vm:fpu2 3 a b))
      (define (fmax a b) (vm:fpu2 4 a b))
      (define (fmin a b) (vm:fpu2 5 a b))
))
