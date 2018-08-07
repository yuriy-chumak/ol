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
      (define (fsqrt num) (vm:fp1 0 num))
      (define (fsin num)  (vm:fp1 1 num))
      (define (fcos num)  (vm:fp1 2 num))
      (define (ffloor num)(vm:fp1 3 num))
      (define (fceil num) (vm:fp1 4 num))
      (define (ffrac num) (vm:fp1 5 num))
           
      (define (fadd a b)  (vm:fp2 0 a b))
      (define (fmul a b)  (vm:fp2 1 a b))
      (define (fsub a b)  (vm:fp2 2 a b))
      (define (fdiv a b)  (vm:fp2 3 a b))
      (define (fmax a b)  (vm:fp2 4 a b))
      (define (fmin a b)  (vm:fp2 5 a b))
))
