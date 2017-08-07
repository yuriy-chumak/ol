; otus lisp math for inexact numbers
(define-library (otus imath)
   (export
      fsqrt
      fsin fcos
      fadd fmul fsub fdiv
   )
   (import
      (r5rs core))

(begin
   (define (fsqrt x)
      (vm:fpu1 0 x))

   (define (fsin x)
      (vm:fpu1 1 x))

   (define (fcos x)
      (vm:fpu1 2 x))


   (define (fadd x y)
      (vm:fpu2 0 x y))
   (define (fmul x y)
      (vm:fpu2 1 x y))
   (define (fsub x y)
      (vm:fpu2 2 x y))
   (define (fdiv x y)
      (vm:fpu2 3 x y))

))