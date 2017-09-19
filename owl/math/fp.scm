;;;
;;; math module for inexact (floating point) numbers
;;;

(define-library (owl math fp)
   (export 
      fp:sqrt fp:sin fp:cos
      )

   (import 
      (r5rs core) (src vm))

   (begin
      (define fp1 (vm:new-raw-object TBYTECODE '(25 3 0 6  33 4 5 6    24 6  17)))
      (define fp2 (vm:new-raw-object TBYTECODE '(25 4 0 7  34 4 5 6 7  24 7  17)))


      (define (fp:sqrt num) (fp1 0 num))
      (define (fp:sin num)  (fp1 1 num))
      (define (fp:cos num)  (fp1 2 num))
           
))
