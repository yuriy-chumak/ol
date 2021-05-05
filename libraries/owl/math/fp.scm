;;;
;;; math module for inexact (floating point) numbers
;;;

(define-library (owl math fp)
   (export
      fless?
      fadd fsub fmul fdiv
      fsqrt fsin fcos ftan
      fasin facos fatan fatan2
      flog flog2 fexp fexpt
      ffloor
   )

   (import
      (scheme core))

   (begin
      (define-syntax FP1 (syntax-rules () ((FP1 name value)
         (define (name z) (vm:fp1 value z)))))
      (define-syntax FP2 (syntax-rules () ((FP2 name value)
         (define (name a b) (vm:fp2 value a b)))))

      (FP2 fless? #xD9)

      ; basic fpu math
      (FP2 fadd #xC1)
      (FP2 fsub #xE9)
      (FP2 fmul #xC9)
      (FP2 fdiv #xF9)

      ; additional math staff
      (FP1 fsqrt #xFA)
      (FP1 ffloor #xFC)

      (FP1 fsin  #xFE) ; sine
      (FP1 fasin #x8E) ; arcsize
      (FP1 fcos  #xFF) ; cosine
      (FP1 facos #x8F) ; 
      (FP1 ftan  #xF2)
      (FP1 fatan #xF3)
      (FP1 flog  #xF1)
      (FP1 fexp  #x81)

      (FP2 fatan2 #xF3)
      (FP2 flog2  #xF1)
      (FP2 fexpt  #x81) ; a ** b
))
