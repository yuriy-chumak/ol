(define-library (owl r5rs)
   ; basic OL functions (procedures):
   ; quote
   ; lambda
   ; rlambda
   ; _branch
   ;
   ; extended OL function (primops):
   ; car, cdr, ... ??


   (export
      λ
   )

   (begin

      (define-syntax λ 
         (syntax-rules () 
            ((λ . x) (lambda . x))))
))