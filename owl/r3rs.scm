; http://people.csail.mit.edu/jaffer/R3RS_ANNOUNCE.txt
(define-library (owl r3rs)
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
      ; по списку из r3rs надо пооформлять все макросы

      ; 1. Overview of Scheme
      ;   ... можно скопипастить стандарт?

      ; 2. Lexical conventions
      ;   ... можно скопипастить стандарт?

      ; 3. Basic concepts
      ;   ... можно скопипастить стандарт?

      ; 4. Expressions
      ; 4.1 Primitive expression types
      ; 4.1.1 Variable references
      ; 4.1.2 Literal expressions
      ; ...

      ; 4.1.4 lambda expressions
      (define-syntax λ
         (syntax-rules () 
            ((λ . x) (lambda . x))))


      ; Continuation - http://en.wikipedia.org/wiki/Continuation
))