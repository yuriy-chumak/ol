(define-library (tests circular)
   (import
      (r5rs base)
      (tests circular))
   (export bad-kitty)
   (begin
      (define bad-kitty 
         (map (λ (x) 'paw) (iota 0 1 4)))))
