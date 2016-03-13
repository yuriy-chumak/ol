(define-library (tests circular)
   (import
      (r5rs core)
      (tests circular))
   (export bad-kitty)
   (begin
      (define bad-kitty 
         (map (λ (x) 'paw) (lrange 0 1 4)))))
