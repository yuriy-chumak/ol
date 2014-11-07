;;;; random number generator
(define-library (owl random!)

   (export   
      srand rand)

   (import
      (owl math)
      (owl time)
      (owl defmac))

   (begin
      (define *seed* 
         (let* ((ss ms (clock))) (cons ss ms)))

      (define (srand seed)
         (set-car! *seed* seed))
      (define (rand limit)
         (let* ((x (car *seed*))
                (a _ (fx* x 214013))
                (b _ (fx+ a 2531011))
                (c _ (fx>> b 16))
                (o p d (fxqr 0 c limit)))
            (set-car! *seed* c)
            d))
))

;                (b p (fx+ a 2531011))
;                (c q (fx>> b 16))
;                (d r (fx% c limit)))
