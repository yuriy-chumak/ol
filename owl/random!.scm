;;;; random number generator
(define-library (owl random!)

   (export   
      rand!)

   (import
      (r5rs core)
      (owl math)
      (owl time))

   (begin
      ; (rand limit)
      (define rand!
         (let* ((ss ms (clock))
                (seed (cons ms ss)))
            (lambda (limit)
               (let* ((x (car seed))
                      (a _ (fx:* x 214013))
                      (b _ (fx:+ a 2531011))
                      (c _ (fx:>> b 16))
                      (o p d (fx:/ 0 c limit)))
                  (set-car! seed c)
                  d))))
))
