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
                      (a _ (vm:mul x 214013))
                      (b _ (vm:add a 2531011))
                      (c _ (vm:shr b 16))
                      (o p d (vm:div 0 c limit)))
                  (set-car! seed c)
                  d))))
))
