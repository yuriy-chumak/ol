; https://rosettacode.org/wiki/Matrix_multiplication#Ol

(define (matrix-multiply matrix1 matrix2)
(map
   (lambda (row)
      (apply map
         (lambda column
            (apply + (map * row column)))
         matrix2))
   matrix1))
 
(print
   (matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4))))
