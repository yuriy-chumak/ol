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

; long version
(define (matrix-multiply A B)
   (define m (length A))
   (define n (length (car A)))
   (assert (eq? (length B) n) ===> #true)
   (define q (length (car B)))
   (define (at m x y)
      (lref (lref m x) y))


   (let mloop ((i (- m 1)) (rows #null))
      (if (< i 0)
         rows
         (mloop
            (- i 1)
            (cons
               (let rloop ((j (- q 1)) (r #null))
                  (if (< j 0)
                     r
                     (rloop
                        (- j 1)
                        (cons
                           (let loop ((k 0) (c 0))
                              (if (eq? k n)
                                 c
                                 (loop (+ k 1) (+ c (* (at A i k) (at B k j))))))
                           r))))
               rows)))))

; test:
(define M 372)
(define N 17)

; [0 1 2 ... 371]
; [1 2 3 ... 372]
; [2 3 4 ... 373]
; ...
; [16 17 ... 387]
(define A (map (lambda (i)
                  (iota M i))
            (iota N)))

; [0 1 2 ... 16]
; [1 2 3 ... 17]
; [2 3 4 ... 18]
; ...
; [371 372 ... 387]
(define B (map (lambda (i)
                  (iota N i))
            (iota M)))

(for-each print (matrix-multiply A B))
