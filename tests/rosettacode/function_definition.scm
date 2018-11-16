; http://www.rosettacode.org/wiki/Function_definition

(lambda (x y)
   (* x y))

(define multiply (lambda (x y) (* x y)))

(define (multiply x y) (* x y))

(define (multiply x y)
   (let loop ((y y) (n 0))
      (if (= y 0)
         n
         (loop (- y 1) (+ n x)))))

(print (multiply 7 8))
