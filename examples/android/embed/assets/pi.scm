(define-library (pi)
   (export pi)

   (import (otus lisp))

(begin
   (actor 'pi (lambda ()
   
      (let loop ((q 1) (r 0) (t 1) (k 1) (n 3) (l 3))
         (if (< (- (+ (* 4 q) r) t) (* n t))
            (begin
               ; (display n):
               (let*((envelope (wait-mail))
                     (sender msg envelope))
                  (mail sender n))

               (loop (* q  10)
                     (* 10 (- r (* n t)))
                     t
                     k
                     (- (quotient (* 10 (+ (* 3 q) r)) t) (* 10 n))
                     l
                     ))
            (begin
               (loop (* q k)
                     (* (+ (* 2 q) r) l)
                     (* t l)
                     (+ k 1)
                     (quotient (+ (* q (* 7 k)) 2 (* r l)) (* t l))
                     (+ l 2)
                     )))) ))

   (define (pi)
      (await (mail 'pi #f)))
))