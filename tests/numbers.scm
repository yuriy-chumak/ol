; http://math.d3.ru/comments/591298/
(define (factors n)
  (define (*factors d)
      (cond ((> d n) (list))
            ((= (mod n d) 0) (cons d (*factors (+ d 1))))
            (else (*factors (+ d 1)))))
  (*factors 1))

(define (sum-factors n)
      (let ((f (factors n)))
        (let ((s (- (apply + f) n)))
           (print n ": " f " > " s)
         s)))

(define (go-throw-factors-cycle n)
  (define (*go x)
      (let ((s (sum-factors x)))
        (if (or (= s n) (= s 0) (= s x)) s (*go s))))
  (*go n))

(go-throw-factors-cycle 14316)
