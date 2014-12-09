; http://math.d3.ru/comments/591298/
(define (factors n)
   (let *factors ((d 1))
      (cond ((> d n) (list))
            ((= (mod n d) 0) (cons d (*factors (+ d 1))))
            (else (*factors (+ d 1))))))

(define (sum-factors n)
      (let ((f (factors n)))
        (let ((s (- (apply + f) n)))
           (print n ": " f " > " s)
         s)))

(define (go-throw-factors-cycle n)
  (let *go ((x n))
      (let ((s (sum-factors x)))
        (if (or (= s n) (= s 0) (= s x)) s (*go s)))))

(go-throw-factors-cycle 14316)
