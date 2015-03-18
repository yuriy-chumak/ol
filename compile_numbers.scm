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

(define (main)
(let* ((s1 (time-ms))
       (__ (go-throw-factors-cycle 14316))
       (s2 (time-ms)))
   (let* ((ss (floor (/ (- s2 s1) 1000)))
          (ms (- s2 s1 (* 1000 ss))))
      (print "*** "
         ss "."
         (if (< ms 100) "0" "")
         (if (< ms 10)  "0" "")
         ms
      ))))


(vector->file (list->vector (fasl-encode (lambda (args)
   (halt
   (main)))))
"numbers.bin")
