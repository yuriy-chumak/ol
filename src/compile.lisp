(define (sleep1) (syscall 1200 #f #f #f))

(define (factors n)
   (let *factors ((d 1))
      (cond ((> d n) (list))
            ((= (mod n d) 0) (cons d (*factors (+ d 1))))
            (else (*factors (+ d 1))))))

(define (sum-factors n)
      (let ((f (factors n)))
        (let ((s (- (apply + f) n)))
           (print n ": " f " > " s)
           (sleep1)
         s)))

(define (go-throw-factors-cycle n)
  (let *go ((x n))
      (let ((s (sum-factors x)))
        (if (or (= s n) (= s 0) (= s x)) s (*go s)))))

(define main (lambda (args)
   ;(print (sum-factors 14316))
   (go-throw-factors-cycle 14316)
))


(display "unsigned char *language = (unsigned char*) \"")

(for-each (lambda (x)
             (display "\\x")
             (display (string (ref "0123456789abcdef" (div x 16))))
             (display (string (ref "0123456789abcdef" (mod x 16)))))
          (fasl-encode main))
(display "\";")
