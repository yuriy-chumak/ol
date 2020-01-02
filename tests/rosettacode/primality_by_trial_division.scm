; https://rosettacode.org/wiki/Primality_by_trial_division#Ol

(define (prime? number)
   (define max (sqrt number))
   (define (loop divisor)
      (or (> divisor max)
          (and (> (modulo number divisor) 0)
               (loop (+ divisor 2)))))
   (or (= number 1)
       (= number 2)
       (and
          (> (modulo number 2) 0)
          (loop 3))))

; first prime numbers less than 100
(for-each (lambda (n)
      (if (prime? n)
         (display n))
      (display " "))
   (iota 100))
(print)

; few more sintetic tests
(for-each (lambda (n)
      (print n " - prime? " (prime? n)))
   '(
      1234567654321 ; 1111111 * 1111111
      679390005787 ; really prime, I know that
      679390008337 ; same
      666810024403 ; 680633 * 979691 (multiplication of two prime numbers)
      12345676543211234567654321
      12345676543211234567654321123456765432112345676543211234567654321123456765432112345676543211234567654321
   ))
