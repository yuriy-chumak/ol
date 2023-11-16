; https://rosettacode.org/wiki/Multifactorial

(define (multifactorial n d)
   (fold * 1 (iota (div n d) n (negate d))))

(for-each (lambda (i)
      (display "Degree ")
      (display i)
      (display ":")
      (for-each (lambda (n)
            (display " ")
            (display (multifactorial n i)))
         (iota 10 1))
      (print))
   (iota 5 1))

; ---
(define (!!!!! n) (multifactorial n 5))
(print (!!!!! 74))

(import (math infix-notation))
(define \\postfix-functions (put \\postfix-functions '!!!!! #t))

(print (\\
   2 + 74!!!!!
))
