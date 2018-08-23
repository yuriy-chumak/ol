; http://www.rosettacode.org/wiki/Detect_division_by_zero
(import (owl math fp))

(define (fzero? n)
   (equal? (fdiv 1 (inexact n)) +inf.0))

(for-each print (list
   (fzero? 5)    ; #false
   (fzero? 0)    ; #true
   (fzero? 1+2i) ; #false
   (fzero? 0+i)  ; #false
   (fzero? (inexact 0)) ; #true
))
