; http://www.rosettacode.org/wiki/Loops/Break
(define *include-dirs* (cons "tests/rosettacode" *include-dirs*))
(import (otus random!))

(call/cc (lambda (break)
   (let loop ()
      (if (= (rand! 20) 10)
         (break #t))
      (print (rand! 20))
      (loop))))
