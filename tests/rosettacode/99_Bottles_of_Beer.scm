; http://rosettacode.org/wiki/99_Bottles_of_Beer
; http://rosettacode.org/wiki/99_Bottles_of_Beer/Lisp
(define nn 99)

(for-each (lambda (n)
   (let ((bottle (lambda (n) (if (eq? n 1) " bottle" " bottles")))
         (m (- n 1)))
      (print
         n (bottle n) " of beer on the wall, "
         n (bottle n) " of beer." "\n"
         "Take one down and pass it around, "
         (if (eq? m 0) "no more" m)
         (bottle m) " of beer on the wall.\n")))
   (reverse (iota nn 1)))
(print
   "No more bottles of beer on the wall, "
   "no more bottles of beer." "\n"
   "Go to the store and buy some more, "
   nn " bottles of beer on the wall.")
