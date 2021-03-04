; http://www.rosettacode.org/wiki/Isqrt_(integer_square_root)_of_X#Ol

(print "Integer square roots of 0..65")
(for-each (lambda (x)
      (display (isqrt x))
      (display " "))
   (iota 66))
(print)

(print "Integer square roots of 7^n")
(for-each (lambda (x)
      (print "x: " x ", isqrt: " (isqrt x)))
   (map (lambda (i)
         (expt 7 i))
      (iota 73 1)))
(print)
