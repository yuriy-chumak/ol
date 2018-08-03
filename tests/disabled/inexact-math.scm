(import (owl math fp))

(print "sqrt(17) = " (fsqrt (exact->inexact 42)))
(print "sin(42) = "  (fsin  (exact->inexact 42)))
(print "cos(42) = "  (fcos  (exact->inexact 42)))

(define a (exact->inexact 1))
(define b (exact->inexact 7))

(print "1 + 7 = " (fadd a b))
(print "1 - 7 = " (fsub a b))
(print "1 * 7 = " (fmul a b))
(print "1 / 7 = " (fdiv a b))
