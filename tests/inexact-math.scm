(import (owl math fp))

(print "sqrt(17) = " (fp:sqrt (exact->inexact 42)))
(print "sin(42) = "  (fp:sin  (exact->inexact 42)))
(print "cos(42) = "  (fp:cos  (exact->inexact 42)))

(define a (exact->inexact 1))
(define b (exact->inexact 7))

(print "a + 7 = " (fp:add a b))
(print "a - 7 = " (fp:sub a b))
(print "a * 7 = " (fp:mul a b))
(print "a / 7 = " (fp:div a b))
