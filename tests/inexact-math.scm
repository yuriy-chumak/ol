(import (owl math fp))

(print "sqrt(17) = " (fp:sqrt (exact->inexact 42)))
(print "sin(42) = "  (fp:sin  (exact->inexact 42)))
(print "cos(42) = "  (fp:cos  (exact->inexact 42)))