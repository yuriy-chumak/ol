; http://rosettacode.org/wiki/Extreme_floating_point_values#Ol
(import (scheme inexact))

(print "infinity: " (/ 1 0))
(print "minus infinity: " (log 0))

(import (owl math fp))
(print "not-a-number: " (fsqrt -1))

; note: your must use equal? or eqv? but not eq? for comparison
(print "is this is not a number? " (equal? (fsqrt -1) +nan.0))
