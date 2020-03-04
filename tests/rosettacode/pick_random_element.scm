; http://www.rosettacode.org/wiki/Pick_random_element
(define *path* (cons "tests/rosettacode" *path*))
(import (otus random!))

(define x '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(print (list-ref x (rand! (length x))))
