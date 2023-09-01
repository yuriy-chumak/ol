Infix Notation
==============
`(infix-notation expression)`, *macro*



```scheme
> (import (math infix-notation))
> (define a 3)
> (define b 5)
> (define c 7)

(infix-notation
   a + b
)
==> (+ a b)

(infix-notation
   b + a
)
==> (+ b a)

(infix-notation
   a + b * c
)
==> (+ a (* b c))

(infix-notation
   (a + b) * c
)
==> (* (+ a b) c)

(infix-notation
   a * (b + c)
)
==> (* a (+ b c))

(infix-notation
   a / b + c / d
)
==> (+ (/ a b) (/ c d))

(infix-notation
   (a + b) * (c + d)
)
==> (* (+ a b) (+ c d))

(infix-notation
   ((a + b) * c) - d
)
==> (- (* (+ a b) c) d)


```