Numbers
=======

Ol's number types:
- `type-enum+`, 0
- `type-enum-`, 32
- `type-int+`, 40
- `type-int-`, 41
- `type-rational`, 42
- `type-complex`, 43
- `type-inexact`, 44

# number?
`(number? obj)`, *procedure*


# complex?
`(complex? obj)`, *procedure*


# real?
`(real? obj)`, *procedure*


# rational?
`(rational? obj)`, *procedure*


# integer?
`(integer? obj)`, *procedure*


# exact?
`(exact? z)`, *procedure*


# inexact?
`(inexact? z)`, *procedure*


# exact-integer?
`(exact-integer? z)`, *procedure*


# finite?
`(finite? z)`, *procedure*, `(scheme inexact)` library


# infinite?
`(infinite? z)`, *procedure*, `(scheme inexact)` library


# nan?
`(nan? z)`, *procedure*, `(scheme inexact)` library


# =
`(= z1 z2 z3 ...)`, *procedure*


# <
`(< z1 z2 z3 ...)`, *procedure*


# >
`(> z1 z2 z3 ...)`, *procedure*


# <=
`(<= z1 z2 z3 ...)`, *procedure*


# >=
`(>= z1 z2 z3 ...)`, *procedure*


# zero?
`(zero? z)`, *procedure*


# positive?
`(positive? z)`, *procedure*


# negative?
`(negative? z)`, *procedure*


# odd?
`(odd? z)`, *procedure*


# even?
`(even? z)`, *procedure*


# max
`(max x1 x2 ...)`, *procedure*


# min
`(min x1 x2 ...)`, *procedure*


# +
`(+ z1 ...)`, *procedure*


# *
`(* z1 ...)`, *procedure*


# -
`(- z)`, *procedure*
`(- z1 z2 ...)`, *procedure*


# -
`(/ z)`, *procedure*
`(/ z1 z2 ...)`, *procedure*


# abs
`(abs x)`, *procedure*


<!-- # floor
`(floor/ n1 n2)`, *procedure*


# floor-quotient
`(floor-quotient n1 n2)`, *procedure*


# floor-remainder
`(floor-remainder n1 n2)`, *procedure*


# truncate
`(truncate/ n1 n2)`, *procedure*


# truncate-quotient
`(truncate-quotient n1 n2)`, *procedure*


# truncate-remainder
`(truncate-remainder n1 n2)`, *procedure* -->


# quotient
`(quotient n1 n2)`, *procedure*


# remainder
`(remainder n1 n2)`, *procedure*


# modulo
`(modulo n1 n2)`, *procedure*


# gcd
`(gcd n1 ...)`, *procedure*


# lcm
`(lcm n1 ...)`, *procedure*


# numerator 
`(numerator q)`, *procedure*


# denominator
`(denominator q)`, *procedure*


# floor
`(floor x)`, *procedure*
# ceiling
`(ceiling x)`, *procedure*
# truncate
`(truncate x)`, *procedure*
# round
`(round x)`, *procedure*
# rationalize
`(rationalize x y)`, *procedure*


# exp
`(exp z)`, *procedure*, `(scheme inexact)` library
# log
`(log z)`, *procedure*, `(scheme inexact)` library
`(log z1 z2)`, *procedure*, `(scheme inexact)` library
# sin
`(sin z)`, *procedure*, `(scheme inexact)` library
# cos
`(cos z)`, *procedure*, `(scheme inexact)` library
# tan
`(tan z)`, *procedure*, `(scheme inexact)` library
# asin
`(asin z)`, *procedure*, `(scheme inexact)` library
# acos
`(acos z)`, *procedure*, `(scheme inexact)` library
# atan
`(atan z)`, *procedure*, `(scheme inexact)` library
`(atan y x)`, *procedure*, `(scheme inexact)` library


# square
`(square z) `
# sqrt
`(sqrt z)`
# exact-integer-sqrt
`(exact-integer-sqrt k)`
# expt
`(expt z1 z2)`

<!-- (make-rectangular x1 x2) complex library procedure
(make-polar x3 x4) complex library procedure
(real-part z) complex library procedure
(imag-part z) complex library procedure
(magnitude z) complex library procedure
(angle z) complex library procedure -->

# inexact
`(inexact z)`
# exact
`(exact z)`


# number->string
`(number->string z)`, *procedure*
`(number->string z radix)`, *procedure*

```scheme
(number->string 0)            ==> "0")
(number->string 1.2)          ==> "6/5")
(number->string 1.2 4)        ==> "12/11")
(number->string -77)          ==> "-77")
(number->string 7-4i)         ==> "7-4i")
(number->string +inf.0)       ==> "+inf.0")
```

# string->number
`(string->number string)`, *procedure*
`(string->number string radix)`, *procedure*

```scheme
(string->number "100")    ==> 100
(string->number "100" 16) ==> 256
(string->number "1e2")    ==> 100
```
