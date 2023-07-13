Numerical operations
====================

Mathematically, numbers are arranged into a tower of subtypes in which each level is a subset of the level above it:
* number
* complex number
* real number
* rational number
* integer
* natural

[number?](#number), [complex?](#complex), [real?](#real), [rational?](#rational), [integer?](#integer), [natural?](#natural),
[exact?](#exact), [inexact?](#inexact),
[finite?](#finite), [infinite?](#infinite), [nan?](#nan),
[zero?](#zero), [positive?](#positive), [negative?](#negative), [odd?](#odd), [even?](#even),
[=](#=), [<](#-1), [>](#-2), [<=](#-3), [>=](#-4),
[max](#max), [min](#min),
[+](#-5),[-](#-),[*](#-6),[/](#-7),
[abs](#abs),
[quotient](#quotient), [remainder](#remainder), [modulo](#modulo),
[numerator](#numerator), [denominator](#denominator),
[floor](#floor), [ceiling](#ceiling), [truncate](#truncate), [round](#round),
[rationalize](#rationalize),
[square](#square), [sqrt](#sqrt)

# number?
`(number? obj)`, *procedure*

Returns #true if *obj* is a number. Any number.

```scheme
(number? 1)                   ==>  #true
(number? '(1))                ==>  #false
(number? 1.23)                ==>  #true
(number? 4/21)                ==>  #true
(number? 7-4i)                ==>  #true
(number? 1.22e10)             ==>  #true
(number? #b10110001)          ==>  #true
(number? #o771263)            ==>  #true
(number? #xDEAFBEEF)          ==>  #true
(number? #b1011.1)            ==>  #true
(number? "1234")              ==>  #false
(number? (string->number "1234))  ==>  #true
(number? #i3.14)              ==>  #true
(number? +inf.0)              ==>  #true
(number? +nan.0)              ==>  #true
```

# complex?
`(complex? obj)`, *procedure*

Returns #true if *obj* is a complex number.  
Note that rational, integer, and real numbers are also complex.

```scheme
(complex? 3+7i)               ==>  #true
(complex? (complex 3 7))      ==>  #true
(complex? 3+0i)               ==>  #true
(complex? 17)                 ==>  #true
(complex? #b1101)             ==>  #true
(complex? "3+7i")             ==>  #false
(complex? 11/17)              ==>  #true
(complex? +inf.0)             ==>  #true
```

# real?
`(real? obj)`, *procedure*

Returns #true if *obj* is a complex number.  
Real numbers are those numbers used to measure a continuous one-dimensional quantity such as a distance, duration or temperature.

```scheme
(real? 3)                     ==>  #true
(real? 3.4)                   ==>  #true
(real? 1e10)                  ==>  #true
(real? 4/21)                  ==>  #true
(real? 3+7i)                  ==>  #false
(real? #i3.14)                ==>  #true
(real? +inf.0)                ==>  #true
(real? +nan.0)                ==>  #true
(real? '(1))                  ==>  #false
(real? "1234")                ==>  #false
```

# rational?
`(rational? obj)`, *procedure*

Returns #true if *obj* is a ratinal number.  
Rational numbers are numbers that can be expressed as the quotient or fraction of two integers.

```scheme
(rational? -inf.0)            ==>  #false
(rational? 2)                 ==>  #true
(rational? 6/10)              ==>  #true
(rational? 6/3)               ==>  #true
(rational? 3+4i)              ==>  #false
(rational? #i3.14)            ==>  #false
```

# integer?
`(integer? obj)`, *procedure*

Returns #true if *obj* is an integer number.  
Note that zero is an integer too.

```scheme
(integer? 3+0i)               ==>  #true
(integer? 3+4i)               ==>  #false
(integer? 3.0)                ==>  #true
(integer? 3.4)                ==>  #false
(integer? 8/4)                ==>  #true
(integer? 8/5)                ==>  #false
(integer? #i3)                ==>  #false
```

# natural?
`(natural? obj)`, *procedure*

Returns #true if *obj* is a natural number.  
Natural numbers are those numbers used for counting and ordering. Note that zero is not natural.

```scheme
(natural? 3.0)                ==>  #true
(natural? 3.1)                ==>  #false
(natural? 1)                  ==>  #true
(natural? 0)                  ==>  #false
(natural? -1)                 ==>  #false
(natural? 8/5)                ==>  #false
(natural? #i3)                ==>  #false
(natural? 3+4i)               ==>  #false
```

# exact?
`(exact? z)`, *procedure*

Returns #true if *z* is an exact number.  
Exact numbers are all numbers except inexact.

```scheme
(exact? 3)                    ==>  #true
(exact? #i3)                  ==>  #false
(exact? 3.14)                 ==>  #true
(exact? 3+7i)                 ==>  #true
(exact? #i3+2i)               ==>  #false
(exact? 3/7-i)                ==>  #true
(exact? +inf.0)               ==>  #false
```

# inexact?
`(inexact? z)`, *procedure*

Returns #true if *z* is an inexact number.  
Inexact numbers are platform-specific floating-point numbers occupying fixed computer memory (usually 64 bits).

```scheme
(inexact? 3)                  ==>  #false
(inexact? #i3)                ==>  #true
(inexact? 3.14)               ==>  #false
(inexact? 3+7i)               ==>  #false
(inexact? 3/7-i)              ==>  #false
(inexact? +inf.0)             ==>  #true
(inexact? #i3.14)             ==>  #true
```

# exact-integer?
`(exact-integer? z)`, *procedure*

Returns #true if *z* is both exact and an integer.
Note that `exact-integer?` is exactly the same as `integer?`.

```scheme
(exact-integer? 3)            ==>  #true
(exact-integer? 3+0i)         ==>  #true
(exact-integer? 8/5)          ==>  #false
(exact-integer? 8/4)          ==>  #true
(exact-integer? -42)          ==>  #true
(exact-integer? +inf.0)       ==>  #false
```

# finite?
`(finite? z)`, *procedure*

Returns #true for all real numbers except +inf.0, -inf.0, and +nan.0,
and for complex numbers if their real and imaginary parts are both finite.

```scheme
(finite? 3)                   ==>  #true
(finite? 3.14)                ==>  #true
(finite? +inf.0)              ==>  #false
(finite? -inf.0)              ==>  #false
(finite? +nan.0)              ==>  #false
(finite? #i3.14)              ==>  #true
(finite? #i2-7i)              ==>  #true
(finite? +inf.0-7i)           ==>  #false
```

# infinite?
`(infinite? z)`, *procedure*

Returns #true for the real numbers +inf.0 and -inf.0,
and for complex numbers if their real or imaginary parts or both are infinite.

```scheme
(infinite? 3)                 ==>  #false
(infinite? 3.14)              ==>  #false
(infinite? +inf.0)            ==>  #true
(infinite? -inf.0)            ==>  #true
(infinite? +nan.0)            ==>  #false
(infinite? #i3.14)            ==>  #false
(infinite? #i2-7i)            ==>  #false
(infinite? +inf.0-7i)         ==>  #true
(infinite? (complex 11 +inf.0)) ==>  #true
```

# nan?
`(nan? z)`, *procedure*

Returns #true for +nan.0,
and for complex numbers if their real or imaginary parts or both are +nan.0

```scheme
(nan? +nan.0)                 ==>  #true
(nan? 32)                     ==>  #false
(nan? +nan.0+5.0i)            ==>  #true
(nan? (complex 5.0 +nan.0))   ==>  #true
(nan? 5.0++nan.0i)            ==>  #true
(nan? 1+2i)                   ==>  #false
```

# zero?
`(zero? z)`, *procedure*

Returns #true if z is a zero.

```scheme
(zero? 0)                     ==>  #true
(zero? #i0)                   ==>  #true
(zero? #i0.0000000000000001)  ==>  #false
(zero? (- 7 7))               ==>  #true
(zero? (+ 7 7))               ==>  #false
```

# positive?
`(positive? x)`, *procedure*

Returns #true if x is positive. Complex numbers are not applicable.  
Note that zero is neither positive nor negative.

```scheme
(positive? 0)                 ==>  #false
(positive? -1)                ==>  #false
(positive? 42)                ==>  #true
(positive? -3/7)              ==>  #false
(positive? 3/-7)              ==>  #false
(positive? -inf.0)            ==>  #false
(positive? +inf.0)            ==>  #true
(positive? +nan.0)            ==>  #false
(positive? #i0)               ==>  #false
```

# negative?
`(negative? x)`, *procedure*

Returns #true if x is negative. Complex numbers are not applicable.  
Note that zero is neither negative nor positive.

```scheme
(negative? 0)                 ==>  #false
(negative? -1)                ==>  #true
(negative? 42)                ==>  #false
(negative? -3/7)              ==>  #true
(negative? 3/-7)              ==>  #true
(negative? -inf.0)            ==>  #true
(negative? +inf.0)            ==>  #false
(negative? +nan.0)            ==>  #false
(negative? #i0)               ==>  #false
```

# odd?
`(odd? n)`, *procedure*

Returns #true if *n* is odd, *n* must be natural or zero.

```scheme
(odd? 1)                      ==>  #true
(odd? 77)                     ==>  #true
(odd? 1234567890)             ==>  #false
```

# even?
`(even? n)`, *procedure*

Returns #true if n is even, *n* must be natural or zero.

```scheme
(even? 1)                     ==>  #false
(even? 77)                    ==>  #false
(even? 1234567890)            ==>  #true
```

# <a name="="></a>=
`(= z1 z2 ...)`, *procedure*

Returns #true if arguments are equal in a mathematical sense.

```scheme
(= 123)                       ==>  #true
(= 123 123)                   ==>  #true
(= 123 123.0)                 ==>  #true
(= 123 #i123)                 ==>  #true
(= 123 123 (+ 100 23))        ==>  #true
(= 123 123.2)                 ==>  #false
(= 123 +nan.0)                ==>  #false
(= 7/11 14/22)                ==>  #true
(= 7/10 21/30 0.7)            ==>  #true
(= 0.7 #i0.7)                 ==>  #true
(= 3+8i #i3+#i8i)             ==>  #true
```

# <
`(< x1 x2 ...)`, *procedure*

Returns #true if arguments monotonically increasing.  
Note that complex numbers are not applicable.

```scheme
(< 1)                         ==>  #true
(< 1 2 3)                     ==>  #true
(< 3 2 1)                     ==>  #false
(< -1)                        ==>  #true
(< 1 2 3 1)                   ==>  #false
(< -2 0 4)                    ==>  #true
```

# >
`(> x1 x2 ...)`, *procedure*

Returns #true if arguments monotonically decreasing.
Note that complex numbers are not applicable.

# <=
`(<= x1 x2 ...)`, *procedure*

Returns #true if arguments monotonically non-decreasing.
Note that complex numbers are not applicable.

# >=
`(>= x1 x2 ...)`, *procedure*

Returns #true if arguments monotonically non-increasing.
Note that complex numbers are not applicable.

# max
`(max x1 x2 ...)`, *procedure*

Returns the maximum of arguments.  
Note that complex numbers are not applicable.

# min
`(min x1 x2 ...)`, *procedure*

Returns the minimum of arguments.  
Note that complex numbers are not applicable.

# +

`(+ z1 z2 ...)`, *procedure*

Returns the sum of arguments.

# -

`(- z)`, *procedure*

Returns the additive inverse of its argument.

# -

`(- z1 z2 ...)`, *procedure*

Returns the difference of arguments, associating to the left.

# *

`(* z1 z2 ...)`, *procedure*

Returns the product of arguments.

# /

`(/ z)`, *procedure*

Returns the multiplicative inverse of its argument.

# /

`(/ z1 z2 ...)`, *procedure*

Returns the quotient of arguments, associating to the left.

# abs
`(abs x)`, *procedure*

Returns the absolute value of its argument.

```scheme
(abs 3)                       ==>  3
(abs -3)                      ==>  3
(abs -3333333333333333333333) ==>  3333333333333333333333
(abs -7/11)                   ==>  7/11
(abs #i-17.33)                ==>  #i17.33
```

# quotient
`(quotient n1 n2)`, *procedure*

# remainder
`(remainder n1 n2)`, *procedure*

# modulo
`(modulo n1 n2)`, *procedure*

# numerator
`(numerator q)`, *procedure*

# denominator
`(denominator q)`, *procedure*

# floor
`(floor x)`, *procedure*

```scheme
(floor -4.3)                  ==> -5
(floor -4.6)                  ==> -5
(floor  4.3)                  ==>  4
(floor  4.6)                  ==>  4
```

# ceiling
`(ceiling x)`, *procedure*

```scheme
(ceiling -4.3)                ==> -4
(ceiling -4.6)                ==> -4
(ceiling  4.3)                ==>  5
(ceiling  4.6)                ==>  5
```

# truncate
`(truncate x)`, *procedure*

```scheme
(truncate -4.3)               ==> -4
(truncate -4.6)               ==> -4
(truncate  4.3)               ==>  4
(truncate  4.6)               ==>  4
```

# round
`(round x)`, *procedure*

```scheme
(round -4.3)                  ==> -4
(round -4.6)                  ==> -5
(round  4.3)                  ==>  4
(round  4.6)                  ==>  5
```

# rationalize
`(rationalize x y)`, *procedure*

Returns the simplest rational number differing from *x* by no more than *y*.

```scheme
(rationalize 0.723 1/10)      ==>  2/3
(rationalize 0.723 1/100)     ==>  5/7
(rationalize -0.723 1/100)    ==> -5/7
(rationalize 10197734562406803221/17452826108659293487 1/10)     ==>   1/2
(rationalize 10197734562406803221/17452826108659293487 1/100)    ==>   7/12
(rationalize 10197734562406803221/17452826108659293487 1/1000)   ==>   7/12
(rationalize 10197734562406803221/17452826108659293487 1/10000)  ==>  52/89
```

# square
`(square z)`, *procedure*

Returns the square of *z*.

```scheme
(square 2)                    ==>  4
(square -2)                   ==>  4
(square 3/7)                  ==>  9/49
(square 1+2i)                 ==> -3+4i
```

# sqrt
`(sqrt x)`, *procedure*
`(sqrt x n)`, *procedure*

Returns the principal square root of *x*. For exact numbers precision *n* can be provided. Default precision is 1/10000.  
The `sqrt` of inexact numbers ignores precision and produces inexact result.

Note: `sqrt` for an integer argument and precision 0 throws a run-time error if the result is non-integer.

```scheme
(sqrt 4)                      ==>  2
(sqrt -4)                     ==>  0+2i
(sqrt 0)                      ==>  0
(sqrt #i27.1441)              ==>  #i5.21
(sqrt 5)                      ==>  51841/23184
(sqrt 5 0.000001)             ==>  5374978561/2403763488
(sqrt 9/16)                   ==>  3/4
(sqrt -9/16)                  ==>  0+3/4i
(sqrt 0+3/4i 0.1)             ==>  291/476+291/476i
```

