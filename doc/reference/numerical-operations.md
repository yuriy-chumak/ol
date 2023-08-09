Numerical operations
====================

Mathematically, numbers are arranged into a tower of subtypes in which each level is a superset of the level above it:
* natural (ℕ, positive countable numbers)
* integer (ℤ, additionally includes negative numbers and zero)
* rational (ℚ, a fraction of integer numbers)
* real (ℝ, includes *inexact* numbers)
* complex number (ℂ, includes *complex* numbers)
* number (any number above)

Please note that Ol works with *exact* numbers by default. If you want to use *inexact* numbers (machine defined numbers like *float* or *double*, if supported), you should explicitly note this using `#i` prefix or `inexact` function.

```scheme
> 1.2
6/5

> #i1.2
1.199999999

> 1.2e15
1200000000000000

> (inexact 1.2e15)
1.2e15
```

Number notation allows the use of a radix prefix for binary (*#b*), octal (*#o*), decimal (*#d*), or hexadecimal (*#x*) numbers. The decimal prefix is used by default and can be omitted.  
The decimal point is allowed, of course.

```scheme
#b1000001    ==>  65
#b100.001    ==>  33/8
#b1.01e11    ==>  125000000000

#o1234567    ==>  342391
#o-1234567   ==> -342391
#xdeadbeef   ==>  3735928559
#xDeADBeEf   ==>  3735928559

; 17₈ + 10.00001₂
> (+ #o17 #b10.00001)
545/32

; 17₈ + 10.00001₂ - 11.08₁₆
> (+ #o17 #b10.00001 #x-11.08)
0

; inexact (machine double floating point) of 11100₂
> #i#b11100
28.0
```

## TOC
[number?](#number), [complex?](#complex), [real?](#real), [rational?](#rational), [integer?](#integer), [natural?](#natural),
[exact?](#exact), [inexact?](#inexact),
[finite?](#finite), [infinite?](#infinite), [nan?](#nan),
[zero?](#zero), [positive?](#positive), [negative?](#negative), [odd?](#odd), [even?](#even),
[=](#=), [<](#-1), [>](#-2), [<=](#-3), [>=](#-4),
[max](#max), [min](#min),
[+](#-5), [-](#-), [*](#-6), [/](#-7),
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
(number? (string->number "1234"))  ==>  #true
(number? #i3.14)              ==>  #true
(number? +inf.0)              ==>  #true
(number? +nan.0)              ==>  #true
```

# complex?
`(complex? obj)`, *procedure*

Returns #true if *obj* is a complex number.  
Note that rational, integer, inexact, and real numbers are also complex. If you want to know if a number is exactly complex and not a real or something like, use `(eq? (type obj) type-complex)`.

```scheme
(complex? 3+7i)               ==>  #true
(complex? (complex 3 7))      ==>  #true
(complex? 3+0i)               ==>  #true
(complex? 17)                 ==>  #true
(complex? #b1101)             ==>  #true
(complex? "3+7i")             ==>  #false
(complex? 11/17)              ==>  #true
(complex? +inf.0)             ==>  #true

(eq? (type 11/7) type-complex)  ==>  #false
(eq? (type 3+0i) type-complex)  ==>  #false
(eq? (type 3+2i) type-complex)  ==>  #true
```

# real?
`(real? obj)`, *procedure*

Returns #true if *obj* is a real number.  
Real numbers are those numbers used to measure a continuous one-dimensional quantity such as a distance, duration or temperature. Note that rational, integer, and inexact numbers, +inf.0, -inf.0, and +nan.0 are also real.

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
Rational numbers are numbers that can be expressed as the quotient or fraction of two integers. Note that integer numbers are also rational.  
If you want to know if a number is exactly rational and not an integer, use `(eq? (type obj) type-rational)`.

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
Inexact numbers are platform-specific floating-point numbers occupying fixed computer memory (usually 64 bits). If supported by platform, sure.

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
(zero? (- #i1.0000000000000001 1))  ==> #true ; inexact math is so inexact
(zero? (- 1.0000000000000001 1))    ==> #false ; but exact is exact. smile
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

Returns #true if arguments are equal in a mathematical sense. Up to 249 arguments are supported.

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

Returns #true if arguments monotonically increasing. Up to 249 arguments are supported.  
Note that complex numbers are not applicable.

```scheme
(< 1)                         ==>  #true
(< -1)                        ==>  #true
(< 1 2 3)                     ==>  #true
(< 1 #i2 3)                   ==>  #true
(< 1 2 2 3)                   ==>  #false
(< 1 2 3 1)                   ==>  #false
(< -2.1 0 4)                  ==>  #true
```

# >
`(> x1 x2 ...)`, *procedure*

Returns #true if arguments monotonically decreasing. Up to 249 arguments are supported.  
Note that complex numbers are not applicable.

```scheme
(> 1)                         ==>  #true
(> -1)                        ==>  #true
(> 3 2 1)                     ==>  #true
(> 3 #i2 1)                   ==>  #true
(> 3 2 2 1)                   ==>  #false
(> 3 2 1 2)                   ==>  #false
(> 4 0 -2.1)                  ==>  #true
```

# <=
`(<= x1 x2 ...)`, *procedure*

Returns #true if arguments monotonically non-decreasing. Up to 249 arguments are supported.  
Note that complex numbers are not applicable.

```scheme
(<= 1)                         ==>  #true
(<= -1)                        ==>  #true
(<= 1 2 3)                     ==>  #true
(<= 1 #i2 3)                   ==>  #true
(<= 1 2 2 3)                   ==>  #true
(<= 1 2 3 1)                   ==>  #false
(<= -2.1 0 4)                  ==>  #true
```

# >=
`(>= x1 x2 ...)`, *procedure*

Returns #true if arguments monotonically non-increasing. Up to 249 arguments are supported.  
Note that complex numbers are not applicable.

```scheme
(>= 1)                         ==>  #true
(>= -1)                        ==>  #true
(>= 3 2 1)                     ==>  #true
(>= 3 #i2 1)                   ==>  #true
(>= 3 2 2 1)                   ==>  #true
(>= 3 2 1 2)                   ==>  #false
(>= 4 0 -2.1)                  ==>  #true
```

# max
`(max x1 x2 ...)`, *procedure*

Returns the maximum of arguments. Up to 249 arguments are supported.  
Note that complex numbers are not applicable.

```scheme
(max 7)                        ==>  7
(max -100 100)                 ==>  100
(max 1 2 3)                    ==>  3
(max -1 -2 -3)                 ==> -1
(max 1 #i3 2)                  ==> #i3
```

# min
`(min x1 x2 ...)`, *procedure*

Returns the minimum of arguments. Up to 249 arguments are supported.  
Note that complex numbers are not applicable.

```scheme
(min 7)                        ==>  7
(min -100 100)                 ==> -100
(min 1 2 3)                    ==>  1
(min -1 -2 -3)                 ==> -3
(min #i1 3 2)                  ==> #i1
```

# +
`(+ z1 z2 ...)`, *procedure*

Returns the sum of arguments. Up to 249 arguments are supported.  
Produces exact results when all given arguments are exact.

```scheme
(+)                            ==>  0
(+ 1)                          ==>  1
(+ 1 2 3 4)                    ==>  10
(+ 7/2 9/2)                    ==>  8
(+ #i9 1)                      ==>  #i10
(+ 2+3i 11/7)                  ==>  25/7+3i

> (+ 2+3i #i10.01 #b1010 #o2.2)
24.2599999+3i

> (apply + '(17 -3 45.67 111222333444555666))
11122233344455572567/100
```

# -
`(- z)`, *procedure*

Returns the additive inverse of its argument.

```scheme
(- 7)                          ==> -7
(- -9)                         ==>  9
(- 3+2i)                       ==> -3+2i
```

# -
`(- z1 z2 ...)`, *procedure*

Returns the difference of arguments, associating to the left. Up to 249 arguments are supported.  
Produces exact results when all given arguments are exact.

```scheme
(- 1)                          ==> -1
(- 1 2 3 4)                    ==> -8
(- 7/2 9/2)                    ==> -1
(- #i9 1)                      ==> #i8
(- 2+3i 11/7)                  ==> 3/7+3i

> (- 2+3i #i10.01 #b1010 #o2.2)
-20.2599999+3i

> (apply - '(17 -3 45.67 111222333444555666))
-11122233344455569167/100
```

# *
`(* z1 z2 ...)`, *procedure*

Returns the product of arguments. Up to 249 arguments are supported.  
Produces exact results when all given arguments are exact.

```scheme
(* 1)                           ==>  1
(* 1 2 3 4 5)                   ==>  120
(* 1 2 0 3 4 5)                 ==>  0
(* 0+i 0+i)                     ==> -1
(* 7 9 #i3)                     ==>  #i189

; inexact numbers can't be complex
> (* #i3 7-8i)
21.0

; but complex can be inexact
> (* 7-8i #i3)
21.0-24.0i
```

# /

`(/ z)`, *procedure*

Returns the multiplicative inverse of its argument.

```scheme
(/ 7)                           ==>  1/7
(/ -8)                          ==> -1/8
(/ 1+2i)                        ==>  1/5-2/5i
(/ 1/5-2/5i)                    ==>  1+2i
(/ 0)                           ==>  +inf.0
(/ +inf.0)                      ==>  #i0
```

# /

`(/ z1 z2 ...)`, *procedure*

Returns the quotient of arguments, associating to the left. Up to 249 arguments are supported.  
Produces exact results when all given arguments are exact.

```scheme
(/ 1 2 3 4 5)                   ==>  1/120
(/ 1 2 0 3 4 5)                 ==>  +inf.0
(/ 0+i 0+i)                     ==>  1
(/ 0+i 0+i 0+i)                 ==>  0-i

> (/ 7 9 #i3)
0.259259259
```

# abs
`(abs x)`, *procedure*

Returns the absolute value of its argument.  
Note that complex numbers are not applicable.


```scheme
(abs 3)                       ==>  3
(abs -3)                      ==>  3
(abs -3333333333333333333333) ==>  3333333333333333333333
(abs -7/11)                   ==>  7/11
(abs #i-17.33)                ==>  #i17.33
```

# quotient
`(quotient n1 n2)`, *procedure*

Returns the quotient of *n1*/*n2*.

```scheme
(quotient 7 2)                ==>  3
(quotient 0 11)               ==>  0
(quotient -10 -3)             ==>  3
```

# remainder
`(remainder n1 n2)`, *procedure*

Returns the remainder of *n1*/*n2*.

```scheme
(remainder 7 2)               ==>  1
(remainder 0 11)              ==>  0
(remainder -10 -3)            ==> -1
```

# modulo
`(modulo n1 n2)`, *procedure*

Returns the modulo of *n1*/*n2*.

```scheme
(modulo 7 2)                  ==>  1
(modulo 0 11)                 ==>  0
(modulo -10 -3)               ==> -1
```

# numerator
`(numerator q)`, *procedure*

Returns the numerator of *q*.

```scheme
(numerator 1)                 ==>  1
(numerator 7/3)               ==>  7
(numerator 3/7)               ==>  3
(numerator 3/33)              ==>  1
(numerator 33/3)              ==>  11
```

# denominator
`(denominator q)`, *procedure*

Returns the denominator of *q*.

```scheme
(denominator 1)               ==>  1
(denominator 7/3)             ==>  3
(denominator 3/7)             ==>  7
(denominator 3/33)            ==>  11
(denominator 33/3)            ==>  1
```

# floor
`(floor x)`, *procedure*

Returns the largest integer not larger than *x*.

```scheme
(floor -4.3)                  ==> -5
(floor -4.6)                  ==> -5
(floor  4.3)                  ==>  4
(floor  4.6)                  ==>  4
```

# ceiling
`(ceiling x)`, *procedure*

Returns the smallest integer not smaller than *x*.

```scheme
(ceiling -4.3)                ==> -4
(ceiling -4.6)                ==> -4
(ceiling  4.3)                ==>  5
(ceiling  4.6)                ==>  5
```

# truncate
`(truncate x)`, *procedure*

Returns the integer closest to x whose absolute value is not larger than the absolute value of *x*

```scheme
(truncate -4.3)               ==> -4
(truncate -4.6)               ==> -4
(truncate  4.3)               ==>  4
(truncate  4.6)               ==>  4
```

# round
`(round x)`, *procedure*

Returns the closest integer to *x*, rounding to even when *x* is halfway between two integers.

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
