Numbers
=======

Mathematically, numbers are arranged into a tower of subtypes, in which each level is a superset of the level above it:
* natural (ℕ, positive countable numbers)
* integer (ℤ, additionally includes negative numbers and zero)
* rational (ℚ, a fraction of integer numbers)
* real (ℝ, includes *inexact* numbers)
* complex number (ℂ, includes *complex* numbers)
* number (any number above)

Note that by default Ol works with *exact* numbers (meaning that all numbers are exact except those expicitly marked).
If you want to use *inexact* numbers (machine-defined numbers such as *float* or *double*, if supported), you must explicitly indicate this using `#i` prefix or the `inexact` function.

```scheme
> 1.2
6/5

> #i2.135
2.13499999

> 2.5e17
250000000000000000

> (inexact 2.5e17)
2.5e17
```

Number notation allows the use of a radix prefix for binary (`#b`), octal (`#o`), decimal (`#d`), or hexadecimal (`#x`) numbers. The decimal prefix is used by default and can be omitted. Decimal point is allowed for any radix, of course.

```scheme
#b1000001    ==>  65
#b100.001    ==>  33/8
#b1.01e11    ==>  125000000000

#o1234567    ==>  342391
#o-1234567   ==> -342391
#o12.33      ==>  667/64

#xdeadbeef   ==>  3735928559
#xDeADBeEf   ==>  3735928559

; 17₈ + 10.00001₂
> (+ #o17 #b10.00001)
545/32

; 17₈ + 10.00001₂ - 11.08₁₆
> (+ #o17 #b10.00001 #x-11.08)
0

; inexact (machine floating point) of 11100₂
> #i#b11100
28.0

; inexact (machine floating point) of 12.33₈
> #i#o12.33
10.421875
```

### Special numeric constants

* *+inf.0* is a positive infinity. Always inexact, there is no exact analogue.
* *-inf.0* is a negative infinity. Always inexact, there is no exact analogue.
* *+nan.0* is a not-a-number. Always inexact, there is no exact analogue.
* There is no constant for negative exact zero. *+0*, *0*, and *-0* are always read as just "0" (not positive, not negative). But if you really need it, you can get one using the Ol syntax `(vm:cast 0 type-value-)`.
  * Note: you can't create *positive zero*.
* There is no constant for negative inexact zero. *#i+0*, *#i0*, and *#i-0* are always read as just "#i0" (not positive, not negative). But if you really need it, you can get one using the syntax `(* -1 #i0)`.
  * Note: you can't create *positive zero*.

While *exact* numbers retain their form independently regardless of the platform architecture and bit depth, *inexact* numbers may differ in form and limits. This leads to different behavior of +inf.0, -inf.0, +nan.0 in some math procedures like `<`, `>`, `+`, etc.

For example, the `(< 1e40 +inf.0)` will produce #true under `aarch64` and #false under `armv7`. That's because of default inexact number bits width under these platforms. We will use numbers of a sufficient size so that they work equally on all platforms regardless of bit depth. But always remember about the possibility of ambiguous behavior of very large and very small imprecise numbers.

#### +inf.0
#### -inf.0
#### +nan.0

#### -0 and -0.0

Please don't use *-0* if you can. The behavior of such numbers is unspecified, platform dependent, and not guaranteed by Ol. *-0* is only supported for compatibility with some external programs (e.g. blender3d, which sometimes stores very small negative numbers as -0).

```scheme
; there is no convenient way to create -0
> (define |-0| (vm:cast 0 type-value-))
> (define |-0.0| (* -1 #i0))

(< |-0| 0)                    ==>  #true
(negative? |-0|)              ==>  #true
(= |-0| 0)                    ==>  #false

```


```scheme
(< -inf.0 +inf.0)             ==>  #true
(< 100000 +inf.0)             ==>  #true
(= 1e1000 +inf.0)             ==>  #true
(> -inf.0 +inf.0)             ==>  #false
(< -inf.0 +inf.0)             ==>  #true
(> -inf.0 +inf.0)             ==>  #false
(< +nan.0 +inf.0)             ==>  #false ; not-a-number is not a number
; special numeric constants always can be compared with itself
(= +nan.0 +nan.0)             ==>  #true
```


## TOC
[number?](#number), [complex?](#complex), [real?](#real), [rational?](#rational), [integer?](#integer), [natural?](#natural),
[exact?](#exact), [inexact?](#inexact),
[finite?](#finite), [infinite?](#infinite), [nan?](#nan),
[zero?](#zero), [positive?](#positive), [negative?](#negative), [odd?](#odd), [even?](#even),

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
(number?
   (string->number "1234"))   ==>  #true
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

The numbers *+inf.0*, *-inf.0*, and *+nan.0* are real but not rational.
```scheme
(real? +inf.0)                ==>  #true
(real? -inf.0)                ==>  #true
(real? +nan.0)                ==>  #true
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

(rational? "1234")            ==>  #false
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
