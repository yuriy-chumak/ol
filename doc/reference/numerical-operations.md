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


# number?
`(number? obj)`, *procedure*

Returns #true if *obj* is a number. Any number.

```scheme
(number? 1)                   ==>  #true
(number? '(1))                ==>  #false
(number? 7-4i)                ==>  #true
(number? #o771263)            ==>  #true
(number? "1234")              ==>  #false
(number? 4/21)                ==>  #true
(number? #i3.14)              ==>  #true
(number? +inf.0)              ==>  #true
```

# complex?
`(complex? obj)`, *procedure*

Returns #true if *obj* is a complex number.  
Note that rational, integer, and real numbers are also complex.

```scheme
(complex? 3+7i)               ==>  #true
(complex? 3+0i)               ==>  #true
(complex? 17)                 ==>  #true
(complex? #b110111)           ==>  #true
(complex? "3+7i")             ==>  #false
(complex? +inf.0)             ==>  #true
(complex? -inf.0)             ==>  #true
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
(rational? 6/10)              ==>  #true
(rational? 6/3)               ==>  #true
(rational? 3+4i)              ==>  #false
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
```

# exact?
`(exact? obj)`, *procedure*

Returns #true if *obj* is an exact number.  
Exact numbers are all numbers except inexact.

```scheme
(exact? 3)                    ==>  #true
(exact? 3.14)                 ==>  #true
(exact? 3+7i)                 ==>  #true
(exact? 3/7-i)                ==>  #true
(exact? +inf.0)               ==>  #false
```

# inexact?
`(inexact? obj)`, *procedure*

Returns #true if *obj* is an inexact number.  
Inexact numbers are platform-specific floating-point numbers occupying fixed computer memory (usually 64 bits).

```scheme
(inexact? 3)                    ==>  #false
(inexact? 3.14)                 ==>  #false
(inexact? 3+7i)                 ==>  #false
(inexact? 3/7-i)                ==>  #false
(inexact? +inf.0)               ==>  #true
(inexact? #i3.14)               ==>  #true
```

# finite?
`(finite? obj)`, *procedure*

Returns #true for all real numbers except +inf.0, -inf.0, and +nan.0,
and for complex numbers if their real and imaginary parts are both finite.

```scheme
(finite? 3)                     ==>  #true
(finite? 3.14)                  ==>  #true
(finite? +inf.0)                ==>  #false
(finite? -inf.0)                ==>  #false
(finite? +nan.0)                ==>  #false
(finite? #i3.14)                ==>  #true
(finite? #i2-7i)                ==>  #true
(finite? +inf.0-7i)             ==>  #false
```

# infinite?
`(infinite? obj)`, *procedure*

Returns #true for the real numbers +inf.0 and -inf.0,
and for complex numbers if their real or imaginary parts or both are infinite.

```scheme
(infinite? 3)                   ==>  #false
(infinite? 3.14)                ==>  #false
(infinite? +inf.0)              ==>  #true
(infinite? -inf.0)              ==>  #true
(infinite? +nan.0)              ==>  #false
(infinite? #i3.14)              ==>  #false
(infinite? #i2-7i)              ==>  #false
(infinite? +inf.0-7i)           ==>  #true
```

# nan?
`(nan? obj)`, *procedure*

Returns #true for +nan.0,
and for complex numbers if their real or imaginary parts or both are +nan.0

```scheme
(nan? +nan.0)                  ==> #true
(nan? 32)                      ==> #false
(nan? +nan.0+5.0i)             ==> #true
(nan? 5.0++nan.0i)             ==> #true
(nan? 1+2i)                    ==> #false
```

