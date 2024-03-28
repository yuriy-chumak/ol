Infix Notation
==============

Lisp is a very simple and elegant language.
Unfortunately, the generally accepted mathematical notation contradicts the Lisp one, which brings some inconvenience in the transfer of formulas into the language. So, the `infix-notation` macro was introduced.

# infix-notation
`(infix-notation expression)`, *macro*

Converts an *expression* in infix notation to prefix.  
`\\` is a shortcut for `infix-notation`. `\\` is experimental and may change in the future.

Note the use of spaces!
* `a + b` is a valid sum of `a` and `b`
* `a+b` is a symbol `|a+b|` which may or may not bound in the current environment,
* `a !` is a symbol `a` and a function `!`,
* `a!` is a symbol `|a!|`,
* but `18!` is a number `18` and a function `!`,
* spaces before and after `(` and `)` may be ommited,
* spaces around `,` may be ommited,

```scheme
; preamble:
> (import (math infix-notation))
> (define a 3)
> (define b 5)
> (define c 7)
> (define ^ expt)
> (define (f x) (\\ x + 2))
> (define (g x y) (\\ 2 * x + 3 * y + 1))

; simple arithmetic:
(\\  2 + 3)                 ==>   5
(\\  2 + 3 * 7)             ==>  23
(\\  (2 + 3) * 7)           ==>  35

; some operators have right associativity:
(\\  2 ^ 3 ^ 4)             ==>  2417851639229258349412352
(\\  2 ^ (3 ^ 4))           ==>  2417851639229258349412352
(\\  (2 ^ 3) ^ 4)           ==>  4096

; symbols are supported, sure:
(\\  a + b)                 ===  (+ a b)
(\\  b + a)                 ===  (+ b a)
(\\  a + b)                 ==>  8

(\\  a + b * c)             ===  (+ a (* b c))
(\\  (a + b) * c)           ===  (* (+ a b) c)
(\\  a * (b + c))           ===  (* a (+ b c))
(\\  a / b + c / d)         ===  (+ (/ a b) (/ c d))
(\\  (a + b) * (c + d))     ===  (* (+ a b) (+ c d))
(\\  a + b * c + d)         ===  (+ (+ a (* b c)) d)

(\\  a + b * c)             ==>  38
(\\  (a + b) * c)           ==>  56

; well, let's try some functions:
(\\  f(4))                  ==>   6
(\\  f(3 + 5))              ==>  10
(\\  g(3, 5))               ==>  22
(\\  g(3 5))                ==>  22 ; commas may be ommited

(\\  f(g(f(4),f(f(8)))) )   ===  (f (g (f 4) (f (f 8))))
(\\  f(g(f(4),f(f(8)))) )   ==>  51

; how about suffix functions:
(\\  18!   )                ==>  6402373705728000
(\\  18 !  )                ==>  6402373705728000
(\\  (18)! )                ==>  6402373705728000
(\\  (10 + 8)! )            ==>  6402373705728000
```

## Extending and Customizing
We may add custom operators:
```scheme
> (import (math infix-notation))

; let's define "power" operator
> (define (⊡ a b) (expt a b))

; add this operator with higher priority (+ has 2, * has 3)
> (define \\operators (put \\operators '⊡ 4))

; now use it
(\\  2 + 11 ⊡ 5)            ==>  161053
(\\  (2 + 11) ⊡ 5)          ==>  371293
```

We may extend our math with custom suffixes:

```scheme
> (import (math infix-notation))

; let's define "Fourth power" function
> (define (⁴ x) (* x x x x))

; add this function to the postfix dictionary
> (define \\postfix-functions (put \\postfix-functions '⁴ #t))

; now use it
(\\  2⁴ )                   ==>  16
(\\  2 ⁴ )                  ==>  16
(\\  (2)⁴ )                 ==>  16
(\\  (2 + 1)⁴ )             ==>  81

; and try it with a function
> (define (f x) (\\ x + 2))

(\\  f(2)⁴ )                ==> 256
(\\  f(2⁴) )                ==>  18
```

## Popular Mistakes

Spaces with symbols are very important!  
Check this code:
```scheme
> (import (math infix-notation))

; define some symbol value
> (define a 3)

;  and some dumb symbol |a!|
> (define a! 777)

(\\  a !)                   ==>  6    ; is a factorial of 3
(\\  a!)                    ==>  777  ; is a value of |a!|
```

## More Samples To Be a Regression Testing

You know, all provided examples and samples in the Ol Reference are used as a tests. From development point of view mainly as a regression tests (auto checks that new one Ol version is not broke something old). And a bit as a current development checks (kind of TDD).

Infix notation may have a very very much different arguments. So, a very very much different tests are required. So, here it is!

```scheme
> (import (math infix-notation))
> (define (² x) (* x x))

; one math operation
(\\  3 + 8)                  ==>  11
(\\  8 + 3)                  ==>  11
(\\  7 - 2)                  ==>   5
(\\  2 - 7)                  ==>  -5
(\\  3 * 8)                  ==>  24
(\\  8 * 3)                  ==>  24
(\\  8 / 3)                  ==> 8/3
(\\  3 / 8)                  ==> 3/8
(\\  3 ^ 8)                  ==> 6561
(\\  8 ^ 3)                  ==> 512

; two same operations
(\\  2 + 5 + 7)              ==>  14
(\\  2 - 5 - 7)              ==> -10
(\\  2 * 5 * 7)              ==>  70
(\\  2 / 5 / 7)              ==> 2/35
(\\  2 ^ 3 ^ 4)              ==> 2417851639229258349412352

; two mixed operations
(\\  2 + 5 - 7)              ==>   0
; pay attention, the * is performed before +, because of order of operations.
(\\  2 + 5 * 7)              ==>  37
(\\  2 + 5 / 7)              ==> 19/7
(\\  2 + 5 ^ 7)              ==> 78127

(\\  2 - 5 + 7)              ==>   4
(\\  2 - 5 * 7)              ==> -33
(\\  2 - 5 / 7)              ==> 9/7
(\\  2 - 5 ^ 7)              ==> -78123

(\\  2 * 5 + 7)              ==>  17
(\\  2 * 5 - 7)              ==>   3
(\\  2 * 5 / 7)              ==> 10/7
(\\  2 * 5 ^ 7)              ==> 156250

(\\  2 / 5 + 7)              ==>  37/5
(\\  2 / 5 - 7)              ==> -33/5
(\\  2 / 5 * 7)              ==>  14/5
(\\  2 / 5 ^ 7)              ==> 2/78125

(\\  2 ^ 5 + 7)              ==>  39
(\\  2 ^ 5 - 7)              ==>  25
(\\  2 ^ 5 * 7)              ==> 224
(\\  2 ^ 5 / 7)              ==>  32/7

; how about brackets?
(\\  (2 + 5) * 7)            ==>  49
(\\  (2 + 5) / 7)            ==>   1
(\\  (2 - 5) ^ 7)            ==> -2187
(\\  (2 * 5) ^ 7)            ==> 10000000
(\\  (2 / 5) ^ 7)            ==> 128/78125

; more, more insane brackets!
(\\  (((((2 + 5))))) * 7)    ==>  49
(\\  (((((2 * 5))) ^ 7)))    ==> 10000000

; some four operands tests
(\\  2 + 5 + 7 + 11 + 13)    ==>  38
(\\  2 + 5 * 7 + 11 * 13)    ==> 180
(\\  2 + 5 * 7 ^ 11 - 13)    ==> 9886633704
(\\  2 + 5 * 7 ^ 11 * 13)    ==> 128526238297
(\\  2 / 5 / 7 / 11 / 13)    ==> 2/5005

(\\  2 / (5 / 7) / 11 / 13)  ==> 14/715
(\\  2 / (5 / 7 / 11) / 13)  ==> 154/65

(\\  (2 + 5) * 7 ^ (11 - 13))  ==>  1/7

; ok... let's some negative and rational numbers
(\\  2 + 5/3 + 7)            ==> 32/3
(\\  2 + 5/3 + +7)           ==> 32/3
(\\  2 + 5/3 + -7)           ==> -10/3
(\\  2 + 5/3 * -7)           ==> -29/3

; btw, we have a postfix functions
(\\  7!)                     ==> 5040
(\\  7! + 5!)                ==> 5160
(\\  (5 + 7)!)               ==> 479001600
(\\  7! + 5! * 3!)           ==> 5760
(\\  7 ! + 5 ! * 3 !)        ==> 5760
(\\  (7!) + (5!) * (3!))     ==> 5760
(\\  (7! + 5!) * 3!)         ==> 30960
(\\  (7 ! + (5)!) * ((3))!)  ==> 30960
(\\  7² + 5² * 3²)           ==> 274

; did we forget functions? no, we didn't.
(\\ sqrt(7))                 ==> 23543191457/8898489952
(\\ sqrt(7, 0.001))          ==> 108497/41008
(\\ gcd(3 * 8, 44))          ==>   4
(\\ 2 ^ gcd(3 * 8, 44))      ==>  16
(\\ expt(2, gcd(3 * 8, 44))) ==>  16

; ...
```
